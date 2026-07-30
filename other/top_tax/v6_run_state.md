# The v6 grid: what is fixed, what is not, and how to run it

2026-07-29. Written mid-debug, so that the state survives the session.

The v6 grid re-runs the top-tax dials on the three-rung model, so that every dial
carries a mechanical rung of its own rather than an inferred one. Two attempts
have been made and both stopped. The plumbing faults behind the first are fixed
and committed, and so is the modelling fault behind the second.

## The problem: a marginal tax rate of fifty

The gains bathtub stopped on two of the thirty scenarios in the first tranche:

| scenario | reported marginal cost | age index |
|---|---|---|
| `s_ord_r44p8` | 1.0141 | 28 |
| `s_wealth_r1_t150` | 41.1556 | 40 |

The net-of-tax response form is `(1 - MC) ^ eta`, undefined as the marginal cost
reaches one, and `src/sim/kg/bellman.R` stops rather than clamp. That refusal is
what surfaced the problem, and it is worth keeping.

In the Bellman the cost of realizing is

    MC = tau + beta (1 - m) (W_next - h) + beta m F

`tau` is the gain-weighted average, within an age cohort, of each record's
`mtr_kg_lt`. In the mechanical detail for `s_wealth_r1_t150` in 2045 that column
runs from -0.0774 to 50.1880, four records above one and thirty-two negative.

**The cause.** A marginal rate is measured by recomputing liability with the
variable bumped a dollar. The child credit phases out in $1,000 blocks at 5%, so
the dollar of gains that carries a record across a block boundary costs $50, and
the measured rate is 50 plus the preferred rate plus the NIIT -- 50.188 exactly.
Two blocks at once give 100.18, and the value of 60.0 comes from a different
step. The derivative is right and the rate is not: a realization decision is made
in thousands of dollars, not in the dollar that straddles a step.

**The fix** (`9487f5eb2`) drops records measuring outside the unit interval from
the cell averages, numerator and denominator together, in the three aggregators
in `src/sim/kg/apply.R` that read the column. A marginal rate on realized gains
lies in the unit interval; a number outside it is a measurement of a step rather
than of a rate. A cell that loses more than a hundredth of its weighted
realizations that way is now reported.

The alternative of bounding each record's rate at 1 was rejected: it pins the
same record at a 100% tax on gains, which is no more a price than 50 is. Widening
the probe past a dollar would have worked too, but the dollar bump is the
model-wide MTR convention and `mtr_kg_lt` has other consumers.

**How much it moves**, measured on `kg_v5_revmax` baseline detail over 2027-2055
by `other/kg_model_tests/mtr_range_impact.R`, output kept alongside it as
`mtr_range_impact.txt`:

- One record, occasionally two or four, per affected cell-year.
- Out-of-range records are 3 in 100,000 of weighted realizations, which is why
  this went unseen.
- In the cells they touch they are decisive. The worst is age 42 in 2041, where
  the cell rate reads 0.3365 against 0.2173 without them -- twelve points, from a
  record holding 0.24% of the cell's realizations. Age 44 in 2032 moves nine
  points, age 53 in 2028 four.
- Most cell-years move under 5e-4, and dropping the negative records moves a rate
  up by about that much.

This was the baseline leg, so v5's shipped gains numbers carry the same
contamination. It is small in aggregate and concentrated in a few thin cells.

**Consequences for the calibration.** Editing `apply.R`, `inputs.R` and
`constants.R` makes `eta_logs` stale, correctly: the cell rates its derivation
ran on have moved. It is being re-derived on the three-point grid
(`other/kg_model_tests/form_ab/launch_eta_dial_logs.sbatch`, vintage suffix
`_range`), and carries a dated acknowledgement in the meantime so that the
verification runs can proceed. The measurement protocol in
`measure_efull_logs.R` was deliberately left alone: its `dtau` is a difference of
realization-weighted rates across a 5pp shock, in which a stepped record's $50
appears on both sides and cancels, and the protocol is shared with the levels pin.

**The ordinary-rate dial at 1.0141** is 1.4% over the cap, so removing the
contaminated records from its cells may well clear it. If it does not, that is a
statement about where the net-of-tax response form runs out of range at a 44.8%
top rate, and belongs in the methodology memo rather than in a clamp. The
two-dial run under `config/runscripts/tests/mtr_range_capfix.csv` is what decides
this.

## What is fixed and committed

- `006c4436f` sizes the grid tranches to the scratch quota.
- `5a285c698` fixes `swap_tax_law`, which dropped only the incoming law's
  columns. A vector-valued subparameter becomes one column per element, indexed,
  and the two laws need not agree on the count: a two-bracket wealth tax gives
  `wealth.rates1` and `wealth.rates2` where the baseline's single 0% bracket
  gives `wealth.rates`. The leftovers priced a schedule belonging to neither law.
  It crashed every wealth dial on the mechanical rung; where the counts differ
  but the names do not, it would have been silent. Same commit switches the SLURM
  drivers to `conditionMessage`, without which an rlang condition reports as an
  empty string -- which is why the first failure took an hour to read.
- `25ba91a02` adds `years_per_task` and fixes `purge_detail`, which listed the
  static and conventional trees only and so left the no-wealth and mechanical
  detail on disk.

Uncommitted: the tranche driver's success check. It matched `^taxsim` against
`sacct` output, which right-pads the job name, so the guard never fired and the
driver twice walked past a failed tranche onto the next. Now matches unanchored,
and verified to report 227 bad jobs on the window it previously scored as clean.

## Verified good

The mechanical rung itself is sound at grid scale: in the second attempt, phases
2A, 2MN, 2MW and 2M all completed 480 of 480 tasks, wealth dials included.

`config/runscripts/tests/mech_law_swap_smoke.csv` covers the four dial families
whose law shape differs from baseline, and all three rungs come out for each. On
the taxable-maximum dial the employer wage adjustment shows as the step from
static to mechanical: payroll 2,033.7 static against 2,018.5 mechanical, income
tax 2,562.0 against 2,529.3. That step is the whole point of the rung.

## Disk

The grid writes five detail trees per scenario under the mechanical rung, about
25G a scenario, against a 10T group quota. `df` on the scratch path reports the
quota; the 700T reading is the filesystem, not the allowance.

On 2026-07-29 the v3, v5 and partial v6 vintages and `kg_v3_regress` were
deleted, on the author's instruction, freeing roughly 6.7T. **v5 is gone,
including its totals**: every deliverable built from it survives only as the
extracted files already in the repository. Anything needing a number not already
extracted needs v6.

With that space the whole grid fits in one submission, which is the better shape:
one set of eleven phase barriers rather than eight. The tranche machinery stays
in the repository for the next time the quota is tight.

## How to run the grid

The runscripts are generated, 225 counterfactuals over
`config/runscripts/top_tax/dials.csv` plus `dials_patch4.csv`, split into
`config/runscripts/top_tax/v6/tranche*.csv`, each carrying the baseline row.

Preconditions:

- The bathtub problem above is resolved, or the affected dials are knowingly
  excluded.
- `df -h /nfs/roberts/scratch/pi_nrs36` shows enough headroom: about 5.5T for one
  submission of everything, or 0.75T per tranche of thirty.
- Nothing of yours is in the queue: `squeue -u $USER`.

### One submission, if the quota allows

Concatenate the tranches into one runscript, keeping a single baseline row, and
launch through an sbatch wrapper. Phase 0 parses globals and reads Tax-Data in
process, so it must not run on a login node.

    # from the repository root
    python3 - <<'EOF'
    import csv, glob
    out, seen, hdr, base = [], set(), None, None
    for f in sorted(glob.glob('config/runscripts/top_tax/v6/tranche*.csv')):
        rows = list(csv.reader(open(f, newline='')))
        hdr = rows[0]
        for r in rows[1:]:
            if r[0] == 'baseline': base = r
            elif r[0] not in seen: seen.add(r[0]); out.append(r)
    with open('config/runscripts/top_tax/v6/all.csv', 'w', newline='') as fh:
        w = csv.writer(fh, lineterminator='\n')
        w.writerow(hdr); w.writerow(base)
        for r in out: w.writerow(r)
    print(len(out), 'counterfactuals')
    EOF

    sbatch --partition=week --time=2-00:00:00 -c 2 --mem=32G \
      --job-name=v6-all --output=other/top_tax/logs/v6_all_%j.log \
      --wrap="cd $PWD && bash slurm_run.sh top_tax/v6/all NULL 1 \
              top_tax_dials_30y_v6 1 0 NULL 1 batch 2"

### Tranche by tranche, if it is tight

    sbatch other/top_tax/run_v6_tranches.sbatch top_tax_dials_30y_v6 2

The driver runs whatever `tranche*.csv` files exist, in order, waits for each to
drain, checks that its jobs completed, and stops on any failure.

### The arguments

`slurm_run.sh <runscript> <scenario_id> <local> <vintage> <pct_sample> <stacked>
<baseline_vintage> <delete_detail> [submit_mode] [years_per_task]`

- `stacked` is 0. v5 produced no stacked artifacts and the dials are independent.
- `delete_detail` is 1. Distribution tables are built from detail in Phase 3b and
  the purge runs in Phase 4, after them. Totals, receipts and supplemental
  survive, which is what the charts read.
- `batch` is required at this size: one array per phase, about a dozen sbatch
  calls. Chains mode would need a dozen per scenario against a 200-per-hour cap.
- `years_per_task` of 2 halves the task count. Verified byte-identical against 1
  on both the mechanical and the kg smokes; see
  `other/performance/years_per_task.md`.

### Watching it

    squeue -u $USER -o "%.12i %.22j %.9T %.10M"
    tail -f other/top_tax/logs/v6_all_*.log

Per-phase outcomes, which is what tells you a phase failed rather than finished:

    sacct -j <phase job id> -X -n --format=State | sort | uniq -c

An empty queue is not success. A failed phase leaves its dependents cancelled, so
a broken run drains sooner than a healthy one. Check states, never the queue.

### When it finishes

Expect, per scenario: `static`, `mechanical`, `conventional` each with `totals`
and `supplemental`, plus `mechanical_no_wealth` and `conventional_no_wealth` with
neither, being measurement passes. Then the chart work: figures 3 and 4 and the
interactive gain a third bar, read from the mechanical rung's receipts.
