# The v6 grid: what is fixed, what is not, and how to run it

2026-07-29. Written mid-debug, so that the state survives the session.

The v6 grid re-runs the top-tax dials on the three-rung model, so that every dial
carries a mechanical rung of its own rather than an inferred one. Two attempts
have been made and both stopped. The plumbing faults behind the first are fixed
and committed; one modelling fault remains open, and the run cannot finish until
it is resolved or consciously worked around.

## The open problem: a marginal tax rate above one

The gains bathtub stopped on two of the thirty scenarios in the first tranche:

| scenario | reported marginal cost | age index |
|---|---|---|
| `s_ord_r44p8` | 1.0141 | 28 |
| `s_wealth_r1_t150` | 41.1556 | 40 |

The net-of-tax response form is `(1 - MC) ^ eta`, undefined as the marginal cost
reaches one, and `src/sim/kg/bellman.R` stops rather than clamp. That refusal is
deliberate and correct; it is how the problem surfaced at all.

In the Bellman the cost of realizing is

    MC = tau + beta (1 - m) (W_next - h) + beta m F

**The measured cause.** In the mechanical detail for `s_wealth_r1_t150` in 2045,
`mtr_kg_lt` runs from -0.0774 to **50.1880**, with four records above one and
thirty-two negative. A marginal rate on gains cannot exceed one. `tau` is the
gain-weighted cell average of that column, so a single poisoned record carries
into the cell and straight into the marginal cost. The other inputs were checked
on the same file and are sound: `mtr_net_worth` runs 0 to 0.01 with no negatives,
`mtr_estate` 0 to 0.4.

The likely mechanism is the numerical derivative behind an MTR: liability is
recomputed with gains bumped by a dollar and the difference divided by the bump,
so a record whose liability moves for a threshold reason while its own gains are
negligible returns an enormous ratio. This is a hypothesis about the mechanism.
The rate being out of range is a measurement.

**The two failures are different in kind.** 1.0141 on the top ordinary rate dial
is a boundary: push rates high enough and this response form genuinely runs out
of room. 41.16 on a wealth dial is a handful of poisoned records. A fix for the
second does not settle the first.

**Hypotheses already eliminated**, so that nobody spends the time again:

- Not the wealth carrying cost. `h` is the gain-weighted average of the wealth
  rate times the gains rate, bounded near 0.004 at a 1% wealth tax and scaled by
  `kg.wealth_carry_scale`, which is 1. It also enters with a minus sign, so it
  lowers the cost, as its comment says.
- Not `mtr_net_worth` being pathological. Checked; clean.
- Not the marginal-rate convention flip on its own. The crossing diagnostic for
  both failing scenarios shows the two conventions agreeing to five decimals,
  with shifts of exactly the statutory rate. But note the diagnostic compares
  reform-against-baseline differences while the Bellman consumes levels, so the
  flip is not fully exonerated as the thing that tipped these dials over.

**Both dials completed in v5**, with full conventional output, under the same
response form. So something in this branch moved them across the cap. The
candidates are the marginal rates now being read off the mechanical frame rather
than the static one, and the gains elasticity re-pin from 1.6625 to 1.6692.

**Suggested next steps**, cheapest first:

1. Find the four records with `mtr_kg_lt > 1` and read their gains, liability and
   bump. That says whether the cause is the threshold-crossing derivative or
   something else.
2. Decide what an out-of-range MTR should do. Options: reject at source in
   `calc_mtrs`, exclude such records from the cell average, or bound the reported
   rate. Each has a different meaning and the choice belongs to the author, not
   to a patch.
3. Only then consider the boundary case. If a 44.8% top rate legitimately drives
   the marginal cost to one, that is a statement about the response form's range
   of validity and belongs in the methodology memo rather than in a clamp.

Do not clamp the marginal cost to make the run finish. If the cell rates are
contaminated, the twenty-eight scenarios that stayed under the cap are wrong too,
quietly.

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
