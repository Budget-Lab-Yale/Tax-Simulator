# Tax-Simulator performance audit — 2026-07-25

Scope: where does the model's compute actually go, and what serial work could be
parallelized or eliminated. Everything below is measured, not estimated, except
where explicitly labeled as an estimate.

Branch at time of audit: `wealth` (working tree carried the uncommitted
corp-rate on-model changes to `src/calc/do_taxes.R` and `src/data/tax_law.R`).

---

## 1. Method

### 1.1 Cluster-level accounting

`sacct -u $USER --starttime=2026-07-05`, aggregated by job name over completed
tasks. This covers roughly three weeks of real production and calibration work,
so the mix is representative of how the model is actually used.

| phase (job name)      | n tasks | mean s | max s | CPU-hours |
|-----------------------|--------:|-------:|------:|----------:|
| `taxsim-cf-static` (2A)  | 36,839 | 101 | 329 | ~1,030 |
| `taxsim-cf-conv` (2C)    | 32,461 | 105 | 268 | ~947 |
| `taxsim-cf-convnw` (2N)  | 32,146 |  76 | 190 | ~679 |
| `taxsim-bathtub` (2B)    |  1,700 | 167 | 924 | ~79 |
| `taxsim-postproc` (3b)   |  1,717 | 136 | 1,538 | ~65 |
| `taxsim-wealth` (2W)     |  1,510 | 166 | 546 | ~70 |
| `taxsim-baseline` (1)    |  1,388 |  76 | 140 | ~29 |
| `taxsim-frozen` (1B)     |  1,974 |  67 | 331 | ~37 |
| `taxsim-agg` (3a)        |  1,617 |  40 |  82 | ~18 |

**~92% of the model's CPU budget is inside per-scenario-year worker tasks**
(phases 1, 2A, 2C, 2N). Post-processing, aggregation and the bathtub pre-passes
together are under 10%. Any optimization effort that does not touch the year
worker is optimizing the wrong thing.

### 1.2 Within-task profile

A single full-sample baseline year-task was re-run under `Rprof` on a compute
node against a freshly built staging dir, with output redirected to scratch.

- runscript: `config/runscripts/tests/perf_probe.csv` (baseline + one
  `top_tax/dials` scenario, `years = 2026:2027`)
- 221,831 records, `pct_sample = 1`, 89 `detail_vars`, 10 `mtr_vars`
  (`wages1 wages2 part_active sole_prop1 scorp_active kg_lt rent char_cash
  net_worth estate`, all `nextdollar`)
- `pass_type = 'both'`, `Rprof` interval 20ms
- harness: `/nfs/roberts/scratch/pi_nrs36/jar335/perf_probe/probe2.R`,
  `probe2.sbatch`
- raw output: `/nfs/roberts/scratch/pi_nrs36/jar335/perf_probe/probe2.log`,
  `summary2.rds`
- peak RSS 4.0 GB single-threaded (`sacct` `MaxRSS`, job 19600018)

`run_one_year` elapsed 53.2s. Inclusive times:

| region | inclusive s | share |
|---|---:|---:|
| `calc_mtrs` (10-var loop) | 39.36 | **75.0%** |
| `calc_qbi_ded` (summed over all calculator passes) | 14.70 | **28.0%** |
| `do_taxes` (all invocations) | 44.06 | 83.9% |
| `do_1040` (all invocations) | 34.80 | 66.2% |
| `left_join` (tax law, random numbers, MTR re-joins) | 7.12 | 13.6% |
| `do_payroll_taxes` | 5.94 | 11.3% |
| `read_microdata` | 1.46 | 2.8% |
| `write_csv` / `vroom_write_` (detail) | 1.40 | 2.7% |
| `calc_estate` | 0.76 | 1.5% |
| `calc_estate_mortality` | 0.72 | 1.4% |
| `derive_vars` | 0.22 | 0.4% |
| `parse_calc_fn_input` (all calls) | 0.04 | 0.1% |

Top self-time entries — note that the top of this list is dataframe *machinery*,
not tax arithmetic:

| function | self s | self % |
|---|---:|---:|
| `vec_slice` | 6.34 | 12.1% |
| `vec_group_id` | 4.50 | 8.6% |
| `.Call` | 3.78 | 7.2% |
| `pmax` | 3.72 | 7.1% |
| `pmin` | 1.72 | 3.3% |
| `vec_duplicate_any` | 1.60 | 3.1% |
| `vroom_write_` | 1.40 | 2.7% |
| `vec_case_when` | 1.38 | 2.6% |
| `list_unchop` | 1.34 | 2.6% |
| `vec_unique_loc` | 1.10 | 2.1% |

Two conclusions worth stating plainly:

1. **I/O is not a bottleneck inside the workers.** Reading Tax-Data and writing
   detail together are 5.5% of a year-task. Switching detail to parquet would be
   a storage and post-processing win, not a worker win.
2. **`parse_calc_fn_input` is free** (0.1%). The input-validation convention
   costs nothing; leave it alone.

---

## 2. Findings, ranked by payoff per unit of risk

### 2.1 `calc_qbi_ded` reshape round-trip — 28% of total runtime — FIXED 2026-07-25

**Status: done.** Measured outcome, paired A/B in a single process on one node
(`/nfs/roberts/scratch/pi_nrs36/jar335/perf_probe/qbi_ab_timing.R`):

| | old (pivot) | new (wide) |
|---|---:|---:|
| the function alone, per call | 2.59s | 0.21s |
| a full year-task (`pass_type = 'both'`) | 82.3s | 58.0s |

**24.3s saved per year-task, 29.6%**, reproducible across rounds
(82.5 / 82.2 vs 57.2 / 58.7). 11 `calc_qbi_ded` calls per year-task. Verified
bit-identical (see the verification note at the end of this section).

Text below describes the problem as found.


`src/calc/functions/deductions/qbi_ded.R:85-137`.

The function pivots the entire frame **long** over four business types
(`sole_prop`, `part`, `scorp`, `farm`), i.e. 221,831 → 887,324 rows, does the
per-business arithmetic, pivots **wide** again, and `left_join`s the result back
onto the main frame by `id`:

```
pivot_longer(names_to = c('series', 'business_type'), names_sep = '[.]')  # 4x rows
  -> pivot_wider(names_from = series)
  -> mutate(...)                     # the actual QBI math
  -> pivot_wider(names_from = business_type)
  -> left_join(by = 'id')
```

This runs **once per calculator pass**, i.e. ~11-14 times per year-task (1 real
pass + 10 MTR recomputes + kg deemed/law-only legs where applicable). At 14.70s
of a 53.2s task it is the largest single line item in the model, and the
`vec_slice` / `vec_group_id` / `vec_duplicate_any` / `list_unchop` /
`vec_unique_loc` self-time at the top of the profile is predominantly this
reshape plus its rejoin.

The math itself is per-business and embarrassingly wide-form: four independent
column groups with no cross-business dependency until the final
`qbi_ded.sole_prop + qbi_ded.part + qbi_ded.scorp + qbi_ded.farm` sum. Nothing
requires the long representation. Computing it in wide form over the four
business types removes the row explosion, both pivots, and the join.

Realized saving: **29.6% of a year-task**, with no parallelism and no change in
results.

A related observation worth recording: the frame at the `calc_qbi_ded` call site
is **594 columns wide**, so at 221,831 rows every full-frame copy there moves
roughly 1 GB. QBI was the worst instance, but the whole `do_1040` chain is a
sequence of `bind_cols(calc_x(.))` calls on a frame of that width, and each one
copies it. That is the structural reason `vec_slice` sits at the top of the
self-time table. Reducing the number of full-frame copies in the chain is a
plausible next lever, separate from anything in §2.2-2.7.

**How it was verified bit-identical.** The old implementation was kept verbatim
(`/nfs/roberts/scratch/pi_nrs36/jar335/perf_probe/qbi_old.R`) and
`qbi_diff_check.R` ran both side by side:

- **22 live calls** across two real full-sample year-tasks (221,831 records),
  covering the main pass and all 10 MTR-perturbed frames, asserting
  `identical()` on every call.
- **28 law and edge variants** on a captured real frame: `qbi.po_type = 1` (the
  OBBB branch no shipped reform activates), `min_value` 0 / 400 / 1000,
  `wage_exception` on and off, `po_range = 0` (division by zero),
  `po_thresh = 0`, `rate = 0`, `wage_limit = 0`, `txbl_inc_limit` 0 and 1,
  all-NA `sstb`/`wagebill`, all-TRUE and all-FALSE `sstb`, zero / negative / NA
  business income, high income with zero wagebill, and taxable income mid-
  phaseout — each crossed with both `po_type` branches.

All passed. Plus a full pre/post `main.R` byte-diff on `tests/simplify_smoke`.

### 2.2 The MTR loop is 75% of a year-task and is trivially parallel

`src/sim/run.R:645` (static pass) and `src/sim/run.R:995` (conventional pass)
map `calc_mtrs` over `mtr_vars` with a serial `map2`. Each iteration is a full
independent `do_taxes` recompute on a perturbed copy of the frame.

Purity was verified, not assumed:

- no `runif` / `rnorm` / `sample(` / `set.seed` anywhere under `src/calc/`
- no `<<-` anywhere under `src/calc/`
- random draws are pre-materialized columns joined by `id`
  (`globals$random_numbers`, `r.salt_workaround`), so a fork sees identical
  draws

Therefore `mclapply` over the loop is **output-identical**, not merely
approximately so.

Memory is the binding constraint: peak RSS is 4.0 GB single-threaded, and each
fork's `mutate` chain allocates its own copies. 4-way forking wants the current
`--mem=16G` raised to ~32G; 8-way wants ~48-64G.

Important caveat: this reduces **wall clock per task, not CPU-hours**. It pays
off to the degree the arrays are not already saturating the core allocation the
partition will give them. It is most valuable for the long-tail tasks
(`cf-static` max 329s) and for interactive/iterative single-scenario work, and it
composes with §2.3 rather than substituting for it.

### 2.3 MTRs are computed for variables nobody reads

All 10 `mtr_vars` are computed for every scenario-year on both the static and
conventional passes, regardless of which behavior modules a given scenario
actually registers. Each unused variable costs roughly 7% of a year-task
(39.4s / 10 vars / 53.2s), times two passes, times every year.

What the modules actually reference (`grep -ho 'mtr_[a-z0-9_]*'` over
`config/scenarios/behavior/`): `mtr_net_worth`, `mtr_kg_lt`, `mtr_estate`,
`mtr_estate_ded`, `mtr_part_active`, `mtr_sole_prop1`, `mtr_rent`,
`mtr_scorp_active`, `mtr_wages1`, `mtr_wages2`, `mtr_char_cash`, plus
tips/OT/auto variants in modules not used by the top-tax runscripts.

The needed set could be derived per scenario from its registered behavior
modules, keeping the existing guaranteed kg/wealth `net_worth` / `estate`
fallbacks and the union requirement on the baseline leg (the baseline cannot know
which scenarios will consume its detail, and `read_mtr` in `kg_dynamics.R`
already hard-stops when the baseline is missing `estate`). Downstream readers of
`mtr_*` columns in detail (distribution, diagnostics) need auditing before this
is safe.

### 2.4 SLURM DAG uses global array barriers, so every phase runs at the speed of the slowest scenario

`slurm_run.sh` chains phase-level array dependencies: `--dependency=afterok:$P2A`
gates the *entire* 2B array on the *entire* 2A array, and so on through
2A → 2B → 2N → 2W → 2C. Consequences:

- one straggler scenario-year stalls every other scenario's next phase
- 2N (wealth-only scenarios) waits on the whole 2B array even when the scenario
  has no kg bathtub at all
- with heterogeneous scenarios in one runscript, total wall clock is
  `sum over phases of (slowest task in that phase)`, not
  `slowest scenario's own chain`

Fix directions: submit per-scenario dependency chains instead of per-phase
arrays, or use `--dependency=aftercorr` where the two manifests align 1:1 by
array index. Pure wall-clock win, no extra CPU. Note 2B/2W are per-scenario while
2A/2C/2N are per-scenario-year, so `aftercorr` does not apply to those edges
without a manifest change.

There is one deliberate serialization to preserve: Phase 3a runs the baseline
aggregation before the counterfactual aggregations on purpose, because scenario
receipts read the baseline's `totals/estate.csv` and racing them makes receipts
fall back to rebuilding the series from 15-significant-digit detail CSVs, which
drifts against `main.R`. Don't "fix" that one.

### 2.5 Phase 3b: the distribution microdata is built three times where two are identical

`build_distribution_microdata(id, baseline_id, yr, other_taxes, ...)` is called:

- once from `process_for_distribution` (`src/data/post_processing/distribution.R:427`),
  static leg, `write_supplemental = TRUE`
- twice from `process_for_etrs` (`src/data/post_processing/distribution_etrs.R:133`),
  once per `reform_leg` in `c('static', 'conventional')`, `write_supplemental = FALSE`

The first and the static-leg third differ only in the `write_supplemental` side
effect. Each build reads both legs' 150MB detail, enforces the record universe,
runs the estate heir rank-matching allocator, applies the heir copy-split, and
attaches the corp stock allocation bases — so this is the expensive part of
post-processing. Memoizing per `(id, yr, reform_leg)` removes a third of it.

Additionally, everything in Phase 3b is serial inside one job (observed max
1,538s): seven independent products (`build_1040_report`, `calc_rev_est`,
`build_distribution_tables`, `build_distribution_etrs`,
`build_timeburden_table`, `build_horizontal_table`, `kg_dyn_build_summary`), and
inside the distribution builders, serial loops over `dist_years` and over the 21
`dist_cuts`. Candidates for an array over (scenario × product) or an `mclapply`.

### 2.6 Phase 0 setup is serial on the login node

`src/slurm/setup.R:105` loops over scenarios building `vat_price_offset`,
`excess_growth_offset`, `indexes` and `build_tax_law` one at a time before any
job is submitted. Measured at roughly 30s per scenario in the probe. A
30-scenario runscript therefore spends ~15 minutes of serial login-node time as a
prologue. `mclapply` over scenarios, or promote it to a Phase 0 array job.

### 2.7 Full-file `fread()` where a handful of columns are needed

Detail files are 98 columns / 150MB per scenario-year. Unselected reads:

| site | columns actually needed |
|---|---|
| `src/sim/wealth_dynamics.R:995` (`wealth_dyn_read_convnw_detail`) | 16 of 98 |
| `src/sim/wealth_dynamics.R:1030` (`wealth_dyn_read_baseline_detail`) | 3-5 of 98 |
| `src/sim/wealth_dynamics.R:267` | check |
| `src/sim/run.R:564` | check |
| `src/slurm/setup.R:162` (baseline MTR prebuild) | `id`, `year`, `mtr_*` |
| `src/data/post_processing/distribution.R:287` | check |

Adding `fread(select = ...)` is a ~6x parse reduction at those sites, which land
mostly in the 2W and 2B jobs (~150 CPU-hours combined) and in Phase 0.

### 2.8 Ideas considered and their status

- **Stack all 10 MTR perturbations into one 10N-row frame and call `do_taxes`
  once.** The profile shows a large share of cost is per-call dataframe machinery
  rather than arithmetic, so this would cut CPU-hours, not just wall clock,
  unlike §2.2. Blockers are surmountable (random draws are `id`-keyed, calc chain
  is pure), but it is invasive and interacts with the frame conventions
  documented in `calc_mtrs` (`baseline_pr_er` pre/post-frame rescale trap).
  Worth doing only after §2.1, and only under the smoke-diff harness.
- **Cache the prepared input frame across the static / convnw / conv passes.**
  The tax-law join, SALT workaround, SS COLA, capital adjustment and excess
  growth steps are recomputed identically on each pass. But `left_join` is 13.6%
  total and much of that is the MTR re-joins, so the recoverable share is small,
  and a 150MB rds round-trip may cost as much as recomputing. Low priority.
- **Detail in parquet instead of CSV.** 2.7% of a worker, so not a worker win.
  But it is 1.1 TB per vintage (measured on `top_tax_dials_v2`) and it compounds
  with §2.7 for the bathtub and post-processing readers. Storage-driven, not
  CPU-driven.
- **Analytic instead of brute-force MTRs.** Rejected. The +$1 recompute is exact
  and that exactness is a core property of the model.
- **BLAS threading / `-c 1`.** Nothing to gain; there is no linear algebra in the
  hot path.

---

## 3. Recommended order

1. ~~§2.1 QBI reshape~~ — **DONE 2026-07-25, 29.6% per year-task, bit-identical.**
2. §2.7 selective `fread` + §2.5 memoize the distribution microdata — small,
   safe, pure waste elimination.
3. §2.4 per-scenario DAG chains — wall clock only, no CPU cost, no results risk.
4. §2.2 fork the MTR loop — needs a `--mem` bump; wall clock only.
5. §2.3 prune unused `mtr_vars` — largest remaining CPU win but needs an audit of
   every downstream `mtr_*` reader first.
6. §2.6 parallelize Phase 0, §2.5 fan out Phase 3b products.
7. §2.8 MTR frame-stacking, only if §2.1 through §2.5 prove insufficient.

---

## 4. Reproducing the measurements

```bash
# cluster-level accounting
sacct -u $USER --starttime=2026-07-05 --format=JobName%22,ElapsedRaw,State -X -n -P

# within-task profile (writes probe2.log + summary2.rds)
sbatch /nfs/roberts/scratch/pi_nrs36/jar335/perf_probe/probe2.sbatch
```

`probe2.sbatch` runs `src/slurm/setup.R` on `tests/perf_probe` to build a fresh
staging dir (a staging dir from an older vintage will not work — the serialized
tax law predates current calculator requirements and fails on
`pref.no_ord_cap`), then profiles one baseline year-task with output redirected
to scratch so no production vintage is touched.

### The smoke-diff harness recipe is stale

`other/simplify_cleanup/run_smoke_{pre,post}.sh` run `tests/simplify_smoke` at
`pct_sample = 0.05`. That no longer works: the wealth-dynamics channel is now
applied model-wide by default (the calibrated `default` financing profile), and
`wealth_dyn_check_run_compat()` hard-stops on `pct_sample != 1`. Both legs fail
identically with "wealth_dynamics (s > 0) requires pct_sample = 1".

Either run the smoke at `pct_sample = 1` (what this audit did — a full-sample
3-scenario / 3-year smoke takes roughly 30-50 minutes per leg) or add
`wealth_financing = none` to `config/runscripts/tests/simplify_smoke.csv`, which
is faster but stops exercising the wealth code paths. The scripts in
`other/simplify_cleanup/` should be updated either way so the next person does
not lose a cycle to it.

### Misc

Note: `config/runscripts/tests/perf_probe.csv` was overwritten during this audit;
it previously existed untracked from an earlier profiling session and its prior
contents are not recoverable. Current contents are baseline + one
`top_tax/dials` scenario at `2026:2027`, with the `dep.Off-Model-Estimates.*`
columns removed (the pinned `20250925` vintage no longer exists under OME v5).
