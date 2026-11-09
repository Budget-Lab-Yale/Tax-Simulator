# Years per task

`slurm_run.sh` takes an optional tenth argument, `years_per_task`, defaulting to
1. It is how many consecutive years one array task of a per-year phase runs.

Every year-task is its own R process and pays a fixed toll before doing any
work: start R, source every file under `src/`, load the packages, read `globals`
and `return_vars`. Measured on the 2026-07-29 eta grid, a year-task averaged
about 1.4 minutes on the mechanical rung and 0.9 on the no-wealth passes, and the
difference between a pass computing two MTR blocks and one computing none was
about 0.5 minutes. Most of a task's runtime is the toll rather than the
calculation, so running several years in one process pays it once per batch.

Two limits. The saving is the R startup alone: the microdata read, the
calculation and the detail write scale with years and do not shrink. And if
fair-share ever granted the full partition, batching would lengthen the critical
path rather than shorten it, because each barrier would wait on a longer task.
It is a win while we queue.

Only the per-year phases batch: 1, 2A, 2MN, 2M, 2N and 2C. The pre-passes (1B,
2B, 2MW, 2W) run their recurrence over all years in one job already.

`years_per_task` does not change results, so it is not in the vintage manifest,
which records what a run assumed rather than how it was scheduled.

## What batching shares between years

A virgin R process per year made every memoized cache implicitly per-year.
Batching makes them per-batch, so a cache keyed on scenario while holding
year-specific content would turn an invisible bug into wrong numbers for the
second and later years of a batch. Every `new.env()` and `<<-` under `src/`:

| state | key | year-invariant |
|---|---|---|
| `.tax_law_cache` (`data/tax_law.R`) | config path plus file mtimes for the raw load; `list(years, indexes)` for the parse | yes, years are in the key |
| `.calib_cache` (`misc/calibrations.R`) | normalized file path | yes |
| `.scenario_config_active` (`misc/scenario_config.R`) | the active legs, plus which pass is running | yes; `run_one_year` clears the pass label on exit |
| `.run_state` (`misc/scenario_config.R`) | name, run-scoped | yes, and nothing outside the file writes it |
| `.payroll_cache` (`sim/payroll.R`) | the reform and baseline tax law layer IDs | yes |
| `.wealth_profile_cache` (`sim/wealth_dynamics.R`) | profile kind and value, age range, bin count | yes |
| `.corp_cache` (`sim/corp/paths.R`) | scenario ID, for the gate and the analytic paths | yes, the paths span the horizon |
| `globals$estate_params` (`sim/run.R`) | assigned from the Tax-Data path at the top of each year | rewritten every year |
| `results` push (`post_processing/distribution_etrs.R`) | function-local accumulator | not shared |

Nothing needed its key fixed or a clear at the top of the loop.

## The random seed

`reconstitute_environment` seeds once per process. On the `run_one_year` path the
only draws are in `behavior/employment/bastian.R`, which calls
`set.seed(globals$random_seed)` itself; the `r.*` columns are drawn in
`parse_globals` and arrive through `globals`. So a second year in a batch would
not in fact diverge. The worker reseeds at the top of each year anyway, which
makes the RNG state at each `run_one_year` entry the state it has today in a
process of its own, rather than a claim about which code draws.

## The measured split

`config/runscripts/tests/mech_smoke.csv` at full sample, batch mode, run twice at
1 and 2 years per task on 2026-07-29 (vintages `ypt1` and `ypt2` under the local
root). Outputs were byte-identical across 272 files and 25 workbooks. Total
year-task seconds by phase, from `sacct`:

| phase | tasks at 1 | total | tasks at 2 | total |
|---|---|---|---|---|
| 1 baseline | 3 | 127s | 2 | 98s |
| 2A static | 15 | 715s | 10 | 518s |
| 2MN mech-no-wealth | 6 | 305s | 4 | 239s |
| 2M mechanical | 9 | 491s | 6 | 382s |
| 2N conv-no-wealth | 6 | 320s | 4 | 227s |
| 2C conventional | 15 | 678s | 10 | 497s |
| all | 54 | 2636s | 36 | 1961s |

A quarter off total task-seconds. Solving the two totals per phase for a fixed
cost F paid once per task and a per-year cost V:

    Y = 3 years, so   Y(F + V) = total at 1
                      2F + YV  = total at 2

gives F between 29 and 39 seconds across the six phases, and V between 8 and 18.
The fixed toll is three quarters of a year-task on this runscript, which is the
premise the change rests on and is now measured rather than differenced.

The saving at batch size k is F/(F + V) times the fraction of tasks the batching
removes. Here Y = 3 and k = 2 removes a third of the tasks, so the ceiling was
26 percent and 25.6 is what came out. On a 31-year grid, k = 2 removes 48 percent
of tasks and k = 4 removes 74 percent. Grid year-tasks ran nearer 84 seconds than
48, so V there is larger and F/(F + V) closer to 0.4: expect about 20 percent off
at k = 2 and 30 at k = 4. Those are task-seconds, not wall clock, which in the
throughput-limited regime track each other and under full concurrency do not.

## Walltime and memory

The year phases ask for `30 min × years_per_task`; at 1 that formats to the same
half hour they have always asked for. Memory stays at 24G because the worker
frees the year's result and MTR frames and runs `gc()` before the next year reads
its own, so peak is one year's and not the batch's.

## The kg path

`config/runscripts/tests/mech_kg_smoke.csv`, full sample, batch mode, at one year
per task and at two: byte-identical over 167 files and 13 workbooks, every task
COMPLETED in both, Phase 1 shrinking from four tasks to two. This is the path
worth checking separately, because the gains recurrence runs sequentially over
years and Phase 1B caches its inputs to a file that a batched task now reads
once rather than per year.

Runs `kg_ab_k1` and `kg_ab_k2`, launched by
other/performance/run_kg_batch_ab.sbatch.
