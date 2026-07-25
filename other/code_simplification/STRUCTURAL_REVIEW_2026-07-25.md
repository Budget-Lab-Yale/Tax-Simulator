# Structural simplification review — 2026-07-25

_Branch `wealth`. Successor to `other/simplify_review_codebase_2026-07-01.md`
(the "codebase sweep") and `other/simplify_review_src_wealth_2026-06-22.md`._

**Scope and posture.** The two prior reviews were line-level sweeps: they hunted
duplicated blocks and applied them in verified byte-identical batches. This one
is deliberately different. It asks why the codebase keeps growing and what
structural changes would slow that down, rather than cataloguing every
extractable helper. Where a finding overlaps an open item from the July 1 review
that is noted inline.

Everything below is intended to be behavior-preserving. Nothing here is a bug
report.

---

## The shape of it

`src/` is 21,161 lines of R across 60 files. That is not a large codebase. The
problem is distribution: four files hold a third of it, and those four are the
ones under active development.

| File | Total | Comment | Blank | Code | Comment share |
|---|---|---|---|---|---|
| `src/sim/kg_dynamics.R` | 3,645 | 1,214 | 450 | 1,981 | 33% |
| `src/sim/corp_incidence.R` | 1,590 | 564 | 216 | 810 | 35% |
| `src/sim/run.R` | 1,300 | 481 | 162 | 657 | 37% |
| `src/sim/wealth_dynamics.R` | 1,172 | 435 | 178 | 559 | 37% |
| `src/calc/do_taxes.R` | 922 | 366 | 145 | 411 | 40% |
| `src/data/post_processing/distribution.R` | 706 | 243 | 95 | 368 | 34% |
| `src/data/post_processing/revenue.R` | 702 | 233 | 108 | 361 | 33% |

Churn confirms the concentration. Over the last 200 commits: `kg_dynamics.R` 39
touches, `run.R` 29, `distribution.R` 15, `config_parser.R` 12.

Three distinct forces are at work, and it is worth separating them because they
call for different remedies.

**1. Provenance narrative lives in source.** A third of the two biggest channel
files is comment, and much of it is calibration history rather than mechanism —
superseded spec versions, pin dates, grid values, the reasoning for one
functional form over another, pointers to fit CSVs. Example, `kg_dynamics.R`
lines 195–226: thirty lines of prose to set two numbers. This is genuinely
valuable material and it should not be deleted, but source files are the wrong
container for it. It is the main reason the files feel unmanageable, and it is
also the cheapest thing to fix.

**2. Four channels, each hand-wired.** kg_dynamics, wealth_dynamics,
corp_incidence, and sigma_conversion (plus the smaller corp_rate) all implement
the same lifecycle independently. Adding the fifth cost edits in five files.
This is the force that actually compounds.

**3. Real duplication in the orchestrator.** `run_one_year()` is 670 lines
(`src/sim/run.R:433–1098`) with a static body and a conventional body that do
nearly the same five things. This is ordinary refactoring debt, mechanical to
pay down.

---

## Tier 1 — do these first

### 1. Collapse the static/conventional duplication in `run_one_year()`

Highest value per unit of risk, and it unblocks item 2.

**Current state.** Four distinct duplications inside one function:

- **The MTR block.** Six `calc_mtrs()` call sites in `run.R` — lines 644, 712,
  738, 774 (static side) and 966, 994 (conventional side). Every one repeats
  `select(-all_of(return_vars %>% unlist() %>% set_names(NULL)))`; four of the
  six repeat the same six-argument actuals bundle (`actual_liab_iit`,
  `actual_liab_pr`, `actual_liab_wealth`, `actual_liab_estate`,
  `actual_estate_p_dsue`, `baseline_pr_er`). The DSUE blend that feeds
  `actual_liab_estate` is computed verbatim twice, at 640 and 990.
- **Totals assembly.** The identical five-element list (`pr`, `1040`,
  `1040_by_agi`, `estate`, `wealth`) is built at 818 and 1064.
- **Detail write.** Two `select(all_of(globals$detail_vars), starts_with('mtr_'),
  any_of(...))` blocks at 810 and 1052. The `any_of()` list on the conventional
  side has accreted to thirteen columns and is edited by every new channel.
- **The deemed fold.** The same four-line liability fold at 802 and 1039.

**Change.** Add four helpers, ideally in `run.R` next to the existing
`write_pass_outputs()` (which the July 1 review already extracted for the same
reason):

```r
strip_calc_vars = function(df, drop_mtrs = FALSE)   # the select(-all_of(...)) idiom
mtr_actuals     = function(taxed)                   # named list incl. the DSUE blend
run_mtr_block   = function(taxed, scenario_info, year, baseline_pr_er = NULL)
collect_totals  = function(taxed, year)
write_detail    = function(taxed, path)             # OPTIONAL_DETAIL_COLS constant
fold_deemed     = function(taxed, liab_deemed)
```

`run_mtr_block()` absorbs the `map2()` over `mtr_vars`/`mtr_types`, the
`bind_cols()`/`relocate()` shaping, the join back onto the frame, and the
`mtr_estate_ded` derivation — all of which appear on both sides.

**Care required.** The `baseline_pr_er` frame convention is the trap here. Per
`calc_mtrs()`'s parameter doc (`do_taxes.R:566–589`) and the memory note on
`calc_mtrs_pr_er_bug`, post-`do_taxes` frames must pass `NULL` and pre-`do_taxes`
frames must thread `baseline_pr_er`. Five of the six sites are post-frame
(`NULL`); the conv-no-wealth `mtr_net_worth` call at 966 is pre-frame and threads
it. Make this an explicit argument of the helper, not a default, so the
asymmetry stays visible.

The three "guaranteed fallback" MTR blocks (`mtr_kg_lt_lawonly` at 712,
`mtr_net_worth` at 738, `mtr_estate` at 774) are single-variable calls on
purpose-built frames; they can share `strip_calc_vars()` and `mtr_actuals()` but
should stay separate calls rather than being folded into the generic loop.

**Payoff.** Roughly 200 lines to 60 in the MTR block alone; `run_one_year()` from
about 670 lines to about 300. This is the July 1 review's open item #7
(`calc_scenario_mtrs` closure) and the 2026-06-22 review's open #2 (deemed
dead-leg extraction), the latter now partly done via
`kg_dyn_recompute_deemed_tax()`.

**Verification.** `tests/simplify_smoke` pre/post byte-diff, per the harness
described in `other/simplify_cleanup/`. Add a kg + wealth scenario to the smoke
runscript if one is not already there, so all six MTR sites are exercised.

### 2. Make the channel lifecycle declarative

This is the change that alters the growth rate rather than the current line
count. It supersedes and generalizes the July 1 review's open item #2 ("extract
the shared channel guard rails, kg ↔ wealth"), which was scoped to two channels
before corp_incidence and sigma landed.

**Current state.** Each channel independently supplies:

| Concern | kg_dynamics | wealth_dynamics | corp_incidence | sigma_conversion |
|---|---|---|---|---|
| activation | `scenario_uses_kg_dynamics()` | `scenario_uses_wealth_dynamics()` | `scenario_uses_corp_incidence()` | `scenario_uses_sigma()` |
| run compat | `kg_dyn_check_run_compat()` (`run.R:1197`) | `wealth_dyn_check_run_compat()` | `corp_check_run_compat()` | — |
| calibration staleness | `kg_dyn_check_calibration_provenance()` | `wealth_dyn_check_provenance()` | — | — |
| pre-pass | `run_frozen_pass()` + `run_bathtub_pass()` | `run_wealth_bathtub_pass()` | analytic (`corp_resolve_paths`) | rides kg bathtub |
| record applier | `kg_dyn_apply_to_records()` (in module) | `wealth_dyn_apply_to_records()` | `corp_apply_to_records()` | `sigma_apply_conversions()` |
| state I/O | `kg_dyn_state_{dir,path}` ×2 | via `cohort_state_path()` | none | in kg state |

The three `*_check_run_compat()` functions assert the same three preconditions —
`pct_sample == 1`, no VAT, no excess growth — with different prose. Compare
`run.R:1220–1247` against the corresponding blocks in `wealth_dynamics.R:497`
and `corp_incidence.R:516`. Three copies of one rule is a drift hazard: a fourth
precondition (say, a Tax-Data vintage floor) has to be added three times or it
silently applies to some channels only.

**Change, step 1 (small, do it with item 1).** One function:

```r
check_raw_data_channel_compat = function(channel, scenario_info,
                                         vat_price_offset, excess_growth_offset)
```

taking the channel name for the message and hard-stopping on the three shared
conditions. Each channel then adds only its own extra checks (kg's `mtr_vars`
must contain `kg_lt`; wealth's provenance stamp). Saves roughly 90 lines and
removes the three-way drift.

**Change, step 2 (design pass first).** A channel registry — a list of records,
one per channel:

```r
CHANNELS = list(
  kg_dynamics = list(
    uses    = scenario_uses_kg_dynamics,
    compat  = kg_dyn_check_run_compat,
    prepass = list(frozen = run_frozen_pass, bathtub = run_bathtub_pass),
    apply   = NULL,          # applied inside the behavior module
    order   = 20
  ),
  corp_incidence = list(uses = scenario_uses_corp_incidence, ..., order = 10),
  wealth_dynamics = list(uses = scenario_uses_wealth_dynamics, ..., order = 30)
)
```

Two call sites then become loops instead of hardcoded branch trees:

- `do_scenario()` (`run.R:65–151`). Today: two booleans, a nested comment block
  explaining the four combinations, and a hand-sequenced chain. With a registry:
  resolve the active set, run pre-passes in declared order, run the passes the
  active set requires.
- `run_one_year()` (`run.R:579–584`, `850–919`). Today: three
  `uses_X = ID != 'baseline' && scenario_uses_X(...)` lines and a hardcoded
  applier sequence (corp, then wealth haircut, then behavior, then corp kg
  quantity) with the ordering rationale in comments. With a registry: iterate
  appliers by `order`, and keep the ordering rationale in one place next to the
  order numbers.

**Payoff.** Modest in lines (maybe 80). Large in marginal cost: adding a channel
becomes one registry entry plus its own file, instead of edits to `do_scenario`,
`run_one_year`, both compat paths, `setup.R`, and `worker.R`.

**Care required.** The applier ordering is load-bearing and subtle — corp before
the wealth haircut, the haircut before behavior, corp's kg quantity margin after
`kg_dyn_apply_to_records`. Encode those as explicit order numbers with the
reasoning attached, and assert the resolved order in a unit test so a future
registry edit cannot silently reorder them. Do not attempt this at the same time
as item 1.

### 3. Split `kg_dynamics.R` into a directory

3,645 lines in one file, 39 touches in 200 commits. The seams are already visible
in the function inventory:

| New file | Functions (current line ranges) |
|---|---|
| `kg/constants.R` | header + constants + `kg_dyn_active_{eta,timeable_share}` (1–235) |
| `kg/inputs.R` | `kg_dyn_load_*`, `kg_dyn_build_{heir_matrix,extended_grid}`, `kg_dyn_pack_*`, `kg_dyn_attach_record_attrs`, `kg_dyn_aggregate_cells` (381–960, 2209–2543) |
| `kg/bellman.R` | `kg_dyn_bellman_sweep_age`, `kg_dyn_solve_bellman` (961–1279) |
| `kg/timing.R` | `kg_dyn_validate_timing_params`, `kg_dyn_build_planned_timing`, `kg_dyn_build_scenario_rate` (1280–1426) |
| `kg/recurrence.R` | `kg_dyn_cell_m_eff`, `kg_dyn_step_recurrence`, `kg_dyn_build_regime_mix`, `kg_dyn_build_cell_table`, `kg_dyn_run_{bathtub,frozen}_pass` (1427–1569, 1861–1971, 2607–3299) |
| `kg/tau_eq.R` | `kg_dyn_tau_eq_*`, `kg_dyn_check_tau_eq`, `kg_dyn_carry_slack` (1570–1860) |
| `kg/apply.R` | `kg_dyn_apply_to_records`, `kg_dyn_apply_mech_to_records`, `kg_dyn_aggregate_cell_{mtr,carry,estate}` (1972–2100, 3300–3467) |
| `kg/state.R` | `kg_dyn_state_*`, `kg_dyn_mech_state_*`, `kg_dyn_inputs_cache_path`, `scenario_uses_kg_dynamics`, `kg_dyn_resolve_year_regime` (2101–2208) |
| `kg/diag.R` | `kg_dyn_write_estate_exposure_diag`, `kg_dyn_build_summary`, `kg_dyn_wealth_law_active` (2544–2606, 3468–3645) |

**Cost: near zero.** `main.R:22` sources `./src` recursively and
`src/slurm/common.R`'s `reconstitute_environment()` does the same, so no manifest
or loader changes are needed. It is a `git mv` plus a header comment per file.
Do it as a pure move with no edits, so the diff is reviewable as a rename.

Apply the same treatment to `corp_incidence.R` (1,590 lines): `corp/paths.R`
(wedge reading, meta, macro, path construction, 169–1087), `corp/apply.R`
(record applier, kg glue, 1088–1490), `corp/diag.R` (conservation diagnostic,
self-check).

**Note.** The July 1 review's open item #3 (collapse the SLURM pre-pass driver
triple: `frozen.R`, `bathtub.R`, `wealth.R` are near-identical 65-line drivers)
sits naturally alongside this and should be done in the same pass.

### 4. Move calibration provenance out of source; move calibrated scalars into config

**Current state.** 21 `Sys.getenv()` knobs across five files, most in the idiom

```r
KG_DYN_DEFAULT_ETA = local({ v = Sys.getenv('KG_ETA', '2.4825')
                             if (identical(v, 'NA')) NA_real_ else as.numeric(v) })
```

preceded by 20–30 lines of provenance. The full set, by file: `kg_dynamics.R`
(`KG_RESPONSE_FORM`, `KG_ETA`, `KG_ETA_LOGS`, `KG_TIMEABLE_SHARE`,
`KG_TIMEABLE_SHARE_LOGS`, `KG_DEEMED_AVOIDANCE`, `KG_APPLIER_ALLOCATION`,
`KG_WEALTH_CARRY_SCALE`, `KG_STRICT_CALIB`), `corp_incidence.R` (`CORP_SIGMA_N`,
`CORP_KAPPA`, `CORP_PRICED_AS_PERMANENT`), `corp_rate.R` (`CORP_RATE_ETI`),
`sigma_conversion.R` (`SIGMA_CONV`, `SIGMA_RECORD_DUMP`, `SIGMA_TAU_EQ_FDCHECK`),
`wealth_dynamics.R` (`WEALTH_STRICT_CALIB`), `distribution_etrs.R`
(`DIST_HOUSING_STRUCTURE_SHARE`).

Two problems beyond volume. A run's effective calibration is scattered across
five R files plus the environment, so there is no single place to read or record
it — the calibration-watch machinery (`calibration_reference.csv`, the pre-push
hook) exists precisely because this is hard to see. And env-var overrides do not
appear in any output artifact, so a swept corner is invisible in the vintage.

**Change.** Two separable pieces.

*(a) Values into config.* `config/calibration/{kg,corp,sigma,wealth,dist}.yaml`,
each entry carrying `value`, `pinned` (date), `memo` (path), and optional `env`
(override variable name). One reader:

```r
calib_param = function(key)   # 'kg.eta' -> value, honoring the declared env override
```

Write the fully resolved parameter set to `{output_root}/calibration.csv` at
setup, next to `dependencies.csv` and `behavioral_assumptions.csv`. That closes
the invisible-override gap and gives the staleness watch a single input.

*(b) Narrative into memos.* Reduce each source comment to the number, the pin
date, and a pointer. The material is already written up in
`other/kg_model_tests/`, `other/top_tax/eta_dial/`, `other/wealth_dynamics/`, so
this is mostly deletion plus a link. Where a memo does not exist for a
constant's story, create it before deleting.

**Payoff.** About 600 lines out of the two largest channel files, no information
lost. Also removes the `local({...as.numeric...})` boilerplate at 21 sites.

**Care required.** `KG_RESPONSE_FORM` is validated at load and must keep its
hard-stop on an invalid value. Several constants intentionally default to `NA`
so an uncalibrated sim fails at the Bellman guard rather than running — the
reader must preserve `NA` semantics rather than coercing to a default.

---

## Tier 2 — worth doing

### 5. Make the SLURM phase table declarative

**Current state.** The phase set is spelled out three times: the manifest and
`N_PHASE*` counts in `src/slurm/setup.R`, two `switch()` blocks in
`src/slurm/worker.R` (phase → `pass_type` at 97, phase → output filename at
120), and nine near-identical sbatch blocks in `slurm_run.sh` (353 lines, of
which roughly 230 are the nine blocks). CLAUDE.md carries a twelve-row "keeping
the SLURM pipeline in sync" table because of this.

**Change.** A bash `submit_phase` function taking name, array count,
dependency, walltime, memory, driver script, and driver args — the nine blocks
differ only in those. Then a single phase table (name, driver, granularity,
dependencies, `pass_type`, output suffix) that `setup.R` reads to build
manifests, `worker.R` reads to dispatch, and `slurm_run.sh` reads to submit.

**Payoff.** `slurm_run.sh` from 353 to roughly 120 lines, and most of the
CLAUDE.md sync table becomes unnecessary — documentation currently standing in
for an abstraction.

**Care required.** Phase 2N/2W are conditional on `s > 0` scenarios and the 2C
dependency chain differs accordingly. Keep the conditionality in the table
(a `when` predicate), not in the submitter.

### 6. Config-layer consolidations

Small, safe, and they remove per-channel edit points.

- **Detail-path construction.** `read_static_detail()` exists
  (`src/data/helpers.R:11`) but eight sites still hand-build the path:
  `run.R:563` and `:1082`, `wealth_dynamics.R:1026`, `sigma_conversion.R:312`,
  `:315`, `:583`, `kg_dynamics.R:2460`, `:2473`, `slurm/setup.R:161`,
  `main.R:95`. Add `detail_path(id, leg, year)` and route all of them through it.
- **Runscript defaults.** `parse_globals()` has five consecutive "if column
  absent, add default" blocks (`config_parser.R:152–180`) covering
  `excess_growth`, `excess_growth_start_year`, `excess_growth_all_rev`,
  `wealth_financing`, `s`. Replace with a `RUNSCRIPT_DEFAULTS` table and one
  loop. Every new channel column currently touches both `parse_globals()` and
  `get_scenario_info()`; a shared table makes it one edit.
- **Dependency defaults loop.** `config_parser.R:126–149` has nested branches
  whose control flow (three `next` statements, a `stop`, and a fall-through) is
  hard to read and flattens to roughly eight lines: for each dep, if neither
  vintage nor ID column is present set both defaults; if exactly one is present
  fill the other or stop.
- **Year-spec parsing.** `get_scenario_info()` parses the `years` /
  `dist_years` colon-or-space format three times (`:389–413`). This is the
  2026-06-22 review's open #8 (`parse_year_range`), still open.

### 7. `do_scenario()` post-processing list

`run.R:158–181` is seven hardcoded post-processing calls. It is fine as is, but
it is duplicated in `slurm/aggregate.R` Phase 3b (the CLAUDE.md sync table's
second row). A `POST_PROCESSORS` vector of functions consumed by both would
retire that row.

---

## What to leave alone

Worth stating explicitly, since a reviewer looking for line-count reductions
will be tempted by these.

- **The `calc/` layer.** `do_1040()` (`do_taxes.R:274–443`) is a long
  `bind_cols(calc_*(.))` chain that reads like the tax form itself. The section
  ordering encodes statutory sequencing. Leave it.
- **`calc_mtrs()`'s composite-variable branches** (`do_taxes.R:647–670` and
  `:694–757`). These look like duplicated lookup tables but encode genuinely
  irregular facts about which columns are components of which. The July 1 review
  already declined this (its item #29) on the grounds that the two maps are
  intentionally asymmetric — the extensive branch skips the tips/ot aggregates.
  That reasoning still holds.
- **`cohort_bathtub.R`.** A clean shared primitive; kg and wealth already reuse
  it correctly after the July 1 item #1 merge.
- **The `estate.R` two-branch DSUE calculation.** The duplication is the
  nonlinearity, not an accident.

---

## Suggested order of attack

1. Item 1 (`run_one_year()` extraction) and item 2 step 1 (shared compat check)
   in one batch. Both are behavior-preserving and share a verification run.
2. Item 3 (file splits) as a pure `git mv` batch, plus the July 1 open item #3
   (SLURM pre-pass driver triple). No logic edits — reviewable as renames.
3. Item 6 (config-layer consolidations) and item 7. Small and independent.
4. Item 4 (calibration config + provenance migration). Larger, mostly
   mechanical, but touches the calibration-watch machinery, so do it on its own.
5. Item 5 (SLURM phase table). Independent of the rest.
6. Item 2 step 2 (channel registry). Design pass first, then implement against
   the already-simplified `run_one_year()`.

Steps 1–3 are the ones that make the codebase feel smaller. Step 6 is the one
that keeps it that way.

## Verification

Use the existing harness: `tests/simplify_smoke`, pre/post worktree sbatch runs,
byte-diff of all outputs (`other/simplify_cleanup/compare_smoke.sh`). Two
additions needed for this batch:

- The smoke runscript must include a kg_dynamics scenario **and** a `s > 0`
  wealth scenario, or items 1 and 2 leave four of the six MTR sites and the
  entire 2N/2W path unexercised. Note that kg requires `pct_sample = 1`
  (`kg_dyn_check_run_compat`), so the kg leg of the smoke cannot be subsampled —
  budget for a full-sample run or gate that leg behind a separate longer job.
- xlsx outputs are never byte-identical (docProps timestamp); compare sheet XML
  only, as the prior batches did.

## Housekeeping (not code)

About 50 loose files sit at the repository root: `clausing_*.sbatch` (11),
`estate_*.sbatch` (16), `kg_*.sbatch` (5), plus `debug_dist_3b.R`,
`rerun_dist_frac.R`, `test_config_fix.R`, `compare_mech_state.R`,
`slurm-17426256.out`, `slurm-wealth-bathtub-18397737.out`. Move them under
`other/<project>/` alongside the memos they belong to, and gitignore
`slurm-*.out`, `*.log`, `*.aux`. Zero risk, and it makes the tree legible to
anyone arriving at it fresh.
