# State Individual Income Tax Module — Implementation Plan

**Date:** 2026-07-08
**Companion doc:** `state_tax_model_research_notes.md` (same directory) — evidence and
citations behind every design choice here.

---

## 1. Goal and guiding decisions

Add state individual income tax modeling to Tax-Simulator on a long-lived branch,
mirroring the federal design. Parameters built from primary governmental sources
(state forms, instructions, statutes); existing models (TAXSIM, TPC/Bakija,
PolicyEngine) used for validation and structural reference only.

Decisions already made (validated by research):

- **D1 — With-state / without-state mode.** Without-state runs are bit-identical to
  today. With-state runs use the same PUF records carrying 51 state-specific weight
  vectors, with the constraint `Σ_st w_{i,st} = w_i` so federal aggregates are
  mechanically unchanged. This is the OTA TP-6 / TPC split-weight design.
- **D2 — Federal once, states as a downstream layer.** The federal static pass runs
  once per record-year. State liability is computed per state on the same records
  (51 cheap vectorized passes over ~221k records). Baseline federal calculation keeps
  *reported* `salt_inc_sales`; the coupled federal↔state iteration (TAXSIM-style,
  ≤3 rounds) is a later, opt-in mode for reforms that need it.
- **D3 — Uniform state parameters, state as a join key.** Unlike PolicyEngine's
  state-prefixed variable names, a vectorized model wants ONE set of
  `calc_st_*()` functions with uniform column names (`st_ord.rates1`,
  `st_std.amount`, …) and per-state *values* supplied via a `(year, filing_status,
  state)` join. Structural heterogeneity (own-base states, coupling rules) is encoded
  as parameter flags, not per-state code, wherever possible.

---

## 2. Architecture

### 2.1 Data: state weights

**Format:** one file per year, long:
`state_weights_{year}.csv` with columns `id, state, weight` (rows where weight > 0;
state = 2-letter code + `DC` + `OA` for other areas). Long beats wide because
downstream joins in the per-state loop are `filter(state == st) %>% left_join(by='id')`.

**Construction:** two methods are prototyped and compared head-to-head in Phase 1 —
**Approach A** (classical calibration, below) and **Approach B** (differentiable /
ML reweighting). Both honor `Σ_st w_{i,st} = w_i` and emit the identical file format,
so they are swappable behind `build_state_weights(method = c("calibration","gradient"))`.
Approach B and the A/B comparison harness are specified in
`state_weights_ml_alternative.md`. Approach A (the baseline):
1. Ingest SOI Historic Table 2 CSVs (latest published year, currently ~2022):
   per-state × AGI-class return counts and amounts for AGI, wages, interest,
   dividends, capital gains, SALT deductions, mortgage interest, EITC.
2. Stratify PUF records into AGI classes matching HT2 (TPC uses 9 strata).
3. Estimate initial `p(state | x)` from HT2 shares within stratum (x = AGI class ×
   filing status × Schedule-A-relevant indicators: has `salt_inc_sales`, has
   mortgage interest, has EITC — mirroring OTA's conditioning set).
4. Calibrate within each stratum so state-weighted totals hit HT2 targets
   (raking / Deville-Särndal via R `survey::calibrate` or a small ipfp loop), with
   the sum-to-national-weight constraint enforced by construction
   (`w_{i,st} = p̂(st|x_i)·w_i`, probabilities renormalized after calibration).
5. **Target years:** HT2 is published for every historical year (through ~2022), so
   calibrate each historical simulation year (2017–2022) to its own HT2 vintage.
   For later years, hold the last calibrated `p(st|x)` fixed (weights then inherit
   Tax-Data's national weight growth). Revisit with state population projections in
   a later phase.
6. Diagnostics report in the OTA style: per-state correlation and mean absolute
   relative difference vs HT2 for targeted AND untargeted variables.

**Home:** prototype as `src/data/state_weights.R` + a build script under `other/`;
**production home is upstream Tax-Data** (new interface file alongside
`tax_units_{year}.csv`), versioned via `interface_versions.yaml`. Migrate once stable.

### 2.2 Tax law configuration

**Separate state tax-law root, reusing all existing machinery per state:**

```
config/scenarios/tax_law_state/
├── baseline/
│   ├── il/            # one dir per jurisdiction, lowercase postal code
│   │   ├── agi.yaml   # conformity + additions/subtractions params
│   │   ├── ded.yaml   # standard deduction, itemized rules, coupling flags
│   │   ├── ord.yaml   # rates and brackets
│   │   ├── credits.yaml  # EITC match, CTC, CDCTC, property/renter credits
│   │   └── exempt.yaml   # personal exemptions
│   ├── co/ …
│   └── ny/ …
└── {public|private|tests}/{reform_name}/{st}/*.yaml   # overrides, same mechanics
```

- Each state directory is parsed with the **existing** `load_tax_law_input()` /
  `parse_param()` pipeline (they already handle indexation, filing-status mappers,
  time series). A new thin wrapper `build_state_tax_law()` loops jurisdictions,
  prefixes parameter names with `st_` (filenames stay clean), pivots wide exactly
  like `build_tax_law()`, adds a `state` column, and `bind_rows()`. Output written to
  `supplemental/tax_law_state.csv`.
- Reform override semantics identical to federal: reform dir overlays baseline at the
  subparameter level, per state. Runscript gets an optional `state_tax_law` column
  (default `baseline`).
- **Conformity encoded as parameters** in `agi.yaml`/`ded.yaml` per state, e.g.:
  - `st_agi.start_point`: 0 = own base, 1 = federal AGI, 2 = federal taxable income
  - `st_ded.item_coupling`: 0 = independent choice, 1 = must match federal,
    2 = federal itemizers may choose (NE-style)
  - `st_ded.fed_tax_ded_limit`: deduction cap for federal income tax paid
    (0 = none, Inf = unlimited (AL)); plus phase-out params (MO, OR)
  - `st_credits.eitc_match`: share of federal EITC (0 where none)
  - `st_ded.salt_addback`: 1 if state income tax must be backed out of federal
    itemized deductions (uncapped line-5a semantics)
- **`reference` metadata field** (PolicyEngine practice): every parameter cites form
  line / statute section + URL. Requires a small `parse_subparam()` change to
  tolerate-and-ignore a `reference` key — verify with a unit test.
- **Indexation:** state YAML uses existing `i_measure` fields. New index series
  needed in `generate_indexes()` (`src/data/economy.R`): chained CPI already exists;
  add GDP deflator (from Macro-Projections) and alias state-specific CPIs (AZ Phoenix
  MSA, CA CPI) to national CPI-U initially with a documented approximation flag.
  Per-state base years/rounding already fit `i_base_year`/`i_increment`/`i_direction`.

### 2.3 Calculator

New directory `src/calc/state/`, functions obeying the existing contract
(`return_vars$…` registration, `req_vars`, `parse_calc_fn_input()`, one `mutate`,
return registered columns):

```
do_state_taxes(tax_units_post_federal, st)   # orchestrator, mirrors do_1040()
├── calc_st_agi()      # start from agi / txbl_inc / own base per st_agi.start_point;
│                      #   additions (e.g. non-own-state muni interest — data-limited,
│                      #   phase-in later), subtractions (state refunds via state_ref,
│                      #   SS/retirement exclusions, federal tax ded for AL/MO/OR)
├── calc_st_ded()      # standard vs itemized under coupling flag; SALT add-back
├── calc_st_exempt()   # personal/dependent exemptions or credits
├── calc_st_txbl()
├── calc_st_tax()      # reuse integrate_rates_brackets()
├── calc_st_credits()  # EITC/CTC/CDCTC matches on federal amounts; nonrefundable
│                      #   ordering per state form
└── calc_st_liab()     # -> liab_st_iit
```

- Inputs: federal-pass outputs already on the tibble (`agi`, `txbl_inc`, itemizer
  flag, *uncapped* itemized components, `eitc`, `ctc_*`, `cdctc_*`) + `st_*` law
  columns. **Task:** ensure the federal calc exposes the sub-line detail states need
  (notably uncapped SALT components and pre-limitation itemized totals) in
  `return_vars` — plumbing only, no recomputation.
- Genuinely idiosyncratic structures that resist parameterization (PA's eight income
  classes, NJ's own base) get `case_when(st == …)` blocks inside the generic
  functions — contained, documented exceptions rather than per-state files.
- **Filing-status caveat:** states with joint-vs-separate optimization (TAXSIM
  computes both, takes lower) are out of scope for v1; note in known-differences.

### 2.4 Orchestration and outputs

- **Mode switch:** new optional runscript column `states` (empty/absent = current
  behavior; `all` or space-delimited list, e.g. `IL CO NY`). Parsed in
  `get_scenario_info()` (`src/misc/config_parser.R`), which also validates that the
  baseline row sets `states` whenever any counterfactual does.
- In `run_one_year()`, after the federal static pass:
  ```
  for (st in scenario_info$states):
      join st slice of state tax law (year, filing_status)
      do_state_taxes() -> liab_st_iit per record
      join state weights (id) -> aggregate totals
  ```
  Per-record results accumulate into (a) a long totals table and (b) optional state
  detail, off by default; when enabled, written as ONE compact per-year matrix
  `detail/state/{year}.csv` (`id` + one liability column per state), not 51 full
  detail files.
- **Outputs:** `totals/state.csv` (year × state × variable levels),
  `supplemental/state_rev_est.csv` (deltas vs baseline, by state), and a state
  distribution cut later. Add new variables to `globals$detail_vars` only if state
  detail is on.
- **SLURM:** per the CLAUDE.md sync table — the state loop lives inside
  `run_one_year()` (no worker changes), but new totals/post-processing must be added
  to `src/slurm/aggregate.R` Phases 3a/3b, and `states`/`state_tax_law` scenario-info
  fields to `src/slurm/setup.R` serialization.

### 2.5 Coupled mode (deferred, Phase 7)

For reforms where state tax changes should feed federal SALT (or federal changes feed
AL/MO/OR bases): iterate federal↔state ≤3 rounds on *liability* (TAXSIM precedent),
replacing `salt_inc_sales` with computed state income tax + an imputed sales-tax
election for non-income-tax states (Pub 600-style regression — separate task). Only
itemizers (~9% of filers) trigger the loop; run it on that subset. Not needed for
state-static estimates, which is everything in Phases 1–6.

---

## 3. Phases and deliverables

**Phase 0 — Scaffold (few days)**
Branch `state-tax`. Commit both research/plan docs. CLAUDE.md addendum describing the
state module conventions. Decision sign-off on §5 open items.

**Phase 1 — State weights prototype + A/B bake-off (2–3 weeks)**
HT2 ingestion → build BOTH Approach A (classical calibration) and Approach B
(differentiable reweighting) behind `build_state_weights(method=)` → run the shared
comparison harness (`state_weights_ml_alternative.md` §4) → `state_weights_{year}.csv`
+ OTA-style diagnostics for each. Acceptance: chosen method hits targeted variables
within 2% for ≥99% of state×stratum targets (TPC benchmark), with untargeted-variable
MARD and downstream pilot-state liability reported honestly for both methods. Includes
a `torch`-for-R availability check on the cluster before committing to B in production.

**Phase 2 — Parameter schema + pilot states (2–3 weeks, parallel with Phase 1)**
Implement `build_state_tax_law()`, `st_` naming, `reference` field tolerance, index
series additions. Encode **IL** (flat rate, federal-AGI start — simplest real state),
**CO** (flat rate, federal-*taxable-income* start — tests deduction flow-through),
**NY** (graduated brackets, own itemized rules, 30% EITC match, household credit —
complexity ceiling). Sources: NBER historical forms archive + state DOR current
forms; Tax Foundation tables as transcription cross-check only. Every parameter cites
its form line/statute.

**Phase 3 — State calculator (2–3 weeks)**
`src/calc/state/` per §2.3, driven by the three pilot states. Federal return-var
plumbing for uncapped itemized detail. Per-state unit tests from form worksheets
(`src/tests/state/`): hand-computed returns at multiple income/filing-status points,
including bracket boundaries and credit phase-outs.

**Phase 4 — Orchestration + outputs (1–2 weeks)**
Runscript `states` column, `run_one_year()` state loop, totals/rev-est outputs, SLURM
sync. Acceptance: a with-state run's federal outputs are byte-identical to a
without-state run of the same scenario.

**Phase 5 — Validation harness (1–2 weeks)**
TAXSIM-35 comparison via `usincometaxes` (extends `src/tests/test_taxsim.R`): map
records, compare `siitax` per state/year, report match rates at $15/$100 tolerances
with a maintained known-differences list (recent-year TAXSIM state law is inflated
2020/2021 law — expect systematic gaps there). Aggregate benchmarks: state liability
totals vs HT2 "total tax" and vs state revenue-agency published estimates for pilot
states.

**Phase 6 — 50-state rollout (bulk of calendar time; parallelizable)**
Batch by structural family, easiest first, validating each batch through the Phase 5
harness before the next:
1. No-income-tax states (AK FL NV SD TN TX WA WY, NH interest/div phase-out) — config
   stubs so `all` mode is total.
2. Flat-rate federal-AGI states (AZ CO* GA† ID† IN KY MI NC PA‡ UT …).
3. Graduated federal-AGI states (largest group: CA VA MD‡‡ MN WI MO OH OK …).
4. Federal-taxable-income states (ID MT ND OR SC …).
5. Own-base states (AL AR MS NJ PA) — most custom logic.
6. Federal-deductibility states (AL MO OR) — need the §2.5 loop for federal reforms;
   state-static works without it.
   (*CO is fed-taxable; †recently flat; ‡PA own-base flat; ‡‡MD county add-on
   deferred — verify each during encoding, not from this list.)

**Phase 7 — Later scope, in rough priority order**
Coupled federal↔state iteration + sales-tax election imputation; state MTRs and
combined-MTR behavioral feedback; state distribution tables; local income taxes (NYC,
MD counties); state AMTs; historical years pre-2017; state population-projection
aging of weights.

---

## 4. Validation strategy (summary)

| Level | Test | Benchmark |
|---|---|---|
| Parameter | transcription check | Tax Foundation / TPC / ITEP tables |
| Record | form-worksheet unit tests | hand-computed state returns |
| Record | cross-model | TAXSIM-35 (`usincometaxes`), PolicyEngine spot checks (2021+) |
| Aggregate | weighted totals | SOI HT2 state × AGI class |
| Aggregate | revenue estimates | state revenue-agency estimates (pilot states) |
| Invariant | federal unchanged | with-state vs without-state byte-diff |

---

## 5. Open decisions (need sign-off before/during Phase 0)

1. **Years of state law to encode: DECIDED (JI, 2026-07-08) — 2017-forward**, matching
   the microdata floor so every simulatable year is covered. Validation coverage
   splits cleanly: 2017–2020 against TAXSIM's actually-coded state law, 2021+ against
   PolicyEngine (TAXSIM 2021+ uses inflated prior-year parameters).
2. **Weights home: DECIDED (2026-07-08) — prototype in-repo, migrate to Tax-Data
   once the calibration spec and diagnostics stabilize.** Discipline: write the
   weights file in the exact format the eventual Tax-Data interface will use, so
   migration changes a path, not the model.
3. **State detail files: DECIDED — off by default.** When enabled, write one compact
   per-year liability matrix (`id` + one liability column per state, ~100–200 MB/yr)
   rather than 51 full detail files.
4. **`OA` (other areas) bucket: DECIDED — carry as a 52nd jurisdiction with no tax
   calculation.** Keeps `Σ_st w = w_national` exact and state totals reconcilable to
   HT2 by construction.
5. **State mode switch: DECIDED — runscript `states` column** (per-scenario control,
   reproducible-by-config). Add a validation rule in `get_scenario_info()`: if any
   counterfactual row sets `states`, the baseline must too (state deltas need
   baseline state liability).
6. **Pilot state #3: DECIDED — NY** (complexity gauntlet on strong primary sources
   and strong TAXSIM/PolicyEngine coverage, so mismatches are likely ours). CA goes
   first in the Phase 6 rollout, where CalEITC (an independent phase-in/out schedule,
   not a federal match) is the acceptance test for credit-schema generality and
   CA-CPI for the indexation series work.

---

## 6. Risks

- **HT2 lag vs projection years** — weights are as good as the national aging
  assumption; state-differential growth untracked in v1 (accepted, documented).
- **Conditional-independence limits (OTA caveat)** — weights can't capture
  state-policy-driven attribute variation; mitigated by SALT/mortgage/EITC
  conditioning variables, and by keeping state-varying *attributes* in the calc
  layer, not the weight layer.
- **State filer counts** — federal filers ≠ state filers (TPC overcounted 8–35%).
  Encode state filing thresholds/requirements per state from day one.
- **`salt_inc_sales` conflation** — income vs sales tax not separable in the data;
  fine for state-static; the coupled mode needs the imputation task before it's real.
- **TAXSIM recent-year state law is approximate** — validation gaps in 2021+ may be
  TAXSIM's, not ours; PolicyEngine (2021+) is the tie-breaker.
- **Maintenance load** — 41 income-tax states × annual updates is the real long-run
  cost; the `reference` discipline and per-state test suites are what keep it
  tractable (PolicyEngine's experience).
