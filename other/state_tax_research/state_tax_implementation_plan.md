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
  today. With-state runs use the same PUF records carrying split-weight vectors over
  53 jurisdictions (51 modeled + `PR`/`OA` no-tax buckets, §5.4), with the constraint
  `Σ_st w_{i,st} = w_i` so federal aggregates are mechanically unchanged. This is
  the OTA TP-6 / TPC split-weight design.
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
state = 2-letter code incl. `DC`, plus `PR` and `OA` no-tax buckets — HT2 reports
Puerto Rico and Other Areas separately, so both are carried: 51 modeled + 2 buckets
= 53 jurisdictions). Long beats wide because
downstream joins in the per-state loop are `filter(state == st) %>% left_join(by='id')`.

**Construction:** two methods are prototyped and compared head-to-head in Phase 1 —
**Approach A** (classical calibration, below) and **Approach B** (differentiable /
ML reweighting). Both honor `Σ_st w_{i,st} = w_i` and emit the identical file format,
so they are swappable behind `build_state_weights(method = c("calibration","gradient"))`.
Approach B and the A/B comparison harness are specified in
`state_weights_ml_alternative.md`. Approach A (the baseline):
1. Ingest SOI Historic Table 2 CSVs (latest published year, currently ~2022):
   per-state × AGI-class return counts — total (N1) AND by filing status
   (single MARS1, joint MARS2, HoH MARS4) — the number of individuals (N2;
   labeled "exemptions" through TY2017, "individuals" from TY2018 — same
   people-on-returns concept, calibrated within-year so the relabel never
   crosses a cell), and amounts for AGI, wages, interest, dividends, capital
   gains, SALT deductions, mortgage interest, EITC.
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

**Filers vs non-filers (both approaches).** The Tax-Data microdata carries a full
population: filers AND non-filers, each a weighted tax-unit record with the same
imputed `weight`; `filer` is an exogenous 0/1 field (`variable_guide.csv` line 4),
only flipped endogenously when a refundable CTC/rebate induces filing
(`do_taxes.R:127`). HT2 is **filers only**, so it cannot pin non-filer geography, and
non-filers are geographically unlike filers (low-income, elderly-SS-only, students).
Therefore **partition records by `filer` status and target each partition separately**,
then concatenate:
- **Filers → HT2** (steps 1–6 above).
- **Non-filers → ACS/Census population margins**, by state × cell, cells built from
  model-native variables — broad age band (`age1`/`age2`, catches elderly SS-only),
  an income tier, and dependent presence (`n_dep`). Minimum target: non-filing count
  by state; preferred: state × age × income-tier.
The per-record split constraint holds automatically (each record is in exactly one
partition), so federal totals stay invariant. **Reconciliation is at the
INDIVIDUAL level, by group** (amended JI 2026-07-13, replacing the filing-unit
comparison, which depended on the v0 ACS filing model): compare weighted counts
of single adults, married adults, and children/dependents by state — IRS side
from HT2 filing-status counts and N2 (married adults = 2·MARS2 + MFS residual;
single = MARS1 + MARS4; dependents = N2 − returns − MARS2), ACS side by direct
person-level tabulation (18+ × MARST for adults, under-18 for children). The
difference IS the non-filer population by group and state, model-free.
Implemented as `compare_individuals_acs_irs()`; 2022 results: married-adult
coverage 85.6% national (74.5% MS – 95.3% SD), single-adult 77.6% (64.5% WV –
86.3% MN), children 109.2% (IRS dependents include 18+ students; dependent
filers double-count — documented construction gap). The plan's original
`sum(weight · n_people)` Census-population total remains a secondary check.

A companion **wage-based diagnostic** (JI 2026-07-13) compares wage earners
and wage dollars by state against two independent sources:
`compare_wages_acs_irs()` (ACS INCWAGE) and `fetch_qwi_state_payroll()`
(Census QWI/LEHD total payroll + stable job counts; workplace-based, UI-
covered, requires CENSUS_API_KEY). Dollars are the clean comparison — 2022:
IRS $9.63T vs ACS $9.97T (96.6% national; 81.7% DC – 101.8% CA, the CA
overage from ACS top-coding) — a high anchor confirming HT2 wage targets.
Counts carry concept gaps (returns vs persons vs jobs): returns-with-wages /
persons-with-wages = 75.5%, tight across states (70.9–78.8%), so residual
variation is informative.

QWI 2022 results (43/51 states published; payroll proxied as 3·EmpS·EarnS
since the API's Payroll variable is null at state tabulations): IRS/proxy =
105% nationally (proxy omits unstable-job earnings), with the
workplace-vs-residence signature unmistakable — DC 57.8%, MD 131.7%, VA
117.1% vs their tight residence-based ACS ratios. QWI is therefore a
STRUCTURAL cross-check and demographic-cell source, not a level anchor.

**LODES resolves the residence problem** (JI 2026-07-13,
https://lehd.ces.census.gov/data/lodes/): the same LEHD job frame published
on both bases. RAC files aggregate to residence-based state employment
directly — with age bands (≤29/30–54/55+) and monthly earnings bands — via
`fetch_lodes_rac()`; verified 2022 that the residence ordering is restored
where QWI diverged (DC 285k resident workers vs 485k workplace jobs; MD
2.51M residents vs 2.16M in-state jobs). OD files give the state-to-state
commuter matrix via `fetch_lodes_od_matrix()` — 2022: DC residents hold only
31.0% of DC-workplace primary jobs (MD 39.0%, VA 25.9%), quantifying the
QWI anomaly exactly — and serve as the reallocation operator for converting
QWI workplace payroll to residence basis (OD shares × workplace earnings).
Concept caveats: JT01 primary jobs are point-in-time (below any-wage-in-year
ACS/IRS counts by construction), same UI universe as QWI, LODES8 runs
through 2022.

Beyond the weights, **LODES is a candidate source for resolving cross-border
tax liability** (JI 2026-07-13): states tax nonresidents on wages earned
in-state, residents claim credits for taxes paid to other states, and
reciprocity agreements (and DC's Home Rule nonresident-tax bar) rewire the
DC/MD/VA-type flows entirely. Modeling any of that requires knowing where a
resident's wages are EARNED — exactly the OD matrix. A future extension
could impute a work-state distribution to each record's wage income from
the OD shares (by residence state × earnings band), feeding nonresident
returns, other-state credits, and reciprocity rules in the calculator.
Added to the Phase 7 list.

**Candidate person-level target dimensions (JI 2026-07-13).** The PUF splits
wages by earner with demographics (`wages1/2` × `age1/2` × `male1/2`), so
record-level x-vectors like "wage dollars earned by males 25–34" are
directly constructible — fully compatible with the engines' target format:
- **QWI sex × age** (verified: Emp/EmpS/EarnS publish by sex × 8 WIA age
  bands × state, quarterly) ↔ PUF earner counts and wage dollars by sex ×
  age band. Jobs≠persons, workplace-based, UI-covered — use for structure.
- **ACS marital status × age × wages** (INCWAGE × MARST × AGE): QWI lacks
  marital status; ACS supplies it residence-based, matching PUF geography.
  Two-earner-couple rates by state (both spouses with wages) are a sharp
  derived margin for joint-return geography.
- **Elderly (65+) wage participation** (QWI A08 / ACS) ↔ PUF earners 65+ —
  retirement-state signal (FL/AZ).
- **Not matchable**: QWI race/ethnicity and education crosses, industry,
  firm size — the PUF carries none of these.
Recommended use: start as UNTARGETED validation cells in the harness
(metric 2, where OTA's weights showed weakness); promote to calibration
targets only where state fit is poor — thin demographic cells overfit.
Caveats to document: ACS is residence-based vs HT2 filing-address-based (minor at
state level); "non-filer" is not observed in ACS, so the imputed `filer` flag is the
authority for *who* is a non-filer and ACS supplies only the geographic *margins*.
Open sub-decision: non-filer cell granularity (state-total vs state×age×income) —
tune in Phase 1 against the reconciliation and downstream refundable-credit results.
This matters most for the EITC and the `rebate` (stimulus/UBI) module, where
non-filers are the population of interest.

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
│   │   ├── exempt.yaml   # personal exemptions
│   │   └── filing.yaml   # filing-requirement thresholds (gross-income tests)
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
  - `st_agi.conformity_year`: 0 = rolling conformity (state base moves with the
    scenario's federal law); else the fixed IRC conformity date (e.g. CA ≈ 2015).
    **Encode from day one** (read off the same statutes as everything else), but v1
    *mechanics* treat every state as rolling. Fixed-date mechanics are deferred to
    Phase 7. The reference base must be calculated on the same post-behavior records
    as the scenario, not joined from a baseline detail file; selective-conformity
    states also need a configuration bridge. Until then, a federal-reform run that
    requests a fixed-conformity state must hard-stop rather than publish a
    rolling-conformity estimate.
  - `st_ded.item_coupling`: 0 = independent choice, 1 = must match federal,
    2 = federal itemizers may choose (NE-style)
  - `st_ded.fed_tax_ded_limit`: deduction cap for federal income tax paid
    (0 = none, Inf = unlimited (AL)); plus phase-out params (MO, OR)
  - `st_credits.eitc_match`: share of federal EITC (0 where none)
  - `st_ded.salt_addback`: 1 if state income tax must be backed out of federal
    itemized deductions (uncapped line-5a semantics)
- **Encoding convention — anchor every year-keyed value list at 2017** (the state
  law floor). A subparameter whose first specified year is later (e.g. a credit
  enacted in 2025 encoded only as `'2025': …`) produces an empty series when the
  simulation window ends before that year and breaks the parse; the 2017 anchor
  (usually `0` for a not-yet-enacted credit) is also the semantically correct
  value. Discovered in Phase 4 single-year builds; enforced by the year-window
  regression in `test_state_tax_law`.
- **`reference` metadata field** (PolicyEngine practice): every parameter cites form
  line / statute section + URL. Parser status (verified 2026-07-12): unknown keys are
  already tolerated for **unindexed** subparams (early return, `tax_law.R:311`) but
  break **indexed** ones — with `indexation_defaults` present, the `map2()` at
  `tax_law.R:319-320` requires equal lengths and errors; without defaults, the extra
  key is swept into the indexation-year handling and corrupts the series. Fix: strip
  `reference` in `parse_subparam()` before the indexation-key sweep. The unit test
  MUST cover an indexed subparameter under `indexation_defaults` — an
  unindexed-only test passes vacuously.
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
└── calc_st_liab()     # -> liab_st_iit; applies st_filing.req_threshold
                       #   (filing-status-mapped gross-income test): records below
                       #   the state filing requirement get zero liability and are
                       #   excluded from state filer counts (§6 TPC overcount risk)
```

- Inputs: federal-pass outputs already on the tibble (`agi`, `txbl_inc`, itemizer
  flag `itemizing`, itemized components incl. pre-limitation total
  `item_ded_ex_limits`, `eitc`, `ctc_*`, `cdctc_*`) + `st_*` law columns.
  **Verified 2026-07-12:** all of the above are registered `return_vars`, and raw
  input columns survive downstream because `do_taxes()` binds calc output onto the
  original records — so NO federal `return_vars` plumbing is needed. Uncapped SALT
  components (`salt_inc_sales`, `salt_prop`, `salt_pers`) reach the state layer as
  input columns; `calc_st_*` functions declare them in `req_vars` directly.
  **DECIDED (2026-07-12) — state SALT add-back uses the post-workaround value.**
  `do_taxes()` mutates `salt_inc_sales` in place (the "Shift SALT" block,
  `do_taxes.R:713`), subtracting PTE-workaround amounts recharacterized as
  entity-level losses. Those dollars were not paid as individual state income tax
  on Schedule A, so the post-workaround value is the *correct* individual amount
  for state add-back semantics. If a pristine pre-workaround value is ever needed,
  capture it before that mutation — do not re-derive it downstream.
- Genuinely idiosyncratic structures that resist parameterization (PA's eight income
  classes, NJ's own base) get `case_when(st == …)` blocks inside the generic
  functions — contained, documented exceptions rather than per-state files.
- **Filing-status caveat:** states with joint-vs-separate optimization (TAXSIM
  computes both, takes lower) are out of scope for v1; note in known-differences.

### 2.4 Orchestration and outputs

- **Mode switch:** new optional runscript column `states` (empty/absent = current
  behavior; `all` or space-delimited list, e.g. `IL CO NY`). Per-row parsing in
  `get_scenario_info()` (`src/misc/config_parser.R`). The cross-row consistency rule
  — baseline's `states` must be a **superset** of every counterfactual's — cannot
  live there (`get_scenario_info()` filters to a single ID, `config_parser.R:321`,
  and never sees other rows); it goes in **`parse_globals()`**, which reads the full
  runscript CSV (`config_parser.R:114-118`) and already does runscript-level
  defaulting. Runtime guard for the case parse-time can't see: if `baseline_vintage`
  points at a pre-existing baseline run without state totals, the state
  revenue-estimate builder fails fast with a clear message instead of emitting
  empty deltas.
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
- **Outputs:** `totals/state.csv` (year × state × variable levels; structural
  precedent is the long `1040_by_agi.csv`), `supplemental/state_rev_est.csv`
  (deltas vs baseline, by state), and a state distribution cut later. State detail
  goes exclusively through the compact matrix — do NOT touch `globals$detail_vars`
  (an unconditional hardcoded vector, `config_parser.R:237-253`; conditional
  membership would be new machinery for no benefit).
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

### 2.6 Local income taxes (deferred; design sketch, JI 2026-07-12)

Eventually the model should cover localities with individual income taxes. Proposed
approach: **extend the split-weight design one level down**, using **IRS SOI
county-level data** (https://www.irs.gov/statistics/soi-tax-stats-county-data —
county × AGI-class return counts and income items, same family as HT2) to further
split a state's weights across sub-state areas, conditional on the state weights:

- **Formal structure:** split `w_{i,st}` into `w_{i,st,loc}` with
  `Σ_loc w_{i,st,loc} = w_{i,st}` — the same per-record split constraint one level
  down, so state totals (and hence national totals) are preserved by construction.
  Both Phase 1 weight engines (calibration and gradient) apply unchanged with county
  targets in place of HT2 targets.
- **Cleanest implementation:** treat taxing localities as **additional jurisdiction
  columns in the same weights file** — e.g., split NY into `NY-NYC` and `NY-xNYC`,
  both running NY state law, with NYC local law layered on the first. The weights
  format (`id, state, weight` with `state` generalized to a jurisdiction key), the
  per-jurisdiction loop, and the sum-to-national invariant all carry over without
  structural change. NYC = 5 boroughs = 5 counties, so SOI county data targets it
  directly.
- **Scoping task (first step):** enumerate the subset of states with local income
  taxes and the localities within them, by structure: NYC + Yonkers (NY); MD county
  add-on rates (statewide, county-level — good county-data fit); IN county rates;
  OH municipal and school-district taxes and PA local EIT (**sub-county** — county
  data cannot resolve these; approximate at county level or defer); KY occupational
  licenses; MO (KC/St. Louis earnings taxes); AL occupational; DE Wilmington; NJ
  Newark; OR metro (Portland).
- **Known limits to document:** residence-based reweighting cannot capture
  **workplace-based** taxes (OH municipal work-place withholding, Yonkers
  nonresident earnings tax, KC/St. Louis earnings taxes on commuters); SOI county
  data is filer-only (non-filer county margins would need ACS, mirroring §2.1);
  sub-county jurisdictions (OH/PA) need an allocation assumption on top of county
  calibration.
- Open to alternative reweighting refinements — the NYC/non-NYC split is the
  motivating case; the same mechanism generalizes to any sub-state partition with
  SOI county coverage.

Priority: after the 50-state rollout stabilizes (Phase 7 list); NYC first (largest
local PIT, cleanest county mapping), MD counties second (statewide coverage,
county-native).

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
MARD and downstream pilot-state liability reported honestly for both methods.
**Torch question RESOLVED (2026-07-12):** `torch` is not installed on the cluster, so
Approach B is implemented dependency-free in `src/data/state_weights.R` (softmax/Adam
with analytic gradients + a finite-difference self-test); reproducibility via
`globals$random_seed` and deterministic full-batch gradients. **Prototype status:**
HT2 ingestion, ACS non-filer margins, and both fitting engines exist; still missing
are the `build_state_weights(method=)` dispatcher and the `state_weights_{year}.csv`
assembly/writer.

**Phase 2 — Parameter schema + pilot states (2–3 weeks, parallel with Phase 1)**
Implement `build_state_tax_law()`, `st_` naming, `reference` field tolerance, index
series additions. Encode **IL** (flat rate, federal-AGI start — simplest real state),
**CO** (flat rate, federal-*taxable-income* start — tests deduction flow-through),
**NY** (graduated brackets, own itemized rules, 30% EITC match, household credit —
complexity ceiling). Sources: NBER historical forms archive + state DOR current
forms; Tax Foundation tables as transcription cross-check only. Every parameter cites
its form line/statute.

**Standing workstream — state parameter packets (starts in Phase 2 and runs through
Phase 6)**
Do not wait for the weights to start state-law research. For any state, the following
can proceed immediately and in parallel with Phases 1–5: source-packet assembly,
baseline YAML drafting, `reference` cleanup, worksheet-style unit tests,
known-differences notes, and TAXSIM / PolicyEngine spot checks. Only aggregate
validation remains weights-blocked. Track the queue in
`other/state_tax_research/state_parameter_rollout.csv` and use
`other/state_tax_research/source_packets/TEMPLATE.md` for per-state documentation.

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

**Phase 6 — 50-state completion / backlog burn-down (bulk of calendar time; parallelizable)**
Batch by structural family, easiest first, validating each batch through the Phase 5
harness before the next. By the time this phase starts, research packets and many YAML
drafts should already exist; this phase should finish and validate the backlog rather
than begin state discovery from zero:
1. No-broad-income-tax states. Immediate zero-tax stubs apply only to jurisdictions
   with no state individual fiscal program in the 2017-forward model window
   (`AK FL NV SD TX WY`). `NH`, `TN`, and `WA` are now implemented through special
   profiles: narrow investment-income tax for NH/TN, and capital-gains excise plus
   Working Families Tax Credit transfer for WA. Their remaining Phase 6 work is
   cross-model and aggregate validation, not initial law discovery.
2. Flat-rate federal-AGI states (AZ CO* GA† ID† IN KY MI NC PA‡ UT …).
3. Graduated federal-AGI states (largest group: CA VA MD‡‡ MN WI MO OH OK …).
4. Federal-taxable-income states (ID MT ND OR SC …).
5. Own-base states (AL AR MS NJ PA) — most custom logic.
6. Federal-deductibility states (AL MO OR) — need the §2.5 loop for federal reforms;
   state-static works without it.
   (*CO is fed-taxable; †recently flat; ‡PA own-base flat; ‡‡MD county add-on
   deferred — verify each during encoding, not from this list.)

**Phase 7 — Later scope, in rough priority order**
Coupled federal↔state iteration + sales-tax election imputation; frozen-base
mechanics for fixed-date-conformity states under federal reforms (§2.2
`st_agi.conformity_year`); local income taxes via county-level sub-state weights
(§2.6 — NYC/Yonkers, MD counties first); cross-border wage taxation
(nonresident returns, other-state credits, reciprocity) with work-state
imputation from the LODES OD matrix (§2.1); state MTRs and
combined-MTR behavioral feedback; state distribution tables; state AMTs; historical
years pre-2017; state population-projection aging of weights.

---

**Current Phase 6 packet status (2026-07-13).** Indiana, Kentucky, and Michigan
now have primary-source packets, baseline YAML, worksheet tests, and generic
support for refundable EITC matches and Kentucky's family-size percentage-of-tax
credit. California now has the same artifacts plus reusable exemption-credit and
independent earned-income-credit support. All four remain pending cross-model and
weights-dependent aggregate validation. California's exact $50 CalEITC lookup,
full 2018-24 rate-bracket transcription, and California-specific index series are
explicit follow-up items rather than complete claims.

Illinois, Colorado, and New York source packets have now been normalized and
their next-state research cohorts are documented in
`other/state_tax_research/state_tax_batch_analysis.md`: `IL/IN/MI` for rolling
federal-AGI flat-tax validation, `CO/ND/SC` for federal-taxable-income
construction, and `NY/CT` for graduated federal-AGI schedules with high-income
calculation layers. These are research batches, not claims that ND, SC, or CT
are implemented.

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
4. **Non-state buckets: DECIDED (amended 2026-07-12) — carry `PR` AND `OA` as
   no-tax jurisdictions** (HT2 reports Puerto Rico separately from Other Areas, so
   the weights prototype carries both: 51 modeled + 2 buckets = 53). Keeps
   `Σ_st w = w_national` exact and state totals reconcilable to HT2 by construction.
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
  Encode state filing thresholds/requirements per state from day one (schema home:
  `filing.yaml` → `st_filing.*`, applied in `calc_st_liab()`, §2.3).
- **Fixed-date IRC conformity** — `st_agi.start_point` treats "federal AGI" as the
  scenario's computed AGI, but fixed-date-conformity states (CA and others) do not
  move with federal reforms. Invisible for state-static baseline estimates; wrong in
  detail for federal reforms with states on. `st_agi.conformity_year` encoded from
  day one, frozen-base mechanics in Phase 7, known-differences warning in between
  (§2.2).
- **`salt_inc_sales` conflation** — income vs sales tax not separable in the data;
  fine for state-static; the coupled mode needs the imputation task before it's real.
- **TAXSIM recent-year state law is approximate** — validation gaps in 2021+ may be
  TAXSIM's, not ours; PolicyEngine (2021+) is the tie-breaker.
- **Maintenance load** — 41 income-tax states × annual updates is the real long-run
  cost; the `reference` discipline and per-state test suites are what keep it
  tractable (PolicyEngine's experience).
