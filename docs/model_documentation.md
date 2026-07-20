# Tax-Simulator: Model Documentation

**Last updated:** 2026-07-19 (branch `state-tax`)
**Scope:** the full federal model — data, configuration, calculator, behavior,
outputs, validation — plus the state income tax module and its current status.
**Audience:** a new analyst who needs to understand, run, and extend the model.
**Related:** `README.md` (quick start), `CLAUDE.md` (operational reference for
agents/automation), `docs/website/` (the older R Markdown site this document
supersedes in coverage), `.claude/skills/policy-config/SKILL.md` (reform YAML
details), `other/state_tax_research/` (state workstream design docs).

---

## 1. What the model is

Tax-Simulator is The Budget Lab at Yale's policy microsimulation model for
U.S. federal (and, in progress, state) individual income and payroll taxes.
It simulates a representative population of tax units, calculates each unit's
liability under current law and under counterfactual reforms, and aggregates
the results into revenue estimates, distribution tables, and effective
marginal tax rate (EMTR) analyses.

Conceptually it has two components:

1. **A policy calculator** — a deterministic, fully parameterized function
   representing tax law: inputs are a tax unit's attributes, outputs are
   liability and every intermediate 1040 quantity. Current law is just one
   configuration of the calculator's parameters.
2. **A population simulator** — the machinery that projects the tax-unit
   population forward (delegated to the upstream **Tax-Data** model, §3) and
   simulates behavioral responses to policy (behavioral modules, §7).

The model produces **static** estimates (behavior held fixed), **conventional**
estimates (tax-avoidance-style responses via behavioral feedback modules), and
**partial dynamic** estimates (first-order economic responses such as labor
supply). Full general-equilibrium dynamics are out of scope by design.

## 2. The model ecosystem

Tax-Simulator is one node in a system of versioned models that communicate
through files. `config/interfaces/interface_versions.yaml` pins the vintage of
every dependency; `config/interfaces/output_roots.yaml` maps `local` and
`production` filesystem roots. Every run reads dependency data from
`{root}/model_data/{MODEL}/v{version}/{vintage}/{scenario}/` and writes its own
output under a vintage stamp (timestamp by default). Runscripts can override
any dependency vintage per scenario via `dep.{MODEL}.vintage` / `dep.{MODEL}.ID`
columns.

Current dependencies:

| Model | Role |
|---|---|
| **Tax-Data** | The input microdata: projected tax-unit files per year (§3) |
| **Macro-Projections** | Economic series: CPI-U, chained CPI, Average Wage Index, aggregates used for indexation and growth adjustments |
| **Value-Added-Tax-Model** | Price-level offsets when a VAT scenario is stacked |
| **Off-Model-Estimates** | Estimates appended to receipts outside the microsimulation |
| **Wealth-Tax-Simulator / Cost-Recovery-Simulator / Estate-Tax-Distribution** | Companion simulators whose outputs join at post-processing |

## 3. Input data: the Tax-Data pipeline

The microdata come from the companion repository
[**Tax-Data**](https://github.com/Budget-Lab-Yale/Tax-Data), which produces one
tax-unit file per year (`tax_units_{year}.csv`, ~220k records for recent
vintages). Tax-Simulator treats these files as given; anything about *who is in
the data and what their attributes are* is Tax-Data's domain. The pipeline, in
execution order (`Tax-Data/src/main.R`):

1. **Base file.** The 2015 IRS Public Use File (~150k records, ~1% sample)
   plus its demographic supplement (age ranges, gender for a subset). The PUF
   is restricted-access; the model cannot run without it, which is why the
   repos are public but the model is not fully open-source.
2. **Historical aging to 2017** (`process_targets.R`, `reweight.R`,
   `create_2017_puf.R`). Record weights are adjusted by a linear-programming
   targeting algorithm to hit SOI Publication 1304 tabulations (return counts
   and amounts by AGI group, filing status × age), then dollar amounts are
   rescaled — the Ricco (2020) / O'Hare (2009) two-stage approach.
3. **Historical aging 2018+** (`project_puf.R`). TCJA broke the comparability
   of itemized-deduction tabulations (the itemizer universe collapsed), so
   later years use demographic weight growth (SSA population by age × marital
   status) plus per-filer growth scaling of variables mapped to income
   categories, anchored to CBO's supplemental revenue projections, NIPA
   mortgage interest, and Giving USA charitable totals.
4. **Non-filer imputation** (`impute_nonfilers.R`). Synthetic non-filer
   records appended from Piketty–Saez–Zucman's 2017 public-use microdata, so
   reforms affecting non-filers (e.g., fully refundable credits) are scored on
   a full-population base.
5. **Variable imputation** (`process_puf.R`, `impute_variables.R`). Age
   (primary, secondary, dependents), gender, the earnings split on joint
   returns (Saez tabulations), and pass-through/QBI structure — SSTB status
   (20% assumption), employer status (Treasury TP-4 probabilities), and wage
   bills allocated to match observed 2018 QBI deduction aggregates.
6. **Future aging** (`project_puf.R`). Same method as 2018+ historical aging
   but with CBO demographic and 1040-aggregate projections (Long-Term Budget
   Outlook beyond the ten-year window). Economic projections are swappable —
   an alternative macro scenario is just a different Macro-Projections
   vintage.

**Data caveats to keep in mind downstream** (from Tax-Data's own notes):
QBI/SSTB structure is imputed and calibrated *by construction* to 2018
aggregates — not robust to reforms that dramatically restructure §199A;
several TCJA-censored variables are projected coarsely; some 2015/2017-era
hard-coding remains; the LP targeting algorithm currently runs for historical
years only.

## 4. Running the model

### 4.1 Runscripts

A runscript (`config/runscripts/*.csv`) is the recipe for a simulation: one
row per scenario, with `baseline` as a reserved ID. Required columns: `ID`,
`tax_law` (a directory under `config/scenarios/tax_law/`), `behavior`
(a module path under `config/scenarios/behavior/`, blank for none), `years`
(`start:end` — start at least one year before any policy change), `dist_years`
(years for the expensive distribution tables), `mtr_vars` / `mtr_types`
(space-delimited; paired). Optional columns: dependency overrides, and the
state-mode columns `states`, `state_tax_law`, `state_detail` (§10).

### 4.2 Invocation

```bash
Rscript src/main.R <runscript> <scenario_id|NULL> <user_id> <local> \
    <vintage|NULL> <pct_sample> <stacked> <baseline_vintage|NULL> \
    <delete_detail> <multicore>
```

`multicore` parallelizes across `scenario` or `year` (`none` on Windows;
`year` is unsafe for behavioral modules that require sequential years).
`baseline_vintage` reuses an existing baseline run rather than recomputing.

### 4.3 Execution flow

`main.R` → `parse_globals()` (parses the runscript, resolves interface paths,
validates cross-scenario consistency, creates the output tree) → for each
scenario, `do_scenario()`:

1. Build price/wage indexes and offsets (`generate_indexes()`; VAT price and
   excess-growth offsets), then `build_tax_law()` from the scenario's YAML.
2. `run_sim()` → `run_one_year()` per year: read microdata, join tax law by
   year × filing status, apply the SALT-workaround, SS COLA, capital, and
   excess-growth adjustments; run the **static pass** (`do_taxes()` + MTRs +
   detail write + totals); if behavioral modules are configured, run the
   **conventional pass** (feedback → `do_taxes()` again).
3. Post-processing: totals and receipts; then 1040 reports, revenue
   estimates, distribution, horizontal-equity, and time-burden tables
   (counterfactuals), or the CBO comparison (baseline only).

After all scenarios, `main.R` builds stacked reports (`stacked=1`): stacked
1040 reports, stacked revenue estimates, and stacked state revenue estimates —
incremental deltas in runscript order.

### 4.4 SLURM pipeline

`slurm_run.sh` distributes the same computation across cluster nodes
(setup → baseline year array → counterfactual year array → aggregation →
stacked phase). It **duplicates orchestration logic** from `main.R` /
`run_sim()` / `do_scenario()`; the sync table in `CLAUDE.md` maps which
changes require which `src/slurm/*.R` updates. Changes confined to
`run_one_year()`, calculator functions, YAML, or behavioral modules never
need SLURM syncing.

## 5. Tax law parameterization

Tax law lives in YAML files, one per thematic parameter group
(`config/scenarios/tax_law/baseline/*.yaml`: `ord`, `pref`, `agi`, `std`,
`item`, `pe`, `qbi`, `amt`, `niit`, `ctc`, `eitc`, `cdctc`, `char`, `below`,
`pr`, `rebate`, …). Each **parameter** contains **subparameters** holding a
time series of values plus optional indexation rules (`i_measure`,
`i_base_year`, `i_direction`, `i_increment`), with file-level
`indexation_defaults` and a `filing_status_mapper` for status-varying values.
`build_tax_law()` parses this into a wide year × filing-status table
(`ord.rates1`, `eitc.po_thresh_1`, …) that joins directly onto tax units.

**Reforms are subparameter-level overwrites, not merges.** A reform directory
contains only the YAML for parameters it changes, but each overridden
subparameter must carry its complete time series and indexation fields.
Common mistakes and full override semantics are documented in the
`policy-config` skill. Reforms that need new *structure* (a parameter that
does not exist under current law) extend the calculator function and the
baseline YAML together — the `rebate` and `agi_surtax` parameters exist for
exactly this kind of generic hook.

## 6. The federal tax calculator

`src/calc/` implements the calculator as pure functions of
(tax unit attributes, law parameters). `do_taxes()` orchestrates:
`derive_vars()` → payroll (`calc_pr`: FICA/SECA, OASDI/HI, Additional
Medicare, employer/employee split) → the 1040 pass (`do_1040()`, run twice
when charitable above-the-line vs. itemized optimization applies) →
filer/expanded-income flags → `remit_taxes()` (withheld vs. non-withheld
timing).

`do_1040()` sequence — the model's 1040 in code order:

```
calc_kg          Schedule D: taxable capital gains (incl. basis inflation option)
calc_agi         AGI, above-the-line deductions, taxable SS (calc_ss), excess business loss
calc_std_ded     standard deduction
calc_item_ded    itemized deductions (medical, SALT, interest, charitable, casualty, misc)
calc_pe_ded      personal exemptions
calc_qbi_ded     §199A QBI deduction
calc_below_ded   below-the-line deductions (tips, overtime, senior)
calc_txbl_inc    standard-vs-itemized choice, taxable income
calc_tax         regular tax: ordinary brackets + preferred-rate buckets (incl. 1250/collectibles)
calc_alt_max     Alternative Maximum Tax cap (IRC §1A)
calc_amt         Alternative Minimum Tax
calc_cdctc … calc_eitc
                 credits in order: CDCTC, education, saver's, (caregiver: disabled),
                 CTC/ACTC/ODC, EITC
calc_rebate      generic per-person refundable credit (stimulus/UBI hook)
calc_wage_subsidy  individual-level wage subsidy hook
calc_niit        Net Investment Income Tax
calc_agi_surtax  generic AGI surtax hook
calc_liab        credit allocation (refundable/nonrefundable), refund, net liability
```

**MTR machinery** (`calc_mtrs()`): EMTRs are computed by perturb-and-recompute
on the full calculator. `"nextdollar"` adds $1 to the variable (correctly
propagating composite variables, e.g. `tips1` → tips → wages) and takes the
liability delta; `"extensive"` zeroes the variable and divides by its original
value (average rate on the whole amount). MTRs feed behavioral modules and the
published EMTR outputs; requesting them in the runscript is what makes them
available to modules.

## 7. Behavioral feedback

Behavioral modules (`config/scenarios/behavior/{type}/{name}.R`) implement
`do_{type}()` functions receiving the full tax-unit frame plus baseline and
static-counterfactual MTR frames, and returning the modified frame. They run
at the start of the conventional pass, so the conventional estimate embeds the
response. The helper `apply_mtr_elasticity()` (`src/sim/behavior.R`) covers
standard elasticity applications with four functional forms (`semi`, `arc`,
`netoftax`, `taxprice`); heterogeneous elasticities, probabilistic extensive-
margin responses (with `set.seed(globals$random_seed)`), and arbitrary logic
are all supported — modules are ordinary R. The behavioral-modules paper in
`docs/behavior_modules.pdf` develops the framework in full.

## 8. Outputs

Per scenario, under `{output_root}/{ID}/`:

```
{ID}/
├── static/                          # always
│   ├── detail/{year}.csv            # per-unit microdata incl. mtr_* columns
│   │   └── state/{year}.csv         # state mode + state_detail=1: id × per-state liability
│   ├── totals/                      # payroll.csv, 1040.csv, 1040_by_agi.csv,
│   │   ...                          # receipts.csv, state.csv (state mode)
│   └── supplemental/
│       ├── tax_law.csv              # the parsed law actually used
│       ├── distribution.csv         # counterfactuals, dist_years only
│       ├── horizontal.csv           # horizontal-equity ETR dispersion
│       ├── time_burden.csv          # compliance time burden
│       ├── state_rev_est.csv        # state mode, counterfactuals
│       ├── 1040.xlsx                # 1040 report
│       └── cbo_comparison.csv       # BASELINE ONLY: line-by-line vs CBO
└── conventional/                    # counterfactuals (copied from static if no behavior)
    └── (same tree; revenue estimates are conventional-aware)
```

Revenue estimates (`revenue_estimates.csv`, deltas vs. baseline) are written
by `calc_rev_est()`; stacked variants land at the vintage root. Distribution
is a static concept and exists only under `static/`. `delete_detail=1` purges
the large detail files after post-processing.

## 9. Validation and testing

- **CBO comparison** (baseline-only, automatic): every baseline run writes a
  line-by-line comparison of the simulated 1040 build-up against CBO's
  published baseline detail (`resources/cbo/`).
- **Cross-model, federal**: `src/tests/test_taxsim.R` maintains a
  record-level crosswalk to NBER TAXSIM-35 (run locally via `usincometaxes`
  WASM). The crosswalk was substantially repaired in July 2026 (a dozen
  latent input bugs; QBI inputs now mapped). Known remaining federal-side
  divergences are cataloged with a review policy in
  `other/state_tax_research/cross_model/federal_divergences.md` —
  notably un-root-caused EITC amount disagreements and a p99 federal-AGI
  tail vs. TAXSIM.
- **Unit tests**: `src/tests/` (tax law parsing regression tests, state
  worksheet tests, employment-elasticity module tests that run on source).
- **State cross-model harness**: §10.4.

## 10. The state income tax module

*Status date: 2026-07-19, branch `state-tax`. Design of record:
`other/state_tax_research/state_tax_implementation_plan.md` (seven phases);
running status: `other/state_tax_research/STATUS.md` and
`state_weights_phase1_summary.md`.*

### 10.1 Architecture

State taxes are a **strictly downstream, vectorized layer**: the federal
calculator runs once, state-blind, and each state's calculator then runs on
the federally-calculated units. Three structural commitments:

1. **Split state weights.** Each record's national weight is split across 53
   jurisdictions (50 states + DC modeled; PR and OA as no-tax buckets):
   `Σ_st w(i,st) = w_i`. National aggregates are therefore invariant to state
   mode *by construction* — acceptance-tested: with-state federal outputs are
   byte-identical to without-state.
2. **Parameter reuse.** State law lives in per-state YAML under
   `config/scenarios/tax_law_state/{baseline|reform}/{st}/`, parsed by the
   same machinery as federal law with `st_` prefixing, primary-source
   citations on every subparameter, and credit tables carried as attributes.
   Reform overlays work exactly like federal ones (`state_tax_law` runscript
   column).
3. **One-way federal → state in v1.** No SALT circularity, no federal↔state
   iteration (TAXSIM does three rounds), no PTET modeling. Planned for
   Phase 7.

Runscript surface: `states` (codes or `all`; baseline row must be a superset
of every counterfactual — validated), `state_tax_law`, `state_detail`.
Per year inside `run_one_year()`: build state law → validate federal
conformity (fixed-date-conformity states like CA and SC are *guarded*: they
are excluded from `states=all` and refuse federal-reform runs until frozen-
base bridge law is available) → build reference contexts (re-running federal
variables under the frozen law where needed) → build weights → static and
conventional passes each call `get_state_totals()`, which loops states
through `do_state_taxes()` — `calc_st_agi → st_ded → st_exempt → st_txbl →
st_tax → st_credits → st_liab` plus `st_special` for narrow taxes (WA capital
gains excise, WFTC; NH/TN interest-and-dividends taxes) — and aggregates with
the split weights. Outputs: `totals/state.csv`, `supplemental/state_rev_est.csv`
(counterfactual deltas), optional compact per-year state detail matrix, and
stacked state revenue estimates.

### 10.2 What is encoded (Phase 2/3, done)

25 states have baseline configs: 14 broad-income-tax states (AZ CA CO CT GA
IL IN KY MI NC ND NY SC VA) plus OH and UT (added July 2026), the narrow-tax
states NH/TN, WA's excise+credit regime, and six no-tax stubs (AK FL NV SD TX
WY). Encodings run 2017-forward from forms and statutes, including structural
features like NY's tax-benefit recapture, CO's TABOR rate history, OH's
Business Income Deduction and ordered credit stack, and UT's taxpayer tax
credit. Each state's known modeling gaps are documented in
`other/state_tax_research/state_parameter_rollout.csv` (the tracker).

### 10.3 State weights (Phase 1) — decided, not yet in production

The production dispatcher **still returns a uniform placeholder split**, so
state-level *totals are not yet meaningful*; all plumbing and contracts are
real. The bake-off concluded 2026-07-19
(`state_weights_phase1_summary.md` has methods, results, and the TPC/OTA
comparison):

- Targets: 22 SOI Historic Table 2 series × AGI stub × state for filers
  (share-normalized to PUF national totals) + ACS cells for non-filers.
- Method: counts-backbone IPF prior → joint gradient fit (softmax
  parameterization with KL anchor — the Deville–Särndal exponential-tilting
  family; per-target IPF was *proven structurally unable* to satisfy ~21
  constraints per cell).
- Chosen configuration: β=1e-4, 3000 cosine-annealed Adam steps — **95.3% of
  targets within 2% (MARD 0.43%)**, best untargeted generalization, healthy
  weight diagnostics. Tighter anchors fit better on paper (96.9%) but
  degrade held-out geography and weight quality — rejected as overfitting.
- Pilot liability under candidate weights: IL within ~1% of external
  benchmarks; CO consistent once TABOR refunds are accounted for; NY
  overshoots even after the (large) PTET adjustment.

### 10.4 Cross-model validation (Phase 5) — running, triage open

A record-level harness (`other/state_tax_research/cross_model/`) validates
each state's calculator against **TAXSIM-35 (2017–2020)** and **PolicyEngine
US (2021–2024)** on stratified PUF samples, with a federal-alignment "clean
subset" filter, a machine-readable known-differences list (with predicate-
based exclusions), and per-state markdown verdicts. Status: **9 states pass**
(the six stubs, NH, TN, IL); **16 remain in triage**, with failure clusters
identified (deductions-stage for CA/KY/NY/VA — SALT-circularity-flavored;
state-AGI-stage for MI/NC; exemptions for IN/AZ/GA; near-threshold CT/WA/OH/UT).
Notable finding: **neither TAXSIM nor PolicyEngine models the Ohio Business
Income Deduction** — validated exclusions carry it. Upstream-shareable issues
for both external models are drafted in
`cross_model/external_model_issues.md` (not yet sent).

### 10.5 Open questions and needed improvements (flagged)

**Blocking state results being publishable:**

- [ ] **Wire the decided weights into production** (`build_split_weights(
  method="gradient")` with the chosen hyperparameters; per-year
  `state_weights_{year}.csv` writer; flip the tracker's `blocked_weights`
  aggregate column). Until then every state total is a placeholder.
- [ ] **Weights are fit on 2022 only.** The 2017–2024 panel needs per-year
  fits (HT2 exists for all years) and a policy for projected years — the
  amortized multi-year approach in the alternatives memo is the candidate.
- [ ] **Cross-model triage for the 16 open states** — each needs its residual
  clusters explained (known-difference row or bug fix) to reach the ≥95%
  clean-match acceptance bar.

**Known weaknesses in the weights (documented in the Phase 1 summary):**

- Untargeted geography is far from targeted quality (MARD ~22% held-out vs
  0.4% targeted); retirement income, social security, and business income
  geography are the weak dimensions — **demographic target expansion**
  (QWI sex×age, ACS marital×age; fetchers already built) is the ranked #1
  improvement.
- A 2.3% structural core of targets (thin PUF support × extreme HT2 shares,
  top AGI stubs) cannot be fit by any configuration — needs assembly-level
  pruning/reporting.
- Filing-propensity geography leaks into count targets (return-count errors
  correlate −0.6 with state EITC take-up); take-up stays an untargeted
  covariate, and a filing-rate covariate for the non-filer partition is the
  candidate fix.
- `kg_amt` and negative business income are excluded (sign-mixed);
  the dual-space maxent alternative would reclaim them.

**Concept and coverage gaps (by design in v1, need eventual resolution):**

- [ ] **Nonresident and cross-border liability** (NY nonresidents alone:
  ~$8bn/yr): residence-based weights cannot represent it; LODES
  origin-destination commuter matrices are the planned Phase 7 basis.
  Reciprocity agreements and other-state credits are in the same bucket.
- [ ] **PTET (pass-through entity tax) credits** are unmodeled — material for
  NY/IL/CA liability levels post-2021 and the main wedge in the NY pilot.
- [ ] **SALT circularity / federal↔state iteration** (Phase 7) — currently
  one-way; matters most for itemizers near the standard/itemized margin.
- [ ] **Local income taxes** (NYC, Yonkers, MD counties, OH school districts):
  planned as a second-level weight split using SOI county data (plan §2.6).
- [ ] **Fixed-date conformity bridges** for CA and SC (frozen-federal-base
  overlays) — until built, those states are guarded against federal-reform
  runs.
- [ ] **State EITC take-up**: state credits currently assume full claiming
  among filers; fine for validation, a bias for revenue estimates.
- [ ] **Behavioral feedback is federal-only**: no migration responses, no
  state-MTR-driven behavior; state EMTRs are not yet computed.

**Process/infrastructure:**

- [ ] The cross-model harness's raw per-year files hold only the most recent
  run's state set — partial reruns silently strip other states' stage
  diagnostics from regenerated reports (manual merge required today).
- [ ] `STATUS.md` in the state research directory trails the running log;
  the tracker CSV (`state_parameter_rollout.csv`) is the current source of
  truth for per-state status.
- [ ] Send the drafted TAXSIM/PolicyEngine issue reports upstream.

## 11. Known limitations and open questions — federal model and data

- **QBI/SSTB structure is imputed** and calibrated to 2018 aggregates by
  construction (§3); treat §199A restructuring reforms with caution.
- **Federal cross-model divergences** await review: EITC amount
  disagreements with TAXSIM are not root-caused; the federal AGI p99 tail
  (~+$14k vs TAXSIM) has untraced candidates (taxable SS, capital-loss
  limitation interplay). See `cross_model/federal_divergences.md` — policy
  is document-then-condition-away for state validation, but someone should
  confirm none indicates a bug in our federal calculator.
- **PUF vintage**: the 2015 base is aged nine-plus years; distributional
  detail for post-2015 phenomena (gig income composition, post-TCJA
  itemization behavior) rests on imputation and aggregate calibration.
- **Caregiver credit** is implemented but disabled (hard-set to 0) in
  `do_1040()` pending design decisions.
- **Documentation debt**: the `docs/website/` R Markdown site is ~19 months
  stale (December 2024) and predates the QBI crosswalk work, recent runscript
  options, and the entire state module; `post_processing.Rmd` is a stub, and
  the user guide has an empty data-zip link. This document is the interim
  replacement; the website should either be regenerated from it or retired.
