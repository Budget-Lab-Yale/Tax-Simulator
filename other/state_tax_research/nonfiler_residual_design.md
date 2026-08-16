# Non-Filer Estimation — Residual-Methodology Redesign (Design Memo)

**Date:** 2026-08-16
**Status:** Design only — nothing in this memo has been run or implemented.
**Prompted by:** the Affordability-Project income memo (`docs/Income.docx`,
"Filing status", "Calibration A — the universe mismatch", and "Aligning the
code" sections), which specifies a residual non-filer methodology and
explicitly asks that its upgraded filing model replace the v0 rule the state
weights currently rest on.
**Companions:** `state_weights_phase1_summary.md` (bake-off record; §7 ranks
the improvements this memo addresses), `state_weights_fit_issues.md` (v0
filing-model bias note), `state_tax_implementation_plan.md` §2.1 (weights
design), `STATUS.md` (Phase 1 close-out checklist).
**Code discussed:** `src/data/state_weights.R` (Tax-Simulator, this branch);
`src/impute_nonfilers.R`, `src/project_puf.R` (Tax-Data, main @ d5f1f51).

This memo re-considers how non-filers are estimated in (1) the national
production PUF built by Tax-Data and (2) the state split-weights fit on this
branch, and designs how the income memo's residual methodology — non-filing
adults as a population-minus-filers residual, disciplined by SSA age and
earnings margins, imposed jointly with weight calibration — applies to both.
It specifies (but does not run) the diagnostic harness that turns the design
decisions into evidence, and sequences the work across the two repos.

---

## 1. Purpose and scope

Two systems currently carry a non-filer population, and neither count is
anchored to anything:

- **Tax-Data** appends ~27.6M (TY2022) non-filing tax units from PSZ/DINA to
  the PUF. Their weights are DINA's own, never calibrated; every target in
  `config/target_info/baseline.csv` is a filer concept.
- **Tax-Simulator's state weights** place those units across states using
  ACS margins built with a v0 filing rule known to over-assign filers by ~7%
  nationally, with a 20pp state spread.

The income memo's residual methodology gives both systems the anchor they
lack: non-filing adults by state and age = Census PEP population (net of
group quarters) minus filing adults derived from HT2 by filing-status
identities, age-shaped by OASDI beneficiary counts and earnings-shaped by
SSA covered-worker tables. This memo translates that method into these two
codebases. Scope: design and diagnostics specification only. No code
changes, no data fetched, no fits re-run.

## 2. Current state

### 2.1 Tax-Data: the PSZ append

`src/impute_nonfilers.R` (148 lines) reads `usdina2017.dta`, filters to
DINA's `filer == 0`, collapses person rows to 13,204 tax units, and
`bind_rows`-appends them to the 2017 PUF (`impute_nonfilers.R:145-146`).
Pipeline position: `main.R:41`, after the LP reweight
(`create_2017_puf.R`), before `impute_variables.R`. Weights are DINA's tax-
unit weights, `dweghttaxu / 1e5` (`impute_nonfilers.R:22`), unadjusted.

Resulting non-filer mass (production vintage `model_data/Tax-Data/v1/
2026070814/baseline`, verified 2026-08-16): 26.18M units in 2017, 27.62M in
2022, 32.54M in 2035. Filing-status mix (2022): 10,989 single / 2,037 MFJ /
178 HoH; `dep_status = 0` on every record (no non-filer is ever a
dependent; no MFS).

Six defects, in rough order of downstream damage:

1. **Investment income is identically zero.** `fiint`, `fidiv`, `fikgi` are
   DINA *fiscal-income* variables — income as reported on returns — and are
   zero for non-filers by construction. Verified in the 2022 output:
   weighted non-filer `txbl_int`, `div_ord`, `div_pref`, `kg_lt` are all
   $0.000B, against `wages` $147.3B and `gross_ss` $244.7B. Consequences:
   (a) the state fit's non-filer `income_tier()` cells are assigned on an
   understated income proxy; (b) in Tax-Data's new wealth stack, non-filers
   are mis-placed by `compute_broad_income()`
   (`src/imputations/helpers.R:192-241`), which drives SCF wealth-cell
   assignment and DFA aging buckets.
2. **Dividends are silently dropped even where DINA has them.**
   `impute_nonfilers.R:46` writes `qual_div`, a column that does not exist
   in the output schema (`config/variable_guide/baseline.csv` defines
   `div_ord`/`div_pref`); `write_outputs.R`'s `out_cols` intersection
   discards it.
3. **Ages are a three-point band.** DINA `ageprim` takes three values, so
   `impute_nonfilers.R:92-96` draws age groups uniformly within coarse
   bands: 2017 output has bands 1/2/3 at 1,514/1,456/1,489 records and
   4/5 at 2,341/2,433 — flat by construction — with 3,971 records (10.5M
   weighted) piled into band 6 (65+). The state fit's non-filer cells are
   `age_band × income_tier`, so this smear propagates directly into state
   placement.
4. **No dependent non-filers, no MFS** (`impute_nonfilers.R:25-26`) —
   acceptable v1 approximations, but undocumented ones.
5. **`filer = 0` is set only implicitly**, via the zero-fill of
   `remaining_vars` (`impute_nonfilers.R:136-142`). Nothing asserts it. The
   flag is load-bearing for both the CTC/rebate become-filer logic
   (Tax-Simulator `src/calc/do_taxes.R:126-127`) and the entire state-
   weights partition.
6. **The aging path is unmoored.** `project_puf.R`'s
   `compute_weights_for_year()` grows non-filer weights by married-only
   population factors for 2018-19 (`:333-341`, the only filer-aware branch)
   and by pure married×age demographics from 2020 on (`:344-365`) — no
   filer term at all. The filer/non-filer split therefore drifts with
   demographics regardless of what return counts do, exactly the
   universe-mismatch failure the income memo's worked example describes.

The stated rationale for the whole approach is one sentence in
`docs/model_documentation.md:84-87` ("synthetic non-filer records appended
from PSZ… so reforms affecting non-filers are scored on a full-population
base"). No limitation is documented anywhere.

### 2.2 Tax-Simulator: the v0 non-filer treatment in the state weights

The split-weights fit partitions records on the Tax-Data `filer` flag
(`src/data/state_weights.R:639` filers, `:698` non-filers). The filer
partition targets 22 HT2 series × 10 stubs × 52 areas (10,229 share-
normalized targets). The non-filer partition gets **count-only** targets on
1,390 `state × age_band(7) × income_tier(5)` cells (`:696-736`, x ≡ 1 at
`:723`), with priors and cell totals from `build_acs_margins()` (`:242-313`)
— the **v0 filing rule**: pointer-built tax units, filer iff unit
`sum(pmax(INCTOT, 0)) ≥` a hardcoded standard-deduction table
(`filing_threshold()`, `:182-192`). Because each non-filer cell has single
membership and count-only targets, the calibration engine reproduces the
ACS margin *exactly* in one pass (`state_weights_phase1_summary.md` §2) —
the non-filer "fit" is pure prior reproduction of the v0 margin, errors
included.

Known problems, two previously documented and one new:

- **−7% filer bias.** v0 produces 148.2M ACS filer units vs HT2's 159.7M
  TY2022 returns (`state_weights_fit_issues.md:84-91`), from the omitted
  age-65 standard-deduction bump, the $400 SE rule, dependent filers, and
  above/below-threshold filing behavior.
- **Filing-propensity leak.** The postmortem found state `n_returns` errors
  correlate −0.61 with EITC take-up (`state_weights_phase1_summary.md`
  §5.3) and pre-registered "a filing-rate covariate for the non-filer
  partition" as the fix (§7).
- **NEW — group-quarters persons are untreated.** `build_acs_margins()`
  never reads or filters `GQ` (verified 2026-08-16: the variable appears
  nowhere in `state_weights.R`): the ~8M group-quarters persons enter the
  unit builder as mostly income-less single-person "units" and land
  overwhelmingly in the non-filer margins. Whether that is a level error
  depends on the universe (§3.0): unlike the Affordability spine, the PUF
  universe INCLUDES GQ residents, so their presence in the margins is
  correct in principle — but the v0 treatment is wrong in composition
  (dorm students counted as independent non-filer units in the college's
  state, when they are mostly dependents claimed on parents' returns, often
  in another state), and the income memo's GQ warning bites wherever the
  anchors and the margins disagree about who is in the universe.

One stale comment worth killing when touched: `state_weights.R:166-167`
claims the extract lacks `SCHOOL`. The common extract
(`shared/raw_data/ACS/acs_common/us2022a/variables.csv`) carries `GQ`,
`SCHOOL`, `EDUC`, `RELATE`, `EMPSTAT`, `LABFORCE`, `SEX`, `RACE`, `HISPAN`,
`INCWAGE`, `INCBUS00`, `INCSS`, `INCSUPP`, `INCWELFR`, `FOODSTMP`,
`INCRETIR`, `INCINVST`, `HINSCAID`, `HINSCARE` — nearly the full Cilke
(1998) covariate set. **No new IPUMS pull is needed for anything in this
memo.**

## 3. The residual methodology, restated for these codebases

### 3.0 The universe question (why GQ treatment differs from the Affordability spine)

The income memo subtracts group quarters from PEP because its ACS spine
*excludes* GQ records — universe matching for a household-only file. The
PUF universe is different: DINA targets the full resident population aged
20+. Verified 2026-08-16 against the pinned vintage: `usdina2017.dta`
adults sum to 241.96M, matching PEP's 2017 resident 20+ population (~242M),
not the household-only population (~235M). The Tax-Data non-filer mass
(31.6M adults / 26.18M units in 2017) therefore already contains GQ
non-filing adults — prisoners, most nursing-home residents, non-filing
students. Three consequences:

- **Anchors use the full resident population.** Non-filing adults by state
  = PEP resident adults (NO blanket GQ subtraction) − HT2 filing adults. A
  GQ-subtracted anchor would rake genuine non-filer mass *out of* exactly
  the prison/college/nursing-home states the income memo worries about —
  the same error with the opposite sign.
- **The invariant is universe consistency, not exclusion.** The same GQ
  population must sit inside (or outside) all three objects: the Tax-Data
  non-filer partition, the PEP-side anchor, and the ACS margins. Blanket
  exclusion could only be made consistent by also carving GQ adults out of
  the DINA append, which is not possible — DINA carries no GQ flag.
- **An adult-dependent netting refinement follows** from the same
  adult-space accounting: the residual includes adult dependents claimed on
  filed returns, who are neither filing adults nor non-filer-unit heads —
  they ride filer records whose state placement the filer partition already
  determines. The non-filer-partition target is therefore PEP adults −
  filing adults − adult dependents claimed on returns, the last estimated
  from the HT2 dependents identity net of the under-18 population (T2/T5
  material; carried in the tolerance if too noisy to estimate directly).

### 3.1 The anchors

For each HT2 year (2014, 2016-2022; 2017 and 2022 first):

1. **Filing adults by state** from HT2 filing-status identities — already
   implemented in `compare_individuals_acs_irs()`
   (`state_weights.R:349-352`), with the QSS/MFS and adult-dependent
   caveats documented at `:325-341`:
   `married = 2·n_joint + (n_returns − n_single − n_joint − n_hoh)`;
   `single = n_single + n_hoh`; `dependents = n_indiv − (n_returns +
   n_joint)`. TY2022 coverage: married 85.6% of ACS married adults, single
   77.6%, children 109.2%.
2. **Population** = Census PEP resident state × age — no GQ subtraction,
   per §3.0. (GQ composition by type × state is still tabulated from the
   on-disk IPUMS extract, with a national cross-check against ACS table
   B26001 — but as the T7 diagnostic and the dorm-student dependent share,
   not as a subtraction.)
3. **Residual non-filing adults by state** = (2) − (1), net of the
   adult-dependent adjustment in §3.0. An anchor with an explicit
   tolerance, not an exact count (return-state vs residence and
   facility-state vs tax-residence wedges, status-mapping wedges, vintage
   differences — income memo fn. 8).
4. **Age shape**: 65+ anchored by SSA OASDI beneficiaries by state; working
   ages shaped by the covered-worker margin (SSA persons-with-wages minus
   HT2 returns-with-wages; nationally ~75% ± 9pp across states); remainder
   smoothed by the upgraded ACS non-filer age shape.
5. **Cross-checks, never targets**: QCEW state wage totals on the dollar
   side; the QWI/LODES fetchers already in `state_weights.R` stay
   diagnostics.

### 3.2 The filing model

- **v1a — deterministic upgrades** to `build_acs_margins()`:
  **differentiated GQ treatment** in place of the v0 non-treatment — keep
  institutional residents (`GQ == 3`) as own-state non-filer units unless
  income makes them filers; reclassify college-age dorm residents
  (`GQ == 4`, in school, age < 24) as dependents rather than unit heads
  (they are claimed on parents' returns, generally elsewhere, and HT2
  already counts them in N2); leave military barracks residents to the
  income test (most have wages and classify as filers); report GQ weight by
  type and state. Then: extend `filing_threshold()` to add the age-65
  additional standard deduction; add the $400 SE rule via `INCBUS00`; let
  dependents with own income above the dependent filing floor form filing
  units; use `SCHOOL` to keep 19-24 household students dependent.
- **v1b — probabilistic layer**, behind a `filing_model = c("v1a","cilke")`
  argument: below the threshold, Cilke (1998) group probits
  (hand-transcribed coefficients as a repo resource) with group constants
  calibrated so ACS filer counts hit the HT2 identities by state ×
  dependent status; above the threshold, an IRS Pub 5785 non-filing hazard
  — a national scalar for v1 (~10.6M above-threshold non-filers, TY2014
  level) allocated by the publication's relative risks.
- **Acceptance metric**: v1 ACS filer units vs HT2 `n_returns` by state.
  The −7% national bias and its 20pp state spread should collapse to
  within the stated tolerance.

### 3.3 "Impose it jointly" — what that means here

The income memo's worked example (7% filer over-assignment → raking drains
the non-filer pool → state income-per-adult biased up 7% while every
targeted diagnostic looks perfect) does **not** apply mechanically inside
`build_split_weights()`: the filer and non-filer partitions are fit
separately, and the row constraint `Σ_s W[i,s] = w_i` holds within each
partition, so weight cannot drain between filers and non-filers *inside the
state fit*. The failure mode lives in two other places, and the joint-fit
requirement translates accordingly:

1. **The national filer/non-filer split** is set upstream in Tax-Data and
   is currently unmoored (§2.1, defects 3 and 6). Fix: a national residual
   calibration of non-filer weights in Tax-Data (§5.2), and an aging path
   that keeps the split consistent with projected return counts (§5.3).
2. **The state placement of the non-filer partition** currently reproduces
   a biased margin exactly. Fix: compute the non-filer state targets as the
   arithmetic residual of the *same HT2 vintage* the filer partition is fit
   to (§6.2). Then, if both partitions hit their targets, fitted state
   adult populations reproduce PEP−GQ **by construction** — the population
   identity is enforced through target self-consistency, not through a
   stacked optimization.
3. **A population-identity diagnostic** in `validate_state_weights.R`:
   fitted filer adults + fitted non-filer adults per state (× age band
   where supported) vs PEP−GQ, with the anchor tolerance. If soft-target
   trade-offs push this beyond tolerance, the escalation path is a single
   stacked fit — `fit_gradient()` already accepts arbitrary row sets, so
   concatenating `(w, P0)` across partitions and adding identity targets
   spanning both is mechanical — but it should not be built speculatively.

## 4. Diagnostic harness (specified, not run)

Home: `other/state_tax_research/nonfiler_residual/`, numbered scripts
sourcing `src/data/state_weights.R`, scratch under the existing
`state_weights_tmp/` convention. The ACS tabulation steps run under
`sbatch` (login node OOM-kills near 5-8 GB; the weights inputs alone are
~1 GB serialized). Everything else is login-node safe.

### 4.1 `01_fetch_residual_inputs.R` — data acquisition

Fetchers follow the `fetch_qwi()` pattern (`state_weights.R:429`): small
functions, source caveats in comments, paths derived from
`raw_data_root()`, never hardcoded. New shared-store families mirror the
existing store layout (each with a manifest):

| Series | Source | Proposed store |
|---|---|---|
| PEP state × single-year-age × sex, 2020-2024 vintage | census.gov popest `sc-est2024-syasex.csv` (verify filename at fetch time; civilian variant as sensitivity) | `raw_data/Census-PEP` |
| PEP intercensal 2010-2020 (back-year anchors) | census.gov popest intercensal state files | same |
| Group quarters by state × age | tabulated from the on-disk IPUMS extract, `GQ ∈ {3,4}` × `STATEFIP` × age band; national check vs ACS B26001 via the Census API | derived (script output), not a raw store |
| OASDI beneficiaries by state (65+ rows) | SSA statcomps `oasdi_sc/{year}` | `raw_data/SSA-OASDI-SC` |
| Covered workers: persons and wage dollars by state | SSA statcomps `eedata_sc/{year}` | `raw_data/SSA-EEDATA-SC` |
| QCEW state annual wage totals | BLS CEW annual singlefile | `raw_data/BLS-QCEW` |
| Pub 5785 above-threshold composition | hand-transcribed CSV with page citations | repo: `nonfiler_residual/resources/pub5785_hazard.csv` |
| Cilke (1998) probit coefficients | hand-transcribed CSV (9 group equations) | repo: `nonfiler_residual/resources/cilke_coefs.csv` |

Note: `raw_data/SSA-Demographic/v3` was checked and holds only national
series — not usable for the state margins. Also settle the HT2 store
duplication first (§7.4) so the new families land under one convention.

### 4.2 `02_build_residual_anchors.R` — the anchor computation

First refactor, then compute: promote the identities out of
`compare_individuals_acs_irs()` into an exported `ht2_filing_persons(ht2)`
returning `(state, married_filing_adults, single_filing_adults,
dependents)` — one definition per computation; the diagnostic, the target
builder, and (per the income memo) Affordability-Index all call the same
function. Then compute §3.1 steps 2-4 and emit the cross-repo artifacts:

- `residual_anchors_{year}.csv` — `(state, age_band, nonfiling_adults,
  tolerance)` plus a national row;
- `nonfiler_wage_margin_{year}.csv` — covered-worker minus HT2 wage-return
  counts by state, with the returns-per-person ratio and QCEW dollar check.

### 4.3 `03_diagnose_current_nonfilers.R` — the re-consideration tables

Reads the production `tax_units_{year}.csv` non-filer slice, the v0
margins, and the anchors; writes `results/` CSVs and a findings memo in the
style of `state_weights_phase1_summary.md`:

| Table | Question it answers |
|---|---|
| **T1 National level** | PSZ non-filer adults (Σ w·(1+married)) and units vs the residual anchor vs Pub 5785's above-threshold 10.6M + implied below-threshold mass, TY2017/2022 |
| **T2 Age composition** | non-filer adult shares by the 7 `age_band()` bands: PSZ (flat within 3 DINA bands) vs the OASDI-anchored shape; 65+ share vs OASDI directly |
| **T3 Income composition** | share of non-filer units with wages/interest/dividends/gains > 0 (the latter three identically zero today) and the `income_tier()` distribution under current zeros vs a repaired proxy |
| **T4 Aging path** | 2017-2035 non-filer mass from the weight ledger vs an anchor-implied path (projected adults × held filing rates) vs projected return counts — quantifies the drift |
| **T5 State margins** | per state: PEP adults, GQ, filing adults, residual, v0 ACS non-filer units, v0 with GQ excluded; the −7% filer check by state; correlation of the v0-vs-residual gap with EITC take-up (reuse `EITC_TAKEUP` from `sweep_state_weights.R`) |
| **T6 Cell support** | residual anchor sizes by state × age band — feeds D2 |
| **T7 GQ composition** | GQ adults by type (institutional / dorm / military) × state × age from the extract; non-filer mass at stake per state under exclusion vs differentiated treatment; the dorm-student dependent share — sizes D4 |

### 4.4 Decision points the diagnostics feed

| # | Decision | Recommendation |
|---|---|---|
| D1 | Does Tax-Data need a national non-filer calibration? | Expect yes from T1/T2/T4; adopt §5.2 |
| D2 | Direct state×age residual margins vs the TPC low-AGI-coefficient fallback | Direct margins with explicit tolerance; collapse age bands where T6 shows thin cells |
| D3 | Above-threshold hazard: national scalar vs cell-varying | Scalar for v1 (the income memo's own defensible default); revisit if T5 says the state fit is driven by it |
| D4 | GQ handling: blanket exclusion (as in the Affordability spine) or differentiated treatment? | **Differentiated** (§3.0/§3.2): the PUF universe includes GQ residents (verified against DINA totals), so exclusion is universe-inconsistent and would drain genuine non-filer mass from GQ-heavy states. The dorm-student reclassification + institutional retention is decision-independent and should still ship ahead of the rest, sized by T7 |
| D5 | Non-filer target space: adults or units? | Adults: x-vector `1 + (filing_status == 2)` instead of the current x ≡ 1, since the anchors are adult counts |
| D6 | Age allocation of the state residual | Layered (OASDI 65+, covered-worker working ages, ACS smoothing); decided by how well the layers reconcile in T2/T5 |

## 5. Overall-PUF rework design (Tax-Data)

All in Phase 1 (base construction) plus one Phase 2 fix. Nothing goes
through `module_deltas` — Phase 3 modules cannot add rows or reweight
(`materialize.R:264` matches by id) — and no forbes-splice row-adding is
needed: the PSZ record set stays; values and weights change.

### 5.1 Composition fixes in `impute_nonfilers.R`

- **Dividends**: route `fidiv` to `div_ord`/`div_pref` (minimal: all
  `div_ord`; better: split by the qualified share among bottom-two-stub PUF
  filers). Trivial; do regardless of everything else.
- **Investment income**: first action at implementation is to inspect
  `usdina2017.dta` for DINA's national-income counterparts to the fiscal
  `fiint`/`fidiv`/`fikgi` (DINA carries NI interest/dividend/equity
  returns). Option A: scale those to per-unit amounts. Option B (fallback):
  hot-deck from stub-1/2 PUF filers conditioned on age band × has-wages.
  Either beats identical zeros.
- **Age detail**: replace the flat `runif` draw
  (`impute_nonfilers.R:92-96`) with a within-band distribution from the
  national anchor age shape (committed as
  `resources/nonfiler_age_shape.csv`, produced by the Stage-D harness).
  **This is the single highest-value fix for the state weights**, since the
  non-filer state cells key on `age_band(age1)`.
- **Assertions**: set `filer = 0` explicitly; `stopifnot` on it and on
  `dep_status == 0` rather than relying on the zero-fill.
- **Out of scope for v1** (record as TODOs): dependent non-filers, MFS, an
  in-Tax-Data Cilke model. Cilke belongs where filing is modeled on survey
  records (the ACS margins here; later Affordability-Index); Tax-Data's
  DINA units arrive with a filer flag.

### 5.2 New `src/calibrate_nonfilers.R` (Phase 1)

Sourced in `main.R` between `impute_nonfilers.R` and `impute_variables.R`:
a post-append rake of **non-filer weights only** to the national residual
anchors by age band × marital status (≤14 cells — closed-form cell-ratio
adjustment; do not touch `reweight.R`'s filer LP). Targets read from a
committed `resources/nonfiler_targets_2017.csv` snapshot with a provenance
header, generated by the Stage-D harness — Tax-Data gains no HT2/PEP
readers and no server paths. Emits a before/after diagnostic table. This is
the national half of the income memo's "force the model and the counts to
agree."

### 5.3 Aging-path fix in `project_puf.R` (Phase 2)

In `compute_weights_for_year()`: make the non-filer path residual-by-
construction — each year's non-filer adult mass = projected adults (the
`demog` table already loaded) − projected filer adults (2018-19 from the
IRS return-count factors as now; 2020+ filer adults grow with the same
married×age demographics they already use), scaled within age×married
cells. Minimal change: one new factor table alongside
`population_factors_2020plus`, applied `if_else(filer == 0, …)` symmetric
with the existing filer branch at `:333`. Filer weights and the ledger
architecture are untouched.

**Priority order for the state weights downstream:** age detail (§5.1c) >
national level + aging (§5.2/§5.3) > investment income (§5.1b) > dividends
bug (trivial, always).

## 6. State-weights rework design (Tax-Simulator)

### 6.1 Filing model in `build_acs_margins()`

Implement §3.2: extend `read_acs_extract()`'s default `cols` with `GQ,
SCHOOL, EMPSTAT, SEX, EDUC, INCWAGE, INCBUS00, INCSS, INCSUPP, INCWELFR,
FOODSTMP, INCRETIR` (all present; fix the stale `:166` comment); v1a
deterministic upgrades unconditionally; v1b behind the `filing_model`
argument. The calibration of Cilke group constants to
`ht2_filing_persons()` totals by state × dependent status *is* the joint
filing-model/count step, executed on the ACS margin side.

### 6.2 Non-filer targets in `build_weight_inputs()` (`:694-736`)

- **Primary targets**: the residual anchors `(state × age_band)`,
  share-normalized like every other target (PUF non-filer national adult
  total × residual state share within age band), with the adult x-vector
  per D5.
- **Additional margins**: OASDI beneficiary counts as targets on the
  65-74/75+ bands; the covered-worker wage margin as a `has_wages` count
  target on the non-filer partition (with a documented tolerance for the
  returns-vs-persons concept gap). QCEW stays a diagnostic, never a target.
- **Income tiers move to the prior**: keep `income_tier` in the prior
  (upgraded v1 ACS shares) and demote the current 1,390 count-only cells
  from exact targets to prior-only or soft targets. The anchors own the
  level; the ACS owns the within-state shape. This turns the non-filer
  partition from exact-IPF reproduction of a biased margin into a genuine
  calibration — it should run through `fit_gradient()` (today it only ever
  sees `fit_calibration()`-trivial cells).
- **Hard prerequisite**: the Tax-Data age fix (§5.1c) must land first, or
  `age_band(tu_n$age1)` cell assignment stays smeared across the very
  dimension the anchors discipline.

### 6.3 Validation additions

- The population-identity check of §3.3 (fitted filer + non-filer adults vs
  PEP−GQ per state) in `validate_state_weights.R`, with tolerance.
- Re-run the EITC take-up correlation (postmortem §5.3); the −0.61 should
  attenuate.
- A new held-out metric where gains *are* expected: state adults by age
  band vs PEP (currently unscored anywhere).

### 6.4 Expected effects, stated honestly

This rework fixes the state placement of the ~27.6M non-filer units — the
income memo's headline income-per-adult bias — and the pre-registered
filing-propensity leak. It does **not** directly fix the filer-partition
held-out misses (taxable pensions 17.0, Schedule C 30.8, capital gains 61.0
MARD; `state_weights_phase1_summary.md` §5.2): those are a filer-target
poverty problem, complementary to §7-item-1 demographic target expansion.
Do not promise held-out MARD gains from the non-filer rework alone.

## 7. Cross-repo sequencing and interfaces

### 7.1 Roadmap

Sequencing decision (JI, 2026-08-16): the non-filer rework lands **before**
the production weights swap-in, so the swap-in fit happens once, on
upgraded margins — not fit-on-v0-then-re-fit. One exception this memo still
argues for: the differentiated GQ treatment (D4) is decision-independent
and should ship ahead of the rest, sized by T7 first.

1. **Stage D — diagnostics** (this repo, `nonfiler_residual/`): fetchers,
   anchors, tables T1-T7, findings memo, decisions D1-D6. ~1.5-2 weeks.
2. **GQ treatment fix** in `build_acs_margins()` (D4: dorm-student
   reclassification, institutional retention, GQ reporting), as soon as T7
   sizes it.
3. **Tax-Data rework** (§5): composition fixes, national calibration,
   aging fix; full pipeline re-run and new vintage. ~1-2 weeks + cluster
   run.
4. **State-weights rework** (§6) on the new Tax-Data vintage: margins v1,
   residual targets, re-fit (config-7 hyperparameters unless the sweep says
   otherwise), validation battery. ~2-3 weeks.
5. **Production swap-in** per the existing checklist
   (`state_weights_phase1_summary.md:260-263`, `STATUS.md` §Phase-1
   close-out): structural-core pruning, `build_split_weights(method =
   'gradient')`, `state_weights_{year}.csv` writer, dispatcher flip at
   `src/sim/run.R:433`.
6. **Cross-validation + handoff**: identity diagnostic, held-out battery,
   pilot-state liability re-check, memo updates (including a pointer from
   the income memo's "Aligning the code" section to this workstream).
   ~1 week.

### 7.2 Interfaces (one definition per computation)

- **`ht2_filing_persons()`**: single home in Tax-Simulator. Recommend
  splitting `state_weights.R` (1,003 lines) into `src/data/ht2.R` (reader,
  `HT2_TARGET_MAP`, stub logic, identities) and `src/data/filing_model.R`
  (thresholds, Cilke, hazard, `build_acs_margins`), leaving engines and
  assembly in place — this is what makes the income memo's "source the
  shared functions from Tax-Simulator" workable for Affordability-Index
  without dragging in the split-weight scaffolding it explicitly does not
  want.
- **`residual_anchors_{year}.csv`**: one builder (the Stage-D scripts,
  promoted to maintained code), three consumers — the state target
  assembly, Tax-Data (as a committed snapshot), Affordability-Index.
- **`filer` flag contract**: authority stays with Tax-Data; post-rework it
  means "residual-anchored non-filing unit." Document in both repos; add
  the missing assertion (§5.1).
- **`state_weights_{year}.csv`**: schema unchanged `(id, state, weight)`;
  vintage-tag fits (`v0-margins` vs `v1-residual`) so downstream consumers
  can tell them apart. Eventual home of the whole weights model remains
  Tax-Data ("migration changes a path, not the model") — the `ht2.R` /
  `filing_model.R` split makes that move easier, not harder.

### 7.3 Division of labor with the Affordability pipeline

Per the income memo's alignment section: the two state systems solve
inverse problems (this branch splits national weights; the ACS spine
calibrates in place), so what transfers is machinery, not architecture —
`ht2.R`, `filing_model.R`, the anchors, the identities, the diagnostics.
The upgraded filing model built here (§6.1) is exactly what the memo says
should replace the v0 rule; building it once, in this repo, serves both.
One universe caveat for that sharing (§3.0): the two systems legitimately
differ on group quarters — the ACS spine excludes GQ and subtracts it from
its anchors; the PUF includes GQ and must not — so any shared state margin
or anchor file must carry an explicit universe tag (`resident` vs
`household`) before either side reuses the other's numbers.

### 7.4 Housekeeping

Settle the HT2 store duplication before adding new store families:
`raw_data/IRS-GEO` and `raw_data/IRS-Ind` hold byte-identical HT2 files;
the IRS-GEO NOTES.md says IRS-Ind is the maintained mirror, and the income
memo names IRS-Ind as the single source. Either repoint `ht2_path()`
(one line) or record IRS-GEO as canonical — but decide once.

## 8. Open questions

- **DINA national-income variables**: availability and quality of the NI
  counterparts to `fiint`/`fidiv`/`fikgi` for non-filers — inspect the
  `.dta` at implementation (§5.1b decides A vs B on this).
- **Residual tolerance**: the anchor's stated tolerance must be quantified
  from the wedges (return-state vs residence, QSS/MFS residual, PEP/ACS/IRS
  vintage gaps) rather than picked; T5 provides the raw material.
- **PEP vintage consistency** (income memo fn. 8): which PEP vintage pairs
  with which ACS controls for each anchor year.
- **Back-year anchors** (2014, 2016-2019): needed only when back-year
  weights are fit; intercensal PEP and older statcomps availability to be
  confirmed in Stage D.
- **Above-threshold hazard geography** (D3 follow-on): if the scalar
  version leaves state-correlated residuals, the cell version needs a
  defensible state allocation — Pub 5785 is national.
