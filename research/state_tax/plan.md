---
title: "State Individual Income Tax Module — Plan"
role: plan
workstream: state_tax
status: current
updated: 2026-11-10
sot: self
supersedes: [research/archive/state_tax_plan_2026-10-22_pre-rewrite.md]
superseded_by: null
---

# State Individual Income Tax Module — Plan

What is decided, what is built, and what is next. The module is built and
validated record by record; this plan now looks forward. The plan as it stood
through the build (phases 0-6, the full as-built design, the weights
prototypes) is `research/archive/state_tax_plan_2026-10-22_pre-rewrite.md`.
Evidence behind the original design choices:
`research/state_tax/notes/state_tax_model_research_notes.md`.

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

### 2.1 to 2.4 As built (pointers)

The design of record for what exists is `docs/model_documentation.md` section 10.
In one paragraph: state weights are fit and published by Tax-Data and read here
through the `State-Weights` interface (`build_state_weights(method = "interface")`
in `src/data/state_weights.R`; `research/state_weights/MOVED.md`). State law is
one directory per jurisdiction under `config/scenarios/tax_law_state/baseline/`,
parsed by the federal machinery with an `st_` prefix
(`config/scenarios/tax_law_state/README.md` holds the conventions, and
`params_schema.yaml` there declares every parameter). The calculator is the
generic `src/calc/state/` stack, run per state after the federal pass inside
`run_one_year()`. Outputs are `totals/state.csv`, `supplemental/state_rev_est.csv`,
and the optional detail files described in the repo `CLAUDE.md`.

Three conventions from the build still bind new work:

- **Anchor every year-keyed value list at 2017**, the state law floor. A
  subparameter first keyed later produces an empty series when the simulation
  window ends before that year.
- **Every subparameter carries a `reference:`** to a form line or statute.
- **A missing later year silently reuses the last keyed value**
  (`generate_time_series()` in `src/data/tax_law.R`). Unless the subparameter
  has an `i_measure` rule, a dollar amount is therefore held fixed, not indexed.
  Section 3.3 exists because of this.

### 2.5 Coupled mode (deferred)

For reforms where state tax changes should feed federal SALT (or federal changes feed
AL/MO/OR bases): iterate federal↔state ≤3 rounds on *liability* (TAXSIM precedent),
replacing `salt_inc_sales` with computed state income tax + an imputed sales-tax
election for non-income-tax states (Pub 600-style regression — separate task). Only
itemizers (~9% of filers) trigger the loop; run it on that subset. Not needed for
state-static estimates.

### 2.6 Local income taxes (deferred; design sketch, JI 2026-07-12)

Eventually the model should cover localities with individual income taxes. Proposed
approach: **extend the split-weight design one level down**, using **IRS SOI
county-level data** (https://www.irs.gov/statistics/soi-tax-stats-county-data —
county × AGI-class return counts and income items, same family as HT2) to further
split a state's weights across sub-state areas, conditional on the state weights:

- **Formal structure:** split `w_{i,st}` into `w_{i,st,loc}` with
  `Σ_loc w_{i,st,loc} = w_{i,st}` — the same per-record split constraint one level
  down, so state totals (and hence national totals) are preserved by construction.
  The Tax-Data fit applies unchanged with county targets in place of the
  state targets.
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
  data is filer-only (non-filer county margins would need ACS);
  sub-county jurisdictions (OH/PA) need an allocation assumption on top of county
  calibration.
- Open to alternative reweighting refinements — the NYC/non-NYC split is the
  motivating case; the same mechanism generalizes to any sub-state partition with
  SOI county coverage.

Priority: after sections 3.2 and 3.3; NYC first (largest
local PIT, cleanest county mapping), MD counties second (statewide coverage,
county-native).

---

## 3. What is built and what is next

**Built (phases 0-6 of the original plan).** All 51 jurisdictions are encoded
for TY2017-2025, enabled, and clear the cross-model bar (TAXSIM-35 for
2017-2020, PolicyEngine US for 2021-2025; 95% of federally aligned records
within $100 in every cell). Production state weights are the default. Full-sample
state-mode runs exist, with a first comparison to Census collections and state
agency tables (`research/state_tax/aggregate/README.md`). Current facts are in
`research/STATUS.md`; per-state status is in
`research/state_tax/state_parameter_rollout.csv`.

**Next, in this order** (JI, 2026-11-10): 3.1 documentation, then 3.2 and 3.3 in
parallel. Everything in section 7 waits.

### 3.1 Documentation

Shrink `research/STATUS.md` and this plan to what is current, index the gap
diagnostics and the PTET work, and bring `research/tools/check_conventions.R`
to zero findings. Done 2026-11-10 except where section 7 says otherwise.

### 3.2 Pass-through entity taxes (PTET)

A PTET is an elective state income tax paid by a partnership or S corporation on
its owners' behalf. The entity deducts it federally without limit, so the owners
avoid the federal cap on the deduction for state and local taxes (SALT); the
state then gives the owners a credit or an income exclusion. Today
`do_salt_workaround_baseline()` (`src/calc/do_taxes.R`) approximates the federal
half in every year and every state, and the state calculator has nothing. It is
the leading explanation for modeled NY, CA and CT liability running 1.2-1.5 times
the agency figures from TY2021.

Documents: the proposal `research/state_tax/ptet/ptet_proposal.md`; the law table
`research/state_tax/ptet/ptet_state_law.csv` with notes in
`research/state_tax/ptet/ptet_state_law.md`; draft per-state parameters and the
list of law items still on secondary sources in
`research/state_tax/ptet/params_draft/README.md`. Nothing is implemented.

**Decided (JI).**

- Calibrate to the Joint Committee on Taxation paper (Comey, Gallagher and
  Gorman 2025; `docs/papers/`): for TY2024, $30.9B of S-corporation and $33.1B
  of partnership tax moved off individual returns, an $18.1B federal cost, about
  80% of it in the top 1%; 85% of partnership PTET benefits individuals.
  (2026-10-28)
- **Two modes.** In a federal-only run each record's PTET uses a national rate
  series by year and entity type, stored in federal law YAML. In a state-mode
  run federal tax is computed per state fraction of a tax unit, each with that
  state's PTET, and summed with the state weights. The state-mode calculation is
  the reference; the national series is derived from it.
- **Rate buckets bound the cost.** States are grouped into K buckets per year
  (tunable, about 12 by default); each stacked row carries the record's
  weight-averaged rate within its bucket, so a record's total PTET is exact.
- **Rate.** The top PTET rate per state, year and entity type. Graduated entity
  schedules (NY and others) are documented, not applied. Maryland's county rate
  is excluded from the state credit.
- **Enacted law only.** No PTET in 2017; 2018-2020 only where enacted. This
  raises federal baseline revenue in those years and needs John Ricco's sign-off.
- **Reforms regenerate the national series.** A federal-only reform run needs a
  series produced by a state-mode run of that reform; the model stops with a
  clear error if it is missing.

**Design.** The amount is `rate[state, year, entity] x max(0, income)` where a
PTET is available, with the partnership amount scaled by the 85% individual
share. A record elects when its federal gain (marginal rate times the PTET that
would otherwise be lost to the SALT cap) exceeds a threshold scaled by a take-up
parameter per entity type, on the existing `salt_workaround` random-number
stream. Federally the amount is an active loss and reduces `salt_inc_sales`. On
the state side `st_agi.R` adds the federal deduction back and restores
`salt_inc_sales` for the SALT add-back, then applies the state's own credit
(`credit_share x ptet`) or exclusion. This reverses the 2026-07-12 decision that
the state add-back reads the post-workaround value (section 5, item 7); record
the reversal in `research/decisions_log.md` when it lands. The stacked pass is a
wrapper around the unchanged `do_taxes()`, placed after behavioral feedback.

**Steps, each separately verifiable.**

| # | Work | Gate | Moves the federal baseline? |
|---|---|---|---|
| 1 | Characterization test pinning today's `do_salt_workaround_baseline()`; a federal test runner | Totals match `research/state_tax/ptet/01_quantify_workaround.R` | No |
| 2 | `st_ptet.*` schema; generate `{st}/ptet.yaml` from the law table | Quick state tests pass; state totals identical | No |
| 3 | `calc_ptet()`, federal-only, behind a mode switch (legacy or new) | Legacy mode bit-identical to step 1; zero take-up equals workaround off | Only when the default flips |
| 4 | State add-back, credit and exclusion | Worksheet cases: NY credit, CT 87.5%, WI exclusion, PA none | No (state totals move) |
| 5 | Stacked wrapper, state-to-bucket map, unfiltered weights, collapse back to one row per record. Measure runtime and memory for one year at K = 12 and at K = every distinct rate before fixing the default | Zero take-up equals the unstacked run exactly; row weights sum to the record weight | Yes, in state-mode runs |
| 6 | Calibrate to the TY2024 targets; build the national series; validate against published NY, CA, CT, NYC and MA credit totals and the agency ratios | Targets met; NY, CA and CT ratios fall toward about 1.1 | Yes |
| 7 | Retire scenario parameters nothing reads; map `item.salt_workaround_allowed_*` to take-up caps | Scenarios parse | No |

**Checkpoints that need JI, in order.**

1. After step 1: may the PTET adjustment move to after the Social Security
   cost-of-living, capital and excess-growth adjustments? It must, to stack late,
   and it changes excess-growth scenarios slightly.
2. Before step 3: the base. Rate times pass-through income (proposed: brings in
   non-itemizers, drops sales tax) or itemized SALT as today.
3. After step 5's measurement: the default K, and whether fractional flags in
   collapsed detail files are acceptable for distribution tables.
4. Before step 6: the sign-off package for John Ricco (the pre-2021 revenue
   change; the level and mix from 2021).
5. Deferred unless raised: the MN and OR sunsets after 2027, the resident credit
   for another state's PTET, and the law items still on secondary sources.

### 3.3 TY2026 state law

Scope (JI, 2026-11-10): the seven states with known unencoded provisions, plus
an audit of statutorily indexed values in the states the tracker calls
`carry-forward`. Not a full 51-state re-research. No external model covers 2026,
so validation is worksheet cases plus the smoke and continuity tests.

| State | Work |
|---|---|
| MT | 2026 rate and bracket keys (HB 337 of 2025: 5.65%); confirm the capital-gains rates |
| ND | 2026 brackets from the estimated-tax form; mark preliminary |
| NM | 2026 child-credit amounts and low-income rebate table, if published |
| DC | Child-care credit match of 24.25%; the new $1,000 child credit (D.C. Code 47-1806.17) after researching its phase-out and refundability |
| RI | Resolve whether the child credit and surtax start in TY2026 or TY2027; encode the published 2026 indexed amounts now |
| CT | $500 family child care home credit (PA 25-168), against an optional supplemental input because occupation is unobserved |
| SC | H.4216 / Act 110 restructure: primary research first, then likely new generic parameters. Until encoded, SC 2026 and later must fail loudly or carry an explicit known difference |

Per state, follow `research/state_tax/state_parameter_workflow.md`: packet
section with the 2026 sources, a `reference:` on every value, "PRELIMINARY" where
the source is an estimated-tax form, at least one TY2026 worksheet case, and the
tracker cell updated.

**Indexed-value audit.** For each carry-forward state, list the parameters
indexed by statute (brackets, standard deduction, exemptions, credit thresholds)
in `research/state_tax/ty2026_indexed_audit.csv`, then for each row transcribe
the published TY2026 value, or add an `i_measure` rule where the statute gives a
formula the parser supports, or leave it fixed and record "awaiting publication".
Add a coverage script that reports the last keyed year per state parameter, and
rewrite the tracker's boilerplate `ty2026_law` cells to say what is true.

### 3.4 Later scope, in rough priority order

Coupled federal-state iteration with a sales-tax election imputation (2.5);
local income taxes through county-level sub-state weights (2.6); cross-border
wage taxation (nonresident returns, other-state credits, reciprocity) with a
work-state imputation from the Census LODES origin-destination matrix; state
marginal rates and combined-rate behavioral feedback; state distribution tables;
state AMTs; years before 2017; aging the weights with state population
projections.

---

## 4. Validation strategy (summary)

| Level | Test | Benchmark |
|---|---|---|
| Parameter | transcription check | Tax Foundation / TPC / ITEP tables |
| Record | form-worksheet unit tests | hand-computed state returns |
| Record | cross-model | TAXSIM-35 (2017-2020) and PolicyEngine US (2021-2025), `src/tests/state/cross_model/` |
| Aggregate | state totals by year | Census state tax collections (`research/state_tax/aggregate/README.md`) |
| Aggregate | resident totals by income class | state agency tables (NY, CA, CT so far), same README |
| Invariant | federal unchanged | with-state vs without-state byte-diff |

Federal-side disagreements are logged to
[`cross_model/federal_divergences.md`](cross_model/federal_divergences.md) and
conditioned out of the state metrics, per the 2026-07-18 policy. **One of them
is now a confirmed defect on our side and is owned by the federal model, not
this plan:** section 5 of that file, married-filing-separately drawing the
unmarried aged/blind standard-deduction bonus (IRC 63(f)(3)). One-line fix,
deliberately not applied inside state triage because it moves federal taxable
income in every year.

**Divergence classes are swept across states, not re-derived per state.** Where
a class has been verified in one state, the sweep asks every state's *resolved*
law whether the class has a route there, measures whether it bites, and tests
the measurement against the states whose encoding gives it no route at all. The
2026-08-22 pass is recorded in
[`cross_model/class_sweep_2026_08_22.md`](cross_model/class_sweep_2026_08_22.md):
22 rows added across two classes, 11 candidates measured and rejected. The
control is the load-bearing part — it is what separated a mechanism-specific
predicate (state-itemizing, which fires on no record in any of the ten no-route
states) from one that merely tracks record complexity (federal-itemizing, which
depresses match rates in states with no route at all).

---

## 5. Decisions (all made)

Reasons and dates are in the archived plan and `research/decisions_log.md`.

1. **Years of state law:** 2017 forward, matching the microdata floor.
2. **Weights home:** Tax-Data, read through the `State-Weights` interface
   (moved 2026-09-12).
3. **State detail files:** off by default; one compact per-year liability matrix
   when on (`state_detail`), plus the long component file at `state_detail = 2`.
4. **Non-state buckets:** `PR` and `OA` are carried as no-tax jurisdictions, so
   53 jurisdictions and the split weights sum to the national weight.
5. **Mode switch:** the runscript `states` column; the baseline row must cover a
   superset of every counterfactual.
6. **Fixed-date and selective conformity:** groups reference the enacted federal
   baseline (decision S44).
7. **State SALT add-back reads the post-workaround `salt_inc_sales`**
   (2026-07-12). Section 3.2 reverses this when the PTET work lands.
8. **Year horizon:** law encoded through TY2025 and carried forward; TY2026
   changes recorded per state in the tracker (decision S45). Section 3.3 is the
   work this left.

---

## 6. Risks

- **State weights in projection years** carry the TY2023 state shares;
  state-differential growth is not tracked.
- **Conditional-independence limits.** Weights cannot capture attribute variation
  driven by state policy; state-varying attributes belong in the calculator.
- **`salt_inc_sales` mixes income and sales tax.** Fine for state-static
  estimates; the coupled mode and the PTET base (3.2, checkpoint 2) both run
  into it.
- **Published benchmarks are not the model's concept.** Census collections
  include nonresidents, timing and PTET effects; agency tables differ in which
  credits sit inside liability. The gap diagnostics locate differences; they do
  not size them.
- **No external model for 2026.** TY2026 law is validated by worksheet cases only.
- **Maintenance load.** 41 income-tax states with annual updates is the real
  long-run cost; the `reference:` discipline, the per-state tests and the
  frozen-value coverage script (3.3) are what keep it tractable.

---

## 7. Open and deferred notes this plan owns

`research/CONVENTIONS.md` requires a note with `status: open` to be cited from
its workstream's plan; an open note no plan points at is how outstanding work
goes missing. This section is that citation, and the waiting list: work that is
real but not on the track in section 3.

| Note | Status | What it owns, and what unblocks it |
|---|---|---|
| `research/state_tax/ptet/ptet_proposal.md` | **open** | The PTET proposal for John Ricco. Active: section 3.2. Its decision 3 (the base) and decision 4 (how the federal pass handles state) are superseded or pending as 3.2 records. |
| `research/state_tax/ptet/params_draft/README.md` | **open** | 42 draft `ptet.yaml` files and a schema snippet, not read by the model; the list of PTET law items still on secondary sources; five disagreements with our own state rate parameters (NE, GA, AR, MT, ME) that belong to 3.3. |
| `research/state_tax/aggregate/README.md` | current (evidence) | The gap diagnostics: modeled liability against Census collections and agency tables. Rerun after 3.2 step 6. Per-state aggregate validation beyond NY, CA and CT needs more agency tables in the State-Ind repo. |
| `research/state_tax/notes/state_data_imputation_plan.md` | **open** | The data extensions the state calculators need, tiered by materiality — 14 items since 2026-10-13, rebuilt from the machine-readable crosswalk `research/state_tax/tax_data_gaps.csv` (614 rows, 45 jurisdictions; decision S43). **Tier 1 is the live one**: tenure/rent/property tax (23 states, 12 high-materiality rows, incl. MN's 2024+ renter credit), the pension source split (36 states; the military flag alone unlocks most), and household-resources income (12 states). Item 4 (elective deferrals) is a Tax-Data FILL, not an imputation: `trad_contr_er1/2` exists and is all zero. Since S43 encoding no longer waits on any of this — a data-blocked provision is encoded against an optional variable in `config/interfaces/supplemental_inputs_schema.yaml` and switched on by a producer through the supplemental-inputs interface (`research/state_tax/supplemental_inputs_interface.md`, `role: method`). §6's `salt_inc_sales` risk is the same work seen from the coupled-mode side. |
| `research/state_tax/notes/ca_misc_amt_plan.md` | **deferred** (was `open`; changed 2026-08-19 to match the note's own §Sequencing) | CA miscellaneous itemized deductions and CA AMT, in that order — implementing misc without AMT overstates deductions for exactly the high-income itemizers most likely to be in AMT. **Trigger: when top-of-distribution CA output matters** (P1 production use, or CA distributional tables). Not needed for the cross-model bar, which CA already clears. |
| `research/state_tax/notes/or_kicker_design_note.md` | **deferred** (JI, 2026-10-13; S46) | The Oregon kicker (ORS 291.349) and every other provision that reads a PRIOR-YEAR state liability (HI/IA/LA/MT carryforwards; crosswalk item 13, overlay `prior_year_liab`). Not encoded; excluded in the harness where an external model pays it. **Trigger: when Oregon output matters** -- then design the prior-year carry through the sequential year loop (and its SLURM equivalent) before writing the credit. |
| `research/state_tax/notes/state_interstate_migration_design_note.md` | **deferred** | Interstate migration. No trigger set. |
| `research/state_tax/notes/datagap_spec_mi_mo_or.md` | **open** | **JI review of the circuit-breaker provisions (S46).** Before the generic refundable circuit-breaker component (MI-1040CR homestead credit, MO-PTS/PTC property tax credit; reads only the rent / property-tax / household-resources overlays) ships, JI reviews the provisions as encoded: the MI senior rate ladder and senior/disabled exemption ladder, the MO step table, the household-resources income concepts, and which other states' circuit breakers (WI Schedule H, MT 2EC, ME 5219-KK, RI-1040H, VT, NJ ANCHOR) it should carry next. The three review items the spec flags are `cb_elig_disabled`, `cb_offsets_nonref` and the MO TY2026 values. The sibling spec `research/state_tax/notes/datagap_spec_hi_ky_pa.md` was executed 2026-10-13 (commit 507c41ef1). |
| `research/state_tax/notes/bakija_comparison.md` | **open** | Bakija's IncTaxCalc (2019) read as a mechanism checklist against our module. Owns four items: **document the CO and WI state AMTs** (the no-state-AMT policy requires documenting them and both are silent, unlike MN/CA/IA), feed his 39-parameter circuit-breaker taxonomy into the Tier 1 rent/property case, declare the four spouse-allocation dimensions per state when AL/AR/MS are wired (his `itemalloc`/`stdalloc`/`exalloc`/`credalloc` against our single even-split convention), and audit our low-income relief against his 11 `lowtype` designs. Also records what he models that we ALREADY do, so it is not re-derived. Unblocked by nothing; the AMT documentation is the cheap half. |
| `research/state_tax/acs_test/README.md` | current (evidence) | Which rows of the data-gap registry the ACS records under the Affordability-Index matched file could fill. Feeds tier 1 of the imputation plan. |
| `research/state_tax/cross_model/external_model_issues.md` | current (evidence) | The TAXSIM (T1-T81) and PolicyEngine (P1-P61) findings. **Not yet sent upstream.** |
| Merge to `main` (no note) | waiting | `origin/main` moved state law to `config/scenarios/tax_law/default/state/{st}/` in November 2026 and this branch is several hundred commits behind, so the merge is a path migration. See `research/STATUS.md`. |

Also waiting, with no note of their own: the Wisconsin 2025 Act 15
retirement-income election (documented in `research/source_packets/wi.md`),
interstate migration beyond the design note, local income taxes (2.6) and the
coupled mode (2.5).

Notes with `status: current` (`ca_p1_readiness_analysis`, `ca_parameter_analysis`,
`child_credit_survey`, `elderly_retirement_provisions`,
`state_eitc_align_crosscheck`) are surveys and cross-checks cited from where they
are used; `status: historical` and `status: executed` notes are records. Neither
kind carries outstanding work.
