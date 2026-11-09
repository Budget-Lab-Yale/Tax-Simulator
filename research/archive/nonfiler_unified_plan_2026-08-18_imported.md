---
title: "Unified non-filer + state-weights implementation plan (imported)"
role: plan
workstream: state_weights
status: superseded
updated: 2026-08-19
true_as_of: 2026-08-18
sot: research/state_weights/plan.md
supersedes: []
superseded_by: research/state_weights/plan.md
---

> **Imported 2026-08-19 from outside version control** (`~/.claude/plans/`), where it
> was drafted 2026-08-18 and cited by nothing in the repo. Kept whole because it is the
> fuller of the two same-scope plans and its Step 4 is the origin of
> `research/state_weights/nonfiler_federal_validation.md`. **Superseded by**
> `research/state_weights/plan.md`, which merged it with the 2026-08-19 to-do.
>
> Three things in the text below were already wrong when it was imported, and are
> corrected in the merged plan: it cites `Non-Filer Proposal.docx` (renamed
> 2026-08-18), it treats Cilke (1998) as the below-threshold model (replaced by Mok
> 2017 on 2026-08-18), and it calls the design memo 579 lines (it is now 1,079).
> Paths in the body are pre-reorganization and are left as they were written.

# Unified non-filer + state-weights implementation plan

## Context

**Why this work exists.** Two systems in the Budget Lab tax model carry a non-filer
population, and neither count is anchored to anything:

- **Tax-Data** appends ~27.6M (TY2022) non-filing tax units to the PUF straight from
  PSZ/DINA, at DINA's own uncalibrated weights. Every target in its
  `config/target_info/baseline.csv` is a filer concept.
- **Tax-Simulator's state weights** place those units across states using ACS margins
  built from a v0 filing rule (a hardcoded standard-deduction threshold) that
  over-assigns filers by ~7% nationally, with a 20pp state spread.

`other/state_tax_research/Non-Filer Proposal.docx` (Aug 2026) is the narrative case for
replacing this with a **residual methodology**: non-filing adults = resident adults
(Census PEP) − filing adults (IRS Pub 1304 / HT2 identities), disciplined by SSA age and
earnings margins, with a two-part filing model (below-threshold voluntary filing per
Cilke 1998; above-threshold non-compliance per IRS Pub 5785). The implementation-level
companion is `other/state_tax_research/nonfiler_residual_design.md` (579 lines).

**What is already done.** Stage D — the diagnostic harness — is **executed and committed**
(`4783dc3e9`, 2026-08-16): anchors built for TY2017/2022, diagnostic tables T1–T7 run,
findings in `other/state_tax_research/nonfiler_residual/04_findings.md`, and decisions
D1–D6 resolved. The diagnostics confirmed the proposal's numbers and sized the defects:

| # | Finding | Number |
|---|---|---|
| F1 | Non-filer mass short | 32.4M non-filer adults vs a defensible 38–41M anchor → **~15–25% short (6–9M adults)** |
| F2 | Age composition **inverted** | 8.9% of non-filer adults at 18–25 vs anchor 24.2%; 42.9% at 65+ vs 25.1% |
| F3 | Investment income identically zero | 0.0% with interest/dividends/gains vs Pub 5785's 14%/9%/4% |
| F4 | Aging drift | 26.2M units (2017) → 33.0M (2035), share 14.7% → 15.8%, no return-count discipline after 2019 |
| F5 | v0 filer bias reproduces | v0 ACS filer units = 0.933 × HT2 returns; non-filer margins 0.78× (DC) to 1.51× (SD) of anchor |
| F6 | Group quarters first-order in small states | 8.15M GQ persons = 16.8% of national residual, but 41.6% SD / 33.8% AK / 32.6% VT |
| F7 | Above-threshold non-filers are SE-shaped | 10.6–11.9M units, <20% married, ~45% with business/farm income |

**Intended outcome.** A Tax-Data vintage whose non-filer slice is anchored in level, age
shape and income composition; a federal-level validation record proving what did and did
not move; and state weights fit to residual anchors rather than to a biased survey margin
— ending with the production dispatcher off `placeholder`, so state totals become
meaningful for the first time.

**Sequencing decision already on record** (design memo §7.1, JI 2026-08-16): the
non-filer rework lands **before** the production weights swap-in, so the swap-in fit
happens once on upgraded margins rather than fit-on-v0-then-re-fit. The one exception is
the differentiated GQ treatment, which is decision-independent and ships early.

**Decisions taken for this plan (2026-08-18):**

1. **Tax-Data work happens on the Yale HPC.** Tax-Data is not cloned on this machine, and
   its inputs (`puf_2015.csv`, `usdina2017.dta`) and output vintages live on cluster NFS.
   Clone `Budget-Lab-Yale/Tax-Data` into HPC space; edit and run there.
2. **The filing model is estimated on the CPS ASEC, then transferred to the ACS** — the
   proposal's §2.2 route, not the design memo's ACS-only shortcut. This adds an ASEC
   extract and an explicit transfer/recalibration step, and it answers the proposal's own
   first open question rather than deferring it.
3. **The blocked SSA state tabulations get downloaded manually** (ssa.gov 403-blocks the
   cluster egress IP). This is an early task on JI and it gates the state age layering
   (D6) and the covered-wage margin.
4. **Scope runs through production swap-in**: the `state_weights_{year}.csv` writer,
   projection carry-forward, the dispatcher flip at `src/sim/run.R:433`, and the
   validation battery.

---

## Pre-flight: three things to settle before writing code

These are cheap, login-node-safe, and each one can invalidate work done on top of it.

### P1 — Resolve the Tax-Data vintage discrepancy (blocking)

The design memo §2.1 states the production non-filer masses were "verified 2026-08-16"
against vintage `2026070814`. But `config/interfaces/interface_versions.yaml` pinned
`Tax-Data: default_vintage: '2026030513'` at the Stage D commit (verified:
`git show 4783dc3e9:config/interfaces/interface_versions.yaml`), and
`03_diagnose_current_nonfilers.R:48-53` resolves its input path *from that yaml*. So
F1–F4 were computed against `2026030513`, not the vintage the memo cites.

**Action:** confirm which vintage is current production, then re-run
`Rscript other/state_tax_research/nonfiler_residual/03_diagnose_current_nonfilers.R --tables`
against it and diff `results/T1..T4` against the committed versions. Correct whichever
document is wrong. Everything downstream is calibrated to these numbers.

### P2 — Reconcile the age bands (blocking for step 4)

The two systems cut age differently, and the anchors are meant to discipline exactly this
dimension:

| Function | Bands |
|---|---|
| `age_band()` — `src/data/state_weights.R:197-200`, used for the non-filer fit cells | `u25` (<25), `25_34`, `35_44`, `45_54`, `55_64`, `65_74`, `75p` |
| `a16_band()` — `nonfiler_residual/02_build_residual_anchors.R:47-51`, used by the anchors | `18_25` (18–25), `26_34`, `35_44`, `45_54`, `55_64`, `65p` |

Two mismatches: the young boundary (25 vs 26, and `u25` has no lower bound while the
anchor starts at 18), and 65+ (two bands vs one). Pub 1304 Table 1.6's bands are fixed by
the publication, so the reconciliation is one of: collapse `age_band()` to the T1.6 cut
for non-filer targets only; keep `age_band()` and apportion the `65p` anchor across
`65_74`/`75p` using the PEP single-year-age detail already fetched; or carry both and map
at target-assembly time. **Recommendation:** keep `age_band()` as the fit dimension and
split `65p` with PEP single-year age (the `Census-PEP` store is state × single-year-age ×
sex, so this is free), and re-cut the under-26 anchor band using the same PEP detail.
Decide and record before the state target assembly is touched.

### P3 — Unblock the two data dependencies

- **SSA manual downloads (JI, workstation).** Fetch SSA statcomps *OASDI Beneficiaries by
  State and County* (`oasdi_sc`) and *Earnings and Employment Data by State and County*
  (`eedata_sc`) for 2017 and 2022, place per the `README_MANUAL_DOWNLOAD.md` in
  `raw_data/SSA-OASDI-SC` and `raw_data/SSA-EEDATA-SC`, then re-run scripts 01 → 02 → 03.
  This fills the `ssa_covered_persons` column stubbed `NA_real_` at
  `02_build_residual_anchors.R:197`, the OASDI age margins (D6), and the covered-worker
  earnings shape. Check series continuity first — SSA has been revising geography
  assignment, and the covered-earnings series runs only through 2023.
- **Cilke (1998) coefficient transcription.** `nonfiler_residual/resources/cilke_coefs.csv`
  does not exist yet. Source: Treasury OTA WP-78,
  `home.treasury.gov/system/files/131/WP-78.pdf`. Nine group probit equations, hand
  transcribed with page citations in the header, following the pattern already set by
  `resources/pub5785_table1_potential_nonfilers.csv`.

---

## Step 1 — Filing model on the CPS ASEC (new work, per decision 2)

The proposal specifies CPS ASEC estimation; nothing has been built for it. This is the
largest genuinely new piece of the plan, and it has three research components that should
run **before** any code is written.

### 1.0 Get the ASEC data the shared-store way, not by a bespoke pull

**Do not pull an ASEC extract ourselves as the first move.** Once on the cluster:

1. **Check the shared data drive first.** Look under `{production}/raw_data` for an
   existing CPS/ASEC family (`CPS/cps_common`, `IPUMS-CPS`, or similar) the way the ACS
   extract already lives at `raw_data/ACS/acs_common/us{year}a/` — documented at
   `state_weights.R:158-159` as `usa_{year}a.dat.gz` + DDI xml + `variables.csv`, vintages
   2006–2024. If an ASEC family is already there with the years and variables we need,
   read it and stop.
2. **If it is not there, add it through the common IPUMS-download repo**, following the
   same structure that produced `acs_common` — same directory layout (`.dat.gz` + DDI +
   `variables.csv`), same `manifest.csv` provenance convention that
   `01_fetch_residual_inputs.R:45-57` uses, and the same maintained-repo pattern as
   `IRS-Ind`. Identify that repo on the cluster (it is not visible from this machine) and
   extend it, so the ASEC family is a maintained store other projects can consume —
   Affordability-Index needs the same data.
3. **Only fall back to a hand-rolled extract if neither exists**, and if so, register it in
   the common convention rather than as a one-off under `state_tax_research/`.

Years: the anchor years (2017, 2022) at minimum. Variables needed for the Cilke covariate
set plus the threshold test: age, marital status and spouse pointer, household
relationship, school enrollment, dependent pointers, wages, self-employment income, Social
Security, public assistance, interest/dividend income, state.

### 1.1 Research pass A — ASEC tax-unit and income construction

The ASEC is **not** the ACS with more income detail; the tax-unit and income construction
differ enough that reusing the ACS pointer logic unexamined would be a mistake. Before
building, research and write up the right structure:

- **Tax-unit construction on the ASEC.** The ASEC has a richer relationship structure and
  its own family/subfamily concepts, and it carries dependent and filing-related variables
  the ACS lacks. Survey the established approaches: **PolicyEngine's Enhanced CPS** (which
  models filing on the CPS directly — see `state_tax_model_research_notes.md:166-167` on
  their quantile-regression-forest PUF fusion plus gradient-descent reweighting), the
  **Census SPM/tax-model** unit construction, **TAXSIM's** CPS input conventions, and the
  **Urban/TPC** CPS-based approaches. Record where they agree and where they diverge, and
  pick deliberately.
- **Income concept alignment.** ASEC income items are survey-reported, differently
  top-coded, and differently defined from both ACS `INCTOT`-family variables and the PUF's
  1040 concepts. The filing threshold is a *tax* concept, so the mapping from ASEC income
  items to gross income for the threshold test needs to be explicit and documented, not
  inferred.
- **Deliverable:** a short design note in `nonfiler_residual/` recording the chosen unit
  and income construction with citations, in the same style as the existing memos. This is
  what makes the ASEC→ACS transfer in 1.5 interpretable rather than a black box.

### 1.2 Research pass B — ✅ DONE 2026-08-18. Answer: replace Cilke with Mok (2017)

**Executed. Deliverable: `other/state_tax_research/nonfiler_residual/05_filing_model_literature.md`,
references in `nonfiler_residual/resources/filing_model_refs.bib`.** The finding changes
step 1.3:

- **Cilke (1998) should be replaced, not re-calibrated.** **Mok (2017), CBO Working
  Paper 2017-06, Table 14** gives fourteen group-specific filing probits — coefficients,
  standard errors, cell Ns, and weighted filing rates — estimated on the **2007 CPS ASEC
  linked via PIK to the IRS Individual Master File for TY2006**, on a covariate set Mok
  describes as "similar to the set of covariates used by Cilke." Same design, 16 years
  newer, with the per-cell filing rates published as ready-made calibration targets.
  **VERIFIED against the PDF 2026-08-18** (JI downloaded it to the Affordability
  literature folder): all 14 equations, Panel A coefficients, cell Ns and every filing
  rate check out exactly, as do the 141.7M/117.9M/23.7M totals and the 83%/94%/27%
  match diagnostic. Two warnings the verification added — **Panel E's columns run "Age
  65 or Older" FIRST** (reverse of the intuitive order, and text extraction returns the
  headers reversed), so transcribe from a rendered image; and **Mok's CPS frame excludes
  the institutionalized and military-barracks populations**, so her coefficients do not
  cover the group-quarters records our PUF universe includes. One claim was **wrong and
  is corrected**: rank-and-cut does *not* outperform by ~10M — Mok's conclusion says
  correct classification is "similar under both methods."
- **Recommendation: fit both.** The two use overlapping CPS-native covariates, so score
  the same ASEC file under each, calibrate both to the same administrative target, and
  compare implied non-filer age/income distributions against Brady–Bass's by-age shares.
  That makes "keep the 1990s slopes?" empirical and documents the answer either way. If
  only one, fit Mok. Also test Mok's **rank-and-cut within cell** assignment against
  intercept-calibration-plus-uniform-draw — correct classification is similar under both,
  but rank-and-cut matches filer demographics by construction.
- **Pub 5785 stands** — no successor edition exists, and the TY2017–2022 tax-gap figures
  are projections without counts. Add Hertz et al. (2021) (TY2010 predecessor, eight
  method variants), Treasury OTA's Jan-2025 study (50.343M TY2022 non-filers), and OTA
  TP-12 (Treasury's current construction recipe) as companions.
- **Reframing worth absorbing:** Treasury, IRS and JCT all *abandoned* the survey-probit
  approach rather than update it, and published the error — a reweighted ASEC reaches
  42.0M against a 50.7M administrative target, ~17% short. We have no administrative
  microdata, so a survey model is still the only route open; but that known error
  direction belongs in P4's tolerances, and §3.5 of the proposal should say so.
- **A premise in my own brief was wrong.** TCJA did *not* sharply reduce filing
  requirements for the main statuses: thresholds are standard deduction *plus*
  exemptions pre-TCJA, so zeroing exemptions offset most of the increase (single
  $10,400 → $12,000, +15.4%; MFJ $20,800 → $24,000, +15.4%), and returns filed rose.
  But the **dependent** threshold nearly doubled ($6,350 → $12,000) and **MFS**
  collapsed ($4,050 → $5). Dependents are Cilke's *largest* group — 31.1% of his
  below-threshold population, 36.4% of his non-filers — so this is an argument for
  revisiting the v1 decision to leave dependent non-filers and MFS out of scope.
- **Two extraction hazards recorded:** the `MARRIED` coefficient in the above-threshold
  hazard literature flips sign across vintages of the same model by the same authors, so
  no single vintage is settled; and Cilke's Table 3 must be extracted with PyMuPDF word
  positions — `pdftotext -layout` silently mis-assigns its coefficients, which produces
  plausible-looking wrong numbers.

<details>
<summary>Original scope of this pass (superseded by the findings above)</summary>

Both parameter sources are old, and the plan hard-codes them. Check for updates before
transcribing:

- **Cilke (1998)** — Treasury OTA WP-78. Look for published re-estimates, successor OTA
  working papers, or later Treasury/JCT work using the same group-probit structure with
  updated coefficients. The 1990s filing environment differs materially from today's, which
  is exactly why the design memo re-calibrates the group constants; if someone has already
  re-estimated the *slopes*, that is strictly better than re-calibrating constants on
  1990s slopes.
- **Note in our favour:** Cilke was estimated on the **CPS ASEC** originally. That makes
  the ASEC the model's *native* environment, so decision 2 removes a transfer step at
  estimation rather than adding one — the recalibration burden now falls on the ASEC→ACS
  step (1.5), where it belongs and can be measured, instead of being silently embedded in
  an ACS-only fit. Expect minor adjustments relative to what an ACS-only pass would have
  produced, and document them.
- **IRS Pub 5785** — the transcribed tables are TY2014–2016
  (`resources/pub5785_table1_potential_nonfilers.csv`,
  `pub5785_table3_notfiler_units.csv`). Check for a newer edition or successor tax-gap
  publication with later tax years, and for the related IRS tax-gap series (the
  TY2020–2022 tax-gap projections). If newer statistics exist, the above-threshold level
  anchor (~11.19M units) and the receipt-rate ceilings (14%/9%/4%/48%/14%) should be
  updated, and the Stage-D findings that cite them re-stated.
- **Also worth a look:** post-2016 academic and policy work on the non-filing margin
  (EITC non-claiming, the 2020–2021 EIP non-filer outreach literature, which generated a
  lot of new evidence on who does not file). The EIP era is the best-measured non-filing
  episode in existence and postdates every parameter source in this plan.
- **Deliverable:** a findings note recording what was found, what is adopted, and what is
  retained from the original sources with justification. If nothing newer exists, that
  negative result is worth recording too — it is the defence of using 1998 coefficients.

</details>

### 1.3 Build the models

1. **Build tax units from ASEC households**, per the 1.1 design note. Factor the unit
   builder so ASEC and ACS share what is genuinely shared rather than each carrying its own
   copy, but do not force one implementation where the surveys legitimately differ.
2. **Compute the filing threshold each unit faces**, including the three things the v0 rule
   omits (documented at `state_weights.R:170-173`): the age-65 additional standard
   deduction, the $400 self-employment filing rule, and the separate lower filing floors
   for dependents with their own income.
3. **Estimate the two models.**
   - *Below threshold* — **Mok (2017) Table 14 group probits** (per 1.2), with Cilke's
     Table 3 fit alongside as the comparison. Mok's published per-cell filing rates are
     the calibration targets; test her rank-and-cut-within-cell assignment against
     intercept calibration plus a uniform draw.
   - *Above threshold* — a non-filing hazard scaled to Pub 5785's level
     (10.57/11.09/11.90M units, TY2014-16; avg 11.19M) and allocated by its relative rates
     by age, income and marital status. A single national scalar for v1 per **D3**; F7's
     ~45%-business/farm signature is the documented upgrade path to SE-aware cells.
4. **Calibrate jointly, not sequentially** (proposal §3.3). Choose the below-threshold group
   constants and the above-threshold scalar *together* so the combined file hits (i)
   population by age, (ii) filer counts by age × marital status × income, and (iii) the
   implied non-filing OASDI-beneficiary and wage-earner counts. Each target carries an
   explicit tolerance — see P4 below.
5. **Transfer to the ACS.** Re-calibrate the group constants on the ACS (the destination
   survey for state allocation, which carries less income detail), and **verify the two
   surveys imply similar national filing rates**. Report the ASEC-vs-ACS filing-rate
   comparison as a first-class result: it is the proposal's own §6 open question, and if the
   two disagree materially the transfer step needs rethinking before the state fit depends
   on it. Per 1.2, this is where the ASEC-native origin of the Cilke equations gets paid
   for — document the adjustments the transfer requires.

**Acceptance:** ACS v1 filer units vs HT2 `n_returns` by state — the 0.933 national ratio
and its 0.91–1.03 state range (F5) should collapse inside the stated tolerance.

### P4 — Quantify the anchor tolerances (do this here, not later)

Both the proposal (§6) and the design memo (§8) flag that the residual tolerance must be
built up from the known wedges rather than picked. The wedges are already enumerated at
`02_build_residual_anchors.R:26-32`: return-state vs residence, the MFS/QSS residual in
the HT2 identities, adult dependents riding filer records, MFJ spouses assigned the
primary's age band, and PEP/ACS/IRS vintage gaps. T5 provides the raw material. Produce a
per-wedge magnitude and a combined tolerance per anchor, and write it into
`residual_anchors_{year}.csv` as the `tolerance` column the design memo §4.2 specifies.
Every acceptance test downstream reads this number, so a placeholder here silently
weakens every later check.

---

## Step 2 — Differentiated group-quarters treatment (ships first, independently)

**Why first:** decision-independent (**D4** is settled), sized by T7, and it changes the
ACS margins that everything else is fit to. `GQ` currently appears nowhere in
`src/data/state_weights.R`, so 8.15M GQ persons enter the unit builder as income-less
single-person "units" and land in the non-filer margins untreated.

The PUF universe **includes** GQ residents (verified: DINA adults 241.96M ≈ PEP 2017
resident 20+ of ~242M, not the household-only ~235M), so blanket exclusion is
universe-inconsistent — F6 confirms it overshoots the other way (AZ falls to 0.77× the
residual). In `build_acs_margins()`:

- `GQ == 3` (institutional) — keep as own-state non-filer units unless income makes them
  filers.
- `GQ == 4` + in school + age < 24 (dorm students, 2.81M) — reclassify as **dependents**,
  not unit heads. They are claimed on parents' returns, generally in another state, and
  HT2 already counts them in `N2`.
- Military barracks — leave to the income test; most have wages and classify as filers.
- Report GQ weight by type × state as a standing diagnostic.

Also extend `read_acs_extract()`'s default `cols` with the covariates the extract already
carries (`GQ`, `SCHOOL`, `EMPSTAT`, `SEX`, `EDUC`, `INCWAGE`, `INCBUS00`, `INCSS`,
`INCSUPP`, `INCWELFR`, `FOODSTMP`, `INCRETIR`) and **fix the stale comment at
`state_weights.R:166-167`** claiming the extract lacks `SCHOOL` — it does not. No new
IPUMS USA pull is needed.

---

## Step 3 — Tax-Data rework (part (a))

**Venue:** clone `Budget-Lab-Yale/Tax-Data` into HPC space; edit and run there. All
changes are Phase 1 (base construction) plus one Phase 2 fix. Nothing routes through
`module_deltas` — Phase 3 modules cannot add rows or reweight — and no row-adding is
needed: the PSZ record set stays, values and weights change.

**Priority order** (design memo §5, and it matters — the state fit keys on age):
age detail > national level + aging > investment income > the dividends bug.

### 3a. Composition fixes in `src/impute_nonfilers.R`

- **Age detail (highest value).** Replace the flat `runif` draw within DINA's 3-point
  `ageprim` bands (`impute_nonfilers.R:92-96`) with a draw from the anchored national age
  shape, committed as `resources/nonfiler_age_shape.csv` and generated by the Stage-D
  harness. This is what fixes F2, and the state non-filer cells key on `age_band(age1)`.
- **Dividends bug (trivial, do regardless).** `impute_nonfilers.R:46` writes `qual_div`,
  a column absent from the output schema (`config/variable_guide/baseline.csv` defines
  `div_ord`/`div_pref`), so `write_outputs.R`'s `out_cols` intersection silently discards
  it. Route `fidiv` to `div_ord`/`div_pref`.
- **Investment income (fixes F3).** First action at implementation: inspect
  `usdina2017.dta` for DINA's *national-income* counterparts to the fiscal
  `fiint`/`fidiv`/`fikgi` (which are return-reported concepts and zero for non-filers by
  construction). Option A — scale those to per-unit amounts. Option B (fallback) —
  hot-deck from stub-1/2 PUF filers conditioned on age band × has-wages. Either beats
  identical zeros. **Discipline for both:** Pub 1304 Table 1.1
  (`income_sources_{year}.xls`, already in `raw_data/IRS-Ind/national/by_size/`) gives
  return counts with each income type in the bottom AGI classes; the repaired non-filers
  should sit **at or below** Pub 5785's receipt rates (14% interest, 9% dividends, 4%
  gains, 48% SS, 14% pensions).
- **Assertions.** Set `filer = 0` explicitly rather than relying on the zero-fill of
  `remaining_vars` (`impute_nonfilers.R:136-142`), and `stopifnot` on it and on
  `dep_status == 0`. The flag is load-bearing for both the CTC/rebate become-filer logic
  (`Tax-Simulator src/calc/do_taxes.R:126-127`) and the entire state-weights partition.
- **Record as explicit TODOs, not silent omissions:** dependent non-filers, MFS units, and
  an in-Tax-Data Cilke model (Cilke belongs where filing is modeled on survey records).

### 3b. New `src/calibrate_nonfilers.R` (Phase 1) — resolves **D1** (yes)

Sourced in `main.R` between `impute_nonfilers.R` and `impute_variables.R`. A post-append
rake of **non-filer weights only** to the national residual anchors by age band × marital
status (≤14 cells — a closed-form cell-ratio adjustment; **do not touch `reweight.R`'s
filer LP**). Targets read from a committed `resources/nonfiler_targets_2017.csv` snapshot
with a provenance header, generated by the Stage-D harness — so Tax-Data gains no HT2/PEP
readers and no server paths. Emits a before/after diagnostic table.

Calibrate to the **comparable-universe** anchor, not the raw residual: DINA's universe is
20+, PEP carries 8.71M 18–19-year-olds (~5–6M non-filing), and ≥5.5M claimed adult
dependents sit in the residual but ride filer records. That is the 38–41M figure, not
46.5M. Getting this wrong in either direction is the single largest level risk in the plan.

### 3c. Aging-path fix in `src/project_puf.R` (Phase 2) — fixes F4

In `compute_weights_for_year()`, make the non-filer path residual-by-construction: each
year's non-filer adult mass = projected adults (the `demog` table is already loaded) −
projected filer adults, scaled within age × married cells. Today the function grows
non-filer weights by married-only population factors for 2018–19 (`:333-341`, the only
filer-aware branch) and by pure married × age demographics from 2020 on (`:344-365`) with
no filer term at all. Minimal change: one new factor table alongside
`population_factors_2020plus`, applied `if_else(filer == 0, …)` symmetric with the
existing filer branch at `:333`. Filer weights and the ledger architecture untouched.

For projection years, anchor the filer side on IRS Publication 6187 (published projections
of return filings) and the population side on Census projections cross-checked against
CBO's demographic outlook, per proposal §3.4.

### 3d. Build a new vintage

Full Tax-Data pipeline re-run under `sbatch`. **Stage the changes as separate vintages**
so each can be A/B'd independently — see step 4.

---

## Step 4 — Federal-level validation (part (b))

The key insight that makes this testable cheaply: **the model's post-processing is sharply
split between filer-gated and full-population aggregates**, so the rework has a predicted
signature, and anything outside that signature is a bug.

Verified in `src/data/post_processing/summary_stats.R`:

- `get_1040_totals()` at `:141-149` pre-multiplies the reporting counts by `filer`
  (`n_returns = filer`, `n_returns_dep`, `n_adults`, `n_people`, `n_single`, `n_joint`,
  `n_hoh`, `n_dep`), and `:193-195` sums every **tax variable** as `. * weight * filer` —
  so non-filer records contribute **exactly zero** to every 1040 dollar aggregate.
- `:144` defines `n_nonfilers = !filer` and `:140` `n_tax_units = 1`, both summed on
  unrestricted `weight`.
- `:198-200` computes `mtr_*` as `weighted.mean(., weight)` — **unrestricted**, so
  non-filers are in the denominator.
- **`get_pr_totals()` at `:214-278` has NO `filer` gate** — `:267-270` sums every payroll
  variable on unrestricted `weight`. Payroll totals therefore **include non-filers**.

And `src/data/post_processing/distribution.R` uses unrestricted `weight` throughout:
`income_pctile` / `agi_pctile` at `:265` and `:296`, `n_tax_units = sum(weight)` at `:338`,
and the `share_cut.*` / `share_raise.*` denominators at `:354-361`. `horizontal.R`
references `filer` nowhere, so it too includes non-filers.

### 4.0 Three verified hazards that shape the whole battery

**(i) Random numbers are bound by row POSITION, not by id.** `src/sim/run.R:348-357` does
`read_microdata(year) %>% filter(id %in% globals$sample_ids) %>% ... %>%
bind_cols(globals$random_numbers)`, where `globals$random_numbers`
(`src/misc/config_parser.R:227-239`, seeded `set.seed(76)`) is a tibble of nine `r.*`
columns with `length(sample_ids)` rows, joined **positionally**. If the reworked Tax-Data
writes rows in a different order — a sort inside `calibrate_nonfilers.R`, a changed append
order — every *filer* record receives different draws for `r.cdctc_takeup`,
`r.salt_workaround`, `r.bus_loss`, `r.oasdi_exp`, `r.new_car`, `r.eitc_precert`. Filer-side
aggregates would then move for reasons having nothing to do with non-filers, and the entire
invariance battery becomes uninterpretable.

**Blocking gate: ordered `identical()` on the `id` column** — across vintages *and* across
years (the design also assumes every year's file shares the 2017 file's row order). Not
`setequal()`.

**(ii) `sample_ids` come from one vintage's 2017 file.** `config_parser.R:219-225` takes
`interface_paths %>% filter(interface == 'Tax-Data') %>% slice(1)` — the **first runscript
row**, which in a two-vintage A/B is the *old* vintage. Any record present only in the new
vintage is silently dropped by `filter(id %in% globals$sample_ids)`, even at
`pct_sample = 1`. Since the rework keeps the PSZ record set intact this should be a no-op,
but it must be verified rather than assumed.

**(iii) The current-law baseline already routes money to non-filers in 2020–2021.**
Verified in the baseline tax law: `config/scenarios/tax_law/baseline/rebate.yaml` sets
`value` to 1800 (2020), 1400 (2021), 0 (2022); `ctc.yaml` sets `min_refund_young` 3600 and
`min_refund_old` 3000 in 2021 only. So `become_filer_rebate` fires in 2020–2021 and
`become_filer_ctc` fires in 2021 **without any reform scenario**.

This is the most valuable single finding for part (b): **run the baseline over 2020–2022 and
the non-filer channel is live, disciplined against published actuals** — EIP3 ≈ $402–411B
disbursed, advance CTC ≈ $93B. It is the only place in the battery where a model output
that depends on the non-filer level is checked against a real-world number. It also means
my framing below has to be year-conditional: current-law invariance holds for 2017–2019 and
2022+, but **not** for 2020–2021, where the level change *should* move federal outlays.

### 4a. The predicted signature — write it down before running anything

**In years 2017–2019 and 2022+** (no refundable credit reaches non-filers under baseline law):

| Must NOT move | Must move |
|---|---|
| Every dollar aggregate and nonzero count in `totals/1040.csv` and `totals/1040_by_agi.csv` | `n_tax_units` (reported) |
| `n_returns`, `n_returns_dep`, `n_dep`, `n_simple_filers` | **`totals/payroll.csv` — every line**, because `get_pr_totals()` is not filer-gated |
| **Every line of `supplemental/cbo_comparison.csv`** — all mapped lines are 1040 tax vars plus `n_returns`/`n_itemizing`, all filer-gated | `revenues_payroll_tax` in `receipts.csv`, following payroll |
| `time_burden` outputs (it filters `filer != 0` at `time_burden.R:70`) | `mtr_*` weighted means (non-filers are in the denominator) |
| Any reform score with no `become_filer` path — see 4f | Percentile **cut points** in the production distribution universe (but *not* visible in the A/B's own `distribution.csv` — see 4g) |

**In 2020–2021** the rebate and fully-refundable-CTC baseline provisions are live, so
`n_returns`, `rebate`, `ctc_ref`, `ref` and `outlays_tax_credits` **must also move** — that
is the signal, not noise.

**`n_nonfilers` is computed and thrown away.** `summary_stats.R:143` derives
`n_nonfilers = !filer`, but the reported set `demographic_vars` (`:23-30`) is only
`n_tax_units`, `n_returns`, `n_returns_dep`, `n_dep`, `n_simple_filers`. The single most
relevant metric for this rework never reaches an output file. **Add `n_nonfilers` to
`demographic_vars`** — a one-line change that makes non-filer mass a permanent column of
`1040.csv` in every future run. While there, `n_adults` (`:144`) reads
`filer * (1 * (filing_status == 2))` where the adult count should be
`1 + (filing_status == 2)`, and `n_people` (`:145`) reads `n_dep` before its own
reassignment on the next line; both are unreported today, so fix or delete them rather than
leaving broken derivations in place.

**Payroll revenue will move, and that is the correct behaviour** — non-filers owe payroll
tax on wages whether or not they file, and the current non-filer slice already carries
$147.3B of wages (2022). Raking non-filer weights up by 15–25% and repairing their wage
distribution therefore changes a headline federal number. **Predict and quantify this
before the run**, roughly as the non-filer wage total × the effective payroll rate scaled
by the weight change; a payroll delta far from that estimate means the calibration moved
weight into the wrong wage cells. This is the one place where a non-filer-only change hits
federal revenue directly, and it would read as a bug if nobody expected it.

**The CBO comparison being invariant is the strongest single check in the battery.** If
`cbo_comparison.csv` is not identical between the old and new vintage under current law,
either a filer-side variable was touched or the filer weights moved — both contradict the
design (§5.2 rakes non-filer weights only; §5.3 leaves filer weights and the ledger
untouched). Investigate before proceeding, do not absorb it.

**The distributional shift is real and must be communicated, not explained away.** Adding
6–9M non-filer adults, correcting their age distribution, and giving them nonzero
investment income moves the income percentile boundaries that every published
distributional table rests on — with identical tax law. This is the mechanical consequence
of a better full-population base, but it means a vintage swap changes published
distribution tables, and that needs flagging to whoever owns those outputs.

### 4b. Pre-flight vintage gate and Tax-Data-side assertions, before any model run

Cheapest checks, and they catch most errors. Two layers.

**Layer 1 — a pre-flight vintage comparison, login-node, minutes.** New script
`nonfiler_residual/05_preflight_vintage.R --old <vintage> --new <vintage>`, run before any
cluster time is spent. Fail any of these and stop:

| Check | Criterion | Guards |
|---|---|---|
| Ordered id identity, every year 2017:2035 | `identical(old$id, new$id)` — exact, ordered | hazard 4.0(i), the positional random-number pairing |
| Within-vintage id order stability | `identical(ids_2017, ids_y)` for all `y` | the cross-year pairing assumption |
| Row count and column set equal; no new column names; `qual_div` absent | exact | the class of bug F3 came from |
| **Filer slice byte-equality** — the `filer == 1` subset identical on every column including `weight`, all years | exact | proves the "non-filer only" claim *before* any run, and is what makes 4a an exact-equality test rather than a tolerance argument |
| `filer ∈ {0,1}`, `dep_status ∈ {0,1}`, no NA; `filer == 0 ⟹ dep_status == 0` | hard | |

Also make the Stage-D diagnostics re-runnable against a candidate vintage: `03_diagnose_
current_nonfilers.R:47-53` currently hardcodes the vintage from `interface_versions.yaml`.
Add a `--vintage` argument, then re-run `--tables` on each candidate and diff T1–T4 against
V0. This costs nothing (13k rows per year) and is the primary acceptance evidence for F1–F4.

**Layer 2 — in-pipeline assertions.** In `impute_nonfilers.R` and `calibrate_nonfilers.R`:

**Hard (`stopifnot`) — invariants that cannot be true and wrong:**
- `filer == 0` and `dep_status == 0` on every appended record (§3a — currently only
  implicit via the zero-fill at `impute_nonfilers.R:136-142`).
- **Row order preserved end to end:** `identical(ids_in, ids_out)`. This is the assertion
  that makes hazard 4.0(i) a non-issue rather than a landmine.
- No `NA` in the columns the append writes; `age1` within the band it was drawn from;
  `age1 >= 18`; `filing_status == 2 ⟹ age2 >= 18`.
- `all(weight > 0)` and `is.finite(weight)` on the non-filer slice — a rake against a
  near-zero target cell can produce zeros or infinities.
- Filer weights bit-identical before and after `calibrate_nonfilers.R` — this is the
  guarantee that makes 4a's invariance prediction hold, so assert it rather than hope.
- Post-calibration non-filer adult total within the P4 tolerance of the target, and each of
  the ≤14 age × marital cells within 0.5% of its own target.
- `div_ord + div_pref` reconciles to the DINA `fidiv` aggregate within 1% — the actual test
  of the F3 dividend fix.
- **No column written that is absent from the committed output schema** — the generic form
  of the `qual_div` bug, which is exactly the failure of writing a column nobody checks.

**Soft (printed before/after diagnostic table) — judgment calls:**
- Non-filer adult level vs the 38–41M comparable-universe anchor.
- Age distribution vs `resources/nonfiler_age_shape.csv` (the F2 fix — report the 18–25
  and 65+ shares explicitly, since those are the two the diagnostics indicted).
- Receipt rates for interest / dividends / gains / SS / pensions against Pub 5785's
  14% / 9% / 4% / 48% / 14%. **These are ceilings, not targets** — our imputed non-filers
  should sit at or below them, because Pub 5785's universe is information-return based and
  wider than ours.
- Weighted totals by `income_tier()` before/after, since the state fit's non-filer prior
  keys on it.
- **Weight concentration.** 13,204 non-filer records carry 27.6M units today (mean weight
  ≈ 2,090); at 38–41M adults the mean goes to ~2,900–3,100, and the 18–25 band must carry
  ~11.5M adults on roughly 3,200 records. Report the max and 99th-percentile non-filer
  weight and the before/after weight-ratio distribution. Nationally this is tolerable; it is
  thin for the state × age-band cells the rework exists to serve, so record it here rather
  than letting step 5 discover it.

### 4c. Stage the changes as separate vintages

The four Tax-Data changes touch different things and two of them can mask each other (the
calibration rake and the aging fix both move non-filer weights). Build vintages
incrementally:

| Vintage | Contains | What its A/B isolates |
|---|---|---|
| V0 | current production | baseline for every diff |
| V1 | 3a composition fixes only (dividends, investment income, age draw) — **no reweighting** | Attribute changes at fixed weights. `n_tax_units`, `n_nonfilers` **and `totals/payroll.csv`** must all be **unchanged** (the fixes touch investment income and age, never wages); only distribution cut points and `mtr_*` move |
| V2 | V1 + `calibrate_nonfilers.R` | The level fix alone. `n_nonfilers` and payroll move; all **1040** filer-gated aggregates still frozen |
| V3 | V2 + `project_puf.R` aging fix | Projection-year behaviour only — 2017 and other historical years should be **identical to V2** |

The staging pays off precisely because payroll separates the two changes: it must be frozen
in V1 and must move in V2. V1's prediction is the sharpest — a change in `n_tax_units` or
payroll at that stage means the composition fix accidentally touched weights or wages. V3's
is nearly as sharp: any change in a pre-2018 year means the aging fix leaked into history.

Whether this is worth the extra cluster runs: **yes for V1 and V3**, because each has an
exact-equality prediction that a combined run cannot test. V2 could be folded into V3 if
cluster time is tight, at the cost of losing the level-vs-aging attribution.

### 4d. The vintage A/B runs

Use the pattern already in `config/runscripts/tests/new_baseline.csv`, which runs the same
`baseline` tax law twice against different Tax-Data vintages via the
`dep.Tax-Data.vintage` / `dep.Tax-Data.ID` columns:

**Run 1 — the cheap A/B, 3 years, all four vintages.**
`config/runscripts/tests/nonfiler_ab.csv`:

```csv
ID,tax_law,behavior,years,dist_years,mtr_vars,mtr_types,dep.Tax-Data.vintage,dep.Tax-Data.ID
baseline,baseline,,2020:2022,2022,,,<V0>,baseline
nf_v1,baseline,,2020:2022,2022,,,<V1>,baseline
nf_v2,baseline,,2020:2022,2022,,,<V2>,baseline
nf_v3,baseline,,2020:2022,2022,,,<V3>,baseline
```

Years **2020:2022** deliberately: 2020 and 2021 are the live become-filer years per hazard
4.0(iii), and 2022 is both the anchor year and the first CBO-overlap year. Because
`stacked = 1` makes `build_stacked_1040_reports()` compute `. - lag(.)` in runscript row
order within year, this single runscript yields the V1/V2/V3 incremental attribution for
free.

**Run 2 — the full window, for the aging fix only.** The aging fix is the one change whose
signature is *year-shaped*, so three years cannot test it. Same runscript with
`years 2017:2030`, `dist_years 2026`, and only the `baseline` (V0) and `nf_v3` rows — V1 and
V2 add nothing over the window once run 1 passes. Acceptance: `n_tax_units` growth smooth
and monotone (no year-over-year jump > 1%); non-filer share of units flat within ±0.3pp from
2019 on (the direct F4 fix, cross-checked against T4); `n_returns` path identical to V0's;
no discontinuity in `revenues_payroll_tax` at the 2020 `population_factors_2020plus` seam.

Notes that apply to both:

- **Set `dist_years` explicitly.** Blank means *all years* (`config_parser.R:351-352`),
  against a Phase-3b budget of roughly an hour and 16GB, while `process_for_distribution()`
  expands each year's records sixfold. This is the one configuration mistake that turns a
  trivial run into a failed one.
- **`pct_sample = 1`, not less.** The non-filer slice is only ~13,204 records; sampling at
  0.1 leaves ~1,320 and destroys precisely the signal under test, while saving little
  (runtime is dominated by filer records). The A/B *is* paired — same `set.seed(76)`, same
  `sample_ids` — so a sampled run is defensible for the filer-side invariance check alone,
  but since that check is exact-equality anyway there is no reason to accept the noise. Use
  a `pct_sample = 0.1` pass only as a smoke test that the pipeline runs.
- **`delete_detail = 0`** is required: every record-level diagnostic below reads
  `static/detail/{year}.csv`. Budget 10–20GB on scratch and clean up after run 2.
- **`mtr_vars`:** include at least one so the `mtr_*` shift is observed rather than inferred.
- **Hazard 4.0(ii) applies:** `sample_ids` come from the *first* row's vintage (V0). With the
  4b gate passed this is a no-op, but do not run the A/B before that gate passes.

**Files to diff**, per scenario, under `<vintage_root>/<id>/static/` (written at
`src/sim/run.R:212-232`): `totals/1040.csv`, `totals/1040_by_agi.csv`, `totals/payroll.csv`,
`totals/receipts.csv`, plus `stacked_1040.xlsx` for the V1/V2/V3 attribution. Write a diff
script that classifies every changed cell against the 4a table and **fails on any change in
the must-not-move column** — that is the acceptance gate, not eyeballing. Commit it beside
the runscript so the next Tax-Data vintage swap is checked the same way.

Acceptance, in order:

1. **V1:** `n_tax_units`, `n_returns` and every `payroll.csv` column identical to V0 to full
   printed precision, all years. Any movement means row order or filer weights changed —
   back to the 4b gate. Only 2020–2021 `rebate`/`ctc_ref` may move, and only through the AGI
   phase-out on newly-nonzero non-filer investment income; expect well under $1B.
2. **V2:** `n_returns` identical to V0 in 2022 (2020–21 exempt); `n_tax_units` up by exactly
   the non-filer unit delta measured in 4b; and the `revenues_payroll_tax` delta explained to
   within 10% by (Δ non-filer weighted `gross_wages`) × the effective OASDI+HI rate —
   **computed from the detail files, not asserted**.
3. **V2, 2021 — the external check.** Baseline `rebate`, `ctc_ref` and `outlays_tax_credits`
   rise, and their *levels* are compared against published actuals: EIP3 ≈ $402–411B
   disbursed (~165M payments), advance CTC ≈ $93B. **A vintage whose 2021 baseline overshoots
   the actuals is over-massed regardless of what the anchors say.** This is the only place a
   model output that depends on the non-filer level meets an independent real-world number.
4. **V3:** 2017 bit-identical to V2.
5. **Non-filer credit mass, from `static/detail/{year}.csv`** (all needed columns are in
   `detail_vars`, `config_parser.R:241-258`): weighted count of `filer == 0`, non-filer adults
   `sum(weight * (1 + (filing_status == 2)))`, and non-filer `sum(weight * eitc)`,
   `sum(weight * ctc_ref)`, `sum(weight * rebate)`. That last group is credit mass the model
   computes and then multiplies out of every total — see 4f.

### 4e. The CBO benchmark — a null test, and say so

`cbo_comparison.R` benchmarks the baseline against the committed
`resources/cbo/cbo_iit_detail_feb2026.csv`. Verified properties that matter:

- **It runs only for `ID == 'baseline'`** (stated in its own header, and gated in
  `run.R`/`slurm/aggregate.R`). In a `new_baseline.csv`-style A/B the new vintage sits in a
  non-baseline row and **gets no CBO comparison at all**. A separate single-row runscript is
  required: `config/runscripts/tests/nonfiler_cbo.csv` with one `baseline` row pointing at
  V3, years `2022:2030`, compared against the V0 production run's `cbo_comparison.csv`.
- **The reference covers 2022–2036 only** (verified: minimum year 2022; 2022
  `Number of returns (millions)` = **161.3M**). TY2022 is in scope; **TY2017 is not, and
  never will be from this reference.** Benchmark 2017 `n_returns` against SOI Pub 1304
  TY2017 (~152.9M returns) by hand and record it in the findings memo. Do **not** regenerate
  the reference via `other/cbo/process_cbo_revenue.py` for this exercise — changing both
  arms at once destroys the comparison.

**`n_returns` should not move, and that is the point.** `summary_stats.R:141` defines
`n_returns = filer`, and the rework changes non-filer records only, so the correct
expectation against CBO's 161.3M is **zero change**. The CBO comparison's role here is a
**tripwire on the invariance claim, not a benchmark on the reworked object** — CBO's 1040
build-up publishes no non-filer line and therefore provides no direct discipline on
non-filers whatsoever. Be explicit about this in the write-up. The defensible claim is: the
anchors set the level, the 2021 baseline refundable-credit actuals are the one external
check on that level, and CBO confirms we did not disturb the filer side. Anything stronger
overstates what CBO's build-up contains.

Acceptance: `n_returns` `pct_diff` changes by < 0.1pp from V0's; every AGI, taxable-income
and tax line changes by < 0.05pp (all are filer-restricted, so more than that is a 4b gate
failure that slipped through); `n_itemizing` unchanged (non-filers do not itemize).

Worth adding to `cbo_comparison_mapping()` while in the file — these rows exist in the
reference and are unmapped today: `Earned income tax credit` → `eitc`,
`Child tax credit/credit for other dependents` → `ctc_nonref + ctc_ref`, and
`Number affected by the AMT` → `n_liab_amt`. EITC and CTC are the two lines the reform tests
below exercise hardest; having them permanently benchmarked is cheap and useful beyond this
project.

### 4f. Reform scenarios — where the change *should* bite

The change shows up through the become-filer path: `src/calc/do_taxes.R:126-127` sets
`filer = filer + (become_filer_ctc == 1 | become_filer_rebate == 1)`, with
`become_filer_ctc` at `src/calc/functions/credits/ctc.R:232`
(`filer == 0 & qual_ei == 0 & ctc_ref > 0`) and `become_filer_rebate` at
`src/calc/functions/credits/rebate.R:86` (`filer == 0 & rebate > 0`). Both gate on
`filer == 0`, so the increment is at most 1 — `filer` stays binary and `n_returns` remains a
clean return count.

`calc_rev_est()` always differences against the single `baseline` row, so scoring a reform
*at* a vintage requires that vintage on **every** row. Build two paired runscripts,
`nonfiler_reforms_v0.csv` and `nonfiler_reforms_v3.csv`, identical but for the vintage
columns — and set the vintage on the reform rows too, since omitting it silently reverts to
the `interface_versions.yaml` default.

| Scenario | Expected | Red flag |
|---|---|---|
| **Fully refundable CTC** (`tests/ctc_baseline.csv` pattern) | Outlay **rises**, bounded by (Δ non-filer units with `n_dep > 0` and `qual_ei == 0`) × max refundable value. The F2 age fix moves mass from 65+ into 18–25, *raising* the share of non-filers with young children, so expect a **larger** rise than the pure level effect implies | Outlay falls, or rises beyond the bound. Also: `Δn_returns` and `Δoutlays` must move **together** — they are linked through `do_taxes.R:127` |
| **Rebate / UBI** (`tests/rebate.csv`; or free, via the 2021 baseline) | The cleanest quantitative check: `become_filer_rebate` has **no earnings condition**, so `Δn_returns(V3)/Δn_returns(V0)` should track `nonfiler_units(V3)/nonfiler_units(V0)` within ±5%, the residual attributable to the AGI phase-out on newly-nonzero investment income and to the marital-composition change from the rake | Ratio materially below the unit ratio ⇒ new non-filers are phasing out, i.e. the repaired investment income is too large (contradicting T3) or landed on the wrong 1040 line. Above ⇒ a weight/count accounting error |
| **Non-refundable control** (`tests/amt_repeal.csv` or `mort_int_repeal.csv`) | Score **bit-identical** across V0 and V3 | Any change at all. This is the single cleanest "did we break the filer side" test in the battery, because it has no non-filer channel whatsoever |

### 4f-bis. A finding the reform tests will surface: EITC has no become-filer path

**Correcting an assumption worth stating plainly.** I expected an EITC expansion to score
differently under the reworked file. It will not. `grep become_filer src/` returns exactly
two definitions — `ctc.R:232` and `rebate.R:86`. **There is no `become_filer_eitc`.** An
EITC reform therefore scores *identically* across vintages, and `Δscore ≈ 0` is the
**passing** result; a nonzero EITC delta would indicate the weight change leaked into filer
records.

That is a modelling gap, not a test artifact, and it cuts against the proposal's own
motivation. The proposal (§1) indicts DINA precisely because "all imputed tax units with
children and positive earned income file taxes, so there are no non-filers with eligibility
for refundable tax credits." The rework fixes that on the *data* side — it creates
earnings-bearing non-filer units with children. But on the *model* side those units still
cannot claim EITC by filing, and their computed `eitc` is multiplied out of every total by
the `* filer` gate in `get_1040_totals()`.

Compounding it, `become_filer_ctc` requires `qual_ei == 0` **exactly**, so a non-filer with
$1 of earned income who gains a refundable CTC keeps `filer = 0` and has its `ctc_ref`
multiplied out too. The rework *grows* the positive-earnings non-filer population, so this
silently-dropped credit mass grows with it — meaning the reworked file could make refundable-
credit scores *less* complete while looking like it did nothing.

**Action:** measure it (4d acceptance item 5 reports non-filer `eitc`/`ctc_ref`/`rebate`
mass), and raise the `qual_ei == 0` condition and the missing EITC path as an explicit
design decision **before** shipping the vintage. Do not fix either blind — both are
deliberate-looking choices whose rationale is not documented, and changing them changes
published refundable-credit scores. But do not let the rework ship with a memo claiming it
improves refundable-credit analysis when for EITC it cannot.

### 4g. Distributional and horizontal checks — do NOT read the built-in tables naively

Two verified structural problems mean the shipped tables cannot answer this question as-is:

- **`distribution.csv` is blind to a vintage reweighting.** `process_for_distribution()`
  (`distribution.R:130-163`) reads `weight`, `expanded_inc`, `agi`, ages and `filing_status`
  from the **baseline** detail file and joins only `(id, liab_iit_pr_reform)` from the
  counterfactual arm. In a vintage A/B, the new vintage's weights and incomes never enter the
  table. It is a *policy* differencer, not a *vintage* differencer. **This does not mean the
  distributional consequence is absent** — it means the A/B run will not show it. Once V3
  becomes the production baseline, every published reform's quintile boundaries and age cuts
  move, because the universe is all `dep_status == 0` units *including* non-filers.
- **`horizontal.csv` from a vintage A/B will contain garbage.** `build_horizontal_table()`
  (`horizontal.R:85-101`) filters `expanded_inc > 0` on the bound frame, then `left_join`s
  `inc` from the *baseline* arm also filtered to `expanded_inc > 0`. A record that is
  income-zero in the old vintage but income-positive in the new one — **exactly what the F3
  investment-income repair creates** — survives the filter with `inc = NA`, giving
  `etr = NA` and an NA income rank that contaminates the top quintile. Discard this table
  until `build_horizontal_table()` ranks on the scenario's own income (or uses an
  `inner_join`). Worth fixing on its own merits: it is a latent bug for *any* counterfactual
  that changes the set of income-positive records.

**Do this instead — a purpose-built level comparison.** New script
`nonfiler_residual/06_compare_level_dist.R`, reading `static/detail/2022.csv` from each arm
and computing, **on each arm separately** with `dep_status == 0`: weighted `expanded_inc`
quintile boundaries; count and share in the negative-income group; mean `expanded_inc` and
mean `liab_iit_net` by quintile; and the age cuts `distribution.R` uses. Expected:

- Quintile 1's boundary and mean income **fall**; Q2–Q5 boundaries shift down modestly.
- The percentile universe grows by the non-filer delta, so a fixed dollar income maps to a
  **higher** percentile.
- The `29 and under` share **rises** and `65+` **falls**, tracking the F2 correction. This is
  the most visible distributional consequence and the entire point of the age fix.
- **Red flags:** the negative-income group grows (nothing here should create negative
  expanded income — if it does, the DINA national-income mapping produced negative interest
  or dividends); any *upper*-quintile boundary moving more than ~0.5%; new non-filer mass
  landing above Q2; or a *fall* in the `29 and under` share.

**Horizontal equity, once the NA bug is fixed:** expect Quintile 1's within-group IQR to
**fall**, because the repair adds a large homogeneous block of `etr ≈ 0` records to the
bottom. That is a mechanical artifact of universe expansion, **not** an equity improvement,
and must be labelled as such. Red flag: Q1 IQR *rises*, which would mean the new non-filers
carry heterogeneous nonzero liability — i.e. repaired investment income large enough to
generate tax on people the model says do not file. Cross-check against Pub 5785's *amounts*
(≈$1.5B interest, $2.7B dividends, $1.2B gains across ~50M people in TY2016 — very small per
capita), not just its receipt rates. **This is the check most likely to catch an
over-generous hot-deck** in step 3a.

`time_burden.R:70` filters `filer != 0`, so its outputs belong in the must-not-move set.

### 4h. External triangulation — and its limits

Realistic:
- **The 2021 baseline refundable-credit levels vs Treasury/IRS actuals** (EIP3 ≈ $402–411B
  and ~165M payments; advance CTC ≈ $93B). The highest-value item in this section and the
  only external number that constrains the non-filer *level* through a tax-model output.
- **CBO's `Number of returns`** — filer side only, a null test (4e).
- **SOI Pub 1304 return counts** for TY2017 and TY2022 — filer side, and it covers 2017
  where the CBO reference does not.
- **TPC's published tax-unit count** (they report units *including* non-filers, ~190–200M)
  vs our `n_tax_units`. A genuine level check with a real concept gap: their unit definition
  and non-filer construction differ from ours, so treat a 5–10% gap as informative rather
  than failing.
- **The `cross_model/` harness, at record level.** It is unweighted, so it can say nothing
  about counts or levels. But it can do one very useful thing cheaply: push a stratified
  sample of the repaired non-filer records through TAXSIM-35 / PolicyEngine **as if they
  filed**, and check `agi`, `txbl_inc`, `liab_iit`, `eitc`, `ctc_ref` record by record. That
  is precisely the bug class that produced the `qual_div` failure — new income landing on the
  wrong line. Log divergences to `cross_model/federal_divergences.md`, which already exists.

Not realistic, and say so rather than implying otherwise:
- **The Pub 5785 receipt rates and the PEP−Pub 1304 age shape are calibration targets**, not
  validation. Checking output against them is a convergence check. There is no independent
  source for a non-filer age distribution or non-filer investment-income receipt rates, and
  Pub 1304 Table 1.1's bottom-AGI receipt counts — the closest thing to an independent bound
  — is also assigned as discipline in step 3a. Be honest that this loop is closed.
- **PolicyEngine's non-filer *count* is not a usable benchmark.** Their Enhanced-CPS unit
  definition, their own filing-requirement model, and their calibration targets differ enough
  that a level comparison is uninterpretable. Their value here is as a *record-level*
  calculator check (above) and as background for the ASEC construction research (step 1.1),
  not as a count benchmark.
- Anything requiring another model's *age distribution of non-filers* — not published at the
  granularity F2 is about.
- Reconciling our count directly to Pub 5785's 49.7–51.7M *persons*: different universe
  (information-return based, person level) and different years. A triangulation, not a
  target — which is exactly how the proposal §3.2 uses it.

### 4i. Regression guard

The rework makes `filer` load-bearing in ways nothing currently asserts. Permanent checks:

- **In Tax-Data:** the hard assertions from 4b, in the pipeline itself so a future change
  cannot silently reintroduce zero-investment-income or flat-age non-filers.
- **In Tax-Simulator `src/tests/`:** a new test asserting the filer-gating contract — that
  every `get_1040_totals()` tax aggregate is invariant to non-filer weights, while
  `n_tax_units`, `n_nonfilers` and every `get_pr_totals()` aggregate scale with them.
  Construct it by taking a small tax-unit fixture, doubling every non-filer weight, and
  asserting both halves. This encodes the 4a table as an executable invariant rather than
  a claim in a memo, and it would catch a future post-processing change that dropped the
  `* filer` gate in `get_1040_totals()` — or silently added one to `get_pr_totals()`.
- **The diff script from 4d**, committed alongside the runscript, so the next Tax-Data
  vintage swap is checked the same way.
- **An ordered-id-identity test** (`test_nonfiler_id_order()`), asserting `identical()` on the
  `id` column across all available years. This is the guard that would catch the positional
  random-number hazard 4.0(i) — the highest-probability way this rework silently corrupts the
  filer side, and currently unguarded anywhere.
- **A schema guard in `read_microdata()`** (`src/data/economy.R:477-494`), which today does
  `fread()` with **zero validation** — a renamed or dropped Tax-Data column produces NAs or an
  unhelpful downstream error. This is the natural home for the `filer`/`dep_status` domain
  checks and a column manifest, and it protects every consumer, not just this project.

Structure these as a `src/tests/test_nonfilers.R` function-only file following the convention
in `src/tests/test_state_tax_law.R`, guarded with a `file.exists()` skip so the suite still
runs where the restricted data is absent.

---

## Step 5 — State-weights rework (part (c))

All in `src/data/state_weights.R`, on the new Tax-Data vintage. **Hard prerequisite:** the
Tax-Data age fix (3a) must land first, or `age_band(tu_n$age1)` cell assignment stays
smeared across the very dimension the anchors discipline.

### 5a. Margins v1 in `build_acs_margins()`

The GQ treatment from step 2 plus the v1a deterministic threshold upgrades (age-65 bump,
$400 SE rule, dependents with own income above the dependent floor forming filing units,
`SCHOOL` keeping 19–24 household students dependent), then the v1b probabilistic layer
behind a `filing_model = c("v1a", "cilke")` argument, carrying the ASEC-estimated
coefficients transferred per step 1.6.

### 5b. Non-filer targets in `build_weight_inputs()` (`:718-758`)

This is the substantive change: today the non-filer partition has **count-only targets on
1,390 `state × age_band(7) × income_tier(5)` cells with `x ≡ 1`** (`:745`) and priors from
the v0 ACS margin. Because each cell has single membership, the calibration reproduces the
biased margin *exactly in one pass* — it is prior reproduction, not a fit.

- **Primary targets:** the residual anchors `(state × age_band)`, share-normalized like
  every other target (PUF non-filer national adult total × residual state share within
  band), with the **adult x-vector `1 + (filing_status == 2)`** per **D5** — the anchors
  are adult counts, not unit counts.
- **Additional margins** (needs P3): OASDI beneficiary counts as targets on the 65+
  band(s); the covered-worker margin as a `has_wages` count target, with a documented
  tolerance for the returns-vs-persons concept gap. **QCEW stays a diagnostic, never a
  target** — it counts jobs and payroll, not people.
- **Income tiers move to the prior.** Keep `income_tier` in the prior (upgraded v1 ACS
  shares) and demote the 1,390 count-only cells from exact targets to prior-only/soft.
  The anchors own the level; the ACS owns the within-state shape. This is what turns the
  non-filer partition into a genuine calibration that actually runs through
  `fit_gradient()` — today it only ever sees `fit_calibration()`-trivial cells.
- **Cell support is adequate** (**D2**, T6): smallest state residuals are WY 56.4k, SD
  73.0k, VT 73.9k, AK 87.3k, ND 87.6k — far above thin-cell territory for state × 6 bands.

### 5c. What "impose it jointly" does and does not mean here

The proposal's worked failure mode (filer over-assignment → raking drains the non-filer
pool → state income-per-adult biased up while every targeted diagnostic looks perfect)
does **not** apply mechanically inside `build_split_weights()`: the two partitions are fit
separately and `Σ_s W[i,s] = w_i` holds within each, so weight cannot drain between filers
and non-filers inside the state fit. The failure lives in the two places this plan
addresses instead — the national split (step 3) and the state placement of the non-filer
partition (5b). The population identity is then enforced through **target
self-consistency**: if the filer weights hit their HT2 return-count targets and the
non-filer weights hit the residual anchors, fitted total adults by state reproduce PEP by
construction, because the targets were built to add up.

Escalation path if soft-target trade-offs push the identity outside tolerance: a single
stacked fit. `fit_gradient()` already accepts arbitrary row sets, so concatenating
`(w, P0)` across partitions and adding identity targets spanning both is mechanical. **Do
not build it speculatively** — build it only if the diagnostic in 5d fails.

### 5d. Validation additions in `validate_state_weights.R`

- **Population-identity check:** fitted filer adults + fitted non-filer adults per state
  (× age band where supported) vs PEP, with the P4 tolerance. This is the diagnostic that
  decides whether the stacked-fit escalation is needed.
- **Re-run the EITC take-up correlation.** The postmortem's `n_returns` signed-error
  correlation of **−0.61** with take-up (`state_weights_phase1_summary.md` §5.3) was
  pre-registered as the filing-propensity leak; it should attenuate once filing is modeled
  rather than assumed. Note F5 found the v0-vs-residual *gap* correlates only +0.12 with
  take-up, so most of the v0 error geography is GQ and threshold coarseness — do not
  expect the −0.61 to vanish entirely, and say so when reporting.
- **New held-out metric where gains *are* expected:** state adults by age band vs PEP,
  currently scored nowhere.

### 5e. Re-fit

Config 7 hyperparameters (counts IPF prior → gradient, β=1e-4, λ=1, lr 0.1 cosine, 3,000
steps) unless a fresh sweep says otherwise. Run under `sbatch` with inputs staged on NFS
scratch — the login node OOM-kills at ~7–8 GB and piping masks the kill (pipeline exit
status is `tail`'s).

**State honestly what this does not fix.** The filer-partition held-out misses — taxable
pensions 17.0, Schedule C 30.8, capital gains 61.0 MARD — are a filer-target *poverty*
problem, not a non-filer problem. Do not promise held-out MARD gains from this rework.
Likewise the 239 of 10,229 filer cells (2.3%) that miss 2% in every sweep config are a
target-consistency issue, untouched here.

---

## Step 6 — Production swap-in

Per the existing checklist (`state_weights_phase1_summary.md:260-263`, `STATUS.md` Phase-1
close-out):

1. Structural-core pruning.
2. `build_split_weights(method = 'gradient')`.
3. The `state_weights_{year}.csv` writer for 2014 and 2016–2022 — schema unchanged
   `(id, state, weight)`, **vintage-tagged** (`v0-margins` vs `v1-residual`) so downstream
   consumers can tell the two fits apart. Decide the 2013/2015 HT2 gap: interpolate or
   skip.
4. Projection-year carry-forward.
5. **The dispatcher flip.** `src/sim/run.R:430-435` currently calls
   `build_state_weights(..., method = 'placeholder', ...)`, and
   `build_state_weights()` (`state_weights.R:1004-1025`) hard-`stop()`s on anything else.
   Flip it, and delete the placeholder trap documented at `:995-999` (rows sum to
   `(n_states/53) × weight_i` when a subset of states is requested — so nobody can
   reconstruct a national total from `state.csv`).
6. Pilot-state liability re-check. Watch **NY** in particular: under the v0 fit it came in
   at $63.55bn against NY DTF's TY2022 full-year-resident liability of $41.17bn (their
   10.7M-return All-Filers total is $50.48bn, so ours overstates even the wider concept) —
   consistent with too much top-income mass. NY is also where the candidate weight sets
   differ most ($2.9bn, 4.7%). Treat it as the decisive diagnostic, with the caveat below.

---

## Step 7 — Interfaces, shared machinery, and documentation

**One definition per computation** (design memo §7.2):

- `ht2_filing_persons()` — already promoted out of `compare_individuals_acs_irs()` to
  `state_weights.R:339-348`. Single home in Tax-Simulator; the diagnostic, the target
  builder, and Affordability-Index all call it.
- `residual_anchors_{year}.csv` — one builder (the Stage-D scripts, promoted from
  prototype to maintained code), three consumers: the state target assembly, Tax-Data (as
  a committed snapshot), and Affordability-Index.
- **`filer` flag contract** — authority stays with Tax-Data; post-rework it means
  "residual-anchored non-filing unit." Document in both repos and add the missing
  assertion (3a).
- **Universe tags are mandatory on anything shared.** The two state systems legitimately
  differ on group quarters: the Affordability ACS spine covers the *household* population
  and subtracts GQ from its anchors; the PUF covers the *full resident* population and
  must not. Any anchor or margin file crossing between them carries an explicit
  `resident` vs `household` tag, or the same file silently means two different things.
  This is the one place where sharing machinery between the two projects can go wrong
  quietly.

**Documentation updates — do not skip these:**

- **`other/state_tax_research/STATUS.md` says nothing about this workstream.** Verified:
  grepping it for `nonfiler|non-filer|residual|Stage D` returns only incidental hits, and
  its "Done" list still credits the v0 non-filer ACS cells that the Stage-D diagnostics
  indict. Neither `nonfiler_residual_design.md` nor `04_findings.md` appears in its
  companion-docs list at `:54-60`. Add the workstream, and add a note to the Phase 1
  close-out item recording that the non-filer rework lands first.
- `docs/model_documentation.md` — the non-filer append is documented in one sentence at
  `:82-87` with **no limitation recorded anywhere**. Document the method and its known
  omissions (dependent non-filers, MFS). Also update the placeholder caveats at `:368-369`
  and the open checkbox at `:408-411` once step 6 lands.
- `CLAUDE.md` — the `states` runscript column carries the "state weights are a uniform
  PLACEHOLDER" warning; remove it at swap-in.
- Add a pointer from the Affordability income memo's "Aligning the code" section to this
  workstream.
- The two `.docx` deliverables (`Non-Filer Proposal.docx`,
  `nonfiler_residual_design.docx`) are untracked. Decide whether they are committed
  artifacts or local renders.

---

## Discrepancies between the proposal and the implementation record

Worth resolving in the proposal text, since it is the document that will be read:

1. **Survey.** Proposal §2.2 says the filing model is built on the CPS ASEC; the design
   memo §3.2/§6.1 and all Stage-D code build it on the IPUMS **ACS** extract. Resolved
   here in favour of the proposal (decision 2), which means the design memo's §6.1 needs
   amending — it currently asserts "no new IPUMS pull is needed for anything in this memo."
   Note the ASEC route is also the *native* one for Cilke, which was estimated on the ASEC:
   the recalibration burden moves to the explicit ASEC→ACS transfer step rather than being
   embedded invisibly in an ACS-only fit.
2. **§3.5's robustness checks are weaker than they sound.** The proposal proposes comparing
   national non-filer counts and composition against PolicyEngine, TPC and CBO. Verified: the
   committed CBO reference is an IIT build-up with **no non-filer line at all**, so CBO can
   only confirm we did not disturb the filer side; and PolicyEngine's non-filer count rests on
   a different unit definition and its own filing model, so a level comparison is
   uninterpretable. TPC's tax-unit count is a genuine (if concept-gapped) check. The strongest
   external discipline available is one the proposal does not mention: the **2020–2021 baseline
   rebate and advance-CTC outlays against published actuals**, which depend directly on the
   non-filer level. Rewrite §3.5 around that.
3. **The parameter sources may be stale and the proposal treats them as fixed.** Cilke is
   1998 and Pub 5785 is TY2014–2016; the proposal cites both without noting that newer
   estimates may exist, and the 2020–2021 EIP non-filer outreach period generated substantial
   new evidence that postdates both. Research pass B (step 1.2) settles this; §2.2 and §3.1
   should record the outcome either way.
4. **The SSA targets are not yet obtainable.** Proposal §2.1 item 3 and §4.1 lean on SSA
   OASDI and covered-worker tabulations as anchors. ssa.gov 403-blocks the cluster; the
   stores exist but are empty. The proposal should note the manual-download dependency,
   and §6's vintage-pairing question should absorb the SSA geography-revision caveat that
   is currently a parenthetical.
5. **Worth adding to §6's open questions:** the proposal lists dependent non-filers and MFS as
   known omissions, but not the two model-side gaps the federal pass surfaced — that EITC has
   no become-filer path and that `become_filer_ctc` requires exactly zero earned income
   (4f-bis). Those bound what the rework can deliver for refundable-credit analysis, which is
   the proposal's headline motivation, so they belong in the memo rather than only in the code.
6. **Every quantitative claim in the proposal checks out** against Stage D — the 15–25%
   shortfall, the 9%/24% and 43%/25% age figures, 205.5/206.1 and 214.1/213.1 filing
   adults, 47.3M/46.5M residuals, 11%–28% state range, 0.78×–1.51×, ~8M GQ at 17%/28–42%,
   26.2M→33.0M and 14.7%→15.8%, and the 14%/9%/48% receipt rates. No corrections needed.

---

## Verification

The federal battery is step 4 (its 4a table is the acceptance gate). State-side and
end-to-end checks:

- **Anchors:** the two independent constructions of national filing adults must continue
  to agree within ~0.5% (205.5 vs 206.1M in 2017; 214.1 vs 213.1M in 2022), and the T1.6
  block sums must reproduce published all-returns totals exactly (the parse check already
  in `02_build_residual_anchors.R:96`).
- **Filing model:** ACS v1 filer units vs HT2 `n_returns` by state, inside the P4
  tolerance; ASEC-vs-ACS national filing rates reported side by side.
- **State fit:** targeted within-2% and MARD against config-7's 95.3%/0.43 baseline;
  the population identity per state within tolerance; state adults by age band vs PEP as
  the held-out metric; the EITC take-up correlation attenuated from −0.61.
- **Pilot liability:** IL / CO / NY recomputed; NY is the decisive case.
- **End-to-end:** a state-mode run under the new weights with `states` set, confirming
  federal outputs remain byte-identical to a federal-only run (the acceptance property
  already established in Phase 4).
- **Regression guard:** the permanent checks belong in `src/tests/` — see step 4.

## Sequence and rough effort

Stage D is done, so the critical path starts at P1–P3. The design memo's §7.1 estimates
are carried forward, with the ASEC step added by decision 2.

| # | Work | Depends on | Effort |
|---|---|---|---|
| P1–P2 | Vintage reconciliation, age-band decision | — | ~1 day, login node |
| P3 | SSA downloads (JI) | — | ~1 day, parallel |
| 1.0 | Locate/register the ASEC shared store | cluster access | ~1–2 days |
| ~~1.2~~ | ~~Cilke & Pub 5785 currency~~ — **DONE 2026-08-18**: replace Cilke with Mok (2017) | — | complete |
| 1.1 | Research pass A: ASEC unit/income construction | — | ~3–5 days, parallelizable |
| 2 | GQ treatment in `build_acs_margins()` | P2 | ~2–3 days |
| 1.3 | Filing models, joint calibration, ACS transfer | 1.0–1.2, P3, P4 | **~2–3 weeks** (the long pole) |
| 3 | Tax-Data rework + V1/V2/V3 vintages | P1, step 1.3 age shape | ~1–2 weeks + cluster runs |
| 4 | Federal validation battery | step 3 | ~1 week, mostly human diff-reading |
| 5 | State-weights margins/targets + re-fit | steps 1, 2, 3, P3 | ~2–3 weeks |
| 6 | Production swap-in | steps 4, 5 | ~1 week |
| 7 | Interfaces + documentation | ongoing | folded in |

Three things can start immediately and in parallel: the GQ fix (step 2), research pass A
(1.1), and the SSA downloads. **Research pass B (1.2) is already done** — and it paid for
itself: the answer was "yes, updated coefficients exist," so transcribing Cilke's 1998
values as the primary model would have been wasted work. What to transcribe now is **Mok
(2017) Table 14** — the PDF is in hand and verified; transcribe from a rendered image,
watching Panel E's reversed column order.
Step 1.3 is the long pole and the only piece whose scope is genuinely uncertain.

**Cluster compute is not the binding constraint on step 4.** The whole battery is a few
hundred core-minutes: run 1 is ~16 short jobs (30 min / 16GB each), the CBO run ~10, the full
window ~30, the paired reform runscripts ~24. The real costs are disk (`delete_detail = 0`
across four vintages: budget 10–20GB on scratch), the Phase-3b blowout if `dist_years` is left
blank, and human diff-reading — which the V1/V2/V3 staging minimizes by turning two of three
comparisons into exact-equality tests.

## Risk register

| Risk | Why it matters | Mitigation |
|---|---|---|
| **The comparable-universe anchor is mis-set** | Calibrating to 46.5M instead of 38–41M would inflate non-filer mass by ~15%; calibrating to 32.4M would entrench the current shortfall. The 18–19-year-old and adult-dependent wedges overlap heavily and only a lower bound (5.48M) is estimated | Plumb Table 1.7 into T1 — it is read in script 02 but never reaches the diagnostic (`03:154` hardcodes `dependent_filer_returns = NA_real_`). Estimate the overlap directly rather than carrying it in the tolerance |
| **ASEC and ACS disagree on national filing rates** | Would invalidate the transfer step and, with it, decision 2's whole premise | Make the comparison a gate in step 1.6, not a footnote. Fallback is the memo's ACS-only route, which is already fully specified |
| **SSA files never arrive** | Gates D6 state age layering and the covered-wage margin | State weights v1 can ship on PEP + HT2 residual anchors alone with the ACS shape smoothing all ages; the SSA margins become a v1.1 refinement. Note this degrades the 65+ state split, which is where non-filer mass concentrates |
| **Age-band reconciliation done implicitly** | The anchors exist to discipline age; a silent mismatch at 25/26 and 65+ would quietly undo F2's fix | P2 is a blocking decision with a written record, not an implementation detail |
| **NY liability does not improve** | NY was 63.55 vs 41.17 benchmark and is where candidate weight sets differ most; if the non-filer rework does not move it, the cause is filer-side top-income placement, which this plan explicitly does not fix | Pre-register NY as diagnostic, not acceptance. Report it either way rather than reframing after the fact |
| **Two Tax-Data changes interact** | The calibration rake and the aging fix both touch non-filer weights; a compensating pair of errors would look clean in aggregate | Stage them as separate vintages and A/B each independently (step 3d / step 4c) |
| **Row order changes and silently rerandomizes every filer** | `bind_cols(globals$random_numbers)` pairs draws by **position** (`run.R:348-357`); a sort inside `calibrate_nonfilers.R` would move filer-side aggregates for reasons unrelated to non-filers and make the whole battery uninterpretable | The ordered-id gate in 4b, the in-pipeline `identical(ids_in, ids_out)` assertion, and the permanent regression test in 4i. Highest-probability silent failure in the plan |
| **Payroll revenue moves and nothing benchmarks it** | `get_pr_totals()` is not filer-gated, so raking non-filer weights up 15–25% changes baseline payroll receipts with no tax-law change — and `cbo_comparison.R` is IIT-detail only, so no check exists | Predict the magnitude before running (4a/4d item 2). **Answer the upstream question first: do Tax-Data's aggregate wage control totals already include non-filer records?** If they do, raking non-filers up without renormalizing filers — which §5.2 explicitly declines to do — breaks the national wage reconciliation. Check total `gross_wages` against the Macro-Projections/NIPA wage series and SSA covered wages |
| **The rework cannot improve EITC scoring** | There is no `become_filer_eitc`, and `become_filer_ctc` requires `qual_ei == 0` exactly, so the newly-created earnings-bearing non-filers' refundable credits are computed and then multiplied out of every total | Measure the dropped credit mass (4d item 5); raise both conditions as explicit design decisions before shipping (4f-bis). Do not let the vintage ship behind a claim it improves refundable-credit analysis generally |
| **Published distribution tables move at swap-in** | The distribution universe is all `dep_status == 0` units *including* non-filers, so quintile boundaries and age cuts shift under identical law — and the A/B's own `distribution.csv` will not reveal it, because `process_for_distribution()` reads everything from the baseline arm | Size it with the purpose-built script in 4g *before* the swap-in, and tell whoever owns the published tables ahead of time rather than after someone asks why last quarter's numbers changed |
| **Two post-processing routines mislead under a vintage A/B** | `horizontal.csv` produces NA-contaminated top-quintile IQRs; `distribution.csv` is blind to reweighting | Fix `build_horizontal_table()`'s ranking join (a latent bug regardless), and use the 4g replacement script rather than reading the shipped tables as if they worked |

## Ops notes

- All fits and the ACS/ASEC tabulations run under `sbatch`; the login node OOM-kills at
  ~7–8 GB and **piping masks the kill** (pipeline exit status is `tail`'s, so a killed job
  looks like a clean one). Existing pattern: `nonfiler_residual/run_acs_tabulation.sbatch`
  (`--mem=48G`, `--time=02:00:00`, `module load R/4.4.2-gfbf-2024a`), and scratch at
  `/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`.
- `state_weights.R` uses `fread(cmd = 'zcat ...')` at `:121` — POSIX only, will not run on
  Windows. Cluster-only for anything touching HT2.
- On the HPC, R is not on `PATH` by default: load the module in the **same** shell command
  that calls `Rscript`.
