---
title: "A state-weight-inclusive model with an updated non-filer pull — the plan"
role: plan
workstream: state_weights
status: current
updated: 2026-08-23
sot: self
supersedes: []
superseded_by: null
---

# A state-weight-inclusive model with an updated non-filer pull — the plan

**The plan of record for this workstream. Start here.** What is decided, what is
next, what is blocked, and the critical path, through to the production
state-weights swap-in.

**Scope:** everything between here and the production state-weights swap-in
(groups P–H), plus a **second phase added 2026-08-23** covering the national
tenure / rent / property-tax imputation and its per-state use (groups I–J).
Weights and non-filers are one workstream because the non-filer rework lands
*before* the Phase 1 swap-in, so the fit happens once on upgraded margins rather
than fit-then-refit. Tenure and rent joined it for the same reason in reverse:
they need the ACS cells and the fit machinery that groups A–F build, and the
cross-model triage keeps terminating in their absence.

**Method:** every claim below was checked against the code and the shared store
on 2026-08-19, not read off the memos. Where a memo and the tree disagree, the
tree wins and the discrepancy is called out.

**Companions** — one job each (`research/README.md` holds the role table):
`nonfiler_residual_design.md` is the **method** of record (the *why*, and each
decision with its evidence); `nonfiler_residual/04_findings.md` is the
**evidence** (Stage D, F1–F7, frozen); `nonfiler_federal_validation.md` is the
**procedure** for task group E; `research/decisions_log.md` records settled
arguments. Also read against: `research/STATUS.md`, `research/state_tax/plan.md`
§2.1, `state_weights_phase1_summary.md` §5/§7,
`research/archive/07_ssa_inputs_plan_2026-08-19_executed.md`.

**Merged 2026-08-19** from two same-scope documents: the 2026-08-19 to-do that
had been checked line-by-line against the tree, and a fuller 2026-08-18 plan
that had been living outside version control. The to-do won on every fact; the
imported plan contributed its risk register (§1.4), its effort table (Part 3),
its ops notes (§P) and its Step 4, which became the federal-validation
procedure. Both predecessors are in `research/archive/`
(`nonfiler_state_weights_todo_2026-08-19_pre-merge.md`,
`nonfiler_unified_plan_2026-08-18_imported.md`). Revision history at the end.

---

## Part 1 — Review

### 1.1 The plan is sound; the sequencing decision is the right one

The core architecture holds up. The residual anchor is validated two ways
(Pub 1304 T1.6 vs the HT2 identities agree to ±0.5%, and the 46.5M residual
triangulates against Pub 5785), the six decisions D1–D6 are evidence-backed,
and the 2026-08-16 sequencing call — non-filer rework **before** the weights
swap-in — is correct and worth defending: the alternative fits the production
weights on margins that Stage D has already shown run 0.78×–1.51× of the
anchor.

Two structural strengths worth preserving as the work proceeds. **§5.4's
V1/V2/V3 vintage split** turns two of the three A/Bs into exact-equality tests,
which is the cheapest possible way to keep a national-file rework honest. And
**§3.2.2's observation that Mok's regressors are all CPS-native** is the load-
bearing insight of the whole filing-model section — the linkage bought the
identification, and scoring needs only survey variables.

### 1.2 Six findings from the SSA work today that change the plan

The SSA inputs are now in place and documented (`SSA-{OASDI,EEDATA}-SC/NOTES.md`).
Reading the publications properly turned up six things the memo does not know,
three of which contradict it.

1. **Use the 51-jurisdiction sum, not `All areas`.** `All areas` includes
   beneficiaries residing abroad and in the territories, and overstates the
   US-resident 65+ margin by **2.5–2.6%**: the anchor values are **44,635,968
   (2017)** and **50,766,317 (2022)**, not 45,808,776 / 52,052,807. The
   verification script deliberately checks `All areas` — that is a file-identity
   check, not the anchor.

2. **⚠ CONTRADICTION — OASDI cannot support the `65_74` / `75p` split that
   §6.2 specifies.** §6.2 calls for "OASDI beneficiary counts as targets on the
   65-74/75+ bands." OASDI-SC publishes the 65+ cut **by sex only**; there is no
   finer age detail in the publication at all. Either the 65+ bands collapse to
   one, or the split comes from somewhere else. This has to be settled in
   pre-flight, because D2's "state × 6 age bands" rests on it.

3. **⚠ CONTRADICTION — EEDATA is a 1% sample, so it cannot be a hard target.**
   The source is the Continuous Work History Sample, 1-in-100. §6.2 proposes the
   covered-worker margin "as a `has_wages` count target on the non-filer
   partition." A state with 300k covered workers rests on ~3,000 sampled
   records. It should enter as a **soft target or a prior**, never as a hard
   constraint, and small states need an explicit tolerance. Note the asymmetry:
   OASDI-SC is **100 percent data**, EEDATA is not — the two SSA margins are not
   of equal authority and the design currently treats them as if they were.

4. **⚠ CORRECTION — the QCEW dollar cross-check must use EEDATA Table 4 (HI),
   not Table 1 (OASDI).** Table 1's earnings are capped at the taxable maximum
   ($127,200 in 2017; $147,000 in 2022) and run **~17% below QCEW in both
   years**; Table 4 is uncapped and agrees with QCEW to ~1% (1.007× in 2017,
   1.013× in 2022). Reading the OASDI gap as a data problem would be a wasted
   investigation. There are in fact **two covered-worker universes** — OASDI
   (Tables 1/2) and HI/Medicare (Tables 4/5), differing by ~4.1M persons — and
   which one is *the* margin is an unmade decision.

5. **UPGRADE — EEDATA Tables 2/5 carry state × age directly.** The memo assumed
   the working-age layer would have to be inferred from a national
   persons-vs-returns ratio ("~75% ± 9pp"). It does not: covered workers are
   published by state × age band (Under 20, 20–29, …, 60–61, 62–64, 65–69, 70+).
   This is strictly better than what D6 was designed around, and it partly
   compensates for finding 2. The bands are cut around Social Security
   eligibility and do not nest inside `age_band()` without a decision.

6. **Coverage is wider than the anchors need, in one direction only.** OASDI
   runs 2017–2025 (plus a 1999–2025 flat series); **EEDATA stops at data year
   2023**. Back-year weights (design memo §8: 2014, 2016–2019) are fully
   supported. Forward extension past 2023 has **no covered-worker margin at
   all** — a hard bound on the method, not a publication lag to wait out.

### 1.3 Four places the memos have drifted from the tree

- **The CPS ASEC family already exists.** §4.1 says to "check `{production}/raw_data`
  for a registered CPS/ASEC family; if absent, add one." It is not absent:
  `raw_data/CPS-ASEC/` holds **2.4 GB** of IPUMS CPS extracts across 7 vintages
  (latest 2025-06-19, data years ~2010–2024), plus `CPS-ASEC-Panel/` (303 MB).
  Three consequences: the proposed path `raw_data/CPS/cps_common` is **wrong** —
  the family is `CPS-ASEC` and follows a `v1/{vintage}/historical/` convention,
  not `acs_common`'s; the extract carries **`FILESTAT`, `DEPSTAT`, `ADJGINC`,
  `FAMUNIT`, `HIMCAIDLY`, `FOODSTAMP`** — i.e. IPUMS's own tax-filer recode and
  one of Mok's Medicaid covariates, which is directly material to research pass
  A; but the extract is only **21 variables** and **lacks most of Mok's
  covariate set** (income-component presence indicators, `EDUC`, `RACE`,
  `HISPAN`, `INCWELFR`, `INCSUPP`). So the work is a **variable extension to an
  existing shared request**, not a new family — which is exactly what
  `research/state_tax/notes/state_data_imputation_plan.md`'s new note prescribes.

- **The Tax-Data vintage question — RESOLVED 2026-08-19.** The memo cited
  `2026070814`; `interface_versions.yaml` pinned `2026030513` (what Stage D
  actually read). Checked directly: **`2026070814` is the newest *complete*
  vintage** — the four August vintages (`2026081212`–`2026081216`) contain only
  `factor_ledger.rds` / `weight_ledger.rds`, **no `tax_units_*` files**, and
  `2026080610` is empty; they are partial runs. The pin is now advanced to
  `2026070814`. The schema change is **purely additive** (52 columns: wealth
  `accruals.*`/`basis.*`, consumption `c_*`, `forbes_*`), and the non-filer
  object is materially unchanged — TY2022 non-filer units 27.63M → 27.62M,
  adults 32.38M → 32.36M, TY2017 age bands moving ≤1.2pp with **F2's inversion
  intact on both**. **F1–F4 stand as computed** and do not need re-running.

- **Two housekeeping items are already done.** `ht2_path()` is already repointed
  to `IRS-Ind` (`state_weights.R:65`), and the stale `SCHOOL` comment is already
  corrected (`:169` now says SCHOOL is in the common extract). §7.4 and §6.1
  still list both as to-do.

- **Line references have drifted.** The identities are at `state_weights.R:339`
  (`ht2_filing_persons()`), not `:349-352`. Verified still true: the non-filer
  target x-vector is `rep(1, ...)` (`:745`), `become_filer_ctc` requires
  `qual_ei == 0` exactly (`ctc.R:232`), and there is no `become_filer_eitc`.

### 1.4 Three risks the plan under-weights

1. **The anchors are close to unfalsifiable.** §5.2 calibrates Tax-Data
   non-filer weights *to* the Stage-D anchors, and §6.2 targets the state
   weights to the *same* anchors. If the anchor level is wrong, every downstream
   diagnostic still passes. §5.4 names the one genuine external check — the 2021
   baseline pays EIP3 (~$402–411B) and advance CTC (~$93B) to non-filers against
   published actuals — and it currently sits as one bullet among six. **It is the
   only place the non-filer level meets an observed dollar amount, and it should
   be run early**, on V2, not as a late confirmation.

2. **The critical path is long and almost entirely serial** — pre-flight →
   research pass A → filing model → Tax-Data rework → federal validation → state
   rework → swap-in, at the memo's own estimates roughly **7–9 weeks**. Only two
   things genuinely parallelize (the GQ fix, which D4 makes decision-independent,
   and the SSA readers). Everything else waits on the Tax-Data age fix, which
   §6.2 correctly calls a hard prerequisite. Worth stating the date honestly
   rather than discovering it.

3. **Two scope decisions are floating.** The **dependents/MFS** question (§3.2.5)
   is "decide explicitly" with no assigned point in the sequence — but it gates
   both ASEC unit construction and the Tax-Data rework, so it has to be decided
   *before* research pass A finishes, not after. And the **EITC become-filer
   gap** (§8) is recorded but unscheduled, even though it means the rework's
   headline population — earnings-bearing non-filers — remains unscoreable for
   any EITC reform. Both need an owner and a slot.

#### The full register

Carried in from the 2026-08-18 plan. The three above are the ones this review
judged under-weighted; these are the standing risks, each with its mitigation.

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


---

## Part 1.5 — Decisions taken 2026-08-19, and what was implemented

Five decisions closed pre-flight in one session. Recorded here because each
changed something in the code or the sourcing, not just the plan.

| # | Decision | By | Implemented as |
|---|---|---|---|
| **Dependents & MFS** | **both IN SCOPE** | JI | design memo §3.2.5, §5.1; gates the ASEC unit builder (A2/C2) |
| **Covered-worker universe** | **HI (Medicare)**, Tables 4/5 | JI | design memo §3.1 step 5, §6.2; `SSA-EEDATA-SC/NOTES.md`; tag `covered_worker_hi` |
| **Tax-Data vintage** | **`2026070814`** | verified | `interface_versions.yaml`; F1–F4 confirmed to stand |
| **Age bands** | **7 cells / 6 targets** | computed | `src/data/state_weights.R`: `age_band()`, `target_age_band()`, `a16_band()` |
| **Anchor tolerance** | **state-varying, computed** | computed | `08_residual_tolerance.R` → `results/residual_tolerance_{year}.csv` |

**Code changes.** `src/data/state_weights.R` gains `AGE_BANDS` /
`TARGET_AGE_BANDS`, a re-cut `age_band()` (bottom break moved 25 → 26), a new
`target_age_band()`, and `a16_band()` promoted from the two anchor scripts so
there is one definition rather than three. `01_fetch_residual_inputs.R` picked up
three fixes during the SSA work (the `retrieved`-date coercion bug, year parsing
for manually-placed files, `NOTES.md` exclusion). New:
`08_residual_tolerance.R`. All parse clean; band boundaries unit-checked at
17/18/25/26/34/35/64/65/74/75/90.

**Two sources found that the plan did not know about**, both already in the
shared store and needing readers only:

- **SOI IRA study Table 4, column (1)** — Form 1040 filers by five-year age band
  to 80+, TY2000–2023. Supplies the `65_74`/`75p` split that Pub 1304 Table 1.6
  cannot, as a *share* (its level runs 4.5–5.7% low on a different MFJ age
  convention). See P1.
- **SOI Form W-2 study Tables 1.A–3.A** (TY2019–2020 only) — 1.A is taxpayers
  with wage income by age on a **box-1** concept; 2.A gives **elective retirement
  contributions of $379.2B (TY2020)**, a population total and a component of the
  box-1/box-5 wedge. **Trap recorded:** 2.A's universe is only the 62.6M
  taxpayers *with* contributions (42.8% of the 146.2M with wage income, confirmed
  against 3.A's total row), so its `Medicare wages` column is **not** an
  aggregate.

**That gap is now CLOSED (JI pointed to the source, 2026-08-19).** Table 5.A ships
inside the SOI Form W-2 study's **all-tables workbook**, `{yy}inallw2.xls` — a
different naming lineage from the per-table `{yy}in{NN}w2all.xlsx` files the
downloader already knows about, which is why it was missed. **TY2014, 2016, 2017
and 2018 exist; 2015 and everything after 2018 do not** (probed). Placed as
`IRS-Ind/national/w2/w2_all_{year}.xls` and registered in the manifest. Each
workbook carries **51 sheets** against the 4 per-table files currently held, so
this is a substantially richer source than the store reflected — **the IRS-Ind
downloader should learn the `{yy}inallw2.xls` lineage**, and its `NOTES.md` should
gain a W-2 family section recording the return-based universe.

## Part 2 — Implementation to-do

Ordered by dependency. **P** = pre-flight, **A**–**G** = the sequence.
Effort estimates follow the memo's own where it gives them.

**Ops constraints that apply to every task below.**

- All fits and the ACS/ASEC tabulations run under `sbatch`. The login node
  OOM-kills at ~7–8 GB and **piping masks the kill** — a pipeline's exit status is
  `tail`'s, so a killed job looks like a clean one. Existing pattern:
  `research/state_weights/nonfiler_residual/run_acs_tabulation.sbatch`
  (`--mem=48G`, `--time=02:00:00`, `module load R/4.4.2-gfbf-2024a`); scratch at
  `/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`.
- `src/data/state_weights.R:121` uses `fread(cmd = 'zcat ...')` — POSIX only.
  Anything touching HT2 is cluster-only, not Windows.
- On the HPC, R is not on `PATH` by default: load the module in the **same** shell
  command that calls `Rscript`.

### P — Pre-flight (~2 days, login node, blocks everything)

- [x] **P1. Age-band set — DECIDED and IMPLEMENTED 2026-08-19.** Two band
      spaces, because the cells and the targets have different sourceable
      resolutions, and `src/data/state_weights.R` now carries both:
      - **`age_band()` — the CELL space, 7 bands:**
        `18_25 / 26_34 / 35_44 / 45_54 / 55_64 / 65_74 / 75p`.
      - **`target_age_band()` — the TARGET space, 6 bands:** the same, with
        `65_74`+`75p` collapsed to `65p`.
      - **`a16_band()`** (person age, adults) moved here too, so the anchors and
        the fit share one definition and cannot drift apart again; the duplicate
        copies in `02_`/`03_` are deleted.

      Why, with the numbers:
      - **Pub 1304 Table 1.6 sets the resolution.** The residual is
        PEP adults − filing adults; PEP has single-year age, but T1.6 is the only
        published source of **filing adults by age** and its top band is
        `65 and over` (verified in the file). So the residual can be no finer
        than T1.6 — except where another source supplies the split.
      - **The 25/26 boundary was a live silent bug, now fixed.** `age_band()` cut
        at 25 and `a16_band()` at 26, so **4.33M people aged exactly 25** sat in
        one cell on the target side and another on the record side — more than
        the entire residual of all but the largest states.
      - **65+ IS splittable nationally — new source found 2026-08-19.**
        **SOI IRA study Table 4, column (1)** publishes *"Number of taxpayers who
        filed Form 1040"* by five-year age band to 80+, **TY2000–2023** (no 2003),
        covering both anchor years and every back year. `65 under 70`+`70 under
        75` and `75 under 80`+`80 and over` aggregate exactly to `65_74`/`75p`.
        Use it for the **share, not the level**: its 65+ total runs 4.5–5.7% below
        T1.6's, consistently across years, almost certainly because it assigns
        each taxpayer their **own** age where T1.6 assigns joint filers the
        **primary's**. Applying its share to T1.6's level gives, TY2022:

        | band | PEP pop | filing adults | residual | non-filing rate |
        |---|---|---|---|---|
        | `65_74` | 34.00M | 27.92M | 6.09M | **17.9%** |
        | `75p` | 23.50M | 17.95M | 5.55M | **23.6%** |

        The population splits 59/41 but the **residual splits 52/48** — the
        non-filing rate rises 5.7pp above 75. Collapsing to `65p` everywhere would
        hide that gradient, which is why the cell space keeps the split.
      - **But no STATE source can split 65+.** SSA OASDI-SC publishes 65+ by sex
        only. So the split lives in the national age shape (which is what the
        Tax-Data age draw, D1, actually needs — F2's headline fix) and in the ACS
        prior, never in a state target.
      - **EEDATA (HI) maps onto the target space at exactly one point** —
        `65-69`+`70+` = `65+`. Its other bands (`<20/20-29/.../60-61/62-64`) align
        with nothing, so it enters as a **coarse 20-64 working-age margin per
        state**, not band-by-band.
      - **Both systems already share the MFJ approximation** (T1.6's own footnote:
        *"Age for joint returns was based on the primary taxpayer's age"*), so no
        reconciliation is needed there — but note it is exactly the concept the
        IRA-T4 share does *not* share, which is why that transfer is a share and
        carries a widened tolerance.
      - **Also checked and ruled out:** none of the other thirteen Pub 1304
        by-size tables carries an age dimension, and **SOI Form W-2 study Table
        1.A** has ideal bands (incl. `65 under 75`/`75 and over`) on a box-1
        concept but exists only for **TY2019–2020**, so it cannot serve the anchor
        years.
- [x] **P2. Resolve the Tax-Data vintage.** *Done 2026-08-19* — pin advanced to
      `2026070814` (the newest complete vintage; the August ones are ledger-only
      partial runs), schema verified purely additive, non-filer mass and age
      composition verified materially unchanged, **F1–F4 stand as computed**.
      Residual: `03_diagnose_current_nonfilers.R` should still gain a
      `--vintage` argument so this stays checkable rather than re-derived.
- [x] **P3. Dependents and MFS — DECIDED IN SCOPE (JI, 2026-08-19).** Both are
      modelled. Consequences now carried into the plan: the ASEC unit builder
      must form and retain dependent and MFS units **from the start** (A2/C2);
      **Mok's Panel E becomes load-bearing**, which makes its reversed column
      order (65+ first) a correctness risk rather than a transcription nicety
      (A4); **MFS has no borrowed coefficients** in either Mok or Cilke, so it
      enters through the *threshold* rule (the $5 floor from TY2018) and not a
      probit — document that asymmetry rather than implying MFS is modelled to
      the same standard; and `impute_nonfilers.R`'s `dep_status == 0` assertion
      (D1) becomes a **reconciliation question**, not an invariant, since the two
      universes now disagree about whether a non-filer can be a dependent.
- [x] **P4. Covered-worker universe — DECIDED: HI (Medicare) (JI, 2026-08-19).**
      **Table 4** for persons and dollars, **Table 5** for the state x age
      detail. HI is the closer analogue to the W-2 universe (it picks up the
      state and local government employment outside OASDI coverage, ~4.1M
      persons) and on the dollar side it is decisive: uncapped, matching QCEW to
      ~1%, where OASDI's capped earnings sit ~17% low. Tag every margin file
      with the universe (`covered_worker_hi`) so the choice travels with the
      data — design memo §7.3 requires the tag and the two universes are easy to
      confuse downstream.
- [x] **P5. Anchor tolerance — COMPUTED 2026-08-19, state-varying.** The
      structural fact: the residual is a **small difference of two large
      numbers** (18.2% of adults), so input errors amplify by
      `input / residual` — **4.6x nationally, 3.6x (MS) to 9.4x (SD) by state**.
      Demonstrated rather than asserted: the two SOI routes to filing adults
      disagree by **-0.47%**, PEP agrees exactly between the constructions, and
      the resulting national residual gap is **2.14% — exactly -0.47% x 4.6**.
      Feeding the two measured components (+/-0.5% filing-adults construction
      from the T1.6-vs-HT2 disagreement; +/-0.5% PEP vintage from the
      SSA-published vs our-vintage population spread) through the amplification,
      in quadrature:
      - **National: +/-3.5%** — +/-1.68M on 2022's 46.5M residual, +/-1.63M on
        2017's 47.3M.
      - **State: +/-2.2% (MS) to +/-6.3% (SD)** in 2022, +/-2.2% (DC) to
        +/-6.7% (SD) in 2017 — a flat tolerance is wrong by
        ~2.6x across states, and it is **inverted from intuition**: the states
        with the *smallest* non-filer share need the *widest* tolerance. Those
        states (SD, MN, NH, UT, NE, WY) are also where EEDATA's 1% sampling error
        bites hardest, so they are doubly weak — and SD is F5's worst v0 offender
        at 1.51x. Small-state residuals are the least trustworthy cells in the
        system, not the easiest.
      - **Deliberately excluded:** the adult-dependent netting (~5.5M lower
        bound, 12% of the residual) is a **bias to remove**, not a tolerance —
        only its estimation error belongs here. EEDATA sampling error touches the
        covered-worker margin only; the ~17% ASEC understatement touches the
        filing model, not the anchor.
      - **CLOSED 2026-08-19 (with A1).** `residual_tolerance_{year}.csv` is
        emitted per year and the fit reads it rather than re-deriving it. The
        script had been reading `T5_state_margins.csv` — the v0-vs-anchor
        diagnostic, which exists for 2022 only — through a **silent fallback**,
        so the 2017 file carried 2022's tolerances under a 2017 name. It now
        reads `residual_anchors_{year}.csv` and **stops** if the year is
        missing, which is what surfaced the substitution in the first place.

### A — Close out the SSA inputs and research pass A (~1 week + 3–5 days, parallel)

- [x] **A0. SSA acquisition, verification, registration and documentation.**
      *Done 2026-08-19* — record in `research/archive/07_ssa_inputs_plan_2026-08-19_executed.md`.
      Workbooks verified
      (4/4 cells PASS), flat series adopted as source of record (exact against
      the workbooks: 59 areas × 11 measures × 2 years), both families registered,
      `NOTES.md` written and placed, Chrome confirmed absent from the cluster,
      three bugs fixed in `01_fetch_residual_inputs.R`.
- [x] **A1. The two SSA readers — DONE 2026-08-19.**
      `read_ssa_oasdi_65p()` and `read_ssa_eedata_hi()` in
      `src/data/state_weights.R`; the `ssa_covered_persons = NA_real_` stub is
      gone. Every requirement is **encoded as an assertion, not a comment**: the
      51-jurisdiction sum (never `All areas`); the flat series checked against
      the same year's workbook (51 states x the 65+ measures, exact, both years);
      a named error for the missing **2010** and normalization of the pre-2007
      `Virgin Islands` label; EEDATA **Table 4 (HI)** `Number → Wage and salary`
      for the level and **Table 5** for state x age; published column layouts
      asserted **by name**, so an edition that moves a column fails loudly
      instead of silently misreading; universe tags `covered_worker_hi` /
      `beneficiary` stamped on every output.
      `02_build_residual_anchors.R` re-run clean on both anchor years: the wage
      margin gains covered persons and dollars, the state anchors gain the 65+
      beneficiary count and its PEP coverage ratio, and
      `ssa_age_margin_{year}.csv` is new.
      **Two findings** (`04_findings.md` F8-F9): the returns-per-covered-person
      wedge lands at **0.760/0.743**, confirming the memo's ~75% ±9pp, and HT2
      wages are 93.5%/90.6% of SSA HI wages against 94.1%/91.7% of QCEW — the
      two frames agree to ~1pp. And **Table 5's age detail is on the TOTAL
      covered universe, not wage-and-salary** (its all-ages column is exactly
      Table 4's `Number Total`), a universe switch between the two sheets that
      the design had not noted; memo §5 amended.
- [x] **A2. Research pass A — ASEC tax-unit and income construction — DONE
      2026-08-19.** Deliverable: **`research/state_weights/nonfiler_residual/10_asec_tax_unit_design.md`**,
      with every number reproducible from
      **`research/state_weights/nonfiler_residual/09_asec_tax_unit_diagnostics.R`** (tables `asec_A1..A6`).
      **Decision: build our own units on Mok's conventions; do not build on the
      Census recodes.** `FILESTAT`'s filer *count* is calibrated to
      administrative totals (O'Hara's $2,000 floor exists for that purpose), so
      adopting it would make the C6 gate vacuous — and its *mix* is wrong
      anyway: head-of-household is **41.5% short** in TY2022 and MFS does not
      exist. `ADJGINC`/`TAXINC` are statistically matched to the **SOI PUF**, the
      same file we are correcting, so they are circular. `DEPSTAT` has **10.8%**
      of dependents pointing at someone the same model codes as a non-filer.
      Seven decisions **D-A1–D-A7**, including **MFS as a calibrated post-step**
      so Mok's coefficients stay valid, and dependents constructed/retained/tagged
      per P3.
      **Four findings worth carrying:** (i) `FILESTAT` is **broken in TY2020–21**
      (non-filing adults 42.9M → 11.7M → 11.5M → 44.0M) and `DEPSTAT` in
      **TY2014**; (ii) our extract **silently loses pension and annuity income
      from TY2018** — the ASEC 2019 redesign moved them to
      `INCPEN1`/`INCPEN2`/`INCRANN`, which we do not pull, and our two anchor
      years straddle the break; (iii) the ASEC wage aggregate is **sound**
      (1.011× SSA HI covered wages in TY2022) while dividends are at 0.449 of
      SOI; (iv) the ASEC–anchor non-filer gap **is** the group-quarters universe
      difference — the identity closes to 0.03M in both anchor years, which the
      C6 gate must not read as transfer failure.
- [x] **A2b. Extract additions from A2 §5 — DONE 2026-08-23.** Re-pulled
      **11 of 11 samples**; the twelve new names are present in every year's DDI
      per the availability matrix below, and the TY2018+ pension variables carry
      real data (`INCPEN1` 7,532 persons at a \$16,800 median in TY2022, once the
      IPUMS NIU sentinels 9999999 / 99999999 are screened — a first check counted
      `> 0` and reported all 146,133 records as non-empty, which was the
      sentinel, not data). Twelve variables added to
      `config/parameters.cps.yaml` (84 total): `INCPEN1` `INCPEN2` `INCRANN`
      `SRCPEN1` `SRCPEN2` `INCRET1` `INCRET2` `LINENO` `FAMREL` `FTYPE` `FAMID`
      `INCCAPG`.
      **Validation found three of A2 §5's names do not exist in IPUMS CPS.**
      `get_metadata()` in ipumsr 0.10.0 covers NHGIS datasets, not microdata
      variable availability, so the check was done the way the API itself does
      it — submit a probe extract and read the rejection. Results: `CAPGAIN`/
      `CAPLOSS` are the **wrong names** and the real variable is **`INCCAPG`**;
      and **`INCALIM` is genuinely unavailable** — neither it nor `INCALOTH`
      exists in any of the eleven samples, so alimony has no separate IPUMS CPS
      variable for these years and sits inside `INCASIST`/`INCOTHER`. That is a
      documented limitation against Mok's AGI list, not an oversight.
      **⚠ CORRECTION to how that probe should be read.** A pooled probe over all
      eleven samples does NOT establish per-sample availability. IPUMS rejects
      only a variable available in **none** of the selected samples — its own
      wording, *"not available in any of the samples currently selected"* — so
      the probe proved existence in **at least one** year, which is a weaker
      claim than this task asked for ("validate every name across all eleven
      samples"). The per-sample truth comes from the pull itself, which submits
      one sample at a time and logs what it drops: for ASEC 2016 it dropped
      `INCPEN1` `INCPEN2` `INCRANN` `SRCPEN1` `SRCPEN2` `INCRET1` `INCRET2`
      `INCCAPG` — **eight of the twelve additions.**
      **This is expected, not a failure.** The pension variables are the
      *post-redesign* ones: the ASEC 2019 restructure is what created them, so
      they exist from TY2018 on and cannot exist before, which is exactly why A2
      wanted them ("restores pension and annuity income **from TY2018 on**").
      The pre-TY2018 years keep `INCRETIR`, already pulled. The consequence to
      carry forward is that **the retirement-income covariate is built from
      different variables either side of TY2018**, so C5 must construct it as a
      spliced series and check continuity across the break rather than assume
      one definition spans the window.

      **⚠ C5 INPUT — the splice is NOT a sum, and the measure is unresolved.**
      Measured on the new extract 2026-08-23 (weighted by ASECWT, \$bn):

      | measure | TY2017 | TY2018 | TY2022 |
      |---|---|---|---|
      | `INCRETIR` alone | **570.6** | 178.7 | 221.5 |
      | `INCPEN1+INCPEN2+INCRANN` | absent | 453.8 | 505.7 |
      | naive sum of all | — | 632.5 | **727.1** |
      | union, per-person `max` | — | — | 677.7 |

      `INCRETIR` **falls 69%** across the redesign (570.6 → 178.7): that is the
      break. The tempting reading — splice by summing `INCRETIR` and the
      components from TY2018 — is **wrong**, because they **overlap at the
      person level**. In TY2022, **1,985 persons hold both**, carrying \$94.3bn
      of `INCRETIR` and \$128.9bn of components between them, so a naive sum
      double-counts. That is also why the summed series looked reassuringly
      continuous with TY2017 — the continuity was partly the double-count.

      The three candidate TY2022 measures — 221.5 (INCRETIR only), 505.7
      (components only), 727.1 (naive sum) — **straddle** TY2017's 570.6, so the
      data cannot settle which is right. **C5 must read the IPUMS CPS definition
      of post-redesign `INCRETIR`** (harmonized aggregate, or residual "other
      retirement income"?) before choosing, since that determines whether the
      correct TY2018+ measure is the components alone, the components plus a
      genuinely-residual `INCRETIR`, or a per-person union. Do not pick by
      whichever number looks smoothest against TY2017.
      **Why the probe was not optional:** `download_ipums.R` prunes unavailable
      variables from the API error and continues. For an optional variable that
      is correct; for `INCPEN1` and `LINENO` it would have produced an extract
      that looked complete and could not do the job.
      *A first validation attempt reported all fourteen variables as
      non-existent. That was a bad `get_metadata()` call, caught by testing a
      known-good variable (`INCWAGE`) as a control — which is why the control
      was run.*
      **⚠ The re-pull needs `--overwrite`, and without it it is a silent
      no-op.** The first pull exited **0**, printed `SBATCH_DONE_OK`, and had
      done nothing: `download_ipums.R` skips any sample whose folder already
      contains a DDI, so all eleven were skipped and the log's real verdict was
      the last line, **`Pulled 0 of 11 samples`**. Every new variable would have
      been absent from a store that looked freshly built. This is the
      cluster-notes rule exactly — a completed job is not a successful one, and
      the exit code is not the check. The store was backed up to
      `/nfs/roberts/scratch/pi_nrs36/ji252/cps_asec_common_backup_20260823`
      (86 MB) before the overwrite, since `--overwrite` replaces eleven years of
      a shared extract.
      **Acceptance for this task is not "the job finished":** it is `Pulled 11
      of 11 samples`, the twelve new names present in every year's DDI, and the
      TY2018+ pension variables non-empty — which is the whole point of the
      re-pull.
      Original text of this task, for reference:
      `INCPEN1`/`INCPEN2`/`INCRANN` (+`SRCPEN1/2`, `INCRET1/2`) — **blocking for
      C5**; `LINENO` — **blocking for C2**, and it accounts for 100% of today's
      unresolved `DEPSTAT` pointers; `CAPGAIN`/`CAPLOSS`; `FAMREL`/`FTYPE`/`FAMID`
      (IPUMS CPS has **no** `SUBFAM`/`SFTYPE`/`SFRELATE` — those are IPUMS USA,
      which is itself a transfer risk since the ACS side has them natively);
      `INCALIM`. **Validate every name against the IPUMS API across all eleven
      samples before pulling**, per A3's standard.
- [x] **A3. Shared CPS-ASEC extract — DONE 2026-08-19.** Pulled through
      `common_ipums_download` (`config/parameters.cps.yaml`, committed) into
      **`raw_data/CPS-ASEC/cps_asec_common/`**: ASEC **2015–2025**, i.e. income
      years **2014–2024**, covering both anchor years and every back year. 72
      variables, all validated against the IPUMS API before pulling; 92 returned
      and common to all 11 years; no case selection, so group quarters are
      retained. **All 16 Mok covariates are present** (verified against the DDI).
      Required one fix to the shared downloader, since it parsed only one of
      IPUMS's two data-quality-flag error phrasings and hard-failed on CPS.
      Add variables to this request rather than forking an extract.
- [x] **A4. Mok (2017) Table 14 — DONE 2026-08-19.**
      `research/state_weights/nonfiler_residual/resources/mok_coefs.csv`,
      **14 groups x 17 terms = 238 rows** with SEs, significance stars, group Ns
      and weighted filing rates. Transcribed from rendered page images as
      instructed. Both documented Panel E traps confirmed (columns run **65+
      first**, Ns 909 vs 62,438; `.` for self-employment in the 65+ column) —
      plus **a third this plan did not have: Panel E has no "Retirement income"
      row at all**, so its equations carry 15 covariates where every other panel
      carries 16. Placeholder cells are stored blank with a `note`, never as zero.
- [x] **A5. Cilke (1998) Table 3 — DONE 2026-08-19** (not skipped).
      `research/state_weights/nonfiler_residual/resources/cilke_coefs.csv`,
      **9 groups x 24 terms = 216 rows**, plus the WP-78 PDF committed as
      `research/state_weights/nonfiler_residual/resources/cilke1998_ota_wp78.pdf`
      (treasury.gov serves it to automated retrieval, unlike cbo.gov). **14 cells
      are published as `0.0000/0.0000` = not estimated** for that group; stored
      blank, and the count is asserted so a later edit cannot turn one into a real
      zero. Each group's equation spans two PDF pages, so the CSV carries
      `pdf_page` per row.
- [x] **A4/A5 verification.**
      `research/state_weights/nonfiler_residual/11_verify_coef_transcriptions.py`
      recomputes each PDF page's multiset of numeric tokens and checks every value
      the CSVs claim from that page — **454 values across 7 pages, all present**.
      It catches typos and dropped digits, not a same-page cell swap; that is what
      reading the images is for.
      **⚠ The two tables have OPPOSITE dependent variables: Mok predicts
      P(files), Cilke predicts P(does not file).** Mixing the conventions inverts
      a model while producing plausible probabilities. Checkable anchor: *no
      earned income* is positive in all nine Cilke columns (0.5069 … 0.9598),
      against Mok's positive wage-presence terms (0.552 … 0.9).
      **Open before scoring Mok:** her footnote calls the omitted group
      "non-Hispanic white with more than a college education", which sits oddly
      with a two-dummy education scheme whose natural omitted group is
      high-school-to-some-college. It decides which population the intercept
      describes.
- [x] **A6. Re-run 01 → 02 → 03 — DONE 2026-08-23.** The anchors came back
      **byte-identical** on both years, so A1's readers were already reflected
      and the pipeline reproduces from a clean run: wedge 0.760/0.743, OASDI 65+
      44.6M/50.8M (87.5%/88.3% of PEP), residual 47.3M/46.5M. Only T1–T4 moved,
      because they read production Tax-Data and the vintage pin advanced in P2 —
      small changes, and **the age inversion is undisturbed**: Tax-Data puts
      41.4%/43.4% of non-filing adults at 65+ against a residual-implied
      25.1%/25.0%, and 9.96%/9.46% at 18–25 against 24.2%/22.2%.
      **The substantive part was that the SSA margins existed in the anchor files
      but never reached T5** — `03`'s assembly selected four columns from the
      anchors and dropped the rest. T5 now joins `beneficiaries_65p`, `pep_65p`,
      `ssa_65p_coverage`, `ssa_covered_persons`, `ssa_covered_wages` and
      `ht2_returns_per_ssa_person`. **D6 is resolved** (appended, not rewritten —
      `04_findings.md` is a frozen evidence document); new finding **F10** records
      the re-run.
      One trap worth carrying: the first T5 summary divided *all* HT2 returns by
      covered persons and printed 0.928 next to a state range of 0.638–0.783 —
      **outside the range of its own components**, which is impossible for a
      weighted mean and is the tell that two different quantities were being
      compared (the state column's numerator is returns *with wages*). Fixed, and
      the message now names the numerator.

### B — GQ treatment (~2–3 days, ships independently)

- [ ] **B1. Differentiated GQ in `build_acs_margins()`** (D4, sized by F6:
      8.15M GQ persons = 16.8% of the national residual but **42% in SD**, 34%
      AK, 33% VT). Keep institutional residents (`GQ == 3`) as own-state
      non-filer units unless income makes them filers; reclassify college-age
      dorm residents (`GQ == 4`, in school, age < 24) as dependents rather than
      unit heads; leave military barracks to the income test; report GQ weight by
      type and state. Add `GQ`/`SCHOOL` to `read_acs_extract()`'s default `cols`.
      *Decision-independent — do not hold this behind anything else.*

### C — Filing model on the ASEC, transferred to the ACS (~1–2 weeks)

- [ ] **C1. v1a deterministic upgrades to `filing_threshold()`** (§3.2.1): the
      age-65 additional standard deduction, the $400 SE rule via `INCBUS00`,
      dependents with own income above the dependent filing floor forming filing
      units, `SCHOOL` keeping 19–24 household students dependent.
- [ ] **C2. Build ASEC tax units and income concepts** per A2's design note.
- [ ] **C3. Score the below-threshold probits and the above-threshold hazard**
      (Pub 5785, ~11.19M above-threshold units TY2014-16, national scalar for
      v1 per D3).
- [ ] **C4. Calibrate jointly, not sequentially** — group constants and the
      above-threshold scalar chosen **together** against the population, filer-
      count and SSA margins.
- [ ] **C5. Check Mok's covariates survive the transfer** before fitting. Any
      regressor not reproducible on the ACS is one the transfer silently drops.
      Map the means-tested-transfer and Medicaid terms to
      `INCWELFR`/`INCSUPP`/`FOODSTMP`/`HINSCAID`, education and race to
      `EDUC`/`RACE`/`HISPAN`.
- [ ] **C6. GATE — report ASEC and ACS implied national filing rates side by
      side.** If the two surveys imply materially different rates, the transfer
      needs rethinking **before** the state fit rests on it. This is a gate, not
      a footnote (§3.2.3).
- [ ] **C7. Score GQ records under an explicitly stated assumption and report
      them separately** in T7. Mok's CPS frame **excludes** the institutionalized
      and military-barracks populations, and our PUF universe includes them —
      scoring her equations there extrapolates outside her estimation frame.
- [ ] **C8. Acceptance:** v1 ACS filer units vs HT2 `n_returns` by state — the
      −7% national bias and its 20pp state spread collapse to within P5's
      tolerance.

### D — Tax-Data rework, as three vintages (~1–2 weeks + cluster time)

Build **V1 / V2 / V3 as separate vintages** (§5.4) so each change A/Bs
independently, and in this priority order: *age detail > national level + aging
> investment income > the dividends bug (trivial, always).*

- [ ] **D1. V1 — composition fixes at fixed weights** (`impute_nonfilers.R`):
      route `fidiv` to `div_ord`/`div_pref` (the `qual_div` column is silently
      discarded today); replace the flat `runif` age draw (`:92-96`) with the
      anchor age shape (`resources/nonfiler_age_shape.csv`) — **the single
      highest-value fix for the state weights**, since F2 shows the age
      composition is not merely blurred but *inverted* (8.9% at 18–25 vs the
      anchor's 24.2%); repair investment income (inspect `usdina2017.dta` for the
      DINA national-income counterparts first, hot-deck as fallback), disciplined
      by F3's Pub 5785 receipt ceilings (14% interest / 9% dividends / 4% gains);
      set `filer = 0` explicitly with `stopifnot`.
- [ ] **D2. V2 — `src/calibrate_nonfilers.R`**, a post-append rake of non-filer
      weights only to the national residual anchors by age × marital status
      (≤14 cells, closed-form; do **not** touch `reweight.R`'s filer LP). Targets
      from a committed snapshot with a provenance header, so Tax-Data gains no
      HT2/PEP readers and no server paths.
- [ ] **D3. V3 — aging fix in `project_puf.R`** `compute_weights_for_year()`:
      make the non-filer path residual-by-construction. One new factor table
      alongside `population_factors_2020plus`, applied `if_else(filer == 0, …)`.
- [ ] **D4. ⚠ Assert id-order stability across the rework.** `run.R:348-357`
      binds precomputed random numbers **positionally**. If `calibrate_nonfilers.R`
      reorders rows, **every filer record is silently rerandomized**. Assert
      `identical(ids_in, ids_out)` and gate the A/B on ordered id identity.

### E — Federal validation battery (~1 week, mostly diff-reading)

> **The runbook is `research/state_weights/nonfiler_federal_validation.md`** — the
> predicted signature, the pre-flight vintage gate, the V1/V2/V3 staging, the A/B
> runs, the CBO null test, the reform scenarios, the distributional checks, the
> external triangulation and the regression guard, with its 4a table as the
> acceptance gate. That document owns *how*; the four items here own *whether and
> when*, and E4's early-run instruction overrides its ordering.

- [ ] **E1. Exact-equality tests.** V1 must leave `n_tax_units`, `n_returns` and
      payroll totals untouched; V3 must be **bit-identical to V2 in 2017**.
- [ ] **E2. The tripwire.** Every 1040 dollar aggregate is summed as
      `. * weight * filer` (`summary_stats.R:193-195`), so `totals/1040.csv` and
      every line of `supplemental/cbo_comparison.csv` must be **identical** under
      current law in 2017–2019 and 2022+. Any movement falsifies the
      "non-filer only" claim.
- [ ] **E3. Institute a combined-universe wage constraint** (reframed 2026-08-19,
      then **sourced** the same day). The upstream question is **answered: no** —
      Tax-Data's controls are SOI filer aggregates and the LP is solved on the
      filer file before the non-filer append, so §5.2's non-renormalization is
      safe. The exposure is the opposite: **nothing constrains filer + non-filer
      wages at all**. Adopt **SSA HI wage-and-salary (EEDATA Table 4)** as the
      constraint.
      - **The box-1/box-5 wedge is no longer an estimate.** SOI's Form W-2 study
        **Table 5.A** publishes both boxes on one universe:
        **3.51% (2014), 3.83% (2016), 3.63% (2017), 4.20% (2018)** — trending up
        as deferral participation rises. Placed in the store as
        `IRS-Ind/national/w2/w2_all_{year}.xls` and registered in its manifest.
        The wedge is **elective deferrals only**: Section 125 cafeteria
        contributions sit outside box 1 *and* box 5, so they never appear in it.
      - **⚠ This overturns the first reading.** The $871.6B TY2022 PUF-vs-SSA gap
        was provisionally attributed to deferrals plus cafeteria plans, implying
        the PUF total was roughly right. At the measured wedge only ~$400B is
        explained, leaving **~$450B (4.3%) of genuinely missing wage mass** — the
        constraint would **not** be satisfied today.
      - **And it yields a direct measurement of non-filer wages.** The IRS W-2
        study's universe is W-2 income on **filed returns** (return-based tables,
        filing-status footnotes); SSA's is **all** covered workers. Differencing
        them on the same box-5 concept isolates wage earners who never appear on
        a return: **$480.6B / 20.3M persons (2017)** and **$590.3B / 23.6M
        (2018)** — against which the PUF carries **$116.2B (24%)** and **$121.3B
        (21%)**. First dollar-denominated read on F1, and consistent with F2: the
        PUF's non-filers are the wrong *kind*, not merely too few.
      - **Upper bound, not a target.** The residual also contains wage-earning
        **dependents**, outside the IRS taxpayer count but riding filer records in
        the PUF — another reason P3's dependents-in-scope decision matters. The
        ~$25,000 implied average is high for a below-threshold population, which
        says as much.
      - **Coverage binds.** Table 5.A exists for **2014, 2016, 2017, 2018 only**;
        2019/2020 publish Tables 1-4 without it and nothing after 2020 exists
        (probed). TY2017 is covered, **TY2022 is not** — a 2022 constraint rests
        on extrapolating a visibly rising wedge.
      - Predicted movement from the rake: +$22-37B wages, ~+$3.4-5.6B payroll.
- [ ] **E4. ⭐ Run the 2021 refundable-credit check EARLY, on V2.** The
      current-law baseline pays EIP3 and advance CTC to non-filers, so a plain
      2020–2022 baseline run meets published actuals (**EIP3 ≈ $402–411B**,
      **advance CTC ≈ $93B**). Per §1.4 above this is the **only external check on
      the non-filer level** — a file whose 2021 baseline overshoots is over-massed
      whatever the anchors say. Do not leave it to the end.

### F — State-weights margins, targets and re-fit (~2–3 weeks)

- [ ] **F1. Residual anchors as the primary non-filer targets** (§6.2),
      share-normalized like every other target, with the **adult x-vector
      `1 + (filing_status == 2)`** replacing the current `rep(1, ...)`
      (`state_weights.R:745`) per D5.
- [ ] **F2. Demote the income tiers to the prior.** Today the 1,390 count-only
      single-membership cells are reproduced exactly in one pass — the non-filer
      "fit" is pure reproduction of a biased margin (F5: 0.78× DC to 1.51× SD).
      The anchors own the level; the ACS owns the within-state shape. This is
      what turns the partition into a genuine calibration that reaches
      `fit_gradient()`.
- [ ] **F3. Add the SSA margins with the right status** — OASDI 65+ (100% data,
      can bear a firmer target, but **only on a single 65+ band**, per P1) and
      the **HI** covered-worker margin from Table 4/5 (**1% sample → soft target
      or prior only**, per §1.2/3). QCEW stays a diagnostic, never a target.
- [ ] **F4. Split `state_weights.R`** (1,025 lines) into `src/data/ht2.R`
      (reader, `HT2_TARGET_MAP`, stub logic, identities) and
      `src/data/filing_model.R` (thresholds, probits, hazard,
      `build_acs_margins`), leaving engines and assembly in place. This is what
      makes "source the shared functions from Tax-Simulator" workable for
      Affordability-Index without dragging in the split-weight scaffolding.
- [ ] **F5. Validation additions** — the population-identity check (fitted filer
      + non-filer adults vs PEP per state, with tolerance) in
      `research/state_weights/scripts/validate_state_weights.R`; re-run the EITC take-up correlation (the −0.61
      on `n_returns` should attenuate); add state adults by age band vs PEP as a
      new held-out metric.
- [ ] **F6. Re-fit** with config-7 hyperparameters (β=1e-4) unless the sweep says
      otherwise. **Do not promise held-out MARD gains** from the non-filer rework
      alone — pensions (17.0), Schedule C (30.8) and capital gains (61.0 MARD)
      are a *filer*-target poverty problem, addressed by the demographic target
      expansion ranked #1 in `research/state_weights/state_weights_phase1_summary.md` §7, not by this.

### G — Swap-in and close-out (~1–2 weeks)

- [ ] **G1. Phase 1 close-out** per the existing checklist: tune to the
      ≥99%-within-2% bar, structural-core pruning,
      `build_split_weights(method = 'gradient')`, the
      `state_weights_{year}.csv` writer for 2014/2016–2022 (decide the 2013/2015
      HT2 gaps: interpolate or skip), projection-year carry-forward, dispatcher
      flip at `src/sim/run.R:433`.
- [ ] **G2. Vintage-tag the weights files** (`v0-margins` vs `v1-residual`) so
      downstream consumers can tell them apart; schema stays `(id, state, weight)`.
- [ ] **G3. Cross-validation + handoff**: identity diagnostic, held-out battery,
      pilot-state liability re-check, and a pointer from the income memo's
      "Aligning the code" section to this workstream.

### I — National tenure / rent / property-tax imputation (~3–4 weeks)

**Scope note: this extends the workstream past the swap-in.** Groups P–H end at
production state weights. Groups I and J are a second phase, added 2026-08-23,
because the cross-model triage keeps terminating in the same missing variables
and because the federal side needs the same imputation for a different reason.

**Why it is here rather than in `state_tax/`.** It is a *data* task on the same
ACS/ASEC assets groups A–C build, and its calibration cells are the ones the
weights fit already uses. Doing it inside this workstream reuses that
infrastructure; doing it separately would rebuild it.

**What is actually missing today** (measured on the 2019 file, 2026-08-23):

| variable | source | coverage |
|---|---|---|
| `salt_prop` | PUF E18500, a **Schedule A** field | itemizers |
| `first_mort_int` / `second_mort_int` | imputed, labelled "for itemizers" | itemizers |
| rent paid | **does not exist** | nobody |
| tenure (own/rent) | **does not exist** | nobody |

97.2% of itemizers carry a property-tax or mortgage signal against **23.9% of
non-itemizers** — roughly **0.4x** the coverage a ~65% homeownership rate
implies. Renters are wholly unobserved. Note `rent` in Tax-Data is rental and
royalty INCOME (Schedule E), not rent paid; it is an easy variable to misread.

- [ ] **I1. Choose the donor survey: CPS ASEC or ACS.** They are not
      interchangeable here and the choice is not obvious.
      - **ASEC** is what group A/C already pulls (`raw_data/CPS-ASEC/`, 2015–2025,
        72 variables), carries the tax-unit and filing-model machinery this
        workstream is building, and is the base the filing probits are scored on
        — so a tenure/rent imputation off ASEC inherits all of that for free.
      - **ACS** has vastly larger samples (so state x income x age x
        household-size cells are actually populated), carries `TENURE`, gross
        rent and **property-tax amounts** directly, and is already registered at
        `shared/raw_data/ACS/acs_common`.
      - **Correction (2026-08-23):** an earlier draft of this task said property
        tax was the decisive advantage because "ASEC does not collect it". That
        is wrong — API probing during A2b found **`PROPTAX` in IPUMS CPS,
        available in all eleven samples**. Two caveats keep ACS ahead on this
        variable anyway: `PROPTAX` is a Census **tax-model** item, so under S12
        it is verification-only and cannot feed an imputation; and ASEC's sample
        will not populate state x income x age x household-size cells the way
        ACS does. The advantage is real but it is about sample size and S12, not
        about whether the variable exists.
      - Likely answer is **both, at different jobs**: ACS for the tenure/rent/
        property-tax joint distribution and its state detail, ASEC where the
        imputation must line up with the filing model. State the split
        explicitly rather than picking one and living with the gap.
- [ ] **I2. Method: follow the TPC / OTA / CBO precedent, which is
      statistical matching.** None of the reference models impute this inside the
      calculator: TPC, OTA and CBO all constrained-statistically-match the PUF to
      a household survey; PolicyEngine sidesteps it by running on a survey base
      where tenure and rent are native; TAXSIM and Bakija both take rent and
      property tax as *inputs* (Bakija converts rent to a property-tax
      equivalent with a `cbrenteq` rate). So the method is settled by precedent:
      donor imputation / constrained match within cells, not a behavioural model.
      **S12 binds here:** Census tax-model outputs (ASEC `PROPTAX`, `TAXINC`,
      `ADJGINC`, …) may benchmark the result and may not be an input to it.
      Two calibrations worth copying from Bakija's experience: he **smooths**
      step-function circuit-breaker phase-ins, and he declines homestead
      exemptions entirely. That is a sane accuracy ceiling, not a shortcut.
- [ ] **I3. ⭐ Exploit the PRE-TCJA itemizer population — the idea that makes
      this tractable.** Post-TCJA roughly a tenth of filers itemize; pre-TCJA it
      was about three in ten. So the 2017-and-earlier PUF observes Schedule A
      property tax and mortgage interest on a **much larger and far less
      selected** slice of the population than the current file does. Use it to
      estimate the relationship between *observable tax variables* (AGI, filing
      status, dependants, wages, age proxies, state) and (property tax, mortgage
      interest) on that broader base, then apply the fitted relationship forward
      to the post-TCJA non-itemizing population.
      The load-bearing question is **how much of that relationship transfers**.
      Itemizing is still selective pre-TCJA, so the estimates are conditional on
      a selected sample — just much less so. Quantify the residual selection by
      re-fitting on the post-TCJA itemizers alone and comparing coefficients;
      the divergence between the two fits is the honest error bar on the
      transfer. Decide from that whether to use the pre-TCJA fit directly, use
      it only as a prior for the survey match, or discard it.
- [ ] **I4. Calibration targets, and the tension between them.** The federal
      side wants mortgage interest reconciling to the **SOI Schedule A
      aggregate** so published revenue estimates hold. The state side wants
      property tax and rent reconciling to **ACS distributions** and to each
      state's published program statistics (WI DOR Schedule H claims by income
      range, MN DOR renter-credit totals, MI homestead reports). Those anchors
      can pull the same imputation in opposite directions. Fit once against both
      sets of margins; do not fit twice and discover they disagree.
- [ ] **I5. GATE — decide the blast radius before writing anything.** Two
      options, and they are not equivalent:
      (a) impute **tenure and rent only**, leaving `salt_prop` and the mortgage
      fields as they are — self-contained, unlocks the renter credits, leaves
      the federal MID problem untouched; or
      (b) **re-impute property tax and mortgage interest for the whole
      population** — fixes the federal problem too, but overwrites Tax-Data
      fields that existing federal estimates reconcile against, so it needs
      sign-off beyond this workstream.
      **Recommendation: (a) first, with (b) specified but gated**, because (a)
      cannot invalidate a published federal number and (b) can.

**Why (b) matters, stated once so it is not lost:** mortgage interest is imputed
*conditional on itemizing*, and itemizing is **endogenous to policy**. That is
harmless for current law and harmful for exactly the reforms most likely to be
asked about — change the standard deduction, the SALT cap or the mortgage
interest cap and the newly-itemizing population's Schedule A amounts are
under-imputed by around 60%. The `*_item_ded_potential` columns added 2026-08-15
inherit this precisely: they preserve as-if-itemizing amounts computed *before*
`do_1040()` zeroes them, but "as-if" can only recover what was imputed, so for a
non-itemizing homeowner with no imputed mortgage interest the as-if amount is
zero. Every independent-election state (CA OR HI DE AR MS MT AL …) therefore
understates state itemized deductions for the population its credits target.

### J — Put tenure and rent to work per state (~2–3 weeks)

The choice here is genuinely open and should be made on evidence from group I,
not now.

- [ ] **J1. Decide: targets in the fit, or a post-fit imputation.**
      - **(a) Second stage of the weights fit.** Add owner/renter counts — and
        ideally a rent or property-tax amount — to the state margin set, so the
        fitted weights reproduce each state's tenure composition by construction.
        Cheapest path (the fit machinery and the ACS cells already exist) and it
        makes tenure consistent with every other state margin. Limitation: it
        gets the state *composition* right without giving any individual record a
        credible rent amount, so amount-based credits stay approximate.
      - **(b) Per-state imputation after weighting.** Assign each record a
        tenure and a rent/property-tax amount conditioned on its state. More
        useful for the credits, which are amount-based and sharply
        income-tested, but it can fight the weights unless the same margins
        discipline both.
      - These are not exclusive: (a) makes the geography right, (b) makes the
        record right. If both, (a) must come first so (b) calibrates inside a
        consistent state distribution.
- [ ] **J2. Acceptance.** State-level owner/renter shares within tolerance of
      ACS; imputed rent and property-tax distributions matching ACS by state and
      income band; and — the real test — **program-statistic reconciliation** on
      the states whose credits this unlocks (WI Schedule H claims by income
      range, MN renter-credit totals, MI homestead reports).
- [ ] **J3. Retire the exclusion rows this makes unnecessary.** The
      Tier-1-blocked property/rent class is currently handled by exclusion on
      both legs, and the list is now long enough to be its own acceptance test:
      RI-1040H (TAXSIM 2017-2020 and, from 2026-08-23, PolicyEngine 2021-2024),
      MT's property rebate, VT and MN renter credits, WI's homestead credit, and
      the IL/CT property-tax credit inputs. Each row retired is a real
      validation gain rather than a bookkeeping change.

### H — Model-side gaps to decide, not defer

These do not block the sequence, but the rework grows the population they
affect, so they should be decided before G, not discovered after.

- [ ] **H1. `become_filer_eitc` does not exist.** Only `ctc.R:232` and
      `rebate.R:86` define one. The earnings-bearing non-filers this rework
      creates cannot claim EITC, and their computed `eitc` is multiplied out of
      every total by the `* filer` gate — **an EITC reform will score identically
      across vintages**. Decide whether to add it; it changes published
      refundable-credit scores, so it is a design decision, not a bug fix.
- [ ] **H2. `become_filer_ctc` requires `qual_ei == 0` exactly**
      (`ctc.R:232`), so a non-filer with $1 of earned income who gains a
      refundable CTC keeps `filer = 0` and has `ctc_ref` dropped from every
      total. Measure the silently-dropped credit mass, which grows with the
      rework, and raise the condition explicitly.
- [ ] **H3. Send the cross-model issue docs upstream** (T1–T9 / P1–P5,
      `research/state_tax/cross_model/taxsim_bug_reports.do` built but not sent) — unrelated to this workstream
      but the longest-standing open item in `research/STATUS.md`.

---

## Part 2.5 — Open notes this plan owns

`research/CONVENTIONS.md` requires a note with `status: open` to be cited from
its workstream's plan. This section is that citation.

| Note | Status | What it owns, and how it relates to Part 2 |
|---|---|---|
| `research/state_weights/notes/nonfiler_proposal_rewrite_plan.md` | **open** | Rewriting `research/docx_sources/nonfiler_proposal_jii.docx` as a co-author-facing methodology proposal. A **communication** deliverable, not a modelling task, so it does not sit on the critical path — but it depends on the record being settled, and its own §"Discrepancies between the proposal and the implementation record" is the list of places the proposal and this plan disagree. Best cut **after A6 closes D6**, when the residual method stops moving. |
| `research/state_weights/notes/state_weights_alternatives.md` | **deferred** | Alternative weight constructions. Superseded in practice by the Phase 1 bake-off; kept for the reasoning. |

`state_weights_fit_issues.md` is `status: current` — an issues log cited from
where its findings are used, not a task list — so it is not listed here.

---

## Part 3 — Critical path and honest dates

```
P ✓ ────┬─> A2 ✓ ─> A2b extract re-pull (1-2d) ─> A4 (1d) ─> C (1-2w) ─┐
        ├─> A1 ✓ ─> A6 re-run (1d) ─────────────────────────────────────┤
        └─> B GQ fix (2-3d) ────────────────────────────────────────────┤
                                                                    v
                                    D Tax-Data V1/V2/V3 (1-2w + cluster)
                                                    │
                                    E federal validation (1w)  ← E4 runs on V2, early
                                                    │
                                    F state margins + re-fit (2-3w)
                                                    │
                                    G swap-in + close-out (1-2w)
                                                    │
                        ┌───────────────────────────┴──────────┐
                        │ SECOND PHASE (added 2026-08-23)      │
                        │ I national tenure/rent/proptax (3-4w)│
                        │        │  I5 is a GATE               │
                        │ J per-state use (2-3w)               │
                        └──────────────────────────────────────┘
```

**Groups I and J add roughly 5–7 weeks** and sit after G, with one exception
worth noting: **I1 (choose the donor survey) and I3 (the pre-TCJA itemizer fit)
depend on nothing** and can be settled during the long pole at C. Doing so is
cheap and de-risks the phase, because I3's answer determines whether the
imputation needs a full survey match or can lean on the pre-TCJA relationship.

**Roughly 8–11 weeks end to end**, of which only P, A1, A2/A3 and B parallelize.
**A2b is now the gate on phase C**, not A2: the design note is written, and what
C2 and C5 wait on is the re-pulled extract (`LINENO`, and the pension/annuity
variables the ASEC 2019 redesign split out from under us).
The Tax-Data age fix (D1) is the true bottleneck: §6.2 makes it a hard
prerequisite, because until it lands `age_band(tu_n$age1)` stays smeared across
the exact dimension the anchors discipline.

**Three things that would most reduce risk, in order:**

1. **Run E4 (the 2021 EIP3 / advance-CTC check) as soon as V2 exists.** It is
   the only external check on the non-filer level, and everything after D is
   built on that level being right.
2. **Settle P1 (the age bands) before anything else.** It is cheap, it is now
   constrained by what SSA actually publishes, and it silently invalidates D2,
   D6, §6.2 and the Tax-Data age draw if left implicit.
3. **Ship B (the GQ fix) immediately.** It is decision-independent, F6 has
   already sized it, and it is 42% of the residual in South Dakota.

### Per-task effort, and what the cluster actually costs

The table below is the 2026-08-18 breakdown, kept because it prices each task
separately. **Where it disagrees with the 8-11 weeks above, the figure above
governs** -- it is the later estimate and it accounts for the ASEC step. Task
letters here are the imported plan's step numbers; the mapping to P/A-H is
one-to-one in order.

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


## Revision history

- **2026-08-19** -- Merged with the 2026-08-18 out-of-repo unified plan and
  renamed to `plan.md` as the one plan of record for this workstream. Added the
  full risk register (§1.4), the per-task effort table and cluster-cost note
  (Part 3), and the ops constraints (Part 2 preamble). Task group E now points at
  `nonfiler_federal_validation.md`, which is the imported plan's Step 4 extracted
  whole. Corrected on the way in: the proposal's filename (renamed 2026-08-18),
  Cilke 1998 as the below-threshold model (replaced by Mok 2017), and the design
  memo's length. Predecessors archived as
  `nonfiler_state_weights_todo_2026-08-19_pre-merge.md` and
  `nonfiler_unified_plan_2026-08-18_imported.md`.
- **2026-08-19** -- Original: the plan review and implementation to-do, checked
  line-by-line against the tree and the shared store.
