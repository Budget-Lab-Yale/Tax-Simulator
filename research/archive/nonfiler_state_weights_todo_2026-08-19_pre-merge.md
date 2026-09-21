# Non-Filer + State Weights — Plan Review and Implementation To-Do

**Date:** 2026-08-19
**Reviews:** `research/state_weights/nonfiler_residual_design.md` (as amended 2026-08-18),
`research/state_weights/nonfiler_residual/04_findings.md`, `research/archive/07_ssa_inputs_plan_2026-08-19_executed.md`,
`research/STATUS.md` §1/§1b, `research/state_tax/plan.md` §2.1 (as amended),
`research/state_weights/state_weights_phase1_summary.md` §5/§7.
**Scope:** everything between here and the production state-weights swap-in.
**Method:** every claim below was checked against the code and the shared store
on 2026-08-19, not read off the memos. Where a memo and the tree disagree, the
tree wins and the discrepancy is called out.

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
      - **National: +/-3.5% (+/-1.68M)** on a 46.5M residual.
      - **State: +/-2.2% (MS) to +/-6.3% (SD)** — a flat tolerance is wrong by
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
      - **Remaining:** emit as `residual_tolerance_{year}.csv` from A1 so the fit
        reads the tolerance rather than re-deriving it.

### A — Close out the SSA inputs and research pass A (~1 week + 3–5 days, parallel)

- [x] **A0. SSA acquisition, verification, registration and documentation.**
      *Done 2026-08-19* — record in `research/archive/07_ssa_inputs_plan_2026-08-19_executed.md`.
      Workbooks verified
      (4/4 cells PASS), flat series adopted as source of record (exact against
      the workbooks: 59 areas × 11 measures × 2 years), both families registered,
      `NOTES.md` written and placed, Chrome confirmed absent from the cluster,
      three bugs fixed in `01_fetch_residual_inputs.R`.
- [ ] **A1. Write the two SSA readers** and replace the
      `ssa_covered_persons = NA_real_` stub at
      `02_build_residual_anchors.R:197`. Follow the `read_pub1304_t16()` pattern.
      Requirements from `NOTES.md`, all of which are easy to get wrong:
      **sum the 51 jurisdictions, never `All areas`**; read the flat series and
      **assert against the workbook** so the agreement stays enforced; handle the
      missing **2010** and the pre-2007 `Virgin Islands` label; for EEDATA read
      **Table 4 (HI), not Table 1 (OASDI)** per P4, taking
      **`Number → Wage and salary`**, not `Total` (`Total` ≠ wage + SE — a worker
      with both is in both components); and pull the **state × age** margin from
      **Table 5**, which the design did not know existed. Stamp the universe tag
      `covered_worker_hi` on the output.
- [ ] **A2. Research pass A — ASEC tax-unit and income construction** (§8, ~3–5
      days). **The longest-lead item on the plan, and the gate on phase C.**
      Survey PolicyEngine's Enhanced CPS, Census SPM units, TAXSIM's CPS
      conventions, Urban/TPC; record where they agree and diverge and choose
      deliberately. **Start from what the extract already carries**:
      `CPS-ASEC/cps_asec_common` holds IPUMS's own `FILESTAT` filer recode,
      `DEPSTAT`, `ADJGINC`, `TAXINC` and `FAMUNIT` — establish what those already
      do before building a unit builder that duplicates them. Dependents and MFS
      are in scope from the start (P3), so the builder must form and retain both.
      Deliverable: a design note in `research/state_weights/nonfiler_residual/`.
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
- [ ] **A4. Transcribe Mok (2017) Table 14** → `resources/mok_coefs.csv`, 14
      group equations with SEs and per-cell filing rates. **Transcribe from a
      rendered image, not a text dump**: Panel E's columns run "Age 65 or Older"
      **first** (sample sizes 909 vs 62,438 disambiguate), and Panel E has `.`
      rather than a coefficient for self-employment in the 65+ column.
- [ ] **A5. Optionally transcribe Cilke** → `resources/cilke_coefs.csv` as the
      comparison fit only. **Extract with PyMuPDF word positions**, not
      `pdftotext -layout`, which silently mis-assigns coefficients. Skip if time
      is short — the memo says if only one is fit, fit Mok.
- [ ] **A6. Re-run 01 → 02 → 03** once A1 lands; move **D6 from partially
      resolved to resolved**; update `04_findings.md` with the T5 state margins
      the OASDI and covered-wage columns were specified with.

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

## Part 3 — Critical path and honest dates

```
P (2d) ─┬─> A2/A3 research + extract (3-5d) ─> A4 (1d) ─> C (1-2w) ─┐
        ├─> A1 SSA readers (2-3d) ─> A6 re-run (1d) ───────────────┤
        └─> B GQ fix (2-3d) ────────────────────────────────────────┤
                                                                    v
                                    D Tax-Data V1/V2/V3 (1-2w + cluster)
                                                    │
                                    E federal validation (1w)  ← E4 runs on V2, early
                                                    │
                                    F state margins + re-fit (2-3w)
                                                    │
                                    G swap-in + close-out (1-2w)
```

**Roughly 8–11 weeks end to end**, of which only P, A1, A2/A3 and B parallelize.
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
