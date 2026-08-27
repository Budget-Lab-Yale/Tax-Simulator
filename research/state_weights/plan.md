---
title: "State weights and the non-filer rebuild — the plan"
role: plan
workstream: state_weights
status: current
updated: 2026-08-27
sot: self
supersedes: []
superseded_by: null
---

# State weights and the non-filer rebuild — the plan

**The plan of record for this workstream. Start here.**

Rewritten 2026-08-24. The predecessor is
`research/archive/state_weights_plan_2026-08-24_pre-asec-rewrite.md`; it records
*how* these conclusions were reached, including one retraction chain worth
reading. This document states where things stand and what happens next.

**Companions**, one job each: `nonfiler_residual_design.md` is the **method** of
record; `nonfiler_residual/04_findings.md` is the frozen **evidence** (F1–F10);
`nonfiler_residual/10_asec_tax_unit_design.md` is the **ASEC unit design**
(D-A1…D-A7); `nonfiler_federal_validation.md` is the **procedure** for the
federal battery; `research/decisions_log.md` holds settled arguments (S-series).

---

## 1. The goal, and the one decision that reshaped it

Give the model state-representative weights. Records are partitioned by the
`filer` flag: filers are targeted to IRS HT2, non-filers to population-based
anchors. The non-filer half is the hard half, because it is the one input
anchored to nothing.

**Decision S13 (JI, 2026-08-24): the DINA non-filer append is replaced, not
patched.** Tax-Data currently appends ~27.6M units from DINA at DINA's own
uncalibrated weights. Measured on 2026-08-24, that file cannot be repaired:

| | |
|---|---|
| level | carries **65%** of the anchor's non-filer adult mass; **77%** of the shortfall is in 18–44 alone |
| investment income | `fiint`, `fidiv`, `fikgi` are **exactly zero** for every non-filer |
| wages | **24%** of what SSA and the SOI W-2 study jointly imply |
| age | `ageprim` is **three values for everyone** — substantially an income-pattern proxy (bucket 65 is 89.5% SS recipients, **0.0%** wage earners) |
| repairability | **11.71M** of bucket 65's 13.09M hold *old-age* SS against an anchor of 11.87M at 65+, so only **26%** of the age movement the shares demand is coherent |

So the national non-filer population will be **built from the CPS ASEC** and
calibrated to the residual anchors. Group D — the three Tax-Data patching
vintages — is dropped. **Group C now builds the population**, not just a filing
model for the ACS margins.

**The counter-argument, stated rather than buried.** Treasury, the IRS and JCT
all abandoned survey-based non-filer construction; the IRS because ASEC
non-filer income *"still fell short of the income reported to the IRS by third
parties"* — a ~17% undercount against a 50.7M target. That is a *count* problem
and we calibrate the count to the anchor. And DINA is worse on income than the
method they rejected: ASEC wages are **1.011×** SSA HI, dividends 0.449 of SOI,
interest 2.624, against DINA's 24% / 0.000 / 0.000.

---

## 2. Where things stand

| group | what it does | state |
|---|---|---|
| **P** | pre-flight: age bands, vintage, dependents/MFS scope, covered-worker universe, anchor tolerance | ✅ 2026-08-19 |
| **A** | inputs: SSA readers, CPS-ASEC extract, ASEC unit design note, Mok/Cilke coefficients, anchor re-run | ✅ 2026-08-23 |
| **B** | differentiated group quarters in the ACS margins | ✅ 2026-08-24 |
| **D0** | the anchor age shape (7-band) | ✅ 2026-08-24 — **role changed**: now a validation target, not an input |
| **F1a** | non-filer targets count **adults**, not tax units | ✅ 2026-08-24 |
| **F1c** | dorm-student netting as a second anchor universe | ✅ 2026-08-24 |
| **C** | **build the national non-filer population** | **next — §3** |
| **E** | federal validation of the new pool | after C |
| **F** | state margins, targets and the re-fit | F1b onward, after C |
| **G** | production swap-in | after F |
| **H** | model-side gaps to decide, not defer | open, §6 |
| **I / J** | national tenure/rent/property-tax imputation and its per-state use | second phase, after G |

**Two things from the completed work are load-bearing and easy to lose:**

- **F1a retired a number the tree had been quoting.** 17.2% of non-filer units
  are joint, so adults/units is 1.1717 on the PUF side and 1.0989 on the ACS
  side. Every margin-vs-anchor MARD before F1a divided units by adults and sat
  near 1.0 by coincidence. Like-for-like the margin runs **11.90%**, not the
  8.79% once recorded for B1. The first honest placement metric is **fitted adult
  shares vs anchor shares: MARD 10.54%, 15 of 51 states within 5%**
  (`scripts/f1_adult_target_check.R`).
- **F1c ships on universe consistency alone.** Netting dorm students out of the
  anchor is right because B1 removed them from the margin and the PUF non-filer
  partition holds no dependents (0 of 13,204 records). It does **not** improve
  the fit — the TY2022 gain did not replicate on TY2017, and it moves only
  ~20 of 51 states toward 1 in either year.

---

## 3. Group C — build the non-filer population

**Two surveys, two jobs.** The ASEC is where the model can be built and checked
(it separates `INCINT`/`INCDIVID`/`INCRENT`, which Mok needs and the ACS
combines into `INCINVST`). The ACS is the only file large enough to populate
state × age × tier cells, so it does **geography only** — which needs no Mok
covariates. This is what makes S13 simpler than the old design, where the model
had to transfer per-record to the ACS and three of Mok's seventeen terms could
not follow.

Prerequisites all in hand: the ASEC unit design note, the 11-year extract with
`LINENO` and the pension variables, Mok/Cilke coefficients, the residual anchors
and the 7-band age shape.

1. **ASEC tax units** — design note §4 rule for rule. Householder is primary,
   spouse via `SPLOC`, children via `MOMLOC`/`POPLOC`, subfamilies from
   `FAMREL`/`FTYPE`/`FAMID`. Dependents constructed, retained and tagged (P3).
   MFS is a calibrated post-step, never in the estimation frame (D-A3).
   **`SCHLCOLL` added to the extract 2026-08-27** — the qualifying-child test
   needs full-time enrollment for 19–24-year-olds and the extract carried no
   enrollment variable at all (`EDUC` is attainment). It was the only genuine
   remaining gap; the pull re-runs all eleven years.
2. **Income concepts** — from ASEC components, not `ADJGINC`, which is matched
   to the PUF and therefore circular (S12). Two measures per D-A5: excluding
   capital gains (what the coefficients score on) and including them (what is
   reported). Never mixed.

   **Two 2019-redesign asymmetries, both settled 2026-08-27 (S16).**
   *Retirement income* is harmonised by IPUMS's own mapping —
   `INCRETIR` for TY2014–17, and `INCRET1+INCRET2+INCPEN1+INCPEN2+INCRANN` for
   TY2018–24. Verified: `INCRETIR` alone breaks **−61% / −51%** between the two
   anchor years while the harmonised series runs **+6.3%/yr nominal with
   recipients in step**. Never use `INCRETIR` alone across 2018.
   *Capital gains* (`INCCAPG`) exist only from TY2018, so the including-gains
   measure can be built for TY2022 and not TY2017 — build it where it exists.
   The excluding-gains measure, which is what the coefficients are scored on,
   exists in both years, so **the filing model is unaffected; the asymmetry hits
   the benchmark.** Neither needed an extract change beyond A2b's.
3. **Filing model** — Mok's 14 group probits below threshold
   (`resources/mok_coefs.csv`); above threshold a hazard calibrated to Pub 5785's
   **11.19M** units, national scalar for v1 (D3).
4. **Joint calibration** — group constants and the hazard scalar chosen
   *together* against the residual anchors: 47.3M (2017) / 46.5M (2022)
   non-filing adults, shaped by `resources/nonfiler_age_shape_{year}.csv`.
5. **⚠ GQ backfill from the ACS.** The ASEC has **0.27M** group-quarters persons
   against a PUF universe that includes them — **8.15M**, 16.8% of the residual,
   **42% in South Dakota**. Household non-filers from the ASEC, GQ non-filers
   from the ACS via `classify_gq()`. Design memo §3.0's universe invariant must
   be **asserted**, or this silently undoes B1.
6. **Emit in the PUF schema** — 189 variables per Tax-Data's
   `config/variable_guide/baseline.csv`, zero-filled where unobserved.

**Where it lives.** Build here, in the research tree; publish the pool to the
shared `model_data` store; have Tax-Data read it through
`config/interfaces/interface_versions.yaml`, exactly the pattern DINA uses now.
Tax-Data then swaps one upstream input for another rather than absorbing a new
model — architecturally right, and the smaller ask of that repo's owner.

**Acceptance is not the old C8.** That tested ACS filer units against HT2
`n_returns` — a *level* test, and after F1 the level comes from the anchors. The
pool's acceptance is the four-dimension comparison in §7.

### 3b. Retire the DINA sex split too (S14)

`demographics.R:27-37` targets DINA male shares for unmarried units in 8 cells
(`filer` × `has_kids` × `employed`). Replace it and the DINA interface leaves
Tax-Data entirely. Measured 2026-08-24: `p_male` runs **0.2476–0.6454**, so the
cells matter — but not where assumed.

| cell | source |
|---|---|
| `filer = 0` (4 of 8) | **disappears** — the ASEC observes `SEX` |
| `filer = 1, employed = 1` | SOI W-2 Table 1: nonjoint by sex = **1.A − 1.D** |
| `filer = 1, employed = 0` | **0.68%** of unmarried units — no source needed |
| `has_kids` | **19.3pp swing**, and no admin table crosses sex with children — see below |

All 51 sheets of `w2_all_2017.xls` were checked: sex is crossed with age, wage
size, AGI size and joint-return share, and **never** with "Return and Earner
Type". So W-2 Table 1 anchors the marginal male share and the ASEC build supplies
the kids split — admin owns the level, survey owns the shape.

---

## 4. Open decisions

Recommendations below; none is signed off.

| # | Question | Recommendation |
|---|---|---|
| **1** | **Mok's omitted group.** Her footnote says "non-Hispanic white with more than a college education", which sits oddly with a two-dummy scheme. | **Settled 2026-08-24 from the coefficients.** `educ_less_than_hs` is negative in 14/14 groups (mean −0.414); `educ_college` is **positive in 10/14** (mean +0.158). If the omitted group were the top category both would be negative. Omitted = **HS to some college**; the footnote is loose. The four negative cells are −0.010 to −0.073, noise. **Approved for implementation 2026-08-27 (JI), with one carried item: JI to read Mok's Table 14 and its footnote directly and confirm the inference, since it contradicts the published wording.** Calibration re-fits the group constants, so a wrong intercept is partly absorbed — but the education *gradient* is not, and no level adjustment repairs that. |
| **2** | **Adult-dependent count.** `DEPSTAT` 13.80M, HT2 identity 5.58M, Mok's linked data +11M. | **Do not reconcile — they measure different things.** The anchor needs *return-claimed* adult dependents = the HT2 identity; Mok's +11M is why that is a **floor**. `DEPSTAT` is the survey benchmark and carries IPUMS's own warning, a TY2014→15 break (6.37M → 13.36M), and **10.8% of pointers landing on a modelled non-filer**. Report all three; average none. **Reframed 2026-08-27 by the SOI sweep JI asked for, and the three figures now reconcile.** Pub 1304 **Table 2.3** splits dependent exemptions by *relationship* — children at home / away from home / parents / other — published through TY2017 only (TCJA repealed exemptions), so it covers one anchor year. TY2017: parents 3.370M + children away 0.422M = **3.792M all-adult floor**; adding mixed "other dependents" 7.755M gives a **11.547M ceiling** on the non-home categories. The HT2 identity's 5.58M sits inside that and is corroborated as a floor; `DEPSTAT`'s 13.80M becomes plausible once adult children at home are added; Mok's +11M is the same gap. **They were never three estimates of one thing.** Five pre-TCJA years are now in the store as `national/by_size/exemptions_{year}.xls`. TY2022 has no analogue in the store — the nearest is the credit for other dependents in Pub 1304's credit table, which needs a fetch. Still unmeasured anywhere: dependent AGE, hence the adult share of the 83.161M children-at-home exemptions. |
| **3** | **Support test.** Mok's is not operationally defined. | Adopt **TRIM3's** (household spending split equally, compared to own income) and record it as the one place D-A2's "follow Mok" cannot be followed. Report how many adult dependents flip under alternative thresholds. **Approved 2026-08-27 (JI).** |
| **4** | **ACS subfamily fidelity.** `SUBFAM`/`SFTYPE`/`SFRELATE` are absent from our ACS extract, and IPUMS's definition differs from Census's. | **Stakes dropped under S13** — the ACS now builds only the prior. Write the rule **once** as a shared helper taking the survey as an argument; test both sides on a common case; extend the extract only if that test shows material disagreement. Exposure for scale: **89.0M people, 26.9%** live in a household containing a relationship the conventions treat differently. **Approved 2026-08-27 (JI).** |
| **5** | **MFS state target.** The HT2 residual absorbs qualifying surviving spouses. | **Measured 2026-08-27 and largely answered** (`notes/anchor_basis_comparison.md` Part C). HT2 carries no MFS series at all, so the residual `N1 − MARS1 − MARS2 − MARS4` is MFS + QSS — but netting T1.6's published MFS off it puts QSS at **0.084M (2017) / 0.056M (2022) returns, 1.4–2.2% of the residual**. So the residual **can** carry the state distribution with a named 2% contamination, and the national level comes from T1.2/T1.6 (3.993M TY2022). Second finding: T1.6 folds QSS into its joint block and therefore counts a surviving spouse as **two** adults, overstating `filing_adults` by 0.03–0.04% — a bias, not noise, and one F1d must correct because it adopts T1.6 as the level. **JI asked whether the PUF can impute a correction: it cannot identify QSS** — `filing_status` comes straight from `MARS` and takes four values, folding QSS into joint exactly as SOI does — **but it supplies an independent second MFS level** (3.186M / 3.768M against the published 3.213M / 3.993M), which brackets QSS at **0.06–0.28M, 1.4–7.0% of the residual**. Report the range, not the point. |

---

## 5. After C

**E — federal validation** (`nonfiler_federal_validation.md` owns the runbook).
The V1/V2/V3 A/B staging is gone with group D, but three items get *more*
important because the whole population is being swapped:

- **E2, the tripwire.** Every 1040 dollar aggregate is summed `* weight * filer`,
  so `totals/1040.csv` and `supplemental/cbo_comparison.csv` must be
  **identical** under current law. Any movement falsifies "non-filer only".
- **E3, the combined-universe wage constraint.** SSA HI wage-and-salary as the
  constraint; the admin-implied non-filer total is **$480.6B (2017)** against the
  PUF's $116.2B.
- **E4, early.** The 2021 baseline pays EIP3 (~$402–411B) and advance CTC
  (~$93B) to non-filers against published actuals. The **only** external check on
  the non-filer level.
- **D4 survives** — assert `identical(ids_in, ids_out)`. `run.R:348-357` binds
  precomputed random numbers **positionally**, so a new builder has the same
  silent-rerandomization hazard a rake would have had.

**F — state margins and the re-fit.** F1a and F1c are done.

- **F1b** — residual anchors as the primary non-filer targets. Was blocked on
  D1; now waits on C's pool.
- **F1d (new, 2026-08-27)** — **fix the anchor basis before F1b uses it.** The
  national anchor takes filing adults from Pub 1304 T1.6 and the *state* anchors
  take them from the HT2 identities, so the 51 states do not sum to the national
  figure: **−1.36% (2017) / +2.14% (2022), sign flipping between the anchor
  years.** The fit targets the state file; this plan quotes the national one.
  Measured in `notes/anchor_basis_comparison.md`: taking the **level** from T1.6
  and the **state shares** from HT2 forces them to agree by construction, gives
  the level T1.6's adults-only universe (which the HT2 basis cannot have, having
  no age), and moves **no state outside its own tolerance in either year**. It is
  a consistency fix, not a change of answer. **Approved 2026-08-27 (JI).**

  **And the level is the out-of-state-corrected one.** SOI's Other Areas
  footnote — *"returns filed from Army Post Office and Fleet Post Office
  addresses by members of the armed forces stationed overseas; and returns filed
  by other U.S. citizens abroad"* — puts those filers outside the Census
  resident population, so they must come out of the level rather than be
  reallocated. That is basis **B2**: `(T1.6 level − out-of-state) × HT2 share`.
  It is a real ~1.0–1.2M change (43 of 51 states outside tolerance in TY2017),
  which is now evidence the correction matters rather than an argument against
  it. Consistent with the filer side, which already carries 53 jurisdictions.

  **Fold in the QSS correction at the same time**, because B2 makes T1.6 the
  level source and T1.6 counts a surviving spouse as two adults: 0.06–0.28M.
  The two universe corrections compound, raising the anchor **+2.4% (2017) to
  +3.2% (2022)**.

  **One thing the same pass opened and did not close:** the 0.3–0.5% "two SOI
  routes disagree" constant behind `E_FILING_ADULTS = 0.005` **does not
  decompose** — naming the identifiable universe differences leaves a remainder
  of 0.9% in 2022 with the sign flipped, so the tolerances are understated on
  that component. The sweep localized it: a third construction from Pub 1304
  Table 2.3 agrees with T1.6 to **−0.057%** against HT2's −0.370%, so the
  remainder is an HT2-versus-Pub-1304 family difference, not Table 1.6.
- **F2** — demote the income tiers to the prior. The non-filer fit still
  converges in **2 iterations to 4.4e-16**, i.e. it is still pure prior
  reproduction; F2 is what makes it a calibration.
- **F3** — SSA margins at the right status: OASDI 65+ can bear a firmer target
  (100% data, single `65p` band only); the HI covered-worker margin is a **1%
  sample**, so soft target or prior only. QCEW stays a diagnostic.
- **F4** — split `state_weights.R` into `ht2.R` and `filing_model.R`, which is
  what makes the filing model sourceable by Affordability-Index.
- **F5** — validation additions: the population identity vs PEP, the EITC
  take-up correlation re-run, state adults by age band as a held-out metric.
- **F6** — re-fit at config-7 (β=1e-4). **Do not promise held-out MARD gains**
  from the non-filer work alone; pensions, Schedule C and capital gains are a
  *filer*-target problem.

**G — swap-in.** Tune to the ≥99%-within-2% bar, write
`state_weights_{year}.csv` for 2014–2022 (**2013 is the only HT2 gap** — the
store carries 2012 and 2014–2022, verified 2026-08-27; earlier drafts said
"2013/2015" and `ht2_2015.csv.gz` is in fact present),
carry forward to projection years, vintage-tag the files, and flip the
dispatcher off `placeholder` at `src/sim/run.R:433`.

Two G items live only in `state_weights_phase1_summary.md` §6 and are easy to
lose, because the *decision* reads as if it had been implemented: **config 7 is
not wired.** `build_split_weights()` forwards hyperparameters through `...` and
nothing supplies them, so `fit_gradient()` still defaults to β=1e-3, constant
lr, 500 steps — the adopted β=1e-4 / cosine / 3,000 exists in prose only. And
the **239-cell structural core** is neither pruned nor reported at assembly;
since scratch was reclaimed (§9 of the summary), that now needs the prior and a
candidate fit regenerated first.

**The filer half is finished as a method and parked by S2, not blocked.** Its
one substantive gap is the summary's §7.1 and it is not something C can fix:
held-out geography is weak exactly where the target set carries no structure
(pensions 17.0, taxable SS 17.1, Sched C amount 30.8, capital gains 61.0 MARD),
which is what F6 warns against expecting from the non-filer rebuild.
Demographic target expansion — QWI sex×age residence-corrected via LODES RAC,
ACS marital×age — is scoped, its fetchers are built (`fetch_qwi()`,
`fetch_lodes_rac()`, `fetch_lodes_od_matrix()`), and it is unstarted.

---

## 6. Decide, don't defer (group H)

- **H1. `become_filer_eitc` does not exist.** Only `ctc.R:232` and `rebate.R:86`
  define one, so an EITC reform scores **identically** across vintages. Adding it
  changes published refundable-credit scores — a design decision, not a bug fix.
- **H2. `become_filer_ctc` requires `qual_ei == 0` exactly**, so a non-filer with
  $1 of earned income keeps `filer = 0` and has `ctc_ref` dropped from every
  total. Measure the dropped mass; it grows with the rebuild.
- **H3. Send the cross-model issue docs upstream** (T1–T9 / P1–P5;
  `taxsim_bug_reports.do` is built, not sent). Longest-standing open item in
  `STATUS.md`.

---

## 7. Verification

**The pool must beat DINA on all four measurable dimensions, not just the level:**

| dimension | benchmark | DINA today |
|---|---|---|
| age composition | `nonfiler_age_shape_{year}.csv` | 0.42 / 0.63 / 1.10 across 18–44 / 45–64 / 65+ |
| wage mass | SSA HI covered wages | $116.2B = 24% of the $480.6B implied |
| SS receipt | OASDI-SC beneficiaries | 11.71M with old-age SS |
| investment income | Pub 5785 receipt ceilings — 14% interest, 9% dividends, 4% gains | **0.000** on all three |

**Sex split:** reproduce the current `male1` distribution under the W-2 target
before swapping. A shift moves EITC and CDCTC through `male1`/`male2`; this is
not a cosmetic variable.

**Ops** (these have all bitten before): fits and ACS/ASEC tabulations run under
`sbatch` — the login node OOM-kills at ~5–8 GB and **piping masks the kill**,
since a pipeline's exit status is `tail`'s. Pattern:
`nonfiler_residual/run_acs_tabulation.sbatch` (48G, ~3m40s, MaxRSS 3.8G). HT2
reading uses `fread(cmd = 'zcat …')` and is POSIX-only. Load the R module in the
**same** shell command as `Rscript`. Run
`Rscript research/tools/check_conventions.R` before pushing documentation.

---

## 8. Open notes this plan owns

`CONVENTIONS.md` requires a `status: open` note to be cited from its plan.

| Note | Status | What it owns |
|---|---|---|
| `notes/student_cross_state_linkage.md` | **open** | Cross-state student linkage, out of B1. IPEDS exists and was measured 2026-08-24 — but reallocation *degrades* the fit under the current anchor (MARD 8.79% → 10.04%) because the PEP anchor already places students in the institution state. `MIGPLAC1` rejected. **The prior question is definitional** — residence or claiming return — and no data source settles it. Validation-only use survives either answer. |
| `notes/anchor_basis_comparison.md` | **open** | Which source sets the level. Recommends F1d's basis change (safe: 0 of 51 states move outside tolerance) and largely answers decision #5 (QSS is 1.4–2.2% of the HT2 status residual). Leaves open the out-of-state bucket question and records why return-claimed adult dependents cannot be sourced better than the current bound — `NUMDEP` is **absent from HT2 2022**, and `N2` breaks 6.3% at TCJA. |
| `notes/nonfiler_proposal_rewrite_plan.md` | **open** | Rewriting the narrative proposal as a co-author-facing methodology document. Communication, not modelling; best cut once the record stops moving. |
| `notes/state_weights_alternatives.md` | **deferred** | Alternative weight constructions; superseded in practice by the Phase 1 bake-off, kept for the reasoning. |

---

## Revision history

- **2026-08-24** — Rewritten. The predecessor had become unreadable end to end
  after a day of layered amendments; it is archived as
  `state_weights_plan_2026-08-24_pre-asec-rewrite.md` and still carries the
  measurements behind group D's removal and the F1a retraction chain. This
  version states conclusions and next steps. Substantive change on the same day:
  S13/S14 drop group D and rebuild the non-filer population from the ASEC.
