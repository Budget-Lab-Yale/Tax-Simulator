---
title: "The non-filer replacement: handoff map"
role: notes
workstream: state_weights
status: open
updated: 2026-08-30
sot: research/state_weights/plan.md
supersedes: []
---

# The non-filer replacement: handoff map

The map of the non-filer workstream for collaborators, organized in four
parts: **(a)** this document, **(b)** fixes and updates to existing Tax-Data
code, **(c)** the non-filer construction itself, **(d)** the state work that
follows. Every claim links its evidence; every settled argument carries its
decision ID (`research/decisions_log.md`, S13–S19). Render per S8 for review;
comments come back to the Markdown.

## What this replaces, in one paragraph

Tax-Data appended DINA microdata as its non-filing population: no observed
ages (three `runif` buckets), no sex, no dependents (hard-coded probability
draws), 0.0% interest and dividend receipt, and an age composition *inverted*
relative to the administrative residual. The replacement is built from the
CPS ASEC — tax units constructed rule-by-rule, a two-model filing decision
(Mok's probits below the filing threshold, a Pub 5785 composition hazard
above it), group quarters backfilled from the ACS — calibrated so that
emitted records plus claimed-dependent netting equal the residual count of
non-filing adults exactly, per age band, per year. It beats the DINA file on
every measurable dimension (`nonfiler_pool/results/acceptance_{year}.csv`).

## (b) The Tax-Data branch: `asec-nonfiler-pool`, three commits

Review in order; `NONFILER_BRANCH_NOTES.md` on the branch carries the
per-commit checklist.

| commit | what | review question |
|---|---|---|
| 1 | **Extensive-factor mask restricted to filers.** A standing defect on `main`: non-filers sat inside the mask that sets every 2020+ growth factor, and their weights grow apart from filers by +4.7% (2025) / +18.6% (2055). DINA's wages and pensions contaminate it *today*. | accept that filer totals move by exactly this correction |
| 2 | **Filer weights on observed IRS counts through 2023** (returns per adult: .5991 → .6314 in 2020 → .6008 in 2023, fully reverted — the demographic handoff now lands after the spike, not inside it), and **non-filer weights on the S18(b) band series** (observed residual counts through 2023, then a 10-year phase to the 2017–19 share norm on CBO band population). | the 10-vs-5-year phase; the stable-netting-share assumption; weights@2023 with income factors@2022 |
| 3 | **Read the published `ASEC-Nonfilers` pool.** `impute_nonfilers.R` collapses to read + assertions + bind; non-filer sex observed, not redrawn (S14); DINA interface stays for its remaining filer-side consumer. | reproduce `male1`/`male2` against `main` — they route EITC/CDCTC |

**Mortality needs no change, and the reason must survive this document:**
ages are never incremented; every cell is rescaled to its year's population,
so a record is an *age slice*, not an aging cohort. `q_death(frozen age,
actual year)` is the correct lookup under that semantics, deaths already
live on the weight side (cohort ratios track CBO survival within 0.5pp), and
q_death never scales weights. "Fixing" frozen ages into cohort aging would
double-count mortality.

**Flagged, not fixed:** the person-slot demography joins `dep_age_group1-3`
— codes 1–4, not ages — as ages, so dependents match the population factor
for age 1–4. Pre-existing on `main`, affects filers too, owner's call.

## (c) The construction, and where its evidence lives

The build is `research/state_weights/nonfiler_pool/01`–`15`, one numbered
script per stage, gates printed and committed. The method reference is the
*Non-Filer Residual* artifact; decisions S13–S18; benchmark disputes in
`notes/anchor_basis_comparison.md`.

- **Years.** Built directly for every year 2014–2023. The ceiling is set by
  the inputs, not the survey: HT2 ends 2022 (state products), Pub 1304 T1.6
  ends 2023 (the national anchor), the ASEC reaches income year 2024. For
  2023 the two S15 level corrections are *carried* from 2022 at 0.59% of the
  published level, since HT2 cannot measure them there.
- **Calibration basis.** The hazard calibrates on each year's own Pub 5785
  column (2014–16) and on rates against our own above-threshold population
  after (S18); the mean-income ratio is flat to 1.1% across the published
  years, the strongest evidence for the approach.
- **Known bounds.** The self-employment margin misses by 1.1–2.4pp in
  projected years — the ASEC's joint distribution cannot supply Pub 5785's
  wages×self-employment overlap (S17, accepted and recorded per year in
  `hazard_margins_{year}.csv`). For 2020–21 the hazard level itself is an
  S20 construction — the projected level deflated by observed pandemic
  excess filing — resting on two stated assumptions (the 2019→2023 rate
  counterfactual and the above-threshold attribution share θ), bracketed by
  report-only sensitivity columns in `pandemic_filing_adjustment_{y}.csv`;
  the post-deflation margin drift (married +4.2pp worst) is committed the
  same S17 way. Aging-vs-rebuild drift:
  `aging_check_summary.csv` — one year costs +1.2%, five cost ~5% on the
  level with the age composition swinging sign, and 2020–21 refuse a
  projection outright; hence S18(c), rebuild through the ceiling.
- **The handoff-year alignment (S19).** At T = 2023 the partition
  `filing adults + non-filer target + netting = adults` is asserted per age
  band on CBO's ssArea universe — the universe that carries every weight
  forward — and stays exact under aging because cells scale by
  `N_b(y)/N_b(T)`. The PEP↔ssArea wedge is published, not absorbed: ~1.5% on
  adults, amplified to **2.6–11.7% in the residual** (2.6% in 18_25; the five
  bands 26_34–65p span 5.6–11.7%) because the residual is a small difference
  of large numbers (`ssarea_alignment_2023.csv`, `ssarea_wedge_2023.csv`).
  The band×married split is deliberately NOT asserted: T1.6's primary-band
  convention makes it unidentifiable, and the unmarried 35_44 cell proves it
  by arithmetic.
- **The pool's per-band ssArea scale** (1.061–1.134) is applied at the 2023
  emit, in `05_emit_pool.R` — upstream, exactly once, gated pre and post
  against the alignment table (audit: `ssarea_scale_audit_2023.csv`).

## (d) The state work that follows

Proposed in `notes/state_work_proposal.md`: the split-weights re-fit on the
new pool (group F), the state wage and age margins for 2014–2016 (blocked
only on three SSA workbooks — `eedata_sc14/15/16.xlsx`, workstation
download), HT2's 2022 ceiling and what it does and does not cap, and
per-state rollout resumption.

## Ceilings and splits a reader must carry

| fact | value |
|---|---|
| non-filer file rebuildable through | **2023** national, 2022 with state products. 2020–21 build under **S20**: the held Pub 5785 hazard level is infeasible there (band 18_25 over-subscribed by the stimulus filing spike), so it is deflated per band by observed excess filing — level 12.39M → 8.89M units in 2020, 12.49M → 10.89M in 2021 (`16_pandemic_filing_adjustment.R`) |
| filer weight targets observed through | **2023** (then CBO demography) |
| income growth factors observed through | 2022, with a 2020–21 gap (upstream) |
| projection universe | CBO ssArea ≡ Macro-Projections cells (verified to the person) |
| anchor universe, 2014–2022 | Census PEP resident (state work stays here) |
| Pub 5785 (obligated non-filers) published | 2014–2016 only; later years projected |
