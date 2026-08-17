# Stage D Findings — Residual Non-Filer Diagnostics

**Date:** 2026-08-16
**Status:** Stage D executed (design memo §4), with two inputs still blocked
(§5 below). All numbers below are reproducible from the scripts in this
directory; result CSVs in `results/`.
**Companions:** `../nonfiler_residual_design.md` (the design this executes),
`../state_weights_phase1_summary.md` (weights bake-off the rework feeds).
**Scripts:** `01_fetch_residual_inputs.R` (login node),
`02_build_residual_anchors.R` (login node),
`03_diagnose_current_nonfilers.R` (`--acs` under `run_acs_tabulation.sbatch`,
job 22410622; `--tables` login node).

---

## 1. What was built

- **New shared-store families**: `raw_data/Census-PEP` (intercensal
  2010-2020 + vintage-2024 state × single-year-age × sex files),
  `raw_data/BLS-QCEW` (state total covered employment/wages, 2017 and 2022,
  from the CEW API). `raw_data/SSA-OASDI-SC` and `raw_data/SSA-EEDATA-SC`
  are created but **empty**: ssa.gov 403-blocks the cluster egress IP
  (verified with browser user-agents) — each carries a
  README_MANUAL_DOWNLOAD.md; re-running script 01 after placing files
  registers them.
- **`ht2_filing_persons()`** promoted out of `compare_individuals_acs_irs()`
  in `src/data/state_weights.R` (one definition per computation);
  `ht2_path()` repointed IRS-GEO → IRS-Ind; `build_acs_margins()` now also
  returns v0 filer units by state (diagnostic need).
- **Residual anchors** for TY2017 and TY2022
  (`results/{national_anchor,residual_anchors,nonfiler_wage_margin}_{year}.csv`):
  national by Pub 1304 T1.6 age bands, state totals by the HT2 identities.
- **Pub 5785 transcribed** (`resources/`): Table 1 (potential non-filers,
  person level) and Table 3 (above-threshold not-filer units) — the hazard's
  level anchor and the receipt-rate discipline.
- **Diagnostic tables T1–T7** (`results/T*.csv`).

## 2. Anchor validation (before using it to judge anything)

Two independent SOI constructions of national filing adults agree:
Pub 1304 T1.6 (returns × marital status × age, under-18 filers netted) vs
the HT2 filing-status identities — **205.5M vs 206.1M (−0.31%) in 2017;
214.1M vs 213.1M (+0.47%) in 2022**. The T1.6 block sums also reproduce the
published all-returns totals exactly (parse check).

The resulting residual — **47.3M (2017) / 46.5M (2022) non-filing adults
18+, 18.7% / 17.9% of PEP resident adults** — triangulates against IRS
Pub 5785's independent, information-return-based count of potential
non-filers: 49.7M–51.7M persons in TY2014-16 (falling filing rates and
population growth make the levels broadly consistent). The memo's
"roughly one in five adults" and ~20pp state spread both reproduce:
state residual shares run **10.6% (SD) to 27.7% (MS)** of adults in 2022.

## 3. Findings

**F1 — The Tax-Data non-filer mass is short, but by less than the raw gap
suggests (T1).** Raw: 32.4M non-filer adults (2022) vs the 46.5M residual —
ratio 0.70. Two wedges narrow it: DINA's universe is 20+, and PEP has 8.71M
18-19-year-olds (roughly 5-6M of them non-filing); and ~5.5M+ claimed adult
dependents (HT2 dependents 78.9M − PEP under-18 73.4M, a lower bound) sit
in the residual but ride filer records in the PUF — with heavy OVERLAP
between the two wedges (adult dependents are mostly 18-22 students). The
defensible comparable anchor is ≈ 38-41M, leaving Tax-Data **~15-25% short
(≈ 6-9M adults)**. → D1: yes, the national calibration (design memo §5.2)
is needed.

**F2 — The age composition is badly wrong, and this is unambiguous (T2,
TY2017 where production ages are native).** Tax-Data puts **8.9%** of
non-filer adults at 18-25 where the anchor says **24.2%** (2.7M vs 11.5M),
and **42.9%** at 65+ where the anchor says **25.1%** (13.2M vs 11.9M — the
level is nearly right at 65+; everything below 35 is missing). The
DINA 3-point age smear doesn't just blur the shape, it inverts it. This is
the single most consequential defect for the state weights, whose non-filer
cells key on age band.

**F3 — The income-composition zeros are confirmed and quantified against
what they should be (T3 vs Pub 5785 Table 1).** Production non-filers:
0.0% with interest, dividends, or capital gains in every year. The IRS's
information-return universe of potential non-filers has **14% with
interest, 9% with dividends, 4% with capital gains, 48% with Social
Security, 14% with pensions** (TY2014-16, person level). The repair targets
are now on paper.

**F4 — The aging drift is real but second-order over the budget window
(T4).** Non-filer units go 26.2M (2017) → 33.0M (2035); the non-filer share
of units rises 14.7% → 15.8% with no return-count discipline after 2019.

**F5 — The v0 filer bias reproduces and its geography is wide (T5).**
v0 ACS filer units = 0.933 of HT2 returns nationally, ranging 0.91–1.03
across states. The v0 non-filer margins run **0.78× (DC) to 1.51× (SD)**
of the residual anchor — a 2× cross-state spread in the margin the current
fit reproduces *exactly* (the non-filer partition is pure prior
reproduction). Correlation of the v0-vs-residual gap with EITC take-up is
mild (+0.12), so the v0 error geography is mostly its own thing (GQ +
threshold coarseness), not simply the take-up story.

**F6 — GQ is a first-order share of the residual in small states (T7).**
8.15M GQ persons (3.61M institutional, 2.81M dorm students 18-24, 1.74M
other) = **16.8% of the national residual**, but **42% in SD, 34% AK, 33%
VT, 31% ND, 28% MN**. Blanket exclusion (the GQ-excluded margin variant)
overshoots the other way — e.g. AZ drops to 0.77× the residual — confirming
the design memo §3.0 call: differentiated treatment, not exclusion. SD is
the clean exhibit: smallest residual share of adults (10.6%) yet v0 margins
1.51× the anchor, GQ 42% of the residual.

**F7 — Above-threshold non-filers are material and SE-shaped (Pub 5785
Table 3).** 10.6M-11.9M units with a filing obligation (TY2014-16), <20%
married, and ~45% with net business/farm income — the self-employment
signature that motivates an SE dimension in the eventual hazard (D3
stays: national scalar for v1, SE-aware cells as the upgrade path).

## 4. Decision points (design memo §4.4), resolved where the evidence is in

| # | Decision | Status |
|---|---|---|
| D1 | National non-filer calibration in Tax-Data? | **Yes** (F1, F2). Calibrate to the comparable-universe anchor (20+, net of claimed adult dependents); the age×marital rake of design memo §5.2 |
| D2 | Direct state margins vs TPC fallback | **Direct margins feasible** (T6): smallest state residuals 56k-88k (WY/SD/VT/AK/ND), far above thin-cell territory; state × 6 age bands stays viable once OASDI lands |
| D3 | Above-threshold hazard scalar vs cell | **Scalar for v1** stands; F7 documents the SE-cell upgrade path |
| D4 | GQ handling | **Differentiated treatment confirmed** (F6): exclusion overshoots; dorm-student reclassification + institutional retention, sized per state by T7 |
| D5 | Target adults not units | **Adopted** in the anchors (T1.6 side counts adults; x-vector `1 + (filing_status==2)` at fit time) |
| D6 | Age allocation layering | **Partially resolved**: the national age shape is now anchored by construction (PEP − T1.6). The state-level 65+/working-age layering awaits the SSA margins (§5) |

## 5. Blocked / remaining before Stage D fully closes

1. **SSA manual downloads** (ssa.gov blocks the cluster): OASDI
   Beneficiaries by State and County (`oasdi_sc`, 2017 & 2022) and Earnings
   and Employment Data by State and County (`eedata_sc`, 2017 & 2022) —
   download on a workstation into `raw_data/SSA-OASDI-SC` /
   `raw_data/SSA-EEDATA-SC` per each store's README, then re-run scripts
   01 → 02 → 03 to fill the OASDI age margins (D6), the
   persons-with-wages column of `nonfiler_wage_margin_{year}.csv`, and the
   covered-worker earnings shape.
2. **Cilke (1998) coefficient transcription** (`resources/cilke_coefs.csv`)
   — needed for v1b implementation, not for these diagnostics.
3. Optional: `--acs 2017` run for a TY2017 T5/T7 (the 2022 versions carry
   the decisions).

## 6. What this feeds next (per the decided sequencing)

Tax-Data rework (design memo §5: age draw from the anchor shape → national
calibration → aging fix → investment-income repair disciplined by F3's
receipt rates) → state-weights margins/targets rework (§6) → production
swap-in on the upgraded fit. The GQ treatment fix in `build_acs_margins()`
(D4) can ship first — it is decision-independent and F6 sizes it.
