# State income tax workstream — status

**As of 2026-07-13** (branch `state-tax`). Companion docs in this directory:
`state_tax_implementation_plan.md` (the design of record, amended in place),
`state_weights_ml_alternative.md` (the A/B bake-off spec),
`state_weights_fit_issues.md` (the engine root-cause record),
`state_tax_model_research_notes.md` (original evidence base).

---

## Done

**Phase 0 — plan and review.** Seven-phase plan committed; full code-verified
review resolved eight findings (SALT post-workaround semantics decided,
`st_agi.conformity_year` added for fixed-date IRC conformity states,
cross-row `states` validation relocated to `parse_globals()`, jurisdiction
set fixed at 53 = 51 modeled + PR/OA, `filing.yaml` schema home, torch
question closed). Plan later extended with locality weights (§2.6) and
LODES-based cross-border liability (Phase 7).

**Phase 2 — parameter schema + pilot states** (`3d0848853`, `bbd285687`).
`build_state_tax_law()` reuses the federal YAML machinery with `st_`
prefixing; `parse_subparam()` tolerates `reference` citation keys (indexed
subparams included — regression-tested). IL, CO, NY encoded 2017-forward
from primary sources with a citation on every subparameter, including the
2017 IL blended rate, CO's TABOR temporary rates and three deduction-addback
regimes, NY's full rate-schedule history through the enacted 2033 reversion,
NY Pease thresholds pulled per-year from the instruction PDFs, and the
2025/2026 credit restructurings in both CO and NY. Encoding conventions
locked: 10-element uniform schedules, anchor-at-2017 year lists,
feature-absent = NA.

**Phase 3 — state calculator** (`41e818d2f`). `src/calc/state/` implements
`do_state_taxes()` + `calc_st_{agi,ded,exempt,txbl,tax,credits,liab}` under
the standard calculator contract, including the NY tax-benefit recapture
from the published worksheet identity (reproduces the printed constants),
the NY §615(f)/(g) itemized limitation tiers, both Empire State child credit
regimes, CO's tiered CTC + FATC, and the state-filer flag. Verified by 12
hand-computed form-worksheet cases plus smoke/subset-state grids.

**Phase 4 — orchestration + outputs** (`38e3bf201`, `4d1a65560`). Runscript
`states`/`state_tax_law`/`state_detail` columns; per-state loop inside
`run_one_year()` (no SLURM worker sync needed); `totals/state.csv`,
`supplemental/state_rev_est.csv`, compact per-year state detail matrix,
stacked state reports; SLURM 3a/3b/4 synced. **Acceptance verified on real
data: with-state federal outputs byte-identical to without-state.** Runs on
uniform placeholder weights until Phase 1 lands (state levels not yet
meaningful; all contracts real).

**Phase 1 — state weights (engines and data done; harness remains).**
- Data: shared stores built and wired — IRS-GEO mirror (HT2 2012–2022,
  percentile, county, ZIP + SOI docguides + per-family change notes;
  public repo johniselin-budget-lab/IRS-GEO, data on the cluster share) and
  the IPUMS ACS extracts. `read_ht2()` ingests the full 24-series target
  map; `read_acs_extract()` handles the fixed-width format, implied
  decimals, and the INCTOT sentinel (`5d92e5763`, `c27cb1c99`).
- Target assembly (`55ee03ec2`): filer HT2 state×stub targets + non-filer
  ACS cells, share-normalized to PUF national totals (levels are fixed by
  construction; only geography is calibrated).
- Engine root-cause (`51a6eefc2`): invariant leak closed (negative HT2
  targets → assembly block + assertions; verified 1.1e-15); multi-series
  IPF non-convergence proven STRUCTURAL (one constraint per cell is its
  valid class — counts-only converges in 1 iteration to 100%).
- Vectorized joint fit (`cf3cd19ee`): counts-backbone IPF prior →
  exponential-tilting gradient engine; 2022 full fit hits 82.9% within 2%,
  MARD 1.43%, 7.5 min under sbatch, loss still descending.
- Reconciliation/validation battery (`7bf614823`, `418795efb`, `726de2d54`,
  `7239c26a6`): model-free individual-level IRS-vs-ACS coverage (married
  85.6% / single 77.6% / children 109.2%, wide state spreads), wage dollars
  (96.6% vs ACS — the high anchor), QWI structural check (workplace-basis
  commuter signature), LODES residence-basis fix (RAC + OD commuter matrix;
  DC residents hold 31% of DC jobs), and the candidate demographic target
  dimensions (QWI sex×age, ACS marital×age).

**Ops learned the hard way** (recorded in the issues doc): weight fits
OOM-kill on the login node (~7 GB cap) and piping masks the kill (pipeline
exit = tail's); run under `sbatch` with inputs staged on NFS scratch
(`/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`).

## Left to do

1. **Phase 1 close-out** — the §4 comparison harness: tune the joint fit to
   the ≥99%-within-2% bar (steps/lr schedule/per-series λ; SALT and
   EITC/AGI families carry the residual), β sweep, untargeted validation on
   the QWI/ACS demographic cells, downstream IL/CO/NY liability under
   candidate weights; then the `state_weights_{year}.csv` writer for
   2014/2016–2022 (2013/2015 HT2 gaps: interpolate or skip, decide),
   projection-year carry-forward, and the dispatcher swap off
   `placeholder`. The prior-only vs joint-fit comparison IS the reframed
   A/B decision.
2. **CO child-care expenses credit** (DR 0347) — researched and encoded
   (TODO carried in `co/credits.yaml`); CO 2026 rate revisit after the
   TABOR certification (~Sept 2026).
3. **Phase 5 — cross-model validation harness (record level: BUILT
   2026-07-18; per-state triage remains).** Harness at
   `other/state_tax_research/cross_model/` (see its README):
   record × state × year design on unweighted PUF samples, TAXSIM-35
   (usincometaxes WASM, 2017–2020) + PolicyEngine US (pinned venv, 2021+),
   $15/$100 match rates plus a federally-aligned "clean" metric, stage
   diagnosis from TAXSIM v32–v40, machine-readable known-differences list
   with record-level predicates. Fixing the federal `taxsim_crosswalk`
   took 12 latent-bug repairs plus income-concept alignment (other_gains,
   part_se double-count, se_health sign, state_ref, gssi rename). Results
   so far: 6 no-tax stubs + NH + TN + IL validate at ~100% clean match;
   the other 13 broad-IIT states sit at 27–86% clean match@$100 with
   per-state stage histograms as punch lists (dominant stages: exemptions
   for AZ/IN, deductions for CA/VA/NY/KY, state AGI for
   CO/ND/SC/GA/MI/NC/SC — the fed-taxable-start states' v32 labels are
   partly a TAXSIM-semantics artifact, see README caveat). PA/ID initial
   runs 2026-07-23: PA 60-63% clean vs TAXSIM (dominant wedge: TAXSIM nets
   losses across PA's floored income classes; TAXSIM DOES model Tax
   Forgiveness — verified) and 81-88% vs PE; ID 59-71% vs TAXSIM (PBF
   filing-edge ±$10, CTC −$205 cluster, fed-taxable v32 wedge flipping
   sign at TCJA) and 43-58% vs PE, where PolicyEngine does not net the
   grocery credit into state_income_tax (whole-window annotate; candidate
   exclusion). Remaining: per-state mismatch triage (ours vs TAXSIM's
   error), aggregate benchmarks vs HT2 total tax (weights-blocked), and
   revenue-agency comparisons.
4. **Phase 6 — 50-state rollout** by structural family (no-tax stubs → flat
   fed-AGI → graduated fed-AGI → fed-taxable → own-base → federal-
   deductibility), CA first (CalEITC as the credit-schema acceptance test;
   CA CPI indexation series).
   **PA + ID encoded 2026-07-23** (25 → 27 jurisdictions): PA is the first
   OWN-BASE state — new generic components landed for class-share bases
   with per-class loss floors (`st_agi.ob_*`, reusable for NJ/AL/AR/MS),
   the poverty-forgiveness credit family (Schedule SP), the per-person
   credit family (ID grocery credit), and a per-return excise (ID PBF —
   never repealed, contrary to prior belief). ID is fed-taxable with a
   CPI-indexed flat-tax zero bracket (not the statutory $2,500/$5,000),
   MFS on the single schedule and HoH on the MARRIED schedule. Research
   surprises: PA enacted a TY2025 state EITC (Working Pennsylvanians Tax
   Credit, 10% federal match, refundable) and a TY2025 student-loan
   deduction; ID's CTC sunsets after TY2025. Worksheet tests PA-1..7b,
   ID-1..7 pass; cross-model triage started (see rollout tracker rows).
5. **Phase 7 — later scope**: coupled federal↔state iteration + sales-tax
   election imputation; frozen-base mechanics for fixed-date conformity;
   locality weights from SOI county data (§2.6; NYC first); cross-border
   wage taxation via the LODES OD matrix; state MTRs and combined-MTR
   behavior; state distribution tables; pre-2017 law; population-projection
   aging of weights.
