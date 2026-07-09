# Top-tax exercise — DESIGN LOCK

**Date: 2026-07-08.** This file is the single source of truth for live design
rulings. Precedence: **this file > σ build plan
(`~/.claude/plans/hey-read-other-top-tax-interaction-prosp-shimmying-coral.md`)
> `interaction_prospectus.html` > `frontier_exercise_notes_2026-07-07.md`**.
Where a lower document conflicts with this one, the lower document is dead on
that point. Context: rulings 1–8 below were made by the author on 2026-07-08
in response to the codex review (`codex_review.md`) and an independent
verification of its findings (most code-level claims confirmed; severity
re-calibrated on several).

## Amendment (2026-07-08, post-validation — author-directed)

**A1. sigma central recalibrated 0.6 → 0.08, superseding ruling 2's
asserted values.** The Step-6 validation surfaced exactly the double-count
ruling 2 anticipated: at sigma = 0.6 the full-stack top-subset ETI came out
0.431 vs the SSG bracket 0.12–0.40, while the stack WITHOUT sigma (entity
shifting + evasion + charity) already delivers 0.2229. The author directed a
rough calibration of sigma to hit a total top ETI of ~0.25 (the SSG central;
concept verified apples-to-apples: taxable income excluding net gains,
after deductions). Solved by linearity and CONFIRMED at sigma = 0.08 (ETI
0.2505, vintage `sigma_calib_confirm`). sigma is now the RESIDUAL conversion
margin conditional on the rest of the stack; the 0.2/0.9 bands are stale.
Full provenance + staleness conditions: `SIGMA_CALIB_PROVENANCE` in
`src/sim/sigma_conversion.R`. The validation exhibit's cross-elasticities
(measured at 0.6) shrink roughly proportionally at 0.08 and remain
direction-consistent with their brackets.

## Live rulings (2026-07-08)

1. **τ_eq is defined by finite difference, then implemented as a recursion
   tested against it.** Ground truth = inject a test dollar into each
   (age, year) cell, run the EXACT `kg_dyn_step_recurrence` dynamics and
   tax-event order forward, measure PV of tax collected. The fast linear
   recursion is then required to match cell-by-cell in tests. Rationale
   (verified): the plan's original sketch used the Bellman's
   `r_exog = (φ_I + planned_share)·r_B` while the recurrence realizes at
   `r_S = r_fixed_B + r_ordinary_S + r_planned_S` (RETIMED planned bucket),
   and the recurrence's event order is deaths-take-the-full-stock
   (decedents don't realize in-year) while the sketch realized first.
2. **σ stays asserted (0.2 / 0.6 / 0.9 %/pp), with the calibration
   comparison run informally at build time** — the perturbation run's 2×2
   own/cross elasticity matrix is produced and eyeballed against the
   literature brackets, and we iterate on σ/pool if it looks wrong, but it
   is NOT a formal pre-registered fail gate and NOT residual calibration.
   The known caveat is accepted: the central 0.6 (P–P-derived) and high 0.9
   (Mortenson face value) are total-response estimates used while the P–P
   entity-shifting module also runs — a calibration double-count risk that
   the informal check is expected to surface if material.
3. **Elasticity bundles are DROPPED for now.** No low/central/high bands
   anywhere: the explorer shows central-bundle numbers only; the Tier-2
   hero-factorial band runs (288) are cut from the campaign
   (≈ 5,184 + standalones ≈ 5,200 runs). This dissolves the prospectus
   §2.2-vs-§2.3 contradiction (band readout promised for every package but
   budgeted only for the hero subset). Bands can be reinstated later as a
   pure run-campaign addition — nothing in the build depends on them.
4. **σ pool stays RECORD-level as planned**: gate = (any active business
   income) AND (taxable income ≥ top-bracket threshold); pool = all wages
   + 0.75·active PT. The known over-breadth (spouse W-2 on small-K-1
   records, one-time-gain gating) is accepted; the pool-composition
   diagnostic in the σ outputs remains the visibility mechanism.
5. **Entity shifting (`pearce_prisinzano.R`) is hardened BEFORE the σ
   validation runs**: SECA companion co-scaling (evasion-module pattern),
   order/required-MTR guards, and a conservation diagnostic
   (PT dollars out = corp-base + kg-offset dollars in). Verified gaps:
   currently no companion co-scaling, no guards, no diagnostics,
   dividends=gains offset assumption.
6. **Conversion inflow timing: end-of-year injection (inheritance-inflow
   convention), stated explicitly, with τ_eq made consistent by
   construction** — the finite-difference harness (ruling 1) injects its
   test dollar the same way, so the wedge prices exactly the dynamics the
   flow experiences. No same-year death split (~1% effect on flows for the
   working-age gated pool; not worth the recurrence complexity).
7. **No per-record σ persistence for normal runs.** The pre-pass persists
   only the cell-level tracker (per year: conv_inflow by age cell, pool
   size, mean wedge, σ stamp). The behavior module recomputes record-level
   conversions via the SAME shared pure function from the same inputs
   (static/baseline MTRs + persisted τ_eq cell table), with a hard
   conservation assert Σ(record Δconv) ≈ persisted cell inflow. Per-record
   dump exists only behind an env knob for smoke/validation/debug runs.
   (Replaces the plan's `sigma_conversion/{year}.csv` single-source-of-truth
   file — avoids ~150k supplemental files that `delete_detail` would not
   purge.)
8. **Documentation scope**: this design lock is the only hygiene item
   adopted now. Literature crosswalk, explorer metric edge-case
   definitions, and the extended decomposition exhibits are deferred to
   write-up time (crosswalk before publication).

## Standing rulings carried forward (2026-07-07 plan session)

- Gain-state entry = recurrence injection, single-state ("B-pure"); no
  separate σ ledger/hazard.
- Wedge = full Bellman-machinery extraction; the crude closed form
  (ρ/δ/x_regime) is exposition only, dead as code. σ hard-errors without
  kg_dynamics.
- Per-record wedge W_i = own-leg calculator MTRs − τ_eq(age_i, t); forcing
  = ΔW_i static-reform-vs-baseline (standard MTR-frame convention; known
  first-order approximation, same as every other behavior module).
- No phase-in φ(t); memoryless annual response.
- No corporate-base-vs-gain-state split parameter; composition is an
  output.
- Module order pinned + asserted: kg_dynamics → conversion/sigma →
  entity_shifting → evasion (charity in the standard stack).
- Build item 2 demoted to a validation run (now the informal check of
  ruling 2).

## Dead sections (do not build from these)

- Prospectus §1.5 crude closed-form wedge — exposition only.
- Prospectus σ phase-in, split parameter, residual calibration —
  superseded (rulings above).
- Prospectus §2.2 low/central/high band readout and §2.3 Tier 2 (288 band
  runs) — dropped for now (ruling 3).
- Notes §3 Layer-3 revenue-max search — dropped 2026-07-07 (unchanged).
- Plan §Step 2 per-record `sigma_conversion/{year}.csv` persistence —
  superseded by ruling 7.
- The ρ-pinning script/runs — subsumed by the Bellman extraction.
