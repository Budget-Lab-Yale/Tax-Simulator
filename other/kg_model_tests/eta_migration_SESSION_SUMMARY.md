# kg_dynamics entropy migration — session summary (2026-07-08/09)

Implementation of `~/.claude/plans/smooth-petting-tiger.md`: migrate the
kg_dynamics realization Bellman from a **quadratic** realization cost to an
**entropy (KL) cost**, with a nested `(Φ, ω)` bucket reparameterization. Branch
`wealth`, from HEAD `7b46a169c`. This doc is the human-readable index; the
mechanical detail lives in the companion files listed at the bottom.

---

## 1. What changed and why

**The object being changed** is the cost of realizing gains away from the
cell's baseline habit — one term in the representative-cell Bellman.

- **Old (quadratic):** `C(r) = (ψ/2)·r²`. Marginal cost linear in `r`. The
  implied realization response is linear-with-a-hard-corner and **saturates**;
  the CG revenue curve doesn't bend, so the revmax grid found no peak. The
  fixed share `φ_I = 0.4` was an uncalibrated 2026-05-06 judgment call.
- **New (entropy / KL):** `C(r) = (1/η)·[r·ln(r/r_B) − r + r_B]`. Marginal cost
  ∝ `ln(r/r_B)`, so the FOC gives the closed form
  **`r_S = r_B·exp(−η·(MC_S − MC_B))`** — a **globally constant
  semi-elasticity** response. It hits the literature elasticity under
  current-law step-up by construction and nests the naive CBO/JCT revmax
  `1/|s|` as its `Φ→0` limit.

**First-principles reading (reservation-benefit model).** Each dollar of gain
has a non-tax reason to sell `b`; you sell iff `b ≥ wedge`, so realizations are
the survival function of the `b`-distribution. Quadratic ⟺ uniform `b` (a hard
wall). Entropy ⟺ exponential `b` (a smooth spectrum: many easily-deterred
dollars, a thin tail of robust sellers). Constant semi-elasticity is the reduced
form the empirical CG literature already assumes, so the entropy cost is the
structural form that reproduces it.

**Nested bucket reparameterization.** The flat `(φ_I, planned_share)` pair
became two primitives:
- `Φ = KG_DYN_SHARE_INERT = 0.50` — **asserted** (SSZZ TPE-36 ~50% "untimeable"),
  env-overridable, NOT calibrated. The ordinary Bellman-responsive share is
  `1 − Φ = 0.50`.
- `ω = KG_DYN_TIMEABLE_FRAC` — **calibrated** (short-run moment). Of the inert
  share, the fraction that is mechanically timeable across nearby years.
- Derived: `φ_I = Φ(1−ω)`, `planned_share = Φ·ω` (so `φ_I + planned = Φ`).

---

## 2. Calibration (converged, 2 iterations)

`other/kg_model_tests/calibrate.R` — outer bisection over `ω` (short-run
moment), inner over `η` (long-run moment; response **decreasing** in η, so the
bracket/bisection direction is flipped vs. the old ψ). Baseline
`kg_recal_2pp_05` (30yr full-sample static detail).

**Final: η* = 4.4984, ω* = 0.5132** (φ_I = 0.2434, planned = 0.2566), Φ = 0.50.

Verify (full sim `kg_eta_recal_iter2`, `measure_dilution.sbatch`):
`E_full_long = −2.523` (nominal −2.52), `E_full_short = +5.039` (nominal +5.04)
— both on target. Dilutions 1.1095 / 1.2880 (calibrate.R). The ω-invariance
health signal held (η ≈ 4.50 across every ω in the outer loop).

`η ≈ 4.50` replaces `ψ ≈ 28.56`; provenance `spec_version = 2L`. Env knobs
`KG_ETA / KG_SHARE_INERT / KG_TIMEABLE_FRAC` (old `KG_PSI / KG_SHARE_PLANNED`
removed). NA-eta hard-stop guards an uncalibrated sim.

---

## 3. Verification — all green

| Check | Result |
|---|---|
| `test_naive_limit.R` | constant-semi-elasticity identity to **1e-10**; Φ→0 revmax argmax **0.360** (≈ 1/2.52) |
| unit suite (planned/charity/provenance) | pass; provenance guard returns TRUE on matching path at final params |
| regression PRE vs POST (`eta_migration_regression.md`) | **mech state byte-identical** (Φ/η-invariant frozen pass); `baseline_check` = 0; static invariant; `rate_up_5pp` offset −59% (~−50% nbhd); carryover/deemed conv totals move (intended) |
| σ re-derivation (Phase 6, `sigma_recal_eta`) | **σ unchanged at 0.08** — the top-ordinary ETI excludes cap gains, so kg is orthogonal; ETI legs reproduced within 0.0001; confirm ETI **0.2505** |
| revmax grid (`revmax_eta_v1`, `revmax_eta_v1_result.md`) | see §5 — corrected reading |

Vintages (local `…/model_data/Tax-Simulator/v1/`): `kg_eta_regress_pre`
(pristine-HEAD worktree), `kg_eta_regress_post`, `kg_eta_recal_iter1/iter2`,
`sigma_recal_eta`, `sigma_confirm_eta`, `revmax_eta_v1`, `clausing_v2_s50_eta`,
`regime_elast_2pp`.

---

## 4. Clausing-Sarin package check (user-requested)

Re-ran the central package (`clausing_v2`, s=0.5) under the new η
(`clausing_v2_s50_eta`) vs the pre-migration `clausing_v2_s50` (`clausing_eta_lineitem_check.md`):
- **Carryover-basis line item: +16%** conventional (255 → 297 $B / 10y) — clean.
- **Pref-rate (+5pp CG) line item: −$1966B** conventional — a **stacking
  artifact**, not a real loss: it mirrors a +$3.1T swing at the ordinary-rate
  layer (01) under the amplified elasticity; the two largely offset.
- **Package net: +$1.4T (+12.6%)** conventional — the robust number.
- Static bit-for-bit invariant. FLAG: per-layer attribution is now very
  stacking-order-sensitive; confirm the layer-01 induced-realization magnitude
  is intended before scoring this package on spec v2.

---

## 5. Two corrections to claims made mid-session (important)

**(a) "Revmax ~45%" was sloped, not peaked.** With Φ=0.50 inert, the aggregate
semi-elasticity of −2.5 implies the *responsive half* has semi-elasticity ≈ 5
(→ responsive-only peak ~20%), but the **inert half pays `τ`·(fixed dollars),
linear, rising to 100%.** So the aggregate single-year Laffer curve has **no
interior peak in the policy range** — single-year `τ·B` is still rising at 45%
(508 → 570 for step-up). What flattens near 45% is the **windowed-cumulative**
Δrev, and that is a **timing artifact** (realizations deferred out of the
10/30-yr window), not a Laffer peak. The revmax argmax pins to the +25pp grid
edge because the curve is still rising. `revmax_eta_v1_result.md` should be read
with this correction (naive 40% is the Φ→0 case; the Φ=0.5 floor pushes the true
peak far out).

**(b) Death-responsiveness ↔ revmax rate is an identity, not a tradeoff.**
`revmax ≈ 1/|elasticity|`, so a regime's elasticity *is* its revmax rate
inverted. The prior Table 2 elasticities (step −2.5, carryover −1.8, deemed
−1.0) imply revmax ≈ 40% / 56% / **100%** — deemed has no in-range interior
peak *by construction*, which is the correct economics (closing the death
escape lets you tax much harder). The entropy fix guarantees each regime has a
peak at 1/|s|; it cannot bring a low-elasticity regime's peak into range.

**Regime elasticity (preliminary, CAVEATED).** A faithful +2pp × 3-regime,
kg-only, sim-year-30 run (`regime_elast_2pp`) gave step −1.73 / carryover −1.60
/ deemed −1.56 when normalized by the statutory Δτ=0.02 — but this **understates
and does not reproduce the −2.52 anchor** because (i) the correct denominator is
the realization-weighted MTR change (as in `measure_dilution`), and (ii) the
`cg_02pp_*` configs are top-only and differ from the calibration's reform. Only
the **ordering** (step > carryover > deemed, compressed) is informative here.
A clean re-measure (calibration reform + weighted dtau, or top-subset
restriction) is an open item before publishing a spec-v2 Table 2.

---

## 6. Open item / candidate "spec v3" — drop the fixed bucket

A session insight worth recording. The discrete **fixed** bucket (`φ_I`) is
essentially a **point mass at `b = ∞`** (infinitely-forced sellers) bolted onto
the smooth exponential spectrum — and it is partly a legacy of the linear cost,
which had no tail and so needed an exogenous floor to pin the response. The
entropy form already embeds the "first dollar easier to sell than the last"
spectrum via reservation-benefit heterogeneity, so a discrete fixed/responsive
split is redundant *as a separate object*.

**Consequence:** the fixed floor is exactly what removes the interior aggregate
revmax (§5a). A single-pool model — one reservation distribution, everything on
the permanent margin responding with constant semi-elasticity `η` (= the −2.5
directly), a *thin* fat-tail for genuinely forced sales if the evidence warrants,
and the short-run timing operation applied to the full pool rather than a
sub-bucket — would:
- kill the artifact bucket,
- give a clean interior aggregate revmax ≈ 1/η,
- make η directly interpretable as the long-run elasticity.

**Before building it:** (i) it needs recalibration (η → 2.5 directly; re-fit the
timing channel); (ii) pin down whether SSZZ's ~50% "untimeable" is a
**short-run timing** statement (belongs in the timing channel, so the permanent
margin needs no 50% floor) or a **long-run** one (would justify a real tail).
That distinction decides how much, if any, forced tail the permanent margin keeps.

---

## 7. Files

**Code:** `src/sim/kg_dynamics.R` (entropy sweep, ψ→η, nested Φ/ω, validator,
provenance spec v2), `src/sim/sigma_conversion.R` (σ prose re-derived, unchanged
0.08).

**Calibration / tests (`other/kg_model_tests/`):** `calibrate.R`,
`calibrate.sbatch`, `measure_dilution.R`, `measure_dilution.sbatch`,
`kg_unit_tests.sbatch`, `test_naive_limit.{R,sbatch}`, `test_planned_timing.R`,
`test_terminal_charity.R`, `test_provenance_guard.R`.

**Docs (`other/kg_model_tests/`):** `representative_cell_bellman_results.md`
(spec-v2 section + superseded banners), `planned_bucket_results.md`,
`carryover_deemed_distribution_design.md`, `eta_migration_regression.md`,
`clausing_eta_lineitem_check.md`, `revmax_eta_v1_result.md`, this file.

**Configs / runscripts:** `config/runscripts/tests/{sigma_recal_eta,
regime_elast_2pp}.csv`; `config/scenarios/tax_law/tests/revmax/cg_02pp_{stepup,
carryover,deemed}/pref.yaml`.

**Not committed here (pre-existing untracked from the revmax-setup line):**
`config/runscripts/tests/revmax_cg{,_smoke}.csv`, `other/top_tax/analyze_revmax.py`,
`other/top_tax/build_revmax_grid.py`, and the `cg_{00,05,…}pp_*` revmax configs.

**No changes** to `run.R`, SLURM files, the applier/state contract, or
`wealth_dynamics.R` (safe-change list).
