# kg_dynamics single-pool collapse — session summary (2026-07-09, spec v3)

Successor to `eta_migration_SESSION_SUMMARY.md`. Collapses the realization model
from the v2 nested (responsive/inert) bucket structure to a **single pool**:
all gains respond on the permanent margin via the entropy Bellman, plus one
calibrated fraction of gains that retimes across year boundaries. Branch
`wealth`, on top of the spec-v2 entropy commits.

---

## 1. What changed and why

**The object removed** is the responsive/inert split. v2 carved baseline
realizations `r_B` into a responsive Bellman slice `(1−Φ)` and an inert slice
`Φ`, the latter split by `ω` into a truly-fixed floor `φ_I = Φ(1−ω)` and a
timeable slice `planned_share = Φω`. The fixed floor `φ_I` was a **point mass at
`b = ∞`** (infinitely-forced sellers) bolted onto the smooth spectrum — a legacy
of the old quadratic (tail-less) cost, which needed an exogenous floor to pin
the response.

**Why it's redundant now.** The entropy realization cost is a reservation-benefit
spectrum: each dollar has a non-tax reason to sell `b` (exponentially
distributed), the holder sells iff `b ≥ wedge`, and realizations are the
survival function. The thin exponential tail *is* the hard-to-deter mass, so a
discrete fixed floor duplicates what the cost already encodes. And the inert
floor is exactly what removed the interior aggregate revmax (it paid `τ` on a
linear, non-responsive base rising to 100%).

**v3 model — one pool, two margins:**

1. **Level margin (entropy Bellman, on ALL gains).** The discretionary
   reference is the full baseline rate `r_D_B = r_B` (no `r_exog` carve-out).
   FOC closed form:
   ```
   r_D = r_B * exp( -eta * (MC_S - MC_B) ),   clipped to [0, 1]
   ```
   where `MC = tau + beta*(1-m)*W_next + beta*m*F`, `F = (1-c_phi)*tau`. Since
   `d ln(r_D)/d MC = -eta` everywhere, **eta is the long-run CG semi-elasticity
   directly** (no responsive-half deflation).
2. **Timing margin (additive short-run overlay).** A single calibrated fraction
   `f = KG_DYN_TIMEABLE_SHARE` of ALL baseline realizations retimes across the
   ±TIMING_WINDOW years toward the lowest-wedge year (move-share =
   `clamp(dwedge/ref_wedge, 0, 1)`), composed as a net shift:
   ```
   r_S = r_ordinary_S + (r_planned_S - r_planned_B)
   ```
   It nets to zero under a uniform permanent shock, so it leaves the long-run
   level response untouched.

The SSZZ ~50% "untimeable" figure moves entirely into the short-run timing
channel; the permanent margin keeps no floor.

---

## 2. Code changes

`src/sim/kg_dynamics.R`:
- Removed `KG_DYN_SHARE_INERT` (Φ), `KG_DYN_PHI_I`, `KG_DYN_SHARE_PLANNED`;
  renamed `KG_DYN_TIMEABLE_FRAC` (ω, fraction of Φ) → `KG_DYN_TIMEABLE_SHARE`
  (fraction of ALL gains). `KG_DYN_SPEC_VERSION → 3L`.
- Bellman sweep/solver: dropped `phi_I`/`planned_share`; `r_exog_B = 0`,
  `r_D_B = clip(r_B, 0, 1)`, cap 1. `kg_dyn_build_scenario_rate` composes the
  overlay; dropped `r_fixed_B`. `kg_dyn_validate_realization_buckets` →
  `kg_dyn_validate_timing_params` (single `timeable_share ∈ [0,1]`, NA allowed
  while uncalibrated). Dropped the `r_exog > 1` degeneracy guard.
- Diagnostic-only drops (state contract preserved — applier reads only
  `rate_factor, extra_R, deemed_factor, R_B, G_B, p_char`): `lambda_I`,
  `r_V_B`, `r_V_S`, `r_fixed_B` removed from `step_recurrence`,
  `build_cell_table`, `build_summary`.
- Provenance guard: `share_inert`/`timeable_frac` checks → single
  `timeable_share`.
- **No SLURM / run.R changes** (wrappers use defaults). `sigma_conversion.R`,
  `calc/functions/income/kg.R`, the `turnover.R` applier: no code change.

Calibrator `other/kg_model_tests/calibrate.R`: nested (Φ,ω) bisection →
**two sequential 1-D bisections** (eta off long-run at any f; f off short-run at
that eta), justified by exact f-invariance of the long-run moment. Tests
updated: `test_planned_timing.R`, `test_naive_limit.R`, `test_terminal_charity.R`,
`test_provenance_guard.R`, and `other/corporate_incidence/verify/check_core.R`.

---

## 3. Calibration (2026-07-09)

Baseline: v2 full-sample vintage (kg_eta_recal_iter2 → then kg_v3_iter1);
Tax-Data 2026050315, Macro 2026022522, applier allocation 0.5.

**eta = 2.3992 (FINAL)** — via the standard dilution loop (long-run dilution
stable ~1.13). Full-sim long-run `E_full = -2.524` at sim-year 30 (nominal
-2.52, on target); the long-run moment is f-invariant in the full sim too
(permanent shock, no retiming), so eta is final independent of f.

**f = KG_DYN_TIMEABLE_SHARE = 0.2542 (FINAL)** — full-sim short-run `E_full =
+5.28` (nominal +5.04, ~5% over). The short-run dilution is **unstable in f**
(measured 1.09 at f=0.24, 1.31 at f=0.29): the short-run moment's denominator
is the realization-weighted dtau at t+1, and raising f pulls gains out of the
hiked year, shrinking that denominator and inflating `E_full_short`
nonlinearly. So the bathtub dilution loop oscillated (undershoot at f=0.24 →
+4.25; overshoot at f=0.29 → +6.08), and the full-sim short-run is sharply
nonlinear/noisy in f near the solution (sim points: 0.2426→+4.25, 0.2542→+5.28,
0.2627→+5.48, 0.2891→+6.08). **Resolution:** eta locked; f pinned by direct
full-sim fitting toward +5.04. Fixed at 0.2542 (~5% over the short-run anchor)
rather than chasing further ~45-min sims for a tighter fit, because the
deliverables (revmax, regression, sigma) are level-margin/eta-driven and
f-insensitive (permanent shocks; sigma orthogonal). See the automation
follow-up above.

Health checks: calibrator f-invariance assertion (long-run at f=0 == f=1)
returned |diff| = 0 exactly; 0 frozen r_D_B=0 cells.

**FOLLOW-UP (calibration automation).** The *outer* dilution loop is still the
historical manual paste-and-rerun (calibrate → paste → full sim → measure →
paste → repeat); the *inner* bathtub bisection is automated. That manual loop
assumes a roughly stable dilution near the operating point, which the
single-pool SHORT-RUN violates (dilution swings 1.09 → 1.31 in f because the
short-run denominator = realization-weighted dtau at t+1 is itself f-dependent),
so it bounces instead of converging. f was therefore pinned by a hand-run 1-D
root-find over the *full-sim* short-run moment (3 sims + a quadratic fit). This
should be scripted: a `pin_f` SLURM driver that does submit-sim → wait-for-DAG →
measure_dilution → secant update on f → resubmit, terminating on
|E_full_short − 5.04| < tol. Not built this session (the loop was ~2 steps from
done); left as a follow-up so the next recalibration isn't hand-cranked.

---

## 4. Verification

- **Unit suite green**: planned-timing / terminal-charity / provenance-guard /
  naive-limit. The naive-limit revmax Laffer curve now bends to a **clean
  interior peak (~0.36–0.38)** (naive 1/2.52 = 0.397), vs v2 running to the
  grid edge — the headline payoff of the collapse.
- **Full pipeline**: 3-year smoke + 30-year recal sims all COMPLETED clean
  (221 tasks each, no errors), confirming the applier / Phase 2B bathtub /
  aggregation / summary all work under the trimmed cell_table.
- **Revalidation** (regression / revmax / σ — no Clausing, per scope):
  - **σ (kg_v3_sigma)**: top-ordinary ETI (with-sigma leg) mean 2026-35 =
    **0.2505**, identical to v2 (sigma_recal_eta). σ unchanged at 0.08 —
    orthogonal to the single-pool change (ETI excludes cap gains).
  - **Regression (kg_v3_regress)**: `baseline_check` invariant clean (static
    exactly 0; conventional max|yr| 0.0013 ≈ 0). Reform conventional Δrev
    (cum 2025-55, $B): rate_up_5pp +1100 (static +2475), carryover +1318
    (static +527), deemed +3148 (static +1632) — all directionally sound. One
    benign, pre-existing failure: `deemed` Phase-3b heir reattribution at 2025
    (Estate-Tax-Distribution has no 2025 heir file; deemed needs years >= 2026)
    — not a v3 issue; revenue estimates wrote fine.
  - **Revmax (kg_v3_revmax)**: the curve is **concave from the first step**
    (marginals decline steadily, vs v2 staying ~+$350B/step to 35%) and bends
    hard to a **plateau by ~45%** — the single pool is more responsive. Honest
    peak location: the statutory-top-rate peak is **~44-45%** (marginal still
    +$104B at the 40% step on cum10, collapsing to +$6B by 45%); the clean
    `1/eta ~ 40%` is a *realization-weighted-rate* statement (shown in the
    synthetic naive-limit unit test), and the statutory peak sits higher because
    the realization-weighted rate is well below the statutory top. Literal grid
    argmax still +25pp/45% (marginal fractionally positive) -> extend grid to
    +30/35pp for the turn-down (open item, as v2).
    Implied realization-weighted semi-elasticity of the realized base:
    **step-up ~2.5-3.2** (30y ~2.5 recovers the -2.52 calibration anchor),
    **carryover ~2.1-2.7**, **deemed ~1.3-1.9** (half as elastic — a chunk is
    realized mechanically at death) -> deemed's revmax rate is highest.
    Charts: `other/kg_model_tests/revmax_v3_laffer.html` (30y, 3 panels x 3
    regime lines: marginal revenue, semi-elasticity, and log-log elasticity
    with the eps=1 revmax reference; step-up eps->1.06 at 45% = peaked, deemed
    only 0.45 = highest revmax rate). Table:
    `python3 other/top_tax/analyze_revmax.py kg_v3_revmax`.

    Cumulative Δrev by regime ($B; 20% = current-law top rate, step-up 0 by
    construction, carryover/deemed nonzero = pure death-regime base effect;
    marginal per 5pp in parens):

    10y (FY27-36):
    | top rate | step-up | carryover | deemed |
    |---|---|---|---|
    | 20% (curr) | 0        | 294        | 748        |
    | 25%        | 338(+338)| 692(+399)  | 1263(+514) |
    | 30%        | 647(+309)| 1061(+369) | 1748(+486) |
    | 35%        | 924(+277)| 1396(+334) | 2201(+453) |
    | 40%        | 1028(+104)| 1527(+131)| 2399(+198) |
    | 45%        | 1034(+6) | 1536(+9)   | 2420(+21)  |

    30y (FY27-56):
    | top rate | step-up | carryover | deemed |
    |---|---|---|---|
    | 20% (curr) | 0          | 1574        | 3437         |
    | 25%        | 1940(+1940)| 3811(+2237) | 6226(+2788)  |
    | 30%        | 3650(+1710)| 5806(+1995) | 8794(+2569)  |
    | 35%        | 5131(+1481)| 7555(+1749) | 11136(+2342) |
    | 40%        | 5686(+555) | 8237(+682)  | 12164(+1027) |
    | 45%        | 5706(+20)  | 8275(+38)   | 12268(+104)  |

    (30y top step is slightly less flat than 10y — deferred realizations from a
    rate hike partly return within the longer window.)

**Overall: spec v3 built, calibrated, and revalidated — all green.**

---

## 5. Files

Code: `src/sim/kg_dynamics.R`. Calibrator/tests: `other/kg_model_tests/{calibrate.R,
calibrate.sbatch, measure_dilution.R, test_planned_timing.R, test_naive_limit.R,
test_terminal_charity.R, test_provenance_guard.R}`,
`other/corporate_incidence/verify/check_core.R`. Runscript:
`config/runscripts/tests/kg_v3_smoke.csv`.

Vintages (local v1): kg_v3_smoke, kg_v3_iter1, kg_v3_iter2, kg_v3_iter3.
