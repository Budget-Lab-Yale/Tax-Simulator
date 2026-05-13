# Muted Anticipation in the Representative-Cell Bellman

**Date:** 2026-05-13
**Status:** Diagnosis + proposed extension; not yet implemented

This memo documents a structural limitation of the quadratic
representative-cell Bellman implemented in `src/sim/kg_dynamics.R`: its
response to *announced future* tax changes is implausibly small, even
when the model handles *contemporaneous* changes correctly. The memo
diagnoses the cause, rules out heterogeneity as the explanation, and
proposes a three-parameter power-form benefit function as the natural
fix.

---

## 1. The evidence

Two new test scenarios (in `config/runscripts/tests/kg_dynamics_time_varying.csv`)
introduce time-varying tax policy:

- **Delayed.** Policy announced in 2026; +5pp on the top LTCG rate
  effective from 2027 onwards. (Permanent post-2027.)
- **Temporary.** +5pp on the top LTCG rate effective in 2026 only;
  reverts to baseline in 2027.

Aggregate `txbl_kg` ($B) by year, new model vs legacy `kg/62`:

| Year | Baseline | New delayed | Legacy delayed | New temp | Legacy temp |
|---:|---:|---:|---:|---:|---:|
| 2026 | 1,554 | 1,542 (-0.8%) | 1,554 (0.0%) | 1,461 (-6.0%) | 1,410 (-9.3%) |
| 2027 | 1,493 | 1,383 (-7.4%) | 1,356 (-9.2%) | 1,495 (+0.2%) | 1,493 (0.0%) |
| 2035 | 1,593 | 1,457 (-8.5%) | 1,450 (-9.0%) | 1,594 (+0.1%) | 1,593 (0.0%) |

Per-age cohort breakdown for the new model under delayed, 2026 (the
announcement year):

| Cohort | %ΔR 2026 |
|---|---:|
| Young (18-39) | +0.81% |
| Middle (40-59) | +0.28% |
| Old (60-79) | -1.46% |
| 80+ pool | -3.34% |

The qualitative pattern is correct: legacy `kg/62` has zero anticipation
response (it is purely contemporaneous), the new Bellman produces real
forward-looking behavior, and there is even a theoretically-correct
*sign flip* across age cohorts in the announcement year under step-up
(working-age cells accelerate to escape the hike; elderly cells defer
because future-tau-up makes future death-state forgiveness more
valuable).

The *magnitudes*, however, are muted. A 5pp permanent tax hike known
one year in advance should plausibly drive a meaningful (~5-10%)
acceleration in realizations for the average young taxpayer. The model
produces less than 1%. This memo is about that gap.

---

## 2. Diagnosis: structural, not heterogeneity-driven

### 2.1 Why heterogeneity isn't the explanation

A natural first guess is that "the model treats all taxpayers within a
cell as identical, so it can't capture concentrated sophisticated-
planner anticipation that pulls up the average." That guess is wrong.

The model uses a global `psi` (curvature) and per-cell `kappa`
(intercept). The cell-level responsiveness *to a given change in
effective tax price* is identical across cells — the heterogeneity in
observed behavior comes from different cells facing different effective
tax prices (via mortality `m` and death-regime forgiveness `F`), not
from different curvature parameters.

So when we calibrate `psi` to match the average contemporaneous
elasticity, we are by construction matching average behavior. The
muted anticipation is *not* about under-weighting the tail.

### 2.2 What the math actually shows

The Bellman's anticipation channel in the announcement year (call it
year `t`) operates through:

```
MC_S(t) - MC_B(t) = beta * (1-m) * [W_S(t+1) - W_B(t+1)]  +  beta * m * [F_S(t) - F_B(t)]
```

Under delayed, `tau(t)` is unchanged, so `F_S(t) = F_B(t)` and the
second term vanishes. Only the first term — through future `W` —
drives any 2026 response.

**Envelope theorem at the t+1 optimum** gives the per-period future-W
sensitivity:

```
∂W*/∂tau ≈ -r_D + (1 - lambda_T - r_D) * beta * m * (1 - c_phi)
        ≈ -r_D    (for working-age, low-m cells under step-up)
```

For typical young-cohort `r_D = 0.02`, this is `-0.02 * Δτ` per period.
Chained backward through the recursion (sustained Δτ from t+1 onwards,
steady state):

```
ΔW(t+1) ≈ -r_D * Δτ / (1 - beta*(1-m))    for working-age
        ≈ -r_D * Δτ * 25                  (with beta = 0.96)
```

So `ΔW(t+1) ≈ -25 * 0.02 * 0.05 = -0.025`, giving
`ΔMC(t) ≈ 0.96 * 0.995 * (-0.025) ≈ -0.024`, and

```
Δr_D(t) = -ΔMC / psi = 0.024 / 23.1 ≈ 0.001
```

In relative terms: `Δr_D / r_D ≈ 5%`.

The model produces **+0.8%**, not the back-of-envelope 5%. The
remaining gap is the second-order envelope correction: in 2027+ under
scenario, `r_D` itself adjusts downward (less realization at higher
tau), shrinking the future tax base that's actually being "escaped" by
locking in today. The envelope assumes `r_D` is held fixed at baseline;
the actual chain through Pass 2 internalizes this and dampens further.

Both numbers — back-of-envelope 5% and actual 0.8% — are well below
naive economic intuition (which might suggest 20-50% acceleration for
sophisticated holders facing a known permanent rate hike).

### 2.3 The actual structural cause

The anticipation channel is intrinsically proportional to
`r_D * Δτ / psi`. For population-average parameters
(`r_D ≈ 0.02`, `psi ≈ 23`), this is small *by construction*:

- **`r_D` is small** because realization rates in the real-world
  population average around 2-5% per year.
- **`psi` is large** because it was calibrated against the average
  contemporaneous response — which itself reflects modestly inelastic
  empirical estimates (literature semi-elast around -0.5 to -1 wrt
  `1-tau`).

The two facts compound: the contemporaneous channel operates through
direct Δτ (order 1 × Δτ); the anticipation channel operates through
`r_D × Δτ` (order 0.02 × Δτ). The ratio is roughly 2-5%, and any
calibration that pins the contemporaneous channel automatically forces
the anticipation channel to be that much smaller.

You cannot tune your way out of this with the quadratic. Smaller `psi`
makes both channels bigger (and breaks the contemporaneous calibration);
larger `psi` makes both smaller. The *ratio* of anticipation to
contemporaneous is structurally pinned.

---

## 3. The deeper issue: functional-form curvature

The quadratic `b(r) = kappa·r - (psi/2)·r^2` has **constant marginal
curvature**: `b''(r) = -psi` everywhere. This means the local
responsiveness near the baseline `r_D_B ≈ 0.02` is the same as the
local responsiveness when a cell is "trying to push" up to
`r_D = 0.05` to lock in a lower rate.

Empirically, this is implausible. The marginal cost of one *extra*
percentage point of realization above the normal cell rate is probably
nearly zero for the first percentage point (transaction costs, capacity
constraints, liquidity, all roughly absent until you push hard), and
only starts to bite when you push toward extreme territory.

What we want is a functional form where:

- **Near the baseline**, `|b''|` is small → cells can amplify
  realizations significantly in response to anticipation signals.
- **At higher `r_D`**, `|b''|` ramps up → unbounded realization is
  still ruled out by feasibility and friction costs.

The quadratic doesn't do this. Any single global curvature parameter
locks both regimes to the same elasticity.

---

## 4. Proposed solution: power-form benefit

Replace the quadratic with a three-parameter power form:

```
b(r) = kappa * r - (psi / (1+alpha)) * r^(1+alpha)

b'(r) = kappa - psi * r^alpha
```

**Parameters:**
- `kappa(a,t)`: per-cell intercept (calibrated to baseline `r_D_B` per
  cell, as today).
- `psi`: global curvature scale (calibrated to contemporaneous semi-
  elasticity target, as today).
- `alpha`: new global curvature shape parameter.

**FOC closed form:**
```
r_D = ((kappa - MC) / psi)^(1/alpha)
```
clipped to `[0, 1 - lambda_T]`.

**Behavior:**
- `alpha = 1` reduces exactly to today's quadratic (useful for
  regression testing).
- `alpha > 1` gives **flatter marginal-benefit slope at small `r`**
  (typical realization range) and **steeper at large `r`** (extreme
  acceleration). For `alpha = 2`, the marginal slope `psi * r^alpha`
  evaluates to `psi * 0.0004 = 0.009` at `r = 0.02`, vs `psi * 0.02 =
  0.46` for the quadratic. About 50× flatter near baseline, so cells
  can amplify realization much more freely in response to a small MC
  change like the announced-hike anticipation signal.
- `alpha < 1` (but `> 0`) is the opposite direction: more local
  curvature, even less anticipation responsiveness.

### 4.1 Two-moment calibration

The power form supports a clean separation of moments:

1. **`psi` → contemporaneous semi-elasticity.** Same target as today
   (`dlog(R)/dtau ≈ -2.52` under a 1pp uniform bump, anchored at sim
   year 30). The relationship is now `psi · alpha · r_D_B^(alpha-1)` at
   the local FOC, but the inversion is still a 1D root find.

2. **`alpha` → anticipation magnitude.** New moment. Three plausible
   empirical anchors:
   - **1986 Tax Reform Act**: realizations roughly doubled in 1986 ahead
     of the 1987 ordinary-income-rate cliff. Very large and very
     concentrated; probably overstates average behavior.
   - **2012 fiscal cliff**: aggregate realizations spiked roughly 40%
     in Q4 2012 ahead of the 2013 rate hike. Closer to representative-
     average and the strongest single anchor.
   - **Cross-country panel**: studies of announced realization timing
     effects across OECD countries suggest a "one-quarter anticipation
     elasticity" around -1 to -2 (wrt expected next-period tau). Less
     model-specific.

Calibration target (concretely): under the delayed scenario described
in this memo, hit something like a 5-10% acceleration in the young-
cohort `%ΔR` in the announcement year. That brings the model in line
with the 2012-style anchor.

### 4.2 Implementation cost

Small. About 30-50 lines of code change in `src/sim/kg_dynamics.R`,
plus a new global constant `KG_DYN_DEFAULT_ALPHA` and a separate
calibration routine for `alpha` analogous to `calibrate_psi.R`.

Specifically:
- `kg_dyn_bellman_sweep_age`: change the FOC and W formulas to use the
  power exponent.
- `kg_dyn_solve_bellman_baseline`: update kappa recovery
  (`kappa = MC_B + psi * r_D_B^alpha`).
- Add `calibrate_alpha.R` (or extend `calibrate_psi.R` to a joint
  bisection over both parameters).

The state file contract and the bathtub recurrence don't change.
Downstream applier and diagnostic code don't change.

### 4.3 Risks and open questions

1. **Anticipation moment selection.** The empirical literature on
   anticipation effects is sparse, noisy, and dominated by reforms with
   very specific institutional features (the 1986 ordinary-income cliff
   was a much bigger deal than the announced LTCG hikes the current
   model is supposed to score). Picking the wrong anchor could leave us
   either still too muted or wildly over-stating anticipation.
2. **Interaction with regime swaps.** The death-regime channels (deemed,
   carryover) already interact with the per-period MC. A more flexible
   `b'` shape might produce qualitatively different deemed-acceleration
   patterns. Worth checking against CBO Option 51 after recalibration.
3. **Functional-form lock-in.** Power form is a defensible choice but
   not the only one (CRRA-style, kinked, soft-plus all give similar
   degrees of freedom). The choice should be documented as a modeling
   decision rather than a derivation.
4. **Calibration cost.** Joint calibration over `(psi, alpha)` is more
   expensive than the current 1D bisection, but still cheap (maybe 50
   Bellman+bathtub evaluations).

---

## 5. Recommendation

Implement the power-form benefit as a swap-in extension, gated by
`KG_DYN_DEFAULT_ALPHA = 1` initially (which preserves current
quadratic behavior bit-for-bit). Once the code path is in place and
tested, recalibrate `psi` against the existing contemporaneous target
and `alpha` against the 2012-fiscal-cliff anticipation anchor. Compare
delayed-scenario `%ΔR` results to see whether the magnitudes come into
the expected 5-10% range for young cohorts.

If results look sensible, set the new `alpha` and `psi` as defaults
and update the results MD accordingly. If they don't, the gated
constant lets us revert cleanly to the quadratic baseline.

---

## Appendix A: Time-varying scenarios used as evidence

The two YAML scenarios that produced the evidence in Section 1:

`config/scenarios/tax_law/tests/kg_dyn_rate_up_5pp_delayed/pref.yaml`:
```yaml
rates:
  value:
    '2026': [0.0, 0.15, 0.20]
    '2027': [0.0, 0.15, 0.25]
```

`config/scenarios/tax_law/tests/kg_dyn_rate_up_5pp_temp/pref.yaml`:
```yaml
rates:
  value:
    '2026': [0.0, 0.15, 0.25]
    '2027': [0.0, 0.15, 0.20]
```

Runscript: `config/runscripts/tests/kg_dynamics_time_varying.csv` runs
both YAMLs under the new model (`kg_dynamics/turnover`) and under
legacy `kg/62`, plus the standard baseline.
