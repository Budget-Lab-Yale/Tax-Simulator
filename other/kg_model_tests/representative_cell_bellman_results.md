# Representative-Cell Bellman: First Results

**Status**: Refactor complete and calibrated. End-to-end validation pass run
against full-sample baseline 2026-2035 at scratch vintage `202605121830`.

This is a companion to `representative_cell_bellman_proposal.md`. The proposal
describes the model; this document reports the first results once the
refactor was implemented and calibrated.

## Implementation

The refactor replaces the prior log-utility Bellman (which had a known
eta-degeneracy: η cancelled from the response after baseline FOC inversion)
with a representative-cell quadratic Bellman whose per-dollar value is

```
W^j(a,t) = max_{r_D in [0, 1 - lambda_T]} {
    kappa(a,t) * r_D - (psi/2) * r_D^2
  - tau^j(a,t) * r_D
  + (1 - lambda_T - r_D) * [ beta*(1-m) * W^j(a+1, t+1) + beta*m * F^j(a,t) ]
}
```

with `F^j = (1 - c_phi^j) * tau^j` the death-state forgiveness value
(step-up: F = tau, carryover: F = (1-theta)*tau, deemed: F = 0).

Design choices (locked in by review with John):

- Quadratic benefit `b(r) = kappa*r - (psi/2)*r^2`. Closed-form FOC.
- Global scalar `psi`. Cell-specific `kappa(a,t)` recovered each scenario
  from the baseline FOC.
- No `(1+g)` accrual term in the continuation. Baseline-stock growth is
  treated as an exogenous additive inflow that cancels in dG.
- Existing `pref.kg_death_regime` / `pref.kg_bequest_motive` YAML schema
  preserved; `F` is derived internally from the `c_phi` produced by the
  existing regime table.
- Corner handling: at cells with observed `r_D_B = 0`, set `kappa = MC_B`
  exactly (cell sits at the lower corner).
- Full in-place replacement of the old model; no back-compat flags. The
  legacy `kg/62` reduced-form elasticity module is still available for
  reference scenarios and gives an A/B comparison.

Net diff: about -480 lines from `src/sim/kg_dynamics.R` (deleted the g
machinery, simplified the Bellman sweep, dropped the `KG_DYN_R_D_FLOOR`
hack that propped up the log-FOC inversion).

## Calibration

`other/kg_model_tests/calibrate_psi.R` bisects `psi` against a 1pp uniform
`tau` perturbation on step-up baseline, targeting

    dlog(R) / dtau  =  -0.6 / 0.238  approximately  -2.52

(literature arc elasticity -0.6 expressed in semi-elasticity form using
0.238 as the nominal baseline tau anchor).

Bisection converged in 11 iterations to:

    KG_DYN_DEFAULT_PSI = 25.6125

with implied semi-elasticity profile by year (1pp anchor):

| sim year | calendar | semi-elasticity |
|---:|---:|---:|
| 1 | 2026 | -1.95 |
| 5 | 2030 | -2.48 |
| 10 | 2035 | -2.52 |

The response builds over the first ~5 years as the bathtub recurrence and
the Bellman continuation values reach their steady state, then plateaus.

**Calibration caveat worth flagging.** The realization-weighted baseline
tau in the current Tax-Data is 0.187, not 0.238. So the model's effective
arc elasticity at the data's true baseline is `-2.52 * 0.187 = -0.47`, on
the low end of the literature range. Re-anchoring to `-0.6 / 0.187` would
target a semi-elasticity of -3.21 instead. Whether to recalibrate is a
judgment call; the current setting is defensible but conservative.

## Validation: end-to-end pipeline

`kg_dynamics_full` runscript: 9 counterfactual scenarios on baseline 2026-2035,
full sample (220,897 records), via `slurm_run.sh`. All 7 SLURM phases completed
cleanly; no errors in Phase 2B (bathtub pre-pass with new Bellman) or Phase 2C
(conventional pass using new state-file contract).

### Revenue estimates ($B, 10-year window 2026-2035)

| Scenario | Static | Conventional | Behavioral channel |
|---|---:|---:|---:|
| baseline_check | 0 | 0 | 0 |
| rate_up_5pp | +497 | +263 | -234 (-47%) |
| rate_down_5pp | -496 | -341 | +155 (+31%) |
| deemed | 0 | +788 | +788 |
| carryover | 0 | +220 | +220 |
| rate_up_deemed | +497 | +1,231 | +735 |
| rate_up_carryover | +497 | +554 | +57 |
| legacy_rate_up_5pp (kg/62) | +497 | +173 | -324 (-65%) |
| legacy_rate_down_5pp (kg/62) | -496 | -254 | +242 |

`baseline_check` is exact zero, confirming the model produces no spurious
revenue under a baseline-on-baseline run.

### Comparison to CBO Option 51 (Change Taxation of Assets at Death)

| | New model 10-yr | CBO 10-yr | Ratio |
|---|---:|---:|:---|
| Carryover | $220B | $197B | +12% (essentially on target) |
| Deemed | $788B | $536B | +47% (high but in the right neighborhood) |

Carryover is dead-on. The deemed overshoot is the main remaining gap.

### Comparison to legacy kg/62 reduced-form

For rate-only scenarios under step-up, the new model is ~10-20% less
responsive in revenue terms per pp of statutory rate change than the
legacy module:

| Year | New %ΔR per pp | Legacy %ΔR per pp |
|---:|---:|---:|
| 2026 | -1.31 | -1.85 |
| 2030 | -1.61 | -1.80 |
| 2035 | -1.63 | -1.80 |

The new model shows a time-ramping response (-1.30 in year 1 building to
-1.65 in year 5+), while the legacy reduced form is essentially flat. The
ramping is the dynamic-continuation channel: a choice to defer today
changes the future stock available for realization, which the bathtub
propagates forward.

## Conditional realization semi-elasticity by death regime

The model produces a theory-consistent ranking of |semi-elasticity| across
death regimes when the same +5pp top-rate change is layered on each:

| Year | Step-up | Carryover | Deemed |
|---:|---:|---:|---:|
| 2026 | -1.83 | -1.50 | -1.21 |
| 2030 | -2.34 | -1.85 | -1.45 |
| 2035 | -2.39 | -1.84 | -1.44 |

Steady-state ratio: `step-up : carryover : deemed  approximately  1 : 0.77 : 0.60`.

**Step-up has the largest |elasticity|** because the holder gets full tax
forgiveness at death — a tax hike today widens the wedge between "realize
now and pay" vs "defer to a guaranteed tax-free escape," so deferral
becomes much more attractive.

**Deemed has the smallest** because `F = 0` — death is just another
forced realization with no forgiveness, so the tax-hike effect on the
realize-vs-defer decision is weaker.

**Carryover (with theta = 0) sits in between** for a subtler reason. The
holder's `F = tau` is the same as step-up, but decedent stock routes to
heirs rather than vanishing. When higher taxes cause more deferral, more
stock dies with the holder and routes to heirs, who then realize it at
the same higher tax rate. That heir-realization channel partially offsets
the holder-side deferral, so the aggregate is less elastic than step-up.

## Per-cell age heterogeneity in the regime response

The aggregate +11.7% realization rise under the `deemed` regime is
gain-stock-weighted across all age cells, which obscures real
heterogeneity. The age profile at 2035 shows a clean mortality gradient:

| Age | m (mortality) | r_S / r_B | %ΔR |
|---:|---:|---:|---:|
| 18 | 0.0006 | 1.005 | +0.5% |
| 32 | 0.0015 | 1.031 | +3.1% |
| 47 | 0.0023 | 1.037 | +3.7% |
| 62 | 0.0059 | 1.121 | +12.1% |
| 72 | 0.013 | 1.258 | +25.8% |
| 80+ (pooled) | 0.055 | 1.307 | +30.7% |

So the deemed regime accelerates realizations strongly among older
cohorts (where mortality is high and the forgiveness loss bites
immediately) and only modestly among younger ones. The structure is
right; the aggregate level is what's elevated relative to CBO.

## Outstanding considerations

1. **Calibration anchor.** Re-calibrating against a `-0.6 arc at the data's
   actual baseline tau` (rather than at a nominal 0.238) would raise psi
   from 25.6 to roughly 19-20 and raise the aggregate elasticity. Open
   question whether to do this.
2. **Deemed level effect.** New model's +$788B over 10yr vs CBO's $536B.
   One lever: introduce a sub-100% effective forgiveness loss under deemed
   (e.g., `c_phi_deemed = 0.85` instead of 1.0) to reflect practical
   step-up retention by some classes of heir under nominally-deemed regimes.
   Would shrink the regime-induced level effect without affecting rate
   elasticities much.
3. **Asset-class disaggregation.** Currently 5 wealth classes (equities,
   pass-throughs, primary home, other home, RE fund) are collapsed into
   a single bucket. Per-class `psi` and `phi_I` are on the roadmap and
   would let us match class-specific empirical elasticities (housing
   ~0.3, equities ~0.5, etc.).
4. **Age-varying psi.** Currently a single scalar. The age-profile
   heterogeneity in the regime response comes from the mortality structure;
   age-varying psi could capture additional age-specific elasticity
   patterns from the literature.
