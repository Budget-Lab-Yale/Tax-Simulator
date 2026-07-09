# Representative-Cell Bellman: First Results

> **SUPERSEDED (2026-07, spec v2).** The quadratic realization cost
> `(psi/2)*r_D^2` and the flat `(phi_I, planned_share)` bucket pair described
> below were replaced by an **entropy (KL / Bregman) realization cost** and a
> **nested `(Phi, omega)` bucket reparameterization**. See the
> **"2026-07: entropy/log cost (spec v2)"** section at the bottom of this file
> for the current formulation. The formula block, the `psi` bullets, and every
> calibrated table below reflect the OLD quadratic model and are retained for
> history only — do not cite `psi` / `planned_share` numbers from them.

**Status (v1, historical)**: Refactor complete, calibrated, and validated
end-to-end. Two review-driven corrections applied (gain-weighted Bellman
mortality; realistic r_B in the 81+ Bellman tail). Calibration anchor moved to
sim year 30 for a permanent-response interpretation.

Companion to `representative_cell_bellman_proposal.md`. The proposal
describes the model; this document reports results from the first
implementation pass and a second pass after a design review.

## Implementation

The refactor replaces the prior log-utility Bellman (which had a known
eta-degeneracy: η cancelled from the response after baseline FOC inversion)
with a representative-cell quadratic Bellman whose per-dollar value is

```
W^j(a,t) = max_{r_D in [0, 1 - lambda_T]} {
    kappa(a,t) * r_D - (psi/2) * r_D^2
  - tau^j(a,t) * r_D
  + (1 - lambda_T - r_D) * [ beta*(1-m_gw) * W^j(a+1, t+1) + beta*m_gw * F^j(a,t) ]
}
```

with `F^j = (1 - c_phi^j) * tau^j` the death-state forgiveness value
(step-up: F = tau, carryover: F = (1-theta)*tau, deemed: F = 0), and
`m_gw` the **gain-stock-weighted** cell mortality
`m_gw = sum(w*m_household*G_unit) / sum(w*G_unit)`.

Design choices (locked in by review):

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
- **Gain-weighted Bellman mortality** (instead of taxpayer-count m).
  The Bellman is normalized per dollar of unrealized gain, so the
  mortality input must be the probability that the *dollar's* holder
  dies, not the average taxpayer. Wealthier holders die less, so
  taxpayer-weighted m is biased high by 2-3x in practice. The bathtub
  recurrence already adopts this convention via `m_eff = mG_record/G_B`;
  the Bellman is now consistent.
- **Realistic r_B in the 81+ Bellman tail.** Past the bathtub topcode
  at age 80, the extended grid sets `r_B(a) = r_B(80)` — the
  empirically observed topcode-pool rate, held flat through age 119.
  Previously these ages had `r_B = 0`, making the Bellman's
  continuation value at age 80 purely death-driven and over-stating
  regime-induced acceleration in older cohorts.
- Full in-place replacement of the old model; no back-compat flags. The
  legacy `kg/62` reduced-form elasticity module is still available for
  reference scenarios and provides an A/B comparison.

## Calibration

`other/kg_model_tests/calibrate_psi.R` bisects `psi` against a 1pp
uniform `tau` perturbation on step-up baseline, targeting

    dlog(R) / dtau  =  -0.6 / 0.238  ~  -2.52

semi-elasticity wrt tau, matching the convention used by the legacy
reduced-form module `kg/62.R` (literature arc elasticity `-0.62` at a
baseline tau of `0.238`).

**Anchor: sim year 30** (permanent-response steady state). The
realization response ramps over the first ~10 years as the bathtub
accumulates stock pressure, then plateaus. Anchoring at year 30
avoids locking the calibration to a transient. The calibration
script fails fast if the baseline has fewer than 30 sim years; the
production calibration runs against `tests/kg_dynamics_baseline_30yr.csv`.

Bisection converges in ~13 iterations to:

    KG_DYN_DEFAULT_PSI = 23.1078

with implied semi-elasticity profile by sim year (1pp anchor):

| sim year | calendar | semi-elasticity |
|---:|---:|---:|
| 1 | 2026 | -2.00 |
| 5 | 2030 | -2.55 |
| 10 | 2035 | -2.59 |
| 20 | 2045 | -2.52 |
| 30 | 2055 | -2.52 |

## Validation: end-to-end pipeline

`kg_dynamics_full` runscript: 9 counterfactual scenarios on baseline
2026-2035, full sample (220,897 records), via `slurm_run.sh`. All 7
SLURM phases completed cleanly; no errors in Phase 2B (bathtub pre-pass
with new Bellman) or Phase 2C (conventional pass using new state-file
contract).

### Revenue estimates ($B, 10-year window 2026-2035)

| Scenario | Static | Conventional | Behavioral channel |
|---|---:|---:|---:|
| baseline_check | 0 | 0 | 0 |
| rate_up_5pp | +497 | +254 | -243 (-49%) |
| rate_down_5pp | -496 | -336 | +160 (+32%) |
| deemed | 0 | +763 | +763 |
| carryover | 0 | +207 | +207 |
| rate_up_deemed | +497 | +1,187 | +690 |
| rate_up_carryover | +497 | +527 | +30 |
| legacy_rate_up_5pp (kg/62) | +497 | +173 | -324 (-65%) |
| legacy_rate_down_5pp (kg/62) | -496 | -254 | +242 |

`baseline_check` is exact zero, confirming the model produces no spurious
revenue under a baseline-on-baseline run.

### Comparison to CBO Option 51 (Change Taxation of Assets at Death)

| | New model 10-yr | CBO 10-yr | Ratio |
|---|---:|---:|:---|
| Carryover | $207B | $197B | +5% (essentially a match) |
| Deemed | $763B | $536B | +42% (improved but still high) |

Carryover lands within 5% of CBO. Deemed is still elevated — driven
mostly by the topcode 80+ pool (+30% rate factor under deemed, since
that pool holds the largest share of gain stock and the loss of step-up
forgiveness bites immediately for high-mortality cohorts). The two
mortality/tail corrections moved deemed from $788B to $763B; further
movement likely needs the levers listed under Outstanding Considerations.

### Comparison to legacy kg/62 reduced-form

For rate-only scenarios under step-up, the new model is roughly 10-20%
less responsive in revenue terms per pp of statutory rate change than
the legacy module:

| Year | New %ΔR per pp | Legacy %ΔR per pp |
|---:|---:|---:|
| 2026 | -1.26 | -1.85 |
| 2030 | -1.55 | -1.80 |
| 2035 | -1.57 | -1.80 |

The new model shows a time-ramping response (-1.26 in year 1 building
to -1.57 by year 10), while the legacy reduced form is essentially flat.
The ramping is the bathtub accumulating policy-induced stock pressure
over time; the per-cell Bellman is forward-looking and solved
immediately, but the aggregate revenue evolves as `dG` grows.

## Conditional realization semi-elasticity by death regime

The model produces a theory-consistent ranking of |semi-elasticity| across
death regimes when the same +5pp top-rate change is layered on each:

| Year | Step-up | Carryover | Deemed |
|---:|---:|---:|---:|
| 2026 | -1.91 | -1.60 | -1.33 |
| 2030 | -2.44 | -1.97 | -1.59 |
| 2035 | -2.48 | -1.96 | -1.57 |

Year-10 ratio: `step-up : carryover : deemed  ~  1 : 0.79 : 0.63`.

**Step-up has the largest |elasticity|** because the holder gets full tax
forgiveness at death — a tax hike today widens the wedge between "realize
now and pay" vs "defer to a guaranteed tax-free escape," so deferral
becomes much more attractive.

**Deemed has the smallest** because `F = 0` — death is just another
forced realization with no forgiveness, so the tax-hike effect on the
realize-vs-defer decision is weaker.

**Carryover (with the baseline theta = 0.5) sits in between** because the
holder's `F = (1 - 0.5) * tau = 0.5 * tau` is exactly half-way between
step-up's and deemed's. The full ranking is just an arithmetic consequence
of the F values: step-up `F = tau`, carryover `F = 0.5 * tau`, deemed
`F = 0`. Larger F means a larger reward for deferring to death, which
amplifies the response to tax changes today. The bathtub also routes
decedent stock to heir cells under carryover (rather than letting it
vanish as under step-up), which adds an heir-realization channel to the
aggregate, but the holder-side F value is the dominant force in the
conditional-elasticity ranking. Re-running carryover with theta = 0 (full
step-up valuation of the heir burden) would bump |elasticity| up close to
step-up's; theta = 1 (full deemed valuation) would push it close to
deemed's. The default 0.5 is a middle-ground placeholder; empirical
theta is not well-pinned-down in the literature.

## Per-cell age heterogeneity in the regime response

The aggregate +11.7% realization rise under the `deemed` regime is
gain-stock-weighted across all age cells. The cell-level response varies
strongly with mortality. Age profile at 2035 under deemed (m is the
taxpayer-weighted mortality reported in cell_table; the Bellman
internally uses the gain-weighted version, which is somewhat lower):

| Age | m | r_S / r_B | %ΔR |
|---:|---:|---:|---:|
| 20 | 0.0008 | 1.009 | +0.9% |
| 40 | 0.0019 | 1.023 | +2.3% |
| 60 | 0.0053 | 1.081 | +8.1% |
| 67 | 0.0087 | 1.118 | +11.8% |
| 72 | 0.013 | 1.238 | +23.8% |
| 80+ (pooled) | 0.055 | 1.301 | +30.1% |

So the deemed regime accelerates realizations strongly among older
cohorts (where mortality is high and the forgiveness loss bites
immediately) and only modestly among younger ones. The structure is
right; the aggregate level is what remains elevated relative to CBO.

## Outstanding considerations

1. **Deemed level effect.** New model's +$763B over 10yr vs CBO's $536B.
   The most plausible single lever is **`c_phi_deemed < 1`** to reflect
   practical step-up retention by surviving spouses, charitable bequests,
   family valuation gaming, etc. (e.g., `c_phi_deemed = 0.85` instead of
   1.0). Would shrink the regime-induced level effect without affecting
   rate elasticities much. Not yet explored.

2. **Asset-class disaggregation.** Currently 5 wealth classes (equities,
   pass-throughs, primary home, other home, RE fund) are collapsed into
   a single bucket. Per-class `psi` and `phi_I` are on the roadmap and
   would let us match class-specific empirical elasticities (housing
   ~0.3, equities ~0.5, etc.). Likely meaningful for the deemed-on-housing
   subset of the response.

3. **Age-varying psi.** Currently a single scalar. The age-profile
   heterogeneity in the regime response comes entirely from the mortality
   and gain-stock structure; age-varying psi could capture additional
   age-specific elasticity patterns from the literature (e.g., retirees
   with stable consumption have lower realization elasticity than working-
   age portfolio rebalancers).

4. **Test harness.** No unit tests yet for the Bellman primitives, the
   bathtub recurrence mass balance, the allocator sums, or the
   death-regime F ordering. Worth adding before the next round of
   model-level changes.

5. **Heir flow refinement.** The Gaussian age-shift `omega` matrix
   (decedent gains routed to ages centered at a-30 with sd=5) is a
   placeholder until the estate module hookup. Real heir age
   distributions vary substantially by decedent age and asset class.

## Change log

- **v1 (commit f5906fc75)**: Initial refactor. psi=25.6125 calibrated at
  sim year 10 of a 10-year baseline. Taxpayer-weighted Bellman mortality.
  Zero r_B in 81-119 tail. Carryover at +12% of CBO; deemed at +47%.
- **v2 (this commit)**: Gain-weighted Bellman mortality; r_B(80) held
  flat in 81+ tail; calibration anchored at sim year 30 of a 30-year
  baseline; psi recalibrated to 23.1078. Carryover at +5% of CBO; deemed
  at +42%. Doc convention clarified to "semi-elast wrt tau" matching
  legacy kg/62. Carryover theta default (0.5) noted explicitly.

---

## 2026-07: entropy/log cost (spec v2)

**This section is current; everything above is the superseded quadratic
model (spec v1).**

### Why

The revmax grid (vintage 202607081937) showed the CG revenue response
*saturating*: the quadratic cost `(psi/2)*r_D^2` gives a linear-with-hard-corner
control, and only the ~34% Bellman-controlled bucket ever responds long-run, so
no revenue-maximizing rate exists in the grid range. Root causes: (a) the fixed
share `phi_I = 0.4` was a 2026-05-06 judgment call, never calibrated; (b) the
local elasticity calibration cannot pin the *global* curvature that determines
revmax; (c) the naive scorekeeper "revmax ~= 40%" is exactly the
constant-semi-elasticity special case.

### The cost function

Replace the quadratic with the **entropy (KL / Bregman) cost** anchored at the
cell's baseline discretionary rate `r_D_B`:

```
C(r_D; r_D_B) = (1/eta) * [ r_D * ln(r_D / r_D_B) - r_D + r_D_B ]
```

so `C'(r_D) = (1/eta) * ln(r_D / r_D_B)` and `C'(r_D_B) = 0` exactly.

- **Pass 1** (baseline inversion): `C'(r_D_B) = 0` => `kappa = MC_B` EXACTLY
  (interior AND corner). No `psi * r_D_B` premium term. `W_B = (MC_B - tau_B) *
  r_D_B + remaining * death_cont`.
- **Pass 2** (scenario FOC): the closed form
  `r_D_S = r_D_B * exp(-eta * (MC_S - MC_B))`, clipped to `[0, r_D_cap]`. Only
  the upper clip can bind; `r_D_B = 0` cells (exactly the `r_B = 0` cells) stay
  0. The response argument is the FULL wedge `Delta MC` (tau + continuation
  `W'` + death-forgiveness `F`), so carryover/deemed regime changes and
  anticipation are all priced with the SAME `eta`.

This makes the discretionary response **globally constant-semi-elasticity in
the structural wedge**: `dln(r_D)/dMC = -eta` everywhere. Consequences:
- The model hits the literature elasticity under current-law step-up **by
  construction** (eta is calibrated against the tau-moment).
- It **endogenously** implies lower rate-elasticities under carryover/deemed
  (the wedge is amplified by the continuation/forgiveness terms).
- It **nests the naive CBO/JCT revmax arithmetic as its `Phi -> 0` limit**:
  with a constant semi-elasticity `s`, the revenue-maximizing rate is `1/|s|`
  (~= 0.397 for `s = -2.52`). Verified end-to-end in
  `test_naive_limit.R` (argmax within 1/2.52 +/- 0.05).

### Nested bucket reparameterization

The flat `(phi_I, planned_share)` pair becomes two primitives:

- `Phi` (`KG_DYN_SHARE_INERT`, env `KG_SHARE_INERT`, **central 0.50** — SSZZ
  TPE-36: ~50% of realizations untimeable): the INERT share, outside the
  discretionary Bellman bucket. The ordinary bucket is `1 - Phi`.
- `omega` (`KG_DYN_TIMEABLE_FRAC`, env `KG_TIMEABLE_FRAC`, **calibrated**): of
  the inert share, the mechanically-timeable fraction.

Derived (all downstream consumers unchanged): `phi_I = Phi*(1 - omega)`,
`planned_share = Phi*omega`. Because `phi_I + planned_share = Phi` independent
of `omega`, `r_exog_B = Phi * r_B` is `omega`-invariant, so the long-run moment
is `omega`-invariant and `eta`/`omega` identify nearly independently (the
calibrator's inner `eta` loop collapses to ~1 iteration after the first outer
`omega` step — a health signal).

The **frozen mechanical pass is Phi-invariant** (`phi_I` feeds only the
`lambda_I` / `r_V_*` diagnostics in `kg_dyn_step_recurrence`; `delta_next` and
all mech cell_table columns are independent of it) => pre/post mech state is
byte-identical across the migration (a regression assertion).

### Code / provenance

`KG_DYN_SPEC_VERSION = 2L`. `KG_DYN_DEFAULT_ETA` replaces `KG_DYN_DEFAULT_PSI`
(NA-guarded until the calibration paste; the finite-eta guard hard-stops an
uncalibrated sim). `KG_DYN_CALIB_PROVENANCE` now stamps
`eta / timeable_frac / share_inert`. Env knobs `KG_PSI` / `KG_SHARE_PLANNED`
are removed; `KG_ETA` / `KG_SHARE_INERT` / `KG_TIMEABLE_FRAC` replace them.
Calibrated by `calibrate.R` (outer bisection over `omega`, inner over `eta`
with the DECREASING-in-eta direction flip); dilution loop / regression memo in
`eta_migration_regression.md`.
