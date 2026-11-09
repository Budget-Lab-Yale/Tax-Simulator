# Clausing-Sarin package: kg line-item revenue under the η recalibration

**Verification requested by user (2026-07-08).** Re-ran the Clausing-Sarin
central package (`clausing_v2.csv`, s=0.5, full sample, 2030:2040, stacked) on
the spec-v2 entropy calibration (η*=4.4984, ω*=0.5132) as vintage
`clausing_v2_s50_eta`, and compared the two kg-driven line items against the
pre-migration run `clausing_v2_s50` (old quadratic ψ, 2026-07-06).

Line item = marginal stacked contribution (layer cumulative − previous layer),
10-yr sum 2030–2039, $B.

## Headline

| line item | static (both) | conv OLD ψ | conv NEW η | Δ |
|---|---:|---:|---:|---:|
| **Carryover basis** (04−03) | 46 | 255 | **297** | +42 (+16%) |
| **Pref CG+div +5pp** (05−04) | 948 | 533 | **−1966** | −2499 |
| **package NET** (full stack) | — | 11,230 | **12,651** | +1,421 (+12.6%) |

- **Static is bit-for-bit invariant** old vs new for every layer — the recal
  only moves the conventional/behavioral channel, as designed.
- **Carryover line item behaves sensibly**: +16% conventional. The new
  calibration induces somewhat more death-realization, and the behavioral
  offset (conv vs static) rises from +449% to +539% — same sign/shape, modest
  amplification.
- **Package net is +$1.4T (+12.6%)** more conventional revenue.

## The pref-rate line item is a stacking-attribution artifact — READ WITH CARE

The −$1966B on the "CG+div rate +5pp" line looks alarming in isolation but is
**not** "raising CG rates loses \$2T." Layer-by-layer marginals (10-yr conv):

| layer (what it changes) | marg OLD | marg NEW |
|---|---:|---:|
| 01 clinton **ordinary** rates | 8,730 | **11,874** (+3,144) |
| 02 restore bottom rates | −3,957 | −3,956 |
| 03 repeal 199A | 1,160 | 1,890 |
| 04 carryover basis | 255 | 297 |
| 05 **pref CG+div** +5pp | 533 | **−1,966** |
| 06 NIIT reform | 359 | 362 |
| 07 estate | 534 | 533 |

Only layers **01** and **05** move materially; 02/03/04/06/07 are essentially
unchanged. The +$3.1T swing at the ordinary-rate layer and the −$2.0T swing at
the CG-rate layer are **two sides of the same amplified realization elasticity**
and largely offset in the net. The stronger η makes realizations far more
responsive to the ordinary-vs-CG rate **gap**: the ordinary-rate hikes (01)
widen it (realizations flood in), the +5pp CG layer (05) closes it (lock-in
reverses the flood). Because 05 is stacked *after* 01, it absorbs the entire
reversal, so its marginal goes deeply negative.

## Assessment / flag

- **Not a crash/NaN**: all totals finite, monotone; the mechanism is the
  intended constant-semi-elasticity response, just large.
- **Own-rate elasticity is correctly calibrated** (−2.52; see
  `eta_migration_regression.md`). What this package stress-tests is the
  **large-wedge / cross-layer extrapolation** of that elasticity, which the old
  quadratic form suppressed (it saturated) and the entropy form does not.
- **FLAG for review before scoring this package on spec v2:** the ±\$2–3T
  swings between the ordinary- and CG-rate layers are much larger than under ψ.
  The net (+\$1.4T) is the robust number; the per-layer attribution is now
  highly stacking-order-sensitive. Worth confirming the layer-01 induced-
  realization magnitude is intended (it is the flip side of the Phase-7 revmax
  behavior — the model can now sit on the far side of the CG Laffer curve).

Vintages: OLD `clausing_v2_s50`, NEW `clausing_v2_s50_eta` (both local v1 root).
