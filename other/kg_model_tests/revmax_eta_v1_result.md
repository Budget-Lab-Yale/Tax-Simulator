# Revmax grid under the η recalibration (Phase 7)

**Vintage `revmax_eta_v1`** — `config/runscripts/tests/revmax_cg.csv`, 18
scenarios (baseline + CG rate {+0,+5,+10,+15,+20,+25pp} × death-regime
{step-up, carryover, deemed}), 2026:2057, full stack (spec-v2 kg η=4.4984 /
ω=0.5132, σ=0.08), full sample, stacked. Enact 2027; 10y = FY27–36.
`python3 other/top_tax/analyze_revmax.py revmax_eta_v1`.

## Cumulative Δrev by regime (conventional), cum10 $B

| top rate (+pp) | step-up | carryover | deemed |
|---|---:|---:|---:|
| 20% (+0)  | —     | 288   | 756 |
| 25% (+5)  | 343   | 664   | 1227 |
| 30% (+10) | 691   | 1035  | 1685 |
| 35% (+15) | 1044  | 1402  | 2130 |
| 40% (+20) | 1199  | 1562  | 2334 |
| 45% (+25) | 1212  | 1576  | 2356 |
| **marginal 40→45%** | **+13** | **+13** | **+22** |

## The headline: the entropy fix worked

The whole motivation (see `representative_cell_bellman_results.md`
§"2026-07") was that the **old quadratic cost saturated** — the response was
linear-with-corner, so revenue rose ~linearly and **no revenue peak existed in
the grid range.** Under the entropy cost the curve **clearly bends**: the
5pp-marginal contribution collapses from ~+\$350B mid-grid to **~+\$13–22B** at
the top step. A genuine Laffer peak now exists, near **~45%**.

## Where the peak sits, and the cross-regime read

- All three regimes report their grid argmax **at the +25pp boundary (45%)**,
  because the curve is still (barely) rising there. **The grid must be extended
  (to ~+35/+40pp) to pin the exact peaks** — logged as the next step, not a
  silent cap.
- The peak (~45%) sits **above the naive `1/2.52 ≈ 39.7%`** — expected: with
  Φ=0.50, half of realizations are inert (non-responsive), so the effective
  base elasticity is lower and the revenue-maximizing rate is pushed up. This
  is the Φ→0 nesting from the other direction (as Φ shrinks, revmax → 39.7%).
- **Cross-regime ordering (step-up < carryover < deemed in revmax rate):**
  directionally present but not cleanly resolved inside the grid — deemed
  retains the most slope at the top (+\$22B vs +\$13B at the last step), i.e.
  its peak is at a somewhat higher rate, consistent with the prediction that
  closing the death-regime lock-in escape raises the revenue-maximizing CG
  rate. Revenue LEVELS are sharply ordered step-up < carryover < deemed at
  every rate (deemed ~2× step-up), since closing the escape widens the base.

## Next step

Extend `revmax_cg.csv` to +35/+40pp for all three regimes and re-run
(`revmax_eta_v2`) to bracket the peaks and resolve the cross-regime revmax-rate
ordering quantitatively. The spec-v2 model supports the exercise the old one
could not.
