# Default financing-profile calibration: the `s(age, nw_pctile)` surface

**Date:** 2026-07-07
**Applies to:** `config/wealth/profiles/default/s.csv` (auto-applied to every
scenario that names no `wealth_financing` profile and sets no scalar `s`)
**Generator:** `s_default()` in `other/wealth_dynamics/write_profiles.py`
**Author decisions:** persistent-flow anchor + full age × wealth surface
(chosen over transitory-MPC anchor and wealth-only gradient, 2026-07-07).

## 1. Concept

The wealth bathtub applies `s = 1 − MPC` to **each year's** above-baseline
during-life tax flow. Reforms scored through this channel are typically
permanent, so from year 2 onward the household has fully internalized the tax
change — the relevant behavioral object is the share of a **persistent** net
income change financed out of saving rather than consumption. This is NOT the
one-year MPC out of a transitory windfall (lottery/stimulus literature), which
is conceptually right only for the first year and roughly 2× smaller at the
top. The transitory gradient is used only as a shape cross-check.

## 2. Bridge formula (wealth-rank gradient)

For a persistent flow change `dY`, local approximation:

```
dC = ε · (C/Y) · dY        =>        s(p) = 1 − ε · (C/Y)(p)
```

- `ε ≈ 0.7` — cross-sectional elasticity of consumption to permanent income;
  Straub (2019), *Consumption, Savings, and the Distribution of Permanent
  Income* ("estimates suggest an elasticity of 0.7, soundly rejecting
  linearity"). ε < 1 is exactly the statement that the rich do not adjust
  consumption one-for-one with permanent income.
- `(C/Y)(p)` — consumption-to-income ratio by rank:
  - **Top 1%:** income share ~20% vs consumption share 6–7% (Mian–Straub–Sufi
    2021, *The Saving Glut of the Rich*) ⇒ C/Y ≈ 0.30 ⇒ **s ≈ 0.80**
  - **P90–99:** C/Y ≈ 0.65–0.75 ⇒ **s ≈ 0.50–0.65**
  - **Middle:** C/Y ≈ 0.9–1.0 ⇒ bridge gives s ≈ 0.3; the hand-to-mouth mix
    (Kaplan–Violante) pulls the realized value to **s ≈ 0.2**
  - **Bottom (positive-NW only; ≤0-NW records are s_eff = 0 by design, D17):**
    constrained mix dominates ⇒ **s ≈ 0.10**

Shape cross-checks (both confirm a steep positive gradient):
- Transitory MPC by deposit quartile 0.44 / 0.42 / 0.34 / 0.22
  (Fagereng–Holm–Natvik 2021, AEJ:Macro) — i.e., transitory `s` runs ~0.55 →
  0.8 in *liquidity*; our persistent-flow values sit below that at the bottom
  (constraints bind harder for persistent losses) and above at the top.
- Saving rates rise steeply in lifetime income (Dynan–Skinner–Zeldes 2004,
  JPE; cells verified against the full text of NBER WP 7906, 2026-07-07). SCF
  median saving rates, age 40–49: Q1 −.015 → Q5 .265, top 5% .368, **top 1%
  .494** on current income (Table 3); instrumented for permanent income the
  top-1% estimate is stable at **.496** (vehicles instrument, Table 4) and
  **.502** (lagged income, Table 5) — "~half of income" for the top 1% across
  all three specifications, vs ~0–3% for the bottom quintile.

**Base nodes** (piecewise-linear in within-age net-worth percentile):

| p | 1 | 30 | 50 | 70 | 85 | 90 | 95 | 99 | 100 |
|---|---|----|----|----|----|----|----|----|-----|
| s | 0.10 | 0.14 | 0.20 | 0.28 | 0.40 | 0.46 | 0.55 | 0.65 | 0.80 |

## 3. Age tilt

Additive tilt, piecewise-linear in age, **attenuated to zero at top wealth
ranks** (full tilt through P90, linearly to 0 at P100):

| age | 18 | 30 | 40 | 50 | 62 | 70 | 80 |
|-----|----|----|----|----|----|----|----|
| tilt | −0.06 | −0.05 | 0.00 | +0.03 | +0.03 | −0.03 | −0.05 |

- **Young (<40):** more likely liquidity-constrained conditional on rank ⇒
  lower s.
- **Peak earners (45–64):** accumulation phase ⇒ highest s.
- **Retirees (65+):** decumulation phase for ordinary ranks ⇒ lower s
  (directionally consistent with the rising-in-age transitory MPC in
  Fagereng–Holm–Natvik).
- **Attenuation:** De Nardi–French–Jones (2010, JPE, *Why Do the Elderly
  Save?*): high-permanent-income elderly do **not** run down wealth with age
  (bequest motives + medical-expense risk), so the retiree tilt must vanish at
  the top — implemented as tilt × atten(p), atten = 1 through p = 90, → 0 at
  p = 100. Age is second-order exactly where the forcing dollars concentrate.
  Independently confirmed by DSZ Table 7 (age 70–79): saving rates of
  high-income elderly stay high (SCF top-1% ≈ .448) — "there is no evidence
  that older high lifetime income households dissave at a faster rate...
  if anything they may continue to save more."

## 4. Resulting surface (spot values)

```
age\p     1     25     50     75     90     95     99    100
  18  0.040  0.073  0.140  0.260  0.400  0.520  0.644  0.800
  30  0.050  0.083  0.150  0.270  0.410  0.525  0.645  0.800
  40  0.100  0.133  0.200  0.320  0.460  0.550  0.650  0.800
  55  0.130  0.163  0.230  0.350  0.490  0.565  0.653  0.800
  70  0.070  0.103  0.170  0.290  0.430  0.535  0.647  0.800
  80  0.050  0.083  0.150  0.270  0.410  0.525  0.645  0.800
```

Checks: 6,300 cells (18–80 × 100), s ∈ [0.04, 0.80], monotone in p at every
age, tilt fully attenuated at p = 100. Unweighted mean s = 0.237; the
**dollar-weighted effective s for top-concentrated reforms (capital gains,
wealth, estate) is ≈ 0.6–0.8**, consistent with the bounding sweep's finding
of ~−$22B per unit-s over 10y for a CG+5pp-style reform — i.e., expect that
sweep's s ≈ 0.7 corner to approximate this profile for such reforms.

## 5. Operational consequences

1. **The channel is now ON model-wide.** Any scenario without
   `wealth_financing = none` (or scalar `s = 0`) activates the split-pass
   machinery: SLURM phases 2N/2W run, ~2× conventional-side compute.
2. Conventional estimates for any reform touching during-life taxes now
   include the wealth-financing drain into estate/capital-income bases. Static
   estimates are unchanged (channel is conventional-only by design, D20/D22).
3. `M.csv` remains identity (full persistence — the realistic near-truth per
   the s×M bounding: M was second-order, ≤±$2B/10y).
4. Prior-vintage conventional results for wealth-adjacent reforms are not
   comparable to post-calibration runs.

## 6. Caveats / future work

- **Symmetric s** (v1 simplification, D18): same share for hikes and cuts.
  Direction-split requires a direction-conditional inflow, not a magnitude
  knob.
- **No time dimension:** one static surface; year-1 responses are closer to
  the (smaller) transitory MPC, so the first year's drain is modestly
  overstated at the top.
- **Rank basis:** the C/Y anchors are income-distribution shares mapped onto
  within-age net-worth ranks; the correlation is strong but not 1 — treat
  band edges as soft.
- All three quantitative anchors verified 2026-07-07: Straub ε ≈ 0.7, MSS
  top-1% shares, and DSZ table cells (against the full NBER WP 7906 text).

## 7. Validation run (2026-07-07, vintage `wealth_default_val`)

Full-sample SLURM run (jobs 17234741/17234762–72, 255 tasks, all COMPLETED),
`config/runscripts/tests/wealth_default_validation.csv`, 2026:2036: three arms
per reform — `s = 0` (off), `s = 0.5` flat (old bounding corner), blank
(→ calibrated default) — for CG+5pp (kg_dynamics composition) and the Warren
wealth tax, plus `estate2009_default` as the channel-on/zero-forcing control.
Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/wealth_default_val/`.

**All checks pass:**

1. **Dormancy / control exact:** conv − static estate = 0.000 every year for
   both `_off` arms AND for `estate2009_default` (profile active, estate tax
   not in the during-life forcing → zero drain, nothing else perturbed).
2. **Static invariance:** static 10-year totals byte-identical across arms
   within each reform (867.3 cg5 / 5,384.8 warren, $B FY2027–36).
3. **Effective s in the predicted band:** estate-drain ratio default/flat50
   × 0.5 ⇒ dollar-weighted effective s ≈ **0.78** for both reforms (predicted
   0.6–0.8; top-concentrated forcing lands at the top of the band).
4. **10-year conventional channel effect** (vs `_off`, FY2027–36, $B):

   | arm | Δincome | Δestate | Δwealth-tax | Δtotal |
   |---|---|---|---|---|
   | cg5_flat50 | −11.4 | −0.3 | — | **−11.9** |
   | cg5_default | −11.1 | −0.4 | — | **−11.6** |
   | warren_flat50 | −52.7 | −9.9 | −73.2 | **−136.0** |
   | warren_default | −83.2 | −15.5 | −115.3 | **−214.3** |

   Warren self-erosion under the calibrated default ≈ **6.2%** of the
   conventional score. For cg5 the calibrated profile shifts the interaction
   toward the estate margin and away from the during-life capital-income
   margin relative to flat-0.5 (drain concentrated in top cells: higher
   estate exposure, lower taxable yield per dollar of wealth) — the total is
   a near-wash there.
5. **No clamp binds** (`fmax` never hit) and no new warnings — only the two
   known flags (kg calib staleness vs Tax-Data 2026060918; the 935-record
   Estate-Tax-Distribution 2026 heir-file gap).

## Sources

- Straub (2019), *Consumption, Savings, and the Distribution of Permanent
  Income* — https://straub.scholars.harvard.edu/publications/consumption-savings-and-distribution-permanent-income
- Mian, Straub, Sufi (2021), *The Saving Glut of the Rich* — https://www.nber.org/papers/w26941
- Fagereng, Holm, Natvik (2021), *MPC Heterogeneity and Household Balance
  Sheets*, AEJ:Macro — https://www.aeaweb.org/articles?id=10.1257/mac.20190211
- Dynan, Skinner, Zeldes (2004), *Do the Rich Save More?*, JPE —
  https://www.nber.org/papers/w7906
- De Nardi, French, Jones (2010), *Why Do the Elderly Save? The Role of
  Medical Expenses*, JPE — https://www.nber.org/papers/w15149
- Kaplan, Violante (2022), *The Marginal Propensity to Consume in
  Heterogeneous Agent Models* — transitory-MPC benchmark ≈ 0.5 annual.
