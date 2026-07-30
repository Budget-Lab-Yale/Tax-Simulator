# Metrics for the top-tax report

*Source vintages: dials `top_tax_dials_30y_v5` (current physics, run 2026-07-28: net-of-tax realization form eta_tilde=1.6625, sigma=0.16, on-model corporate rate to 35, uncapped CG rate, estate-avoidance fix, death-gain exclusion dial; deemed heir attribution is the full-ladder rank match, not comparable to the v3 smear), revmax grid `kg_v5_revmax` (same physics, direct death-regime grid to +30pp). All $ figures $B unless noted; windows FY2027-2036 and FY2027-2056.*

## 1. Income at the top, 2027 (baseline)

| Group | Cash (expanded) $B | Share of cash income | Accrual (Haig-Simons) $B | Share of accrual income | Accrual/cash |
|---|---|---|---|---|---|
| Quintile 5 | 15,022 | 66.9% | 22,424 | 69.4% | 1.49 |
| Top 10% | 11,531 | 51.3% | 17,881 | 55.3% | 1.55 |
| Top 5% | 8,960 | 39.9% | 13,983 | 43.3% | 1.56 |
| Top 1% | 5,212 | 23.2% | 7,751 | 24.0% | 1.49 |
| Top 0.1% | 2,520 | 11.2% | 3,426 | 10.6% | 1.36 |
| Top 0.01% | 1,194 | 5.3% | 1,512 | 4.7% | 1.27 |

- Total income, all tax units 2027: cash **$22.5T**, accrual **$32.3T** (shares are of these totals, which include the negative-income group).
- Top 1% cash income ($5,212B) equals the combined cash income of roughly the **bottom 66%** of tax units (interp within quintiles).
- On an accrual basis the top 1% ($7,751B) matches roughly the **bottom 71%**.

## 2. Baseline federal taxes and ETRs by group, 2027

(taxes included: iit + payroll + estate + deemed + wealth + corp + vat; corp convention: capital_income)

| Group | Taxes $B | ETR cash | ETR accrual | of which: income tax | payroll | estate | corp |
|---|---|---|---|---|---|---|---|
| Quintile 3 | 336 | 14.7% | 12.2% | 3.2% | 11.0% | 0.0% | 0.6% |
| Quintile 4 | 724 | 17.8% | 14.5% | 6.6% | 10.7% | 0.0% | 0.6% |
| Quintile 5 | 3,444 | 22.9% | 15.4% | 15.2% | 5.9% | 0.3% | 1.6% |
| Top 10% | 2,762 | 24.0% | 15.4% | 16.8% | 5.0% | 0.3% | 1.8% |
| Top 5% | 2,204 | 24.6% | 15.8% | 18.2% | 3.9% | 0.4% | 2.1% |
| Top 1% | 1,366 | 26.2% | 17.6% | 20.9% | 1.8% | 0.7% | 2.7% |
| Top 0.1% | 736 | 29.2% | 21.5% | 23.2% | 0.9% | 1.5% | 3.7% |
| Top 0.01% | 358 | 30.0% | 23.7% | 21.9% | 0.5% | 3.1% | 4.5% |

## 3. Group thresholds and 2036 levels (from distribution_etrs baseline columns)

| Group | 2027 income floor $ | 2027 cash income $B | 2036 cash income $B | 2027 taxes $B | 2036 taxes $B | n (2027, M) |
|---|---|---|---|---|---|---|
| Top 10% | 231,730 | 11,531 | 15,718 | 2,762 | 4,000 | 18.55 |
| Top 5% | 345,205 | 8,960 | 12,150 | 2,204 | 3,143 | 9.28 |
| Top 1% | 911,405 | 5,212 | 6,971 | 1,366 | 1,889 | 1.86 |
| Top 0.1% | 4,236,780 | 2,520 | 3,303 | 736 | 984 | 0.19 |
| Top 0.01% | 21,391,035 | 1,194 | 1,512 | 358 | 462 | 0.02 |

- Top 1% accrual income: 2027 **$7,751B**, 2036 **$10,994B**.

## 4. Stylized calculation: doubling the top 1% effective rate

- Top 1% baseline cash ETR 2027 (this tax set): **26.2%**; taxes 2027 $1,366B on $5,212B income.
- 10-year (FY2027-36) top 1% cash income, geometric interp 2027->2036: **$60.5T**; taxes: $16.1T.
- Doubling the ETR (26.2% -> 52.4%) with a FROZEN base adds revenue equal to baseline taxes: **+$16.1T over 10 years**.
- EXTERNAL yardstick: cumulative FY2027-2036 deficits **$24.4T** (CBO Feb 2026 baseline, via CRFB).
- So the frozen-base doubling closes **66%** of the 10-year deficit.
- Same calc on ACCRUAL ETR (2027 accrual ETR 17.6%): doubling that rate on accrual income is the same +$16.1T.

## 5. Standalone lever scores (direct runs, dials v3)

| Lever | 10y static | 10y conv | conv/static | 30y conv |
|---|---|---|---|---|
| Top ordinary rate 37% -> 39.6% | 483 | 415 | 0.86 | 2,079 |
| Top ordinary rate 37% -> 44.8% (REF) | 1,449 | 1,203 | 0.83 | 6,038 |
| Top ordinary rate 37% -> 50% | 2,416 | 1,937 | 0.80 | 9,734 |
| CG & div top rate 20% -> 25% | 547 | 239 | 0.44 | 1,097 |
| CG & div top rate 20% -> 30% | 1,095 | 393 | 0.36 | 1,825 |
| CG & div top rate 20% -> 35% | 1,642 | 462 | 0.28 | 2,191 |
| CG & div top rate 20% -> 40% (REF) | 2,190 | 454 | 0.21 | 2,222 |
| CG & div top rate 20% -> 45% | 2,738 | 376 | 0.14 | 1,943 |
| CG & div top rate 20% -> 50% | 3,286 | 240 | 0.07 | 1,405 |
| Corporate rate 21% -> 24.5% | 829 | 622 | 0.75 | 2,797 |
| Corporate rate 21% -> 28% (REF) | 1,659 | 1,198 | 0.72 | 5,377 |
| Corporate rate 21% -> 31.5% | 2,488 | 1,715 | 0.69 | 7,690 |
| Corporate rate 21% -> 35% | 3,318 | 2,171 | 0.65 | 9,719 |
| Wealth tax 1% > $50M | 2,944 | 2,044 | 0.69 | 10,042 |
| Wealth tax 2% > $50M (REF) | 5,888 | 3,266 | 0.55 | 15,575 |
| Carryover basis at death (no rate change) | 40 | 245 | 6.19 | 1,314 |
| Deemed realization at death (no rate change) | 291 | 685 | 2.36 | 3,113 |
| Estate tax 40->50%, exemption $15.4M->$8.46M (REF) | 317 | 313 | 0.99 | 1,581 |
| Estate tax 40->60%, exemption -> $5M | 722 | 686 | 0.95 | 3,462 |
| Repeal QBI (199A) | 896 | 881 | 0.98 | 3,884 |
| Eliminate SS taxable max | 2,520 | 2,504 | 0.99 | 11,366 |

## 6. Destination ledgers -- receipts-by-head deltas, FY2028-2036 conventional ($B)

(receipts.csv starts FY2028 in this vintage -- no CY2026 lead-in -- so these ledgers cover 9 of the 10 window years; use for composition/direction, not window totals)

| Scenario | income tax | payroll | corporate | estate | wealth | credits (outlay) | TOTAL |
|---|---|---|---|---|---|---|---|
| s_cg_r40 | 538 | 1 | -105 | -1 | 0 | 0 | 432 |
| s_cg_r25 | 252 | 0 | -31 | -1 | 0 | -0 | 221 |
| s_corp_r28 | 65 | 3 | 1,035 | -7 | 0 | -0 | 1,096 |
| s_wealth_r2_t50 | -451 | 1 | -43 | -87 | 3,879 | -0 | 3,299 |
| s_wealth_r1_t50 | -252 | 0 | -22 | -48 | 2,383 | -0 | 2,062 |
| s_deemed_deemed | 695 | 3 | -45 | -34 | 0 | -0 | 620 |
| s_ord_r44p8 | 1,014 | -6 | 117 | -3 | 0 | 0 | 1,122 |
| s_estate_r50_e8p46 | 47 | 0 | -2 | 262 | 0 | 0 | 308 |
| s_taxmax_on | -397 | 2,766 | -25 | -2 | 0 | 0 | 2,343 |
| stack_ref | 3,051 | 2,776 | 930 | 47 | 3,693 | -1 | 10,499 |

(Note: 'TOTAL' here = sum of revenue heads minus credit outlays; small differences vs revenue_estimates.csv reflect the FY booking conventions inside calc_receipts.)

## 7. Parts-vs-whole (naive sum vs actual package)

**Full 8-lever reference stack (stack_ref) vs sum of its 8 solos** (10y unless noted)

- Sum of standalone STATIC scores:        $15,211B
- Sum of standalone CONVENTIONAL scores:  $10,504B
- Package STATIC:                         $15,403B
- Package CONVENTIONAL (the truth):       $11,042B
- Package interaction (pkg conv - sum conv): $538B (+5.1% of the naive conventional sum)
- Behavioral survival: naive 69%, package 72%
- 30y: naive conv $49,156B vs package conv $49,731B (+1.2%)

**Corporate 21->35% + CG/div 20->30% (pc_corpr35_cgr30)** (10y unless noted)

- Sum of standalone STATIC scores:        $4,412B
- Sum of standalone CONVENTIONAL scores:  $2,564B
- Package STATIC:                         $4,412B
- Package CONVENTIONAL (the truth):       $2,514B
- Package interaction (pkg conv - sum conv): $-50B (-1.9% of the naive conventional sum)
- Behavioral survival: naive 58%, package 57%
- 30y: naive conv $11,545B vs package conv $11,308B (-2.1%)

**CG/div 20->40% + deemed realization at death (pr_cg_deemed)** (10y unless noted)

- Sum of standalone STATIC scores:        $2,481B
- Sum of standalone CONVENTIONAL scores:  $1,139B
- Package STATIC:                         $2,658B
- Package CONVENTIONAL (the truth):       $1,947B
- Package interaction (pkg conv - sum conv): $808B (+71.0% of the naive conventional sum)
- Behavioral survival: naive 46%, package 73%
- 30y: naive conv $5,335B vs package conv $9,036B (+69.4%)

**Draft Figure 3 as written -- corporate AND CG/div all to 25% (surrogate-style estimate; no direct run)**

- Sum of standalone static:       $1,495B  (cg25 547 + corp25 948, corp linear-scaled 4/7 of corp28)
- Sum of standalone conventional: $924B  (cg25 239 + corp25 684)
- Package conventional (surrogate: sum + g-scaled cg|corp interaction -5.5): **$918B**
- Surrogate validation context: quiz max err 2.5%, corners 2.4% -- treat as +/- a few percent.

**Draft Figure 4 -- CG/div 20->25% + deemed realization at death**

- Current-physics (dials v3, surrogate composition at cg=25):
  - Sum of standalone static:       $838B   (cg25 547 + deemed 291)
  - Sum of standalone conventional: $925B   (cg25 239 + deemed 685)
  - Package conventional estimate:  **$1,126B** (interaction 201.9)
- Direct runs (kg_v5_revmax, current physics), 10y:
  - Parts static:  cg+5pp 547 + deemed 291 = $838B
  - Parts conv:    cg+5pp 175 + deemed 677 = $851B
  - Package static $882B; package conv **$1,030B**

## 8. Capital-gains Laffer curves by death regime

### 8a. Current physics (dials v3, uncapped CG): step-up direct; deemed/carryover via surrogate composition

| Top CG rate | Step-up 10y conv | Deemed-conditional 10y conv delta* | Carryover-conditional* | Step-up 30y conv |
|---|---|---|---|---|
| 25% | 239 | 441 | 316 | 1,097 |
| 30% | 393 | 797 | 545 | 1,825 |
| 35% | 462 | 1,068 | 691 | 2,191 |
| 40% | 454 | 1,262 | 758 | 2,222 |
| 45% | 376 | 1,386 | 756 | 1,943 |
| 50% | 240 | 1,453 | 697 | 1,405 |

*Conditional columns = the CG-rate increase's own 10y conventional yield when the death regime is already in place (solo cg + g-scaled cg|deemed interaction residual; the regime's own revenue is NOT included). Surrogate-composed -- +/- a few percent.*

### 8b. Direct grid (kg_v5_revmax, current physics): total 10y conventional by cell

| Top CG rate | Step-up | Carryover | Deemed | Step-up static | Deemed static |
|---|---|---|---|---|---|
| 20% | 0 | 242 | 677 | 0 | 291 |
| 25% | 175 | 484 | 1,030 | 547 | 882 |
| 30% | 294 | 677 | 1,342 | 1,095 | 1,474 |
| 35% | 354 | 814 | 1,608 | 1,642 | 2,066 |
| 40% | 336 | 877 | 1,809 | 2,190 | 2,658 |
| 45% | 250 | 872 | 1,948 | 2,738 | 3,251 |

Leakage sign check (static minus conventional, 10y): positive = behavior loses revenue.
- cg+5pp under step-up: static 547, conv 175, leakage 372
- deemed alone: static 291, conv 677, leakage -386
- cg+5pp + deemed package: static 882, conv 1,030, leakage -148

## 9. 'Ask' vs 'collected' ETRs under the full reference stack (stack_ref, 2027)

(accrual = Haig-Simons denominator; cash = expanded income; taxes wealth_cit_vat; corp capital_income)

| Group | Baseline | Static 'ask' | Conventional 'collected' | Avoidance margin (pp) |
|---|---|---|---|---|
| **accrual denominator** | | | | |
| Quintile 5 | 15.4% | 21.1% | 19.7% | 1.4 |
| Top 1% | 17.6% | 31.2% | 27.6% | 3.7 |
| Top 0.1% | 21.5% | 42.7% | 36.3% | 6.4 |
| Top 0.01% | 23.7% | 49.8% | 41.7% | 8.1 |
| **cash denominator** | | | | |
| Quintile 5 | 22.9% | 31.4% | 29.3% | 2.1 |
| Top 1% | 26.2% | 46.3% | 40.9% | 5.4 |
| Top 0.1% | 29.2% | 57.8% | 49.2% | 8.6 |
| Top 0.01% | 30.0% | 62.9% | 52.6% | 10.3 |

## 10. Decade profile of the full stack

| Decade | Static $B | Conv $B | Survival | Conv % of GDP |
|---|---|---|---|---|
| 2027-2036 | 15,403 | 11,042 | 72% | 2.78% |
| 2037-2046 | 24,473 | 16,226 | 66% | 2.82% |
| 2047-2056 | 36,784 | 22,464 | 61% | 2.73% |

## 11. Realization-model color (kg_dynamics_summary)

- **s_cg_r40**: implied realization semi-elasticity mean -2.78 (range -3.12 to -2.64) deemed_realized 10y 0.0 (units as stored)
- **pr_cg_deemed**: implied realization semi-elasticity mean 0.84 (range 0.00 to 1.04) gains deemed-realized at death 10y 3,321B
- **stack_ref**: implied realization semi-elasticity mean 1.22 (range 0.47 to 1.38) gains deemed-realized at death 10y 3,306B

*(Interpretation note: the implied semi-elasticity is an OUTPUT here -- compare it across regimes to quantify 'policy chooses its own elasticity'.)*
