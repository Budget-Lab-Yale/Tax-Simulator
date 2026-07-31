# Metrics for the top-tax report

*Source vintages: dials `top_tax_dials_30y_v6` (current physics, run 2026-07-30: net-of-tax realization form eta_tilde=1.6625, sigma=0.16, on-model corporate rate to 35, uncapped CG rate, estate-avoidance fix, death-gain exclusion dial, income conversion computed once in the pre-pass; deemed heir attribution is the full-ladder rank match, not comparable to the v3 smear), revmax grid `kg_v6_revmax` (same physics, direct death-regime grid to +30pp). All $ figures $B unless noted; windows FY2027-2036 and FY2027-2056.*

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
| Top ordinary rate 37% -> 39.6% | 483 | 415 | 0.86 | 2,082 |
| Top ordinary rate 37% -> 44.8% (REF) | 1,449 | 1,203 | 0.83 | 6,040 |
| Top ordinary rate 37% -> 50% | 2,416 | 1,938 | 0.80 | 9,734 |
| CG & div top rate 20% -> 25% | 547 | 245 | 0.45 | 1,121 |
| CG & div top rate 20% -> 30% | 1,095 | 405 | 0.37 | 1,875 |
| CG & div top rate 20% -> 35% | 1,642 | 483 | 0.29 | 2,284 |
| CG & div top rate 20% -> 40% (REF) | 2,190 | 484 | 0.22 | 2,364 |
| CG & div top rate 20% -> 45% | 2,738 | 416 | 0.15 | 2,141 |
| CG & div top rate 20% -> 50% | 3,286 | 290 | 0.09 | 1,660 |
| Corporate rate 21% -> 24.5% | 829 | 623 | 0.75 | 2,802 |
| Corporate rate 21% -> 28% (REF) | 1,659 | 1,200 | 0.72 | 5,386 |
| Corporate rate 21% -> 31.5% | 2,488 | 1,718 | 0.69 | 7,701 |
| Corporate rate 21% -> 35% | 3,318 | 2,174 | 0.66 | 9,733 |
| Wealth tax 1% > $50M | 2,944 | 2,043 | 0.69 | 10,067 |
| Wealth tax 2% > $50M (REF) | 5,888 | 3,268 | 0.56 | 15,703 |
| Carryover basis at death (no rate change) | 40 | 238 | 5.99 | 1,275 |
| Deemed realization at death (no rate change) | 291 | 679 | 2.33 | 3,088 |
| Estate tax 40->50%, exemption $15.4M->$8.46M (REF) | 317 | 312 | 0.98 | 1,577 |
| Estate tax 40->60%, exemption -> $5M | 722 | 684 | 0.95 | 3,453 |
| Repeal QBI (199A) | 896 | 881 | 0.98 | 3,886 |
| Eliminate SS taxable max | 3,071 | 2,491 | 0.81 | 11,126 |

## 6. Destination ledgers -- receipts-by-head deltas, FY2028-2036 conventional ($B)

(receipts.csv starts FY2028 in this vintage -- no CY2026 lead-in -- so these ledgers cover 9 of the 10 window years; use for composition/direction, not window totals)

| Scenario | income tax | payroll | corporate | estate | wealth | credits (outlay) | TOTAL |
|---|---|---|---|---|---|---|---|
| s_cg_r40 | 567 | 1 | -107 | -1 | 0 | -0 | 460 |
| s_cg_r25 | 257 | 0 | -31 | -1 | 0 | -0 | 226 |
| s_corp_r28 | 67 | 3 | 1,035 | -7 | 0 | -0 | 1,098 |
| s_wealth_r2_t50 | -452 | 1 | -40 | -87 | 3,880 | -0 | 3,301 |
| s_wealth_r1_t50 | -253 | 0 | -21 | -48 | 2,383 | -0 | 2,061 |
| s_deemed_deemed | 689 | 3 | -45 | -34 | 0 | -0 | 614 |
| s_ord_r44p8 | 1,014 | -6 | 117 | -3 | 0 | 0 | 1,122 |
| s_estate_r50_e8p46 | 47 | 0 | -2 | 262 | 0 | 0 | 307 |
| s_taxmax_on | -414 | 2,766 | -20 | -2 | 0 | 0 | 2,329 |
| stack_ref | 3,023 | 2,775 | 941 | 47 | 3,683 | -1 | 10,471 |

(Note: 'TOTAL' here = sum of revenue heads minus credit outlays; small differences vs revenue_estimates.csv reflect the FY booking conventions inside calc_receipts.)

## 7. Parts-vs-whole (naive sum vs actual package)

**Full 8-lever reference stack (stack_ref) vs sum of its 8 solos** (10y unless noted)

- Sum of standalone STATIC scores:        $15,761B
- Sum of standalone CONVENTIONAL scores:  $10,518B
- Package STATIC:                         $16,002B
- Package CONVENTIONAL (the truth):       $11,013B
- Package interaction (pkg conv - sum conv): $494B (+4.7% of the naive conventional sum)
- Behavioral survival: naive 67%, package 69%
- 30y: naive conv $49,172B vs package conv $49,491B (+0.6%)

**Corporate 21->35% + CG/div 20->30% (pc_corpr35_cgr30)** (10y unless noted)

- Sum of standalone STATIC scores:        $4,412B
- Sum of standalone CONVENTIONAL scores:  $2,579B
- Package STATIC:                         $4,412B
- Package CONVENTIONAL (the truth):       $2,533B
- Package interaction (pkg conv - sum conv): $-46B (-1.8% of the naive conventional sum)
- Behavioral survival: naive 58%, package 57%
- 30y: naive conv $11,608B vs package conv $11,394B (-1.8%)

**CG/div 20->40% + deemed realization at death (pr_cg_deemed)** (10y unless noted)

- Sum of standalone STATIC scores:        $2,481B
- Sum of standalone CONVENTIONAL scores:  $1,163B
- Package STATIC:                         $2,658B
- Package CONVENTIONAL (the truth):       $1,960B
- Package interaction (pkg conv - sum conv): $797B (+68.5% of the naive conventional sum)
- Behavioral survival: naive 47%, package 74%
- 30y: naive conv $5,453B vs package conv $9,088B (+66.7%)

**Draft Figure 3 as written -- corporate AND CG/div all to 25% (surrogate-style estimate; no direct run)**

- Sum of standalone static:       $1,495B  (cg25 547 + corp25 948, corp linear-scaled 4/7 of corp28)
- Sum of standalone conventional: $930B  (cg25 245 + corp25 686)
- Package conventional (surrogate: sum + g-scaled cg|corp interaction -5.1): **$925B**
- Surrogate validation context: quiz max err 2.5%, corners 2.4% -- treat as +/- a few percent.

**Draft Figure 4 -- CG/div 20->25% + deemed realization at death**

- Current-physics (dials v3, surrogate composition at cg=25):
  - Sum of standalone static:       $838B   (cg25 547 + deemed 291)
  - Sum of standalone conventional: $924B   (cg25 245 + deemed 679)
  - Package conventional estimate:  **$1,123B** (interaction 199.1)
- Direct runs (kg_v6_revmax, current physics), 10y:
  - Parts static:  cg+5pp 547 + deemed 291 = $838B
  - Parts conv:    cg+5pp 180 + deemed 670 = $851B
  - Package static $882B; package conv **$1,027B**

## 8. Capital-gains Laffer curves by death regime

### 8a. Current physics (dials v6, uncapped CG): step-up direct; deemed/carryover via surrogate composition

| Top CG rate | Step-up 10y conv | Deemed-conditional 10y conv delta* | Carryover-conditional* | Step-up 30y conv |
|---|---|---|---|---|
| 25% | 245 | 444 | 319 | 1,121 |
| 30% | 405 | 803 | 553 | 1,875 |
| 35% | 483 | 1,080 | 706 | 2,284 |
| 40% | 484 | 1,281 | 781 | 2,364 |
| 45% | 416 | 1,412 | 787 | 2,141 |
| 50% | 290 | 1,486 | 736 | 1,660 |

*Conditional columns = the CG-rate increase's own 10y conventional yield when the death regime is already in place (solo cg + g-scaled cg|deemed interaction residual; the regime's own revenue is NOT included). Surrogate-composed -- +/- a few percent.*

### 8b. Direct grid (kg_v6_revmax, current physics): total 10y conventional by cell

| Top CG rate | Step-up | Carryover | Deemed | Step-up static | Deemed static |
|---|---|---|---|---|---|
| 20% | 0 | 234 | 670 | 0 | 291 |
| 25% | 180 | 481 | 1,027 | 547 | 882 |
| 30% | 306 | 678 | 1,343 | 1,095 | 1,474 |
| 35% | 374 | 821 | 1,615 | 1,642 | 2,066 |
| 40% | 366 | 893 | 1,822 | 2,190 | 2,658 |
| 45% | 290 | 896 | 1,967 | 2,738 | 3,251 |

Leakage sign check (static minus conventional, 10y): positive = behavior loses revenue.
- cg+5pp under step-up: static 547, conv 180, leakage 367
- deemed alone: static 291, conv 670, leakage -379
- cg+5pp + deemed package: static 882, conv 1,027, leakage -144

## 9. 'Ask' vs 'collected' ETRs under the full reference stack (stack_ref, 2027)

(accrual = Haig-Simons denominator; cash = expanded income; taxes wealth_cit_vat; corp capital_income)

| Group | Baseline | Static 'ask' | Conventional 'collected' | Avoidance margin (pp) |
|---|---|---|---|---|
| **accrual denominator** | | | | |
| Quintile 5 | 15.4% | 21.3% | 19.7% | 1.6 |
| Top 1% | 17.6% | 31.6% | 27.6% | 4.0 |
| Top 0.1% | 21.5% | 43.0% | 36.3% | 6.8 |
| Top 0.01% | 23.7% | 50.1% | 41.7% | 8.4 |
| **cash denominator** | | | | |
| Quintile 5 | 22.9% | 31.8% | 29.3% | 2.4 |
| Top 1% | 26.2% | 46.9% | 40.9% | 6.0 |
| Top 0.1% | 29.2% | 58.3% | 49.1% | 9.1 |
| Top 0.01% | 30.0% | 63.2% | 52.6% | 10.6 |

## 10. Decade profile of the full stack

| Decade | Static $B | Conv $B | Survival | Conv % of GDP |
|---|---|---|---|---|
| 2027-2036 | 16,002 | 11,013 | 69% | 2.77% |
| 2037-2046 | 25,397 | 16,140 | 64% | 2.80% |
| 2047-2056 | 38,160 | 22,338 | 59% | 2.72% |

## 11. Realization-model color (kg_dynamics_summary)

- **s_cg_r40**: implied realization semi-elasticity mean -2.71 (range -3.05 to -2.57) deemed_realized 10y 0.0 (units as stored)
- **pr_cg_deemed**: implied realization semi-elasticity mean 0.86 (range 0.02 to 1.07) gains deemed-realized at death 10y 3,320B
- **stack_ref**: implied realization semi-elasticity mean 1.22 (range 0.47 to 1.40) gains deemed-realized at death 10y 3,306B

*(Interpretation note: the implied semi-elasticity is an OUTPUT here -- compare it across regimes to quantify 'policy chooses its own elasticity'.)*
