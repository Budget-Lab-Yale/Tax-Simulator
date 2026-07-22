# Metrics for the top-tax report

*Source vintages: dials `top_tax_dials_30y_v3` (current physics, run 2026-07-18: eta=2.4825, sigma=0.16, wealth-carry + estate-margins Tier 1.1/1.2 in; NEW in v3: uncapped CG rate (no_ord_cap), corp.rate entity-shifting fix, estate-avoidance fix), revmax grid `kg_v3_revmax` (kg spec-v3, eta=2.3992, pre-Tier-1 AND pre-uncap -- flagged stale). All $ figures $B unless noted; windows FY2027-2036 and FY2027-2056.*

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
| Top ordinary rate 37% -> 39.6% | 483 | 421 | 0.87 | 2,109 |
| Top ordinary rate 37% -> 44.8% (REF) | 1,449 | 1,225 | 0.85 | 6,139 |
| Top ordinary rate 37% -> 50% | 2,416 | 1,977 | 0.82 | 9,919 |
| CG & div top rate 20% -> 25% | 547 | 235 | 0.43 | 1,086 |
| CG & div top rate 20% -> 30% | 1,095 | 404 | 0.37 | 1,886 |
| CG & div top rate 20% -> 35% | 1,642 | 514 | 0.31 | 2,431 |
| CG & div top rate 20% -> 40% (REF) | 2,190 | 575 | 0.26 | 2,768 |
| CG & div top rate 20% -> 45% | 2,738 | 598 | 0.22 | 2,932 |
| CG & div top rate 20% -> 50% | 3,286 | 591 | 0.18 | 2,966 |
| Corporate rate 21% -> 28% | 900 | 721 | 0.80 | 3,227 |
| Wealth tax 1% > $50M | 2,944 | 2,044 | 0.69 | 10,038 |
| Wealth tax 2% > $50M (REF) | 5,888 | 3,267 | 0.55 | 15,571 |
| Carryover basis at death (no rate change) | 40 | 252 | 6.35 | 1,334 |
| Deemed realization at death (no rate change) | 291 | 707 | 2.43 | 3,187 |
| Estate tax 40->50%, exemption $15.4M->$8.46M (REF) | 317 | 315 | 0.99 | 1,587 |
| Estate tax 40->60%, exemption -> $5M | 722 | 691 | 0.96 | 3,482 |
| Repeal QBI (199A) | 896 | 886 | 0.99 | 3,906 |
| Eliminate SS taxable max | 2,520 | 2,503 | 0.99 | 11,361 |

## 6. Destination ledgers -- receipts-by-head deltas, FY2028-2036 conventional ($B)

(receipts.csv starts FY2028 in this vintage -- no CY2026 lead-in -- so these ledgers cover 9 of the 10 window years; use for composition/direction, not window totals)

| Scenario | income tax | payroll | corporate | estate | wealth | credits (outlay) | TOTAL |
|---|---|---|---|---|---|---|---|
| s_cg_r40 | 652 | 1 | -111 | -2 | 0 | -0 | 541 |
| s_cg_r25 | 248 | 0 | -31 | -1 | 0 | -0 | 217 |
| s_corp_r28 | 116 | 3 | 570 | -5 | 0 | -0 | 685 |
| s_wealth_r2_t50 | -450 | 0 | -43 | -87 | 3,879 | -0 | 3,299 |
| s_wealth_r1_t50 | -252 | 0 | -22 | -48 | 2,383 | -0 | 2,062 |
| s_deemed_deemed | 716 | 2 | -45 | -34 | 0 | -0 | 639 |
| s_ord_r44p8 | 1,033 | -5 | 117 | -3 | 0 | 0 | 1,142 |
| s_estate_r50_e8p46 | 49 | 0 | -2 | 262 | 0 | 0 | 309 |
| s_taxmax_on | -398 | 2,766 | -25 | -2 | 0 | 0 | 2,342 |
| stack_ref | 3,100 | 2,773 | 466 | 50 | 3,715 | -2 | 10,106 |

(Note: 'TOTAL' here = sum of revenue heads minus credit outlays; small differences vs revenue_estimates.csv reflect the FY booking conventions inside calc_receipts.)

## 7. Parts-vs-whole (naive sum vs actual package)

**Full 8-lever reference stack (stack_ref) vs sum of its 8 solos** (10y unless noted)

- Sum of standalone STATIC scores:        $14,451B
- Sum of standalone CONVENTIONAL scores:  $10,199B
- Package STATIC:                         $14,644B
- Package CONVENTIONAL (the truth):       $10,583B
- Package interaction (pkg conv - sum conv): $384B (+3.8% of the naive conventional sum)
- Behavioral survival: naive 71%, package 72%
- 30y: naive conv $47,746B vs package conv $47,624B (-0.3%)

**Corporate 21->28% + CG/div 20->30% (pc_corpr28_cgr30)** (10y unless noted)

- Sum of standalone STATIC scores:        $1,994B
- Sum of standalone CONVENTIONAL scores:  $1,125B
- Package STATIC:                         $1,994B
- Package CONVENTIONAL (the truth):       $1,104B
- Package interaction (pkg conv - sum conv): $-20B (-1.8% of the naive conventional sum)
- Behavioral survival: naive 56%, package 55%
- 30y: naive conv $5,113B vs package conv $5,010B (-2.0%)

**CG/div 20->40% + deemed realization at death (pr_cg_deemed)** (10y unless noted)

- Sum of standalone STATIC scores:        $2,481B
- Sum of standalone CONVENTIONAL scores:  $1,283B
- Package STATIC:                         $2,658B
- Package CONVENTIONAL (the truth):       $1,933B
- Package interaction (pkg conv - sum conv): $650B (+50.7% of the naive conventional sum)
- Behavioral survival: naive 52%, package 73%
- 30y: naive conv $5,955B vs package conv $8,979B (+50.8%)

**Draft Figure 3 as written -- corporate AND CG/div all to 25% (surrogate-style estimate; no direct run)**

- Sum of standalone static:       $1,061B  (cg25 547 + corp25 514, corp linear-scaled 4/7 of corp28)
- Sum of standalone conventional: $647B  (cg25 235 + corp25 412)
- Package conventional (surrogate: sum + g-scaled cg|corp interaction -5.4): **$642B**
- Surrogate validation context: quiz max err 2.5%, corners 2.4% -- treat as +/- a few percent.

**Draft Figure 4 -- CG/div 20->25% + deemed realization at death**

- Current-physics (dials v3, surrogate composition at cg=25):
  - Sum of standalone static:       $838B   (cg25 547 + deemed 291)
  - Sum of standalone conventional: $942B   (cg25 235 + deemed 707)
  - Package conventional estimate:  **$1,105B** (interaction 162.5)
- Direct runs (kg_v3_revmax, STALE eta=2.3992 physics), 10y:
  - Parts static:  cg+5pp 547 + deemed 288 = $835B
  - Parts conv:    cg+5pp 338 + deemed 748 = $1,086B
  - Package static $879B; package conv **$1,263B**

## 8. Capital-gains Laffer curves by death regime

### 8a. Current physics (dials v3, uncapped CG): step-up direct; deemed/carryover via surrogate composition

| Top CG rate | Step-up 10y conv | Deemed-conditional 10y conv delta* | Carryover-conditional* | Step-up 30y conv |
|---|---|---|---|---|
| 25% | 235 | 398 | 289 | 1,086 |
| 30% | 404 | 729 | 512 | 1,886 |
| 35% | 514 | 1,002 | 676 | 2,431 |
| 40% | 575 | 1,226 | 791 | 2,768 |
| 45% | 598 | 1,411 | 868 | 2,932 |
| 50% | 591 | 1,567 | 915 | 2,966 |

*Conditional columns = the CG-rate increase's own 10y conventional yield when the death regime is already in place (solo cg + g-scaled cg|deemed interaction residual; the regime's own revenue is NOT included). Surrogate-composed -- +/- a few percent.*

### 8b. Direct grid (kg_v3_revmax, stale physics, eta=2.3992): total 10y conventional by cell

| Top CG rate | Step-up | Carryover | Deemed | Step-up static | Deemed static |
|---|---|---|---|---|---|
| 20% | 0 | 294 | 748 | 0 | 288 |
| 25% | 338 | 693 | 1,263 | 547 | 879 |
| 30% | 647 | 1,061 | 1,748 | 1,088 | 1,464 |
| 35% | 924 | 1,395 | 2,201 | 1,624 | 2,042 |
| 40% | 1,028 | 1,527 | 2,399 | 1,891 | 2,338 |
| 45% | 1,034 | 1,536 | 2,420 | 1,925 | 2,378 |

Leakage sign check (static minus conventional, 10y): positive = behavior loses revenue.
- cg+5pp under step-up: static 547, conv 338, leakage 209
- deemed alone: static 288, conv 748, leakage -460
- cg+5pp + deemed package: static 879, conv 1,263, leakage -383

## 9. 'Ask' vs 'collected' ETRs under the full reference stack (stack_ref, 2027)

(accrual = Haig-Simons denominator; cash = expanded income; taxes wealth_cit_vat; corp capital_income)

| Group | Baseline | Static 'ask' | Conventional 'collected' | Avoidance margin (pp) |
|---|---|---|---|---|
| **accrual denominator** | | | | |
| Quintile 5 | 15.4% | 20.9% | 19.5% | 1.4 |
| Top 1% | 17.6% | 30.8% | 27.2% | 3.6 |
| Top 0.1% | 21.5% | 42.1% | 35.8% | 6.3 |
| Top 0.01% | 23.7% | 48.9% | 40.9% | 8.0 |
| **cash denominator** | | | | |
| Quintile 5 | 22.9% | 31.2% | 29.1% | 2.0 |
| Top 1% | 26.2% | 45.8% | 40.4% | 5.3 |
| Top 0.1% | 29.2% | 57.1% | 48.6% | 8.5 |
| Top 0.01% | 30.0% | 61.9% | 51.8% | 10.1 |

## 10. Decade profile of the full stack

| Decade | Static $B | Conv $B | Survival | Conv % of GDP |
|---|---|---|---|---|
| 2027-2036 | 14,644 | 10,583 | 72% | 2.66% |
| 2037-2046 | 23,364 | 15,533 | 66% | 2.70% |
| 2047-2056 | 35,265 | 21,508 | 61% | 2.62% |

## 11. Realization-model color (kg_dynamics_summary)

- **s_cg_r40**: implied realization semi-elasticity mean -2.50 (range -2.83 to -2.35) deemed_realized 10y 0.0 (units as stored)
- **pr_cg_deemed**: implied realization semi-elasticity mean 0.86 (range 0.02 to 1.05) gains deemed-realized at death 10y 3,321B
- **stack_ref**: implied realization semi-elasticity mean 1.24 (range 0.49 to 1.40) gains deemed-realized at death 10y 3,306B

*(Interpretation note: the implied semi-elasticity is an OUTPUT here -- compare it across regimes to quantify 'policy chooses its own elasticity'.)*
