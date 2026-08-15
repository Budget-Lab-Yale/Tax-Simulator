# Cross-model validation: DC

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9179|    6759|   0.6563|    0.7177|         0.7685|          0.8137|          0.1973|          0.0035|     86.7788|
| 2017|taxsim       | 11334|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 11883|    8770|   0.7256|    0.8137|         0.8651|          0.9165|          0.1673|          0.0036|    137.1862|
| 2018|taxsim       |  8632|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 11773|    8658|   0.7233|    0.8049|         0.8690|          0.9140|          0.1693|          0.0035|    112.7716|
| 2019|taxsim       |  8741|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 11970|    8746|   0.7185|    0.8034|         0.8651|          0.9117|          0.1645|          0.0035|    159.7252|
| 2020|taxsim       |  8543|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     269|   0.2220|    0.3242|         0.6803|          0.7509|          0.0723|        531.2500| -15563.6522|
| 2022|policyengine |  1530|     316|   0.2843|    0.3523|         0.7880|          0.8323|          0.0843|        516.2197|    805.4161|
| 2023|policyengine |  1533|     358|   0.2753|    0.3359|         0.7709|          0.8073|          0.0841|        540.7101|  27291.0493|
| 2024|policyengine |  1531|     363|   0.2789|    0.3436|         0.7521|          0.8182|          0.0784|        575.2482|  -7572.1280|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  635|
| 2017|TRUE        |2 exemptions     | 1509|
| 2017|TRUE        |3 deductions     | 3868|
| 2017|TRUE        |4 taxable income |  401|
| 2017|TRUE        |5 state EITC     |  236|
| 2017|TRUE        |6 other credits  |  105|
| 2017|TRUE        |7 rate/rounding  |  827|
| 2017|FALSE       |1 state AGI      | 5334|
| 2017|FALSE       |2 exemptions     |  126|
| 2017|FALSE       |3 deductions     |  210|
| 2017|FALSE       |4 taxable income |   16|
| 2017|FALSE       |5 state EITC     |  807|
| 2017|FALSE       |6 other credits  |    1|
| 2017|FALSE       |7 rate/rounding  |   16|
| 2018|TRUE        |1 state AGI      |  557|
| 2018|TRUE        |3 deductions     | 3809|
| 2018|TRUE        |4 taxable income |   16|
| 2018|TRUE        |5 state EITC     |  227|
| 2018|TRUE        |6 other credits  |   84|
| 2018|TRUE        |7 rate/rounding  |  592|
| 2018|FALSE       |1 state AGI      | 5184|
| 2018|FALSE       |3 deductions     |  278|
| 2018|FALSE       |4 taxable income |    1|
| 2018|FALSE       |5 state EITC     |  781|
| 2018|FALSE       |6 other credits  |    1|
| 2018|FALSE       |7 rate/rounding  |   18|
| 2019|TRUE        |1 state AGI      |  570|
| 2019|TRUE        |3 deductions     | 3822|
| 2019|TRUE        |4 taxable income |   22|
| 2019|TRUE        |5 state EITC     |  230|
| 2019|TRUE        |6 other credits  |   80|
| 2019|TRUE        |7 rate/rounding  |  585|
| 2019|FALSE       |1 state AGI      | 5256|
| 2019|FALSE       |3 deductions     |  301|
| 2019|FALSE       |4 taxable income |    2|
| 2019|FALSE       |5 state EITC     |  780|
| 2019|FALSE       |6 other credits  |    1|
| 2019|FALSE       |7 rate/rounding  |   18|
| 2020|TRUE        |1 state AGI      |  254|
| 2020|TRUE        |3 deductions     | 3684|
| 2020|TRUE        |4 taxable income |   27|
| 2020|TRUE        |5 state EITC     |  246|
| 2020|TRUE        |6 other credits  |   73|
| 2020|TRUE        |7 rate/rounding  |  625|
| 2020|FALSE       |1 state AGI      | 5670|
| 2020|FALSE       |3 deductions     |  278|
| 2020|FALSE       |4 taxable income |    2|
| 2020|FALSE       |5 state EITC     |  753|
| 2020|FALSE       |7 rate/rounding  |   18|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:-----|:------|--------:|--------:|:------------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|DC    |taxsim |     2017|     2017|omitted-credit     |annotate |OUR-SIDE, pre-registered. DC's Low Income Credit (47-1806.04(e), repealed after TY2017): nonrefundable, table-driven, mutually exclusive with the DC EITC. TAXSIM models it and we do not, so TAXSIM grants more credit on 2,107 TY2017 records where we have no EITC, with the extra running $119 to $1,025 and a median of $203 -- the $119 floor matching the statute's documented $119-$1,311 range exactly. In TY2019, after repeal, only 479 such records remain. NOTE FOR TRIAGE: this is NOT what makes DC 2017 weak. Because the credit is nonrefundable it is absorbed at low liability, so the affected records match at about 78% -- ABOVE the DC 2017 average of 47% -- and removing them makes the year WORSE (0.471 -> 0.411). Do not chase it as the 2017 driver                             |
|DC    |taxsim |     2017|     2020|external-model-bug |exclude  |TAXSIM subtracts unemployment compensation from DC state AGI in every year (probe: single, 50k wages + 5k UI -> v32 = 50,000 in 2017-2019; in 2020 it stacks the subtraction on top of the federal ARPA exclusion it also applies). The booklets say the opposite: UI appears in no Calculation B subtraction line, and the 2017 and 2020 instructions print "All unemployment compensation received in [year] is taxable." DC first exempted UI in TY2021 -- outside the TAXSIM window. Excluded via predicate on UI receipt (the exposure set)                                                                                                                                                                                                                                                             |
|DC    |taxsim |     2017|     2020|transfer-netting   |exclude  |TAXSIM nets the DC Schedule H property tax credit (v37) into siitax for low/moderate-income records with reported real property tax: point mass at the annual maximum ($1,025 2017-2018 / $1,200 2019-2020; probe: 15k wages + 6k proptax -> v37 = max, siitax negative). Schedule H is a rent- and household-income-based refundable credit we pre-registered as omitted (Tier-1 blocked on rent data; source packet known-difference 4). 365-415 fed-aligned records/yr, 97-99% of them mismatching. Excluded via predicate on TAXSIM paying the credit                                                                                                                                                                                                                                                    |
|DC    |taxsim |     2017|     2020|input-coverage     |exclude  |The dominant DC deduction-stage wedge is the crosswalk representation of Calculation F, not either model's DC law: TAXSIM's v35 equals EXACTLY (mortgage + otheritem + proptax inputs) less the 5%-of-AGI-over-200k limitation on 96.2-96.6% of fed-aligned both-itemizing records in 2018-2020 -- so TAXSIM strips its own computed DC income tax and applies the DC limitation correctly, but it cannot strip the as-reported income/sales taxes the crosswalk hands it inside otheritem (no state calculation can identify otheritem components as SALT), and it never receives investment interest or Schedule A "other" at all (no TAXSIM-35 inputs). Median v35-minus-ours +7.1-7.7k in 2018-2020. Excluded via predicate on the exposure set (itemizers carrying unstrippable or unhanded components) |
|DC    |taxsim |     2017|     2020|structural         |exclude  |TAXSIM's mstat/HoH derivation (the UT structural class) hits DC hard because DC prices filing status into both the deduction and the 2017 exemptions: a single filer with dependents is computed as HoH, receiving the HoH standard deduction (+2,150 in 2017 = 7,800 vs 5,650; +5,800 to +6,250 in 2018-2020) and, in 2017, the HoH EXTRA exemption (probe: single + 2 deps -> v33 = 7,100 = FOUR exemptions); an HoH filer without crosswalk-mappable dependents computes as single (same magnitudes, opposite sign). The 2017 exemption phase-out itself is verified EXACT against our stepped formula at six probe knots (1,704/1,633/1,065/355/71/0), so fractional exemption gaps are this count wedge modulated by the phase-out share. Excluded via the same exposure predicate as UT                |
|DC    |taxsim |     2017|     2020|input-coverage     |annotate |TAXSIM-35 has no blind-status input, so it cannot grant DC's blind additional standard deduction (federal-conformity amounts, 2018+) or the 2017 blind exemption; small point masses at -1,300/-1,550 (married) and -1,650/-1,950 (unmarried) in v34 vs our st_std_ded on blind records (~35/yr in 2019). Same class as the exempt-interest input gap already annotated ALL-states. Annotate only                                                                                                                                                                                                                                                                                                                                                                                                            |

