# Cross-model validation: GA

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.5550|    0.7593|         0.6093|          0.8009|          0.2002|          9.2693|   -194.5558|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5590|    0.6694|         0.6144|          0.7173|          0.2057|          6.9868|   -630.7196|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5705|    0.6770|         0.6236|          0.7220|          0.2070|          5.7838|   -794.3625|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5637|    0.6769|         0.6196|          0.7206|          0.1988|          6.4602|   -679.0051|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     299|   0.3672|    0.5337|         0.7625|          0.8194|          0.1823|         67.6599|    139.9990|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.3483|    0.5238|         0.7530|          0.8125|          0.1720|         74.1123|    180.9958|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.5000|    0.5626|         0.9104|          0.9249|          0.1983|         15.5973|   -220.9937|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4501|
| 2017|TRUE        |3 deductions    |  692|
| 2017|TRUE        |6 other credits |  337|
| 2017|TRUE        |7 rate/rounding |   81|
| 2017|FALSE       |1 state AGI     | 4848|
| 2017|FALSE       |3 deductions    |   65|
| 2017|FALSE       |6 other credits |  351|
| 2017|FALSE       |7 rate/rounding |   26|
| 2018|TRUE        |1 state AGI     | 4788|
| 2018|TRUE        |3 deductions    |  529|
| 2018|TRUE        |6 other credits |  203|
| 2018|TRUE        |7 rate/rounding |   98|
| 2018|FALSE       |1 state AGI     | 5011|
| 2018|FALSE       |3 deductions    |   93|
| 2018|FALSE       |6 other credits |  149|
| 2018|FALSE       |7 rate/rounding |   31|
| 2019|TRUE        |1 state AGI     | 4702|
| 2019|TRUE        |3 deductions    |  511|
| 2019|TRUE        |6 other credits |  207|
| 2019|TRUE        |7 rate/rounding |   80|
| 2019|FALSE       |1 state AGI     | 5035|
| 2019|FALSE       |3 deductions    |   72|
| 2019|FALSE       |6 other credits |  169|
| 2019|FALSE       |7 rate/rounding |   31|
| 2020|TRUE        |1 state AGI     | 4677|
| 2020|TRUE        |3 deductions    |  470|
| 2020|TRUE        |6 other credits |  189|
| 2020|TRUE        |7 rate/rounding |   93|
| 2020|FALSE       |1 state AGI     | 5205|
| 2020|FALSE       |3 deductions    |   72|
| 2020|FALSE       |6 other credits |  174|
| 2020|FALSE       |7 rate/rounding |   53|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|GA    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the HB 1302 one-time surplus tax rebate ($250 single/MFS, $375 HoH, $500 joint, paid 2022 on TY2021 returns) into TAX YEAR 2021 as a NONREFUNDABLE credit (ga_surplus_tax_rebate in the 2021-only non_refundable list, so it is liability-capped via max(0, tax - credits)). Combined with our HB 593 std-deduction vintage fix (anchor moved 2021 -> 2022), the residual clean-mismatch masses (204/329/437 = rebate minus the 46/63 std wedge) are fully attributed. Excluded via predicate on the exported rebate                                                                                                             |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

