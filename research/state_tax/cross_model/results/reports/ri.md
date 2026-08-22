# Cross-model validation: RI

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.6232|    0.8044|         0.7741|          0.8913|          0.1327|          1.3622|    133.0526|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.6208|    0.8047|         0.7713|          0.8927|          0.1308|          1.5064|    135.4936|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.6256|    0.8079|         0.7800|          0.8987|          0.1318|          1.4994|    134.3733|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.6184|    0.8001|         0.7750|          0.8933|          0.1310|          1.2696|    121.1894|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |   889|     239|   0.4781|    0.6839|         0.9205|          0.9707|          0.0945|         17.3607|   1031.5173|
| 2021|policyengine |   647|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.6039|    0.7204|         0.9458|          0.9623|          0.1297|          0.8023|    447.1435|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5869|    0.7113|         0.9338|          0.9487|          0.1348|          2.0907|    477.9293|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.5939|    0.6991|         0.9457|          0.9696|          0.1304|          2.1577|    -56.7413|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3379|
| 2017|TRUE        |2 exemptions    |   76|
| 2017|TRUE        |3 deductions    |  145|
| 2017|TRUE        |6 other credits |   50|
| 2017|TRUE        |7 rate/rounding |   20|
| 2017|FALSE       |1 state AGI     | 5025|
| 2017|FALSE       |3 deductions    |  101|
| 2017|FALSE       |5 state EITC    | 1036|
| 2017|FALSE       |6 other credits |    1|
| 2018|TRUE        |1 state AGI     | 3488|
| 2018|TRUE        |2 exemptions    |   74|
| 2018|TRUE        |3 deductions    |  154|
| 2018|TRUE        |6 other credits |   52|
| 2018|TRUE        |7 rate/rounding |   18|
| 2018|FALSE       |1 state AGI     | 5057|
| 2018|FALSE       |2 exemptions    |    3|
| 2018|FALSE       |3 deductions    |   95|
| 2018|FALSE       |5 state EITC    | 1012|
| 2018|FALSE       |7 rate/rounding |    1|
| 2019|TRUE        |1 state AGI     | 3430|
| 2019|TRUE        |2 exemptions    |   65|
| 2019|TRUE        |3 deductions    |  136|
| 2019|TRUE        |6 other credits |   38|
| 2019|TRUE        |7 rate/rounding |   16|
| 2019|FALSE       |1 state AGI     | 5097|
| 2019|FALSE       |2 exemptions    |    2|
| 2019|FALSE       |3 deductions    |  101|
| 2019|FALSE       |5 state EITC    | 1057|
| 2020|TRUE        |1 state AGI     | 3428|
| 2020|TRUE        |2 exemptions    |   65|
| 2020|TRUE        |3 deductions    |  133|
| 2020|TRUE        |6 other credits |   31|
| 2020|TRUE        |7 rate/rounding |   16|
| 2020|FALSE       |1 state AGI     | 5273|
| 2020|FALSE       |2 exemptions    |    8|
| 2020|FALSE       |3 deductions    |  116|
| 2020|FALSE       |5 state EITC    | 1005|
| 2020|FALSE       |6 other credits |    2|
| 2020|FALSE       |7 rate/rounding |    1|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:-----|:------------|--------:|--------:|:----------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|RI    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books Rhode Island's one-time 2022 child tax rebate ($250 per child, maximum 3, federal AGI at or below $100,000 single / $200,000 joint; FY2023 budget H 7123) into TAX YEAR 2021 via ri_child_tax_rebate -- the year the eligibility return was filed. The rebate was paid as a MAILED CHECK from October 2022 and is not a line on RI-1040, so it is outside our liability concept. Predicted before the run from the package source and CONFIRMED on the first pass: 72 of ~100 clean mismatches sit at exactly +250/+500/+750, and the 2021 cell reads 0.681 against 0.930-0.942 in 2022-2024. Same class as issues-doc P5. Excluded via predicate on the exported credit |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                         |

