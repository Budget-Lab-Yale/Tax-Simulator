# Cross-model validation: KS

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4365|    0.5915|         0.5479|          0.6565|          0.1150|         39.7504|    286.3435|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5206|    0.6626|         0.6532|          0.7393|          0.1050|          9.1336|    187.9419|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5315|    0.6580|         0.6692|          0.7331|          0.1052|          7.1391|    887.9423|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5306|    0.6408|         0.6877|          0.7359|          0.1068|          7.0455|   1148.1417|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     262|   0.2790|    0.4744|         0.7252|          0.9351|          0.0930|        117.9535|    908.3086|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.3856|    0.5127|         0.7243|          0.9136|          0.1043|         86.4066|    398.1267|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.3898|    0.5082|         0.7500|          0.9196|          0.1063|         78.3514|    446.6154|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     345|   0.3852|    0.5009|         0.7913|          0.9449|          0.1122|         98.5710|    -81.0657|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3449|
| 2017|TRUE        |2 exemptions    |  233|
| 2017|TRUE        |3 deductions    | 2311|
| 2017|TRUE        |6 other credits |  398|
| 2017|FALSE       |1 state AGI     | 5338|
| 2017|FALSE       |2 exemptions    |  104|
| 2017|FALSE       |3 deductions    |   95|
| 2017|FALSE       |5 state EITC    | 1041|
| 2017|FALSE       |6 other credits |    4|
| 2018|TRUE        |1 state AGI     | 3088|
| 2018|TRUE        |2 exemptions    |  256|
| 2018|TRUE        |3 deductions    | 1233|
| 2018|TRUE        |6 other credits |  618|
| 2018|FALSE       |1 state AGI     | 5208|
| 2018|FALSE       |2 exemptions    |   98|
| 2018|FALSE       |3 deductions    |   84|
| 2018|FALSE       |5 state EITC    | 1019|
| 2018|FALSE       |6 other credits |    6|
| 2019|TRUE        |1 state AGI     | 3149|
| 2019|TRUE        |2 exemptions    |  239|
| 2019|TRUE        |3 deductions    | 1101|
| 2019|TRUE        |6 other credits |  513|
| 2019|FALSE       |1 state AGI     | 5242|
| 2019|FALSE       |2 exemptions    |  104|
| 2019|FALSE       |3 deductions    |   95|
| 2019|FALSE       |5 state EITC    | 1060|
| 2019|FALSE       |6 other credits |    4|
| 2020|TRUE        |1 state AGI     | 2935|
| 2020|TRUE        |2 exemptions    |  228|
| 2020|TRUE        |3 deductions    | 1052|
| 2020|TRUE        |6 other credits |  466|
| 2020|FALSE       |1 state AGI     | 5696|
| 2020|FALSE       |2 exemptions    |   99|
| 2020|FALSE       |3 deductions    |   66|
| 2020|FALSE       |5 state EITC    |  978|
| 2020|FALSE       |6 other credits |    9|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

