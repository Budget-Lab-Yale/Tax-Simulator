# Cross-model validation: NE

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6578|   0.4402|    0.5493|         0.9764|          0.9898|          0.1305|         43.0044|    673.9472|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    8839|   0.5833|    0.6912|         0.9223|          0.9465|          0.1270|          1.6565|   -365.4885|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    8764|   0.5854|    0.6928|         0.9253|          0.9507|          0.1278|          3.7825|   -625.0956|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    8448|   0.5682|    0.6775|         0.9241|          0.9504|          0.1274|          3.6635|   -438.5291|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.4036|    0.6143|         0.9038|          0.9451|          0.1007|         37.3325|   -172.4286|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.4864|    0.6407|         0.9125|          0.9480|          0.1183|         17.3071|     30.1877|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.4745|    0.6171|         0.9167|          0.9423|          0.1236|         24.9953|     86.2673|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4487|    0.5730|         0.8543|          0.8783|          0.1113|         30.3749|   -438.0351|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  139|
| 2017|TRUE        |3 deductions    |   20|
| 2017|TRUE        |6 other credits |   27|
| 2017|TRUE        |7 rate/rounding |   17|
| 2017|FALSE       |1 state AGI     | 8844|
| 2017|FALSE       |3 deductions    | 2924|
| 2017|FALSE       |5 state EITC    |  930|
| 2018|TRUE        |1 state AGI     |  772|
| 2018|TRUE        |3 deductions    |  161|
| 2018|TRUE        |6 other credits |   32|
| 2018|TRUE        |7 rate/rounding |   24|
| 2018|FALSE       |1 state AGI     | 7255|
| 2018|FALSE       |3 deductions    | 1348|
| 2018|FALSE       |5 state EITC    |  897|
| 2018|FALSE       |6 other credits |   54|
| 2018|FALSE       |7 rate/rounding |    1|
| 2019|TRUE        |1 state AGI     |  777|
| 2019|TRUE        |3 deductions    |  147|
| 2019|TRUE        |6 other credits |   26|
| 2019|TRUE        |7 rate/rounding |   23|
| 2019|FALSE       |1 state AGI     | 7344|
| 2019|FALSE       |3 deductions    | 1255|
| 2019|FALSE       |5 state EITC    |  951|
| 2019|FALSE       |6 other credits |   70|
| 2019|FALSE       |7 rate/rounding |    1|
| 2020|TRUE        |1 state AGI     |  784|
| 2020|TRUE        |3 deductions    |  128|
| 2020|TRUE        |6 other credits |   24|
| 2020|TRUE        |7 rate/rounding |   17|
| 2020|FALSE       |1 state AGI     | 7764|
| 2020|FALSE       |3 deductions    | 1212|
| 2020|FALSE       |5 state EITC    |  892|
| 2020|FALSE       |6 other credits |   62|
| 2020|FALSE       |7 rate/rounding |    1|

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

