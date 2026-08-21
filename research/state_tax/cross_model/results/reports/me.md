# Cross-model validation: ME

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.6717|    0.8310|         0.7952|          0.9019|          0.0225|          2.4885|     65.3727|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5958|    0.7145|         0.7068|          0.7834|          0.0253|          4.9498|     50.0872|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5920|    0.7077|         0.7043|          0.7786|          0.0258|          5.0729|     65.4228|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5435|    0.6817|         0.7044|          0.7797|          0.0269|          7.5113|     67.7841|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.1587|    0.2474|         0.1126|          0.1429|          0.0000|        849.8906|   1684.2213|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.5592|    0.6538|         0.8487|          0.8865|          0.0526|          3.1634|    377.4560|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5454|    0.6301|         0.8291|          0.8739|          0.0510|          5.1400|    404.2378|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     458|   0.5374|    0.6365|         0.8406|          0.8886|          0.0374|          5.3236|   -231.6666|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2086|
| 2017|TRUE        |2 exemptions    |   76|
| 2017|TRUE        |3 deductions    | 1074|
| 2017|TRUE        |6 other credits |  171|
| 2017|FALSE       |1 state AGI     | 4929|
| 2017|FALSE       |3 deductions    |  122|
| 2017|FALSE       |5 state EITC    |  542|
| 2017|FALSE       |6 other credits |    1|
| 2018|TRUE        |1 state AGI     | 2766|
| 2018|TRUE        |2 exemptions    |   11|
| 2018|TRUE        |3 deductions    | 1399|
| 2018|TRUE        |6 other credits |  373|
| 2018|TRUE        |7 rate/rounding |    2|
| 2018|FALSE       |1 state AGI     | 5103|
| 2018|FALSE       |3 deductions    |  166|
| 2018|FALSE       |5 state EITC    |  521|
| 2018|FALSE       |6 other credits |    9|
| 2019|TRUE        |1 state AGI     | 2867|
| 2019|TRUE        |2 exemptions    |   60|
| 2019|TRUE        |3 deductions    | 1286|
| 2019|TRUE        |6 other credits |  363|
| 2019|FALSE       |1 state AGI     | 5154|
| 2019|FALSE       |3 deductions    |  183|
| 2019|FALSE       |5 state EITC    |  560|
| 2019|FALSE       |6 other credits |   14|
| 2019|FALSE       |7 rate/rounding |    1|
| 2020|TRUE        |1 state AGI     | 2817|
| 2020|TRUE        |2 exemptions    |   10|
| 2020|TRUE        |3 deductions    | 1256|
| 2020|TRUE        |6 other credits |  397|
| 2020|TRUE        |7 rate/rounding |    1|
| 2020|FALSE       |1 state AGI     | 5666|
| 2020|FALSE       |3 deductions    |  156|
| 2020|FALSE       |5 state EITC    |  999|
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

