# Cross-model validation: WV

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.7532|    0.8647|         0.8812|          0.9370|          0.1705|          0.1023|     81.4150|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.7515|    0.8649|         0.8824|          0.9372|          0.1663|          0.1313|     74.5460|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.7515|    0.8630|         0.8788|          0.9384|          0.1704|          0.1283|     85.6054|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.7033|    0.8152|         0.8550|          0.9125|          0.1643|          0.2700|     94.0605|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.4957|    0.6135|         0.7280|          0.7701|          0.1766|         16.9090|    958.1739|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     300|   0.5635|    0.6188|         0.7400|          0.7733|          0.1744|          1.7543|    330.8107|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.5799|    0.6361|         0.7113|          0.7560|          0.1772|          1.3473|    440.9287|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.5443|    0.6070|         0.7341|          0.7803|          0.1626|          3.4561|   -161.1610|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2030|
| 2017|TRUE        |4 taxable income |    2|
| 2017|TRUE        |6 other credits  |  394|
| 2017|FALSE       |1 state AGI      | 5191|
| 2017|FALSE       |4 taxable income |    2|
| 2017|FALSE       |6 other credits  |  142|
| 2018|TRUE        |1 state AGI      | 2155|
| 2018|TRUE        |6 other credits  |  363|
| 2018|FALSE       |1 state AGI      | 5212|
| 2018|FALSE       |4 taxable income |    2|
| 2018|FALSE       |6 other credits  |  195|
| 2019|TRUE        |1 state AGI      | 2250|
| 2019|TRUE        |4 taxable income |    2|
| 2019|TRUE        |6 other credits  |  342|
| 2019|FALSE       |1 state AGI      | 5259|
| 2019|FALSE       |4 taxable income |    3|
| 2019|FALSE       |6 other credits  |  175|
| 2020|TRUE        |1 state AGI      | 2608|
| 2020|TRUE        |6 other credits  |  226|
| 2020|FALSE       |1 state AGI      | 5833|
| 2020|FALSE       |4 taxable income |    3|
| 2020|FALSE       |6 other credits  |  170|

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

