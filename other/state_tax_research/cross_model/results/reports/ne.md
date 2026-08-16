# Cross-model validation: NE

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4342|    0.5427|         0.5381|          0.6009|          0.1305|         47.1194|    769.1405|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5311|    0.6719|         0.6584|          0.7491|          0.1270|          7.8300|   -256.7718|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5359|    0.6746|         0.6647|          0.7529|          0.1278|          6.9313|   -531.5768|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5155|    0.6550|         0.6615|          0.7509|          0.1274|         10.9409|   -342.4642|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     262|   0.3038|    0.4539|         0.8969|          0.9618|          0.0998|        167.3809|      5.4483|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     300|   0.3523|    0.4610|         0.8767|          0.9300|          0.1166|        150.5047|    191.8664|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     337|   0.3475|    0.4710|         0.8872|          0.9199|          0.1193|        163.4664|    250.3255|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.3252|    0.4243|         0.8121|          0.8671|          0.1061|        237.5170|   -292.3791|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4749|
| 2017|TRUE        |3 deductions    | 1713|
| 2017|TRUE        |6 other credits |   26|
| 2017|TRUE        |7 rate/rounding |   17|
| 2017|FALSE       |1 state AGI     | 5406|
| 2017|FALSE       |3 deductions    |  164|
| 2017|FALSE       |5 state EITC    |  926|
| 2018|TRUE        |1 state AGI     | 4172|
| 2018|TRUE        |3 deductions    |  844|
| 2018|TRUE        |6 other credits |   82|
| 2018|TRUE        |7 rate/rounding |   24|
| 2018|FALSE       |1 state AGI     | 5248|
| 2018|FALSE       |3 deductions    |  151|
| 2018|FALSE       |5 state EITC    |  888|
| 2018|FALSE       |6 other credits |    2|
| 2019|TRUE        |1 state AGI     | 4113|
| 2019|TRUE        |3 deductions    |  810|
| 2019|TRUE        |6 other credits |   94|
| 2019|TRUE        |7 rate/rounding |   24|
| 2019|FALSE       |1 state AGI     | 5261|
| 2019|FALSE       |3 deductions    |  157|
| 2019|FALSE       |5 state EITC    |  942|
| 2019|FALSE       |6 other credits |    2|
| 2020|TRUE        |1 state AGI     | 4090|
| 2020|TRUE        |3 deductions    |  778|
| 2020|TRUE        |6 other credits |   85|
| 2020|TRUE        |7 rate/rounding |   16|
| 2020|FALSE       |1 state AGI     | 5758|
| 2020|FALSE       |3 deductions    |  134|
| 2020|FALSE       |5 state EITC    |  886|
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

