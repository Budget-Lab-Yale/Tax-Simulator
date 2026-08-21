# Cross-model validation: AR

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.2815|    0.4038|         0.3220|          0.4367|          0.1528|        257.3231|    -34.7005|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.2684|    0.3991|         0.3079|          0.4356|          0.1462|        256.5027|   -156.8782|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.3046|    0.4315|         0.3345|          0.4501|          0.1693|        221.6116|   -315.2888|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.3037|    0.4354|         0.3370|          0.4579|          0.1704|        202.5966|  -1261.2336|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     262|   0.2423|    0.3788|         0.4885|          0.6183|          0.1229|        250.3349|  37546.5188|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.1639|    0.2472|         0.3488|          0.4219|          0.1341|        289.5456|  32966.2008|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.1608|    0.3639|         0.2946|          0.5536|          0.1262|        209.9755|  12352.0035|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.2339|    0.4374|         0.4566|          0.6416|          0.1235|        163.7501|  17604.6043|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 6623|
| 2017|TRUE        |3 deductions    | 1076|
| 2017|TRUE        |6 other credits |  886|
| 2017|TRUE        |7 rate/rounding |  516|
| 2017|FALSE       |1 state AGI     | 5147|
| 2017|FALSE       |3 deductions    |  305|
| 2017|FALSE       |6 other credits |  352|
| 2017|FALSE       |7 rate/rounding |  622|
| 2018|TRUE        |1 state AGI     | 6734|
| 2018|TRUE        |3 deductions    | 1074|
| 2018|TRUE        |6 other credits |  868|
| 2018|TRUE        |7 rate/rounding |  641|
| 2018|FALSE       |1 state AGI     | 5116|
| 2018|FALSE       |3 deductions    |  325|
| 2018|FALSE       |6 other credits |  358|
| 2018|FALSE       |7 rate/rounding |  637|
| 2019|TRUE        |1 state AGI     | 6616|
| 2019|TRUE        |3 deductions    | 1048|
| 2019|TRUE        |6 other credits |  846|
| 2019|TRUE        |7 rate/rounding |  455|
| 2019|FALSE       |1 state AGI     | 5115|
| 2019|FALSE       |3 deductions    |  311|
| 2019|FALSE       |6 other credits |  336|
| 2019|FALSE       |7 rate/rounding |  453|
| 2020|TRUE        |1 state AGI     | 6912|
| 2020|TRUE        |3 deductions    |  565|
| 2020|TRUE        |6 other credits |  796|
| 2020|TRUE        |7 rate/rounding |  403|
| 2020|FALSE       |1 state AGI     | 5453|
| 2020|FALSE       |3 deductions    |  220|
| 2020|FALSE       |6 other credits |  391|
| 2020|FALSE       |7 rate/rounding |  450|

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

