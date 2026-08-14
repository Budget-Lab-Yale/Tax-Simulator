# Cross-model validation: DE

Class: broad | Generated: 2026-08-13 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4081|    0.4846|         0.5125|          0.5763|          0.2172|        110.9969|   8540.9416|
| 2018|taxsim       | 20515|   13144|   0.4154|    0.4852|         0.5297|          0.5876|          0.2068|        110.9993|  12096.1269|
| 2019|taxsim       | 20514|   13088|   0.4188|    0.5421|         0.5294|          0.6326|          0.2037|         62.5001|   -208.2393|
| 2020|taxsim       | 20513|   12682|   0.4097|    0.5288|         0.5300|          0.6306|          0.1981|         71.5737|   -514.7973|
| 2021|policyengine |  1536|     269|   0.3001|    0.4245|         0.7435|          0.7918|          0.1120|        180.3716| -11884.7797|
| 2022|policyengine |  1530|     317|   0.3288|    0.4235|         0.7855|          0.8233|          0.1144|        180.3705|   1340.6952|
| 2023|policyengine |  1533|     357|   0.3216|    0.4188|         0.7479|          0.7843|          0.1174|        181.7152|  17583.4104|
| 2024|policyengine |  1531|     363|   0.3155|    0.4095|         0.7548|          0.7961|          0.1104|        214.5021|  -3701.7592|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2031|
| 2017|TRUE        |3 deductions    | 3970|
| 2017|TRUE        |6 other credits |  216|
| 2017|TRUE        |7 rate/rounding |  166|
| 2017|FALSE       |1 state AGI     | 5094|
| 2017|FALSE       |3 deductions    |  148|
| 2017|FALSE       |5 state EITC    |  510|
| 2017|FALSE       |6 other credits |    4|
| 2017|FALSE       |7 rate/rounding |    3|
| 2018|TRUE        |1 state AGI     | 2097|
| 2018|TRUE        |3 deductions    | 3668|
| 2018|TRUE        |6 other credits |  229|
| 2018|TRUE        |7 rate/rounding |  187|
| 2018|FALSE       |1 state AGI     | 5140|
| 2018|FALSE       |3 deductions    |  126|
| 2018|FALSE       |5 state EITC    |  541|
| 2018|FALSE       |6 other credits |    1|
| 2018|FALSE       |7 rate/rounding |    4|
| 2019|TRUE        |1 state AGI     | 2036|
| 2019|TRUE        |3 deductions    | 4084|
| 2019|TRUE        |6 other credits |   23|
| 2019|TRUE        |7 rate/rounding |   16|
| 2019|FALSE       |1 state AGI     | 5068|
| 2019|FALSE       |3 deductions    |  530|
| 2019|FALSE       |5 state EITC    |  165|
| 2019|FALSE       |6 other credits |    1|
| 2020|TRUE        |1 state AGI     | 2061|
| 2020|TRUE        |3 deductions    | 3865|
| 2020|TRUE        |6 other credits |   23|
| 2020|TRUE        |7 rate/rounding |   11|
| 2020|FALSE       |1 state AGI     | 5488|
| 2020|FALSE       |3 deductions    |  524|
| 2020|FALSE       |5 state EITC    |  135|
| 2020|FALSE       |6 other credits |    2|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

