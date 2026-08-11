# Cross-model validation: ND

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.5070|    0.6292|         0.5975|          0.7329|          0.1918|         12.9213|    1509.706|
| 2018|taxsim       | 20515|   13144|   0.5729|    0.7004|         0.6652|          0.7862|          0.1806|          2.9920|   -1206.123|
| 2019|taxsim       | 20514|   13088|   0.5833|    0.7053|         0.6740|          0.7880|          0.1835|          2.2928|   -1329.387|
| 2020|taxsim       | 20513|   12682|   0.5573|    0.6813|         0.6668|          0.7820|          0.1792|          4.3607|   -1488.167|
| 2021|policyengine |  1536|     269|   0.1790|    0.2507|         0.4238|          0.4796|          0.1641|        412.7454|   -7829.924|
| 2022|policyengine |  1530|     317|   0.1732|    0.2458|         0.4069|          0.4637|          0.1627|        419.2096|   -3645.158|
| 2023|policyengine |  1533|     356|   0.4247|    0.4925|         0.8624|          0.8736|          0.3686|        109.1397|    1047.594|
| 2024|policyengine |  1531|     363|   0.4291|    0.4794|         0.8485|          0.8595|          0.3762|        146.4849|   -6034.667|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 5260|
| 2017|TRUE        |4 taxable income |    9|
| 2017|FALSE       |1 state AGI      | 4836|
| 2017|FALSE       |4 taxable income |    7|
| 2018|TRUE        |1 state AGI      | 4398|
| 2018|TRUE        |4 taxable income |    2|
| 2018|FALSE       |1 state AGI      | 4351|
| 2018|FALSE       |4 taxable income |   11|
| 2019|TRUE        |1 state AGI      | 4262|
| 2019|TRUE        |4 taxable income |    5|
| 2019|FALSE       |1 state AGI      | 4272|
| 2019|FALSE       |4 taxable income |   10|
| 2020|TRUE        |1 state AGI      | 4224|
| 2020|TRUE        |4 taxable income |    2|
| 2020|FALSE       |1 state AGI      | 4842|
| 2020|FALSE       |4 taxable income |   14|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

