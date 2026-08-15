# Cross-model validation: IN

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3331|    0.6877|         0.4493|          0.8342|          0.0517|         80.7459|    828.4228|
| 2018|taxsim       | 20515|   13144|   0.3361|    0.6910|         0.4548|          0.8389|          0.0533|         80.7459|    827.7707|
| 2019|taxsim       | 20514|   13088|   0.3321|    0.6881|         0.4510|          0.8453|          0.0522|         80.7461|    881.0534|
| 2020|taxsim       | 20513|   12682|   0.3249|    0.6688|         0.4495|          0.8370|          0.0520|         80.7474|    977.1576|
| 2021|policyengine |  1536|     270|   0.2565|    0.5566|         0.6296|          0.8963|          0.0482|         80.7597|  -2149.9210|
| 2022|policyengine |  1530|     318|   0.2771|    0.5745|         0.5818|          0.9245|          0.0464|         80.7543|   1721.7893|
| 2023|policyengine |  1533|     356|   0.2785|    0.5812|         0.6657|          0.9382|          0.0450|         78.7516|   9163.9598|
| 2024|policyengine |  1531|     365|   0.2854|    0.5617|         0.6795|          0.9397|          0.0470|         78.5124|  -1043.5679|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2894|
| 2017|TRUE        |2 exemptions     | 1476|
| 2017|TRUE        |4 taxable income | 2623|
| 2017|TRUE        |5 state EITC     |  217|
| 2017|FALSE       |1 state AGI      | 5358|
| 2017|FALSE       |2 exemptions     |  104|
| 2017|FALSE       |4 taxable income |  105|
| 2017|FALSE       |5 state EITC     |  903|
| 2018|TRUE        |1 state AGI      | 2870|
| 2018|TRUE        |2 exemptions     | 1440|
| 2018|TRUE        |4 taxable income | 2678|
| 2018|TRUE        |5 state EITC     |  177|
| 2018|TRUE        |6 other credits  |    1|
| 2018|FALSE       |1 state AGI      | 5381|
| 2018|FALSE       |2 exemptions     |  109|
| 2018|FALSE       |4 taxable income |  110|
| 2018|FALSE       |5 state EITC     |  854|
| 2019|TRUE        |1 state AGI      | 2768|
| 2019|TRUE        |2 exemptions     | 1458|
| 2019|TRUE        |4 taxable income | 2758|
| 2019|TRUE        |5 state EITC     |  201|
| 2019|FALSE       |1 state AGI      | 5384|
| 2019|FALSE       |2 exemptions     |  125|
| 2019|FALSE       |4 taxable income |  105|
| 2019|FALSE       |5 state EITC     |  903|
| 2020|TRUE        |1 state AGI      | 2743|
| 2020|TRUE        |2 exemptions     | 1413|
| 2020|TRUE        |4 taxable income | 2634|
| 2020|TRUE        |5 state EITC     |  192|
| 2020|FALSE       |1 state AGI      | 5753|
| 2020|FALSE       |2 exemptions     |  123|
| 2020|FALSE       |4 taxable income |  115|
| 2020|FALSE       |5 state EITC     |  876|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

