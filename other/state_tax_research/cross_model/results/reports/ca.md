# Cross-model validation: CA

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4488|    0.5142|         0.5521|          0.6101|          0.1859|         75.8599|   14042.168|
| 2018|taxsim       | 20515|   13144|   0.5330|    0.6019|         0.6764|          0.7250|          0.1746|          3.6627|   -9939.249|
| 2019|taxsim       | 20514|   13088|   0.5279|    0.5923|         0.6812|          0.7267|          0.1576|          4.0100|  -10447.170|
| 2020|taxsim       | 20513|   12682|   0.5168|    0.5819|         0.6859|          0.7281|          0.1539|          8.2900|  -11412.316|
| 2021|policyengine |  1536|     271|   0.2949|    0.3991|         0.8303|          0.9077|          0.1302|        313.6970|  -20774.897|
| 2022|policyengine |  1530|     317|   0.3203|    0.3948|         0.7729|          0.8644|          0.1281|        332.3340|    5728.297|
| 2023|policyengine |  1533|     359|   0.3249|    0.4012|         0.8078|          0.8607|          0.1363|        316.3700|   38751.847|
| 2024|policyengine |  1531|     364|   0.3201|    0.3965|         0.7692|          0.8434|          0.1378|        372.1663|   -4735.039|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  512|
| 2017|TRUE        |3 deductions    | 5113|
| 2017|TRUE        |5 state EITC    |  109|
| 2017|TRUE        |6 other credits |  130|
| 2017|FALSE       |1 state AGI     | 4994|
| 2017|FALSE       |3 deductions    |  250|
| 2017|FALSE       |5 state EITC    |  178|
| 2017|FALSE       |6 other credits |   21|
| 2018|TRUE        |1 state AGI     |  470|
| 2018|TRUE        |3 deductions    | 3511|
| 2018|TRUE        |5 state EITC    |  161|
| 2018|TRUE        |6 other credits |  111|
| 2018|FALSE       |1 state AGI     | 4932|
| 2018|FALSE       |3 deductions    |  188|
| 2018|FALSE       |5 state EITC    |  197|
| 2018|FALSE       |6 other credits |   11|
| 2019|TRUE        |1 state AGI     |  463|
| 2019|TRUE        |3 deductions    | 3375|
| 2019|TRUE        |5 state EITC    |  196|
| 2019|TRUE        |6 other credits |  138|
| 2019|FALSE       |1 state AGI     | 4980|
| 2019|FALSE       |3 deductions    |  222|
| 2019|FALSE       |5 state EITC    |  303|
| 2019|FALSE       |6 other credits |    7|
| 2020|TRUE        |1 state AGI     |  223|
| 2020|TRUE        |3 deductions    | 3397|
| 2020|TRUE        |5 state EITC    |  241|
| 2020|TRUE        |6 other credits |  123|
| 2020|FALSE       |1 state AGI     | 5429|
| 2020|FALSE       |3 deductions    |  201|
| 2020|FALSE       |5 state EITC    |  292|
| 2020|FALSE       |6 other credits |    5|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

