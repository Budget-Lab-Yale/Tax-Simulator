# Cross-model validation: CA

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.4516|    0.5165|         0.5427|          0.6002|          0.1860|         71.8565|   14105.547|
| 2018|taxsim       | 20515|   13504|   0.5386|    0.6093|         0.6680|          0.7164|          0.1746|          3.6593|   -9871.917|
| 2019|taxsim       | 20514|   13433|   0.5334|    0.5975|         0.6733|          0.7176|          0.1576|          3.0500|  -10394.417|
| 2020|taxsim       | 20513|   13070|   0.5238|    0.5894|         0.6767|          0.7186|          0.1540|          6.9273|  -11353.481|
| 2021|policyengine |  1536|     271|   0.2949|    0.3991|         0.8303|          0.9077|          0.1302|        313.6970|  -20774.897|
| 2022|policyengine |  1530|     317|   0.3203|    0.3948|         0.7729|          0.8644|          0.1281|        332.3340|    5728.297|
| 2023|policyengine |  1533|     359|   0.3249|    0.4012|         0.8078|          0.8607|          0.1363|        316.3700|   38751.847|
| 2024|policyengine |  1531|     364|   0.3201|    0.3965|         0.7692|          0.8434|          0.1378|        372.1663|   -4735.039|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  509|
| 2017|TRUE        |3 deductions    | 5403|
| 2017|TRUE        |5 state EITC    |  110|
| 2017|TRUE        |6 other credits |  133|
| 2017|FALSE       |1 state AGI     | 4633|
| 2017|FALSE       |3 deductions    |  264|
| 2017|FALSE       |5 state EITC    |  178|
| 2017|FALSE       |6 other credits |   20|
| 2018|TRUE        |1 state AGI     |  477|
| 2018|TRUE        |3 deductions    | 3726|
| 2018|TRUE        |5 state EITC    |  162|
| 2018|TRUE        |6 other credits |  119|
| 2018|FALSE       |1 state AGI     | 4576|
| 2018|FALSE       |3 deductions    |  200|
| 2018|FALSE       |5 state EITC    |  197|
| 2018|FALSE       |6 other credits |    9|
| 2019|TRUE        |1 state AGI     |  460|
| 2019|TRUE        |3 deductions    | 3592|
| 2019|TRUE        |5 state EITC    |  196|
| 2019|TRUE        |6 other credits |  140|
| 2019|FALSE       |1 state AGI     | 4639|
| 2019|FALSE       |3 deductions    |  234|
| 2019|FALSE       |5 state EITC    |  303|
| 2019|FALSE       |6 other credits |    7|
| 2020|TRUE        |1 state AGI     |  229|
| 2020|TRUE        |3 deductions    | 3633|
| 2020|TRUE        |5 state EITC    |  242|
| 2020|TRUE        |6 other credits |  122|
| 2020|FALSE       |1 state AGI     | 5031|
| 2020|FALSE       |3 deductions    |  216|
| 2020|FALSE       |5 state EITC    |  291|
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

