# Cross-model validation: GA

Class: broad | Generated: 2026-07-19 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3643|    0.4795|         0.4330|          0.5396|          0.1719|        134.0165|    353.6497|
| 2018|taxsim       | 20515|   13144|   0.3645|    0.4309|         0.4365|          0.5046|          0.1762|        221.9971|  -4097.6417|
| 2019|taxsim       | 20514|   13088|   0.3644|    0.4309|         0.4368|          0.5057|          0.1757|        212.7471|  -4168.4669|
| 2020|taxsim       | 20513|   12682|   0.3602|    0.4290|         0.4342|          0.5036|          0.1691|        212.7473|  -4664.7748|
| 2021|policyengine |  1536|     270|   0.1615|    0.2038|         0.4037|          0.4481|          0.1491|        478.1241|  -8129.0492|
| 2022|policyengine |  1530|     315|   0.2261|    0.3471|         0.6476|          0.6984|          0.1425|        212.7450|   3637.3099|
| 2023|policyengine |  1533|     357|   0.2277|    0.3620|         0.5938|          0.6471|          0.1324|        196.2487|  17438.7280|
| 2024|policyengine |  1531|     364|   0.4017|    0.4520|         0.9203|          0.9203|          0.1600|        221.0783|  -1322.8861|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3777|
| 2017|TRUE        |2 exemptions    | 2785|
| 2017|TRUE        |3 deductions    |  461|
| 2017|TRUE        |6 other credits |  311|
| 2017|TRUE        |7 rate/rounding |   89|
| 2017|FALSE       |1 state AGI     | 3842|
| 2017|FALSE       |2 exemptions    | 1185|
| 2017|FALSE       |3 deductions    |  222|
| 2017|FALSE       |6 other credits |  340|
| 2017|FALSE       |7 rate/rounding |   29|
| 2018|TRUE        |1 state AGI     | 3816|
| 2018|TRUE        |2 exemptions    | 2801|
| 2018|TRUE        |3 deductions    |  464|
| 2018|TRUE        |6 other credits |  187|
| 2018|TRUE        |7 rate/rounding |  138|
| 2018|FALSE       |1 state AGI     | 3966|
| 2018|FALSE       |2 exemptions    | 1187|
| 2018|FALSE       |3 deductions    |  293|
| 2018|FALSE       |6 other credits |  148|
| 2018|FALSE       |7 rate/rounding |   38|
| 2019|TRUE        |1 state AGI     | 3713|
| 2019|TRUE        |2 exemptions    | 2863|
| 2019|TRUE        |3 deductions    |  478|
| 2019|TRUE        |6 other credits |  199|
| 2019|TRUE        |7 rate/rounding |  118|
| 2019|FALSE       |1 state AGI     | 3986|
| 2019|FALSE       |2 exemptions    | 1199|
| 2019|FALSE       |3 deductions    |  273|
| 2019|FALSE       |6 other credits |  170|
| 2019|FALSE       |7 rate/rounding |   40|
| 2020|TRUE        |1 state AGI     | 3749|
| 2020|TRUE        |2 exemptions    | 2708|
| 2020|TRUE        |3 deductions    |  410|
| 2020|TRUE        |6 other credits |  180|
| 2020|TRUE        |7 rate/rounding |  128|
| 2020|FALSE       |1 state AGI     | 4157|
| 2020|FALSE       |2 exemptions    | 1265|
| 2020|FALSE       |3 deductions    |  286|
| 2020|FALSE       |6 other credits |  178|
| 2020|FALSE       |7 rate/rounding |   63|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

