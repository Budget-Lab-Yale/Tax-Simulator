# Cross-model validation: NY

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9623|    7121|   0.6467|    0.7476|         0.8114|          0.8406|          0.1697|          2.4731|    3691.065|
| 2017|taxsim       | 10890|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9531|    7158|   0.6566|    0.7497|         0.8136|          0.8377|          0.1683|          0.5377|    3637.508|
| 2018|taxsim       | 10984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9526|    7119|   0.6469|    0.7462|         0.8142|          0.8385|          0.1673|          0.6318|    3151.137|
| 2019|taxsim       | 10988|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9501|    6780|   0.6459|    0.7436|         0.8165|          0.8409|          0.1627|          0.1212|    3807.243|
| 2020|taxsim       | 11012|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     269|   0.1927|    0.2975|         0.5985|          0.6989|          0.0625|        379.2898|  -25074.017|
| 2022|policyengine |  1530|     317|   0.2830|    0.3699|         0.7760|          0.8454|          0.0719|        295.3237|   -9010.947|
| 2023|policyengine |   713|      63|   0.1417|    0.2090|         0.5873|          0.6667|          0.0000|       1613.5711|   -8877.862|
| 2023|policyengine |   820|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1531|     364|   0.2685|    0.3521|         0.7582|          0.8187|          0.0588|        301.5656|  -17817.998|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  682|
| 2017|TRUE        |3 deductions     | 5324|
| 2017|TRUE        |4 taxable income |    5|
| 2017|TRUE        |5 state EITC     |   71|
| 2017|TRUE        |6 other credits  |  817|
| 2017|TRUE        |7 rate/rounding  |   16|
| 2017|FALSE       |1 state AGI      | 5491|
| 2017|FALSE       |3 deductions     |  335|
| 2017|FALSE       |5 state EITC     | 1025|
| 2017|FALSE       |6 other credits  |   10|
| 2018|TRUE        |1 state AGI      |  688|
| 2018|TRUE        |3 deductions     | 4678|
| 2018|TRUE        |5 state EITC     |   58|
| 2018|TRUE        |6 other credits  |  812|
| 2018|TRUE        |7 rate/rounding  |   10|
| 2018|FALSE       |1 state AGI      | 5458|
| 2018|FALSE       |3 deductions     |  283|
| 2018|FALSE       |5 state EITC     | 1000|
| 2018|FALSE       |6 other credits  |    9|
| 2019|TRUE        |1 state AGI      |  661|
| 2019|TRUE        |3 deductions     | 4719|
| 2019|TRUE        |5 state EITC     |   63|
| 2019|TRUE        |6 other credits  |  823|
| 2019|TRUE        |7 rate/rounding  |   13|
| 2019|FALSE       |1 state AGI      | 5509|
| 2019|FALSE       |3 deductions     |  321|
| 2019|FALSE       |5 state EITC     | 1048|
| 2019|FALSE       |6 other credits  |    7|
| 2020|TRUE        |1 state AGI      |  673|
| 2020|TRUE        |3 deductions     | 4320|
| 2020|TRUE        |5 state EITC     |   44|
| 2020|TRUE        |6 other credits  |  783|
| 2020|TRUE        |7 rate/rounding  |    7|
| 2020|FALSE       |1 state AGI      | 5552|
| 2020|FALSE       |3 deductions     |  387|
| 2020|FALSE       |5 state EITC     |  980|
| 2020|FALSE       |6 other credits  |   48|
| 2020|FALSE       |7 rate/rounding  |    9|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:-----|:------------|--------:|--------:|:----------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                          |
|NY    |policyengine |     2023|     2023|transfer-netting |exclude  |PE books New York's one-time 2025 inflation refund checks (S.3009-C: $200 single / $400 joint tiered down by NY AGI) into TAX YEAR 2023 as ny_inflation_refund_credit — formula_2023 pays it (source comment: the tax effect belongs to the eligibility year) and formula_2024 returns 0. Nearly every low/mid-AGI 2023 NY record shifts by the rebate (160 of ~300 clean mismatches sit exactly at +200/+400; zero-income records show PE = -200/-400), collapsing the 2023 cell (clean 0.833 2022 -> 0.160 2023 -> 0.797 2024). Same class as the IL 2021 rebate (issues doc P2). Excluded via predicate on the exported credit |
|NY    |taxsim       |     2017|     2020|input-coverage   |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate                          |

