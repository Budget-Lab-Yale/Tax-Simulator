# Cross-model validation: MI

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3885|    0.5205|         0.5267|          0.6469|          0.0392|         80.5002|    2396.570|
| 2018|taxsim       | 20515|   13144|   0.3827|    0.5172|         0.5197|          0.6432|          0.0391|         83.8584|    2486.005|
| 2019|taxsim       | 20514|   13088|   0.3851|    0.5127|         0.5308|          0.6420|          0.0392|         86.1958|    2464.176|
| 2020|taxsim       | 20513|   12682|   0.3657|    0.4938|         0.5199|          0.6393|          0.0394|        106.2060|    2868.203|
| 2021|policyengine |  1536|     269|   0.3132|    0.4447|         0.5762|          0.6357|          0.0189|        163.3524|   -2873.965|
| 2022|policyengine |  1530|     318|   0.3144|    0.4157|         0.5126|          0.5629|          0.0255|        258.8256|    2170.980|
| 2023|policyengine |  1533|     358|   0.3138|    0.4331|         0.5866|          0.6844|          0.0248|        186.6595|   11700.697|
| 2024|policyengine |  1531|     365|   0.3142|    0.4141|         0.5890|          0.6548|          0.0209|        260.7598|   -1557.431|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4509|
| 2017|TRUE        |2 exemptions    | 1688|
| 2017|FALSE       |1 state AGI     | 5209|
| 2017|FALSE       |2 exemptions    | 1138|
| 2018|TRUE        |1 state AGI     | 4629|
| 2018|TRUE        |3 deductions    |  353|
| 2018|TRUE        |6 other credits | 1331|
| 2018|FALSE       |1 state AGI     | 5249|
| 2018|FALSE       |3 deductions    |   56|
| 2018|FALSE       |5 state EITC    |  795|
| 2018|FALSE       |6 other credits |  250|
| 2019|TRUE        |1 state AGI     | 4433|
| 2019|TRUE        |3 deductions    |  405|
| 2019|TRUE        |6 other credits | 1303|
| 2019|FALSE       |1 state AGI     | 5272|
| 2019|FALSE       |3 deductions    |   63|
| 2019|FALSE       |5 state EITC    |  898|
| 2019|FALSE       |6 other credits |  241|
| 2020|TRUE        |1 state AGI     | 4420|
| 2020|TRUE        |3 deductions    |  420|
| 2020|TRUE        |6 other credits | 1249|
| 2020|FALSE       |1 state AGI     | 5823|
| 2020|FALSE       |3 deductions    |   51|
| 2020|FALSE       |5 state EITC    |  826|
| 2020|FALSE       |6 other credits |  222|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:-----|:------|--------:|--------:|:------------------|:--------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                     |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                |
|MI    |taxsim |     2017|     2020|external-model-bug |annotate |TAXSIM applies the Tier-2 Michigan Standard Deduction amount ($20,000/$40,000) to ALL filers 67+, ignoring the birth-cohort tiers: a Tier-1 (born before 1946) pensioner is capped at 20,000 instead of the Form 4884 private-pension maximum (52,808 single 2019; probe: 74-year-old with 60k pension -> TAXSIM 1,513.00 vs form-true 118.66), and a Tier-1 filer with wages+interest receives the flat 20,000 the form does not give that cohort (probe: 76-year-old -> TAXSIM 25.50). Non-senior and Tier-2 shapes match to the cent (5 probe cases) |

