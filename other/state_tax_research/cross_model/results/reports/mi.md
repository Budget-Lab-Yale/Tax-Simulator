# Cross-model validation: MI

Class: broad | Generated: 2026-07-19 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3500|    0.4728|         0.4764|          0.5891|          0.0175|        134.1619|    2607.325|
| 2018|taxsim       | 20515|   13144|   0.3407|    0.4656|         0.4644|          0.5810|          0.0174|        141.8182|    2704.148|
| 2019|taxsim       | 20514|   13088|   0.3431|    0.4605|         0.4747|          0.5795|          0.0184|        152.1083|    2690.174|
| 2020|taxsim       | 20513|   12682|   0.3239|    0.4406|         0.4620|          0.5726|          0.0188|        187.6844|    3075.422|
| 2021|policyengine |  1536|     269|   0.2923|    0.4160|         0.4981|          0.5613|          0.0085|        220.3084|   -2735.638|
| 2022|policyengine |  1530|     318|   0.2908|    0.3843|         0.4497|          0.5063|          0.0131|        343.2626|    2290.148|
| 2023|policyengine |  1533|     358|   0.2851|    0.4025|         0.5223|          0.6257|          0.0130|        247.8027|   11851.943|
| 2024|policyengine |  1531|     365|   0.2907|    0.3880|         0.5479|          0.6137|          0.0131|        302.1200|   -1353.177|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3703|
| 2017|TRUE        |2 exemptions    | 3152|
| 2017|FALSE       |1 state AGI     | 5153|
| 2017|FALSE       |2 exemptions    | 1325|
| 2018|TRUE        |1 state AGI     | 3845|
| 2018|TRUE        |3 deductions    | 1864|
| 2018|TRUE        |6 other credits | 1331|
| 2018|FALSE       |1 state AGI     | 5199|
| 2018|FALSE       |3 deductions    |  241|
| 2018|FALSE       |5 state EITC    |  795|
| 2018|FALSE       |6 other credits |  250|
| 2019|TRUE        |1 state AGI     | 3679|
| 2019|TRUE        |3 deductions    | 1893|
| 2019|TRUE        |6 other credits | 1303|
| 2019|FALSE       |1 state AGI     | 5212|
| 2019|FALSE       |3 deductions    |  250|
| 2019|FALSE       |5 state EITC    |  898|
| 2019|FALSE       |6 other credits |  241|
| 2020|TRUE        |1 state AGI     | 3745|
| 2020|TRUE        |3 deductions    | 1829|
| 2020|TRUE        |6 other credits | 1249|
| 2020|FALSE       |1 state AGI     | 5750|
| 2020|FALSE       |3 deductions    |  247|
| 2020|FALSE       |5 state EITC    |  826|
| 2020|FALSE       |6 other credits |  222|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

