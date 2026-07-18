# Cross-model validation: MI

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.3554|    0.4705|         0.4645|          0.5743|          0.0229|        140.8100|    2476.247|
| 2018|taxsim       | 20515|   13504|   0.3460|    0.4617|         0.4542|          0.5657|          0.0228|        153.7136|    2547.995|
| 2019|taxsim       | 20514|   13433|   0.3478|    0.4584|         0.4639|          0.5660|          0.0232|        163.9119|    2555.405|
| 2020|taxsim       | 20513|   13070|   0.3288|    0.4388|         0.4500|          0.5572|          0.0241|        204.9200|    2934.310|
| 2021|policyengine |  1536|     269|   0.2923|    0.4160|         0.4981|          0.5613|          0.0085|        220.3084|   -2735.638|
| 2022|policyengine |  1530|     318|   0.2908|    0.3843|         0.4497|          0.5063|          0.0131|        343.2626|    2290.148|
| 2023|policyengine |  1533|     358|   0.2851|    0.4025|         0.5223|          0.6257|          0.0130|        247.8027|   11851.943|
| 2024|policyengine |  1531|     365|   0.2907|    0.3880|         0.5479|          0.6137|          0.0131|        302.1200|   -1353.177|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4053|
| 2017|TRUE        |2 exemptions    | 3154|
| 2017|FALSE       |1 state AGI     | 4788|
| 2017|FALSE       |2 exemptions    | 1228|
| 2018|TRUE        |1 state AGI     | 4174|
| 2018|TRUE        |3 deductions    | 1866|
| 2018|TRUE        |6 other credits | 1330|
| 2018|FALSE       |1 state AGI     | 4870|
| 2018|FALSE       |3 deductions    |  208|
| 2018|FALSE       |5 state EITC    |  791|
| 2018|FALSE       |6 other credits |  178|
| 2019|TRUE        |1 state AGI     | 4003|
| 2019|TRUE        |3 deductions    | 1895|
| 2019|TRUE        |6 other credits | 1303|
| 2019|FALSE       |1 state AGI     | 4895|
| 2019|FALSE       |3 deductions    |  217|
| 2019|FALSE       |5 state EITC    |  893|
| 2019|FALSE       |6 other credits |  174|
| 2020|TRUE        |1 state AGI     | 4107|
| 2020|TRUE        |3 deductions    | 1832|
| 2020|TRUE        |6 other credits | 1250|
| 2020|FALSE       |1 state AGI     | 5386|
| 2020|FALSE       |3 deductions    |  205|
| 2020|FALSE       |5 state EITC    |  819|
| 2020|FALSE       |6 other credits |  170|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

