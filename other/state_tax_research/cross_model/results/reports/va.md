# Cross-model validation: VA

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.4370|    0.5005|         0.5317|          0.5909|          0.2217|         99.4047|    9749.546|
| 2018|taxsim       | 20515|   13504|   0.5308|    0.5992|         0.6678|          0.7125|          0.2122|          1.2829|    9891.858|
| 2019|taxsim       | 20514|   13433|   0.5237|    0.5778|         0.6588|          0.6969|          0.2162|          2.1376|   16826.418|
| 2020|taxsim       | 20513|   13070|   0.5256|    0.5801|         0.6549|          0.6907|          0.2130|          2.1842|   18044.711|
| 2021|policyengine |  1536|     269|   0.2005|    0.2305|         0.4944|          0.4944|          0.1966|        267.2884|   -9734.853|
| 2022|policyengine |  1530|     316|   0.3850|    0.5007|         0.8987|          0.9399|          0.1072|         98.7486|    1656.090|
| 2023|policyengine |  1533|     357|   0.1239|    0.1755|         0.3305|          0.3445|          0.1063|        280.1290|   15959.911|
| 2024|policyengine |  1531|     364|   0.1248|    0.1731|         0.3269|          0.3489|          0.1012|        299.0597|   -2704.847|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  642|
| 2017|TRUE        |2 exemptions     |   53|
| 2017|TRUE        |3 deductions     | 5408|
| 2017|TRUE        |4 taxable income |  135|
| 2017|TRUE        |6 other credits  |   49|
| 2017|TRUE        |7 rate/rounding  |   15|
| 2017|FALSE       |1 state AGI      | 4675|
| 2017|FALSE       |3 deductions     |  211|
| 2017|FALSE       |4 taxable income |   37|
| 2017|FALSE       |5 state EITC     |  320|
| 2017|FALSE       |6 other credits  |    3|
| 2018|TRUE        |1 state AGI      |  559|
| 2018|TRUE        |2 exemptions     |   56|
| 2018|TRUE        |3 deductions     | 3609|
| 2018|TRUE        |4 taxable income |  167|
| 2018|TRUE        |6 other credits  |   69|
| 2018|TRUE        |7 rate/rounding  |   26|
| 2018|FALSE       |1 state AGI      | 4556|
| 2018|FALSE       |3 deductions     |  163|
| 2018|FALSE       |4 taxable income |   18|
| 2018|FALSE       |5 state EITC     |  400|
| 2018|FALSE       |6 other credits  |    2|
| 2018|FALSE       |7 rate/rounding  |    1|
| 2019|TRUE        |1 state AGI      |  219|
| 2019|TRUE        |2 exemptions     |   47|
| 2019|TRUE        |3 deductions     | 4059|
| 2019|TRUE        |4 taxable income |  151|
| 2019|TRUE        |6 other credits  |   70|
| 2019|TRUE        |7 rate/rounding  |   37|
| 2019|FALSE       |1 state AGI      | 4650|
| 2019|FALSE       |2 exemptions     |    1|
| 2019|FALSE       |3 deductions     |  183|
| 2019|FALSE       |4 taxable income |   24|
| 2019|FALSE       |5 state EITC     |  328|
| 2019|FALSE       |6 other credits  |    2|
| 2020|TRUE        |1 state AGI      |  219|
| 2020|TRUE        |2 exemptions     |   58|
| 2020|TRUE        |3 deductions     | 3965|
| 2020|TRUE        |4 taxable income |  171|
| 2020|TRUE        |6 other credits  |   75|
| 2020|TRUE        |7 rate/rounding  |   22|
| 2020|FALSE       |1 state AGI      | 4645|
| 2020|FALSE       |2 exemptions     |    4|
| 2020|FALSE       |3 deductions     |  177|
| 2020|FALSE       |4 taxable income |   31|
| 2020|FALSE       |5 state EITC     |  360|
| 2020|FALSE       |6 other credits  |    3|
| 2020|FALSE       |7 rate/rounding  |    1|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

