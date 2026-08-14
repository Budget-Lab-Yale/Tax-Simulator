# Cross-model validation: OK

Class: broad | Generated: 2026-08-13 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.1779|    0.4361|         0.2563|          0.5356|          0.0152|        156.8468|    7593.468|
| 2018|taxsim       | 20515|   13144|   0.3184|    0.5924|         0.4514|          0.7272|          0.0107|         54.7107|   -9239.174|
| 2019|taxsim       | 20514|   13088|   0.3166|    0.5846|         0.4519|          0.7194|          0.0118|         59.4596|   -9772.042|
| 2020|taxsim       | 20513|   12682|   0.3187|    0.5719|         0.4664|          0.7201|          0.0115|         70.0722|   -9881.385|
| 2021|policyengine |  1536|     269|   0.2207|    0.4095|         0.4647|          0.8030|          0.0417|        166.5009|   -5713.972|
| 2022|policyengine |  1530|     317|   0.2471|    0.4085|         0.4984|          0.8423|          0.0359|        166.5014|    2217.162|
| 2023|policyengine |  1533|     358|   0.2427|    0.4116|         0.5140|          0.8603|          0.0307|        166.5027|   14919.102|
| 2024|policyengine |  1531|     365|   0.2560|    0.4069|         0.5671|          0.8630|          0.0405|        188.5058|    -452.437|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 1214|
| 2017|TRUE        |2 exemptions     |   61|
| 2017|TRUE        |3 deductions     | 5698|
| 2017|TRUE        |4 taxable income |  269|
| 2017|TRUE        |5 state EITC     |   23|
| 2017|TRUE        |6 other credits  | 2472|
| 2017|FALSE       |1 state AGI      | 5585|
| 2017|FALSE       |2 exemptions     |    3|
| 2017|FALSE       |3 deductions     |  338|
| 2017|FALSE       |4 taxable income |   13|
| 2017|FALSE       |5 state EITC     |  993|
| 2017|FALSE       |6 other credits  |  194|
| 2018|TRUE        |1 state AGI      | 1085|
| 2018|TRUE        |2 exemptions     |   65|
| 2018|TRUE        |3 deductions     | 3472|
| 2018|TRUE        |4 taxable income |    5|
| 2018|TRUE        |5 state EITC     |   30|
| 2018|TRUE        |6 other credits  | 2554|
| 2018|FALSE       |1 state AGI      | 5339|
| 2018|FALSE       |2 exemptions     |    8|
| 2018|FALSE       |3 deductions     |  255|
| 2018|FALSE       |5 state EITC     |  973|
| 2018|FALSE       |6 other credits  |  197|
| 2019|TRUE        |1 state AGI      | 1068|
| 2019|TRUE        |2 exemptions     |   51|
| 2019|TRUE        |3 deductions     | 3510|
| 2019|TRUE        |4 taxable income |    7|
| 2019|TRUE        |5 state EITC     |   23|
| 2019|TRUE        |6 other credits  | 2514|
| 2019|FALSE       |1 state AGI      | 5366|
| 2019|FALSE       |2 exemptions     |    7|
| 2019|FALSE       |3 deductions     |  298|
| 2019|FALSE       |5 state EITC     | 1000|
| 2019|FALSE       |6 other credits  |  176|
| 2020|TRUE        |1 state AGI      | 1033|
| 2020|TRUE        |2 exemptions     |   66|
| 2020|TRUE        |3 deductions     | 3263|
| 2020|TRUE        |4 taxable income |    8|
| 2020|TRUE        |5 state EITC     |   25|
| 2020|TRUE        |6 other credits  | 2372|
| 2020|FALSE       |1 state AGI      | 5848|
| 2020|FALSE       |2 exemptions     |    6|
| 2020|FALSE       |3 deductions     |  244|
| 2020|FALSE       |4 taxable income |    1|
| 2020|FALSE       |5 state EITC     |  929|
| 2020|FALSE       |6 other credits  |  181|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

