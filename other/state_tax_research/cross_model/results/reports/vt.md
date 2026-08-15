# Cross-model validation: VT

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.1850|    0.2495|         0.2770|          0.3417|          0.0094|        235.7620|   37032.657|
| 2018|taxsim       | 20515|   13144|   0.2252|    0.3328|         0.3345|          0.4543|          0.0106|        120.7000|   60910.421|
| 2019|taxsim       | 20514|   13088|   0.2275|    0.3375|         0.3379|          0.4594|          0.0123|        117.2000|   57146.461|
| 2020|taxsim       | 20513|   12682|   0.2165|    0.3189|         0.3302|          0.4480|          0.0125|        144.9344|   69970.935|
| 2021|policyengine |  1536|     271|   0.2500|    0.3405|         0.8524|          0.8708|          0.0781|        485.9347|  -11560.363|
| 2022|policyengine |  1530|     317|   0.3033|    0.3536|         0.8423|          0.8454|          0.0765|        429.2398|   -2457.576|
| 2023|policyengine |  1533|     357|   0.2922|    0.3470|         0.8515|          0.8627|          0.0770|        432.0795|   17171.298|
| 2024|policyengine |  1531|     365|   0.2874|    0.3364|         0.8329|          0.8493|          0.0745|        644.9219|  -14599.637|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 8525|
| 2017|TRUE        |4 taxable income |    1|
| 2017|TRUE        |6 other credits  |  939|
| 2017|FALSE       |1 state AGI      | 7253|
| 2018|TRUE        |1 state AGI      | 3704|
| 2018|TRUE        |3 deductions     |  323|
| 2018|TRUE        |5 state EITC     |    5|
| 2018|TRUE        |6 other credits  | 4715|
| 2018|FALSE       |1 state AGI      | 5765|
| 2018|FALSE       |3 deductions     |  111|
| 2018|FALSE       |5 state EITC     | 1217|
| 2018|FALSE       |6 other credits  |   55|
| 2019|TRUE        |1 state AGI      | 3613|
| 2019|TRUE        |3 deductions     | 5052|
| 2019|FALSE       |1 state AGI      | 5766|
| 2019|FALSE       |3 deductions     | 1416|
| 2020|TRUE        |1 state AGI      | 3613|
| 2020|TRUE        |3 deductions     | 1950|
| 2020|TRUE        |5 state EITC     |    2|
| 2020|TRUE        |6 other credits  | 2928|
| 2020|TRUE        |7 rate/rounding  |    1|
| 2020|FALSE       |1 state AGI      | 6261|
| 2020|FALSE       |3 deductions     |  459|
| 2020|FALSE       |5 state EITC     |  833|
| 2020|FALSE       |6 other credits  |   24|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

