# Cross-model validation: MT

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4616|    0.5560|         0.4930|          0.5831|          0.1493|         39.3250|    566.2813|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4654|    0.5677|         0.5002|          0.6038|          0.1439|         33.9964|  -1099.6450|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4409|    0.5620|         0.5034|          0.6063|          0.1138|         46.4127|  -1314.3010|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4357|    0.5499|         0.5141|          0.6163|          0.1136|         47.9692|  -1018.0372|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.1288|    0.1886|         0.2259|          0.3003|          0.0870|       1178.8329|  -1291.6638|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3944|    0.4952|         0.7518|          0.7991|          0.0938|        108.3700|   -562.8310|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.3915|    0.4883|         0.7179|          0.7799|          0.0959|        119.8476|   -778.2142|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     459|   0.4843|    0.6026|         0.8911|          0.9216|          0.1191|         18.5579|   -454.7827|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 4639|
| 2017|TRUE        |2 exemptions     |   35|
| 2017|TRUE        |3 deductions     | 2366|
| 2017|TRUE        |4 taxable income |    4|
| 2017|TRUE        |7 rate/rounding  |    2|
| 2017|FALSE       |1 state AGI      | 5249|
| 2017|FALSE       |2 exemptions     |    2|
| 2017|FALSE       |3 deductions     |  230|
| 2018|TRUE        |1 state AGI      | 4772|
| 2018|TRUE        |2 exemptions     |   28|
| 2018|TRUE        |3 deductions     | 2228|
| 2018|TRUE        |7 rate/rounding  |    1|
| 2018|FALSE       |1 state AGI      | 5262|
| 2018|FALSE       |2 exemptions     |    6|
| 2018|FALSE       |3 deductions     |  222|
| 2018|FALSE       |4 taxable income |    1|
| 2019|TRUE        |1 state AGI      | 4841|
| 2019|TRUE        |2 exemptions     |   21|
| 2019|TRUE        |3 deductions     | 2107|
| 2019|TRUE        |4 taxable income |    4|
| 2019|TRUE        |7 rate/rounding  |    2|
| 2019|FALSE       |1 state AGI      | 5388|
| 2019|FALSE       |2 exemptions     |    3|
| 2019|FALSE       |3 deductions     |  367|
| 2019|FALSE       |4 taxable income |    3|
| 2019|FALSE       |5 state EITC     |  265|
| 2020|TRUE        |1 state AGI      | 4542|
| 2020|TRUE        |2 exemptions     |   33|
| 2020|TRUE        |3 deductions     | 2090|
| 2020|TRUE        |4 taxable income |    3|
| 2020|TRUE        |6 other credits  |    1|
| 2020|TRUE        |7 rate/rounding  |    4|
| 2020|FALSE       |1 state AGI      | 5835|
| 2020|FALSE       |2 exemptions     |    5|
| 2020|FALSE       |3 deductions     |  312|
| 2020|FALSE       |4 taxable income |    4|
| 2020|FALSE       |5 state EITC     |  268|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

