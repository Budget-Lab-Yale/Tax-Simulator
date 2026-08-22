# Cross-model validation: IA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.3649|    0.4808|         0.5322|          0.6254|          0.1327|        122.9490|   1346.1887|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.3703|    0.4741|         0.5382|          0.6193|          0.1265|        134.8051|   1780.6156|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.3730|    0.4808|         0.5345|          0.6190|          0.1266|        126.4686|   1492.8423|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.2264|    0.3746|         0.3093|          0.4625|          0.1250|        185.9597|    230.2335|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.2654|    0.4044|         0.5714|          0.6676|          0.1084|        185.0815|   4419.7597|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3585|    0.5153|         0.6927|          0.7991|          0.1192|         84.0000|   2270.8791|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.4564|    0.6335|         0.9000|          0.9787|          0.1677|         34.0848|   -995.1575|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     461|   0.4548|    0.6374|         0.9067|          0.9826|          0.1652|         28.9040|  -1058.5400|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 1764|
| 2017|TRUE        |3 deductions     | 2568|
| 2017|TRUE        |4 taxable income |  638|
| 2017|TRUE        |5 state EITC     |   50|
| 2017|TRUE        |6 other credits  |  102|
| 2017|TRUE        |7 rate/rounding  |   26|
| 2017|FALSE       |1 state AGI      | 3215|
| 2017|FALSE       |3 deductions     | 4665|
| 2017|FALSE       |4 taxable income |  537|
| 2017|FALSE       |5 state EITC     |  557|
| 2017|FALSE       |7 rate/rounding  |    4|
| 2018|TRUE        |1 state AGI      | 1827|
| 2018|TRUE        |3 deductions     | 2538|
| 2018|TRUE        |4 taxable income |  585|
| 2018|TRUE        |5 state EITC     |   75|
| 2018|TRUE        |6 other credits  |   87|
| 2018|TRUE        |7 rate/rounding  |   27|
| 2018|FALSE       |1 state AGI      | 3182|
| 2018|FALSE       |3 deductions     | 4697|
| 2018|FALSE       |4 taxable income |  455|
| 2018|FALSE       |5 state EITC     |  569|
| 2018|FALSE       |6 other credits  |    1|
| 2018|FALSE       |7 rate/rounding  |    4|
| 2019|TRUE        |1 state AGI      | 1814|
| 2019|TRUE        |3 deductions     | 2608|
| 2019|TRUE        |4 taxable income |  564|
| 2019|TRUE        |5 state EITC     |   88|
| 2019|TRUE        |6 other credits  |  112|
| 2019|TRUE        |7 rate/rounding  |   16|
| 2019|FALSE       |1 state AGI      | 3058|
| 2019|FALSE       |3 deductions     | 4677|
| 2019|FALSE       |4 taxable income |  471|
| 2019|FALSE       |5 state EITC     |  642|
| 2019|FALSE       |6 other credits  |    1|
| 2019|FALSE       |7 rate/rounding  |    4|
| 2020|TRUE        |1 state AGI      | 1860|
| 2020|TRUE        |3 deductions     | 2479|
| 2020|TRUE        |4 taxable income | 2634|
| 2020|TRUE        |5 state EITC     |   98|
| 2020|TRUE        |6 other credits  |   83|
| 2020|TRUE        |7 rate/rounding  |   12|
| 2020|FALSE       |1 state AGI      | 3632|
| 2020|FALSE       |3 deductions     | 4582|
| 2020|FALSE       |4 taxable income |  526|
| 2020|FALSE       |5 state EITC     |  606|
| 2020|FALSE       |6 other credits  |    1|
| 2020|FALSE       |7 rate/rounding  |    2|

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

