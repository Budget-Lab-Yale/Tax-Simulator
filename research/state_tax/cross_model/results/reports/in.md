# Cross-model validation: IN

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.4009|    0.8727|         0.5643|          0.9557|          0.0566|         43.5575|     61.3375|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.4046|    0.8735|         0.5680|          0.9605|          0.0575|         43.1743|     55.8738|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.4045|    0.8708|         0.5614|          0.9607|          0.0561|         44.0000|     62.4048|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.3953|    0.8525|         0.5649|          0.9614|          0.0566|         48.3928|     78.9382|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     365|   0.3473|    0.6980|         0.6301|          0.8959|          0.0623|         48.4504|    592.4961|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3839|    0.7415|         0.6123|          0.9527|          0.0526|         40.3796|    263.7981|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.3578|    0.7174|         0.6859|          0.9530|          0.0553|         45.9769|    267.9274|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.3722|    0.6974|         0.6913|          0.9565|          0.0557|         45.7287|     -1.8590|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2245|
| 2017|TRUE        |2 exemptions     | 1110|
| 2017|TRUE        |4 taxable income | 1272|
| 2017|TRUE        |5 state EITC     |  212|
| 2017|FALSE       |1 state AGI      | 6713|
| 2017|FALSE       |2 exemptions     |  174|
| 2017|FALSE       |4 taxable income |  861|
| 2017|FALSE       |5 state EITC     |  895|
| 2018|TRUE        |1 state AGI      | 2453|
| 2018|TRUE        |2 exemptions     | 1076|
| 2018|TRUE        |4 taxable income | 1166|
| 2018|TRUE        |5 state EITC     |  172|
| 2018|TRUE        |6 other credits  |    1|
| 2018|FALSE       |1 state AGI      | 6687|
| 2018|FALSE       |2 exemptions     |  179|
| 2018|FALSE       |4 taxable income |  871|
| 2018|FALSE       |5 state EITC     |  844|
| 2019|TRUE        |1 state AGI      | 2507|
| 2019|TRUE        |2 exemptions     | 1078|
| 2019|TRUE        |4 taxable income | 1168|
| 2019|TRUE        |5 state EITC     |  194|
| 2019|FALSE       |1 state AGI      | 6711|
| 2019|FALSE       |2 exemptions     |  189|
| 2019|FALSE       |4 taxable income |  756|
| 2019|FALSE       |5 state EITC     |  897|
| 2020|TRUE        |1 state AGI      | 2377|
| 2020|TRUE        |2 exemptions     | 1051|
| 2020|TRUE        |4 taxable income | 1141|
| 2020|TRUE        |5 state EITC     |  184|
| 2020|FALSE       |1 state AGI      | 7109|
| 2020|FALSE       |2 exemptions     |  171|
| 2020|FALSE       |4 taxable income |  762|
| 2020|FALSE       |5 state EITC     |  856|

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

