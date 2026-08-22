# Cross-model validation: NJ

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.4899|    0.6030|         0.6485|          0.7397|          0.1023|         19.7414|    3378.548|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.5025|    0.6117|         0.6658|          0.7479|          0.1128|         13.3700|    6270.099|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.5015|    0.6035|         0.6569|          0.7434|          0.1178|         13.9347|    5550.681|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.4881|    0.5794|         0.6540|          0.7306|          0.1179|         22.9428|    9341.546|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.4394|    0.5913|         0.6126|          0.7445|          0.0597|         50.0000|    3262.612|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.5057|    0.6468|         0.6690|          0.8369|          0.0719|         11.5134|    3479.337|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5281|    0.6759|         0.6731|          0.8248|          0.0709|          0.1992|    1436.185|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.5226|    0.6548|         0.7152|          0.8326|          0.0600|          3.0054|    2597.680|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3217|
| 2017|TRUE        |2 exemptions     |  664|
| 2017|TRUE        |4 taxable income |   54|
| 2017|TRUE        |6 other credits  |    1|
| 2017|TRUE        |7 rate/rounding  |   66|
| 2017|FALSE       |1 state AGI      | 6647|
| 2017|FALSE       |2 exemptions     |  654|
| 2017|FALSE       |4 taxable income |   61|
| 2017|FALSE       |5 state EITC     |  625|
| 2017|FALSE       |7 rate/rounding  |   33|
| 2018|TRUE        |1 state AGI      | 3200|
| 2018|TRUE        |2 exemptions     |  568|
| 2018|TRUE        |4 taxable income |   46|
| 2018|TRUE        |7 rate/rounding  |   80|
| 2018|FALSE       |1 state AGI      | 6590|
| 2018|FALSE       |2 exemptions     |  604|
| 2018|FALSE       |4 taxable income |   61|
| 2018|FALSE       |5 state EITC     |  668|
| 2018|FALSE       |7 rate/rounding  |   68|
| 2019|TRUE        |1 state AGI      | 3299|
| 2019|TRUE        |2 exemptions     |  572|
| 2019|TRUE        |4 taxable income |   51|
| 2019|TRUE        |7 rate/rounding  |   89|
| 2019|FALSE       |1 state AGI      | 6548|
| 2019|FALSE       |2 exemptions     |  621|
| 2019|FALSE       |4 taxable income |   59|
| 2019|FALSE       |5 state EITC     |  657|
| 2019|FALSE       |7 rate/rounding  |   63|
| 2020|TRUE        |1 state AGI      | 3158|
| 2020|TRUE        |2 exemptions     |  546|
| 2020|TRUE        |4 taxable income |   45|
| 2020|TRUE        |7 rate/rounding  |  165|
| 2020|FALSE       |1 state AGI      | 6787|
| 2020|FALSE       |2 exemptions     |  573|
| 2020|FALSE       |4 taxable income |   52|
| 2020|FALSE       |5 state EITC     |  688|
| 2020|FALSE       |7 rate/rounding  |  236|

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

