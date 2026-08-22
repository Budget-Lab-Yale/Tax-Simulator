# Cross-model validation: AL

Class:  | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.3932|    0.5229|         0.4988|          0.6463|          0.1450|         72.6788|   2566.2654|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.3953|    0.5270|         0.5012|          0.6525|          0.1390|         72.4177|   2481.2555|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.4025|    0.5323|         0.5022|          0.6476|          0.1389|         67.9101|   2357.2580|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.3892|    0.5222|         0.4925|          0.6408|          0.1456|         78.3087|   2320.0505|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.4539|    0.5973|         0.7686|          0.8512|          0.1553|         28.9082|   2888.0854|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.4619|    0.6047|         0.7005|          0.8042|          0.1446|         27.3610|  12958.4333|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.4373|    0.5929|         0.6560|          0.7650|          0.1357|         40.4799|    966.3857|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4374|    0.5791|         0.6587|          0.7739|          0.1443|         34.2897|   2927.7533|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2709|
| 2017|TRUE        |2 exemptions     |  813|
| 2017|TRUE        |3 deductions     | 1733|
| 2017|TRUE        |4 taxable income |  216|
| 2017|FALSE       |1 state AGI      | 6868|
| 2017|FALSE       |2 exemptions     |  380|
| 2017|FALSE       |3 deductions     |  786|
| 2017|FALSE       |4 taxable income |   62|
| 2018|TRUE        |1 state AGI      | 2698|
| 2018|TRUE        |2 exemptions     |  766|
| 2018|TRUE        |3 deductions     | 1776|
| 2018|TRUE        |4 taxable income |  265|
| 2018|FALSE       |1 state AGI      | 6875|
| 2018|FALSE       |2 exemptions     |  348|
| 2018|FALSE       |3 deductions     |  766|
| 2018|FALSE       |4 taxable income |   65|
| 2019|TRUE        |1 state AGI      | 2615|
| 2019|TRUE        |2 exemptions     |  760|
| 2019|TRUE        |3 deductions     | 1905|
| 2019|TRUE        |4 taxable income |  245|
| 2019|FALSE       |1 state AGI      | 6762|
| 2019|FALSE       |2 exemptions     |  355|
| 2019|FALSE       |3 deductions     |  784|
| 2019|FALSE       |4 taxable income |   62|
| 2020|TRUE        |1 state AGI      | 3459|
| 2020|TRUE        |2 exemptions     |  622|
| 2020|TRUE        |3 deductions     | 1092|
| 2020|TRUE        |4 taxable income |  240|
| 2020|FALSE       |1 state AGI      | 7228|
| 2020|FALSE       |2 exemptions     |  378|
| 2020|FALSE       |3 deductions     |  558|
| 2020|FALSE       |4 taxable income |  125|

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

