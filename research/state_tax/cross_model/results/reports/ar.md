# Cross-model validation: AR

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.2818|    0.4047|         0.3849|          0.5164|          0.1528|        255.2909|    -77.9342|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.2689|    0.4004|         0.3694|          0.5133|          0.1461|        254.4287|   -208.5256|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.3043|    0.4317|         0.3942|          0.5242|          0.1693|        219.5375|   -350.4009|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.3044|    0.4363|         0.4016|          0.5354|          0.1703|        202.3165|  -1299.4705|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.2560|    0.4044|         0.4396|          0.6319|          0.1195|        247.4858|  37534.8476|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.1604|    0.2498|         0.2618|          0.3443|          0.1315|        267.1039|  32958.5728|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.1538|    0.3647|         0.2179|          0.5128|          0.1253|        189.0020|  12346.1795|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.2304|    0.4339|         0.3717|          0.6261|          0.1209|        162.9066|  17599.4777|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4631|
| 2017|TRUE        |3 deductions    |  603|
| 2017|TRUE        |6 other credits |  882|
| 2017|TRUE        |7 rate/rounding |  509|
| 2017|FALSE       |1 state AGI     | 7713|
| 2017|FALSE       |3 deductions    |  213|
| 2017|FALSE       |6 other credits |  351|
| 2017|FALSE       |7 rate/rounding |  620|
| 2018|TRUE        |1 state AGI     | 4738|
| 2018|TRUE        |3 deductions    |  601|
| 2018|TRUE        |6 other credits |  858|
| 2018|TRUE        |7 rate/rounding |  626|
| 2018|FALSE       |1 state AGI     | 7719|
| 2018|FALSE       |3 deductions    |  219|
| 2018|FALSE       |6 other credits |  354|
| 2018|FALSE       |7 rate/rounding |  632|
| 2019|TRUE        |1 state AGI     | 4729|
| 2019|TRUE        |3 deductions    |  601|
| 2019|TRUE        |6 other credits |  838|
| 2019|TRUE        |7 rate/rounding |  445|
| 2019|FALSE       |1 state AGI     | 7570|
| 2019|FALSE       |3 deductions    |  221|
| 2019|FALSE       |6 other credits |  333|
| 2019|FALSE       |7 rate/rounding |  448|
| 2020|TRUE        |1 state AGI     | 4779|
| 2020|TRUE        |3 deductions    |  320|
| 2020|TRUE        |6 other credits |  786|
| 2020|TRUE        |7 rate/rounding |  395|
| 2020|FALSE       |1 state AGI     | 7926|
| 2020|FALSE       |3 deductions    |  145|
| 2020|FALSE       |6 other credits |  385|
| 2020|FALSE       |7 rate/rounding |  440|

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

