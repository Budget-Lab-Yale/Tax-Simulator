# Cross-model validation: AR

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6559|   0.2818|    0.4047|         0.5652|          0.7237|          0.1528|        255.2909|    -77.9342|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    7897|   0.2689|    0.4004|         0.4550|          0.6156|          0.1461|        254.4287|   -208.5256|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    7818|   0.3043|    0.4317|         0.4914|          0.6349|          0.1693|        219.5375|   -350.4009|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    7878|   0.3044|    0.4363|         0.4747|          0.6210|          0.1703|        202.3165|  -1299.4705|
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

| year|fed_aligned |stage           |     n|
|----:|:-----------|:---------------|-----:|
| 2017|TRUE        |1 state AGI     |  1621|
| 2017|TRUE        |3 deductions    |     7|
| 2017|TRUE        |6 other credits |   788|
| 2017|TRUE        |7 rate/rounding |   495|
| 2017|FALSE       |1 state AGI     | 10723|
| 2017|FALSE       |3 deductions    |   809|
| 2017|FALSE       |6 other credits |   445|
| 2017|FALSE       |7 rate/rounding |   634|
| 2018|TRUE        |1 state AGI     |  2996|
| 2018|TRUE        |3 deductions    |   195|
| 2018|TRUE        |6 other credits |   750|
| 2018|TRUE        |7 rate/rounding |   584|
| 2018|FALSE       |1 state AGI     |  9461|
| 2018|FALSE       |3 deductions    |   625|
| 2018|FALSE       |6 other credits |   462|
| 2018|FALSE       |7 rate/rounding |   674|
| 2019|TRUE        |1 state AGI     |  2872|
| 2019|TRUE        |3 deductions    |   193|
| 2019|TRUE        |6 other credits |   746|
| 2019|TRUE        |7 rate/rounding |   407|
| 2019|FALSE       |1 state AGI     |  9427|
| 2019|FALSE       |3 deductions    |   629|
| 2019|FALSE       |6 other credits |   425|
| 2019|FALSE       |7 rate/rounding |   486|
| 2020|TRUE        |1 state AGI     |  3263|
| 2020|TRUE        |3 deductions    |    75|
| 2020|TRUE        |6 other credits |   685|
| 2020|TRUE        |7 rate/rounding |   355|
| 2020|FALSE       |1 state AGI     |  9442|
| 2020|FALSE       |3 deductions    |   390|
| 2020|FALSE       |6 other credits |   486|
| 2020|FALSE       |7 rate/rounding |   480|

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

