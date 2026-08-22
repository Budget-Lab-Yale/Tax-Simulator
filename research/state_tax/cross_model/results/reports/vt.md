# Cross-model validation: VT

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6506|   0.2189|    0.2965|         0.5078|          0.5309|          0.0113|        132.4199|   5471.1895|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    8830|   0.2729|    0.3935|         0.4446|          0.5421|          0.0128|        111.0000|   6346.7926|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    8755|   0.2847|    0.4050|         0.4593|          0.5584|          0.0149|        111.0000|   4762.3207|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    8421|   0.2600|    0.3792|         0.4369|          0.5447|          0.0152|        111.0000|   5316.9009|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.4189|    0.5956|         0.8512|          0.8871|          0.0964|         35.0767|   1202.0483|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.5057|    0.6004|         0.8396|          0.8585|          0.0911|         11.4006|    348.5083|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5108|    0.6059|         0.8697|          0.8846|          0.0959|         10.9104|    285.7669|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4974|    0.6035|         0.8630|          0.8935|          0.0904|         17.1151|   -289.4678|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |     n|
|----:|:-----------|:----------------|-----:|
| 2017|TRUE        |1 state AGI      |  2379|
| 2017|TRUE        |4 taxable income |     1|
| 2017|TRUE        |6 other credits  |   939|
| 2017|FALSE       |1 state AGI      | 13488|
| 2018|TRUE        |1 state AGI      |  1852|
| 2018|TRUE        |3 deductions     |    58|
| 2018|TRUE        |5 state EITC     |     5|
| 2018|TRUE        |6 other credits  |  3372|
| 2018|FALSE       |1 state AGI      |  8210|
| 2018|FALSE       |3 deductions     |   362|
| 2018|FALSE       |5 state EITC     |  1195|
| 2018|FALSE       |6 other credits  |   887|
| 2019|TRUE        |1 state AGI      |  1912|
| 2019|TRUE        |3 deductions     |  3230|
| 2019|FALSE       |1 state AGI      |  8250|
| 2019|FALSE       |3 deductions     |  2401|
| 2020|TRUE        |1 state AGI      |  1919|
| 2020|TRUE        |3 deductions     |  1154|
| 2020|TRUE        |5 state EITC     |     2|
| 2020|TRUE        |6 other credits  |  2057|
| 2020|FALSE       |1 state AGI      |  8775|
| 2020|FALSE       |3 deductions     |   927|
| 2020|FALSE       |5 state EITC     |   806|
| 2020|FALSE       |6 other credits  |   552|

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

