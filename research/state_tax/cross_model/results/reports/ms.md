# Cross-model validation: MS

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4401|    0.5063|         0.4563|          0.5284|          0.2315|         89.9935|    770.8470|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4360|    0.5347|         0.4573|          0.5651|          0.2363|         64.8649|   -484.7712|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4477|    0.5376|         0.4661|          0.5626|          0.2463|         57.8320|   -705.7590|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4388|    0.5270|         0.4624|          0.5589|          0.2451|         68.0972|   -549.7885|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.4881|    0.6058|         0.7555|          0.8516|          0.2346|         23.8209|   -533.1852|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.4785|    0.5925|         0.7075|          0.8113|          0.2296|         30.7122|  17137.7803|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.4598|    0.5834|         0.6766|          0.7872|          0.2645|         42.9910|   -371.0977|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4600|    0.5670|         0.6543|          0.7522|          0.2557|         63.2194|     30.3184|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4629|
| 2017|TRUE        |2 exemptions    |  322|
| 2017|TRUE        |3 deductions    | 2105|
| 2017|TRUE        |7 rate/rounding |  416|
| 2017|FALSE       |1 state AGI     | 4694|
| 2017|FALSE       |2 exemptions    |   82|
| 2017|FALSE       |3 deductions    |  559|
| 2017|FALSE       |7 rate/rounding |   58|
| 2018|TRUE        |1 state AGI     | 4619|
| 2018|TRUE        |2 exemptions    |  322|
| 2018|TRUE        |3 deductions    | 2069|
| 2018|TRUE        |7 rate/rounding |  504|
| 2018|FALSE       |1 state AGI     | 4787|
| 2018|FALSE       |2 exemptions    |   85|
| 2018|FALSE       |3 deductions    |  518|
| 2018|FALSE       |7 rate/rounding |   60|
| 2019|TRUE        |1 state AGI     | 4453|
| 2019|TRUE        |2 exemptions    |  325|
| 2019|TRUE        |3 deductions    | 2098|
| 2019|TRUE        |7 rate/rounding |  506|
| 2019|FALSE       |1 state AGI     | 4774|
| 2019|FALSE       |2 exemptions    |   83|
| 2019|FALSE       |3 deductions    |  519|
| 2019|FALSE       |7 rate/rounding |   80|
| 2020|TRUE        |1 state AGI     | 5527|
| 2020|TRUE        |2 exemptions    |  262|
| 2020|TRUE        |3 deductions    | 1073|
| 2020|TRUE        |7 rate/rounding |  365|
| 2020|FALSE       |1 state AGI     | 5155|
| 2020|FALSE       |2 exemptions    |  101|
| 2020|FALSE       |3 deductions    |  362|
| 2020|FALSE       |7 rate/rounding |  130|

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

