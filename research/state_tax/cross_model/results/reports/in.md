# Cross-model validation: IN

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.3872|    0.7781|         0.4721|          0.8560|          0.0566|         48.4536|    105.4501|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.3902|    0.7791|         0.4758|          0.8585|          0.0575|         48.4496|    107.3469|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.3903|    0.7853|         0.4751|          0.8678|          0.0561|         48.4486|    106.8027|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.3796|    0.7601|         0.4737|          0.8598|          0.0566|         54.1561|    124.5542|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     262|   0.3106|    0.6689|         0.6221|          0.8969|          0.0589|         66.8305|    617.7881|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     302|   0.3444|    0.7055|         0.5894|          0.9338|          0.0517|         56.1527|    282.0567|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     335|   0.3526|    0.7070|         0.6806|          0.9433|          0.0570|         53.6713|    289.0586|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     347|   0.3591|    0.6852|         0.6888|          0.9510|          0.0557|         64.5771|     19.4155|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 4736|
| 2017|TRUE        |2 exemptions     | 1107|
| 2017|TRUE        |4 taxable income | 1223|
| 2017|TRUE        |5 state EITC     |  212|
| 2017|FALSE       |1 state AGI      | 5458|
| 2017|FALSE       |2 exemptions     |   92|
| 2017|FALSE       |4 taxable income |   35|
| 2017|FALSE       |5 state EITC     |  888|
| 2018|TRUE        |1 state AGI      | 4943|
| 2018|TRUE        |2 exemptions     | 1069|
| 2018|TRUE        |4 taxable income | 1111|
| 2018|TRUE        |5 state EITC     |  171|
| 2018|TRUE        |6 other credits  |    1|
| 2018|FALSE       |1 state AGI      | 5451|
| 2018|FALSE       |2 exemptions     |   99|
| 2018|FALSE       |4 taxable income |   54|
| 2018|FALSE       |5 state EITC     |  835|
| 2019|TRUE        |1 state AGI      | 4894|
| 2019|TRUE        |2 exemptions     | 1071|
| 2019|TRUE        |4 taxable income | 1123|
| 2019|TRUE        |5 state EITC     |  193|
| 2019|FALSE       |1 state AGI      | 5459|
| 2019|FALSE       |2 exemptions     |  112|
| 2019|FALSE       |4 taxable income |   52|
| 2019|FALSE       |5 state EITC     |  884|
| 2020|TRUE        |1 state AGI      | 4797|
| 2020|TRUE        |2 exemptions     | 1044|
| 2020|TRUE        |4 taxable income | 1095|
| 2020|TRUE        |5 state EITC     |  183|
| 2020|FALSE       |1 state AGI      | 5858|
| 2020|FALSE       |2 exemptions     |  110|
| 2020|FALSE       |4 taxable income |   48|
| 2020|FALSE       |5 state EITC     |  850|

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

