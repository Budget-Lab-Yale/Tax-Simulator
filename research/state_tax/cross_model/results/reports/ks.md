# Cross-model validation: KS

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6513|   0.4365|    0.5915|         0.8970|          0.9085|          0.1150|         39.7504|    286.3435|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    7804|   0.5206|    0.6626|         0.8328|          0.8812|          0.1050|          9.1336|    187.9419|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    7828|   0.5315|    0.6580|         0.8540|          0.8827|          0.1052|          7.1391|    887.9423|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    7455|   0.5306|    0.6408|         0.8592|          0.8871|          0.1068|          7.0455|   1148.1417|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     365|   0.4113|    0.6212|         0.8055|          0.8712|          0.0922|         24.6296|    802.4835|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.5557|    0.6582|         0.8042|          0.8514|          0.1043|          2.3246|    303.8459|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5653|    0.6517|         0.8248|          0.8590|          0.1054|          2.4396|    343.2437|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     459|   0.5522|    0.6435|         0.8519|          0.8780|          0.1122|          3.5604|   -192.2008|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  219|
| 2017|TRUE        |2 exemptions    |   18|
| 2017|TRUE        |3 deductions    |   98|
| 2017|TRUE        |6 other credits |  396|
| 2017|FALSE       |1 state AGI     | 8568|
| 2017|FALSE       |2 exemptions    |  319|
| 2017|FALSE       |3 deductions    | 2308|
| 2017|FALSE       |5 state EITC    | 1041|
| 2017|FALSE       |6 other credits |    6|
| 2018|TRUE        |1 state AGI     |  678|
| 2018|TRUE        |2 exemptions    |   20|
| 2018|TRUE        |3 deductions    |  219|
| 2018|TRUE        |6 other credits |  609|
| 2018|FALSE       |1 state AGI     | 7618|
| 2018|FALSE       |2 exemptions    |  334|
| 2018|FALSE       |3 deductions    | 1098|
| 2018|FALSE       |5 state EITC    | 1019|
| 2018|FALSE       |6 other credits |   15|
| 2019|TRUE        |1 state AGI     |  648|
| 2019|TRUE        |2 exemptions    |   19|
| 2019|TRUE        |3 deductions    |  171|
| 2019|TRUE        |6 other credits |  507|
| 2019|FALSE       |1 state AGI     | 7743|
| 2019|FALSE       |2 exemptions    |  324|
| 2019|FALSE       |3 deductions    | 1025|
| 2019|FALSE       |5 state EITC    | 1060|
| 2019|FALSE       |6 other credits |   10|
| 2020|TRUE        |1 state AGI     |  605|
| 2020|TRUE        |2 exemptions    |   19|
| 2020|TRUE        |3 deductions    |  166|
| 2020|TRUE        |6 other credits |  459|
| 2020|FALSE       |1 state AGI     | 8026|
| 2020|FALSE       |2 exemptions    |  308|
| 2020|FALSE       |3 deductions    |  952|
| 2020|FALSE       |5 state EITC    |  978|
| 2020|FALSE       |6 other credits |   16|

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

