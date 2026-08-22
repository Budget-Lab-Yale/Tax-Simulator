# Cross-model validation: LA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4203|    0.5405|         0.4724|          0.5768|          0.1172|         62.5135|    399.7279|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4814|    0.6295|         0.5524|          0.6920|          0.1106|         19.6276|   -307.4666|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4756|    0.6316|         0.5517|          0.6949|          0.1103|         21.3466|   -441.7896|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4590|    0.6107|         0.5499|          0.6941|          0.1090|         25.8767|   -320.9985|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.3055|    0.4633|         0.6061|          0.7328|          0.0870|        121.3073|   2568.2126|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.5890|    0.7555|         0.8797|          0.9717|          0.0947|          0.1961|    264.1344|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.5609|    0.7338|         0.8723|          0.9681|          0.0942|          0.6079|    264.9444|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     463|   0.5661|    0.7139|         0.8898|          0.9784|          0.0887|          2.2867|    -93.4602|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3521|
| 2017|TRUE        |2 exemptions     | 2488|
| 2017|TRUE        |3 deductions     |  970|
| 2017|TRUE        |4 taxable income |  300|
| 2017|FALSE       |1 state AGI      | 4058|
| 2017|FALSE       |2 exemptions     | 1291|
| 2017|FALSE       |3 deductions     |  411|
| 2017|FALSE       |4 taxable income |   60|
| 2017|FALSE       |5 state EITC     |   90|
| 2018|TRUE        |1 state AGI      | 3374|
| 2018|TRUE        |2 exemptions     | 2158|
| 2018|TRUE        |3 deductions     |  501|
| 2018|TRUE        |4 taxable income |  339|
| 2018|FALSE       |1 state AGI      | 4072|
| 2018|FALSE       |2 exemptions     | 1256|
| 2018|FALSE       |3 deductions     |  329|
| 2018|FALSE       |4 taxable income |  107|
| 2018|FALSE       |5 state EITC     |   70|
| 2019|TRUE        |1 state AGI      | 3330|
| 2019|TRUE        |2 exemptions     | 2199|
| 2019|TRUE        |3 deductions     |  516|
| 2019|TRUE        |4 taxable income |  317|
| 2019|FALSE       |1 state AGI      | 4078|
| 2019|FALSE       |2 exemptions     | 1365|
| 2019|FALSE       |3 deductions     |  297|
| 2019|FALSE       |4 taxable income |  124|
| 2019|FALSE       |5 state EITC     |  151|
| 2020|TRUE        |1 state AGI      | 3337|
| 2020|TRUE        |2 exemptions     | 2063|
| 2020|TRUE        |3 deductions     |  488|
| 2020|TRUE        |4 taxable income |  340|
| 2020|FALSE       |1 state AGI      | 4559|
| 2020|FALSE       |2 exemptions     | 1296|
| 2020|FALSE       |3 deductions     |  306|
| 2020|FALSE       |4 taxable income |  108|
| 2020|FALSE       |5 state EITC     |  159|

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

