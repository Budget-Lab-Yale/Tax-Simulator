# Cross-model validation: OK

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6549|   0.2081|    0.5136|         0.4555|          0.8884|          0.0172|         84.9968|    972.7218|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 12929|    7603|   0.4471|    0.8230|         0.5182|          0.9011|          0.0157|         36.9797|     54.5076|
| 2018|taxsim       |  7586|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 12798|    7637|   0.4511|    0.8200|         0.5269|          0.9021|          0.0173|         36.7837|     47.0309|
| 2019|taxsim       |  7716|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 13123|    7313|   0.4441|    0.7953|         0.5338|          0.9007|          0.0168|         38.7285|     61.3041|
| 2020|taxsim       |  7390|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.3302|    0.6792|         0.4683|          0.8402|          0.0469|         48.7509|    690.4917|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3891|    0.7117|         0.5035|          0.9031|          0.0351|         40.0034|    245.7216|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.3742|    0.7018|         0.4979|          0.8936|          0.0346|         47.4953|    264.4813|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.3983|    0.7026|         0.5630|          0.9065|          0.0461|         40.0000|   -175.2999|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 1265|
| 2017|TRUE        |2 exemptions     |    8|
| 2017|TRUE        |3 deductions     |  128|
| 2017|TRUE        |5 state EITC     |   21|
| 2017|TRUE        |6 other credits  | 2253|
| 2017|FALSE       |1 state AGI      | 9254|
| 2017|FALSE       |2 exemptions     |   37|
| 2017|FALSE       |3 deductions     | 2773|
| 2017|FALSE       |4 taxable income |   64|
| 2017|FALSE       |5 state EITC     |  963|
| 2017|FALSE       |6 other credits  |  190|
| 2018|TRUE        |1 state AGI      | 1625|
| 2018|TRUE        |2 exemptions     |    6|
| 2018|TRUE        |3 deductions     |  266|
| 2018|TRUE        |5 state EITC     |   27|
| 2018|TRUE        |6 other credits  | 2251|
| 2018|FALSE       |1 state AGI      | 7861|
| 2018|FALSE       |2 exemptions     |   37|
| 2018|FALSE       |3 deductions     | 1354|
| 2018|FALSE       |5 state EITC     |  943|
| 2018|FALSE       |6 other credits  |  218|
| 2019|TRUE        |1 state AGI      | 1656|
| 2019|TRUE        |2 exemptions     |    6|
| 2019|TRUE        |3 deductions     |  256|
| 2019|TRUE        |5 state EITC     |   21|
| 2019|TRUE        |6 other credits  | 2197|
| 2019|FALSE       |1 state AGI      | 7966|
| 2019|FALSE       |2 exemptions     |   30|
| 2019|FALSE       |3 deductions     | 1308|
| 2019|FALSE       |5 state EITC     |  971|
| 2019|FALSE       |6 other credits  |  179|
| 2020|TRUE        |1 state AGI      | 1558|
| 2020|TRUE        |2 exemptions     |    9|
| 2020|TRUE        |3 deductions     |  227|
| 2020|TRUE        |5 state EITC     |   24|
| 2020|TRUE        |6 other credits  | 2071|
| 2020|FALSE       |1 state AGI      | 8389|
| 2020|FALSE       |2 exemptions     |   36|
| 2020|FALSE       |3 deductions     | 1209|
| 2020|FALSE       |4 taxable income |    2|
| 2020|FALSE       |5 state EITC     |  897|
| 2020|FALSE       |6 other credits  |  192|

## Known differences applied

|state |model  | year_min| year_max|category                    |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:-----|:------|--------:|--------:|:---------------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural                  |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2017|     2024|structural                  |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2021|     2024|vintage                     |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2017|     2024|input-coverage              |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|ALL   |taxsim |     2017|     2024|input-coverage              |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|federal-side                |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|OK    |taxsim |     2018|     2020|external-deduction-artifact |exclude  |TAXSIM applies Oklahoma's $17,000 itemized cap (68 O.S. 2358(D)(1), TY2018+) as a FLAT cap, without the statutory exemptions for charitable contributions and medical expenses. v35_state_itemized_deduction equals exactly 17,000 on 91% of OK itemizer records in every cap year, and "our itemized = TAXSIM 17,000 + charity + medical" holds exactly to the dollar on 69% of them (median residual 0). TY2017 is the control: no cap existed, TAXSIM never sits at 17,000, and the identity has no hits. TAXSIM therefore runs HIGH. The predicate keys on the failure itself -- TAXSIM pinned at the flat cap while ours exceeds it, which is only possible when the exemptions were dropped. Excluding lifts OK from 0.727/0.719/0.720 to 0.872/0.869/0.873 in TY2018/2019/2020. See T13 |
|ALL   |both   |     2017|     2024|structural                  |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                                                                                                                                      |

