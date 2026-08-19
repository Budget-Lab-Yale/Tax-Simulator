# Cross-model validation: OK

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.2081|    0.5136|         0.2684|          0.5670|          0.0172|         84.9968|    972.7218|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 12929|    9804|   0.4471|    0.8230|         0.5412|          0.8742|          0.0157|         36.9797|     54.5076|
| 2018|taxsim       |  7586|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 12798|    9704|   0.4511|    0.8200|         0.5459|          0.8703|          0.0173|         36.7837|     47.0309|
| 2019|taxsim       |  7716|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 13123|    9521|   0.4441|    0.7953|         0.5591|          0.8747|          0.0168|         38.7285|     61.3041|
| 2020|taxsim       |  7390|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.2415|    0.4804|         0.4521|          0.8123|          0.0469|        120.0000|    766.1357|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.2822|    0.4899|         0.4850|          0.8538|          0.0351|        109.0500|    313.4528|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     337|   0.2740|    0.4875|         0.4866|          0.8635|          0.0363|        117.4642|    335.7801|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     347|   0.2852|    0.4809|         0.5562|          0.8703|          0.0443|        120.0043|    -95.0550|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 4714|
| 2017|TRUE        |2 exemptions     |   42|
| 2017|TRUE        |3 deductions     | 2717|
| 2017|TRUE        |4 taxable income |   60|
| 2017|TRUE        |5 state EITC     |   21|
| 2017|TRUE        |6 other credits  | 2260|
| 2017|FALSE       |1 state AGI      | 5805|
| 2017|FALSE       |2 exemptions     |    3|
| 2017|FALSE       |3 deductions     |  184|
| 2017|FALSE       |4 taxable income |    4|
| 2017|FALSE       |5 state EITC     |  963|
| 2017|FALSE       |6 other credits  |  183|
| 2018|TRUE        |1 state AGI      | 3938|
| 2018|TRUE        |2 exemptions     |   35|
| 2018|TRUE        |3 deductions     | 1454|
| 2018|TRUE        |5 state EITC     |   28|
| 2018|TRUE        |6 other credits  | 2283|
| 2018|FALSE       |1 state AGI      | 5548|
| 2018|FALSE       |2 exemptions     |    8|
| 2018|FALSE       |3 deductions     |  166|
| 2018|FALSE       |5 state EITC     |  942|
| 2018|FALSE       |6 other credits  |  186|
| 2019|TRUE        |1 state AGI      | 4062|
| 2019|TRUE        |2 exemptions     |   29|
| 2019|TRUE        |3 deductions     | 1375|
| 2019|TRUE        |5 state EITC     |   22|
| 2019|TRUE        |6 other credits  | 2215|
| 2019|FALSE       |1 state AGI      | 5560|
| 2019|FALSE       |2 exemptions     |    7|
| 2019|FALSE       |3 deductions     |  189|
| 2019|FALSE       |5 state EITC     |  970|
| 2019|FALSE       |6 other credits  |  161|
| 2020|TRUE        |1 state AGI      | 3878|
| 2020|TRUE        |2 exemptions     |   40|
| 2020|TRUE        |3 deductions     | 1282|
| 2020|TRUE        |4 taxable income |    2|
| 2020|TRUE        |5 state EITC     |   24|
| 2020|TRUE        |6 other credits  | 2095|
| 2020|FALSE       |1 state AGI      | 6069|
| 2020|FALSE       |2 exemptions     |    5|
| 2020|FALSE       |3 deductions     |  154|
| 2020|FALSE       |5 state EITC     |  897|
| 2020|FALSE       |6 other credits  |  168|

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

