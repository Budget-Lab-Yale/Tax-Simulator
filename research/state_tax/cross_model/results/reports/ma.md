# Cross-model validation: MA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9979|    4154|   0.4230|    0.6404|         0.6887|          0.8936|          0.0802|         33.8080|    245.2611|
| 2017|taxsim       | 10534|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9804|    5415|   0.4287|    0.6416|         0.6198|          0.8253|          0.0804|         32.3369|   -156.3119|
| 2018|taxsim       | 10711|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9815|    5392|   0.4341|    0.6308|         0.6269|          0.8310|          0.0790|         31.7646|    312.3243|
| 2019|taxsim       | 10699|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9777|    5178|   0.4023|    0.6275|         0.5981|          0.8279|          0.0796|         36.3939|    216.6111|
| 2020|taxsim       | 10736|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.0657|    0.0887|         0.1484|          0.1676|          0.0461|        873.5651|  11724.8541|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3129|    0.4189|         0.5437|          0.6336|          0.0675|        179.9982|   -340.5952|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.4175|    0.5601|         0.6987|          0.8098|          0.0605|         50.0001|    659.1226|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4191|    0.5687|         0.7174|          0.8348|          0.0626|         50.0007|   -608.4713|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |     n|
|----:|:-----------|:----------------|-----:|
| 2017|TRUE        |1 state AGI      |  1502|
| 2017|TRUE        |2 exemptions     |    65|
| 2017|TRUE        |4 taxable income |  1042|
| 2017|TRUE        |7 rate/rounding  |   134|
| 2017|FALSE       |1 state AGI      | 10062|
| 2017|FALSE       |2 exemptions     |   361|
| 2017|FALSE       |4 taxable income |  1410|
| 2017|FALSE       |5 state EITC     |    12|
| 2017|FALSE       |7 rate/rounding  |    57|
| 2018|TRUE        |1 state AGI      |  3320|
| 2018|TRUE        |2 exemptions     |    94|
| 2018|TRUE        |4 taxable income |  1156|
| 2018|TRUE        |7 rate/rounding  |   151|
| 2018|FALSE       |1 state AGI      |  8307|
| 2018|FALSE       |2 exemptions     |   314|
| 2018|FALSE       |4 taxable income |  1269|
| 2018|FALSE       |5 state EITC     |     9|
| 2018|FALSE       |7 rate/rounding  |    50|
| 2019|TRUE        |1 state AGI      |  3234|
| 2019|TRUE        |2 exemptions     |    86|
| 2019|TRUE        |4 taxable income |  1183|
| 2019|TRUE        |7 rate/rounding  |   154|
| 2019|FALSE       |1 state AGI      |  8245|
| 2019|FALSE       |2 exemptions     |   353|
| 2019|FALSE       |4 taxable income |  1320|
| 2019|FALSE       |5 state EITC     |     8|
| 2019|FALSE       |7 rate/rounding  |    34|
| 2020|TRUE        |1 state AGI      |  3525|
| 2020|TRUE        |2 exemptions     |    32|
| 2020|TRUE        |4 taxable income |   982|
| 2020|TRUE        |7 rate/rounding  |   102|
| 2020|FALSE       |1 state AGI      |  8703|
| 2020|FALSE       |2 exemptions     |   319|
| 2020|FALSE       |4 taxable income |  1268|
| 2020|FALSE       |5 state EITC     |     8|
| 2020|FALSE       |7 rate/rounding  |    42|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:-----|:------|--------:|--------:|:------------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                                                                                                                                                                                                                                                                                                                      |
|MA    |taxsim |     2017|     2020|external-model-bug |exclude  |TAXSIM applies the Form 1 line 11 payroll/retirement-contribution deduction as a $2,000 cap PER RETURN rather than per person. Form 1 carries separate lines 11a and 11b and the booklet reads "the amount you, and your spouse if filing jointly, paid ... up to a maximum of $2,000", each line captioned "Not more than $2,000", so the cap is per person and non-transferable. Probe-verified 2026-08-21 against both the local WASM build and the live NBER server (issue T16): TY2018 joint with 60,000 of wages each returns siitax 5,569.20 against the correct 5,467.20, an implied deduction pinned at 2,000; the same pinning holds at 60,000/30,000 and 60,000/10,000, while a single filer at 60,000 is correct. Excluded on two-earner joint returns, where the second earner has contributions of their own for the per-return cap to swallow. Slightly over-broad: a couple whose second earner has very small earnings is affected by less than the tolerance |
|MA    |taxsim |     2017|     2020|external-model-bug |exclude  |TAXSIM grants a flat $2,000 Form 1 line 11 payroll deduction to ANY Massachusetts return with positive gross Social Security, additively on top of any legitimate wage-based deduction, on records that paid no FICA at all. Probe-verified 2026-08-21 against both the local WASM build and the live NBER server (issue T17): holding the return fixed and varying gssi alone, gssi of 5,000/10,000/20,000/30,000/60,000 all draw exactly 2,000 while gssi of 0 correctly draws nothing, so it is neither a contribution nor FICA computed on the benefit. A wage-earner with Social Security gets 4,000 where 2,000 is correct, so working filers are hit as well as retirees, and age is irrelevant. The booklet expressly disallows Medicare PREMIUMS withheld from Social Security, the likely source. Same shape as the Utah retirement-credit issue (T7): a flat state amount keyed to positive gssi alone                                                              |

