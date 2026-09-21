# Cross-model validation: OR

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9053|    6152|   0.7025|    0.8519|         0.8565|          0.9269|          0.1900|          0.0670|    186.6965|
| 2017|taxsim       | 11460|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9297|    6409|   0.7146|    0.8667|         0.8558|          0.9284|          0.1809|          0.6343|     22.9158|
| 2018|taxsim       | 11218|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  1689|    1358|   0.9692|    0.9929|         0.9971|          0.9993|          0.8928|          0.0000|      0.2544|
| 2019|taxsim       | 18825|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9510|    6161|   0.7000|    0.8402|         0.8878|          0.9448|          0.1740|          0.1821|     52.0597|
| 2020|taxsim       | 11003|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     362|   0.3695|    0.5572|         0.8204|          0.8895|          0.0870|         59.7199|   -517.6279|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     422|   0.4145|    0.5916|         0.7536|          0.8768|          0.0999|         43.0883|   -202.2855|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     467|   0.3976|    0.5687|         0.7452|          0.8844|          0.0856|         43.7531|   -139.3929|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     458|   0.3983|    0.5530|         0.7227|          0.8624|          0.0826|         48.9378|  -1197.7881|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  190|
| 2017|TRUE        |3 deductions     |   70|
| 2017|TRUE        |4 taxable income |  442|
| 2017|TRUE        |5 state EITC     |  129|
| 2017|TRUE        |6 other credits  |  129|
| 2017|FALSE       |1 state AGI      | 8633|
| 2017|FALSE       |3 deductions     | 3125|
| 2017|FALSE       |4 taxable income |  119|
| 2017|FALSE       |5 state EITC     |  781|
| 2018|TRUE        |1 state AGI      |  751|
| 2018|TRUE        |3 deductions     |  835|
| 2018|TRUE        |4 taxable income |  178|
| 2018|TRUE        |6 other credits  |    8|
| 2018|FALSE       |1 state AGI      | 8104|
| 2018|FALSE       |3 deductions     | 3033|
| 2018|FALSE       |4 taxable income |   44|
| 2018|FALSE       |5 state EITC     |  371|
| 2019|TRUE        |1 state AGI      | 1541|
| 2019|TRUE        |3 deductions     | 1671|
| 2019|TRUE        |4 taxable income |  528|
| 2019|TRUE        |5 state EITC     |   90|
| 2019|TRUE        |6 other credits  | 2132|
| 2019|FALSE       |1 state AGI      | 8631|
| 2019|FALSE       |3 deductions     | 2757|
| 2019|FALSE       |4 taxable income |  116|
| 2019|FALSE       |5 state EITC     |  801|
| 2019|FALSE       |6 other credits  |   97|
| 2020|TRUE        |1 state AGI      |  832|
| 2020|TRUE        |3 deductions     |  453|
| 2020|TRUE        |4 taxable income |  188|
| 2020|TRUE        |5 state EITC     |  110|
| 2020|TRUE        |6 other credits  |  140|
| 2020|FALSE       |1 state AGI      | 8588|
| 2020|FALSE       |3 deductions     | 2124|
| 2020|FALSE       |4 taxable income |   23|
| 2020|FALSE       |5 state EITC     |  863|
| 2020|FALSE       |6 other credits  |    1|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                                                                                                                                                                                                                                                                                                                                       |
|OR    |taxsim |     2017|     2020|input-coverage |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): Oregon elects to itemize fully independently of the federal return and Schedule OR-A removes state income tax from the federal itemized total, so a large share of Oregon filers itemize state-side only and the exposed population includes federal standard-deduction takers. The crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip, and investment interest and Schedule A "other" have no TAXSIM inputs at all. Measured: among records both models itemize in 2018, the state itemized-deduction gap correlates with xw_unhanded_item at r = 0.975 and only 0.2% of those records agree on the deduction. Excluded via the standard exposure predicate                                                                                                                                                                  |
|OR    |taxsim |     2019|     2019|state-law      |exclude  |The Oregon kicker (ORS 291.349) is a surplus credit equal to a fixed percentage of the PRIOR year Oregon liability before credits -- 17.171% of TY2018 liability on the TY2019 return. TAXSIM credits it; we do not, because the percentage applies to a prior-year liability that a cross-sectional harness run cannot observe. Measured: among non-itemizers with liability above $500, diff / st_tax_pre_credit has a mode at exactly 0.172 (n = 3,124 of ~4,000), and the same ratio is exactly 0.000 in 2017, 2018 and 2020 -- so TAXSIM applies no kicker in the other three window years, including TY2017 when Oregon in fact paid one. Removing the affected records takes the 2019 cell to 0.999, which attributes the whole of that year gap to this one provision. Not a permanent difference: Tax-Simulator runs years in sequence over the same tax units, so prior-year Oregon liability IS available in a production run -- see the state_tax plan item on modelling the kicker |

