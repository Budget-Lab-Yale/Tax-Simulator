# Cross-model validation: HI

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9010|    6317|   0.5932|    0.7888|         0.6156|          0.8039|          0.0001|          9.6893|   -170.8276|
| 2017|taxsim       | 11503|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9476|    6625|   0.4763|    0.6930|         0.5725|          0.7384|          0.0000|         18.5296|   -168.2444|
| 2018|taxsim       | 11039|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9409|    6609|   0.4751|    0.6904|         0.5668|          0.7331|          0.0000|         18.9672|   -253.0148|
| 2019|taxsim       | 11105|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9581|    6324|   0.4603|    0.6715|         0.5727|          0.7340|          0.0000|         22.2764|   -233.5902|
| 2020|taxsim       | 10932|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.0589|    0.0870|         0.0441|          0.0579|          0.0000|        487.4520|  13191.0348|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     421|   0.2778|    0.3935|         0.4964|          0.6485|          0.0114|        253.9868|  11891.2621|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     467|   0.2714|    0.3518|         0.5396|          0.6253|          0.0121|        388.2074|   7681.8621|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     458|   0.3209|    0.3870|         0.6528|          0.6987|          0.0148|        369.0091|  11297.5549|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 1133|
| 2017|TRUE        |3 deductions    | 1422|
| 2017|TRUE        |6 other credits |    2|
| 2017|TRUE        |7 rate/rounding |    1|
| 2017|FALSE       |1 state AGI     | 9412|
| 2017|FALSE       |3 deductions    | 2623|
| 2017|FALSE       |6 other credits |    8|
| 2017|FALSE       |7 rate/rounding |    6|
| 2018|TRUE        |1 state AGI     | 2666|
| 2018|TRUE        |3 deductions    | 2425|
| 2018|TRUE        |6 other credits |    6|
| 2018|TRUE        |7 rate/rounding |   12|
| 2018|FALSE       |1 state AGI     | 8136|
| 2018|FALSE       |3 deductions    | 2256|
| 2018|FALSE       |6 other credits |    2|
| 2019|TRUE        |1 state AGI     | 2722|
| 2019|TRUE        |3 deductions    | 2355|
| 2019|TRUE        |6 other credits |    7|
| 2019|TRUE        |7 rate/rounding |    8|
| 2019|FALSE       |1 state AGI     | 8212|
| 2019|FALSE       |3 deductions    | 2157|
| 2020|TRUE        |1 state AGI     | 2694|
| 2020|TRUE        |3 deductions    | 2277|
| 2020|TRUE        |6 other credits |    6|
| 2020|TRUE        |7 rate/rounding |    5|
| 2020|FALSE       |1 state AGI     | 8249|
| 2020|FALSE       |3 deductions    | 2325|
| 2020|FALSE       |6 other credits |    2|
| 2020|FALSE       |7 rate/rounding |    2|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:-----|:------|--------:|--------:|:--------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                    |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding              |
|HI    |taxsim |     2017|     2020|input-coverage |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): Hawaii computes its own itemized deductions on Hawaii AGI under an election independent of the federal one, applies a SALT-income disallowance above $100k/$150k/$200k and a fixed overall limitation on Hawaii AGI, and does not follow TCJA on the itemized base (misc-2% survives). The crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip or disallow, and investment interest and Schedule A "other" have no TAXSIM inputs at all. Excluded via the standard exposure predicate |

