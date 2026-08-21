# Cross-model validation: HI

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9010|    6860|   0.5868|    0.7837|         0.5907|          0.7783|          0.0001|          9.9809|   -153.9247|
| 2017|taxsim       | 11503|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9475|    7192|   0.4665|    0.6822|         0.5368|          0.7027|          0.0000|         20.5406|   -144.2949|
| 2018|taxsim       | 11040|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9409|    7159|   0.4635|    0.6798|         0.5309|          0.6984|          0.0000|         21.5336|   -233.9990|
| 2019|taxsim       | 11105|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9581|    6922|   0.4461|    0.6565|         0.5326|          0.6936|          0.0000|         25.4487|   -192.9783|
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
| 2017|TRUE        |1 state AGI     | 6025|
| 2017|TRUE        |3 deductions    | 2720|
| 2017|TRUE        |6 other credits |   10|
| 2017|TRUE        |7 rate/rounding |    7|
| 2017|FALSE       |1 state AGI     | 5491|
| 2017|FALSE       |3 deductions    |  432|
| 2018|TRUE        |1 state AGI     | 6234|
| 2018|TRUE        |3 deductions    | 2777|
| 2018|TRUE        |6 other credits |    7|
| 2018|TRUE        |7 rate/rounding |   11|
| 2018|FALSE       |1 state AGI     | 5545|
| 2018|FALSE       |3 deductions    | 1036|
| 2018|FALSE       |6 other credits |    1|
| 2019|TRUE        |1 state AGI     | 6275|
| 2019|TRUE        |3 deductions    | 2715|
| 2019|TRUE        |6 other credits |    7|
| 2019|TRUE        |7 rate/rounding |    8|
| 2019|FALSE       |1 state AGI     | 5537|
| 2019|FALSE       |3 deductions    | 1060|
| 2020|TRUE        |1 state AGI     | 6124|
| 2020|TRUE        |3 deductions    | 2596|
| 2020|TRUE        |6 other credits |    8|
| 2020|TRUE        |7 rate/rounding |    6|
| 2020|FALSE       |1 state AGI     | 5737|
| 2020|FALSE       |3 deductions    | 1249|
| 2020|FALSE       |7 rate/rounding |    1|

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

