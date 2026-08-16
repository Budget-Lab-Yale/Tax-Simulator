# Cross-model validation: NY

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9131|    6922|   0.6531|    0.7659|         0.8014|          0.8463|          0.1772|          2.4739|   1744.6299|
| 2017|taxsim       | 11382|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  8991|    6889|   0.6630|    0.7704|         0.8068|          0.8460|          0.1777|          0.8287|   1687.5817|
| 2018|taxsim       | 11524|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  8937|    6842|   0.6589|    0.7684|         0.8103|          0.8486|          0.1766|          0.9755|   1637.0116|
| 2019|taxsim       | 11577|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  8925|    6520|   0.6565|    0.7647|         0.8090|          0.8483|          0.1714|          0.8546|   1582.6938|
| 2020|taxsim       | 11588|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.2159|    0.3532|         0.5862|          0.6973|          0.0776|        277.1808|   2289.9348|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.3348|    0.4531|         0.7774|          0.8605|          0.0850|        163.2182|   1236.9259|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |   360|      48|   0.1917|    0.3417|         0.5625|          0.7083|          0.0000|        352.4336|   3517.9260|
| 2023|policyengine |  1173|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.3200|    0.4252|         0.7572|          0.8295|          0.0704|        196.7342|    124.4797|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3902|
| 2017|TRUE        |3 deductions     | 2562|
| 2017|TRUE        |4 taxable income |    4|
| 2017|TRUE        |5 state EITC     |   70|
| 2017|TRUE        |6 other credits  |  647|
| 2017|TRUE        |7 rate/rounding  |    6|
| 2017|FALSE       |1 state AGI      | 5701|
| 2017|FALSE       |3 deductions     |  185|
| 2017|FALSE       |5 state EITC     |  990|
| 2017|FALSE       |6 other credits  |    5|
| 2018|TRUE        |1 state AGI      | 3955|
| 2018|TRUE        |3 deductions     | 2088|
| 2018|TRUE        |5 state EITC     |   58|
| 2018|TRUE        |6 other credits  |  603|
| 2018|TRUE        |7 rate/rounding  |    2|
| 2018|FALSE       |1 state AGI      | 5649|
| 2018|FALSE       |3 deductions     |  179|
| 2018|FALSE       |5 state EITC     |  969|
| 2018|FALSE       |6 other credits  |    6|
| 2019|TRUE        |1 state AGI      | 4029|
| 2019|TRUE        |3 deductions     | 2008|
| 2019|TRUE        |5 state EITC     |   62|
| 2019|TRUE        |6 other credits  |  587|
| 2019|TRUE        |7 rate/rounding  |    5|
| 2019|FALSE       |1 state AGI      | 5692|
| 2019|FALSE       |3 deductions     |  195|
| 2019|FALSE       |5 state EITC     | 1017|
| 2019|FALSE       |6 other credits  |    4|
| 2020|TRUE        |1 state AGI      | 3858|
| 2020|TRUE        |3 deductions     | 1812|
| 2020|TRUE        |5 state EITC     |   44|
| 2020|TRUE        |6 other credits  |  586|
| 2020|TRUE        |7 rate/rounding  |    4|
| 2020|FALSE       |1 state AGI      | 5762|
| 2020|FALSE       |3 deductions     |  246|
| 2020|FALSE       |5 state EITC     |  947|
| 2020|FALSE       |6 other credits  |   45|
| 2020|FALSE       |7 rate/rounding  |    7|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|NY    |policyengine |     2023|     2023|transfer-netting |exclude  |PE books New York's one-time 2025 inflation refund checks (S.3009-C: $200 single / $400 joint tiered down by NY AGI) into TAX YEAR 2023 as ny_inflation_refund_credit — formula_2023 pays it (source comment: the tax effect belongs to the eligibility year) and formula_2024 returns 0. Nearly every low/mid-AGI 2023 NY record shifts by the rebate (160 of ~300 clean mismatches sit exactly at +200/+400; zero-income records show PE = -200/-400), collapsing the 2023 cell (clean 0.833 2022 -> 0.160 2023 -> 0.797 2024). Same class as the IL 2021 rebate (issues doc P2). Excluded via predicate on the exported credit         |
|NY    |taxsim       |     2017|     2020|input-coverage   |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate                                  |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

