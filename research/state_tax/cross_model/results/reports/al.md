# Cross-model validation: AL

Class:  | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.3953|    0.5198|         0.4179|          0.5488|          0.1450|         74.6566|   2607.2591|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.3974|    0.5218|         0.4195|          0.5542|          0.1391|         75.6428|   2530.9241|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4051|    0.5279|         0.4277|          0.5579|          0.1390|         71.5205|   2399.8301|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.3906|    0.5180|         0.4134|          0.5467|          0.1458|         82.7908|   2364.1968|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.3259|    0.5162|         0.7471|          0.8429|          0.1527|         84.5971|   2808.1946|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.3436|    0.5294|         0.6944|          0.8040|          0.1420|         77.0209|  12880.0416|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.3258|    0.5203|         0.6518|          0.7530|          0.1340|         87.6346|    886.3084|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.3252|    0.5009|         0.6705|          0.7775|          0.1409|         99.2322|   2827.3917|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3816|
| 2017|TRUE        |2 exemptions     |  980|
| 2017|TRUE        |3 deductions     | 2882|
| 2017|TRUE        |4 taxable income |  236|
| 2017|FALSE       |1 state AGI      | 3928|
| 2017|FALSE       |2 exemptions     |  407|
| 2017|FALSE       |3 deductions     | 1215|
| 2017|FALSE       |4 taxable income |   67|
| 2018|TRUE        |1 state AGI      | 3716|
| 2018|TRUE        |2 exemptions     |  944|
| 2018|TRUE        |3 deductions     | 3011|
| 2018|TRUE        |4 taxable income |  281|
| 2018|FALSE       |1 state AGI      | 3916|
| 2018|FALSE       |2 exemptions     |  367|
| 2018|FALSE       |3 deductions     | 1214|
| 2018|FALSE       |4 taxable income |   70|
| 2019|TRUE        |1 state AGI      | 3624|
| 2019|TRUE        |2 exemptions     |  898|
| 2019|TRUE        |3 deductions     | 3053|
| 2019|TRUE        |4 taxable income |  258|
| 2019|FALSE       |1 state AGI      | 3914|
| 2019|FALSE       |2 exemptions     |  386|
| 2019|FALSE       |3 deductions     | 1238|
| 2019|FALSE       |4 taxable income |   71|
| 2020|TRUE        |1 state AGI      | 5115|
| 2020|TRUE        |2 exemptions     |  691|
| 2020|TRUE        |3 deductions     | 1728|
| 2020|TRUE        |4 taxable income |  251|
| 2020|FALSE       |1 state AGI      | 4466|
| 2020|FALSE       |2 exemptions     |  404|
| 2020|FALSE       |3 deductions     |  884|
| 2020|FALSE       |4 taxable income |  138|

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

