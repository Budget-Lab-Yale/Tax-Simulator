# Cross-model validation: MO

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.3858|    0.5294|         0.4085|          0.5596|          0.1925|         67.1692|   1202.0716|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5164|    0.6146|         0.5702|          0.6674|          0.2161|         10.4448|  -1382.9289|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5384|    0.6193|         0.5894|          0.6676|          0.2181|          5.9772|  -1707.2250|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5103|    0.5959|         0.5810|          0.6634|          0.2127|         11.7779|  -1705.4053|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.3797|    0.6067|         0.6501|          0.8843|          0.1962|         60.1402|   -856.9906|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.5092|    0.6170|         0.8203|          0.8865|          0.1972|         12.5517|  -1255.8032|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.4762|    0.5877|         0.7821|          0.8568|          0.2360|         26.5544|  -2103.4543|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4704|    0.5835|         0.8130|          0.8739|          0.2243|         21.8809|  -2286.8526|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3911|
| 2017|TRUE        |2 exemptions     | 2056|
| 2017|TRUE        |3 deductions     | 2094|
| 2017|TRUE        |4 taxable income |    5|
| 2017|FALSE       |1 state AGI      | 5353|
| 2017|FALSE       |2 exemptions     |  295|
| 2017|FALSE       |3 deductions     |  101|
| 2018|TRUE        |1 state AGI      | 4518|
| 2018|TRUE        |2 exemptions     |  345|
| 2018|TRUE        |3 deductions     | 1320|
| 2018|TRUE        |4 taxable income |    7|
| 2018|FALSE       |1 state AGI      | 5367|
| 2018|FALSE       |2 exemptions     |    7|
| 2018|FALSE       |3 deductions     |  110|
| 2019|TRUE        |1 state AGI      | 4558|
| 2019|TRUE        |2 exemptions     |  357|
| 2019|TRUE        |3 deductions     | 1028|
| 2019|TRUE        |4 taxable income |    5|
| 2019|FALSE       |1 state AGI      | 5352|
| 2019|FALSE       |2 exemptions     |    5|
| 2019|FALSE       |3 deductions     |   86|
| 2020|TRUE        |1 state AGI      | 4552|
| 2020|TRUE        |2 exemptions     |  348|
| 2020|TRUE        |3 deductions     |  990|
| 2020|TRUE        |4 taxable income |    9|
| 2020|FALSE       |1 state AGI      | 5875|
| 2020|FALSE       |2 exemptions     |    2|
| 2020|FALSE       |3 deductions     |   84|

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

