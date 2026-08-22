# Cross-model validation: IA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.3619|    0.4782|         0.4424|          0.5307|          0.1327|        128.1932|   1422.5737|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.3644|    0.4715|         0.4466|          0.5277|          0.1265|        138.4002|   1872.7469|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.3676|    0.4793|         0.4495|          0.5339|          0.1266|        128.3281|   1567.6868|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.2238|    0.3750|         0.2613|          0.4062|          0.1250|        187.5520|    304.7732|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.2654|    0.4044|         0.5714|          0.6676|          0.1084|        185.0815|   4419.7597|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3585|    0.5153|         0.6927|          0.7991|          0.1192|         84.0000|   2270.8791|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.4564|    0.6335|         0.9000|          0.9787|          0.1677|         34.0848|   -995.1575|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     461|   0.4548|    0.6374|         0.9067|          0.9826|          0.1652|         28.9040|  -1058.5400|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 4356|
| 2017|TRUE        |3 deductions     | 2465|
| 2017|TRUE        |4 taxable income |  636|
| 2017|TRUE        |5 state EITC     |   50|
| 2017|TRUE        |6 other credits  |  102|
| 2017|TRUE        |7 rate/rounding  |   26|
| 2017|FALSE       |1 state AGI      | 3930|
| 2017|FALSE       |3 deductions     | 1576|
| 2017|FALSE       |4 taxable income |  481|
| 2017|FALSE       |5 state EITC     |  552|
| 2017|FALSE       |7 rate/rounding  |    4|
| 2018|TRUE        |1 state AGI      | 4419|
| 2018|TRUE        |3 deductions     | 2443|
| 2018|TRUE        |4 taxable income |  583|
| 2018|TRUE        |5 state EITC     |   74|
| 2018|TRUE        |6 other credits  |   87|
| 2018|TRUE        |7 rate/rounding  |   27|
| 2018|FALSE       |1 state AGI      | 3938|
| 2018|FALSE       |3 deductions     | 1616|
| 2018|FALSE       |4 taxable income |  401|
| 2018|FALSE       |5 state EITC     |  558|
| 2018|FALSE       |6 other credits  |    1|
| 2018|FALSE       |7 rate/rounding  |    3|
| 2019|TRUE        |1 state AGI      | 4294|
| 2019|TRUE        |3 deductions     | 2509|
| 2019|TRUE        |4 taxable income |  563|
| 2019|TRUE        |5 state EITC     |   88|
| 2019|TRUE        |6 other credits  |  112|
| 2019|TRUE        |7 rate/rounding  |   16|
| 2019|FALSE       |1 state AGI      | 3888|
| 2019|FALSE       |3 deductions     | 1636|
| 2019|FALSE       |4 taxable income |  401|
| 2019|FALSE       |5 state EITC     |  636|
| 2019|FALSE       |6 other credits  |    1|
| 2019|FALSE       |7 rate/rounding  |    4|
| 2020|TRUE        |1 state AGI      | 4369|
| 2020|TRUE        |3 deductions     | 2377|
| 2020|TRUE        |4 taxable income | 2623|
| 2020|TRUE        |5 state EITC     |   97|
| 2020|TRUE        |6 other credits  |   83|
| 2020|TRUE        |7 rate/rounding  |   12|
| 2020|FALSE       |1 state AGI      | 4453|
| 2020|FALSE       |3 deductions     | 1536|
| 2020|FALSE       |4 taxable income |  410|
| 2020|FALSE       |5 state EITC     |  600|
| 2020|FALSE       |7 rate/rounding  |    2|

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

