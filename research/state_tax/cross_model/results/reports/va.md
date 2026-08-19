# Cross-model validation: VA

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4996|    0.5786|         0.5578|          0.6275|          0.2584|         15.1792|   1251.3495|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5941|    0.6797|         0.6841|          0.7467|          0.2471|          0.4208|   1225.2531|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5993|    0.6730|         0.6836|          0.7384|          0.2539|          0.4227|   1940.1892|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5994|    0.6737|         0.6834|          0.7371|          0.2505|          0.3074|   2011.7238|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     300|   0.4777|    0.6135|         0.9067|          0.9500|          0.1288|         22.6310|    154.3702|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1533|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1531|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3649|
| 2017|TRUE        |2 exemptions     |   31|
| 2017|TRUE        |3 deductions     | 2372|
| 2017|TRUE        |4 taxable income |  106|
| 2017|TRUE        |6 other credits  |   47|
| 2017|TRUE        |7 rate/rounding  |   50|
| 2017|FALSE       |1 state AGI      | 5211|
| 2017|FALSE       |3 deductions     |   64|
| 2017|FALSE       |4 taxable income |   32|
| 2017|FALSE       |5 state EITC     |  313|
| 2017|FALSE       |6 other credits  |    1|
| 2018|TRUE        |1 state AGI      | 3250|
| 2018|TRUE        |2 exemptions     |   28|
| 2018|TRUE        |3 deductions     | 1290|
| 2018|TRUE        |4 taxable income |  116|
| 2018|TRUE        |6 other credits  |   63|
| 2018|TRUE        |7 rate/rounding  |   58|
| 2018|FALSE       |1 state AGI      | 5077|
| 2018|FALSE       |3 deductions     |   66|
| 2018|FALSE       |4 taxable income |   18|
| 2018|FALSE       |5 state EITC     |  382|
| 2019|TRUE        |1 state AGI      | 3302|
| 2019|TRUE        |2 exemptions     |   23|
| 2019|TRUE        |3 deductions     | 1279|
| 2019|TRUE        |4 taxable income |  102|
| 2019|TRUE        |6 other credits  |   50|
| 2019|TRUE        |7 rate/rounding  |   53|
| 2019|FALSE       |1 state AGI      | 5154|
| 2019|FALSE       |2 exemptions     |    1|
| 2019|FALSE       |3 deductions     |   61|
| 2019|FALSE       |4 taxable income |   22|
| 2019|FALSE       |5 state EITC     |  310|
| 2020|TRUE        |1 state AGI      | 3216|
| 2020|TRUE        |2 exemptions     |   31|
| 2020|TRUE        |3 deductions     | 1235|
| 2020|TRUE        |4 taxable income |  120|
| 2020|TRUE        |6 other credits  |   48|
| 2020|TRUE        |7 rate/rounding  |   60|
| 2020|FALSE       |1 state AGI      | 5208|
| 2020|FALSE       |2 exemptions     |    3|
| 2020|FALSE       |3 deductions     |   66|
| 2020|FALSE       |4 taxable income |   27|
| 2020|FALSE       |5 state EITC     |  339|
| 2020|FALSE       |6 other credits  |    3|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|VA    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the fall-2022 Virginia rebate ($250 single / $500 joint; 2022 Special Session HB 30) into TAX YEAR 2021 via va_rebate (parameter: 2021 250/500, 2022 0). Clean mismatches mass at +250/+500 (71+14 of ~130). Excluded via predicate on the exported rebate                                                                                                                                                                                                                                                                                                                                                                       |
|VA    |policyengine |     2023|     2024|transfer-netting |exclude  |PE books the 2023 Virginia rebate (HB6001: $200/$400, paid fall 2023) AND its HB 1600 reauthorization for TY2024 into tax years 2023 and 2024 via va_rebate (parameter: 2023 200/400 through 2024; 0 from 2025). Clean mismatches mass at +200/+400 in BOTH years (129+34 in 2023, 131+45 in 2024), depressing both cells to ~0.35 from 0.940 in 2022. Excluded via predicate on the exported rebate                                                                                                                                                                                                                                      |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

