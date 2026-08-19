# Cross-model validation: SC

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4960|    0.5480|         0.5340|          0.5837|          0.2812|         19.2412|   2732.4426|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5877|    0.6780|         0.6504|          0.7312|          0.3026|          0.4403|   -325.3807|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5998|    0.6823|         0.6603|          0.7324|          0.3088|          6.7993|   -427.7698|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5934|    0.6786|         0.6575|          0.7359|          0.3018|          0.7519|   -411.2025|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.3140|    0.3362|         0.5517|          0.5594|          0.3123|        619.7428|     89.7764|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     300|   0.4426|    0.5136|         0.8533|          0.9033|          0.2892|         68.6917|   -345.9312|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     337|   0.4296|    0.5056|         0.8427|          0.8843|          0.2921|         93.0400|  -1032.1762|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.4296|    0.4983|         0.8497|          0.8757|          0.2765|        103.1212|  -1127.6503|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6258|
| 2017|TRUE        |4 taxable income |    5|
| 2017|TRUE        |6 other credits  |  270|
| 2017|FALSE       |1 state AGI      | 5352|
| 2017|FALSE       |4 taxable income |    7|
| 2017|FALSE       |6 other credits  |   43|
| 2018|TRUE        |1 state AGI      | 4664|
| 2018|TRUE        |2 exemptions     |  353|
| 2018|TRUE        |4 taxable income |   24|
| 2018|TRUE        |6 other credits  |  150|
| 2018|FALSE       |1 state AGI      | 5156|
| 2018|FALSE       |2 exemptions     |   22|
| 2018|FALSE       |4 taxable income |    3|
| 2018|FALSE       |5 state EITC     |   60|
| 2019|TRUE        |1 state AGI      | 4700|
| 2019|TRUE        |2 exemptions     |  244|
| 2019|TRUE        |4 taxable income |   19|
| 2019|TRUE        |6 other credits  |  108|
| 2019|FALSE       |1 state AGI      | 5203|
| 2019|FALSE       |2 exemptions     |   14|
| 2019|FALSE       |4 taxable income |    1|
| 2019|FALSE       |5 state EITC     |   27|
| 2020|TRUE        |1 state AGI      | 4649|
| 2020|TRUE        |2 exemptions     |  224|
| 2020|TRUE        |4 taxable income |   11|
| 2020|TRUE        |6 other credits  |  112|
| 2020|FALSE       |1 state AGI      | 5383|
| 2020|FALSE       |2 exemptions     |   28|
| 2020|FALSE       |4 taxable income |    1|
| 2020|FALSE       |5 state EITC     |   23|
| 2020|FALSE       |6 other credits  |    8|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|SC    |taxsim |     2017|     2020|state-law      |annotate |Small schedule-constant differences: our subtraction-method constants transcribed from the published SC1040TT vs TAXSIM's schedule (+$6.80 at top-bracket and +$3.20 at mid-bracket incomes on 2019 probe cases); bounded ~$10 and inside the $15 tolerance after the 2026-08-11 aged-deduction and TWEC encodings closed the large wedges                                                                                                                                                                                                                                                                                                |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

