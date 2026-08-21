# Cross-model validation: MD

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4108|    0.5412|         0.5171|          0.6246|          0.1378|         63.4668|    759.3341|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4934|    0.6498|         0.6230|          0.7528|          0.1272|         17.8178|   -580.2995|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.2104|    0.6360|         0.2422|          0.7387|          0.1266|         68.8750|   -793.5322|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4568|    0.5974|         0.5949|          0.7294|          0.1221|         37.5763|   -661.3600|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     260|   0.2935|    0.4462|         0.7885|          0.8615|          0.0887|        129.2489|    -57.6407|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     298|   0.3848|    0.5039|         0.8322|          0.9128|          0.1280|         96.4797|    167.3939|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.3717|    0.5065|         0.8601|          0.9137|          0.1201|         93.4374|    241.4206|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     345|   0.3591|    0.4765|         0.8435|          0.9130|          0.0991|        130.4707|   -327.1511|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4809|
| 2017|TRUE        |2 exemptions    |  213|
| 2017|TRUE        |3 deductions    | 1686|
| 2017|TRUE        |6 other credits |   38|
| 2017|FALSE       |1 state AGI     | 5474|
| 2017|FALSE       |2 exemptions    |    7|
| 2017|FALSE       |3 deductions    |  138|
| 2017|FALSE       |5 state EITC    |  999|
| 2017|FALSE       |6 other credits |   13|
| 2018|TRUE        |1 state AGI     | 4443|
| 2018|TRUE        |2 exemptions    |  210|
| 2018|TRUE        |3 deductions    |  849|
| 2018|TRUE        |6 other credits |   34|
| 2018|FALSE       |1 state AGI     | 5334|
| 2018|FALSE       |2 exemptions    |   14|
| 2018|FALSE       |3 deductions    |  138|
| 2018|FALSE       |5 state EITC    |  994|
| 2018|FALSE       |6 other credits |    8|
| 2019|TRUE        |1 state AGI     | 5505|
| 2019|TRUE        |2 exemptions    |  175|
| 2019|TRUE        |3 deductions    | 4398|
| 2019|TRUE        |6 other credits |   17|
| 2019|FALSE       |1 state AGI     | 5553|
| 2019|FALSE       |2 exemptions    |   10|
| 2019|FALSE       |3 deductions    |  609|
| 2019|FALSE       |5 state EITC    |  534|
| 2019|FALSE       |6 other credits |    8|
| 2020|TRUE        |1 state AGI     | 4582|
| 2020|TRUE        |2 exemptions    |  207|
| 2020|TRUE        |3 deductions    |  764|
| 2020|TRUE        |5 state EITC    |  142|
| 2020|TRUE        |6 other credits |   34|
| 2020|FALSE       |1 state AGI     | 5852|
| 2020|FALSE       |2 exemptions    |    4|
| 2020|FALSE       |3 deductions    |  262|
| 2020|FALSE       |5 state EITC    |  873|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:-----|:------|--------:|--------:|:------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                             |
|MD    |taxsim |     2019|     2019|external-model-bug |annotate |TAXSIM applies the MD standard-deduction MINIMUM ($1,550/$3,100) to every 2019 non-itemizer: probe-verified v34 = 1,550 at $100k wages where 15% x AGI caps at the $2,250/$4,550 maximum. Produces flat -$33 (single, 700 x 4.75%) / -$69 and -$83 (joint, 1,450 x rate) masses on ~3,900 records. 2018 and 2020 probe correct-to-one-index-step (2020 uses the 2019 maxima). ANNOTATE, not exclude: the per-record effect ($33-$83) never breaches the $100 bar, so it binds match@15 only (2019 match@15 24% vs match@100 69.5%); an earlier exclude on the bug signature removed match@100 PASSES and depressed the 2019 cell to 0.488 (reverted) |
|MD    |taxsim |     2017|     2024|data-proxy         |annotate |Two-income married couple subtraction attribution: TAXSIM attributes joint unearned income across spouses when computing the lesser-earning spouse's income, granting the $1,200 subtraction to one-earner couples with unearned income; our proxy attributes earned income only (per-spouse ownership of unearned income is unobserved in the PUF). +$57 mass (~190/yr, 78% wages2 == 0, 76% joint unearned > $2,400). Both are proxies for Worksheet 13D; neither is form-true                                                                                                                                                                     |
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding            |

