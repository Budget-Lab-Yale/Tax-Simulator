# Cross-model validation: MD

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9132|    6340|   0.7047|    0.8581|         0.8959|          0.9655|          0.2189|          0.0039|     11.5174|
| 2017|taxsim       | 11381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 12221|    7530|   0.6569|    0.8376|         0.8471|          0.9542|          0.1632|          0.1941|     29.1585|
| 2018|taxsim       |  8294|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 12092|    7516|   0.2696|    0.8167|         0.3509|          0.9448|          0.1618|         34.3189|     -0.4188|
| 2019|taxsim       |  8422|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 12309|    7192|   0.5970|    0.7589|         0.8119|          0.9255|          0.1537|          4.9219|     26.5606|
| 2020|taxsim       |  8204|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     362|   0.4198|    0.5998|         0.8149|          0.8702|          0.0913|         33.4787|   -153.4341|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     421|   0.5232|    0.6442|         0.8884|          0.9382|          0.1280|          9.4281|     95.9045|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     467|   0.5160|    0.6448|         0.8801|          0.9208|          0.1227|         10.3584|    166.8820|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     458|   0.5078|    0.6348|         0.8843|          0.9389|          0.1017|         13.1605|   -420.1632|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  643|
| 2017|TRUE        |2 exemptions    |   11|
| 2017|TRUE        |3 deductions    |   40|
| 2017|TRUE        |6 other credits |   38|
| 2017|FALSE       |1 state AGI     | 9640|
| 2017|FALSE       |2 exemptions    |  209|
| 2017|FALSE       |3 deductions    | 1784|
| 2017|FALSE       |5 state EITC    |  999|
| 2017|FALSE       |6 other credits |   13|
| 2018|TRUE        |1 state AGI     | 1452|
| 2018|TRUE        |2 exemptions    |   41|
| 2018|TRUE        |3 deductions    |   75|
| 2018|TRUE        |6 other credits |   34|
| 2018|FALSE       |1 state AGI     | 8325|
| 2018|FALSE       |2 exemptions    |  183|
| 2018|FALSE       |3 deductions    |  912|
| 2018|FALSE       |5 state EITC    |  994|
| 2018|FALSE       |6 other credits |    8|
| 2019|TRUE        |1 state AGI     | 2097|
| 2019|TRUE        |2 exemptions    |   33|
| 2019|TRUE        |3 deductions    | 3192|
| 2019|TRUE        |6 other credits |   17|
| 2019|FALSE       |1 state AGI     | 8961|
| 2019|FALSE       |2 exemptions    |  152|
| 2019|FALSE       |3 deductions    | 1815|
| 2019|FALSE       |5 state EITC    |  534|
| 2019|FALSE       |6 other credits |    8|
| 2020|TRUE        |1 state AGI     | 1471|
| 2020|TRUE        |2 exemptions    |   39|
| 2020|TRUE        |3 deductions    |   75|
| 2020|TRUE        |5 state EITC    |  142|
| 2020|TRUE        |6 other credits |   34|
| 2020|FALSE       |1 state AGI     | 8963|
| 2020|FALSE       |2 exemptions    |  172|
| 2020|FALSE       |3 deductions    |  951|
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
|MD    |taxsim |     2017|     2020|input-coverage     |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): Maryland allows itemized deductions built from federal Schedule A less state income tax, and the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem where nothing can identify them as SALT to strip. Measured 2026-08-22: federal itemizers match at 0.179 against 0.921 for non-itemizers, pooled 2017-2020                                                                                                                                                                                                                                     |

