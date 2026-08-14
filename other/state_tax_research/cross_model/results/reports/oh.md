# Cross-model validation: OH

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 12679|    9421|   0.7982|    0.8603|         0.8890|          0.9134|          0.2670|          0.0609|    125.8476|
| 2017|taxsim       |  7834|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 12639|    9439|   0.7932|    0.8545|         0.8863|          0.9092|          0.2601|          0.0478|    125.7989|
| 2018|taxsim       |  7876|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 12763|    9461|   0.8018|    0.8532|         0.8888|          0.9098|          0.3448|          0.0978|    180.0092|
| 2019|taxsim       |  7751|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 12616|    9037|   0.7788|    0.8304|         0.8911|          0.9118|          0.3400|          0.0933|    184.3852|
| 2020|taxsim       |  7897|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |   912|     238|   0.7697|    0.8640|         0.9748|          0.9874|          0.3004|          0.0918|  -6191.2235|
| 2021|policyengine |   624|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |   893|     265|   0.7872|    0.8567|         0.9585|          0.9698|          0.3113|          0.1062|  -5199.0505|
| 2022|policyengine |   637|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |   921|     297|   0.7818|    0.8436|         0.9630|          0.9663|          0.2877|          0.1014|    992.7666|
| 2023|policyengine |   612|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |   917|     307|   0.7677|    0.8332|         0.9577|          0.9739|          0.2781|          0.0031|  -4753.2355|
| 2024|policyengine |   614|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3203|
| 2017|TRUE        |5 state EITC    |   13|
| 2017|TRUE        |6 other credits |  416|
| 2017|TRUE        |7 rate/rounding |  590|
| 2017|FALSE       |1 state AGI     | 5020|
| 2017|FALSE       |5 state EITC    |  223|
| 2017|FALSE       |6 other credits |   10|
| 2017|FALSE       |7 rate/rounding |   20|
| 2018|TRUE        |1 state AGI     | 3237|
| 2018|TRUE        |5 state EITC    |    5|
| 2018|TRUE        |6 other credits |  445|
| 2018|TRUE        |7 rate/rounding |  595|
| 2018|FALSE       |1 state AGI     | 5037|
| 2018|FALSE       |5 state EITC    |  220|
| 2018|FALSE       |6 other credits |   12|
| 2018|FALSE       |7 rate/rounding |   13|
| 2019|TRUE        |1 state AGI     | 3049|
| 2019|TRUE        |6 other credits |  388|
| 2019|TRUE        |7 rate/rounding |  632|
| 2019|FALSE       |1 state AGI     | 4989|
| 2019|FALSE       |5 state EITC    |  139|
| 2019|FALSE       |6 other credits |    5|
| 2019|FALSE       |7 rate/rounding |   12|
| 2020|TRUE        |1 state AGI     | 3125|
| 2020|TRUE        |6 other credits |  349|
| 2020|TRUE        |7 rate/rounding |  612|
| 2020|FALSE       |1 state AGI     | 5469|
| 2020|FALSE       |5 state EITC    |  111|
| 2020|FALSE       |6 other credits |   10|
| 2020|FALSE       |7 rate/rounding |   12|

## Known differences applied

|state |model        | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                            |
|:-----|:------------|--------:|--------:|:--------------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                    |
|ALL   |taxsim       |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                               |
|OH    |taxsim       |     2017|     2020|state-law      |exclude  |TAXSIM does not model the OH Business Income Deduction (IT BUS, R.C. 5747.01(A)(31)): first $250k/$125k-MFS of business income is deducted and the excess taxed at a flat 3%, while TAXSIM taxes business income at regular schedule rates; excluded via predicate on records claiming the BID                                         |
|OH    |policyengine |     2021|     2024|state-law      |exclude  |PolicyEngine US does not model the OH Business Income Deduction either (verified 1.775.7: no IT BUS variable or parameter under gov/states/oh; deductions limited to 529/medical/educator/conformity/179-addback/uniformed-services), so business income is taxed at regular rates; excluded via predicate on records claiming the BID |
|OH    |taxsim       |     2017|     2020|data-proxy     |annotate |Residual +$650 point-mass cluster (~150/yr): TAXSIM grants the OH Joint Filing Credit (5-20% capped $650) on returns where our earned-income proxy for each spouse's $500 qualifying-income test denies it; suspected mechanism is qualifying income beyond earnings (documented proxy limitation in the OH tracker row)               |
|OH    |taxsim       |     2017|     2020|data-proxy     |annotate |Residual -$200 point-mass cluster (~150/yr): we grant the OH retirement income credit (table max $200) on records where TAXSIM does not; suspected retirement-income input/concept difference (43% of non-BID residual mismatches have st_retire_credit > 0)                                                                           |

