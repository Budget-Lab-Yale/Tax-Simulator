# Cross-model validation: OH

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 11832|    9131|   0.8101|    0.8818|         0.8741|          0.9144|          0.2780|          0.0617|     44.7679|
| 2017|taxsim       |  8681|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 11694|    9091|   0.8089|    0.8775|         0.8748|          0.9101|          0.2711|          0.0506|     36.9366|
| 2018|taxsim       |  8821|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 11758|    9092|   0.8206|    0.8800|         0.8762|          0.9122|          0.3635|          0.0965|     44.7745|
| 2019|taxsim       |  8756|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 11632|    8675|   0.7943|    0.8541|         0.8788|          0.9152|          0.3577|          0.0960|     61.3070|
| 2020|taxsim       |  8881|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |   817|     233|   0.8029|    0.9143|         0.9700|          0.9914|          0.3293|          0.0943|    936.2437|
| 2021|policyengine |   719|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |   785|     254|   0.8408|    0.9172|         0.9606|          0.9803|          0.3389|          0.1066|    564.9694|
| 2022|policyengine |   745|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |   816|     285|   0.8297|    0.8971|         0.9614|          0.9719|          0.3162|          0.1035|    442.0026|
| 2023|policyengine |   717|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |   810|     297|   0.8235|    0.8901|         0.9596|          0.9798|          0.3074|          0.0044|      9.0901|
| 2024|policyengine |   721|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3964|
| 2017|TRUE        |5 state EITC    |   13|
| 2017|TRUE        |6 other credits |  301|
| 2017|TRUE        |7 rate/rounding |  320|
| 2017|FALSE       |1 state AGI     | 5117|
| 2017|FALSE       |5 state EITC    |  214|
| 2017|FALSE       |6 other credits |    5|
| 2017|FALSE       |7 rate/rounding |   12|
| 2018|TRUE        |1 state AGI     | 4037|
| 2018|TRUE        |5 state EITC    |    5|
| 2018|TRUE        |6 other credits |  329|
| 2018|TRUE        |7 rate/rounding |  302|
| 2018|FALSE       |1 state AGI     | 5137|
| 2018|FALSE       |5 state EITC    |  212|
| 2018|FALSE       |6 other credits |   10|
| 2018|FALSE       |7 rate/rounding |    6|
| 2019|TRUE        |1 state AGI     | 3892|
| 2019|TRUE        |6 other credits |  288|
| 2019|TRUE        |7 rate/rounding |  299|
| 2019|FALSE       |1 state AGI     | 5058|
| 2019|FALSE       |5 state EITC    |  136|
| 2019|FALSE       |6 other credits |    4|
| 2019|FALSE       |7 rate/rounding |    5|
| 2020|TRUE        |1 state AGI     | 3957|
| 2020|TRUE        |6 other credits |  248|
| 2020|TRUE        |7 rate/rounding |  272|
| 2020|FALSE       |1 state AGI     | 5568|
| 2020|FALSE       |5 state EITC    |  105|
| 2020|FALSE       |6 other credits |    4|
| 2020|FALSE       |7 rate/rounding |    3|

## Known differences applied

|state |model        | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|OH    |taxsim       |     2017|     2020|state-law      |exclude  |TAXSIM does not model the OH Business Income Deduction (IT BUS, R.C. 5747.01(A)(31)): first $250k/$125k-MFS of business income is deducted and the excess taxed at a flat 3%, while TAXSIM taxes business income at regular schedule rates; excluded via predicate on records claiming the BID                                                                                                                                                                                                                                                                                                                                            |
|OH    |policyengine |     2021|     2024|state-law      |exclude  |PolicyEngine US does not model the OH Business Income Deduction either (verified 1.775.7: no IT BUS variable or parameter under gov/states/oh; deductions limited to 529/medical/educator/conformity/179-addback/uniformed-services), so business income is taxed at regular rates; excluded via predicate on records claiming the BID                                                                                                                                                                                                                                                                                                    |
|OH    |taxsim       |     2017|     2020|data-proxy     |annotate |Residual +$650 point-mass cluster (~150/yr): TAXSIM grants the OH Joint Filing Credit (5-20% capped $650) on returns where our earned-income proxy for each spouse's $500 qualifying-income test denies it; suspected mechanism is qualifying income beyond earnings (documented proxy limitation in the OH tracker row)                                                                                                                                                                                                                                                                                                                  |
|OH    |taxsim       |     2017|     2020|data-proxy     |annotate |Residual -$200 point-mass cluster (~150/yr): we grant the OH retirement income credit (table max $200) on records where TAXSIM does not; suspected retirement-income input/concept difference (43% of non-BID residual mismatches have st_retire_credit > 0)                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |both         |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

