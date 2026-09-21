# Cross-model validation: PA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6801|   0.4604|    0.5980|         0.7306|          0.8197|          0.1976|         28.5534|   1291.5780|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    8911|   0.4562|    0.5939|         0.6081|          0.7455|          0.1931|         31.0760|   1114.6931|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    8837|   0.4636|    0.6015|         0.6166|          0.7521|          0.1966|         29.0404|   1086.3215|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    8514|   0.4420|    0.5848|         0.5911|          0.7354|          0.1897|         36.2839|   1203.5976|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.6741|    0.7125|         0.8352|          0.8571|          0.1570|          0.0036|    949.7289|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     421|   0.6827|    0.7274|         0.8504|          0.8765|          0.1613|          0.0036|    927.1139|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.6811|    0.7182|         0.8034|          0.8269|          0.1443|          0.0034|    310.2786|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     459|   0.6609|    0.7043|         0.8105|          0.8388|          0.1261|          0.0036|    675.6600|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |     n|
|----:|:-----------|:---------------|-----:|
| 2017|TRUE        |1 state AGI     |  1920|
| 2017|TRUE        |6 other credits |    51|
| 2017|FALSE       |1 state AGI     | 10565|
| 2017|FALSE       |6 other credits |    88|
| 2018|TRUE        |1 state AGI     |  3768|
| 2018|TRUE        |6 other credits |    43|
| 2018|FALSE       |1 state AGI     |  8880|
| 2018|FALSE       |6 other credits |    59|
| 2019|TRUE        |1 state AGI     |  3687|
| 2019|TRUE        |6 other credits |    40|
| 2019|FALSE       |1 state AGI     |  8892|
| 2019|FALSE       |6 other credits |    72|
| 2020|TRUE        |1 state AGI     |  3761|
| 2020|TRUE        |6 other credits |    51|
| 2020|FALSE       |1 state AGI     |  9178|
| 2020|FALSE       |6 other credits |    73|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|PA    |taxsim |     2017|     2020|state-law      |annotate |TAXSIM's PA base appears to net losses across income classes (median our st_agi minus v32 = +$10k among mismatches; 84% of mismatches positive, mean +$7.8k) where PA law bars cross-class and spousal offsets, flooring each class at zero; our within-unit class netting is itself a documented approximation. Note TAXSIM DOES model Tax Forgiveness (verified: 86% of forgiveness records match raw; back-adding the credit drops match to 14%)                                                                                                                                                                                       |
|PA    |taxsim |     2017|     2020|data-proxy     |annotate |Residual beyond the class-netting row: small Tax Forgiveness credit gaps (+$20-70, cred_gap = diff exactly, ~50/yr) from eligibility-income attribution differences in the Schedule SP table (both models proxy per-spouse eligibility income). With the class-netting row this attributes 100% of PA clean mismatches: 99% carry an AGI-base wedge (the netting class), 1% are pure forgiveness credit gaps                                                                                                                                                                                                                              |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

