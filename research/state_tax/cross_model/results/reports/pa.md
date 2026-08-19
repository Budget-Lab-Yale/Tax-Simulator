# Cross-model validation: PA

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4612|    0.6100|         0.5084|          0.6635|          0.1981|         26.2362|   1043.2954|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4560|    0.6055|         0.5020|          0.6567|          0.1940|         28.9838|    970.8726|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4641|    0.6137|         0.5095|          0.6636|          0.1975|         25.8149|    938.6653|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4366|    0.5937|         0.4816|          0.6445|          0.1905|         33.8560|   1036.4810|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.6920|    0.7381|         0.8774|          0.8927|          0.1519|          0.0035|    602.5665|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     300|   0.7020|    0.7520|         0.8567|          0.8867|          0.1560|          0.0035|    612.8422|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.7001|    0.7398|         0.8006|          0.8274|          0.1435|          0.0033|    159.1025|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     347|   0.6757|    0.7226|         0.8329|          0.8646|          0.1209|          0.0036|    314.1410|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 6775|
| 2017|TRUE        |6 other credits |   60|
| 2017|FALSE       |1 state AGI     | 5674|
| 2017|FALSE       |6 other credits |   78|
| 2018|TRUE        |1 state AGI     | 6914|
| 2018|TRUE        |6 other credits |   45|
| 2018|FALSE       |1 state AGI     | 5710|
| 2018|FALSE       |6 other credits |   56|
| 2019|TRUE        |1 state AGI     | 6816|
| 2019|TRUE        |6 other credits |   46|
| 2019|FALSE       |1 state AGI     | 5742|
| 2019|FALSE       |6 other credits |   64|
| 2020|TRUE        |1 state AGI     | 6974|
| 2020|TRUE        |6 other credits |   52|
| 2020|FALSE       |1 state AGI     | 6049|
| 2020|FALSE       |6 other credits |   71|

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

