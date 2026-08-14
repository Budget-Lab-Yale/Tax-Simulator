# Cross-model validation: PA

Class: broad | Generated: 2026-08-13 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3864|    0.5165|         0.4779|          0.6263|          0.1634|         82.2711|    5643.372|
| 2018|taxsim       | 20515|   13144|   0.3797|    0.5097|         0.4706|          0.6174|          0.1584|         88.2315|    4689.885|
| 2019|taxsim       | 20514|   13088|   0.3825|    0.5133|         0.4757|          0.6235|          0.1595|         84.6784|    4790.427|
| 2020|taxsim       | 20513|   12682|   0.3591|    0.4956|         0.4460|          0.6015|          0.1540|        106.0969|    5432.805|
| 2021|policyengine |  1536|     269|   0.5514|    0.5944|         0.8625|          0.8773|          0.1172|          0.0050|    8936.640|
| 2022|policyengine |  1530|     316|   0.5523|    0.6013|         0.8259|          0.8608|          0.1163|          0.0053|   15809.141|
| 2023|policyengine |  1533|     357|   0.5597|    0.6008|         0.7815|          0.8123|          0.1096|          0.0048|   18432.497|
| 2024|policyengine |  1531|     365|   0.5336|    0.5807|         0.8055|          0.8384|          0.0914|          0.0209|   14375.765|

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

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                 |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                            |
|PA    |taxsim |     2017|     2020|state-law      |annotate |TAXSIM's PA base appears to net losses across income classes (median our st_agi minus v32 = +$10k among mismatches; 84% of mismatches positive, mean +$7.8k) where PA law bars cross-class and spousal offsets, flooring each class at zero; our within-unit class netting is itself a documented approximation. Note TAXSIM DOES model Tax Forgiveness (verified: 86% of forgiveness records match raw; back-adding the credit drops match to 14%) |
|PA    |taxsim |     2017|     2020|data-proxy     |annotate |Residual beyond the class-netting row: small Tax Forgiveness credit gaps (+$20-70, cred_gap = diff exactly, ~50/yr) from eligibility-income attribution differences in the Schedule SP table (both models proxy per-spouse eligibility income). With the class-netting row this attributes 100% of PA clean mismatches: 99% carry an AGI-base wedge (the netting class), 1% are pure forgiveness credit gaps                                        |

