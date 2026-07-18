# Cross-model validation: TN

Class: narrow | Generated: 2026-07-18 | Verdict: **PASS**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.9894|    0.9924|         0.9913|          0.9944|          0.6429|               0|    -36.3925|
| 2018|taxsim       | 20515|   13504|   0.9910|    0.9942|         0.9924|          0.9959|          0.6407|               0|    -32.7217|
| 2019|taxsim       | 20514|   13433|   0.9906|    0.9948|         0.9924|          0.9968|          0.6285|               0|    -54.5583|
| 2020|taxsim       | 20513|   13070|   0.9917|    0.9967|         0.9930|          0.9983|          0.6338|               0|    -21.0485|
| 2021|policyengine |  1536|     270|   1.0000|    1.0000|         1.0000|          1.0000|          1.0000|               0|      0.0000|
| 2022|policyengine |  1530|     314|   1.0000|    1.0000|         1.0000|          1.0000|          1.0000|               0|      0.0000|
| 2023|policyengine |  1533|     352|   1.0000|    1.0000|         1.0000|          1.0000|          1.0000|               0|      0.0000|
| 2024|policyengine |  1531|     359|   1.0000|    1.0000|         1.0000|          1.0000|          1.0000|               0|      0.0000|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage        |   n|
|----:|:-----------|:------------|---:|
| 2017|TRUE        |1 state AGI  | 114|
| 2017|TRUE        |2 exemptions |   3|
| 2017|FALSE       |1 state AGI  | 101|
| 2018|TRUE        |1 state AGI  | 102|
| 2018|TRUE        |2 exemptions |   1|
| 2018|FALSE       |1 state AGI  |  82|
| 2019|TRUE        |1 state AGI  | 102|
| 2019|FALSE       |1 state AGI  |  91|
| 2020|TRUE        |1 state AGI  |  90|
| 2020|TRUE        |2 exemptions |   1|
| 2020|FALSE       |1 state AGI  |  80|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

