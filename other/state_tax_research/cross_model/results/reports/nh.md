# Cross-model validation: NH

Class: narrow | Generated: 2026-08-13 | Verdict: **PASS**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.9981|    1.0000|         0.9994|          1.0000|          0.6860|               0|     -0.1108|
| 2018|taxsim       | 20515|   13144|   0.9982|    1.0000|         0.9988|          1.0000|          0.6797|               0|     -0.1085|
| 2019|taxsim       | 20514|   13088|   0.9978|    1.0000|         0.9990|          1.0000|          0.6700|               0|     -0.1307|
| 2020|taxsim       | 20513|   12682|   0.9980|    1.0000|         0.9991|          1.0000|          0.6740|               0|     -0.1196|
| 2021|policyengine |  1536|     272|   0.7708|    0.8060|         0.9669|          0.9816|          0.6296|               0|  -3520.6631|
| 2022|policyengine |  1530|     314|   0.7595|    0.7961|         0.9554|          0.9586|          0.6170|               0|  -4315.1263|
| 2023|policyengine |  1533|     348|   0.7789|    0.8330|         0.9569|          0.9626|          0.6419|               0|  -4052.1370|
| 2024|policyengine |  1531|     363|   0.7675|    0.8269|         0.9394|          0.9614|          0.6218|               0|  -4195.7308|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage       |  n|
|----:|:-----------|:-----------|--:|
| 2017|TRUE        |1 state AGI |  8|
| 2017|FALSE       |1 state AGI | 30|
| 2018|TRUE        |1 state AGI | 16|
| 2018|FALSE       |1 state AGI | 21|
| 2019|TRUE        |1 state AGI | 13|
| 2019|FALSE       |1 state AGI | 33|
| 2020|TRUE        |1 state AGI | 12|
| 2020|FALSE       |1 state AGI | 29|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

