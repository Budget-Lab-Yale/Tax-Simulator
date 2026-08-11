# Cross-model validation: CO

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3664|    0.4696|         0.5061|          0.5752|          0.1195|        138.8989|    5826.001|
| 2018|taxsim       | 20515|   13144|   0.4073|    0.5279|         0.5618|          0.6528|          0.1219|         68.4430|   -5507.212|
| 2019|taxsim       | 20514|   13088|   0.4042|    0.5264|         0.5620|          0.6558|          0.1212|         67.7879|   -5377.670|
| 2020|taxsim       | 20513|   12682|   0.3973|    0.5183|         0.5487|          0.6402|          0.1189|         75.2056|   -6255.132|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1533|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1531|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6465|
| 2017|TRUE        |4 taxable income |    1|
| 2017|FALSE       |1 state AGI      | 6531|
| 2018|TRUE        |1 state AGI      | 5759|
| 2018|TRUE        |4 taxable income |    1|
| 2018|FALSE       |1 state AGI      | 6399|
| 2019|TRUE        |1 state AGI      | 5733|
| 2019|FALSE       |1 state AGI      | 6490|
| 2020|TRUE        |1 state AGI      | 5723|
| 2020|FALSE       |1 state AGI      | 6639|
| 2020|FALSE       |4 taxable income |    2|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:-----|:------------|--------:|--------:|:----------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                         |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                         |
|CO    |policyengine |     2021|     2024|transfer-netting |exclude  |PolicyEngine nets TABOR refund mechanisms (co_sales_tax_refund: 2022 six-tier $153+ by AGI doubled joint, 2023 flat $800/$1,600, 2024 tiers $177+) into state_income_tax; our liability concept excludes TABOR refunds (we encode TABOR rate reductions, which are liability). PE's pre-refund co_income_tax_before_refundable_credits matches our calculator exactly on hand cases                                                                                                                                                              |
|CO    |taxsim       |     2017|     2020|structural       |annotate |Triage verified NO CO encoding defects: seven probe shapes match TAXSIM to the cent (all rate vintages incl. the 2019 TABOR 4.5%, MFJ, pension exclusion zeroing, and the SS-within-cap + pension mechanics on a 67-year-old with gross SS 25k / pension 20k / wages 30k). The residual clean-match gap is the pre-registered federal-side structural wedge (SALT circularity, sales-tax imputation, itemization flips) which a federal-TAXABLE-income start absorbs dollar-for-dollar into the state base -- concentrated in pre-2018 itemizers |

