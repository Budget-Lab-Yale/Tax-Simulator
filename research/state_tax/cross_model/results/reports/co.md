# Cross-model validation: CO

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4295|    0.5491|         0.5321|          0.6068|          0.1392|         48.4899|    667.9224|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.4760|    0.6183|         0.5886|          0.6886|          0.1410|         22.8834|   -790.8139|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.4765|    0.6210|         0.5895|          0.6925|          0.1410|         21.8719|   -803.8525|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.4675|    0.6110|         0.5786|          0.6802|          0.1387|         24.2708|   -861.1947|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1533|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1531|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6578|
| 2017|TRUE        |4 taxable income |    1|
| 2017|FALSE       |1 state AGI      | 6524|
| 2018|TRUE        |1 state AGI      | 5962|
| 2018|TRUE        |4 taxable income |    1|
| 2018|FALSE       |1 state AGI      | 6393|
| 2019|TRUE        |1 state AGI      | 5942|
| 2019|FALSE       |1 state AGI      | 6485|
| 2020|TRUE        |1 state AGI      | 5925|
| 2020|FALSE       |1 state AGI      | 6652|
| 2020|FALSE       |4 taxable income |    2|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|CO    |policyengine |     2021|     2024|transfer-netting |exclude  |PolicyEngine nets TABOR refund mechanisms (co_sales_tax_refund: 2022 six-tier $153+ by AGI doubled joint, 2023 flat $800/$1,600, 2024 tiers $177+) into state_income_tax; our liability concept excludes TABOR refunds (we encode TABOR rate reductions, which are liability). PE's pre-refund co_income_tax_before_refundable_credits matches our calculator exactly on hand cases                                                                                                                                                                                                                                                       |
|CO    |taxsim       |     2017|     2020|structural       |annotate |Triage verified NO CO encoding defects: seven probe shapes match TAXSIM to the cent (all rate vintages incl. the 2019 TABOR 4.5%, MFJ, pension exclusion zeroing, and the SS-within-cap + pension mechanics on a 67-year-old with gross SS 25k / pension 20k / wages 30k). The residual clean-match gap is the pre-registered federal-side structural wedge (SALT circularity, sales-tax imputation, itemization flips) which a federal-TAXABLE-income start absorbs dollar-for-dollar into the state base -- concentrated in pre-2018 itemizers                                                                                          |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

