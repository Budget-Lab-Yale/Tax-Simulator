# Cross-model validation: UT

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 12892|    9160|   0.6935|    0.8036|         0.7486|          0.8217|          0.1874|          0.1550|    -28.4781|
| 2017|taxsim       |  7621|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 12729|    9107|   0.7894|    0.8823|         0.8798|          0.9284|          0.2008|          0.0046|    165.7407|
| 2018|taxsim       |  7786|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 12589|    9006|   0.7906|    0.8801|         0.8774|          0.9268|          0.2019|          0.1034|   -131.3870|
| 2019|taxsim       |  7925|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 12599|    8669|   0.7653|    0.8571|         0.8830|          0.9316|          0.1932|          0.2011|     80.8274|
| 2020|taxsim       |  7914|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.6459|    0.7773|         0.9284|          0.9559|          0.2389|          0.3195|    -49.8068|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.7046|    0.7809|         0.9196|          0.9409|          0.2542|          1.7498|    133.5529|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.7010|    0.7701|         0.9060|          0.9274|          0.2645|          0.3271|    115.2873|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.6852|    0.7661|         0.9326|          0.9543|          0.2600|          0.0271|   -340.2655|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2364|
| 2017|TRUE        |2 exemptions    | 1844|
| 2017|TRUE        |6 other credits |   61|
| 2017|FALSE       |1 state AGI     | 4762|
| 2017|FALSE       |2 exemptions    |   89|
| 2017|FALSE       |6 other credits |    7|
| 2018|TRUE        |1 state AGI     | 2108|
| 2018|TRUE        |2 exemptions    |  374|
| 2018|TRUE        |6 other credits |  580|
| 2018|FALSE       |1 state AGI     | 4753|
| 2018|FALSE       |2 exemptions    |   58|
| 2018|FALSE       |6 other credits |   23|
| 2019|TRUE        |1 state AGI     | 2200|
| 2019|TRUE        |2 exemptions    |  367|
| 2019|TRUE        |6 other credits |  551|
| 2019|FALSE       |1 state AGI     | 4796|
| 2019|FALSE       |2 exemptions    |   54|
| 2019|FALSE       |6 other credits |   20|
| 2020|TRUE        |1 state AGI     | 2152|
| 2020|TRUE        |2 exemptions    |  333|
| 2020|TRUE        |6 other credits |  541|
| 2020|FALSE       |1 state AGI     | 5273|
| 2020|FALSE       |2 exemptions    |   51|
| 2020|FALSE       |6 other credits |   16|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|UT    |taxsim |     2017|     2020|external-model-bug |exclude  |TAXSIM grants the UT retirement credit to ANY record with Social Security income, ignoring both the born-before-1953 cohort gate and the 2.5c/$ MAGI phase-out: probe-verified flat $288/person (= 6% x $4,800; $576/couple; $271 under the 2017 vintage constant) paid to a 40-year-old with $2M wages and $1 of SS, at any income. Our encoding applies the cohort gate and phase-out per TC-40 instructions. Excluded via predicate on SS receipt (the exposure set)                                                                                                                                                                   |
|UT    |taxsim |     2017|     2020|structural         |exclude  |TAXSIM derives head-of-household treatment from dependents presence and ignores mstat (probe: single+2deps and HoH+2deps return identical siitax and credits to the cent; HoH+0deps computes as single). PUF returns filed single-with-dependents (and HoH returns whose dependents do not map into the crosswalk dep slots) therefore get the wrong federal standard deduction and phase-out threshold inside the UT taxpayer-credit base: symmetric +/-$464 (2019) masses = 6% x (18,350 - 12,200) + 1.3% x threshold gap. Input-representation limit, not fixable in the crosswalk. Excluded via predicate on the exposure set         |
|UT    |taxsim |     2017|     2020|structural         |annotate |Stage-table caveat: our UT representation carries the exemption piece of the taxpayer tax credit (75% of federal exemptions in 2017; $579/dependent 2018+) inside the credit base, so st_exempt = 0 by design and v33-vs-st_exempt comparisons misattribute UT wedges to the exemptions stage (zero liability effect wherever the credit is phased out). Read UT stage tables credits-first                                                                                                                                                                                                                                               |
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

