# Cross-model validation: MN

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9033|    6358|   0.7179|    0.8033|         0.8746|          0.9173|          0.1894|          0.0370|   -495.3304|
| 2017|taxsim       | 11480|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  8988|    6280|   0.7432|    0.8247|         0.8830|          0.9215|          0.1951|          0.0791|     51.2984|
| 2018|taxsim       | 11527|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  8934|    6271|   0.6726|    0.7483|         0.7697|          0.8235|          0.1882|          0.0041|    144.5582|
| 2019|taxsim       | 11580|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  8963|    5948|   0.6597|    0.7339|         0.7724|          0.8246|          0.1846|          0.0338|    140.9480|
| 2020|taxsim       | 11550|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     364|   0.4403|    0.6331|         0.8187|          0.8654|          0.1229|         26.1808|   -238.2360|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.5355|    0.6389|         0.8373|          0.8821|          0.1201|          5.5318|    158.2424|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.5428|    0.6646|         0.8248|          0.8526|          0.0916|          4.3709|    422.7174|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     459|   0.5417|    0.6583|         0.8214|          0.8715|          0.0887|          9.8602|   -545.7453|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |     n|
|----:|:-----------|:---------------|-----:|
| 2017|TRUE        |1 state AGI     |  1073|
| 2017|TRUE        |2 exemptions    |     1|
| 2017|FALSE       |1 state AGI     | 12597|
| 2018|TRUE        |1 state AGI     |  1349|
| 2018|TRUE        |2 exemptions    |    20|
| 2018|TRUE        |3 deductions    |   636|
| 2018|TRUE        |5 state EITC    |     5|
| 2018|TRUE        |6 other credits |     5|
| 2018|FALSE       |1 state AGI     |  8519|
| 2018|FALSE       |2 exemptions    |   118|
| 2018|FALSE       |3 deductions    |  1825|
| 2018|FALSE       |5 state EITC    |    18|
| 2018|FALSE       |6 other credits |     3|
| 2019|TRUE        |1 state AGI     |  1625|
| 2019|TRUE        |2 exemptions    |    19|
| 2019|TRUE        |3 deductions    |  1261|
| 2019|TRUE        |5 state EITC    |    16|
| 2019|TRUE        |6 other credits |     3|
| 2019|FALSE       |1 state AGI     |  8820|
| 2019|FALSE       |2 exemptions    |    82|
| 2019|FALSE       |3 deductions    |  1766|
| 2019|FALSE       |5 state EITC    |    23|
| 2020|TRUE        |1 state AGI     |  1674|
| 2020|TRUE        |2 exemptions    |    22|
| 2020|TRUE        |3 deductions    |   923|
| 2020|TRUE        |5 state EITC    |   234|
| 2020|TRUE        |6 other credits |    63|
| 2020|FALSE       |1 state AGI     |  9102|
| 2020|FALSE       |2 exemptions    |    80|
| 2020|FALSE       |3 deductions    |  1522|
| 2020|FALSE       |5 state EITC    |   362|
| 2020|FALSE       |6 other credits |    37|
| 2020|FALSE       |7 rate/rounding |    30|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-----|:------|--------:|--------:|:--------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|MN    |taxsim |     2017|     2020|structural     |annotate |Triage verified NO MN encoding defects on definable shapes: eleven probe cases match TAXSIM to the cent or within ~$1 indexed-constant rounding across ALL THREE regimes (2017 fed-taxable start; the 2018 rebuilt pre-TCJA stack; 2019-20 fed-AGI + MN's own deduction/exemption stack), including dependent exemptions, the sliding SS subtraction on a 67-year-old with SS+pension+wages, the marriage credit (implicit in the exact MFJ 60/40 case), and the Working Family Credit at phase-in/phase-out/childless/2-child edges (exact incl. refundable negatives). Residual clean-match gap concentrates in itemizers (M1SA component differences + the pre-registered SALT-circularity rebuild) and the packet-documented 2017 M1M Pease/exemption addbacks ($186-314k itemizers) |
|MN    |taxsim |     2017|     2020|input-coverage |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate                                                                                                                                                                                 |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                                                                                                                                |

