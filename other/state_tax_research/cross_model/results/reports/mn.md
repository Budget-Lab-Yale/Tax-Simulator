# Cross-model validation: MN

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9033|    6873|   0.7179|    0.8033|         0.8206|          0.8670|          0.1894|          0.0370|   -495.3304|
| 2017|taxsim       | 11480|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  8988|    6886|   0.7432|    0.8247|         0.8419|          0.8883|          0.1951|          0.0791|     51.2984|
| 2018|taxsim       | 11527|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  8934|    6839|   0.6726|    0.7483|         0.7352|          0.7954|          0.1882|          0.0041|    144.5582|
| 2019|taxsim       | 11580|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  8963|    6535|   0.6597|    0.7339|         0.7344|          0.7945|          0.1846|          0.0338|    140.9480|
| 2020|taxsim       | 11550|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.2756|    0.4078|         0.7625|          0.8314|          0.1041|        232.3064|    213.5257|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.3401|    0.4172|         0.7542|          0.8206|          0.1043|        262.3194|    608.1011|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     337|   0.3276|    0.4131|         0.7418|          0.7745|          0.0752|        263.0558|    905.6039|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     345|   0.3191|    0.4000|         0.7101|          0.7797|          0.0704|        332.1486|     -0.8801|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 7207|
| 2017|TRUE        |2 exemptions    |    1|
| 2017|FALSE       |1 state AGI     | 6463|
| 2018|TRUE        |1 state AGI     | 4469|
| 2018|TRUE        |2 exemptions    |  136|
| 2018|TRUE        |3 deductions    | 1695|
| 2018|TRUE        |5 state EITC    |    5|
| 2018|TRUE        |6 other credits |    8|
| 2018|FALSE       |1 state AGI     | 5399|
| 2018|FALSE       |2 exemptions    |    2|
| 2018|FALSE       |3 deductions    |  766|
| 2018|FALSE       |5 state EITC    |   18|
| 2019|TRUE        |1 state AGI     | 4886|
| 2019|TRUE        |2 exemptions    |  100|
| 2019|TRUE        |3 deductions    | 2348|
| 2019|TRUE        |5 state EITC    |   16|
| 2019|TRUE        |6 other credits |    3|
| 2019|FALSE       |1 state AGI     | 5559|
| 2019|FALSE       |2 exemptions    |    1|
| 2019|FALSE       |3 deductions    |  679|
| 2019|FALSE       |5 state EITC    |   23|
| 2020|TRUE        |1 state AGI     | 5022|
| 2020|TRUE        |2 exemptions    |   97|
| 2020|TRUE        |3 deductions    | 2004|
| 2020|TRUE        |5 state EITC    |  257|
| 2020|TRUE        |6 other credits |   90|
| 2020|TRUE        |7 rate/rounding |   17|
| 2020|FALSE       |1 state AGI     | 5754|
| 2020|FALSE       |2 exemptions    |    5|
| 2020|FALSE       |3 deductions    |  441|
| 2020|FALSE       |5 state EITC    |  339|
| 2020|FALSE       |6 other credits |   10|
| 2020|FALSE       |7 rate/rounding |   13|

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

