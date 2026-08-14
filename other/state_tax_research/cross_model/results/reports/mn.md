# Cross-model validation: MN

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3390|    0.4127|         0.4591|          0.5277|          0.0883|        297.6978|  -12117.845|
| 2018|taxsim       | 20515|   13144|   0.4210|    0.5433|         0.5628|          0.6732|          0.0963|         61.2016|   -1934.055|
| 2019|taxsim       | 20514|   13088|   0.3414|    0.4305|         0.4466|          0.5434|          0.0915|        231.9138|   -2224.542|
| 2020|taxsim       | 20513|   12682|   0.3211|    0.3891|         0.4190|          0.4915|          0.0904|        391.4047|   13683.514|
| 2021|policyengine |  1536|     269|   0.2285|    0.3431|         0.7658|          0.8216|          0.0840|        474.7383|  -16551.320|
| 2022|policyengine |  1530|     317|   0.2876|    0.3451|         0.7697|          0.8139|          0.0882|        503.5872|    3258.943|
| 2023|policyengine |  1533|     358|   0.2740|    0.3392|         0.7430|          0.7682|          0.0600|        475.4755|   23730.303|
| 2024|policyengine |  1531|     363|   0.2554|    0.3220|         0.6915|          0.7548|          0.0555|        604.2394|   -3972.853|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 7081|
| 2017|TRUE        |2 exemptions    |    1|
| 2017|FALSE       |1 state AGI     | 6478|
| 2018|TRUE        |1 state AGI     | 1962|
| 2018|TRUE        |2 exemptions    |  340|
| 2018|TRUE        |3 deductions    | 3414|
| 2018|TRUE        |5 state EITC    |    8|
| 2018|TRUE        |6 other credits |   22|
| 2018|FALSE       |1 state AGI     | 5243|
| 2018|FALSE       |2 exemptions    |    6|
| 2018|FALSE       |3 deductions    |  859|
| 2018|FALSE       |5 state EITC    |   24|
| 2019|TRUE        |1 state AGI     | 2027|
| 2019|TRUE        |2 exemptions    |  203|
| 2019|TRUE        |3 deductions    | 4961|
| 2019|TRUE        |5 state EITC    |   40|
| 2019|TRUE        |6 other credits |   12|
| 2019|FALSE       |1 state AGI     | 5428|
| 2019|FALSE       |2 exemptions    |    5|
| 2019|FALSE       |3 deductions    |  806|
| 2019|FALSE       |5 state EITC    |   29|
| 2020|TRUE        |1 state AGI     | 2189|
| 2020|TRUE        |2 exemptions    |  203|
| 2020|TRUE        |3 deductions    | 4497|
| 2020|TRUE        |5 state EITC    |  318|
| 2020|TRUE        |6 other credits |  140|
| 2020|TRUE        |7 rate/rounding |   21|
| 2020|FALSE       |1 state AGI     | 5580|
| 2020|FALSE       |2 exemptions    |   10|
| 2020|FALSE       |3 deductions    |  578|
| 2020|FALSE       |5 state EITC    |  362|
| 2020|FALSE       |6 other credits |   14|
| 2020|FALSE       |7 rate/rounding |   14|

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

