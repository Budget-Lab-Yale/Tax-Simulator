# Cross-model validation: MN

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9522|    7072|   0.7052|    0.7825|         0.8220|          0.8586|          0.1879|          0.0049|  -2143.0996|
| 2017|taxsim       | 10991|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9526|    7154|   0.7440|    0.8125|         0.8513|          0.8829|          0.2023|          0.0048|   -115.5457|
| 2018|taxsim       | 10989|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9522|    7115|   0.6569|    0.7265|         0.7293|          0.7830|          0.1936|          0.0035|     88.7487|
| 2019|taxsim       | 10992|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9549|    6800|   0.6427|    0.7111|         0.7268|          0.7800|          0.1886|          0.0069|    -38.9612|
| 2020|taxsim       | 10964|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     269|   0.2285|    0.3431|         0.7658|          0.8216|          0.0840|        474.7383| -16551.3197|
| 2022|policyengine |  1530|     317|   0.2876|    0.3451|         0.7697|          0.8139|          0.0882|        503.5872|   3258.9421|
| 2023|policyengine |  1533|     358|   0.2740|    0.3392|         0.7430|          0.7682|          0.0600|        475.4755|  23730.3031|
| 2024|policyengine |  1531|     363|   0.2554|    0.3220|         0.6915|          0.7548|          0.0555|        604.2394|  -3972.8533|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 7081|
| 2017|TRUE        |2 exemptions    |    1|
| 2017|FALSE       |1 state AGI     | 6478|
| 2018|TRUE        |1 state AGI     | 2004|
| 2018|TRUE        |2 exemptions    |  341|
| 2018|TRUE        |3 deductions    | 3493|
| 2018|TRUE        |5 state EITC    |    8|
| 2018|TRUE        |6 other credits |   24|
| 2018|FALSE       |1 state AGI     | 5248|
| 2018|FALSE       |2 exemptions    |    6|
| 2018|FALSE       |3 deductions    |  862|
| 2018|FALSE       |5 state EITC    |   22|
| 2019|TRUE        |1 state AGI     | 2027|
| 2019|TRUE        |2 exemptions    |  203|
| 2019|TRUE        |3 deductions    | 4962|
| 2019|TRUE        |5 state EITC    |   40|
| 2019|TRUE        |6 other credits |   12|
| 2019|FALSE       |1 state AGI     | 5430|
| 2019|FALSE       |2 exemptions    |    5|
| 2019|FALSE       |3 deductions    |  806|
| 2019|FALSE       |5 state EITC    |   29|
| 2020|TRUE        |1 state AGI     | 2208|
| 2020|TRUE        |2 exemptions    |  203|
| 2020|TRUE        |3 deductions    | 4528|
| 2020|TRUE        |5 state EITC    |  305|
| 2020|TRUE        |6 other credits |  137|
| 2020|TRUE        |7 rate/rounding |   20|
| 2020|FALSE       |1 state AGI     | 5582|
| 2020|FALSE       |2 exemptions    |   10|
| 2020|FALSE       |3 deductions    |  584|
| 2020|FALSE       |5 state EITC    |  362|
| 2020|FALSE       |6 other credits |   10|
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
|MN    |taxsim |     2017|     2020|input-coverage |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate                                                                                                                                                                                 |

