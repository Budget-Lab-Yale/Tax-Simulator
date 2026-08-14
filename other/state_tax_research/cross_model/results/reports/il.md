# Cross-model validation: IL

Class: broad | Generated: 2026-08-13 | Verdict: **PASS**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 14313|   10754|   0.8318|    0.9306|         0.9955|          1.0000|          0.1842|          0.0028|     70.2124|
| 2017|taxsim       |  6200|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 14144|   10699|   0.8288|    0.9239|         0.9952|          1.0000|          0.1871|          0.0029|     64.1852|
| 2018|taxsim       |  6371|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 14147|   10651|   0.8238|    0.9189|         0.9965|          1.0000|          0.1878|          0.0029|     87.8731|
| 2019|taxsim       |  6367|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 14000|   10154|   0.7954|    0.8907|         0.9953|          1.0000|          0.1858|          0.0030|    105.8447|
| 2020|taxsim       |  6513|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     317|   0.6444|    0.7183|         0.9685|          0.9937|          0.0980|          0.0109|  -1631.9127|
| 2023|policyengine |  1533|     357|   0.6464|    0.7195|         0.9776|          0.9916|          0.1005|          0.0059|  10092.9584|
| 2024|policyengine |  1531|     364|   0.6401|    0.7028|         0.9780|          0.9945|          0.0980|          0.0096|  -6584.0989|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage        |    n|
|----:|:-----------|:------------|----:|
| 2017|TRUE        |1 state AGI  |  111|
| 2017|TRUE        |2 exemptions | 2275|
| 2017|FALSE       |1 state AGI  | 4953|
| 2017|FALSE       |2 exemptions |   79|
| 2017|FALSE       |5 state EITC | 1174|
| 2018|TRUE        |1 state AGI  |  131|
| 2018|TRUE        |2 exemptions | 2364|
| 2018|FALSE       |1 state AGI  | 5028|
| 2018|FALSE       |2 exemptions |   67|
| 2018|FALSE       |5 state EITC | 1189|
| 2019|TRUE        |1 state AGI  |  141|
| 2019|TRUE        |2 exemptions | 2332|
| 2019|FALSE       |1 state AGI  | 5050|
| 2019|FALSE       |2 exemptions |   72|
| 2019|FALSE       |5 state EITC | 1250|
| 2020|TRUE        |1 state AGI  |  145|
| 2020|TRUE        |2 exemptions | 2430|
| 2020|FALSE       |1 state AGI  | 5562|
| 2020|FALSE       |2 exemptions |   70|
| 2020|FALSE       |5 state EITC | 1159|

## Known differences applied

|state |model        | year_min| year_max|category        |action   |description                                                                                                                                                                                                                                          |
|:-----|:------------|--------:|--------:|:---------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural      |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                              |
|ALL   |taxsim       |     2017|     2024|structural      |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                              |
|ALL   |taxsim       |     2021|     2024|vintage         |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                              |
|ALL   |taxsim       |     2017|     2024|input-coverage  |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                  |
|ALL   |taxsim       |     2017|     2024|input-coverage  |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                             |
|ALL   |taxsim       |     2017|     2024|federal-side    |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                             |
|IL    |taxsim       |     2017|     2020|state-law       |exclude  |TAXSIM does not model IL's exemption disallowance above the AGI threshold ($250k single / $500k joint), granting exemptions we correctly deny; excluded via predicate on the affected high-AGI records                                               |
|IL    |policyengine |     2021|     2021|one-time-rebate |exclude  |PolicyEngine nets the one-time 2021 IL individual income tax rebate ($50/filer + $100/dependent, paid 2022) into 2021 state_income_tax; one-time rebates are outside our v1 state liability concept. 2022-2024 IL cells validate at 99.2-99.5% clean |

