# Cross-model validation: IL

Class: broad | Generated: 2026-08-16 | Verdict: **PASS**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 13733|   10498|   0.8290|    0.9380|         0.9824|          1.0000|          0.1802|          0.0040|     32.3694|
| 2017|taxsim       |  6780|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 13515|   10392|   0.8235|    0.9321|         0.9816|          1.0000|          0.1803|          0.0046|     27.8432|
| 2018|taxsim       |  7000|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 13446|   10309|   0.8193|    0.9284|         0.9809|          1.0000|          0.1819|          0.0045|     35.6152|
| 2019|taxsim       |  7068|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 13341|    9834|   0.7880|    0.8975|         0.9779|          1.0000|          0.1798|          0.0046|     52.0460|
| 2020|taxsim       |  7172|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.6652|    0.7809|         0.9502|          0.9934|          0.1174|          0.4194|    271.3525|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.6672|    0.7761|         0.9524|          0.9911|          0.1296|          0.4519|    310.8536|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.6548|    0.7574|         0.9538|          0.9942|          0.1200|          0.4335|    -89.0450|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage        |    n|
|----:|:-----------|:------------|----:|
| 2017|TRUE        |1 state AGI  | 2247|
| 2017|TRUE        |2 exemptions |  439|
| 2017|FALSE       |1 state AGI  | 5043|
| 2017|FALSE       |2 exemptions |   14|
| 2017|FALSE       |5 state EITC | 1133|
| 2018|TRUE        |1 state AGI  | 2398|
| 2018|TRUE        |2 exemptions |  433|
| 2018|FALSE       |1 state AGI  | 5104|
| 2018|FALSE       |2 exemptions |   25|
| 2018|FALSE       |5 state EITC | 1148|
| 2019|TRUE        |1 state AGI  | 2469|
| 2019|TRUE        |2 exemptions |  389|
| 2019|FALSE       |1 state AGI  | 5144|
| 2019|FALSE       |2 exemptions |   18|
| 2019|FALSE       |5 state EITC | 1209|
| 2020|TRUE        |1 state AGI  | 2518|
| 2020|TRUE        |2 exemptions |  446|
| 2020|FALSE       |1 state AGI  | 5660|
| 2020|FALSE       |2 exemptions |   15|
| 2020|FALSE       |5 state EITC | 1111|

## Known differences applied

|state |model        | year_min| year_max|category        |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:---------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural      |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural      |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage         |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage  |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage  |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side    |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|IL    |taxsim       |     2017|     2020|state-law       |exclude  |TAXSIM does not model IL's exemption disallowance above the AGI threshold ($250k single / $500k joint), granting exemptions we correctly deny; excluded via predicate on the affected high-AGI records                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|IL    |policyengine |     2021|     2021|one-time-rebate |exclude  |PolicyEngine nets the one-time 2021 IL individual income tax rebate ($50/filer + $100/dependent, paid 2022) into 2021 state_income_tax; one-time rebates are outside our v1 state liability concept. 2022-2024 IL cells validate at 99.2-99.5% clean                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |both         |     2017|     2024|structural      |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

