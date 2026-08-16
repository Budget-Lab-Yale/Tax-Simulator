# Cross-model validation: RI

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.5645|    0.7172|         0.7791|          0.8712|          0.1154|          1.9512|    1462.613|
| 2018|taxsim       | 20515|   13144|   0.5638|    0.7164|         0.7774|          0.8723|          0.1134|          1.8961|    1461.345|
| 2019|taxsim       | 20514|   13088|   0.5626|    0.7131|         0.7856|          0.8758|          0.1140|          1.3526|    1558.959|
| 2020|taxsim       | 20513|   12682|   0.5558|    0.7078|         0.7779|          0.8723|          0.1130|          2.4606|    1700.178|
| 2021|policyengine |  1240|     189|   0.3218|    0.4492|         0.9418|          0.9735|          0.0702|        168.7464|   -5608.931|
| 2021|policyengine |   296|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     317|   0.3856|    0.4621|         0.9211|          0.9401|          0.1033|        162.1317|    3154.938|
| 2023|policyengine |  1533|     357|   0.3757|    0.4697|         0.9132|          0.9300|          0.1011|        173.1544|   17401.501|
| 2024|policyengine |  1531|     364|   0.3828|    0.4585|         0.9258|          0.9423|          0.1006|        196.2489|   -2084.939|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2459|
| 2017|TRUE        |2 exemptions    |  142|
| 2017|TRUE        |3 deductions    |  163|
| 2017|TRUE        |6 other credits |   93|
| 2017|TRUE        |7 rate/rounding |   35|
| 2017|FALSE       |1 state AGI     | 4860|
| 2017|FALSE       |2 exemptions    |    2|
| 2017|FALSE       |3 deductions    |  104|
| 2017|FALSE       |5 state EITC    | 1073|
| 2017|FALSE       |6 other credits |    2|
| 2018|TRUE        |1 state AGI     | 2460|
| 2018|TRUE        |2 exemptions    |  150|
| 2018|TRUE        |3 deductions    |  175|
| 2018|TRUE        |6 other credits |  117|
| 2018|TRUE        |7 rate/rounding |   24|
| 2018|FALSE       |1 state AGI     | 4876|
| 2018|FALSE       |2 exemptions    |    1|
| 2018|FALSE       |3 deductions    |   99|
| 2018|FALSE       |5 state EITC    | 1044|
| 2018|FALSE       |7 rate/rounding |    2|
| 2019|TRUE        |1 state AGI     | 2361|
| 2019|TRUE        |2 exemptions    |  155|
| 2019|TRUE        |3 deductions    |  156|
| 2019|TRUE        |6 other credits |   99|
| 2019|TRUE        |7 rate/rounding |   35|
| 2019|FALSE       |1 state AGI     | 4969|
| 2019|FALSE       |2 exemptions    |    3|
| 2019|FALSE       |3 deductions    |  105|
| 2019|FALSE       |5 state EITC    | 1088|
| 2019|FALSE       |6 other credits |    2|
| 2020|TRUE        |1 state AGI     | 2401|
| 2020|TRUE        |2 exemptions    |  145|
| 2020|TRUE        |3 deductions    |  152|
| 2020|TRUE        |6 other credits |   85|
| 2020|TRUE        |7 rate/rounding |   34|
| 2020|FALSE       |1 state AGI     | 5112|
| 2020|FALSE       |2 exemptions    |   11|
| 2020|FALSE       |3 deductions    |  122|
| 2020|FALSE       |5 state EITC    | 1044|
| 2020|FALSE       |6 other credits |    4|
| 2020|FALSE       |7 rate/rounding |    1|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:-----|:------------|--------:|--------:|:----------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|RI    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books Rhode Island's one-time 2022 child tax rebate ($250 per child, maximum 3, federal AGI at or below $100,000 single / $200,000 joint; FY2023 budget H 7123) into TAX YEAR 2021 via ri_child_tax_rebate -- the year the eligibility return was filed. The rebate was paid as a MAILED CHECK from October 2022 and is not a line on RI-1040, so it is outside our liability concept. Predicted before the run from the package source and CONFIRMED on the first pass: 72 of ~100 clean mismatches sit at exactly +250/+500/+750, and the 2021 cell reads 0.681 against 0.930-0.942 in 2022-2024. Same class as issues-doc P5. Excluded via predicate on the exported credit |

