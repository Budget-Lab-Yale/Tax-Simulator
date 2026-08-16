# Cross-model validation: GA

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.5022|    0.6799|         0.5947|          0.7707|          0.1760|         14.6022|     245.954|
| 2018|taxsim       | 20515|   13144|   0.4950|    0.5788|         0.5944|          0.6816|          0.1801|         16.4879|   -4205.966|
| 2019|taxsim       | 20514|   13088|   0.4975|    0.5810|         0.5980|          0.6836|          0.1800|         15.5397|   -4272.155|
| 2020|taxsim       | 20513|   12682|   0.4923|    0.5799|         0.5928|          0.6784|          0.1724|         17.5951|   -4769.216|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     315|   0.2987|    0.4327|         0.7746|          0.8159|          0.1477|        158.7191|    3593.404|
| 2023|policyengine |  1533|     357|   0.2844|    0.4220|         0.7619|          0.8011|          0.1350|        161.6474|   17395.165|
| 2024|policyengine |  1531|     364|   0.4017|    0.4520|         0.9203|          0.9203|          0.1600|        221.0783|   -1322.886|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3480|
| 2017|TRUE        |3 deductions    | 1395|
| 2017|TRUE        |6 other credits |  343|
| 2017|TRUE        |7 rate/rounding |   88|
| 2017|FALSE       |1 state AGI     | 3695|
| 2017|FALSE       |3 deductions    |  821|
| 2017|FALSE       |6 other credits |  360|
| 2017|FALSE       |7 rate/rounding |   29|
| 2018|TRUE        |1 state AGI     | 3613|
| 2018|TRUE        |3 deductions    | 1397|
| 2018|TRUE        |6 other credits |  210|
| 2018|TRUE        |7 rate/rounding |  111|
| 2018|FALSE       |1 state AGI     | 3848|
| 2018|FALSE       |3 deductions    |  993|
| 2018|FALSE       |6 other credits |  155|
| 2018|FALSE       |7 rate/rounding |   34|
| 2019|TRUE        |1 state AGI     | 3501|
| 2019|TRUE        |3 deductions    | 1445|
| 2019|TRUE        |6 other credits |  220|
| 2019|TRUE        |7 rate/rounding |   95|
| 2019|FALSE       |1 state AGI     | 3874|
| 2019|FALSE       |3 deductions    |  966|
| 2019|FALSE       |6 other credits |  172|
| 2019|FALSE       |7 rate/rounding |   36|
| 2020|TRUE        |1 state AGI     | 3543|
| 2020|TRUE        |3 deductions    | 1320|
| 2020|TRUE        |6 other credits |  196|
| 2020|TRUE        |7 rate/rounding |  105|
| 2020|FALSE       |1 state AGI     | 4031|
| 2020|FALSE       |3 deductions    |  982|
| 2020|FALSE       |6 other credits |  180|
| 2020|FALSE       |7 rate/rounding |   57|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                      |
|GA    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the HB 1302 one-time surplus tax rebate ($250 single/MFS, $375 HoH, $500 joint, paid 2022 on TY2021 returns) into TAX YEAR 2021 as a NONREFUNDABLE credit (ga_surplus_tax_rebate in the 2021-only non_refundable list, so it is liability-capped via max(0, tax - credits)). Combined with our HB 593 std-deduction vintage fix (anchor moved 2021 -> 2022), the residual clean-mismatch masses (204/329/437 = rebate minus the 46/63 std wedge) are fully attributed. Excluded via predicate on the exported rebate |

