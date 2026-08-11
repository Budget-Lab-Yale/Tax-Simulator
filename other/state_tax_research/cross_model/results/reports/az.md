# Cross-model validation: AZ

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.1572|    0.4561|         0.2186|          0.5488|          0.0058|        140.7483|   5403.9709|
| 2018|taxsim       | 20515|   13144|   0.2144|    0.5809|         0.2900|          0.6802|          0.0038|         77.0203|    853.4244|
| 2019|taxsim       | 20514|   13088|   0.2909|    0.6785|         0.3916|          0.8009|          0.0092|         50.0000|    595.5075|
| 2020|taxsim       | 20513|   12682|   0.2779|    0.6559|         0.3850|          0.7955|          0.0093|         50.0000|    592.9391|
| 2021|policyengine |  1004|     211|   0.1205|    0.4313|         0.2891|          0.8815|          0.0020|        200.0219| 744808.0825|
| 2021|policyengine |   532|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     316|   0.1562|    0.3987|         0.4304|          0.8829|          0.0124|        179.4587|  65045.2691|
| 2023|policyengine |  1533|     357|   0.1579|    0.4142|         0.4566|          0.8683|          0.0196|        159.7273|  46313.0357|
| 2024|policyengine |  1531|     363|   0.1737|    0.4115|         0.4738|          0.8788|          0.0157|        181.7981|  47377.5512|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2890|
| 2017|TRUE        |2 exemptions    | 5188|
| 2017|TRUE        |3 deductions    | 1748|
| 2017|TRUE        |6 other credits |  404|
| 2017|FALSE       |1 state AGI     | 5776|
| 2017|FALSE       |2 exemptions    |  630|
| 2017|FALSE       |3 deductions    |  629|
| 2017|FALSE       |6 other credits |   23|
| 2018|TRUE        |1 state AGI     | 2698|
| 2018|TRUE        |2 exemptions    | 5067|
| 2018|TRUE        |3 deductions    | 1158|
| 2018|TRUE        |6 other credits |  409|
| 2018|FALSE       |1 state AGI     | 5591|
| 2018|FALSE       |2 exemptions    |  601|
| 2018|FALSE       |3 deductions    |  580|
| 2018|FALSE       |6 other credits |   13|
| 2019|TRUE        |1 state AGI     | 2341|
| 2019|TRUE        |2 exemptions    | 2430|
| 2019|TRUE        |3 deductions    | 2662|
| 2019|TRUE        |6 other credits |  530|
| 2019|FALSE       |1 state AGI     | 5450|
| 2019|FALSE       |2 exemptions    |   93|
| 2019|FALSE       |3 deductions    | 1014|
| 2019|FALSE       |6 other credits |   27|
| 2020|TRUE        |1 state AGI     | 2373|
| 2020|TRUE        |2 exemptions    | 2355|
| 2020|TRUE        |3 deductions    | 2552|
| 2020|TRUE        |6 other credits |  520|
| 2020|FALSE       |1 state AGI     | 5995|
| 2020|FALSE       |2 exemptions    |   87|
| 2020|FALSE       |3 deductions    |  907|
| 2020|FALSE       |6 other credits |   24|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:-----|:------------|--------:|--------:|:----------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                     |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                |
|AZ    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the one-time Arizona Families Tax Rebate (SB 1734 / Laws 2023 ch. 147: $250 per dependent under 17, $100 per 17+ dependent, max three, requiring >= $1 of 2021 liability; paid fall 2023 on TY2021 returns) into TAX YEAR 2021 as az_families_tax_rebate. Clean mismatches mass at +250/+500/+750. The separately-tracked 2021 mean_abs_diff ~ $819k is unrelated: ~20 giant-AGI records (up to $27M diffs), ALL fed_aligned == FALSE, already quarantined by the clean metrics. Excluded via predicate on the exported rebate |

