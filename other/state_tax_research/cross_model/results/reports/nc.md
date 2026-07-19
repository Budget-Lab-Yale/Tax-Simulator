# Cross-model validation: NC

Class: broad | Generated: 2026-07-19 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4067|    0.4944|         0.4991|          0.5940|          0.1474|        100.0035|  -11380.797|
| 2018|taxsim       | 20515|   13144|   0.1794|    0.4319|         0.1945|          0.5093|          0.1543|        137.4715|  -10639.030|
| 2019|taxsim       | 20514|   13088|   0.4465|    0.5490|         0.5523|          0.6602|          0.1611|         40.6798|  -10924.120|
| 2020|taxsim       | 20513|   12682|   0.4430|    0.5486|         0.5479|          0.6582|          0.1618|         44.9170|  -10754.668|
| 2021|policyengine |  1536|     269|   0.2832|    0.3626|         0.8178|          0.8513|          0.1387|        282.1922|   -8185.293|
| 2022|policyengine |  1530|     317|   0.3314|    0.3804|         0.8454|          0.8738|          0.1608|        318.1106|    1151.512|
| 2023|policyengine |  1533|     357|   0.3066|    0.3620|         0.8067|          0.8207|          0.1455|        302.9312|   13980.124|
| 2024|policyengine |  1531|     364|   0.3103|    0.3586|         0.8297|          0.8407|          0.1365|        309.3737|   -2767.963|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3893|
| 2017|TRUE        |3 deductions     | 1610|
| 2017|TRUE        |6 other credits  | 1055|
| 2017|FALSE       |1 state AGI      | 5197|
| 2017|FALSE       |3 deductions     |  150|
| 2017|FALSE       |6 other credits  |  265|
| 2018|TRUE        |1 state AGI      | 4152|
| 2018|TRUE        |3 deductions     | 6436|
| 2018|FALSE       |1 state AGI      | 5418|
| 2018|FALSE       |3 deductions     |  829|
| 2019|TRUE        |1 state AGI      | 3860|
| 2019|TRUE        |3 deductions     | 1526|
| 2019|TRUE        |4 taxable income |  474|
| 2019|FALSE       |1 state AGI      | 5238|
| 2019|FALSE       |3 deductions     |  133|
| 2019|FALSE       |4 taxable income |  124|
| 2020|TRUE        |1 state AGI      | 3869|
| 2020|TRUE        |3 deductions     | 1423|
| 2020|TRUE        |4 taxable income |  441|
| 2020|FALSE       |1 state AGI      | 5377|
| 2020|FALSE       |3 deductions     |  151|
| 2020|FALSE       |4 taxable income |  164|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

