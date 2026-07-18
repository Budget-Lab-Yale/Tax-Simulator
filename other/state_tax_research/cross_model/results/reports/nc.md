# Cross-model validation: NC

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.4099|    0.4968|         0.4923|          0.5862|          0.1474|        100.0018|  -11357.120|
| 2018|taxsim       | 20515|   13504|   0.1799|    0.4337|         0.1914|          0.5004|          0.1543|        137.4710|  -10612.183|
| 2019|taxsim       | 20514|   13433|   0.4506|    0.5523|         0.5454|          0.6521|          0.1611|         37.9102|  -10903.561|
| 2020|taxsim       | 20513|   13070|   0.4483|    0.5521|         0.5395|          0.6479|          0.1618|         41.3574|  -10732.025|
| 2021|policyengine |  1536|     269|   0.2832|    0.3626|         0.8178|          0.8513|          0.1387|        282.1922|   -8185.293|
| 2022|policyengine |  1530|     317|   0.3314|    0.3804|         0.8454|          0.8738|          0.1608|        318.1106|    1151.512|
| 2023|policyengine |  1533|     357|   0.3066|    0.3620|         0.8067|          0.8207|          0.1455|        302.9312|   13980.124|
| 2024|policyengine |  1531|     364|   0.3103|    0.3586|         0.8297|          0.8407|          0.1365|        309.3737|   -2767.963|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 4041|
| 2017|TRUE        |3 deductions     | 1731|
| 2017|TRUE        |6 other credits  | 1061|
| 2017|FALSE       |1 state AGI      | 4856|
| 2017|FALSE       |3 deductions     |  154|
| 2017|FALSE       |6 other credits  |  261|
| 2018|TRUE        |1 state AGI      | 4314|
| 2018|TRUE        |3 deductions     | 6605|
| 2018|FALSE       |1 state AGI      | 5067|
| 2018|FALSE       |3 deductions     |  839|
| 2019|TRUE        |1 state AGI      | 3978|
| 2019|TRUE        |3 deductions     | 1653|
| 2019|TRUE        |4 taxable income |  476|
| 2019|FALSE       |1 state AGI      | 4901|
| 2019|FALSE       |3 deductions     |  139|
| 2019|FALSE       |4 taxable income |  123|
| 2020|TRUE        |1 state AGI      | 4033|
| 2020|TRUE        |3 deductions     | 1543|
| 2020|TRUE        |4 taxable income |  443|
| 2020|FALSE       |1 state AGI      | 4980|
| 2020|FALSE       |3 deductions     |  154|
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

