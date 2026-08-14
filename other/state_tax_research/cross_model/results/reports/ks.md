# Cross-model validation: KS

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3817|    0.5116|         0.5313|          0.6280|          0.1020|         87.1601|   -141.1734|
| 2018|taxsim       | 20515|   13144|   0.4608|    0.5775|         0.6432|          0.7110|          0.0946|         36.1448|   -154.5080|
| 2019|taxsim       | 20514|   13088|   0.4646|    0.5679|         0.6569|          0.7036|          0.0943|         35.2127|   4725.0435|
| 2020|taxsim       | 20513|   12682|   0.4735|    0.5600|         0.6833|          0.7090|          0.0950|         32.7726|  10100.2253|
| 2021|policyengine |  1536|     270|   0.2090|    0.3900|         0.7148|          0.9000|          0.0768|        192.1753|  -6432.5491|
| 2022|policyengine |  1530|     317|   0.2980|    0.3993|         0.6656|          0.8391|          0.0876|        231.3429|   2747.6937|
| 2023|policyengine |  1533|     357|   0.2988|    0.3999|         0.6863|          0.8487|          0.0848|        213.2274|  17936.7053|
| 2024|policyengine |  1531|     363|   0.2933|    0.3873|         0.6970|          0.8512|          0.0914|        248.5550|   -429.4843|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  238|
| 2017|TRUE        |2 exemptions     |  305|
| 2017|TRUE        |3 deductions     | 5124|
| 2017|TRUE        |4 taxable income |   12|
| 2017|TRUE        |6 other credits  |  457|
| 2017|FALSE       |1 state AGI      | 5114|
| 2017|FALSE       |2 exemptions     |  108|
| 2017|FALSE       |3 deductions     |  242|
| 2017|FALSE       |4 taxable income |    1|
| 2017|FALSE       |5 state EITC     | 1078|
| 2017|FALSE       |6 other credits  |    4|
| 2018|TRUE        |1 state AGI      |  181|
| 2018|TRUE        |2 exemptions     |  330|
| 2018|TRUE        |3 deductions     | 3397|
| 2018|TRUE        |4 taxable income |    2|
| 2018|TRUE        |6 other credits  |  780|
| 2018|FALSE       |1 state AGI      | 5025|
| 2018|FALSE       |2 exemptions     |  103|
| 2018|FALSE       |3 deductions     |  178|
| 2018|FALSE       |5 state EITC     | 1059|
| 2018|FALSE       |6 other credits  |    7|
| 2019|TRUE        |1 state AGI      |  182|
| 2019|TRUE        |2 exemptions     |  325|
| 2019|TRUE        |3 deductions     | 3347|
| 2019|TRUE        |4 taxable income |    2|
| 2019|TRUE        |6 other credits  |  635|
| 2019|FALSE       |1 state AGI      | 5074|
| 2019|FALSE       |2 exemptions     |  111|
| 2019|FALSE       |3 deductions     |  202|
| 2019|FALSE       |5 state EITC     | 1100|
| 2019|FALSE       |6 other credits  |    6|
| 2020|TRUE        |1 state AGI      |  140|
| 2020|TRUE        |2 exemptions     |  305|
| 2020|TRUE        |3 deductions     | 2969|
| 2020|TRUE        |4 taxable income |    8|
| 2020|TRUE        |6 other credits  |  594|
| 2020|FALSE       |1 state AGI      | 5494|
| 2020|FALSE       |2 exemptions     |  107|
| 2020|FALSE       |3 deductions     |  156|
| 2020|FALSE       |5 state EITC     | 1017|
| 2020|FALSE       |6 other credits  |   10|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

