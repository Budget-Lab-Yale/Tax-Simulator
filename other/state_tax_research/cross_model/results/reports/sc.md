# Cross-model validation: SC

Class: broad | Generated: 2026-07-19 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3751|    0.4083|         0.4409|          0.4758|          0.2277|        353.0435|  20181.1232|
| 2018|taxsim       | 20515|   13144|   0.4218|    0.4714|         0.5061|          0.5541|          0.2458|        159.5613|   -676.4270|
| 2019|taxsim       | 20514|   13088|   0.4236|    0.4709|         0.5097|          0.5551|          0.2488|        170.9102|  -1149.3005|
| 2020|taxsim       | 20513|   12682|   0.4224|    0.4692|         0.5114|          0.5561|          0.2434|        173.2870|   -804.0232|
| 2021|policyengine |  1536|     269|   0.2357|    0.2546|         0.5167|          0.5242|          0.2350|        821.1046| -20892.0763|
| 2022|policyengine |  1530|     316|   0.3144|    0.3693|         0.7785|          0.8165|          0.2222|        338.2987| -10555.9794|
| 2023|policyengine |  1533|     358|   0.3079|    0.3686|         0.7318|          0.7737|          0.2179|        347.0194|    247.2633|
| 2024|policyengine |  1531|     364|   0.3076|    0.3632|         0.7390|          0.7582|          0.2123|        365.4443| -16383.2440|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6728|
| 2017|TRUE        |4 taxable income |   18|
| 2017|TRUE        |6 other credits  |  574|
| 2017|FALSE       |1 state AGI      | 5427|
| 2017|FALSE       |4 taxable income |   20|
| 2017|FALSE       |6 other credits  |   51|
| 2018|TRUE        |1 state AGI      | 5267|
| 2018|TRUE        |2 exemptions     |  448|
| 2018|TRUE        |4 taxable income |   48|
| 2018|TRUE        |6 other credits  |  729|
| 2018|FALSE       |1 state AGI      | 5272|
| 2018|FALSE       |2 exemptions     |   21|
| 2018|FALSE       |4 taxable income |    8|
| 2018|FALSE       |5 state EITC     |   66|
| 2018|FALSE       |6 other credits  |    3|
| 2019|TRUE        |1 state AGI      | 5369|
| 2019|TRUE        |2 exemptions     |  326|
| 2019|TRUE        |4 taxable income |   53|
| 2019|TRUE        |6 other credits  |  669|
| 2019|FALSE       |1 state AGI      | 5357|
| 2019|FALSE       |2 exemptions     |   12|
| 2019|FALSE       |4 taxable income |    8|
| 2019|FALSE       |5 state EITC     |   28|
| 2019|FALSE       |6 other credits  |    2|
| 2020|TRUE        |1 state AGI      | 5201|
| 2020|TRUE        |2 exemptions     |  300|
| 2020|TRUE        |4 taxable income |   43|
| 2020|TRUE        |6 other credits  |  653|
| 2020|FALSE       |1 state AGI      | 5536|
| 2020|FALSE       |2 exemptions     |   33|
| 2020|FALSE       |4 taxable income |    6|
| 2020|FALSE       |5 state EITC     |   27|
| 2020|FALSE       |6 other credits  |   49|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

