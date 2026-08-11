# Cross-model validation: NY

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3284|    0.4172|         0.4718|          0.5323|          0.0921|        253.3141|   14155.444|
| 2018|taxsim       | 20515|   13144|   0.4085|    0.4949|         0.5853|          0.6302|          0.0887|        108.9600|   11991.747|
| 2019|taxsim       | 20514|   13088|   0.3988|    0.4862|         0.5790|          0.6257|          0.0880|        122.9228|   12181.928|
| 2020|taxsim       | 20513|   12682|   0.4059|    0.4946|         0.5841|          0.6307|          0.0857|        108.9347|   12014.006|
| 2021|policyengine |  1536|     269|   0.1921|    0.2936|         0.6022|          0.6989|          0.0625|        384.0877|  -25070.139|
| 2022|policyengine |  1530|     317|   0.2817|    0.3588|         0.7729|          0.8328|          0.0712|        295.3244|   -9003.444|
| 2023|policyengine |  1533|     357|   0.0698|    0.1494|         0.1120|          0.1597|          0.0000|        464.6138|   16129.452|
| 2024|policyengine |  1531|     364|   0.2652|    0.3436|         0.7390|          0.7967|          0.0581|        308.3182|  -17805.237|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  682|
| 2017|TRUE        |3 deductions     | 5323|
| 2017|TRUE        |4 taxable income |    5|
| 2017|TRUE        |5 state EITC     |   71|
| 2017|TRUE        |6 other credits  |  818|
| 2017|TRUE        |7 rate/rounding  |   16|
| 2017|FALSE       |1 state AGI      | 5491|
| 2017|FALSE       |3 deductions     |  333|
| 2017|FALSE       |5 state EITC     | 1026|
| 2017|FALSE       |6 other credits  |   11|
| 2018|TRUE        |1 state AGI      |  656|
| 2018|TRUE        |3 deductions     | 3899|
| 2018|TRUE        |5 state EITC     |   58|
| 2018|TRUE        |6 other credits  |  826|
| 2018|TRUE        |7 rate/rounding  |   12|
| 2018|FALSE       |1 state AGI      | 5406|
| 2018|FALSE       |3 deductions     |  264|
| 2018|FALSE       |5 state EITC     | 1004|
| 2018|FALSE       |6 other credits  |    9|
| 2018|FALSE       |7 rate/rounding  |    1|
| 2019|TRUE        |1 state AGI      |  630|
| 2019|TRUE        |3 deductions     | 3960|
| 2019|TRUE        |5 state EITC     |   63|
| 2019|TRUE        |6 other credits  |  839|
| 2019|TRUE        |7 rate/rounding  |   18|
| 2019|FALSE       |1 state AGI      | 5460|
| 2019|FALSE       |3 deductions     |  299|
| 2019|FALSE       |5 state EITC     | 1055|
| 2019|FALSE       |6 other credits  |    7|
| 2019|FALSE       |7 rate/rounding  |    1|
| 2020|TRUE        |1 state AGI      |  659|
| 2020|TRUE        |3 deductions     | 3762|
| 2020|TRUE        |5 state EITC     |   44|
| 2020|TRUE        |6 other credits  |  797|
| 2020|TRUE        |7 rate/rounding  |   12|
| 2020|FALSE       |1 state AGI      | 5517|
| 2020|FALSE       |3 deductions     |  351|
| 2020|FALSE       |5 state EITC     |  985|
| 2020|FALSE       |6 other credits  |   50|
| 2020|FALSE       |7 rate/rounding  |    9|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

