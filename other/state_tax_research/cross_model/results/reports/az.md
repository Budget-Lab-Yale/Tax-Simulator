# Cross-model validation: AZ

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.1574|    0.4560|         0.2128|          0.5358|          0.0058|        141.2050|   5422.9623|
| 2018|taxsim       | 20515|   13504|   0.2162|    0.5802|         0.2855|          0.6709|          0.0037|         77.5776|    875.7579|
| 2019|taxsim       | 20514|   13433|   0.2950|    0.6845|         0.3881|          0.7951|          0.0092|         50.0000|    613.2254|
| 2020|taxsim       | 20513|   13070|   0.2827|    0.6625|         0.3812|          0.7878|          0.0093|         50.0000|    612.5132|
| 2021|policyengine |  1536|     270|   0.0788|    0.2871|         0.2259|          0.6926|          0.0013|        597.7499| 816225.5279|
| 2022|policyengine |  1530|     316|   0.1562|    0.3987|         0.4304|          0.8829|          0.0124|        179.4587|  65045.2691|
| 2023|policyengine |  1533|     357|   0.1579|    0.4142|         0.4566|          0.8683|          0.0196|        159.7273|  46313.0357|
| 2024|policyengine |  1531|     363|   0.1737|    0.4115|         0.4738|          0.8788|          0.0157|        181.7981|  47377.5512|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3014|
| 2017|TRUE        |2 exemptions    | 5372|
| 2017|TRUE        |3 deductions    | 1803|
| 2017|TRUE        |6 other credits |  405|
| 2017|FALSE       |1 state AGI     | 5404|
| 2017|FALSE       |2 exemptions    |  632|
| 2017|FALSE       |3 deductions    |  631|
| 2017|FALSE       |6 other credits |   23|
| 2018|TRUE        |1 state AGI     | 2838|
| 2018|TRUE        |2 exemptions    | 5220|
| 2018|TRUE        |3 deductions    | 1181|
| 2018|TRUE        |6 other credits |  410|
| 2018|FALSE       |1 state AGI     | 5226|
| 2018|FALSE       |2 exemptions    |  611|
| 2018|FALSE       |3 deductions    |  581|
| 2018|FALSE       |6 other credits |   13|
| 2019|TRUE        |1 state AGI     | 2441|
| 2019|TRUE        |2 exemptions    | 2495|
| 2019|TRUE        |3 deductions    | 2747|
| 2019|TRUE        |6 other credits |  536|
| 2019|FALSE       |1 state AGI     | 5100|
| 2019|FALSE       |2 exemptions    |   98|
| 2019|FALSE       |3 deductions    | 1018|
| 2019|FALSE       |6 other credits |   27|
| 2020|TRUE        |1 state AGI     | 2497|
| 2020|TRUE        |2 exemptions    | 2418|
| 2020|TRUE        |3 deductions    | 2651|
| 2020|TRUE        |6 other credits |  522|
| 2020|FALSE       |1 state AGI     | 5602|
| 2020|FALSE       |2 exemptions    |   88|
| 2020|FALSE       |3 deductions    |  909|
| 2020|FALSE       |6 other credits |   26|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

