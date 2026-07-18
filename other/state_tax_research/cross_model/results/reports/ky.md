# Cross-model validation: KY

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.2179|    0.2686|         0.2193|          0.2693|          0.2098|        246.4473|  -4574.2097|
| 2018|taxsim       | 20515|   13504|   0.3439|    0.4138|         0.4043|          0.4655|          0.1912|        126.5032|    667.6719|
| 2019|taxsim       | 20514|   13433|   0.3378|    0.4035|         0.4004|          0.4551|          0.1913|        126.5041|    452.9684|
| 2020|taxsim       | 20513|   13070|   0.3330|    0.3909|         0.3953|          0.4461|          0.1831|        130.6124|    458.9758|
| 2021|policyengine |  1536|     269|   0.3379|    0.4342|         0.7472|          0.8587|          0.1641|        192.1421|  -6698.3927|
| 2022|policyengine |  1530|     317|   0.3353|    0.4268|         0.7192|          0.8013|          0.1588|        201.4888|   4030.5846|
| 2023|policyengine |  1533|     357|   0.3112|    0.3973|         0.6555|          0.7255|          0.1442|        230.0338|  14596.6386|
| 2024|policyengine |  1531|     364|   0.3076|    0.3991|         0.6566|          0.7445|          0.1378|        228.3276|    173.0358|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3306|
| 2017|TRUE        |3 deductions    | 7200|
| 2017|FALSE       |1 state AGI     | 4936|
| 2017|FALSE       |3 deductions    |  602|
| 2018|TRUE        |1 state AGI     | 3469|
| 2018|TRUE        |3 deductions    | 4052|
| 2018|TRUE        |6 other credits |  523|
| 2018|FALSE       |1 state AGI     | 4938|
| 2018|FALSE       |3 deductions    |  238|
| 2018|FALSE       |6 other credits |  239|
| 2019|TRUE        |1 state AGI     | 3331|
| 2019|TRUE        |3 deductions    | 4723|
| 2019|FALSE       |1 state AGI     | 4982|
| 2019|FALSE       |3 deductions    |  549|
| 2020|TRUE        |1 state AGI     | 3408|
| 2020|TRUE        |3 deductions    | 4495|
| 2020|FALSE       |1 state AGI     | 5075|
| 2020|FALSE       |3 deductions    |  704|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

