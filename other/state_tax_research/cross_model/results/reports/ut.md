# Cross-model validation: UT

Class: broad | Generated: 2026-07-19 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.6116|    0.6988|         0.7451|          0.7890|          0.2037|          0.0709|    2016.001|
| 2018|taxsim       | 20515|   13144|   0.6782|    0.7487|         0.8480|          0.8658|          0.2121|          0.0036|    2516.337|
| 2019|taxsim       | 20514|   13088|   0.6742|    0.7437|         0.8474|          0.8656|          0.2105|          0.0040|    2253.879|
| 2020|taxsim       | 20513|   12682|   0.6579|    0.7301|         0.8503|          0.8686|          0.2050|          0.0052|    1929.175|
| 2021|policyengine |  1536|     269|   0.4876|    0.5775|         0.9665|          0.9814|          0.1823|         20.0141|   -6887.050|
| 2022|policyengine |  1530|     317|   0.5157|    0.5771|         0.9495|          0.9621|          0.2000|          7.1574|    2641.428|
| 2023|policyengine |  1533|     357|   0.5140|    0.5760|         0.9496|          0.9580|          0.2003|          4.6510|   14230.091|
| 2024|policyengine |  1531|     364|   0.5062|    0.5604|         0.9505|          0.9615|          0.2005|         10.3814|   -1247.098|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  130|
| 2017|TRUE        |2 exemptions    | 2792|
| 2017|TRUE        |6 other credits |  415|
| 2017|FALSE       |1 state AGI     | 4489|
| 2017|FALSE       |2 exemptions    |  109|
| 2017|FALSE       |6 other credits |   32|
| 2018|TRUE        |1 state AGI     |   68|
| 2018|TRUE        |2 exemptions    |  644|
| 2018|TRUE        |6 other credits | 1286|
| 2018|FALSE       |1 state AGI     | 4490|
| 2018|FALSE       |2 exemptions    |   70|
| 2018|FALSE       |6 other credits |   43|
| 2019|TRUE        |1 state AGI     |   73|
| 2019|TRUE        |2 exemptions    |  653|
| 2019|TRUE        |6 other credits | 1271|
| 2019|FALSE       |1 state AGI     | 4570|
| 2019|FALSE       |2 exemptions    |   68|
| 2019|FALSE       |6 other credits |   48|
| 2020|TRUE        |1 state AGI     |   58|
| 2020|TRUE        |2 exemptions    |  609|
| 2020|TRUE        |6 other credits | 1231|
| 2020|FALSE       |1 state AGI     | 5018|
| 2020|FALSE       |2 exemptions    |   60|
| 2020|FALSE       |6 other credits |   41|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

