# Cross-model validation: WV

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.7048|    0.7931|         0.9243|          0.9364|          0.1408|          0.0037|    517.9055|
| 2018|taxsim       | 20515|   13144|   0.7034|    0.7910|         0.9261|          0.9355|          0.1355|          0.0037|    515.6439|
| 2019|taxsim       | 20514|   13088|   0.6983|    0.7832|         0.9247|          0.9358|          0.1377|          0.0037|    665.4439|
| 2020|taxsim       | 20513|   12682|   0.6576|    0.7454|         0.8974|          0.9127|          0.1330|          0.0040|    675.0963|
| 2021|policyengine |  1536|     269|   0.4329|    0.5150|         0.7323|          0.7584|          0.1400|         60.3340|  -7613.9790|
| 2022|policyengine |  1530|     316|   0.4719|    0.5137|         0.7468|          0.7753|          0.1392|         59.5316|   -725.3813|
| 2023|policyengine |  1533|     357|   0.4821|    0.5264|         0.7115|          0.7479|          0.1363|         44.7451|  11768.8617|
| 2024|policyengine |  1531|     364|   0.4572|    0.5010|         0.7253|          0.7582|          0.1287|         96.6939|  -5057.1288|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  447|
| 2017|TRUE        |4 taxable income |    4|
| 2017|TRUE        |6 other credits  |  540|
| 2017|FALSE       |1 state AGI      | 4912|
| 2017|FALSE       |4 taxable income |    3|
| 2017|FALSE       |6 other credits  |  149|
| 2018|TRUE        |1 state AGI      |  448|
| 2018|TRUE        |6 other credits  |  523|
| 2018|FALSE       |1 state AGI      | 4910|
| 2018|FALSE       |4 taxable income |    3|
| 2018|FALSE       |6 other credits  |  200|
| 2019|TRUE        |1 state AGI      |  474|
| 2019|TRUE        |4 taxable income |    4|
| 2019|TRUE        |6 other credits  |  507|
| 2019|FALSE       |1 state AGI      | 5014|
| 2019|FALSE       |4 taxable income |    3|
| 2019|FALSE       |6 other credits  |  188|
| 2020|TRUE        |1 state AGI      |  984|
| 2020|TRUE        |6 other credits  |  317|
| 2020|FALSE       |1 state AGI      | 5535|
| 2020|FALSE       |4 taxable income |    5|
| 2020|FALSE       |6 other credits  |  183|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

