# Cross-model validation: MD

Class: broad | Generated: 2026-08-12 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3532|    0.4667|         0.4923|          0.5946|          0.1220|        137.7005|    5671.677|
| 2018|taxsim       | 20515|   13144|   0.4282|    0.5620|         0.5994|          0.7187|          0.1141|         56.9985|   -4330.578|
| 2019|taxsim       | 20514|   13088|   0.1906|    0.5498|         0.2419|          0.7081|          0.1123|         77.2282|   -4560.417|
| 2020|taxsim       | 20513|   12682|   0.3967|    0.5157|         0.5714|          0.6918|          0.1084|         78.2477|   -5208.851|
| 2021|policyengine |  1536|     268|   0.2376|    0.3730|         0.7873|          0.8545|          0.0736|        260.5599|  -10233.123|
| 2022|policyengine |  1530|     314|   0.3131|    0.4105|         0.8376|          0.9013|          0.1078|        199.3618|    1300.366|
| 2023|policyengine |  1533|     357|   0.3079|    0.4194|         0.8487|          0.9048|          0.0946|        178.7896|   15442.184|
| 2024|policyengine |  1531|     363|   0.2992|    0.4010|         0.8485|          0.9036|          0.0836|        216.8094|   -3107.881|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2670|
| 2017|TRUE        |2 exemptions     |  682|
| 2017|TRUE        |3 deductions     | 3250|
| 2017|TRUE        |6 other credits  |   45|
| 2017|FALSE       |1 state AGI      | 5352|
| 2017|FALSE       |2 exemptions     |   23|
| 2017|FALSE       |3 deductions     |  211|
| 2017|FALSE       |5 state EITC     | 1021|
| 2017|FALSE       |6 other credits  |   14|
| 2018|TRUE        |1 state AGI      | 2572|
| 2018|TRUE        |2 exemptions     |  712|
| 2018|TRUE        |3 deductions     | 1942|
| 2018|TRUE        |4 taxable income |    1|
| 2018|TRUE        |6 other credits  |   38|
| 2018|FALSE       |1 state AGI      | 5218|
| 2018|FALSE       |2 exemptions     |   33|
| 2018|FALSE       |3 deductions     |  180|
| 2018|FALSE       |4 taxable income |    1|
| 2018|FALSE       |5 state EITC     | 1026|
| 2018|FALSE       |6 other credits  |    8|
| 2019|TRUE        |1 state AGI      | 2591|
| 2019|TRUE        |2 exemptions     |  721|
| 2019|TRUE        |3 deductions     | 6589|
| 2019|TRUE        |6 other credits  |   21|
| 2019|FALSE       |1 state AGI      | 5399|
| 2019|FALSE       |2 exemptions     |   36|
| 2019|FALSE       |3 deductions     |  692|
| 2019|FALSE       |5 state EITC     |  548|
| 2019|FALSE       |6 other credits  |    8|
| 2020|TRUE        |1 state AGI      | 2686|
| 2020|TRUE        |2 exemptions     |  726|
| 2020|TRUE        |3 deductions     | 1843|
| 2020|TRUE        |5 state EITC     |  143|
| 2020|TRUE        |6 other credits  |   38|
| 2020|FALSE       |1 state AGI      | 5687|
| 2020|FALSE       |2 exemptions     |   29|
| 2020|FALSE       |3 deductions     |  319|
| 2020|FALSE       |5 state EITC     |  904|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:-----|:------|--------:|--------:|:------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                             |
|MD    |taxsim |     2019|     2019|external-model-bug |annotate |TAXSIM applies the MD standard-deduction MINIMUM ($1,550/$3,100) to every 2019 non-itemizer: probe-verified v34 = 1,550 at $100k wages where 15% x AGI caps at the $2,250/$4,550 maximum. Produces flat -$33 (single, 700 x 4.75%) / -$69 and -$83 (joint, 1,450 x rate) masses on ~3,900 records. 2018 and 2020 probe correct-to-one-index-step (2020 uses the 2019 maxima). ANNOTATE, not exclude: the per-record effect ($33-$83) never breaches the $100 bar, so it binds match@15 only (2019 match@15 24% vs match@100 69.5%); an earlier exclude on the bug signature removed match@100 PASSES and depressed the 2019 cell to 0.488 (reverted) |
|MD    |taxsim |     2017|     2024|data-proxy         |annotate |Two-income married couple subtraction attribution: TAXSIM attributes joint unearned income across spouses when computing the lesser-earning spouse's income, granting the $1,200 subtraction to one-earner couples with unearned income; our proxy attributes earned income only (per-spouse ownership of unearned income is unobserved in the PUF). +$57 mass (~190/yr, 78% wages2 == 0, 76% joint unearned > $2,400). Both are proxies for Worksheet 13D; neither is form-true                                                                                                                                                                     |

