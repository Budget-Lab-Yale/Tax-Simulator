# Cross-model validation: UT

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 15481|    9863|   0.6546|    0.7490|         0.7838|          0.8254|          0.1633|          0.0624|    1890.573|
| 2017|taxsim       |  5032|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 15416|    9863|   0.7357|    0.8133|         0.9108|          0.9267|          0.1737|          0.0034|    2452.530|
| 2018|taxsim       |  5099|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 15408|    9824|   0.7324|    0.8080|         0.9089|          0.9247|          0.1732|          0.0037|    1895.389|
| 2019|taxsim       |  5106|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 15391|    9490|   0.7132|    0.7923|         0.9148|          0.9303|          0.1652|          0.0047|    1754.198|
| 2020|taxsim       |  5122|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
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

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:-----|:------|--------:|--------:|:------------------|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                          |
|UT    |taxsim |     2017|     2020|external-model-bug |exclude  |TAXSIM grants the UT retirement credit to ANY record with Social Security income, ignoring both the born-before-1953 cohort gate and the 2.5c/$ MAGI phase-out: probe-verified flat $288/person (= 6% x $4,800; $576/couple; $271 under the 2017 vintage constant) paid to a 40-year-old with $2M wages and $1 of SS, at any income. Our encoding applies the cohort gate and phase-out per TC-40 instructions. Excluded via predicate on SS receipt (the exposure set)                                                                                                                                                           |
|UT    |taxsim |     2017|     2020|structural         |exclude  |TAXSIM derives head-of-household treatment from dependents presence and ignores mstat (probe: single+2deps and HoH+2deps return identical siitax and credits to the cent; HoH+0deps computes as single). PUF returns filed single-with-dependents (and HoH returns whose dependents do not map into the crosswalk dep slots) therefore get the wrong federal standard deduction and phase-out threshold inside the UT taxpayer-credit base: symmetric +/-$464 (2019) masses = 6% x (18,350 - 12,200) + 1.3% x threshold gap. Input-representation limit, not fixable in the crosswalk. Excluded via predicate on the exposure set |
|UT    |taxsim |     2017|     2020|structural         |annotate |Stage-table caveat: our UT representation carries the exemption piece of the taxpayer tax credit (75% of federal exemptions in 2017; $579/dependent 2018+) inside the credit base, so st_exempt = 0 by design and v33-vs-st_exempt comparisons misattribute UT wedges to the exemptions stage (zero liability effect wherever the credit is phased out). Read UT stage tables credits-first                                                                                                                                                                                                                                       |

