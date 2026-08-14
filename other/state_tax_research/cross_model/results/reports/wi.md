# Cross-model validation: WI

Class: broad | Generated: 2026-08-13 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 13602|   10369|   0.8058|    0.8904|         0.9051|          0.9522|          0.2079|          0.5299|   -180.9861|
| 2017|taxsim       |  6911|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 13566|   10379|   0.5922|    0.8932|         0.6312|          0.9537|          0.2060|          0.4170|   -304.8435|
| 2018|taxsim       |  6949|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 20514|   13088|   0.7031|    0.7957|         0.9395|          0.9678|          0.1411|          0.2145|    742.9696|
| 2020|taxsim       | 20513|   12682|   0.7049|    0.8007|         0.9423|          0.9722|          0.1367|          0.3783|    729.5654|
| 2021|policyengine |  1525|     269|   0.2846|    0.4085|         0.7472|          0.7881|          0.1023|        258.5527| -13764.1045|
| 2021|policyengine |    11|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1517|     313|   0.3481|    0.4080|         0.7604|          0.7700|          0.1101|        252.5180|  -3866.1109|
| 2022|policyengine |    13|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1527|     354|   0.3360|    0.3995|         0.7316|          0.7627|          0.1061|        286.6229|  12945.1831|
| 2023|policyengine |     6|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1524|     364|   0.3136|    0.3668|         0.6951|          0.7198|          0.1043|        356.4429| -12615.3257|
| 2024|policyengine |     7|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  638|
| 2017|TRUE        |3 deductions     | 2278|
| 2017|TRUE        |4 taxable income |    3|
| 2017|TRUE        |7 rate/rounding  |  374|
| 2017|FALSE       |1 state AGI      | 5049|
| 2017|FALSE       |3 deductions     |  670|
| 2017|FALSE       |7 rate/rounding  |   13|
| 2018|TRUE        |1 state AGI      |  884|
| 2018|TRUE        |3 deductions     | 2370|
| 2018|TRUE        |4 taxable income |   51|
| 2018|TRUE        |5 state EITC     |   37|
| 2018|TRUE        |6 other credits  |   18|
| 2018|TRUE        |7 rate/rounding  | 2795|
| 2018|FALSE       |1 state AGI      | 5090|
| 2018|FALSE       |3 deductions     |  202|
| 2018|FALSE       |4 taxable income |   11|
| 2018|FALSE       |5 state EITC     |  415|
| 2018|FALSE       |6 other credits  |    6|
| 2018|FALSE       |7 rate/rounding  |   42|
| 2019|TRUE        |1 state AGI      |  358|
| 2019|TRUE        |3 deductions     |  365|
| 2019|TRUE        |4 taxable income |   29|
| 2019|TRUE        |5 state EITC     |   33|
| 2019|TRUE        |6 other credits  |    7|
| 2019|FALSE       |1 state AGI      | 4745|
| 2019|FALSE       |3 deductions     |  130|
| 2019|FALSE       |4 taxable income |   13|
| 2019|FALSE       |5 state EITC     |  407|
| 2019|FALSE       |6 other credits  |    3|
| 2020|TRUE        |1 state AGI      |  298|
| 2020|TRUE        |3 deductions     |  358|
| 2020|TRUE        |4 taxable income |   35|
| 2020|TRUE        |5 state EITC     |   31|
| 2020|TRUE        |6 other credits  |   10|
| 2020|FALSE       |1 state AGI      | 4765|
| 2020|FALSE       |3 deductions     |  140|
| 2020|FALSE       |4 taxable income |   11|
| 2020|FALSE       |5 state EITC     |  403|
| 2020|FALSE       |6 other credits  |    3|

## Known differences applied

|state |model        | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:-----|:------------|--------:|--------:|:------------------|:--------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim       |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                            |
|ALL   |taxsim       |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                            |
|WI    |taxsim       |     2017|     2018|external-model-bug |exclude  |TAXSIM's WI 2017 and 2018 bracket thresholds are stale (~3% low; empirical top-bracket entry ~$320,250 MFJ vs the published $329,810; the 2018 schedule returns byte-identical tax to 2017 despite different published thresholds), overtaxing by a flat ~$12.8 in the 6.27% bracket and ~$143.6 in the 7.65% bracket (the -$144 mass, ~1,190/yr; combined with the capital-loss addback it also produces the -$169/-$335 masses). Our schedule matches the published DOR tables to the cent. 2019-2020 vintages are correct (clean 0.92). Excluded via top-bracket membership where the error exceeds the $100 bar |
|WI    |policyengine |     2021|     2024|transfer-netting   |exclude  |PE nets the WI homestead credit (rent/property-tax-based Schedule H, in wi_refundable_credits) into wi_income_tax; rent is unobserved in the PUF so the credit is one-sided and household-specific (diffuse mismatches, no point mass). Excluded via predicate on the exported credit                                                                                                                                                                                                                                                                                                                               |
|WI    |policyengine |     2021|     2024|structural         |annotate |The diffuse WI PE residual concentrates in itemized-deduction-credit records: our st_item_credit averages $1,852 on clean mismatches vs $51 on matches (2022; two-sided, up to +/-$18k tails). Component proxies inside the 5% credit (medical floor vintages 2017/2019/2020, misc/casualty components) and PE's own itemized-credit modeling differ record-by-record; no point masses. Homestead (separate exclude row) explains only the small pe_wi_homestead > 0 subset — PE cannot compute renter homestead without rent data either                                                                           |

