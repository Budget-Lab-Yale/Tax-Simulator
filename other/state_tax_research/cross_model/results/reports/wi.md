# Cross-model validation: WI

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  8984|    6848|   0.8564|    0.9182|         0.9400|          0.9728|          0.2956|          0.3904|   -214.6968|
| 2017|taxsim       | 11529|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  8990|    6887|   0.7264|    0.9196|         0.7717|          0.9721|          0.2953|          0.0546|   -185.8911|
| 2018|taxsim       | 11525|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9751|    7288|   0.8426|    0.9076|         0.9490|          0.9809|          0.2794|          0.1487|     44.1226|
| 2019|taxsim       | 10763|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 10690|    7474|   0.8217|    0.8962|         0.9536|          0.9860|          0.2457|          0.2950|     73.8507|
| 2020|taxsim       |  9823|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1525|     269|   0.2951|    0.4256|         0.7546|          0.8030|          0.1023|        231.6720| -13780.4703|
| 2021|policyengine |    11|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1517|     313|   0.3830|    0.4397|         0.7827|          0.7923|          0.1101|        193.3330|  -3900.8096|
| 2022|policyengine |    13|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1527|     354|   0.3759|    0.4335|         0.7825|          0.8051|          0.1061|        194.6306|  12911.6461|
| 2023|policyengine |     6|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1524|     364|   0.3524|    0.4009|         0.7555|          0.7692|          0.1043|        278.8839| -12655.2000|
| 2024|policyengine |     7|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  637|
| 2017|TRUE        |3 deductions     | 2394|
| 2017|TRUE        |4 taxable income |    3|
| 2017|TRUE        |6 other credits  |    1|
| 2017|TRUE        |7 rate/rounding  |  262|
| 2017|FALSE       |1 state AGI      | 5050|
| 2017|FALSE       |3 deductions     |  672|
| 2017|FALSE       |7 rate/rounding  |   11|
| 2018|TRUE        |1 state AGI      |  873|
| 2018|TRUE        |3 deductions     | 3872|
| 2018|TRUE        |4 taxable income |   30|
| 2018|TRUE        |5 state EITC     |   36|
| 2018|TRUE        |7 rate/rounding  | 1379|
| 2018|FALSE       |1 state AGI      | 5096|
| 2018|FALSE       |3 deductions     |  266|
| 2018|FALSE       |4 taxable income |    9|
| 2018|FALSE       |5 state EITC     |  394|
| 2018|FALSE       |7 rate/rounding  |   10|
| 2019|TRUE        |1 state AGI      |  348|
| 2019|TRUE        |3 deductions     |  490|
| 2019|TRUE        |4 taxable income |   19|
| 2019|TRUE        |5 state EITC     |   33|
| 2019|FALSE       |1 state AGI      | 4751|
| 2019|FALSE       |3 deductions     |  165|
| 2019|FALSE       |4 taxable income |   13|
| 2019|FALSE       |5 state EITC     |  380|
| 2020|TRUE        |1 state AGI      |  283|
| 2020|TRUE        |3 deductions     |  460|
| 2020|TRUE        |4 taxable income |   16|
| 2020|TRUE        |5 state EITC     |   31|
| 2020|TRUE        |6 other credits  |    4|
| 2020|FALSE       |1 state AGI      | 4761|
| 2020|FALSE       |3 deductions     |  170|
| 2020|FALSE       |4 taxable income |    9|
| 2020|FALSE       |5 state EITC     |  389|
| 2020|FALSE       |6 other credits  |    1|

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
|WI    |taxsim       |     2017|     2020|input-coverage     |exclude  |The WI variant of the crosswalk-exposure class: Schedule 1 computes the itemized-deduction credit from federal Schedule A amounts whether or not the filer itemized federally, so both models now compute it for federal standard-deduction takers (2026-08-15 fix), and the 5% credit inherits the crosswalk's component-representation noise (med_pref allocation, unhanded investment interest and "other"). Excluded via the standard exposure predicate                                                                                                                                                        |

