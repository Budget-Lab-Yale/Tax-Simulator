# Cross-model validation: VA

Class: broad | Generated: 2026-08-13 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4308|    0.4971|         0.5364|          0.5974|          0.2216|        105.5916|    9722.243|
| 2018|taxsim       | 20515|   13144|   0.5178|    0.5880|         0.6667|          0.7138|          0.2122|          5.6676|    9861.604|
| 2019|taxsim       | 20514|   13088|   0.5106|    0.5696|         0.6565|          0.6983|          0.2161|          8.0120|   16802.376|
| 2020|taxsim       | 20513|   12682|   0.5111|    0.5699|         0.6531|          0.6930|          0.2130|          8.0118|   18019.128|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     316|   0.3850|    0.5007|         0.8987|          0.9399|          0.1072|         98.7486|    1656.090|
| 2023|policyengine |  1533|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1531|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  623|
| 2017|TRUE        |2 exemptions     |   46|
| 2017|TRUE        |3 deductions     | 5123|
| 2017|TRUE        |4 taxable income |  131|
| 2017|TRUE        |6 other credits  |   83|
| 2017|TRUE        |7 rate/rounding  |   64|
| 2017|FALSE       |1 state AGI      | 5043|
| 2017|FALSE       |3 deductions     |  199|
| 2017|FALSE       |4 taxable income |   36|
| 2017|FALSE       |5 state EITC     |  325|
| 2017|FALSE       |6 other credits  |    3|
| 2018|TRUE        |1 state AGI      |  543|
| 2018|TRUE        |2 exemptions     |   54|
| 2018|TRUE        |3 deductions     | 3432|
| 2018|TRUE        |4 taxable income |  164|
| 2018|TRUE        |6 other credits  |  103|
| 2018|TRUE        |7 rate/rounding  |   85|
| 2018|FALSE       |1 state AGI      | 4940|
| 2018|FALSE       |3 deductions     |  153|
| 2018|FALSE       |4 taxable income |   18|
| 2018|FALSE       |5 state EITC     |  399|
| 2018|FALSE       |6 other credits  |    1|
| 2018|FALSE       |7 rate/rounding  |    1|
| 2019|TRUE        |1 state AGI      |  248|
| 2019|TRUE        |2 exemptions     |   45|
| 2019|TRUE        |3 deductions     | 3853|
| 2019|TRUE        |4 taxable income |  147|
| 2019|TRUE        |6 other credits  |  107|
| 2019|TRUE        |7 rate/rounding  |   96|
| 2019|FALSE       |1 state AGI      | 5015|
| 2019|FALSE       |2 exemptions     |    1|
| 2019|FALSE       |3 deductions     |  172|
| 2019|FALSE       |4 taxable income |   24|
| 2019|FALSE       |5 state EITC     |  329|
| 2019|FALSE       |6 other credits  |    2|
| 2020|TRUE        |1 state AGI      |  231|
| 2020|TRUE        |2 exemptions     |   57|
| 2020|TRUE        |3 deductions     | 3761|
| 2020|TRUE        |4 taxable income |  169|
| 2020|TRUE        |6 other credits  |   96|
| 2020|TRUE        |7 rate/rounding  |   85|
| 2020|FALSE       |1 state AGI      | 5064|
| 2020|FALSE       |2 exemptions     |    4|
| 2020|FALSE       |3 deductions     |  167|
| 2020|FALSE       |4 taxable income |   31|
| 2020|FALSE       |5 state EITC     |  359|
| 2020|FALSE       |6 other credits  |    3|
| 2020|FALSE       |7 rate/rounding  |    1|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                          |
|:-----|:------------|--------:|--------:|:----------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                              |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                              |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                              |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                             |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                             |
|VA    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the fall-2022 Virginia rebate ($250 single / $500 joint; 2022 Special Session HB 30) into TAX YEAR 2021 via va_rebate (parameter: 2021 250/500, 2022 0). Clean mismatches mass at +250/+500 (71+14 of ~130). Excluded via predicate on the exported rebate                                                                                                                                  |
|VA    |policyengine |     2023|     2024|transfer-netting |exclude  |PE books the 2023 Virginia rebate (HB6001: $200/$400, paid fall 2023) AND its HB 1600 reauthorization for TY2024 into tax years 2023 and 2024 via va_rebate (parameter: 2023 200/400 through 2024; 0 from 2025). Clean mismatches mass at +200/+400 in BOTH years (129+34 in 2023, 131+45 in 2024), depressing both cells to ~0.35 from 0.940 in 2022. Excluded via predicate on the exported rebate |

