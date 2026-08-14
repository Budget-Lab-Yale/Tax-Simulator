# Cross-model validation: DC

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3073|    0.3649|         0.4135|          0.4706|          0.0940|        338.0530|   11360.802|
| 2018|taxsim       | 20515|   13144|   0.4013|    0.4815|         0.5467|          0.6230|          0.1020|        135.2913|   13362.262|
| 2019|taxsim       | 20514|   13088|   0.3979|    0.4754|         0.5463|          0.6193|          0.1016|        150.9431|   12597.150|
| 2020|taxsim       | 20513|   12682|   0.3958|    0.4694|         0.5584|          0.6269|          0.1002|        163.3555|   12485.705|
| 2021|policyengine |  1536|     269|   0.1973|    0.2871|         0.6840|          0.7546|          0.0723|        575.1798|  -11074.565|
| 2022|policyengine |  1530|     316|   0.2608|    0.3301|         0.7880|          0.8323|          0.0843|        554.7554|    7689.692|
| 2023|policyengine |  1533|     358|   0.2518|    0.3118|         0.7709|          0.8073|          0.0841|        590.7514|   34000.858|
| 2024|policyengine |  1531|     363|   0.2541|    0.3207|         0.7521|          0.8182|          0.0784|        620.5019|    -640.193|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3117|
| 2017|TRUE        |2 exemptions     |  917|
| 2017|TRUE        |3 deductions     | 2284|
| 2017|TRUE        |4 taxable income |  329|
| 2017|TRUE        |5 state EITC     |  233|
| 2017|TRUE        |6 other credits  |  101|
| 2017|TRUE        |7 rate/rounding  |  698|
| 2017|FALSE       |1 state AGI      | 5489|
| 2017|FALSE       |2 exemptions     |  114|
| 2017|FALSE       |3 deductions     |  107|
| 2017|FALSE       |4 taxable income |   12|
| 2017|FALSE       |5 state EITC     |  798|
| 2017|FALSE       |7 rate/rounding  |   11|
| 2018|TRUE        |1 state AGI      | 2948|
| 2018|TRUE        |3 deductions     | 2266|
| 2018|TRUE        |4 taxable income |   10|
| 2018|TRUE        |5 state EITC     |  224|
| 2018|TRUE        |6 other credits  |   75|
| 2018|TRUE        |7 rate/rounding  |  435|
| 2018|FALSE       |1 state AGI      | 5342|
| 2018|FALSE       |3 deductions     |  199|
| 2018|FALSE       |4 taxable income |    1|
| 2018|FALSE       |5 state EITC     |  768|
| 2018|FALSE       |6 other credits  |    1|
| 2018|FALSE       |7 rate/rounding  |   13|
| 2019|TRUE        |1 state AGI      | 2865|
| 2019|TRUE        |3 deductions     | 2314|
| 2019|TRUE        |4 taxable income |   16|
| 2019|TRUE        |5 state EITC     |  229|
| 2019|TRUE        |6 other credits  |   74|
| 2019|TRUE        |7 rate/rounding  |  440|
| 2019|FALSE       |1 state AGI      | 5422|
| 2019|FALSE       |3 deductions     |  211|
| 2019|FALSE       |5 state EITC     |  767|
| 2019|FALSE       |7 rate/rounding  |   13|
| 2020|TRUE        |1 state AGI      | 2645|
| 2020|TRUE        |3 deductions     | 2197|
| 2020|TRUE        |4 taxable income |   19|
| 2020|TRUE        |5 state EITC     |  241|
| 2020|TRUE        |6 other credits  |   65|
| 2020|TRUE        |7 rate/rounding  |  433|
| 2020|FALSE       |1 state AGI      | 5835|
| 2020|FALSE       |3 deductions     |  198|
| 2020|FALSE       |4 taxable income |    1|
| 2020|FALSE       |5 state EITC     |  747|
| 2020|FALSE       |7 rate/rounding  |   13|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

