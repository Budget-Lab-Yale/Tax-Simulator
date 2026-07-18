# Cross-model validation: SC

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.3696|    0.4026|         0.4229|          0.4579|          0.2277|        361.7932|  20225.9980|
| 2018|taxsim       | 20515|   13504|   0.4070|    0.4599|         0.4733|          0.5264|          0.2458|        180.4200|  -1198.3516|
| 2019|taxsim       | 20514|   13433|   0.4107|    0.4602|         0.4790|          0.5284|          0.2485|        192.2531|  -1698.1749|
| 2020|taxsim       | 20513|   13070|   0.4065|    0.4558|         0.4742|          0.5240|          0.2432|        196.0468|  -1268.3528|
| 2021|policyengine |  1536|     269|   0.2357|    0.2546|         0.5167|          0.5242|          0.2350|        821.1046| -20892.0763|
| 2022|policyengine |  1530|     316|   0.3144|    0.3693|         0.7785|          0.8165|          0.2222|        338.2987| -10555.9794|
| 2023|policyengine |  1533|     358|   0.3079|    0.3686|         0.7318|          0.7737|          0.2179|        347.0194|    247.2633|
| 2024|policyengine |  1531|     364|   0.3076|    0.3632|         0.7390|          0.7582|          0.2123|        365.4443| -16383.2440|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 7082|
| 2017|TRUE        |4 taxable income |   19|
| 2017|TRUE        |6 other credits  |  665|
| 2017|FALSE       |1 state AGI      | 5084|
| 2017|FALSE       |4 taxable income |   20|
| 2017|FALSE       |6 other credits  |   62|
| 2018|TRUE        |1 state AGI      | 5934|
| 2018|TRUE        |2 exemptions     |  427|
| 2018|TRUE        |4 taxable income |   44|
| 2018|TRUE        |6 other credits  |  708|
| 2018|FALSE       |1 state AGI      | 4963|
| 2018|FALSE       |2 exemptions     |   19|
| 2018|FALSE       |4 taxable income |    7|
| 2018|FALSE       |5 state EITC     |   62|
| 2018|FALSE       |6 other credits  |    2|
| 2019|TRUE        |1 state AGI      | 5990|
| 2019|TRUE        |2 exemptions     |  303|
| 2019|TRUE        |4 taxable income |   54|
| 2019|TRUE        |6 other credits  |  651|
| 2019|FALSE       |1 state AGI      | 5045|
| 2019|FALSE       |2 exemptions     |   10|
| 2019|FALSE       |4 taxable income |    9|
| 2019|FALSE       |5 state EITC     |   24|
| 2019|FALSE       |6 other credits  |    2|
| 2020|TRUE        |1 state AGI      | 5911|
| 2020|TRUE        |2 exemptions     |  284|
| 2020|TRUE        |4 taxable income |   46|
| 2020|TRUE        |6 other credits  |  631|
| 2020|FALSE       |1 state AGI      | 5191|
| 2020|FALSE       |2 exemptions     |   31|
| 2020|FALSE       |4 taxable income |    6|
| 2020|FALSE       |5 state EITC     |   25|
| 2020|FALSE       |6 other credits  |   49|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |
|SC    |taxsim |     2018|     2020|crosswalk      |annotate |QBI inputs zeroed in taxsim_crosswalk, so TAXSIM's federal taxable income (SC start point) lacks QBID differences                                                                                                                   |

