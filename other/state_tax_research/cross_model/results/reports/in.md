# Cross-model validation: IN

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.2223|    0.5521|         0.2836|          0.6617|          0.0516|         92.2892|    872.7407|
| 2018|taxsim       | 20515|   13504|   0.2232|    0.5517|         0.2848|          0.6635|          0.0532|         93.3450|    874.0947|
| 2019|taxsim       | 20514|   13433|   0.2200|    0.5475|         0.2830|          0.6653|          0.0522|         93.8833|    924.3429|
| 2020|taxsim       | 20513|   13070|   0.2155|    0.5357|         0.2816|          0.6582|          0.0517|         96.8985|   1021.7377|
| 2021|policyengine |  1536|     270|   0.1517|    0.4850|         0.3630|          0.8704|          0.0475|        110.0544|  -2122.3110|
| 2022|policyengine |  1530|     318|   0.1810|    0.4856|         0.4434|          0.8491|          0.0458|        110.5758|   1748.3897|
| 2023|policyengine |  1533|     356|   0.1716|    0.5042|         0.4017|          0.8764|          0.0450|        100.0000|   9190.6017|
| 2024|policyengine |  1531|     365|   0.1724|    0.4729|         0.4137|          0.8548|          0.0470|        106.7510|  -1018.2362|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 3104|
| 2017|TRUE        |2 exemptions     | 5530|
| 2017|TRUE        |4 taxable income | 1006|
| 2017|TRUE        |5 state EITC     |    1|
| 2017|FALSE       |1 state AGI      | 5166|
| 2017|FALSE       |2 exemptions     |  638|
| 2017|FALSE       |4 taxable income |   43|
| 2017|FALSE       |5 state EITC     |  465|
| 2018|TRUE        |1 state AGI      | 3126|
| 2018|TRUE        |2 exemptions     | 5531|
| 2018|TRUE        |4 taxable income | 1001|
| 2018|FALSE       |1 state AGI      | 5165|
| 2018|FALSE       |2 exemptions     |  626|
| 2018|FALSE       |4 taxable income |   47|
| 2018|FALSE       |5 state EITC     |  440|
| 2019|TRUE        |1 state AGI      | 2978|
| 2019|TRUE        |2 exemptions     | 5636|
| 2019|TRUE        |4 taxable income | 1015|
| 2019|TRUE        |5 state EITC     |    2|
| 2019|FALSE       |1 state AGI      | 5195|
| 2019|FALSE       |2 exemptions     |  659|
| 2019|FALSE       |4 taxable income |   38|
| 2019|FALSE       |5 state EITC     |  477|
| 2020|TRUE        |1 state AGI      | 2976|
| 2020|TRUE        |2 exemptions     | 5404|
| 2020|TRUE        |4 taxable income | 1008|
| 2020|TRUE        |5 state EITC     |    1|
| 2020|FALSE       |1 state AGI      | 5522|
| 2020|FALSE       |2 exemptions     |  673|
| 2020|FALSE       |4 taxable income |   50|
| 2020|FALSE       |5 state EITC     |  459|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

