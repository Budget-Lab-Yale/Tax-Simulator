# Cross-model validation: NE

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3705|    0.4610|         0.5102|          0.5661|          0.1161|        174.0317|   6607.7582|
| 2018|taxsim       | 20515|   13144|   0.4659|    0.5810|         0.6422|          0.7168|          0.1140|         27.2543|    264.9031|
| 2019|taxsim       | 20514|   13088|   0.4609|    0.5776|         0.6423|          0.7168|          0.1136|         29.1636|    -27.3230|
| 2020|taxsim       | 20513|   12682|   0.4474|    0.5634|         0.6391|          0.7137|          0.1131|         35.3300|    -16.9241|
| 2021|policyengine |  1536|     270|   0.2480|    0.3659|         0.9037|          0.9556|          0.0827|        375.0521| -10381.5504|
| 2022|policyengine |  1530|     316|   0.2856|    0.3712|         0.8766|          0.9177|          0.0980|        375.9443|   4440.7143|
| 2023|policyengine |  1533|     358|   0.2844|    0.3796|         0.8883|          0.9134|          0.0959|        392.3433|  19902.6740|
| 2024|policyengine |  1531|     364|   0.2645|    0.3423|         0.8132|          0.8571|          0.0888|        467.1529|   -878.8147|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2799|
| 2017|TRUE        |3 deductions    | 3546|
| 2017|TRUE        |6 other credits |   29|
| 2017|TRUE        |7 rate/rounding |   39|
| 2017|FALSE       |1 state AGI     | 5291|
| 2017|FALSE       |3 deductions    |  249|
| 2017|FALSE       |5 state EITC    |  960|
| 2018|TRUE        |1 state AGI     | 2542|
| 2018|TRUE        |3 deductions    | 2019|
| 2018|TRUE        |6 other credits |   95|
| 2018|TRUE        |7 rate/rounding |   47|
| 2018|FALSE       |1 state AGI     | 5126|
| 2018|FALSE       |3 deductions    |  204|
| 2018|FALSE       |5 state EITC    |  921|
| 2018|FALSE       |6 other credits |    3|
| 2018|FALSE       |7 rate/rounding |    1|
| 2019|TRUE        |1 state AGI     | 2463|
| 2019|TRUE        |3 deductions    | 2061|
| 2019|TRUE        |6 other credits |  109|
| 2019|TRUE        |7 rate/rounding |   49|
| 2019|FALSE       |1 state AGI     | 5197|
| 2019|FALSE       |3 deductions    |  206|
| 2019|FALSE       |5 state EITC    |  972|
| 2019|FALSE       |6 other credits |    2|
| 2020|TRUE        |1 state AGI     | 2507|
| 2020|TRUE        |3 deductions    | 1931|
| 2020|TRUE        |6 other credits |   94|
| 2020|TRUE        |7 rate/rounding |   45|
| 2020|FALSE       |1 state AGI     | 5651|
| 2020|FALSE       |3 deductions    |  181|
| 2020|FALSE       |5 state EITC    |  923|
| 2020|FALSE       |7 rate/rounding |    3|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

