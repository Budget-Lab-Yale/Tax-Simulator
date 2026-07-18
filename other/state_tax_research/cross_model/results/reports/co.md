# Cross-model validation: CO

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.3683|    0.4711|         0.4955|          0.5646|          0.1195|        138.8962|    5846.275|
| 2018|taxsim       | 20515|   13504|   0.3860|    0.4992|         0.5184|          0.6045|          0.1218|        100.9956|   -5868.301|
| 2019|taxsim       | 20514|   13433|   0.3843|    0.4981|         0.5190|          0.6054|          0.1212|        102.4693|   -5747.035|
| 2020|taxsim       | 20513|   13070|   0.3751|    0.4886|         0.5026|          0.5884|          0.1189|        117.8635|   -6575.115|
| 2021|policyengine |  1536|     271|   0.0026|    0.0111|         0.0000|          0.0000|          0.0000|       1177.0612|  -13138.652|
| 2022|policyengine |  1530|     317|   0.0085|    0.0588|         0.0000|          0.0095|          0.0000|        605.3275|   -8393.629|
| 2023|policyengine |  1533|     357|   0.0020|    0.0104|         0.0000|          0.0000|          0.0000|       1285.1936|    1221.171|
| 2024|policyengine |  1531|     364|   0.0072|    0.0588|         0.0055|          0.0247|          0.0000|        632.1228|  -13034.418|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6789|
| 2017|TRUE        |4 taxable income |    1|
| 2017|FALSE       |1 state AGI      | 6169|
| 2018|TRUE        |1 state AGI      | 6503|
| 2018|TRUE        |4 taxable income |    1|
| 2018|FALSE       |1 state AGI      | 6093|
| 2019|TRUE        |1 state AGI      | 6461|
| 2019|FALSE       |1 state AGI      | 6169|
| 2020|TRUE        |1 state AGI      | 6501|
| 2020|FALSE       |1 state AGI      | 6315|
| 2020|FALSE       |4 taxable income |    2|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |
|CO    |taxsim |     2018|     2020|crosswalk      |annotate |QBI inputs zeroed in taxsim_crosswalk (scorp/pbusinc/pprofinc TODO), so TAXSIM's federal taxable income (CO start point) lacks QBID differences                                                                                     |

