# Cross-model validation: GA

Class: broad | Generated: 2026-07-18 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13458|   0.3647|    0.4780|         0.4233|          0.5287|          0.1722|        137.8272|    384.4966|
| 2018|taxsim       | 20515|   13504|   0.3652|    0.4288|         0.4257|          0.4904|          0.1774|        221.9974|  -4035.1650|
| 2019|taxsim       | 20514|   13433|   0.3656|    0.4303|         0.4269|          0.4935|          0.1769|        212.7474|  -4080.9866|
| 2020|taxsim       | 20513|   13070|   0.3615|    0.4285|         0.4223|          0.4892|          0.1705|        212.7474|  -4603.8190|
| 2021|policyengine |  1536|     270|   0.1615|    0.2038|         0.4037|          0.4481|          0.1491|        478.1241|  -8129.0492|
| 2022|policyengine |  1530|     315|   0.2261|    0.3471|         0.6476|          0.6984|          0.1425|        212.7450|   3637.3099|
| 2023|policyengine |  1533|     357|   0.2277|    0.3620|         0.5938|          0.6471|          0.1324|        196.2487|  17438.7280|
| 2024|policyengine |  1531|     364|   0.4017|    0.4520|         0.9203|          0.9203|          0.1600|        221.0783|  -1322.8861|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3922|
| 2017|TRUE        |2 exemptions    | 2972|
| 2017|TRUE        |3 deductions    |  467|
| 2017|TRUE        |6 other credits |  311|
| 2017|TRUE        |7 rate/rounding |   89|
| 2017|FALSE       |1 state AGI     | 3409|
| 2017|FALSE       |2 exemptions    | 1267|
| 2017|FALSE       |3 deductions    |  225|
| 2017|FALSE       |6 other credits |  340|
| 2017|FALSE       |7 rate/rounding |   29|
| 2018|TRUE        |1 state AGI     | 3991|
| 2018|TRUE        |2 exemptions    | 2967|
| 2018|TRUE        |3 deductions    |  472|
| 2018|TRUE        |6 other credits |  187|
| 2018|TRUE        |7 rate/rounding |  138|
| 2018|FALSE       |1 state AGI     | 3488|
| 2018|FALSE       |2 exemptions    | 1298|
| 2018|FALSE       |3 deductions    |  294|
| 2018|FALSE       |6 other credits |  149|
| 2018|FALSE       |7 rate/rounding |   38|
| 2019|TRUE        |1 state AGI     | 3878|
| 2019|TRUE        |2 exemptions    | 3020|
| 2019|TRUE        |3 deductions    |  484|
| 2019|TRUE        |6 other credits |  199|
| 2019|TRUE        |7 rate/rounding |  118|
| 2019|FALSE       |1 state AGI     | 3505|
| 2019|FALSE       |2 exemptions    | 1321|
| 2019|FALSE       |3 deductions    |  280|
| 2019|FALSE       |6 other credits |  170|
| 2019|FALSE       |7 rate/rounding |   40|
| 2020|TRUE        |1 state AGI     | 3914|
| 2020|TRUE        |2 exemptions    | 2909|
| 2020|TRUE        |3 deductions    |  419|
| 2020|TRUE        |6 other credits |  181|
| 2020|TRUE        |7 rate/rounding |  128|
| 2020|FALSE       |1 state AGI     | 3646|
| 2020|FALSE       |2 exemptions    | 1371|
| 2020|FALSE       |3 deductions    |  289|
| 2020|FALSE       |6 other credits |  177|
| 2020|FALSE       |7 rate/rounding |   63|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

