# Cross-model validation: SC

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4211|    0.4638|         0.5063|          0.5506|          0.2407|        194.6837|  19948.1115|
| 2018|taxsim       | 20515|   13144|   0.5107|    0.5844|         0.6324|          0.6996|          0.2578|          8.7859|   -917.6594|
| 2019|taxsim       | 20514|   13088|   0.5116|    0.5832|         0.6359|          0.6990|          0.2606|         10.3033|  -1396.8138|
| 2020|taxsim       | 20513|   12682|   0.5107|    0.5822|         0.6360|          0.7020|          0.2548|          9.1864|  -1050.0108|
| 2021|policyengine |  1536|     269|   0.2448|    0.2656|         0.5390|          0.5465|          0.2441|        800.0987| -21062.9778|
| 2022|policyengine |  1530|     316|   0.3529|    0.4124|         0.8544|          0.8956|          0.2281|        287.3514| -10717.4316|
| 2023|policyengine |  1533|     358|   0.3457|    0.4110|         0.8408|          0.8715|          0.2257|        268.3547|     90.3781|
| 2024|policyengine |  1531|     364|   0.3462|    0.4076|         0.8516|          0.8626|          0.2175|        293.8276| -16550.9341|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 6134|
| 2017|TRUE        |4 taxable income |   22|
| 2017|TRUE        |6 other credits  |  308|
| 2017|FALSE       |1 state AGI      | 5344|
| 2017|FALSE       |4 taxable income |   22|
| 2017|FALSE       |6 other credits  |   45|
| 2018|TRUE        |1 state AGI      | 4089|
| 2018|TRUE        |2 exemptions     |  468|
| 2018|TRUE        |4 taxable income |   64|
| 2018|TRUE        |6 other credits  |  211|
| 2018|FALSE       |1 state AGI      | 5109|
| 2018|FALSE       |2 exemptions     |   22|
| 2018|FALSE       |4 taxable income |    9|
| 2018|FALSE       |5 state EITC     |   65|
| 2019|TRUE        |1 state AGI      | 4187|
| 2019|TRUE        |2 exemptions     |  340|
| 2019|TRUE        |4 taxable income |   73|
| 2019|TRUE        |6 other credits  |  165|
| 2019|FALSE       |1 state AGI      | 5204|
| 2019|FALSE       |2 exemptions     |   15|
| 2019|FALSE       |4 taxable income |    8|
| 2019|FALSE       |5 state EITC     |   27|
| 2020|TRUE        |1 state AGI      | 4085|
| 2020|TRUE        |2 exemptions     |  317|
| 2020|TRUE        |4 taxable income |   54|
| 2020|TRUE        |6 other credits  |  160|
| 2020|FALSE       |1 state AGI      | 5343|
| 2020|FALSE       |2 exemptions     |   33|
| 2020|FALSE       |4 taxable income |    9|
| 2020|FALSE       |5 state EITC     |   26|
| 2020|FALSE       |6 other credits  |    9|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                |
|:-----|:------|--------:|--------:|:--------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                    |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                    |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                    |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                        |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                   |
|SC    |taxsim |     2017|     2020|state-law      |annotate |Small schedule-constant differences: our subtraction-method constants transcribed from the published SC1040TT vs TAXSIM's schedule (+$6.80 at top-bracket and +$3.20 at mid-bracket incomes on 2019 probe cases); bounded ~$10 and inside the $15 tolerance after the 2026-08-11 aged-deduction and TWEC encodings closed the large wedges |

