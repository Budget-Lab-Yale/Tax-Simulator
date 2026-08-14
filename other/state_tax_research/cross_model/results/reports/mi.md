# Cross-model validation: MI

Class: broad | Generated: 2026-08-14 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 19380|   12396|   0.4112|    0.5510|         0.5562|          0.6832|          0.0415|         59.3851|    1124.088|
| 2017|taxsim       |  1133|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 19389|   12430|   0.4050|    0.5473|         0.5496|          0.6801|          0.0414|         61.9389|    1141.577|
| 2018|taxsim       |  1126|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 19386|   12377|   0.4075|    0.5426|         0.5613|          0.6788|          0.0415|         61.4760|    1206.849|
| 2019|taxsim       |  1128|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 19410|   12013|   0.3865|    0.5218|         0.5488|          0.6749|          0.0417|         78.6446|    1332.932|
| 2020|taxsim       |  1103|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     269|   0.3132|    0.4447|         0.5762|          0.6357|          0.0189|        163.3524|   -2873.965|
| 2022|policyengine |  1530|     318|   0.3144|    0.4157|         0.5126|          0.5629|          0.0255|        258.8256|    2170.980|
| 2023|policyengine |  1533|     358|   0.3138|    0.4331|         0.5866|          0.6844|          0.0248|        186.6595|   11700.697|
| 2024|policyengine |  1531|     365|   0.3142|    0.4141|         0.5890|          0.6548|          0.0209|        260.7598|   -1557.431|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4509|
| 2017|TRUE        |2 exemptions    | 1688|
| 2017|FALSE       |1 state AGI     | 5209|
| 2017|FALSE       |2 exemptions    | 1138|
| 2018|TRUE        |1 state AGI     | 4629|
| 2018|TRUE        |3 deductions    |  353|
| 2018|TRUE        |6 other credits | 1331|
| 2018|FALSE       |1 state AGI     | 5249|
| 2018|FALSE       |3 deductions    |   56|
| 2018|FALSE       |5 state EITC    |  795|
| 2018|FALSE       |6 other credits |  250|
| 2019|TRUE        |1 state AGI     | 4433|
| 2019|TRUE        |3 deductions    |  405|
| 2019|TRUE        |6 other credits | 1303|
| 2019|FALSE       |1 state AGI     | 5272|
| 2019|FALSE       |3 deductions    |   63|
| 2019|FALSE       |5 state EITC    |  898|
| 2019|FALSE       |6 other credits |  241|
| 2020|TRUE        |1 state AGI     | 4420|
| 2020|TRUE        |3 deductions    |  420|
| 2020|TRUE        |6 other credits | 1249|
| 2020|FALSE       |1 state AGI     | 5823|
| 2020|FALSE       |3 deductions    |   51|
| 2020|FALSE       |5 state EITC    |  826|
| 2020|FALSE       |6 other credits |  222|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:-----|:------|--------:|--------:|:------------------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                          |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                     |
|MI    |taxsim |     2017|     2020|external-model-bug |annotate |TAXSIM applies the Tier-2 Michigan Standard Deduction amount ($20,000/$40,000) to ALL filers 67+, ignoring the birth-cohort tiers: a Tier-1 (born before 1946) pensioner is capped at 20,000 instead of the Form 4884 private-pension maximum (52,808 single 2019; probe: 74-year-old with 60k pension -> TAXSIM 1,513.00 vs form-true 118.66), and a Tier-1 filer with wages+interest receives the flat 20,000 the form does not give that cohort (probe: 76-year-old -> TAXSIM 25.50). Non-senior and Tier-2 shapes match to the cent (5 probe cases)                                                                                      |
|MI    |taxsim |     2017|     2020|transfer-netting   |exclude  |TAXSIM nets a flat refundable credit into MI liability wherever its computed household income (v30) collapses to $1.01: the MI-1040CR-7 home heating credit standard-allowance ladder (~370-410 records/yr at 90% of the one-exemption allowance: $349 2017 / $351 2018 / $386 2019 / $418 2020; larger-household steps $469-715). The $1.01 base is itself wrong on many records (multi-million-AGI filers receive the credit; zero-income records show a -$386 refund vs our $0). The home heating credit is an energy-assistance transfer paid outside MI-1040 liability and deliberately outside our IIT concept; excluded via predicate |

