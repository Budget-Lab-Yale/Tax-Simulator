# Cross-model validation: MI

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 15864|    9293|   0.5275|    0.6617|         0.6391|          0.7357|          0.0501|         11.1723|     65.7586|
| 2017|taxsim       |  4649|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 15770|    9234|   0.5424|    0.6595|         0.6525|          0.7357|          0.0504|          6.5406|     75.9485|
| 2018|taxsim       |  4745|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 15586|    9299|   0.5446|    0.6558|         0.6595|          0.7329|          0.0510|          6.1628|     85.3892|
| 2019|taxsim       |  4928|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 15608|    8880|   0.5209|    0.6322|         0.6525|          0.7298|          0.0509|         10.1760|     91.5452|
| 2020|taxsim       |  4905|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.3737|    0.5401|         0.6391|          0.7080|          0.0239|         61.6944|    764.5406|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.3953|    0.5092|         0.6014|          0.6486|          0.0316|         89.6900|    263.5975|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     469|   0.3889|    0.5359|         0.6418|          0.7484|          0.0303|         73.8468|    295.7351|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.3904|    0.5148|         0.6522|          0.7174|          0.0252|         84.1505|    -60.0165|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 3083|
| 2017|TRUE        |2 exemptions    | 1500|
| 2017|FALSE       |1 state AGI     | 6407|
| 2017|FALSE       |2 exemptions    | 1097|
| 2018|TRUE        |1 state AGI     | 3057|
| 2018|TRUE        |3 deductions    |  273|
| 2018|TRUE        |6 other credits | 1212|
| 2018|FALSE       |1 state AGI     | 6264|
| 2018|FALSE       |3 deductions    |   74|
| 2018|FALSE       |5 state EITC    |  790|
| 2018|FALSE       |6 other credits |  239|
| 2019|TRUE        |1 state AGI     | 3046|
| 2019|TRUE        |3 deductions    |  292|
| 2019|TRUE        |6 other credits | 1149|
| 2019|FALSE       |1 state AGI     | 6280|
| 2019|FALSE       |3 deductions    |   81|
| 2019|FALSE       |5 state EITC    |  886|
| 2019|FALSE       |6 other credits |  209|
| 2020|TRUE        |1 state AGI     | 2977|
| 2020|TRUE        |3 deductions    |  287|
| 2020|TRUE        |6 other credits | 1126|
| 2020|FALSE       |1 state AGI     | 6857|
| 2020|FALSE       |3 deductions    |   62|
| 2020|FALSE       |5 state EITC    |  813|
| 2020|FALSE       |6 other credits |  196|

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
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding    |

