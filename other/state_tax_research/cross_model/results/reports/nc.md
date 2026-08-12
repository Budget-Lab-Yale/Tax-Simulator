# Cross-model validation: NC

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4863|    0.5808|         0.6196|          0.7113|          0.1208|         21.4154| -11584.4090|
| 2018|taxsim       | 20515|   13144|   0.4996|    0.6121|         0.6276|          0.7414|          0.1540|         15.0997| -10780.2828|
| 2019|taxsim       | 20514|   13088|   0.5067|    0.6206|         0.6386|          0.7496|          0.1617|         12.5767| -11118.1910|
| 2020|taxsim       | 20513|   12682|   0.5043|    0.6208|         0.6354|          0.7485|          0.1631|         13.4818| -10951.4526|
| 2021|policyengine |  1536|     269|   0.3151|    0.4049|         0.9405|          0.9703|          0.1393|        268.8005|  -8357.7852|
| 2022|policyengine |  1530|     317|   0.3686|    0.4203|         0.9464|          0.9590|          0.1627|        259.7751|    998.3628|
| 2023|policyengine |  1533|     357|   0.3490|    0.4083|         0.9244|          0.9384|          0.1487|        302.7999|  13816.5248|
| 2024|policyengine |  1531|     364|   0.3396|    0.3958|         0.9258|          0.9341|          0.1372|        286.8731|  -2926.8577|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2506|
| 2017|TRUE        |3 deductions     | 2032|
| 2017|TRUE        |6 other credits  |   53|
| 2017|TRUE        |7 rate/rounding  |  389|
| 2017|FALSE       |1 state AGI      | 5158|
| 2017|FALSE       |3 deductions     |  183|
| 2017|FALSE       |6 other credits  |   96|
| 2017|FALSE       |7 rate/rounding  |  121|
| 2018|TRUE        |1 state AGI      | 2485|
| 2018|TRUE        |3 deductions     | 1898|
| 2018|TRUE        |4 taxable income |  512|
| 2018|FALSE       |1 state AGI      | 5081|
| 2018|FALSE       |3 deductions     |  165|
| 2018|FALSE       |4 taxable income |  124|
| 2019|TRUE        |1 state AGI      | 2366|
| 2019|TRUE        |3 deductions     | 1881|
| 2019|TRUE        |4 taxable income |  483|
| 2019|FALSE       |1 state AGI      | 5115|
| 2019|FALSE       |3 deductions     |  149|
| 2019|FALSE       |4 taxable income |  126|
| 2020|TRUE        |1 state AGI      | 2417|
| 2020|TRUE        |3 deductions     | 1755|
| 2020|TRUE        |4 taxable income |  452|
| 2020|FALSE       |1 state AGI      | 5205|
| 2020|FALSE       |3 deductions     |  171|
| 2020|FALSE       |4 taxable income |  168|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                            |
|:-----|:------|--------:|--------:|:------------------|:--------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                    |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                               |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                               |
|NC    |taxsim |     2017|     2017|external-model-bug |annotate |TAXSIM omits the upper AGI cutoff on the TY2017 NC credit for children (G.S. 105-153.10: $100/child up to $100k MFJ / $80k HoH / $50k single-MFS), granting $100-125 per child at any income (record-verified to $1.58M AGI). Small tail: 7 of 976 credit-mass records sit above the statutory cutoff; the other 969 were OUR gap (2017 credit now encoded, test NC-5) |

