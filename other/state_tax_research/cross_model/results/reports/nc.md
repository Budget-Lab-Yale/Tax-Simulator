# Cross-model validation: NC

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9663|    7180|   0.8166|    0.8636|         0.8843|          0.9078|          0.2405|          0.0023|   -812.3857|
| 2017|taxsim       | 10850|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9692|    7280|   0.8370|    0.9274|         0.8853|          0.9600|          0.3105|          0.0020|   -497.2766|
| 2018|taxsim       | 10823|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9681|    7237|   0.8411|    0.9330|         0.8913|          0.9653|          0.3252|          0.0019|   -450.5912|
| 2019|taxsim       | 10833|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9961|    7137|   0.8243|    0.9194|         0.8854|          0.9591|          0.3162|          0.0020|   -817.1628|
| 2020|taxsim       | 10552|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     269|   0.3145|    0.4056|         0.9405|          0.9665|          0.1393|        268.8005|  -8359.3894|
| 2022|policyengine |  1530|     317|   0.3680|    0.4209|         0.9432|          0.9590|          0.1627|        257.7245|    998.0284|
| 2023|policyengine |  1533|     357|   0.3464|    0.4083|         0.9160|          0.9384|          0.1487|        299.7317|  13816.1696|
| 2024|policyengine |  1531|     364|   0.3396|    0.3965|         0.9203|          0.9313|          0.1372|        286.8728|  -2928.4705|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 2506|
| 2017|TRUE        |3 deductions     | 2057|
| 2017|TRUE        |6 other credits  |   53|
| 2017|TRUE        |7 rate/rounding  |  389|
| 2017|FALSE       |1 state AGI      | 5162|
| 2017|FALSE       |3 deductions     |  184|
| 2017|FALSE       |6 other credits  |   96|
| 2017|FALSE       |7 rate/rounding  |  120|
| 2018|TRUE        |1 state AGI      | 2499|
| 2018|TRUE        |3 deductions     | 2122|
| 2018|TRUE        |4 taxable income |  494|
| 2018|FALSE       |1 state AGI      | 5112|
| 2018|FALSE       |3 deductions     |  170|
| 2018|FALSE       |4 taxable income |  122|
| 2019|TRUE        |1 state AGI      | 2382|
| 2019|TRUE        |3 deductions     | 2058|
| 2019|TRUE        |4 taxable income |  473|
| 2019|FALSE       |1 state AGI      | 5127|
| 2019|FALSE       |3 deductions     |  156|
| 2019|FALSE       |4 taxable income |  125|
| 2020|TRUE        |1 state AGI      | 2419|
| 2020|TRUE        |3 deductions     | 1804|
| 2020|TRUE        |4 taxable income |  443|
| 2020|FALSE       |1 state AGI      | 5204|
| 2020|FALSE       |3 deductions     |  178|
| 2020|FALSE       |4 taxable income |  163|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-----|:------|--------:|--------:|:------------------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                      |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                 |
|NC    |taxsim |     2017|     2017|external-model-bug |annotate |TAXSIM omits the upper AGI cutoff on the TY2017 NC credit for children (G.S. 105-153.10: $100/child up to $100k MFJ / $80k HoH / $50k single-MFS), granting $100-125 per child at any income (record-verified to $1.58M AGI). Small tail: 7 of 976 credit-mass records sit above the statutory cutoff; the other 969 were OUR gap (2017 credit now encoded, test NC-5)                                                                                                                                                                                                                                   |
|NC    |taxsim |     2017|     2020|input-coverage     |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate |

