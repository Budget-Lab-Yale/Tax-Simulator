# Cross-model validation: NC

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  9167|    6400|   0.9071|    0.9467|         0.9816|          0.9938|          0.3155|          0.0020|   -126.0077|
| 2017|taxsim       | 11346|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  9138|    6528|   0.8503|    0.9442|         0.9151|          0.9940|          0.3089|          0.0026|    -36.2756|
| 2018|taxsim       | 11377|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9083|    6483|   0.8623|    0.9528|         0.9218|          0.9971|          0.3254|          0.0024|    -12.2952|
| 2019|taxsim       | 11431|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9345|    6343|   0.8492|    0.9426|         0.9212|          0.9962|          0.3187|          0.0025|    -34.7290|
| 2020|taxsim       | 11168|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.5700|    0.7108|         0.9311|          0.9669|          0.1843|          1.6891|    744.7034|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.6591|    0.7415|         0.9598|          0.9740|          0.2121|          0.0050|    163.1168|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.6249|    0.7122|         0.9338|          0.9573|          0.1997|          0.3203|    217.2839|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.6157|    0.6965|         0.9283|          0.9457|          0.1800|          0.3083|   -228.7815|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  166|
| 2017|TRUE        |3 deductions     |   58|
| 2017|TRUE        |6 other credits  |   30|
| 2017|FALSE       |1 state AGI      | 7653|
| 2017|FALSE       |3 deductions     | 1344|
| 2017|FALSE       |6 other credits  |   71|
| 2018|TRUE        |1 state AGI      |  923|
| 2018|TRUE        |3 deductions     |  255|
| 2018|TRUE        |4 taxable income |  485|
| 2018|FALSE       |1 state AGI      | 7164|
| 2018|FALSE       |3 deductions     | 1116|
| 2018|FALSE       |4 taxable income |  150|
| 2019|TRUE        |1 state AGI      |  892|
| 2019|TRUE        |3 deductions     |  211|
| 2019|TRUE        |4 taxable income |  440|
| 2019|FALSE       |1 state AGI      | 7208|
| 2019|FALSE       |3 deductions     | 1009|
| 2019|FALSE       |4 taxable income |  151|
| 2020|TRUE        |1 state AGI      |  833|
| 2020|TRUE        |3 deductions     |  148|
| 2020|TRUE        |4 taxable income |  419|
| 2020|FALSE       |1 state AGI      | 7112|
| 2020|FALSE       |3 deductions     | 1001|
| 2020|FALSE       |4 taxable income |  190|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|NC    |taxsim |     2017|     2017|external-model-bug |annotate |TAXSIM omits the upper AGI cutoff on the TY2017 NC credit for children (G.S. 105-153.10: $100/child up to $100k MFJ / $80k HoH / $50k single-MFS), granting $100-125 per child at any income (record-verified to $1.58M AGI). Small tail: 7 of 976 credit-mass records sit above the statutory cutoff; the other 969 were OUR gap (2017 credit now encoded, test NC-5)                                                                                                                                                                                                                                                                    |
|NC    |taxsim |     2017|     2020|input-coverage     |exclude  |Crosswalk representation of the state itemized base (the DC/CA class): the crosswalk hands TAXSIM as-reported salt_inc_sales + salt_pers inside otheritem, where no state calculation can identify them as SALT to strip (TAXSIM strips its own iterated state tax instead), and investment interest and Schedule A "other" have no TAXSIM inputs at all. The 2026-08-15 state-only-itemization fix extends the exposed population to federal standard-deduction takers, who under this state's independent election now itemize state-side in both models. Excluded via the standard exposure predicate                                  |
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

