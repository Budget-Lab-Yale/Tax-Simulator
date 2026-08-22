# Cross-model validation: LA

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6606|   0.4223|    0.5398|         0.8542|          0.9625|          0.1169|         62.9447|    349.0283|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    8846|   0.5093|    0.6393|         0.7754|          0.8966|          0.1105|         12.9069|   -369.3184|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    8767|   0.5012|    0.6393|         0.7718|          0.8959|          0.1102|         14.7845|   -495.1716|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    8447|   0.4854|    0.6193|         0.7696|          0.8944|          0.1090|         20.1308|   -375.8310|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.3055|    0.4633|         0.6061|          0.7328|          0.0870|        121.3073|   2568.2126|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.5890|    0.7555|         0.8797|          0.9717|          0.0947|          0.1961|    264.1344|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     470|   0.5609|    0.7338|         0.8723|          0.9681|          0.0942|          0.6079|    264.9444|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     463|   0.5661|    0.7139|         0.8898|          0.9784|          0.0887|          2.2867|    -93.4602|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  308|
| 2017|TRUE        |2 exemptions     |  449|
| 2017|TRUE        |3 deductions     |   41|
| 2017|TRUE        |4 taxable income |  193|
| 2017|FALSE       |1 state AGI      | 3795|
| 2017|FALSE       |2 exemptions     | 5609|
| 2017|FALSE       |3 deductions     | 2453|
| 2017|FALSE       |4 taxable income |  216|
| 2017|FALSE       |5 state EITC     |   90|
| 2018|TRUE        |1 state AGI      |  701|
| 2018|TRUE        |2 exemptions     | 1128|
| 2018|TRUE        |3 deductions     |  131|
| 2018|TRUE        |4 taxable income |  295|
| 2018|FALSE       |1 state AGI      | 3510|
| 2018|FALSE       |2 exemptions     | 4193|
| 2018|FALSE       |3 deductions     | 1447|
| 2018|FALSE       |4 taxable income |  256|
| 2018|FALSE       |5 state EITC     |   70|
| 2019|TRUE        |1 state AGI      |  694|
| 2019|TRUE        |2 exemptions     | 1168|
| 2019|TRUE        |3 deductions     |  122|
| 2019|TRUE        |4 taxable income |  299|
| 2019|FALSE       |1 state AGI      | 3564|
| 2019|FALSE       |2 exemptions     | 4301|
| 2019|FALSE       |3 deductions     | 1398|
| 2019|FALSE       |4 taxable income |  257|
| 2019|FALSE       |5 state EITC     |  151|
| 2020|TRUE        |1 state AGI      |  691|
| 2020|TRUE        |2 exemptions     | 1102|
| 2020|TRUE        |3 deductions     |  115|
| 2020|TRUE        |4 taxable income |  315|
| 2020|FALSE       |1 state AGI      | 3961|
| 2020|FALSE       |2 exemptions     | 4194|
| 2020|FALSE       |3 deductions     | 1404|
| 2020|FALSE       |4 taxable income |  270|
| 2020|FALSE       |5 state EITC     |  160|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |both   |     2017|     2024|structural     |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

