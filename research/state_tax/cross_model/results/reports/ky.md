# Cross-model validation: KY

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.4681|    0.6271|         0.5065|          0.6670|          0.2482|         23.6029|   -227.4972|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.5145|    0.6188|         0.5732|          0.6581|          0.2239|         10.0625|   -177.9475|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.5135|    0.6109|         0.5732|          0.6498|          0.2262|          9.3288|   -383.8730|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.5083|    0.5963|         0.5697|          0.6397|          0.2165|         10.6837|   -255.5166|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     261|   0.4531|    0.5469|         0.8391|          0.8736|          0.2073|         40.5749|   -100.5542|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.4645|    0.5451|         0.8007|          0.8306|          0.1998|         40.0041|     48.9247|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.4313|    0.5143|         0.7500|          0.7827|          0.1884|         83.3285|     94.4645|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.4296|    0.5122|         0.7514|          0.7861|          0.1730|         80.2668|   -288.3524|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 4654|
| 2017|TRUE        |3 deductions    | 2024|
| 2017|TRUE        |6 other credits |  192|
| 2017|FALSE       |1 state AGI     | 5214|
| 2017|FALSE       |3 deductions    |  162|
| 2017|FALSE       |6 other credits |  146|
| 2018|TRUE        |1 state AGI     | 4699|
| 2018|TRUE        |3 deductions    | 1210|
| 2018|TRUE        |6 other credits |  238|
| 2018|FALSE       |1 state AGI     | 5188|
| 2018|FALSE       |3 deductions    |  121|
| 2018|FALSE       |6 other credits |  225|
| 2019|TRUE        |1 state AGI     | 4682|
| 2019|TRUE        |3 deductions    | 1192|
| 2019|TRUE        |6 other credits |  267|
| 2019|FALSE       |1 state AGI     | 5224|
| 2019|FALSE       |3 deductions    |  138|
| 2019|FALSE       |6 other credits |  273|
| 2020|TRUE        |1 state AGI     | 4610|
| 2020|TRUE        |3 deductions    | 1128|
| 2020|TRUE        |6 other credits |  287|
| 2020|FALSE       |1 state AGI     | 5378|
| 2020|FALSE       |3 deductions    |  156|
| 2020|FALSE       |6 other credits |  317|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------|--------:|--------:|:------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|KY    |taxsim |     2017|     2017|external-model-bug |annotate |TAXSIM's 2017 KY combined-return computation deducts TWICE the standard deduction per spouse ($4,960 each; verified by direct WASM probe: 40k/30k wage couple returns siitax 3,096.64 = tax(35,040)+tax(25,040)-$20 credits, vs the Form 740 per-column $2,480 giving 3,384.32); affects every married record in 2017                                                                                                                                                                                                                                                                                                                     |
|KY    |taxsim |     2017|     2024|state-law          |annotate |TAXSIM grants both spouses' standard deductions on KY combined returns unconditionally; Form 740 floors each column at zero, so for one-earner couples (spouse column income below the std ded) TAXSIM runs below form-true tax by up to rate x std (~$130 at 5%; verified: a 70k one-earner couple returns the same siitax as a 40k/30k couple)                                                                                                                                                                                                                                                                                          |
|KY    |both   |     2017|     2024|data-proxy         |annotate |Table C family-size credit uses statutory modified gross income (incl. MFS-spouse income, certain municipal interest, lump-sum adjustments); we use federal AGI plus observable state additions (packet-documented approximation); binds only near Table C band edges at low MGI                                                                                                                                                                                                                                                                                                                                                          |
|KY    |both   |     2017|     2024|data-proxy         |annotate |Combined-return column split assigns non-wage income 50/50 (asset ownership unobserved; VA STA precedent) and divides itemized deductions by income share; actual columns follow ownership/election. Material only under the 2017 graduated schedule and at column floors                                                                                                                                                                                                                                                                                                                                                                 |
|ALL   |both   |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

