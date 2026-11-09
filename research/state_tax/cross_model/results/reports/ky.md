# Cross-model validation: KY

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    6618|   0.4814|    0.6264|         0.7477|          0.7815|          0.2480|         19.5344|   -309.4978|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    8906|   0.5499|    0.6301|         0.7622|          0.8198|          0.2238|          1.9807|   -257.4559|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    8837|   0.5479|    0.6196|         0.7598|          0.8094|          0.2262|          1.8634|   -452.3427|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    8514|   0.5469|    0.6086|         0.7555|          0.7933|          0.2163|          1.8520|   -325.9014|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.4462|    0.5367|         0.8182|          0.8623|          0.2073|         48.4057|     14.6670|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     424|   0.4645|    0.5434|         0.7783|          0.8184|          0.2007|         46.2968|    137.4296|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     469|   0.4287|    0.5091|         0.7313|          0.7697|          0.1884|         88.5811|    197.7603|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.4313|    0.5078|         0.7130|          0.7565|          0.1730|         89.1908|   -196.3377|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  486|
| 2017|TRUE        |3 deductions    | 1061|
| 2017|TRUE        |6 other credits |  173|
| 2017|FALSE       |1 state AGI     | 8467|
| 2017|FALSE       |3 deductions    | 1822|
| 2017|FALSE       |6 other credits |  166|
| 2018|TRUE        |1 state AGI     | 1340|
| 2018|TRUE        |3 deductions    |  866|
| 2018|TRUE        |6 other credits |  216|
| 2018|FALSE       |1 state AGI     | 7359|
| 2018|FALSE       |3 deductions    | 1070|
| 2018|FALSE       |6 other credits |  249|
| 2019|TRUE        |1 state AGI     | 1411|
| 2019|TRUE        |3 deductions    |  795|
| 2019|TRUE        |6 other credits |  239|
| 2019|FALSE       |1 state AGI     | 7440|
| 2019|FALSE       |3 deductions    | 1025|
| 2019|FALSE       |6 other credits |  306|
| 2020|TRUE        |1 state AGI     | 1376|
| 2020|TRUE        |3 deductions    |  768|
| 2020|TRUE        |6 other credits |  253|
| 2020|FALSE       |1 state AGI     | 7476|
| 2020|FALSE       |3 deductions    | 1020|
| 2020|FALSE       |6 other credits |  354|

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

