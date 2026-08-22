# Cross-model validation: AZ

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.1887|    0.5408|         0.2338|          0.5845|          0.0059|         83.5473|    768.9568|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.2314|    0.6725|         0.2788|          0.7150|          0.0045|         60.9821|     77.3854|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.3359|    0.7802|         0.4028|          0.8309|          0.0108|         26.2945|   -152.8138|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.3210|    0.7525|         0.3965|          0.8276|          0.0111|         34.6673|    -29.2219|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |   761|     244|   0.2116|    0.6675|         0.3607|          0.8934|          0.0000|         50.0000|  46831.7388|
| 2021|policyengine |   775|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     421|   0.3401|    0.7283|         0.5226|          0.9287|          0.0272|         43.6659|   4975.5569|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     465|   0.3224|    0.7234|         0.5032|          0.9118|          0.0354|         46.2947|   2640.7546|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     458|   0.3409|    0.7183|         0.5393|          0.9127|          0.0287|         41.6848|   5232.4995|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 5437|
| 2017|TRUE        |2 exemptions    | 3396|
| 2017|TRUE        |3 deductions    | 1043|
| 2017|TRUE        |6 other credits |  366|
| 2017|FALSE       |1 state AGI     | 5901|
| 2017|FALSE       |2 exemptions    |  542|
| 2017|FALSE       |3 deductions    |  591|
| 2017|FALSE       |6 other credits |   17|
| 2018|TRUE        |1 state AGI     | 5282|
| 2018|TRUE        |2 exemptions    | 3190|
| 2018|TRUE        |3 deductions    |  893|
| 2018|TRUE        |6 other credits |  373|
| 2018|FALSE       |1 state AGI     | 5722|
| 2018|FALSE       |2 exemptions    |  532|
| 2018|FALSE       |3 deductions    |  554|
| 2018|FALSE       |6 other credits |   13|
| 2019|TRUE        |1 state AGI     | 4761|
| 2019|TRUE        |2 exemptions    | 1280|
| 2019|TRUE        |3 deductions    | 1785|
| 2019|TRUE        |6 other credits |  412|
| 2019|FALSE       |1 state AGI     | 5584|
| 2019|FALSE       |2 exemptions    |   52|
| 2019|FALSE       |3 deductions    |  968|
| 2019|FALSE       |6 other credits |   21|
| 2020|TRUE        |1 state AGI     | 4679|
| 2020|TRUE        |2 exemptions    | 1300|
| 2020|TRUE        |3 deductions    | 1689|
| 2020|TRUE        |6 other credits |  423|
| 2020|FALSE       |1 state AGI     | 6115|
| 2020|FALSE       |2 exemptions    |   53|
| 2020|FALSE       |3 deductions    |  856|
| 2020|FALSE       |6 other credits |   19|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:----------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|AZ    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books the one-time Arizona Families Tax Rebate (SB 1734 / Laws 2023 ch. 147: $250 per dependent under 17, $100 per 17+ dependent, max three, requiring >= $1 of 2021 liability; paid fall 2023 on TY2021 returns) into TAX YEAR 2021 as az_families_tax_rebate. Clean mismatches mass at +250/+500/+750. The separately-tracked 2021 mean_abs_diff ~ $819k is unrelated: ~20 giant-AGI records (up to $27M diffs), ALL fed_aligned == FALSE, already quarantined by the clean metrics. Excluded via predicate on the exported rebate                                                                                                   |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

