# Cross-model validation: NM

Class: broad | Generated: 2026-08-16 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|   12092|   0.2868|    0.4761|         0.3594|          0.5357|          0.0631|        116.1918|    804.1226|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|   12037|   0.3864|    0.6060|         0.4821|          0.6843|          0.0643|         50.9911|   -302.1407|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|   11930|   0.3674|    0.5666|         0.4609|          0.6540|          0.0625|         62.7050|   -515.3084|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|   11502|   0.3611|    0.5530|         0.4687|          0.6590|          0.0658|         67.3800|   -501.0763|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     301|   0.1700|    0.3076|         0.3189|          0.5681|          0.0535|        275.0030|    751.5921|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     336|   0.1435|    0.2844|         0.3423|          0.5774|          0.0311|        282.0283|    539.6817|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     346|   0.1730|    0.2991|         0.3757|          0.5954|          0.0304|        313.5781|    385.8421|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 8121|
| 2017|TRUE        |2 exemptions     |  541|
| 2017|FALSE       |1 state AGI      | 6817|
| 2018|TRUE        |1 state AGI      | 4815|
| 2018|TRUE        |2 exemptions     |   82|
| 2018|TRUE        |3 deductions     |  777|
| 2018|TRUE        |4 taxable income |    9|
| 2018|TRUE        |6 other credits  | 1552|
| 2018|FALSE       |1 state AGI      | 5341|
| 2018|FALSE       |2 exemptions     |   38|
| 2018|FALSE       |3 deductions     |  106|
| 2018|FALSE       |5 state EITC     | 1012|
| 2018|FALSE       |6 other credits  |   76|
| 2019|TRUE        |1 state AGI      | 4874|
| 2019|TRUE        |2 exemptions     |   69|
| 2019|TRUE        |3 deductions     | 1296|
| 2019|TRUE        |4 taxable income |  105|
| 2019|TRUE        |6 other credits  | 1127|
| 2019|FALSE       |1 state AGI      | 5429|
| 2019|FALSE       |2 exemptions     |   39|
| 2019|FALSE       |3 deductions     |  401|
| 2019|FALSE       |4 taxable income |   13|
| 2019|FALSE       |5 state EITC     |  798|
| 2019|FALSE       |6 other credits  |   31|
| 2020|TRUE        |1 state AGI      | 4747|
| 2020|TRUE        |2 exemptions     |   83|
| 2020|TRUE        |3 deductions     | 1177|
| 2020|TRUE        |4 taxable income |   83|
| 2020|TRUE        |6 other credits  | 1097|
| 2020|FALSE       |1 state AGI      | 5935|
| 2020|FALSE       |2 exemptions     |   39|
| 2020|FALSE       |3 deductions     |  381|
| 2020|FALSE       |4 taxable income |    8|
| 2020|FALSE       |5 state EITC     |  727|
| 2020|FALSE       |6 other credits  |   30|

## Known differences applied

|state |model        | year_min| year_max|category         |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:-----|:------------|--------:|--------:|:----------------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|structural       |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2021|     2024|vintage          |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|ALL   |taxsim       |     2017|     2024|input-coverage   |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|federal-side     |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|NM    |policyengine |     2021|     2021|transfer-netting |exclude  |PE books all THREE of New Mexico's one-time 2021 rebates into TY2021 state_income_tax: nm_2021_income_rebate ($250), nm_additional_2021_income_rebate ($500) and nm_supplemental_2021_income_rebate ($500), doubled for joint filers. These were mailed checks (Laws 2021 ch.4 and the 2021 special session), not credits claimed on the PIT-1, so we correctly do not model them. Verified 2026-08-13 by probing PE directly: a zero-income NM household shows nm_refundable_credits = 1,445 = 250 + 500 + 500 + LICTR in 2021, while from 2022 nm_refundable_credits equals LICTR alone even though the rebate variables still compute values -- which is why this row stops at 2021 |
|NM    |policyengine |     2021|     2024|omitted-credit   |annotate |Low Income Comprehensive Tax Rebate (NMSA 7-2-14; PIT-RC Table 1), up to $819 refundable, keyed to a 25-band MODIFIED GROSS INCOME table crossed with exemption count. MGI adds TANF, SSI, general assistance, child support, gifts, inheritances, VA benefits and scholarships and forbids netting losses -- none observable, and it is not a tax-unit concept. PE models it (nm_low_income_comprehensive_tax_rebate) and nets it into state_income_tax in every year. Annotated rather than excluded: it is an OUR-SIDE omission documented in baseline/nm/agi.yaml, and excluding it would drop most low-income NM records from the denominator                                     |
|ALL   |both         |     2017|     2024|structural       |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding                                              |

