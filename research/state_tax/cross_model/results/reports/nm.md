# Cross-model validation: NM

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16848|    9927|   0.2937|    0.4843|         0.4308|          0.6282|          0.0630|        109.2744|    735.8634|
| 2017|taxsim       |  3665|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16697|    9859|   0.4345|    0.6236|         0.5631|          0.7346|          0.0642|         37.4759|   -381.0276|
| 2018|taxsim       |  3818|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16513|    9907|   0.4069|    0.5802|         0.5347|          0.7025|          0.0624|         52.6000|   -583.2818|
| 2019|taxsim       |  4001|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16529|    9470|   0.4020|    0.5703|         0.5483|          0.7141|          0.0658|         56.4980|   -570.8219|
| 2020|taxsim       |  3984|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1141|     423|   0.3006|    0.5057|         0.4988|          0.7187|          0.0552|         99.4200|   -816.2149|
| 2022|policyengine |   389|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.2394|    0.4564|         0.4530|          0.6838|          0.0337|        124.0000|   -527.8205|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.2678|    0.4530|         0.4739|          0.6717|          0.0322|        120.4718|   -874.3677|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 5640|
| 2017|TRUE        |2 exemptions     |  541|
| 2017|FALSE       |1 state AGI      | 9179|
| 2018|TRUE        |1 state AGI      | 2483|
| 2018|TRUE        |2 exemptions     |   83|
| 2018|TRUE        |3 deductions     |  801|
| 2018|TRUE        |4 taxable income |    9|
| 2018|TRUE        |6 other credits  | 1552|
| 2018|FALSE       |1 state AGI      | 6379|
| 2018|FALSE       |2 exemptions     |   42|
| 2018|FALSE       |3 deductions     |  562|
| 2018|FALSE       |5 state EITC     | 1019|
| 2018|FALSE       |6 other credits  |   79|
| 2019|TRUE        |1 state AGI      | 2610|
| 2019|TRUE        |2 exemptions     |   69|
| 2019|TRUE        |3 deductions     | 1318|
| 2019|TRUE        |4 taxable income |  106|
| 2019|TRUE        |6 other credits  | 1128|
| 2019|FALSE       |1 state AGI      | 6535|
| 2019|FALSE       |2 exemptions     |   46|
| 2019|FALSE       |3 deductions     |  863|
| 2019|FALSE       |4 taxable income |   18|
| 2019|FALSE       |5 state EITC     |  802|
| 2019|FALSE       |6 other credits  |   35|
| 2020|TRUE        |1 state AGI      | 2453|
| 2020|TRUE        |2 exemptions     |   83|
| 2020|TRUE        |3 deductions     | 1198|
| 2020|TRUE        |4 taxable income |   86|
| 2020|TRUE        |6 other credits  | 1097|
| 2020|FALSE       |1 state AGI      | 7084|
| 2020|FALSE       |2 exemptions     |   42|
| 2020|FALSE       |3 deductions     |  810|
| 2020|FALSE       |4 taxable income |   16|
| 2020|FALSE       |5 state EITC     |  730|
| 2020|FALSE       |6 other credits  |   34|

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

