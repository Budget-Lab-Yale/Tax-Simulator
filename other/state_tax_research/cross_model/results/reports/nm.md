# Cross-model validation: NM

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.2506|    0.4101|         0.3463|          0.5092|          0.0600|        160.0000|    5872.756|
| 2018|taxsim       | 20515|   13144|   0.3436|    0.5256|         0.4741|          0.6551|          0.0636|         81.7938|   -2865.493|
| 2019|taxsim       | 20514|   13088|   0.3209|    0.4882|         0.4494|          0.6237|          0.0614|        108.8723|   -3321.250|
| 2020|taxsim       | 20513|   12682|   0.3170|    0.4778|         0.4543|          0.6258|          0.0633|        115.7931|   -4124.842|
| 2021|policyengine |  1536|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1530|     317|   0.1405|    0.2549|         0.3249|          0.5710|          0.0497|        428.9848|   11503.613|
| 2023|policyengine |  1533|     357|   0.1207|    0.2296|         0.3557|          0.5826|          0.0261|        450.0851|   20923.407|
| 2024|policyengine |  1531|     364|   0.1385|    0.2410|         0.3681|          0.5769|          0.0255|        486.2045|    4946.402|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      | 8017|
| 2017|TRUE        |2 exemptions     |  541|
| 2017|FALSE       |1 state AGI      | 6814|
| 2018|TRUE        |1 state AGI      | 3447|
| 2018|TRUE        |2 exemptions     |   84|
| 2018|TRUE        |3 deductions     | 1771|
| 2018|TRUE        |4 taxable income |   17|
| 2018|TRUE        |6 other credits  | 1593|
| 2018|FALSE       |1 state AGI      | 5253|
| 2018|FALSE       |2 exemptions     |   40|
| 2018|FALSE       |3 deductions     |  140|
| 2018|FALSE       |5 state EITC     | 1044|
| 2018|FALSE       |6 other credits  |   78|
| 2019|TRUE        |1 state AGI      | 3399|
| 2019|TRUE        |2 exemptions     |   74|
| 2019|TRUE        |3 deductions     | 2392|
| 2019|TRUE        |4 taxable income |  156|
| 2019|TRUE        |6 other credits  | 1185|
| 2019|FALSE       |1 state AGI      | 5374|
| 2019|FALSE       |2 exemptions     |   42|
| 2019|FALSE       |3 deductions     |  440|
| 2019|FALSE       |4 taxable income |   16|
| 2019|FALSE       |5 state EITC     |  822|
| 2019|FALSE       |6 other credits  |   31|
| 2020|TRUE        |1 state AGI      | 3368|
| 2020|TRUE        |2 exemptions     |   85|
| 2020|TRUE        |3 deductions     | 2208|
| 2020|TRUE        |4 taxable income |  120|
| 2020|TRUE        |6 other credits  | 1139|
| 2020|FALSE       |1 state AGI      | 5831|
| 2020|FALSE       |2 exemptions     |   41|
| 2020|FALSE       |3 deductions     |  419|
| 2020|FALSE       |4 taxable income |   12|
| 2020|FALSE       |5 state EITC     |  757|
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

