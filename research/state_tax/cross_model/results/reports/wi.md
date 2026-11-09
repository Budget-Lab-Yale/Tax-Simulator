# Cross-model validation: WI

Class: broad | Generated: 2026-08-22 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       |  8725|    6232|   0.8409|    0.9221|         0.9339|          0.9828|          0.2901|          0.3925|    -88.9520|
| 2017|taxsim       | 11788|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       |  8701|    6225|   0.7212|    0.9230|         0.7957|          0.9807|          0.2871|          0.0752|    -59.5568|
| 2018|taxsim       | 11814|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       |  9131|    6348|   0.8411|    0.9181|         0.9452|          0.9885|          0.2770|          0.2091|     21.2150|
| 2019|taxsim       | 11383|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       |  9836|    6260|   0.8255|    0.9143|         0.9492|          0.9911|          0.2497|          0.3775|     20.1028|
| 2020|taxsim       | 10677|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1163|     363|   0.5039|    0.6561|         0.8485|          0.9146|          0.1307|         14.9962|    637.3391|
| 2021|policyengine |   373|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |  1130|     419|   0.6159|    0.7142|         0.9212|          0.9547|          0.1372|          0.5964|    243.2875|
| 2022|policyengine |   400|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1152|     465|   0.6094|    0.6953|         0.9204|          0.9398|          0.1389|          0.7620|    214.5889|
| 2023|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1144|     459|   0.5822|    0.6748|         0.8954|          0.9259|          0.1311|          1.1649|   -323.4892|
| 2024|policyengine |   387|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage            |    n|
|----:|:-----------|:----------------|----:|
| 2017|TRUE        |1 state AGI      |  432|
| 2017|TRUE        |3 deductions     |   93|
| 2017|TRUE        |4 taxable income |    1|
| 2017|TRUE        |7 rate/rounding  |    5|
| 2017|FALSE       |1 state AGI      | 8019|
| 2017|FALSE       |3 deductions     | 1194|
| 2017|FALSE       |4 taxable income |    1|
| 2017|FALSE       |7 rate/rounding  |   53|
| 2018|TRUE        |1 state AGI      | 1362|
| 2018|TRUE        |3 deductions     |  415|
| 2018|TRUE        |4 taxable income |   75|
| 2018|TRUE        |5 state EITC     |   34|
| 2018|TRUE        |7 rate/rounding  |  651|
| 2018|FALSE       |1 state AGI      | 8044|
| 2018|FALSE       |3 deductions     | 1246|
| 2018|FALSE       |4 taxable income |   25|
| 2018|FALSE       |5 state EITC     |  378|
| 2018|FALSE       |7 rate/rounding  |  130|
| 2019|TRUE        |1 state AGI      |  673|
| 2019|TRUE        |3 deductions     |   64|
| 2019|TRUE        |4 taxable income |   12|
| 2019|TRUE        |5 state EITC     |   32|
| 2019|FALSE       |1 state AGI      | 6479|
| 2019|FALSE       |3 deductions     |  363|
| 2019|FALSE       |4 taxable income |   14|
| 2019|FALSE       |5 state EITC     |  371|
| 2020|TRUE        |1 state AGI      |  583|
| 2020|TRUE        |3 deductions     |   57|
| 2020|TRUE        |4 taxable income |   11|
| 2020|TRUE        |5 state EITC     |   30|
| 2020|FALSE       |1 state AGI      | 6508|
| 2020|FALSE       |3 deductions     |  395|
| 2020|FALSE       |4 taxable income |   11|
| 2020|FALSE       |5 state EITC     |  375|

## Known differences applied

|state |model        | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|WI    |taxsim       |     2017|     2018|external-model-bug |exclude  |TAXSIM's WI 2017 and 2018 bracket thresholds are stale (~3% low; empirical top-bracket entry ~$320,250 MFJ vs the published $329,810; the 2018 schedule returns byte-identical tax to 2017 despite different published thresholds), overtaxing by a flat ~$12.8 in the 6.27% bracket and ~$143.6 in the 7.65% bracket (the -$144 mass, ~1,190/yr; combined with the capital-loss addback it also produces the -$169/-$335 masses). Our schedule matches the published DOR tables to the cent. 2019-2020 vintages are correct (clean 0.92). Excluded via top-bracket membership where the error exceeds the $100 bar                       |
|WI    |policyengine |     2021|     2024|transfer-netting   |exclude  |PE nets the WI homestead credit (rent/property-tax-based Schedule H, in wi_refundable_credits) into wi_income_tax; rent is unobserved in the PUF so the credit is one-sided and household-specific (diffuse mismatches, no point mass). Excluded via predicate on the exported credit                                                                                                                                                                                                                                                                                                                                                     |
|WI    |policyengine |     2021|     2024|structural         |annotate |The diffuse WI PE residual concentrates in itemized-deduction-credit records: our st_item_credit averages $1,852 on clean mismatches vs $51 on matches (2022; two-sided, up to +/-$18k tails). Component proxies inside the 5% credit (medical floor vintages 2017/2019/2020, misc/casualty components) and PE's own itemized-credit modeling differ record-by-record; no point masses. Homestead (separate exclude row) explains only the small pe_wi_homestead > 0 subset — PE cannot compute renter homestead without rent data either                                                                                                 |
|WI    |taxsim       |     2017|     2020|input-coverage     |exclude  |The WI variant of the crosswalk-exposure class: Schedule 1 computes the itemized-deduction credit from federal Schedule A amounts whether or not the filer itemized federally, so both models now compute it for federal standard-deduction takers (2026-08-15 fix), and the 5% credit inherits the crosswalk's component-representation noise (med_pref allocation, unhanded investment interest and "other"). Excluded via the standard exposure predicate                                                                                                                                                                              |
|ALL   |both         |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

