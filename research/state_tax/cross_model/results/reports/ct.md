# Cross-model validation: CT

Class: broad | Generated: 2026-08-21 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 14881|   11192|   0.5525|    0.8619|         0.6516|          0.9360|          0.1503|         10.8922|    -81.7190|
| 2017|taxsim       |  5632|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 14707|   11130|   0.5470|    0.8609|         0.6429|          0.9362|          0.1463|         11.0979|     26.3712|
| 2018|taxsim       |  5808|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 14638|   11035|   0.5181|    0.8263|         0.6123|          0.9031|          0.1482|         13.3347|     53.7715|
| 2019|taxsim       |  5876|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 14582|   10613|   0.5077|    0.7928|         0.6185|          0.8882|          0.1522|         14.1440|     76.2630|
| 2020|taxsim       |  5931|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1172|     363|   0.5555|    0.7261|         0.9421|          0.9862|          0.1143|          3.2394|   1033.9006|
| 2021|policyengine |   364|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2022|policyengine |   843|     298|   0.6856|    0.7639|         0.9463|          0.9765|          0.1400|          0.2317|    471.4811|
| 2022|policyengine |   687|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1157|     468|   0.6716|    0.7416|         0.9615|          0.9701|          0.1469|          0.3236|    440.5281|
| 2023|policyengine |   376|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2024|policyengine |  1150|     460|   0.6643|    0.7496|         0.9500|          0.9826|          0.1330|          0.2762|   -171.6829|
| 2024|policyengine |   381|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2679|
| 2017|TRUE        |2 exemptions    | 1160|
| 2017|TRUE        |6 other credits |  968|
| 2017|TRUE        |7 rate/rounding |  526|
| 2017|FALSE       |1 state AGI     | 4954|
| 2017|FALSE       |2 exemptions    |  137|
| 2017|FALSE       |5 state EITC    | 1112|
| 2017|FALSE       |6 other credits |   10|
| 2017|FALSE       |7 rate/rounding |    3|
| 2018|TRUE        |1 state AGI     | 2819|
| 2018|TRUE        |2 exemptions    | 1112|
| 2018|TRUE        |6 other credits |  659|
| 2018|TRUE        |7 rate/rounding |  912|
| 2018|FALSE       |1 state AGI     | 4939|
| 2018|FALSE       |2 exemptions    |  127|
| 2018|FALSE       |5 state EITC    | 1083|
| 2018|FALSE       |6 other credits |    2|
| 2018|FALSE       |7 rate/rounding |   21|
| 2019|TRUE        |1 state AGI     | 3472|
| 2019|TRUE        |2 exemptions    | 1071|
| 2019|TRUE        |6 other credits |  534|
| 2019|TRUE        |7 rate/rounding |  756|
| 2019|FALSE       |1 state AGI     | 5028|
| 2019|FALSE       |2 exemptions    |  145|
| 2019|FALSE       |5 state EITC    | 1116|
| 2019|FALSE       |6 other credits |    5|
| 2019|FALSE       |7 rate/rounding |   10|
| 2020|TRUE        |1 state AGI     | 3377|
| 2020|TRUE        |2 exemptions    | 1004|
| 2020|TRUE        |6 other credits |  499|
| 2020|TRUE        |7 rate/rounding |  698|
| 2020|FALSE       |1 state AGI     | 5554|
| 2020|FALSE       |2 exemptions    |  128|
| 2020|FALSE       |5 state EITC    | 1020|
| 2020|FALSE       |6 other credits |    5|
| 2020|FALSE       |7 rate/rounding |   11|

## Known differences applied

|state |model        | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:-----|:------------|--------:|--------:|:------------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                                                                                                                                                                                                                       |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|ALL   |taxsim       |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                                                                                                                                                                                                                                  |
|CT    |taxsim       |     2017|     2020|external-model-bug |exclude  |TAXSIM's CT benefit-recapture (Table C) keeps stepping $90 per $5,000 of AGI past the statutory maximum: probe-mapped MFJ 2019 plateau = $6,300 (reached ~$750k) vs the published $5,400 cap at $700,050 -- a flat +$900 on every MFJ return above ~$750k AGI (222 records in 2019, the dominant CT mass). Our plateau matches the published table (pinned by test CT-2 and the continuity sweep). Excluded via the exposure predicate                                                                                                                                                                                                    |
|CT    |taxsim       |     2017|     2020|structural         |annotate |Band-edge steps: CT's Table A exemption phase-down, Table B personal-credit percentage, and Table C/D recapture all move in discrete AGI bands; one-band disagreements (AGI rounding/concept) produce small two-sided masses (+/-$20-90, cred_gap at Table B percentage steps). Same class as the declared continuity-sweep allowances                                                                                                                                                                                                                                                                                                    |
|CT    |policyengine |     2022|     2022|transfer-netting   |exclude  |PE books the one-time 2022 CT child tax rebate ($250/child under 18, max 3, HB 5501 s. 411; paid Aug-Sep 2022 on TY2021 dependents by application) into TY2022 state_income_tax via ct_child_tax_rebate. Clean mismatches mass at +250/+500 (2022 dip to 0.773 vs 0.88 neighbors). Same class as issues-doc P5. Excluded via predicate on the exported rebate                                                                                                                                                                                                                                                                             |
|ALL   |both         |     2017|     2024|structural         |exclude  |US-obligation interest is exempt from state tax in every state (31 U.S.C. 3124); the model subtracts an assumed US_OBLIGATION_INT_SHARE (15%) of taxable interest for states encoding sub_us_int, because the source split is unobserved in the PUF. Neither TAXSIM (no input) nor PolicyEngine (us_govt_interest input not handed; split equally unobservable) takes the subtraction, so records where the assumed subtraction is large enough to break the match tolerance (above roughly $5,000 of taxable interest at top state rates) cannot agree with either external model. The divergence is the assumption, not either encoding |

