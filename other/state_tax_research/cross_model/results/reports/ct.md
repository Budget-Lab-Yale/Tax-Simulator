# Cross-model validation: CT

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 16248|   11696|   0.5394|    0.8311|         0.6565|          0.9292|          0.1483|         11.5081|  -1284.9137|
| 2017|taxsim       |  4265|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2018|taxsim       | 16159|   11686|   0.5343|    0.8288|         0.6474|          0.9290|          0.1463|         11.4765|  -1429.1403|
| 2018|taxsim       |  4356|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2019|taxsim       | 16175|   11644|   0.5050|    0.7923|         0.6177|          0.8945|          0.1474|         14.5377|  -1646.6400|
| 2019|taxsim       |  4339|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2020|taxsim       | 16104|   11181|   0.4952|    0.7599|         0.6241|          0.8797|          0.1506|         15.6657|   -966.4393|
| 2020|taxsim       |  4409|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2021|policyengine |  1536|     270|   0.3242|    0.4290|         0.8444|          0.8815|          0.0931|        182.7708|  -8061.4046|
| 2022|policyengine |  1205|     225|   0.3386|    0.4050|         0.7600|          0.8356|          0.1037|        244.8701|   -985.6695|
| 2022|policyengine |   325|      NA|       NA|        NA|             NA|              NA|              NA|              NA|          NA|
| 2023|policyengine |  1533|     357|   0.3659|    0.4266|         0.8487|          0.8824|          0.1129|        192.9618|  16112.4579|
| 2024|policyengine |  1531|     364|   0.3579|    0.4311|         0.8462|          0.8846|          0.1065|        234.6139|  -6750.2074|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     |  547|
| 2017|TRUE        |2 exemptions    | 1332|
| 2017|TRUE        |6 other credits | 1733|
| 2017|TRUE        |7 rate/rounding |  914|
| 2017|FALSE       |1 state AGI     | 4682|
| 2017|FALSE       |2 exemptions    |  146|
| 2017|FALSE       |5 state EITC    | 1152|
| 2017|FALSE       |6 other credits |   45|
| 2017|FALSE       |7 rate/rounding |   13|
| 2018|TRUE        |1 state AGI     |  614|
| 2018|TRUE        |2 exemptions    | 1325|
| 2018|TRUE        |6 other credits |  862|
| 2018|TRUE        |7 rate/rounding | 1829|
| 2018|FALSE       |1 state AGI     | 4693|
| 2018|FALSE       |2 exemptions    |  139|
| 2018|FALSE       |5 state EITC    | 1117|
| 2018|FALSE       |6 other credits |    6|
| 2018|FALSE       |7 rate/rounding |   43|
| 2019|TRUE        |1 state AGI     | 1293|
| 2019|TRUE        |2 exemptions    | 1262|
| 2019|TRUE        |6 other credits |  699|
| 2019|TRUE        |7 rate/rounding | 1679|
| 2019|FALSE       |1 state AGI     | 4801|
| 2019|FALSE       |2 exemptions    |  153|
| 2019|FALSE       |5 state EITC    | 1151|
| 2019|FALSE       |6 other credits |    6|
| 2019|FALSE       |7 rate/rounding |   38|
| 2020|TRUE        |1 state AGI     | 1324|
| 2020|TRUE        |2 exemptions    | 1147|
| 2020|TRUE        |6 other credits |  637|
| 2020|TRUE        |7 rate/rounding | 1576|
| 2020|FALSE       |1 state AGI     | 5268|
| 2020|FALSE       |2 exemptions    |  136|
| 2020|FALSE       |5 state EITC    | 1061|
| 2020|FALSE       |6 other credits |    8|
| 2020|FALSE       |7 rate/rounding |   50|

## Known differences applied

|state |model        | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:-----|:------------|--------:|--------:|:------------------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                                                                                                                |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                                                                                                                    |
|ALL   |taxsim       |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                                                                                                               |
|ALL   |taxsim       |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                                                                                                               |
|CT    |taxsim       |     2017|     2020|external-model-bug |exclude  |TAXSIM's CT benefit-recapture (Table C) keeps stepping $90 per $5,000 of AGI past the statutory maximum: probe-mapped MFJ 2019 plateau = $6,300 (reached ~$750k) vs the published $5,400 cap at $700,050 -- a flat +$900 on every MFJ return above ~$750k AGI (222 records in 2019, the dominant CT mass). Our plateau matches the published table (pinned by test CT-2 and the continuity sweep). Excluded via the exposure predicate |
|CT    |taxsim       |     2017|     2020|structural         |annotate |Band-edge steps: CT's Table A exemption phase-down, Table B personal-credit percentage, and Table C/D recapture all move in discrete AGI bands; one-band disagreements (AGI rounding/concept) produce small two-sided masses (+/-$20-90, cred_gap at Table B percentage steps). Same class as the declared continuity-sweep allowances                                                                                                 |
|CT    |policyengine |     2022|     2022|transfer-netting   |exclude  |PE books the one-time 2022 CT child tax rebate ($250/child under 18, max 3, HB 5501 s. 411; paid Aug-Sep 2022 on TY2021 dependents by application) into TY2022 state_income_tax via ct_child_tax_rebate. Clean mismatches mass at +250/+500 (2022 dip to 0.773 vs 0.88 neighbors). Same class as issues-doc P5. Excluded via predicate on the exported rebate                                                                          |

