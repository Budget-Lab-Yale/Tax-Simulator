# Cross-model validation: CT

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4850|    0.7403|         0.6543|          0.9005|          0.1174|         16.6961|  -2955.5180|
| 2018|taxsim       | 20515|   13144|   0.4819|    0.7401|         0.6477|          0.9014|          0.1152|         17.0860|  -2086.3235|
| 2019|taxsim       | 20514|   13088|   0.4598|    0.7090|         0.6231|          0.8714|          0.1162|         21.1352|  -2719.1851|
| 2020|taxsim       | 20513|   12682|   0.4537|    0.6862|         0.6307|          0.8587|          0.1183|         23.1384|  -1515.1259|
| 2021|policyengine |  1536|     270|   0.3242|    0.4290|         0.8444|          0.8815|          0.0931|        182.7708|  -8061.4046|
| 2022|policyengine |  1530|     317|   0.3248|    0.3869|         0.7066|          0.7729|          0.1157|        250.2527|   -583.9237|
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

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

