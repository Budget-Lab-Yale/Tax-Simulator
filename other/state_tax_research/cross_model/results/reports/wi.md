# Cross-model validation: WI

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.5348|    0.6141|         0.7163|          0.7597|          0.1379|         12.6585| -42169.4373|
| 2018|taxsim       | 20515|   13144|   0.4043|    0.6064|         0.5160|          0.7578|          0.1362|         19.7587| -43303.0150|
| 2019|taxsim       | 20514|   13088|   0.6629|    0.7548|         0.8878|          0.9207|          0.1412|          0.2748|    727.3790|
| 2020|taxsim       | 20513|   12682|   0.6626|    0.7582|         0.8872|          0.9230|          0.1367|          0.4965|    713.3026|
| 2021|policyengine |  1536|     269|   0.2754|    0.3965|         0.7361|          0.7770|          0.1016|        265.2380| -13183.9109|
| 2022|policyengine |  1530|     317|   0.3294|    0.3915|         0.7256|          0.7382|          0.1092|        258.4898|  -3765.7230|
| 2023|policyengine |  1533|     357|   0.3346|    0.3979|         0.7255|          0.7563|          0.1057|        287.3838|  12897.9404|
| 2024|policyengine |  1531|     364|   0.3122|    0.3658|         0.6951|          0.7198|          0.1039|        359.3109| -12541.1703|

## Mismatch stage diagnosis

Not available: results/raw/*_stages.csv not present on this machine
(raw per-record output is not committed; regenerate with a full
harness run).

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |

