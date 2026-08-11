# Cross-model validation: MD

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3448|    0.4585|         0.4795|          0.5825|          0.1219|        146.2471|    5679.199|
| 2018|taxsim       | 20515|   13144|   0.4128|    0.5486|         0.5768|          0.6996|          0.1140|         57.5027|   -4322.327|
| 2019|taxsim       | 20514|   13088|   0.1907|    0.5414|         0.2414|          0.6951|          0.1124|         79.7524|   -4552.270|
| 2020|taxsim       | 20513|   12682|   0.3885|    0.5055|         0.5593|          0.6778|          0.1084|         92.0389|   -5200.108|
| 2021|policyengine |  1536|     268|   0.2350|    0.3672|         0.7724|          0.8396|          0.0736|        267.4091|  -10223.631|
| 2022|policyengine |  1530|     314|   0.3085|    0.4033|         0.8217|          0.8885|          0.1072|        212.6278|    1308.637|
| 2023|policyengine |  1533|     357|   0.3007|    0.4070|         0.8347|          0.8880|          0.0946|        192.1692|   15452.477|
| 2024|policyengine |  1531|     363|   0.2926|    0.3912|         0.8237|          0.8788|          0.0836|        232.4775|   -3098.667|

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

