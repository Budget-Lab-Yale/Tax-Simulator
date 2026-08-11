# Cross-model validation: MN

Class: broad | Generated: 2026-08-11 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.3390|    0.4127|         0.4591|          0.5277|          0.0883|        297.6978|  -12117.845|
| 2018|taxsim       | 20515|   13144|   0.4210|    0.5433|         0.5628|          0.6732|          0.0963|         61.2016|   -1934.055|
| 2019|taxsim       | 20514|   13088|   0.3414|    0.4305|         0.4466|          0.5434|          0.0915|        231.9138|   -2224.542|
| 2020|taxsim       | 20513|   12682|   0.3211|    0.3891|         0.4190|          0.4915|          0.0904|        391.4047|   13683.514|
| 2021|policyengine |  1536|     269|   0.2285|    0.3431|         0.7658|          0.8216|          0.0840|        474.7383|  -16551.320|
| 2022|policyengine |  1530|     317|   0.2876|    0.3451|         0.7697|          0.8139|          0.0882|        503.5872|    3258.943|
| 2023|policyengine |  1533|     358|   0.2720|    0.3379|         0.7374|          0.7654|          0.0600|        475.4755|   23729.998|
| 2024|policyengine |  1531|     363|   0.2528|    0.3214|         0.6804|          0.7521|          0.0555|        604.2394|   -3973.101|

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

