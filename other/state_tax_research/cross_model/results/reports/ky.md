# Cross-model validation: KY

Class: broad | Generated: 2026-08-15 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4033|    0.5457|         0.4844|          0.6380|          0.2136|         59.4382|    703.2760|
| 2018|taxsim       | 20515|   13144|   0.4483|    0.5360|         0.5573|          0.6292|          0.1927|         50.4375|    608.0534|
| 2019|taxsim       | 20514|   13088|   0.4409|    0.5264|         0.5536|          0.6195|          0.1927|         68.4658|    398.8768|
| 2020|taxsim       | 20513|   12682|   0.4389|    0.5163|         0.5496|          0.6093|          0.1847|         78.4337|    400.6564|
| 2021|policyengine |  1536|     269|   0.3652|    0.4362|         0.8327|          0.8550|          0.1641|        187.5671|  -6715.0090|
| 2022|policyengine |  1530|     317|   0.3680|    0.4366|         0.7950|          0.8139|          0.1595|        187.6457|   4011.7933|
| 2023|policyengine |  1533|     357|   0.3438|    0.4103|         0.7395|          0.7731|          0.1455|        228.0263|  14577.8501|
| 2024|policyengine |  1531|     364|   0.3423|    0.4108|         0.7445|          0.7747|          0.1391|        222.6048|    153.6406|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage           |    n|
|----:|:-----------|:---------------|----:|
| 2017|TRUE        |1 state AGI     | 2851|
| 2017|TRUE        |3 deductions    | 3689|
| 2017|TRUE        |6 other credits |  210|
| 2017|FALSE       |1 state AGI     | 5103|
| 2017|FALSE       |3 deductions    |  240|
| 2017|FALSE       |6 other credits |  147|
| 2018|TRUE        |1 state AGI     | 3143|
| 2018|TRUE        |3 deductions    | 2413|
| 2018|TRUE        |6 other credits |  263|
| 2018|FALSE       |1 state AGI     | 5097|
| 2018|FALSE       |3 deductions    |  171|
| 2018|FALSE       |6 other credits |  231|
| 2019|TRUE        |1 state AGI     | 3043|
| 2019|TRUE        |3 deductions    | 2491|
| 2019|TRUE        |6 other credits |  309|
| 2019|FALSE       |1 state AGI     | 5156|
| 2019|FALSE       |3 deductions    |  189|
| 2019|FALSE       |6 other credits |  282|
| 2020|TRUE        |1 state AGI     | 3076|
| 2020|TRUE        |3 deductions    | 2306|
| 2020|TRUE        |6 other credits |  330|
| 2020|FALSE       |1 state AGI     | 5267|
| 2020|FALSE       |3 deductions    |  201|
| 2020|FALSE       |6 other credits |  329|

## Known differences applied

|state |model  | year_min| year_max|category           |action   |description                                                                                                                                                                                                                                                                                                                                      |
|:-----|:------|--------:|--------:|:------------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                                                                                                                                          |
|ALL   |taxsim |     2017|     2024|structural         |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                                                                                                                                          |
|ALL   |taxsim |     2021|     2024|vintage            |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                                                                                                                                          |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics                                                                                                              |
|ALL   |taxsim |     2017|     2024|input-coverage     |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                                                                                                                                         |
|ALL   |taxsim |     2017|     2024|federal-side       |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement                                                                                                                         |
|KY    |taxsim |     2017|     2017|external-model-bug |annotate |TAXSIM's 2017 KY combined-return computation deducts TWICE the standard deduction per spouse ($4,960 each; verified by direct WASM probe: 40k/30k wage couple returns siitax 3,096.64 = tax(35,040)+tax(25,040)-$20 credits, vs the Form 740 per-column $2,480 giving 3,384.32); affects every married record in 2017                            |
|KY    |taxsim |     2017|     2024|state-law          |annotate |TAXSIM grants both spouses' standard deductions on KY combined returns unconditionally; Form 740 floors each column at zero, so for one-earner couples (spouse column income below the std ded) TAXSIM runs below form-true tax by up to rate x std (~$130 at 5%; verified: a 70k one-earner couple returns the same siitax as a 40k/30k couple) |
|KY    |both   |     2017|     2024|data-proxy         |annotate |Table C family-size credit uses statutory modified gross income (incl. MFS-spouse income, certain municipal interest, lump-sum adjustments); we use federal AGI plus observable state additions (packet-documented approximation); binds only near Table C band edges at low MGI                                                                 |
|KY    |both   |     2017|     2024|data-proxy         |annotate |Combined-return column split assigns non-wage income 50/50 (asset ownership unobserved; VA STA precedent) and divides itemized deductions by income share; actual columns follow ownership/election. Material only under the 2017 graduated schedule and at column floors                                                                        |

