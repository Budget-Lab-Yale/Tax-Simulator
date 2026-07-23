# Cross-model validation: ID

Class: broad | Generated: 2026-07-23 | Verdict: **NEEDS REVIEW**

Acceptance: match@$100 >= 95% in every canonical-window cell
(2017-2020 TAXSIM, 2021+ PolicyEngine), on the clean subset where
defined (federally aligned records; see README).

## Cell summary

| year|model        |     n| n_clean| match_15| match_100| match_15_clean| match_100_clean| share_both_zero| median_abs_diff| mean_signed|
|----:|:------------|-----:|-------:|--------:|---------:|--------------:|---------------:|---------------:|---------------:|-----------:|
| 2017|taxsim       | 20513|   13092|   0.4375|    0.4958|         0.5409|          0.5896|               0|        108.4625|   12022.838|
| 2018|taxsim       | 20515|   13144|   0.5188|    0.5848|         0.6531|          0.7080|               0|         10.0000|   -9180.538|
| 2019|taxsim       | 20514|   13088|   0.5197|    0.5856|         0.6566|          0.7084|               0|         10.0000|   -9282.338|
| 2020|taxsim       | 20513|   12682|   0.5146|    0.5804|         0.6486|          0.6989|               0|         10.0000|  -10551.209|
| 2021|policyengine |  1536|     269|   0.1263|    0.2461|         0.3792|          0.5242|               0|        348.5537|  -20675.182|
| 2022|policyengine |  1530|     316|   0.1386|    0.2627|         0.3734|          0.5601|               0|        345.9000|  -10297.229|
| 2023|policyengine |  1533|     357|   0.1448|    0.2120|         0.4034|          0.4314|               0|        350.0000|    2183.934|
| 2024|policyengine |  1531|     364|   0.1600|    0.2560|         0.4368|          0.5824|               0|        370.8090|  -15742.635|

## Mismatch stage diagnosis (TAXSIM |diff| > $15)

| year|fed_aligned |stage       |    n|
|----:|:-----------|:-----------|----:|
| 2017|TRUE        |1 state AGI | 6010|
| 2017|FALSE       |1 state AGI | 5528|
| 2018|TRUE        |1 state AGI | 4559|
| 2018|FALSE       |1 state AGI | 5313|
| 2019|TRUE        |1 state AGI | 4495|
| 2019|FALSE       |1 state AGI | 5358|
| 2020|TRUE        |1 state AGI | 4456|
| 2020|FALSE       |1 state AGI | 5500|

## Known differences applied

|state |model  | year_min| year_max|category       |action   |description                                                                                                                                                                                                                         |
|:-----|:------|--------:|--------:|:--------------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM optimizes federal itemization using its own computed state income tax (SALT circularity) and iterates federal-state 3 rounds; our pass is one-way federal-to-state until Phase 7                                             |
|ALL   |taxsim |     2017|     2024|structural     |annotate |TAXSIM imputes the sales-tax deduction from IRS Pub. 600 regressions; we use as-reported salt_inc_sales                                                                                                                             |
|ALL   |taxsim |     2021|     2024|vintage        |annotate |TAXSIM state law 2021+ is inflated ~2020 law, not enacted law; cells in this window are non-canonical (PolicyEngine is the tie-breaker)                                                                                             |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no tax-exempt interest input, so it can never apply state exempt-interest addbacks (or count exempt_int in the federal EITC investment-income test); records with exempt_int > 0 are outside the clean-subset metrics |
|ALL   |taxsim |     2017|     2024|input-coverage |annotate |TAXSIM-35 has no state-refund input, so state-mode crosswalk omits state_ref entirely (states subtract their own refunds); TAXSIM federal AGI runs low by state_ref, handled inside the fed_aligned flag                            |
|ALL   |taxsim |     2017|     2024|federal-side   |annotate |State EITCs piggyback on federal EITC; TAXSIM's own federal EITC (amount and eligibility) can differ from ours, propagating scaled differences into state EITC; clean-subset metrics condition on federal EITC agreement            |


## Triage notes (2026-07-23, initial run)

- **PBF**: symmetric +/-$10 point masses (~810 each side, 2019) — both
  models levy the $10 PBF with different required-to-file edges. Annotate
  row added.
- **CTC**: -$205 point mass (~75/yr, 2018+) — we grant where TAXSIM
  doesn't; dependent-age concept difference. Annotate row added.
- **Grocery credit**: TAXSIM models it (no credit-sized point masses;
  cor(diff, credit) ~ 0). PolicyEngine does NOT net it into
  state_income_tax: median mismatch = minus our per-person credit in every
  amount bin (2023). This poisons the whole PE window (every resident
  carries the credit) — verify in PE package source, then either compare
  against a pre-credit PE variable or exclude the window (CO/TABOR
  precedent). The PE cells' 43-58% clean rates should be read through
  this lens.
- **Fed-taxable deduction-stack wedge** (large, high-income, sign flips at
  TCJA: +12k mean 2017, -9k 2018+): same v32-semantics family as CO/ND/SC;
  our schedules are pinned to the published forms by worksheet tests ID-1,
  ID-2, ID-7. Next step: regress the wedge on itemized components and QBI
  as done for CO.
