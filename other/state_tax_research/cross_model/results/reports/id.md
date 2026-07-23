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
- **Fed-taxable deduction wedge — RESOLVED to the itemizer deduction
  rebuild (2026-07-23 dive).** Liability diffs are taxable-income-level
  end to end: cor(diff, taxable wedge x v41 bracket rate) = 1.000 both
  eras. Non-itemizers agree (84.5% of no-QBID non-itemizers at wedge = 0
  in 2019; 93.6% in 2017; v33 exemptions and v34 standard deduction match
  ours to dollars) — the base machinery, schedules, and zero brackets are
  confirmed. The wedge lives on ITEMIZERS with opposite signs by era:
  - 2018-2020 (ours lower): TAXSIM's state itemized rebuild (v35) runs a
    median $21.8k below our (item_ded − line-14 addback) and TAXSIM flips
    36% of our itemizers to the standard deduction — consistent with
    removing its full computed state income tax (SALT circularity)
    instead of the Form 40 line 14 capped property-first-fill formula
    (our encoding matches the published worksheet; test ID-3).
  - 2017 (ours higher): our pre-cap addback = the reported income/sales
    component capped at itemized-over-standard; TAXSIM removes less
    (computed-state-tax concept, possibly Pease-prorated) — item wedge
    cor −0.745 with the SALT income component, p10 −$95k.
  - QBID hypothesis REFUTED: TAXSIM includes QBID (median QBID
    non-itemizer wedge/QBID ratio 0.035); a ~10% subset shows wedge =
    −QBID exactly (suspected wage/SSTB-limit divergence) — small
    annotate row.
  - Residual non-itemizer cluster: care-expense records 3.6x
    over-represented, median wedge +$1.8k — TAXSIM allows more dependent
    care than the Form 39R $3,000/$6,000 worksheet caps (possibly the
    statutory-$12,000 reading of 63-3022D that appears on no published
    worksheet). Candidate upstream issue.
  - Pension-age records are UNDER-represented in the residual — TAXSIM
    does not model the 63-3022A retirement-benefits deduction either, so
    our documented omission is not a cross-model wedge.
