# New York State Source Packet

State: `NY`
Status: `baseline encoded; source packet normalized; record-level worksheet tests complete`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-2035. Published state-level parameters are
  transcribed through 2025, with enacted rate changes where documented.
- Full-year resident state IIT baseline only. New York City, Yonkers, MCTMT,
  nonresident allocation, AMT, and most specialized credits are excluded.
- Major features: rolling federal-AGI base, graduated schedule with benefit
  recapture, independently elected pre-TCJA-style itemization, household,
  EITC, child, and dependent-care credits.

## Primary sources

- [2025 IT-201-I instructions](https://www.tax.ny.gov/forms/current-forms/it/it201i.htm)
  for the state return, rate schedule, filing requirement, and credit links.
- [2025 IT-196-I instructions](https://www.tax.ny.gov/forms/html-instructions/2025/it/it196i-2025.htm)
  for independent state itemization and high-income limitations.
- [2025 resident forms index](https://nystax.gov/pit/file/fullyear.htm) for
  IT-201, IT-196, IT-215, IT-216, and related forms.
- Historical annual IT-201-I, IT-196-I, IT-215-I, and IT-216-I instructions
  cited in YAML are the controlling evidence for 2017-2024 values.

## Parameter inventory

- `agi.yaml`: federal AGI start plus New York additions and subtractions.
- `ded.yaml`: standard deduction, independent pre-TCJA-style itemization,
  Pease, high-income, and charitable-only limitations.
- `exempt.yaml`: $1,000 dependent exemption only.
- `ord.yaml`: rate schedules and tax-benefit recapture.
- `credits.yaml`: household credit, EITC interaction, Empire State child
  credit, and dependent-care credit schedules.
- `filing.yaml`: federal-filer rule and New York AGI/additions thresholds.

## Worksheet tests

- High-income rate-benefit recapture.
- Household-credit reduction of state EITC and Empire State child credit.
- Charitable-only itemization and full recapture.
- Enacted 2026 child-credit schedule.

## Known differences

- NYC, Yonkers, MCTMT, nonresident allocation, AMT, and several refundable or
  specialized credits are intentionally not in the state-IIT result.
- The dependent-care credit's detailed table is represented by interpolated
  anchors; exact table rows remain a follow-up.
- College tuition, noncustodial-parent EITC, and local school-tax credits are
  data-limited or omitted.
- Pre-TCJA itemization uses available component proxies; federal Schedule A
  data do not fully identify every historical New York deduction rule.

## Batch role and validation

- Anchor for the `NY / CT` graduated-federal-AGI, benefit-recapture cohort.
  Connecticut also starts from federal AGI and uses a graduated schedule with
  a high-income calculation layer, but its deductions and credits must remain
  parameterized separately.
- Virginia is a later graduated-federal-AGI batch, not a direct NY peer: it
  combines state standard/itemized deductions, exemptions, and mutually
  exclusive low-income and EITC choices.
- Cross-model: compare 2017, 2024, and 2025 resident returns and check each
  recapture worksheet, IT-215, IT-196, and IT-216 transition.
- Aggregate: after weights land, compare state-only income, liability, EITC,
  and child-credit totals to NY DTF statistics and SOI HT2. Keep local fiscal
  programs outside that comparison.
