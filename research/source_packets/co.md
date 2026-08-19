# Colorado State Source Packet

State: `CO`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-13`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; source packet normalized; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published values are encoded through 2025 or
  2026 where noted; later trigger-dependent credits are not treated as an
  official forecast.
- Resident DR 0104 baseline. Part-year/nonresident allocation, TABOR refunds,
  property-tax/rent/heat rebate, and most specialized credits are outside
  scope.
- Major features: rolling federal-taxable-income start, additions and
  subtractions, flat rate, refundable EITC/child credits, and filing rule.

## Primary sources

- [2025 DR 0104 filing booklet](https://tax.colorado.gov/sites/tax/files/documents/Book104_2025.pdf)
  for the return, DR 0104AD, and credit schedules.
- [DR 0104 return page](https://tax.colorado.gov/DR0104) for the resident
  filing requirement and current return links.
- [Colorado individual income tax guide](https://tax.colorado.gov/individual-income-tax-guide)
  for current DOR guidance.
- Historical DR 0104, DR 0104AD, DR 0104CR, and DR 0104CN forms cited in the
  YAML are the controlling evidence for the 2017-2024 series.

## Parameter inventory

- `agi.yaml`: federal taxable-income start; municipal-interest, U.S.
  obligation, Social Security, pension, charitable, and overtime adjustments.
- `ded.yaml`: no independent state deduction, but federal-deduction-related
  addbacks and limits flow through the federal-taxable starting point.
- `ord.yaml`: annual flat rate, including TABOR rate changes.
- `credits.yaml`: refundable EITC, tiered child credit, and Family
  Affordability Tax Credit.
- `filing.yaml`: federal filing requirement or state liability.

## Worksheet tests

- Federal-taxable base with charitable subtraction.
- SALT addback and Prop FF limitation.
- Pension/Social Security changes before and after 2022.
- Refundable EITC, child credit, and Family Affordability Tax Credit.

## Known differences

- The child-care-expenses and low-income child-care credits are not encoded.
- The 2022-23 child-credit calculation uses a documented proportional proxy
  for the federal credit attributable to under-six children.
- Family Affordability Tax Credit's stepped rules are represented by a linear
  phaseout, and later trigger-dependent years remain zero until published.
- ITIN and younger childless EITC eligibility, retirement ownership, and the
  full set of DR 0104AD adjustments are only partially observable in the PUF.

## Batch role and validation

- Anchor for the `CO / ND / SC` federal-taxable-income cohort. All three let
  federal deductions flow through the starting point before state additions
  and subtractions. Idaho and Oregon do not belong here: their 2025 resident
  returns begin from federal AGI and reconstruct state deductions.
- Cross-model: compare 2017, 2023, and 2025 DR 0104 cases, especially
  deduction addbacks, retirement subtractions, and refundable credits.
- Aggregate: reconcile returns, liability, EITC, and CTC/FATC totals to DOR
  data and SOI HT2 after state weights are available.
