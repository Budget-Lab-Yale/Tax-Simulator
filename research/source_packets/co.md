# Colorado State Source Packet

State: `CO`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

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

## Triage 2026-08-22 — two confirmed causes landed; NOT closed

Colorado moves from 0.9530 / 0.8881 / 0.8829 / 0.8814 to 0.9833 / 0.9444 /
0.9423 / 0.9386. 2017 clears; 2018-2020 do not. Our encoding was not changed,
consistent with the earlier triage that verified no Colorado encoding defects
across seven probe shapes.

### 1. TAXSIM omits the 55-64 pension tier (filed as T19)

C.R.S. 39-22-104(4)(f) subtracts pension and annuity income up to $24,000 at
age 65-plus and up to **$20,000 for ages 55 to 64**. TAXSIM models only the
older tier. Probed on CO 2019 single filers with $20,000 of wages and $30,000
of pension income, varying only age: siitax is **1,701.00 at ages 50, 57, 60
and 64 alike** and drops to 546.75 at 65 and 70. Nothing happens anywhere in
the 55-64 band.

The records agree to the cent: miss modes are -926 / -1,852 in 2017 and -900 /
-1,800 in 2019, which are $20,000 and $40,000 (one and two qualifying people)
at Colorado's 4.63% and 4.50% rates. The mode group is 73 of 76 aged 55-64 with
median retirement income of $44,402 and a $20,013 subtraction on our side. By
age band the clean subset matched at 0.693 for 55-64 against 0.922 for 65-plus
and 0.921 for under-55; among 55-64 filers with retirement income, 0.221.

### 2. The Colorado variant of the crosswalk-exposure class

Colorado has no state itemized deduction, so the `st_itemizing`-keyed predicate
the other class states share never fires here. But C.R.S. 39-22-104(3)(d)
requires federal itemizers to **add back** the state income tax deducted on
Schedule A, and the crosswalk hands TAXSIM as-reported
`salt_inc_sales + salt_pers` inside `otheritem` where nothing can identify them
as state income tax -- so the same unstripped SALT drives an addback instead of
a deduction. Keyed on federal itemizing, as Utah's is. After the pension
exclusion, federal itemizers matched at 0.237 against 0.943 for non-itemizers;
the exposed population is only about 3.5% of the clean subset, so no
materiality bound was needed.

### What is left

2018-2020 sit at ~0.94 and the remaining residual is **diffuse** -- no point
mass survives (the largest is five records), non-itemizers are at 0.943, and
there is no age-band or filing-status concentration left. That is a different
shape from everything resolved here and does not point anywhere yet. Note also
that Colorado has no live PolicyEngine cell to cross-check against: all four
are excluded by the TABOR-netting row, so the TAXSIM window is the only
evidence.

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
