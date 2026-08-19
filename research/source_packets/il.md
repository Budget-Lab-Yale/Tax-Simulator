# Illinois State Source Packet

State: `IL`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-13`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; source packet normalized; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published parameters are transcribed through
  2026 where available; later exemption amounts use the documented CPI proxy.
- Resident IL-1040 baseline. Nonresident allocation, credits for tax paid to
  other states, and business-credit schedules are outside this baseline.
- Major features: rolling federal-AGI base, flat tax, no state standard or
  itemized deduction, retirement and Social Security subtractions, exemptions,
  refundable EITC/child credit, and nonrefundable property-tax credit.

## Primary sources

- [2025 IL-1040 instructions](https://tax.illinois.gov/forms/incometax/currentyear/individual/il-1040-instr.html)
  for the return structure, filing rule, rate, exemptions, and credits.
- [2025 Schedule M instructions](https://tax.illinois.gov/forms/incometax/currentyear/individual/il-1040-schedule-m-instr.html)
  for federal-AGI additions and subtractions.
- [2025 Schedule ICR instructions](https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-schedule-icr-instr.pdf)
  for property-tax and K-12 credit eligibility.
- [2025 Schedule IL-E/EITC instructions](https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-schedule-il-e-eic-instr.pdf)
  for the refundable EITC and child-credit implementation.
- Historical annual IL-1040 instructions and the cited statutes in the YAML
  files are the source of the 2017-2024 time series. Preserve year-specific
  forms when revising a historical value.

## Parameter inventory

- `agi.yaml`: federal AGI start; municipal-interest addback proxy; U.S.
  obligation, Social Security, retirement-income, and state-refund
  subtractions.
- `ded.yaml`: deliberately zero standard and itemized deductions.
- `exempt.yaml`: personal/dependent, age, and blind exemptions plus the
  statutory high-income cliff.
- `ord.yaml`: 2017 blended rate, then the 4.95 percent flat rate.
- `credits.yaml`: refundable federal-EITC match, Illinois child credit, and
  property-tax-credit parameters.
- `filing.yaml`: federal-filing or Illinois-base-over-exemption requirement.

## Worksheet tests

- Basic single filer and the 2017 midyear blended rate.
- Retirement/Social Security subtraction, exemptions, property credit, and
  refundable EITC.
- $250,000/$500,000 exemption and property-credit cliffs.

## Known differences

- `exempt_int` does not identify Illinois-obligation interest, so the all-muni
  addback is an explicit proxy.
- Property-tax credit eligibility requires Illinois principal-residence and
  prior-year payment information that the PUF does not fully identify.
- K-12 education, other-state tax, business, and household-specific credits
  are not calculated; their YAML metadata must not be read as active
  calculator support.
- The expanded EITC's ITIN and age eligibility cannot be fully replicated from
  available data.

## Batch role and validation

- Anchor for the `IL / IN / MI` rolling-federal-AGI, flat-rate validation
  cohort. Do not merge Pennsylvania into this group: its class-income base and
  local taxes are structurally different.
- Cross-model: compare 2017, 2024, and 2025 IL-1040 resident cases against a
  form calculation or TAXSIM where coverage matches.
- Aggregate: once weights land, compare returns, net liability, EITC, and
  property-tax-credit totals to Illinois DOR statistics and SOI HT2.
