# Washington Source Packet

State: `WA`
Status: `encoded_initial_validation`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-forward.
- Profile: `capital_gains_and_transfer`.
- Encoded programs: long-term capital-gains excise tax from 2022 and Working
  Families Tax Credit (WFTC) from 2022.

## Primary Sources

- Washington DOR, [Capital Gains Tax](https://dor.wa.gov/taxes-rates/other-taxes/capital-gains-tax).
- [RCW 82.87.040](https://app.leg.wa.gov/RCW/default.aspx?cite=82.87.040), capital-gains tax rates.
- [RCW 82.87.050](https://app.leg.wa.gov/RCW/default.aspx?cite=82.87.050), standard deduction.
- Washington [Working Families Tax Credit eligibility](https://workingfamiliescredit.wa.gov/eligibility) and [RCW 82.08.0206](https://app.leg.wa.gov/Rcw/default.aspx?cite=82.08.0206).

## Encoded Parameters

- Capital gains: 7% from 2022, with a 2.9% surcharge above $1 million of
  taxable Washington capital gains from 2025. Standard deductions are
  $250,000, $262,000, $270,000, and $278,000 for 2022-2025.
- WFTC: exact 2022-2025 maximum amounts, income limits, investment-income
  limits, phaseout widths, $50 minimum, and 2023 MFS eligibility change.
- Amounts after 2025 intentionally hold at 2025 values pending the annual DOR
  update; they are not an official forecast.

## Worksheet Tests

- 2024 capital-gains tax above the standard deduction.
- 2025 additional 2.9% capital-gains rate.
- 2022 one-child WFTC phaseout and 2025 childless age eligibility.

## Known Differences

- `kg_lt` is a broad proxy for Washington taxable capital gains. The PUF lacks
  asset-level exclusions, Washington allocation, qualified-family-business
  deductions, and tax credits.
- WFTC uses observable EITC-like eligibility variables and assumes full
  take-up. Residence days, SSN/ITIN status, foreign/nonresident rules, and
  actual application status are not observed.
- WFTC is recorded as a standalone refundable transfer, not as broad IIT.

## Next Validation

- Spot-check recent capital-gains and WFTC results against Washington DOR
  examples or PolicyEngine where its rules match the tax year.
- Reconcile weighted capital-gains receipts and WFTC outlays to Washington
  reports after state weights are available.
