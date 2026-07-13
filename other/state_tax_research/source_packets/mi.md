# Michigan State Source Packet

State: `MI`  
Status: `baseline encoded; record-level worksheet tests complete`  
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-2035. Published exemptions and tax rates are transcribed through 2025; 4.25 percent carries forward pending review.
- Resident individual income tax only. Homestead property-tax credit is not part of this initial liability.
- Major features: federal-AGI base, indexed personal/dependent exemptions, flat rate, and refundable EITC.

## Primary sources

- [2025 Michigan individual income tax guidance](https://www.michigan.gov/taxes/iit/tax-guidance/tax-year-info/tax-year-2025-guidance).
- [2025 MI-1040 instructions](https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT/TY2025/MI-1040-Book.pdf).
- [Michigan EITC guidance](https://www.michigan.gov/taxes/iit/tax-guidance/credits-exemptions/eitc).
- [Michigan retirement and pension guidance](https://www.michigan.gov/taxes/iit/tax-guidance/tax-situations/retirement-and-pension-benefits).

## Parameter inventory

- `agi.yaml`: federal AGI start; municipal/U.S.-obligation interest flags; Social Security subtraction.
- `exempt.yaml`: $4,050 in 2017 through $5,800 in 2025 for each taxpayer and dependent.
- `ord.yaml`: 4.25 percent except for the temporary 4.05 percent rate in 2023.
- `credits.yaml`: refundable EITC at 6 percent through 2021 and 30 percent from 2022.

## Worksheet tests

- 2025 single filer with a $5,800 exemption and $1,000 federal EITC: verifies the 30 percent refundable state EITC.

## Known differences

- The retirement subtraction is cohort-, pension-source-, and election-dependent. It is intentionally omitted, rather than applying a false universal pension exclusion.
- Homestead property-tax/rent credit needs household resources, rent, property tax, and disability inputs and is omitted.
- Disabled-veteran, stillbirth, disabled, and other special exemptions are omitted pending eligibility inputs.
- Interest ownership and Michigan-bond shares remain shared PUF proxy limitations.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2017, 2022, 2023, and 2025 cases, emphasizing the EITC expansion and exemption indexation.
- Aggregate: blocked until weights land; reconcile MI HT2 and Department of Treasury annual income-tax revenue, with retirement/homestead differences called out separately.
