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

- The retirement subtraction is ENCODED as of 2026-07-23 (elderly survey follow-up; full research in [raw/mi_retirement_research.md](../raw/mi_retirement_research.md)): per-return age bands on the older spouse (new `pension_excl_band_*` family) expressing the birth-year tiers (Tier 1 born <1946: indexed Form 4884 cap, $50,509/$101,019 in 2017 to $65,897/$131,794 in 2025; Tier 2 born 1946-52: $20,000/$40,000) and the PA 4 of 2023 phase-in (25/50/75/100% of the cap for the expanding born-1946-to-1958/-62/-66 windows, printed amounts $15,380/$32,020/$49,423 single; everyone from 2026, floored at age 59 for the IRA-59.5/plan-retirement rules). The senior investment income subtraction (MCL 206.30(1)(p), born <1946, $11,259 to $14,688 single) is also encoded, reduced by the retirement subtraction taken. The Tier-2/3 67+ Michigan Standard Deduction is ALSO properly encoded (same date) as an all-income deduction via the new `senior_std_*` params: $20,000/$40,000 for 67+ older-spouse returns born after 1945, full for Tier-2 ages, netted by taxable SS (suspended 2026-2028 per PA 4) and exemption amounts for Tier-3 ages per the Worksheet 2 netting, mutually exclusive with the Form 4884 subtraction (calculator takes the better). Motivated by cross-model point masses at +$850/+$1,700 (= 20k/40k x 4.25%) on 67+ no-pension records. Tests MI-2..MI-10. Remaining known-differences: public pensions (unlimited for Tier 1) treated as private/capped — source unobservable; 457/TSP/unmatched-401(k) amounts would not qualify but sit in the PUF pension concept; military-pay/military-retirement reductions to the caps and standard deduction unobserved; SSA-exempt add-ons, fire/police (2023+), and surviving-spouse elections unobservable; 2026+ caps carry 2025 published values (CPI-indexed in law).
- Homestead property-tax/rent credit needs household resources, rent, property tax, and disability inputs and is omitted.
- Disabled-veteran, stillbirth, disabled, and other special exemptions are omitted pending eligibility inputs.
- Interest ownership and Michigan-bond shares remain shared PUF proxy limitations.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2017, 2022, 2023, and 2025 cases, emphasizing the EITC expansion and exemption indexation.
- Aggregate: blocked until weights land; reconcile MI HT2 and Department of Treasury annual income-tax revenue, with retirement/homestead differences called out separately.
