# Indiana State Source Packet

State: `IN`  
Status: `baseline encoded; record-level worksheet tests complete`  
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-2035. State rates are encoded through the enacted 2027 rate; later years carry 2.90 percent pending annual review.
- Resident state IIT only. County income tax, which depends on county of residence on January 1, is outside this state-level baseline.
- Major features: federal-AGI base, broad personal/dependent exemptions, flat state rate, and refundable state EITC.

## Primary sources

- [Indiana current individual forms](https://www.in.gov/dor/tax-forms/individual/current/), including the 2025 IT-40 booklet, Schedule 3, Schedule IN-EIC, and CT-40 county schedule.
- [Indiana Rates, Fees & Penalties](https://www.in.gov/dor/resources/tax-rates-and-reports/rates-fees-and-penalties/) for 2026 and 2027 state rate reductions.
- [Income Tax Information Bulletin #3](https://www.in.gov/dor/files/ib03.pdf) for rate history and exemptions.
- [Indiana EITC credit guidance](https://www.in.gov/dor/i-am-a/individual/tax-credits/) and [IRS state EITC table](https://www.irs.gov/credits-deductions/individuals/earned-income-tax-credit/states-and-local-governments-with-earned-income-tax-credit).

## Parameter inventory

- `agi.yaml`: federal AGI start, municipal-interest addition with own-state carve-out, and statutory interest/Social Security flags.
- `ded.yaml`: no broad standard or itemized deduction.
- `exempt.yaml`: $1,000 personal and $1,500 dependent exemption.
- `ord.yaml`: 3.23 percent through 2022, 3.15 in 2023, then enacted reductions to 2.90 percent in 2027.
- `credits.yaml`: refundable EITC at 9 percent through 2022 and 10 percent from 2023.
- `filing.yaml`: exemption-linked gross-income proxy.

## Worksheet tests

- 2025 single filer with one personal exemption and $1,000 federal EITC: verifies the 3 percent rate and $100 refundable Indiana EITC.

## Known differences

- County income tax is excluded because county January-1 residence is not yet an input; do not interpret `liab_st_iit` as combined state-plus-county liability.
- The $500 elderly/blind exemptions are income-tested and the first-year dependent exemption is $3,000. Both need a generic income-tested exemption feature and dependent-tenure input.
- Indiana-specific renter, 529, adoption, property, and certified credits are not encoded.
- U.S.-obligation and own-state municipal-interest shares are unobserved in the PUF and use shared flags/proxies.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2017, 2023, 2025, and 2027 resident cases against TAXSIM after its Indiana interface is available.
- Aggregate: blocked until state weights land; compare state-only HT2 liability with DOR statistics before separately layering county tax.
