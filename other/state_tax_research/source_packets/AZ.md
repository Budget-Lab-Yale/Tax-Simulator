# Arizona State Source Packet

State: `AZ`  
Status: `baseline encoded; record-level worksheet tests complete`  
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-2035. Published Form 140 values are transcribed through 2025; the 2.5 percent rate and last published deduction values carry forward.
- Baseline only. Resident individual income tax; no nonresident apportionment.
- Major features: federal-AGI base, municipal-interest addition, full taxable-Social-Security subtraction, independent standard/itemized choice, dependent credit from 2019, and the nonitemizer charitable standard-deduction add-on.

## Primary sources

- [Arizona Form 140 booklet landing page](https://azdor.gov/forms/individual/form-140-arizona-resident-personal-income-tax-booklet), including 2017-2025 booklets and X/Y tax tables.
- [2017 Form 140 booklet](https://azdor.gov/sites/default/files/2023-03/FORMS_INDIVIDUAL_2017_140_Booklet.pdf) for pre-TCJA exemptions, deductions, and rates.
- [2019 Form 140 booklet](https://azdor.gov/sites/default/files/2023-03/FORMS_INDIVIDUAL_2019_140Booklet.pdf) for the dependent-credit enactment and charitable standard-deduction increase.
- [2025 Form 140 booklet](https://azdor.gov/sites/default/files/document/FORMS_INDIVIDUAL_2025_140Booklet.pdf) for the current flat rate, standard deduction, and dependent-credit table.
- A.R.S. 43-1022, 43-1023, and 43-1073.01 for additions/subtractions, the repeal of exemptions, and the dependent credit.

## Parameter inventory

- `agi.yaml`: federal AGI start; municipal-interest addition with own-state carve-out; U.S. obligation and Social Security subtraction flags.
- `ded.yaml`: 2017-2025 standard deductions, independent state election, generic federal-itemized carry-through, and 2019-2025 charitable standard-deduction share.
- `exempt.yaml`: 2017-2018 personal/dependent exemptions; zero from 2019.
- `ord.yaml`: indexed graduated schedules through 2022 and 2.5 percent flat rate from 2023; 2021 integrates the Prop. 208 surcharge in the top marginal rate.
- `credits.yaml`: 2019+ $100/$25 dependent credit and its $200k/$400k, 5-points-per-$1k phaseout.
- `filing.yaml`: conservative federal-filer proxy pending an exact gross-income threshold implementation.

## Worksheet tests

- 2025 single filer: flat rate and standard deduction.
- 2025 standard-deduction charitable add-on.
- 2025 dependent-credit phaseout at $201,000 AGI.

## Known differences

- Taxable-interest records do not identify U.S. obligation interest, so `sub_us_int` is documented but cannot yet lower liability.
- Tax-exempt interest does not identify Arizona-bond ownership; the generic own-state municipal-interest proxy applies.
- Arizona Schedule A adjustments for charitable contributions that produced Arizona credits are not observable; the generic federal-itemized carry-through is deliberately conservative and transparent.
- Low-income family, excise, property, donation, and business credits need additional eligibility inputs and are not encoded.
- Exact filing thresholds depend on gross income, age, dependency, and status; the current flag is not an official return-count measure.
- Post-2025 parameters carry the last published values pending annual Form 140 review.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2019, 2022, and 2025 resident cases after the next TAXSIM run is available.
- Aggregate: blocked until state weights land; inspect Arizona HT2 return counts, AGI, and liability distribution, then compare to ADOR annual statistics.
