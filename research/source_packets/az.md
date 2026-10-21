# Arizona State Source Packet

State: `AZ`  
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-20`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published Form 140 values are transcribed through 2025; the 2.5 percent rate and last published deduction values carry forward.
- Baseline only. Resident individual income tax; no nonresident apportionment.
- Major features: federal-AGI base, municipal-interest addition, full taxable-Social-Security subtraction, 25% net long-term capital gain subtraction, TY2025 subtraction of the federal senior deduction (MCTCP worksheet), independent standard/itemized choice, dependent credit from 2019, the nonitemizer charitable standard-deduction add-on, and the refundable Increased Excise Tax Credit.

## Primary sources

- [Arizona Form 140 booklet landing page](https://azdor.gov/forms/individual/form-140-arizona-resident-personal-income-tax-booklet), including 2017-2025 booklets and X/Y tax tables.
- [2017 Form 140 booklet](https://azdor.gov/sites/default/files/2023-03/FORMS_INDIVIDUAL_2017_140_Booklet.pdf) for pre-TCJA exemptions, deductions, and rates.
- [2019 Form 140 booklet](https://azdor.gov/sites/default/files/2023-03/FORMS_INDIVIDUAL_2019_140Booklet.pdf) for the dependent-credit enactment and charitable standard-deduction increase.
- [2025 Form 140 booklet](https://azdor.gov/sites/default/files/document/FORMS_INDIVIDUAL_2025_140Booklet.pdf) for the current flat rate, standard deduction, and dependent-credit table.
- A.R.S. 43-1022, 43-1023, and 43-1073.01 for additions/subtractions, the repeal of exemptions, and the dependent credit.
- [2021 Form 140 booklet](https://azdor.gov/sites/default/files/2023-03/FORMS_INDIVIDUAL_2021_140BOOKLET.pdf), p.59 Tax Tables X/Y: regular rate 1.00% above $250,000 / $500,000 plus the 3.5% Prop. 208 surcharge (line 46b) = 4.5%. ADOR, [Guidance to taxpayers following Prop 208 ruling](https://azdor.gov/news-events-notices/news/guidance-taxpayers-following-prop-208-ruling) (2022): striking the surcharge does not change overall liability.
- [2023 Form 140 booklet](https://azdor.gov/sites/default/files/2023-12/FORMS_INDIVIDUAL_2023_140Booklet.pdf): lines 22-24 and the p.31 Worksheet for Net Long-Term Capital Gain Subtraction (25%, assets acquired after 2011, short-term items excluded); line 56 Increased Excise Tax Credit worksheet.
- [ADOR Middle Class Tax Cuts Package (MCTCP) Worksheet 2025](https://azdor.gov/sites/default/files/document/FORMS_INDIVIDUAL_2025_MCTCP.pdf): federal Schedule 1-A tips, overtime, vehicle-loan interest and senior deductions subtracted on Form 140 page 6 item V.

## Parameter inventory

- `agi.yaml`: federal AGI start; municipal-interest addition with own-state carve-out; U.S. obligation and Social Security subtraction flags; 25% LT gain subtraction (`cap_gains_excl_share`, `cap_gains_base_lt_only`); TY2025 senior-deduction subtraction (`sub_fed_senior_ded`).
- `ded.yaml`: 2017-2025 standard deductions, independent state election, generic federal-itemized carry-through, and 2019-2025 charitable standard-deduction share.
- `exempt.yaml`: 2017-2018 personal/dependent exemptions; zero from 2019.
- `ord.yaml`: indexed graduated schedules through 2022 and 2.5 percent flat rate from 2023; 2021 integrates the Prop. 208 surcharge with the 1.00% regular rate above $250k/$500k (4.5% total; corrected 2026-10-20 from 8%).
- `credits.yaml`: 2019+ $100/$25 dependent credit (the $100 by age under 17, `dep_credit_young_max_age`) and its $200k/$400k, 5-points-per-$1k phaseout; Increased Excise Tax Credit ($25 per person, $100 cap, AGI <= $12,500/$25,000, refundable).
- `filing.yaml`: conservative federal-filer proxy pending an exact gross-income threshold implementation.

## Worksheet tests

- 2025 single filer: flat rate and standard deduction.
- 2025 standard-deduction charitable add-on.
- 2025 dependent-credit phaseout at $201,000 AGI.
- AZ-X1..X7 (2026-10-15/20): age-65 exemption; 2018 joint-with-dependents exemption; TY2021 $350,000 single = the booklet's own Table X example ($11,029 + $3,500); LT gain subtraction with and without a short-term loss; dependent credit by age; TY2025 senior-deduction subtraction; excise credit cap and income limit.

## Known differences

- Taxable-interest records do not identify U.S. obligation interest, so `sub_us_int` is documented but cannot yet lower liability.
- Tax-exempt interest does not identify Arizona-bond ownership; the generic own-state municipal-interest proxy applies.
- Arizona Schedule A adjustments for charitable contributions that produced Arizona credits are not observable; the generic federal-itemized carry-through is deliberately conservative and transparent.
- Low-income family, property (Form 140PTC), donation, and business credits need additional eligibility inputs and are not encoded. The property tax credit needs rent and property tax paid (`tax_data_gaps.csv`); PolicyEngine pays it in the harness (exclusion row).
- LT gain subtraction: the acquisition date is unobserved, so every long-term gain is treated as post-2011 (an upper bound; `tax_data_gaps.csv`).
- Excise credit: the SSN-valid-for-employment test, incarceration, and the $100 cap shared across separate returns in one household are unobserved.
- TY2025 tips and overtime subtractions (MCTCP lines 2-3) are encodable on `sub_fed_tip_ded` / `sub_fed_ot_ded` but held off pending JI's decision on first state use of the Tax-Data tips and overtime imputations (2026-10-20).
- Exact filing thresholds depend on gross income, age, dependency, and status; the current flag is not an official return-count measure.
- Post-2025 parameters carry the last published values pending annual Form 140 review.

## Cross-model and aggregate validation

- Cross-model: see `results/reports/az.md`. Closed 2026-10-20: TAXSIM 2017-20 0.999/0.994/0.988/0.984; PolicyEngine 2021-25 0.999-1.000 (from 0.946-0.953). External rows: T59, T61 (TAXSIM omits the LT gain subtraction), P45 (PolicyEngine `az_additions` has no formula), the PolicyEngine Form 140PTC exclusion; the TAXSIM 2018-20 residual is the crosswalk itemized class (annotated).
- Aggregate: blocked until state weights land; inspect Arizona HT2 return counts, AGI, and liability distribution, then compare to ADOR annual statistics.
