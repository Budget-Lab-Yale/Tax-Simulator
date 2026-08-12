# Rhode Island State Source Packet

State: `RI`
Status: `baseline encoded; worksheet tests RI-1..RI-5 pass`
Last updated: `2026-08-12`

## Scope

- Tax years 2017-2025. Every indexed value is transcribed from the published
  RI-1040 "Tax Rate Schedule and Worksheets" page for that year -- no
  interpolation, no uprating.
- Resident individual income tax only.
- Major structural features: federal-AGI start; **no itemized deduction of any
  kind** (explicit in every year's instructions); ONE graduated
  3.75/4.75/5.99% schedule shared by ALL filing statuses (large marriage
  penalty, no MFS column); own indexed standard deduction and per-exemption
  amount; **both** the standard deduction and the exemptions phase out on a
  stepped 20%-per-increment schedule above one shared threshold, hitting zero
  at 5 increments; refundable EITC match; full-retirement-age + AGI-capped
  Social Security and pension/annuity modifications.

## Primary sources

- **RI Tax Rate Schedule and Worksheets** (the RI-1040 booklet cover page,
  carrying the rate schedule, Standard Deduction Worksheet and Exemption
  Worksheet with all indexed values), verified for EVERY year 2017-2025:
  2017-2020 via the NBER historical mirror, 2021-2025 from tax.ri.gov.
- **RI-1040 Resident Instructions** TY2017-2025 (line 4 deduction chart, line
  6 exemption amount, Schedule M lines 1u/1v/1t, Schedule I CDCTC, Schedule
  EIC percentage, "Who Must File").
- **Modification for Taxable Social Security Income Worksheet** TY2017-2025
  (birth-date gate + filing-status AGI limits).
- **RI Division of Taxation Advisories**: ADV 2019-30, ADV 2020-59,
  ADV 2021-53 (TY2022 inflation adjustments), ADV 2022-40.
- **Statutes**: R.I.G.L. 44-30-2.6 (rate schedule (c)(2)(A), standard
  deduction + phase-out (c)(3)(B)(III), exemption + phase-out (E), allowable
  credits (c)(3)(E), EITC (c)(2)(N)); 44-30-12(c)(8) SS, (c)(9)
  pension/401(k)/annuity, (c)(11) military service pension (2023+),
  (b)(1),(2) out-of-state muni addback, (c)(1) US obligations; 44-30-18
  (credit for tax paid to another state); 44-61-1/44-61-1.1 (bonus
  depreciation / s.179 decoupling); 44-33 (RI-1040H).
- No gaps in 2017-2025: every indexed value is primary. TY2026 values exist
  (ADV 2025-22) but are outside the window and NOT encoded.

## Secondary cross-checks

PolicyEngine US covers 2021-2025 only. Every overlapping value was
cross-checked against the primary worksheets -- rate brackets, standard
deduction (all five statuses including the odd MFS amounts), exemption
amount, phase-out start, phase-out increment, 20% rate, SS limits, pension
limits/cap, EITC match, birth-year gates -- with **ZERO disagreements**. PE
has no pre-2021 values, so 2017-2020 rests on primaries alone. PE
additionally books two items we deliberately exclude (see Known differences).

## Parameter inventory

- `agi.yaml`: start_point 1; rolling conformity; out-of-state muni addback
  with own-state carve-out; US-obligation flag; **`sub_state_ref` 0** (RI
  allows no itemized deduction, so no state-tax deduction was ever taken and
  Schedule M has no refund line); SS modification via `ss_full_sub_allages`
  with the NEW generic `ss_allages_min_age` (66) supplying the
  full-retirement-age gate alongside the AGI cliff; pension modification via
  `pension_excl_*` with `incl_ira` 0 (the instructions restrict it to Form
  1040 line 5b and exclude line 4b IRAs). Both AGI-limit series are
  transcribed independently: they are DISTINCT through TY2021 and identical
  from TY2022.
- `ded.yaml`: own indexed std by status; stepped phase-out via the NEW
  generic `std_po_step` / `std_po_share_per_step` (0.20), mirroring the
  exemption worksheet exactly; `std_po_base` 2 and `po_agi_base` 2 (both
  worksheets read MODIFIED federal AGI, so the SS and pension modifications
  shrink the phase-out measure -- the highest-risk single line in the state);
  `item_allowed` 0.
- `exempt.yaml`: one indexed amount per exemption (taxpayer, spouse, each
  dependent); `dep_filer_zero` 1 (the std deduction is NOT reduced for
  dependent filers); the same stepped 20% phase-out parameters.
- `ord.yaml`: one 3-band schedule for ALL filing statuses (no
  filing_status_mapper) -- the source of RI's large marriage penalty, which
  is correct behavior, not a transcription error.
- `credits.yaml`: 15% refundable EITC rising to 16% in TY2024; 25%
  nonrefundable CDCC.
- `filing.yaml`: `req_type` 3, `req_if_fed_filer` 1.

## Worksheet tests

- RI-1: schedule + own std and exemption, mid-income.
- RI-2: the stepped phase-out of BOTH amounts (2 steps -> share 0.60),
  including the ceiling() rounding.
- RI-2b: past the cliff (5 steps) -- both amounts zero out. Expectation is
  written as continuous arithmetic because the published "Pay" constants are
  rounded to the cent ($7,587.88 for $7,587.875).
- RI-3: SS + pension modifications with the IRA carve-out.
- RI-4: 16% refundable EITC (pins the TY2024 rate change).
- RI-5: 25% nonrefundable care credit.

## Known differences

- **SS modification is all-or-nothing at the unit level.** RI's worksheet
  prorates benefits when only ONE spouse has reached full retirement age;
  we test the primary filer's age. Overstates the subtraction for MFJ
  couples straddling FRA (<= 5.99% of one spouse's taxable SS).
- **Birth-date gate proxied by year-end age 66.** RI's gate advances ~2
  months per year (born on or before 01/01/1952 for TY2017 through
  03/01/1959 for TY2025); 66 is the closest single integer in every year.
  Admits filers born after the cutoff month -- slightly overstates
  eligibility.
- **AGI-limit boundary inclusivity is inverted in both directions** (RI's SS
  test is strict, ours is inclusive; RI's pension test is inclusive, ours is
  strict). Measure-zero; documented only.
- **RI-1040H property-tax relief credit omitted** (Tier-1-blocked: property
  tax and rent unobserved). Understates refunds for 65+/disabled units under
  the household-income limit. PE models it -> expect a systematic PE wedge on
  low-income aged units.
- **2022 child tax rebate omitted** ($250/child, max 3, AGI <= 100k/200k):
  a mailed rebate, not an on-return line, so outside our liability concept
  (P5 class, matching NY/VA/GA/AZ/CT). PE books it in **TY2021** -> expect a
  $250-$750 PE wedge on TY2021 units with qualifying children.
- **US-obligation interest subtraction flagged but not taken** (model-wide).
- Railroad Retirement, military service pension (2023+), 529 contributions,
  bonus-depreciation and s.179 decoupling, fiduciary adjustment: omitted,
  net effect overstates RI AGI.
- **TY2020 ARPA unemployment addback omitted**: RI adds back UI excluded from
  federal AGI; we do not (up to 5.99% x $10,200 ~ $611). Check whether the
  federal side applies the exclusion at all before treating this as live.
- Filing-requirement secondary test (income above std + exemptions) not
  encoded -- zero-liability non-filer counts only.
- TY2026+ not encoded; the RI child credit and millionaire surtax begin
  TY2027.

## Uncertainties

1. **`po_agi_base` / `std_po_base` must be 2, not the schema default of 1.**
   Both RI worksheets read modified federal AGI (line 3), so the SS and
   pension modifications shrink the phase-out measure. Omitting these lines
   would silently phase out seniors who should keep their full deduction and
   exemptions. Highest-risk single line in the state; encoded explicitly.
2. **The SS and pension AGI-limit series are distinct through TY2021 and
   identical from TY2022.** Cross-contaminating them is the year-early anchor
   bug class; both are transcribed independently.
3. **MFS standard deduction EXCEEDS single in TY2023-2024** ($10,025 vs
   $10,000; $10,575 vs $10,550), and the MFS AGI limits carry their own $25
   rounding in several years. Primary-confirmed on both the worksheets and
   the instructions, and PE agrees. Do NOT "correct" these.
4. **`pension_excl_incl_ira = 0` is a substantive reading** resting on "should
   NOT include any amounts for IRAs listed on line 4b". Worth a probe: PE's
   `ri_retirement_income_subtraction` may include IRA distributions, which
   would be a large divergence for IRA-heavy retirees.
5. **The 20% phase-out step contradicts the statute** (44-30-2.6(E)(4)(b)
   reads 2%, and (E)(4)(c) implies status-varying thresholds); the Division's
   published worksheet uses 20% steps and one common threshold in every year
   2017-2025. PE flags the same conflict and also follows the form. We follow
   the form -- noted here so the discrepancy is not rediscovered.
6. The status-invariant rate schedule is genuine (verified on the "FOR ALL
   FILING STATUS TYPES" header in all nine years). If a harness flags RI's
   marriage penalty as anomalous, it is correct behavior.

## Cross-model and aggregate validation

- TAXSIM 2017-2020 (no PE coverage) / PolicyEngine 2021-2024. The two
  systematic PE wedges to pre-register before any run are the RI-1040H
  property-tax credit (low-income aged) and the 2022 child tax rebate booked
  by PE in TY2021 -- both deliberate exclusions, not our-side bugs.
- Aggregate: blocked on weights; benchmark against the RI Division of
  Taxation annual statistical report.
