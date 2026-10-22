# New York State Source Packet

State: `NY`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-22` (Empire State child credit worksheet limits; cross-model close-out); previous: `2026-10-08`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; source packet normalized; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published state-level parameters are
  transcribed through 2025, with enacted rate changes where documented.
- Full-year resident state IIT baseline only. New York City, Yonkers, MCTMT,
  nonresident allocation, AMT, and most specialized credits are excluded.
- Major features: rolling federal-AGI base, graduated schedule with benefit
  recapture, independently elected pre-TCJA-style itemization, household,
  EITC, child, and dependent-care credits.

## Primary sources

- [2025 IT-201-I instructions](https://www.tax.ny.gov/forms/current-forms/it/it201i.htm)
  for the state return, rate schedule, filing requirement, and credit links.
- [2025 IT-196-I instructions](https://www.tax.ny.gov/forms/html-instructions/2025/it/it196i-2025.htm)
  for independent state itemization and high-income limitations.
- [2025 resident forms index](https://nystax.gov/pit/file/fullyear.htm) for
  IT-201, IT-196, IT-215, IT-216, and related forms.
- Historical annual IT-201-I, IT-196-I, IT-215-I, and IT-216-I instructions
  cited in YAML are the controlling evidence for 2017-2024 values.

## Parameter inventory

- `agi.yaml`: federal AGI start plus New York additions and subtractions.
- `ded.yaml`: standard deduction, independent pre-TCJA-style itemization,
  Pease, high-income, and charitable-only limitations.
- `exempt.yaml`: $1,000 dependent exemption only.
- `ord.yaml`: rate schedules and tax-benefit recapture.
- `credits.yaml`: household credit, EITC interaction, Empire State child
  credit, and dependent-care credit schedules.
- `filing.yaml`: federal-filer rule and New York AGI/additions thresholds.

## Worksheet tests

- High-income rate-benefit recapture.
- Household-credit reduction of state EITC and Empire State child credit.
- Charitable-only itemization and full recapture.
- Enacted 2026 child-credit schedule.
- Empire State child credit worksheet limits: zero-income floor, earned-income limit, proration to children 4-16,
  three-child payroll alternative (NY-6 to NY-9).

## Child care credit base, 2026-10-08

Federal base sweep (does the state credit read the federal credit before or after the federal tax-liability limit, Form 2441 line 9/9c vs line 11?), each read from the state's forms and statute. Tax Law 606(c)(1) (pre-2026 text): a percentage of the section 21 credit "without regard to whether the taxpayer in fact claimed the credit"; IT-216 line 11 is "your eligible federal child and dependent care credit before any federal limitation", computed with NY's own expense caps ($7,500 / $8,500 / $9,000 for 3 / 4 / 5+ persons from TY2018); TY2021 decoupled from ARPA. Now `cdctc_fed_base` = 2 (own recomputation) with the federal decimal for TY2017-2025 and the existing 2026 own-rate parameters year-keyed after it. Had taken shares of the CLAIMED federal credit.

## Empire State child credit worksheet limits, 2026-10-22

TY2017-2024 (style 1) the credit is 33% of the pre-TCJA federal child credit "attributable to qualifying
children", or $100 per qualifying child if greater (the floor only at federal AGI within the threshold). Read from
IT-213 (2019) lines 4-16 and IT-213-I Worksheets A-C:

- Line 4 counts every child under 17 who qualifies for the federal CTC, ACTC or credit for other dependents; line 5
  the children aged 4-16 (0-16 from TY2023). Worksheet A line 1 is $1,000 x line 4, phased out at 5% per $1,000
  (rounded up) of federal AGI over $75,000 / $110,000 / $55,000.
- Worksheet A lines 7-10 limit it to federal tax (Form 1040 line 12b) less Schedule 3 lines 1-4 (foreign tax,
  child care, education and saver's credits) and the elderly credit; Worksheet C adds the additional child tax
  credit -- 15% of earned income over $3,000, or for three or more children the larger of that and Schedule 8812
  line 13 (employee payroll tax less the EITC).
- Lines 9-12 prorate the allowed amount to the line 5 children; line 13 takes 33%. A No on line 3 (AGI over the
  threshold) skips only the $100 floor (lines 14-15).

Encoded as `ctc_fed_limit` (style 1 only). Before this, the calculator took 33% of an unlimited $1,000 per
qualifying child, which paid $330 a child to families with no earnings and no federal tax (PolicyEngine and TAXSIM
both pay the $100 floor there). The federal calculator does not compute the elderly credit, so line 8g is not
subtracted. Worksheet tests NY-6 to NY-9.

## Cross-model close-out, 2026-10-22

Attributed to the external models (issues doc T71-T74, P56-P58):

- T71 TAXSIM pays the child credit for children under 4 before TY2023 (IT-213 line 5 says 4-16).
- T72 TAXSIM pays no child credit above the income threshold, where line 13 still applies.
- T73 TAXSIM counts dependents aged 17+ in the line 9 divisor.
- T74 TAXSIM grants the $20,000 pension and annuity exclusion only from 65 (statute: 59 1/2).
- P56 PE books the one-time 2022 supplemental EITC and supplemental child credit payments (Tax Law 606(d)(8),
  606(c-1)(4)) into TY2021 refundable credits (and the 2023 additional child credit payment into TY2023), with the
  supplemental EITC sized at 25% of the federal rather than the New York credit.
- P57 PE takes the IT-196 line 46 adjustment on deductions before the line 40 overall limitation and then
  subtracts that limitation as well.
- P58 PE caps child care expenses at the federal $6,000 before New York's $7,500-$9,000 caps for three or more
  qualifying persons.
- Crosswalk: the PolicyEngine input has no medical, investment-interest, casualty, miscellaneous or other itemized
  deductions (New York PE input-coverage row, twin of the TAXSIM one).

## Known differences

- NYC, Yonkers, MCTMT, nonresident allocation, AMT, and several refundable or
  specialized credits are intentionally not in the state-IIT result.
- The dependent-care credit's detailed table is represented by interpolated
  anchors; exact table rows remain a follow-up.
- College tuition, noncustodial-parent EITC, and local school-tax credits are
  data-limited or omitted.
- Pre-TCJA itemization uses available component proxies; federal Schedule A
  data do not fully identify every historical New York deduction rule.

## Batch role and validation

- Anchor for the `NY / CT` graduated-federal-AGI, benefit-recapture cohort.
  Connecticut also starts from federal AGI and uses a graduated schedule with
  a high-income calculation layer, but its deductions and credits must remain
  parameterized separately.
- Virginia is a later graduated-federal-AGI batch, not a direct NY peer: it
  combines state standard/itemized deductions, exemptions, and mutually
  exclusive low-income and EITC choices.
- Cross-model: compare 2017, 2024, and 2025 resident returns and check each
  recapture worksheet, IT-215, IT-196, and IT-216 transition.
- Aggregate: after weights land, compare state-only income, liability, EITC,
  and child-credit totals to NY DTF statistics and SOI HT2. Keep local fiscal
  programs outside that comparison.
