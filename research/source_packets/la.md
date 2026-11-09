# State Source Packet: Louisiana

State: `LA`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-19`

## Scope

- Tax years covered: 2017-2025
- Baseline only
- Major structural features: an exemption relieved at the LOWEST brackets
  rather than subtracted from income; three rate regimes; a constitutionally
  mandated, uncapped federal income tax deduction through TY2021; an excess
  federal itemized deduction that was narrowed rather than repealed; and a
  child care credit that is two different credits split at $25,000 of AGI

## Primary sources

- LDR Form IT-540 and its instruction booklet, TY2017-TY2025, plus Schedules
  E, F and J and the Refundable Child Care Credit Worksheet
- The complete published LDR tax tables for TY2017-TY2024, parsed cell by
  cell (43,008 cells) to reverse-engineer the computation
- Revenue Information Bulletins 21-032 and 25-012
- La. R.S. Title 47, notably 47:32, 47:44.1, 47:44.2, 47:293, 47:294,
  47:295, 47:297.4, 47:297.8, 47:298 and 47:6104
- Act 395 and Act 134 of the 2021 Regular Session, Act 6 of the 2018 Second
  Extraordinary Session, Act 423 of 2023, and Act 11 of the 2024 Third
  Extraordinary Session

## Parameter inventory by file

### `ord.yaml`

- Encoded: the three rate regimes; the bracket bounds, unchanged and
  unindexed in every year; and the bottom-up exemption rule
- Known approximations: the published tables' $250 band midpoints

### `agi.yaml`

- Encoded: federal-AGI start point; Social Security 100% exempt; the age-65
  retirement exclusion, per person, doubled for TY2025 and indexed from 2026
- Known approximations: the per-spouse ceiling on the exclusion; the named
  Louisiana and federal retirement systems; the separate disability
  exclusion; military pay; the net capital gain deduction

### `ded.yaml`

- Encoded: the uncapped federal income tax deduction with its NIIT addition
  and excess-APTC subtraction, through TY2021; the excess federal itemized
  deduction, on the full federal total through TY2021 and on medical and
  dental alone from TY2022
- Known approximations: the Form 4972 subtraction; the TY2017-19
  foreign-tax-credit add-back election; the school expense deductions

### `exempt.yaml`

- Encoded: the combined personal exemption-standard deduction, as a
  per-return base by status plus $1,000 per dependant, per 65-or-older
  taxpayer and per blind taxpayer, restructured for TY2025 with every add-on
  repealed
- Known approximations: the more-than-eight-exemptions table rule; the
  TY2026 indexed amounts

### `credits.yaml`

- Encoded: the earned income credit at 3.5% then 5%, refundable, with the
  enacted 2031 reversion; both branches of the child care credit, its
  income-conditional refundability, the TY2021 flat decimal and the $25
  backstop
- Known approximations: the School Readiness star multiplier; the
  qualifying-provider rules; the five-year carryforward; the LA Citizens
  insurance credit

### `filing.yaml`

- Encoded: the federal-requirement passthrough. There is no Louisiana dollar
  threshold in any year

## New machinery this state required

- `st_ord.exempt_from_bottom` — the exemption relieved at the lowest
  brackets, `tax = sched(income) - sched(exemption)`
- `st_ded.item_less_fed_std` — only the excess of the selected components
  over the federal standard deduction is deductible
- `st_credits.cdctc_style_switch_agi` — above a federal-AGI line the care
  credit switches from the state's own worksheet to a share of the federal
  credit
- `st_credits.cdctc_ref_agi_limit` — refundability conditional on income
- `st_credits.cdctc_cap_per_return` — the care-credit cap applied per return
  rather than per qualifying child

## Worksheet tests added

LA-1 the bottom-up exemption rule; LA-2 the uncapped federal income tax
deduction; LA-3 the per-dependant add-ons; LA-4 head of household taking the
joint exemption and the single brackets; LA-5 the TY2025 flat rate with no
dependant allowance; LA-6a and LA-6b the retirement exclusion doubling;
LA-7a and LA-7b the earned income credit stepping to 5%; LA-8a and LA-8b the
excess federal itemized deduction narrowed to medical and dental; LA-9 the
refundable care credit off the state worksheet; LA-10 the TY2021 flat .50
decimal; LA-11a and LA-11b the nonrefundable share and the $25 backstop.

## Research findings worth flagging

- **`tax = sched(TTI) - sched(E)`, not `sched(TTI - E)`.** All 43,008 cells
  of the published tables for TY2017-TY2024 reproduce exactly under the first
  form and not the second. R.S. 47:32(A)(1), 47:294 and 47:295(B) require the
  exemption to be "deducted from the lowest tax bracket first and then the
  remaining brackets in increasing order", which LDR states in RIB 21-032
  footnote 5. A TY2021 single filer with one exemption and $27,625 of tax
  table income owes $765, where the naive form gives $675. One consequence
  worth noting: when the exemption exceeds the first bracket, which only
  happens for joint and head-of-household filers with several exemptions, the
  excess is relieved at the SECOND bracket rate.
- **The excess federal itemized deduction was NARROWED, not repealed.**
  Nearly every secondary summary says Act 395 repealed it alongside the
  federal income tax deduction. It narrowed the base to medical and dental
  only, and that version is still on the TY2025 return. Line 8A, the itemized
  total, is still collected but no longer enters the arithmetic.
- **Head of household is a hybrid**: the JOINT exemption base of $9,000 with
  add-ons starting after ONE exemption, and the SINGLE brackets. Verified
  against 1,344 head-of-household table cells per year. From TY2025 it takes
  the full $25,000.
- **From TY2025 there is no dependant allowance of any kind.** Act 11 raised
  the base amounts and repealed every $1,000 add-on, for dependants, age and
  blindness alike.
- **The constitutional amendment did not eliminate the federal income tax
  deduction.** Amendment No. 2 of November 13, 2021 (223,269 to 189,973) made
  it permissive and set a 4.75% rate ceiling; Act 395 did the repealing.
- **TY2021 only, the refundable care credit's AGI decimal table collapses to
  a single .50 row**, tracking the one-year ARPA expansion of the federal
  credit. Hard-coding the sliding scale understates that year by a third.
- Act 11 repealed the R.S. 47:32.1 automatic rate-reduction triggers, so
  unlike Mississippi or North Carolina there is no contingent future rate
  path. The 3% flat rate is the enacted end state.
- The net capital gain deduction (Schedule E code 20E) survives for sales
  perfected before January 1, 2025 despite RIB 25-012 describing it as
  repealed.
- Louisiana runs its education relief as DEDUCTIONS, not credits; the
  R.S. 47:297(D) education credit was sunset by Act 375 of 2017 and is
  unavailable from TY2017 onward.

## Known differences

- **The named retirement systems are the largest gap.** Schedule E codes 02E
  to 05E exempt 100% of LASERS, Teachers' Retirement, federal retirement
  including the military survivor benefit plan, and a long list of other
  Louisiana systems — school employees, State Police, municipal employees and
  police, parochial employees, firefighters, Assessors, Clerks of Court,
  District Attorneys, Registrars of Voters and Sheriffs. In a state where
  public employment is a large share of the retiree population this
  understates the exclusion materially, and none of these systems is
  separable from pension income in the model.
- The age-65 exclusion is per person against that person's own retirement
  income; the model pools the couple's income against the summed cap.
- The School Readiness Credit multiplies the child care credit by a Quality
  Start star rating — 2.0 at five stars down to nothing at one. The star
  rating of a household's provider is not a model input, so the credit is
  understated for three-star-and-above facilities and can be half its true
  value at five stars.
- The school expense deductions ($5,000 per dependant through TY2023,
  $6,000 from TY2024) run on private school tuition, uniforms and supplies,
  none of which are model inputs.
- The separate $6,000 permanent-and-total-disability exclusion, the military
  pay exclusion, and the Hurricane Katrina/Rita Schedule H modified federal
  tax deduction are all real and unobservable.
- The above-$25,000 care credit carries forward five years; the model is a
  cross-section and grants no carryforward.
- The federal AMT treatment inside the federal income tax deduction is
  inferred from the worksheet's silence rather than stated.

## Cross-model validation notes

- TAXSIM years 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: every cell will diverge if the external model
  subtracts the exemption before the brackets rather than relieving it at the
  bottom, and that divergence is largest for filers straddling a bracket;
  retiree cells will diverge on the named-system exclusions; TY2017-2021
  cells turn on whether the external model adds NIIT to and subtracts excess
  APTC from the federal tax deduction; and any TY2021 cell with care
  expenses turns on whether it caught the flat .50 table.

## Aggregate validation notes

- HT2 targets once weights land; LDR publishes an annual tax exemption budget
  and individual income tax statistics for a revenue-agency benchmark.

## Unverified

- The TY2026 indexed exemption and retirement-exclusion amounts: the CPI-U
  formula is enacted but the amounts are unpublished, and the statute
  specifies no rounding rule or base period. The encoding indexes with no
  rounding, which is the literal reading; replace with transcriptions when
  LDR publishes.
- Bill numbers HB 278, HB 274 and HB 10 come from search titles rather than
  enrolled PDFs.
- The constitutional text of Art. VII section 4(A) itself was not retrieved.
- The published table rule for returns with more than eight exemptions is
  internally inconsistent in the booklet and was not resolved.
- An "Acts 2025, No. 473, eff. Jan 1 2026" amendment appears in the current
  text of R.S. 47:293. The 2025 session was not surveyed.
- Withholding tables, IT-540B nonresident proration, and estate and trust
  provisions were not surveyed.
