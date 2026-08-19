# State Source Packet: Montana

State: `MT`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-19`

## Scope

- Tax years covered: 2017-2025, with enacted rates noted through TY2027
- Baseline only
- Major structural features: two regimes either side of SB 399 (2021),
  effective TY2024; a seven-bracket ladder with STATUS-INVARIANT bounds
  before that and two status-varying brackets after; a percentage-of-income
  standard deduction replaced by the federal one; a federal income tax
  deduction that lived INSIDE the state itemized schedule; and a capital
  gains preference that changed form from a credit to a rate schedule

## Primary sources

- Montana DOR Form 2 and its instruction booklet, TY2017-TY2025, plus the
  one-page annual rate sheets, Schedule I and Schedule 3 (TY2024+),
  Schedule 2EC and Form 2441-M, with text extractions retained
- MCA Title 15 Chapter 30, notably 15-30-2103 (rates and indexation),
  15-30-2114 (exemptions), 15-30-2120 (subtractions) and 15-30-2131 to 2132
  (deductions)
- The Legislative Services Division memo on SB 399, which carries the
  statutory base bracket amounts and the pre-2024 provisions it repealed
- SB 159 (2021), SB 399 (2021), SB 121 (2023), HB 221 (2023), HB 225 (2023),
  SB 104 (2023), SB 554 (2023) and HB 337 (2025)

## Parameter inventory by file

### `ord.yaml`

- Encoded: the seven-bracket ladder for TY2017-TY2023 with its bounds
  repeated across all four statuses (which is the encoding of the fact, not
  an oversight), the TY2024+ two-bracket schedule by status, and the
  preferential long-term capital gains rates and thresholds
- Known approximations: TY2026 and TY2027 rates deliberately excluded
  pending published tables; the TY2017-only Pease limitation

### `agi.yaml`

- Encoded: federal-AGI start point in both regimes; the pension/annuity/IRA
  exemption with its two-dollars-per-dollar phase-out; the 65-and-older
  interest exemption; US-obligation interest and state refunds
- Known approximations: Montana's own Social Security schedule; the
  per-spouse ceiling on the pension exemption; railroad retirement, military
  and tribal subtractions; the TY2024 transition adjustment

### `ded.yaml`

- Encoded: the 20%-of-Montana-AGI standard deduction with its floor and cap
  by status group; the federal standard deduction from TY2024; the itemized
  components with state income tax excluded; the federal income tax
  deduction and its status-varying cap, flagged as living inside the
  itemized schedule
- Known approximations: the deduction's cash basis; the recovery worksheet's
  one-year lag; the household and dependent care deduction; the TY2024+ SALT
  cap interaction

### `exempt.yaml`

- Encoded: the per-exemption amount through TY2023, applied to taxpayer,
  spouse, dependants, age and blindness alike, and the TY2024+ age-65
  subtraction in the aged slot
- Known approximations: the double exemption for a disabled child

### `credits.yaml`

- Encoded: the earned income credit at 3%, 10% and 20% of federal,
  refundable; the 2% capital gains credit through TY2023
- Known approximations: the Elderly Homeowner/Renter Credit; the EITC
  reduction worksheet; adoption, pass-through entity, other-state and
  business credits

### `filing.yaml`

- Encoded: the printed gross-income thresholds by status, under 65, for
  TY2017-TY2023, and the federal-requirement passthrough from TY2024
- Known approximations: the age and blindness additions to the threshold;
  the exclusion of unemployment compensation from the gross-income test

## New machinery this state required

- `st_ord.kg_pref_rate_low` / `_rate_high` / `_thresh` — a preferential rate
  schedule on net long-term capital gains, stacked federal-style
- `st_ded.fed_tax_ded_in_itemized` — the federal tax deduction placed inside
  the itemized base rather than alongside it, so it reaches only itemizers
  and feeds the itemize-versus-standard election
- `st_agi.pension_excl_po_rate` — a phase-out expressed in dollars of
  exclusion per dollar of income rather than proportionally
- `st_agi.int_excl_senior` / `_min_age` — a senior exemption on interest
  alone, distinct from the existing senior investment cap
- `st_credits.kg_credit_rate` — a nonrefundable credit as a share of net
  capital gain

## Worksheet tests added

MT-1 the capped percentage standard deduction; MT-2 the federal tax
deduction inside the itemized schedule flipping the election; MT-3 head of
household taking the SINGLE federal tax cap and the JOINT standard-deduction
bounds at once; MT-4 the pension exemption losing two dollars per dollar;
MT-5 the senior interest exemption; MT-6 the standard-deduction floor
binding; MT-7 the 2% capital gains credit; MT-8 the TY2024 two-bracket
regime on the federal standard deduction; MT-9a and MT-9b the preferential
gains schedule stacked above and below the threshold; MT-10 the age-65
subtraction; MT-11a and MT-11b the earned income credit tripling for TY2024.

## Research findings worth flagging

- **The pre-2024 bracket bounds do not vary by filing status at all.** A
  single filer and a joint couple faced the identical ladder. This is the
  fact that makes filing status 2a the dominant choice for two-earner
  couples, and the joint and head-of-household series in `ord.yaml` repeat
  the single one deliberately.
- **The federal income tax deduction was an ITEMIZED deduction.** A filer
  taking Montana's percentage standard deduction got none of it. Montana's
  itemize-versus-standard decision therefore cannot be inherited from the
  federal one for any pre-2024 year.
- **Head of household is grouped with SINGLE for the federal tax deduction
  cap ($5,000) and with JOINT for the standard-deduction bounds (double).**
  The two groupings run in opposite directions, which is easy to get
  backwards in either place.
- **Two rates were enacted and then repealed before they ever applied**: SB
  399's 6.5% top rate (cut to 5.9% by SB 121 before TY2024) and SB 399's 30%
  long-term capital gains subtraction (replaced by HB 221's rate schedule
  before TY2024). Neither was ever law for any taxpayer.
- **The 5.65% rate is a TY2026 figure**, from HB 337 of 2025. Secondary
  summaries have misdated it to TY2024 or TY2025.
- **The Montana EITC's first effective year is TY2019, not TY2017.** HB 391
  created it in the 2017 session, but the DOR booklet dates the start and
  there is no EITC line on the TY2017 or TY2018 return.
- **Montana had no child and dependent care CREDIT** in this period — only
  an itemized deduction on Form 2441-M, which the SB 399 repeal of the
  itemized schedule took with it.
- **The pension exemption has no age test.** It was gated purely on federal
  AGI, so the under-65 and 65-plus caps are the same figure.
- The pre-2024 filing thresholds show two exact regularities in all seven
  years: the under-65 threshold equals that status's standard-deduction
  MAXIMUM, and each 65-or-older or blind exemption adds one personal
  exemption amount. Useful as an internal consistency check, though every
  encoded cell was read from the printed chart rather than derived.
- The TY2024 "repealed deductions" list does not name the federal income tax
  deduction, because that item lived on the itemized schedule and the
  schedule was eliminated wholesale rather than itemized for repeal.

## Known differences

- **Filing status 2a, married filing separately ON THE SAME FORM, is the
  largest Montana gap.** Two columns, a per-spouse standard deduction, a
  per-spouse pension exemption and a per-spouse $5,000 federal tax deduction
  cap, all against the SAME status-invariant ladder — near-universally
  better than joint filing for a two-earner couple through TY2023, and the
  only route to the household and dependent care deduction for a married
  non-joint filer. Needs the generic minimum-liability election pass already
  queued for Wisconsin Act 15, Alabama separate returns and the Arkansas Low
  Income Tax Tables. Montana is the fourth state waiting on it.
- **Montana ran its own IRC 86 Social Security computation** through TY2023,
  on a Montana modified-income base that the pension exemption fed into, and
  with married-filing-separately base and second-tier amounts of $16,000 and
  $6,000 in BOTH columns where the federal rule zeroes the base for spouses
  who lived together. The model carries the federal taxable amount.
- **The federal tax deduction is cash-basis** — tax paid during the year,
  not liability for the year — and a refund of previously deducted federal
  tax is a Montana addition computed on a tax-benefit worksheet that needs
  the prior year's Montana taxable income and standard deduction. The model
  deducts current-year liability and omits the recovery.
- The Elderly Homeowner/Renter Credit is refundable, survived SB 399
  unchanged, and is claimable by filers with no filing requirement at all.
  It runs on rent paid, months of occupancy and the income of every
  household member, none of which are model inputs.
- From TY2024 the adjusted federal itemized deduction starts from the
  SALT-capped federal total; the encoding starts from uncapped property and
  personal property taxes, which agrees exactly whenever property plus
  income taxes fall under the federal cap.
- The TY2024 transition additions and subtractions bridging the two regimes,
  the HB 225 adoption credit, the SB 554 pass-through entity credit and the
  SB 104 working-military-retiree subtraction are all real and none is
  derivable from a cross-section.

## Cross-model validation notes

- TAXSIM years 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: every two-earner married cell before TY2024
  will diverge on the status 2a election; retiree cells before TY2024 will
  diverge on Montana's own Social Security schedule and on whether the
  external model applies the pension phase-out as dollars or as a share;
  itemizer cells before TY2024 turn on whether the external model puts the
  federal tax deduction inside the schedule; and TY2024+ cells with realised
  long-term gains turn on whether it implements the HB 221 stacking.

## Aggregate validation notes

- HT2 targets once weights land; the Montana DOR publishes an annual
  Biennial Report with individual income tax statistics for a revenue-agency
  benchmark.
