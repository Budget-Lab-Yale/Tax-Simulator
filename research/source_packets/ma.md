# State Source Packet: Massachusetts

State: `MA`
Status: `done`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025
- Baseline only
- Major structural features: SCHEDULAR own base (income classes taxed at
  different rates); no standard or itemized deduction; exemptions plus a short
  enumerated deduction list; No Tax Status and the Limited Income Credit; the
  4% surtax from TY2023

## Primary sources

- All nine Form 1 instruction booklets TY2017-TY2025, eight Form 1s, and the
  2023/2024/2025 Schedules B, D, X, Y, CB, DI, C and E, retrieved from the NBER
  historical state tax forms archive (taxsim.nber.org/historical_state_tax_forms/MA/)
- The three Schedule 4% Surtax PDFs and the DOR rates page came via the
  Internet Archive: **mass.gov returns HTTP 403 to every non-browser client**,
  which is worth knowing before anyone tries to refresh these sources
- M.G.L. c. 62 and the Technical Information Releases themselves were NOT
  openable; every value here comes from DOR forms and instructions, with the
  statute and TIR numbers listed for independent confirmation

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 0 with the `ob_*` class shares; `ob_ss_share` 0
  (Social Security wholly excluded); `ob_st_gains_share` 0 (short-term gains
  held out for their own rate); `ob_gains_share` 1 (long-term gains take the
  headline rate)
- Known approximations: the government contributory pension exclusion; the
  state's own capital-loss netting and indefinite carryforward; collectibles

### `ord.yaml`

- Encoded: the headline rate (5.1% / 5.05% / 5.0%) as a flat ten-element
  schedule, and `st_gains_rate` (12% to TY2022, 8.5% from TY2023)

### `surtax.yaml`

- Encoded: `taxable_income_rate` 4% and the indexed threshold from TY2023

### `ded.yaml`

- Encoded: `payroll_ded_cap` $2,000 per person
- Known approximations: public-retirement contributions are not a model input;
  the rental deduction and the pre-2023 dependent-care regimes are documented

### `exempt.yaml`

- Encoded: personal $4,400 per taxpayer ($6,800 head of household), dependent
  $1,000, age-65 $700, blindness $2,200, and the medical/dental exemption

### `credits.yaml`

- Encoded: the Limited Income Credit / No Tax Status mechanism, the earned
  income credit (23% / 30% / 40%), and the Child and Family Tax Credit

### `filing.yaml`

- Encoded: a flat $8,000 Massachusetts gross income threshold, every status,
  every year

## New generic machinery introduced for Massachusetts

1. `st_agi.ob_st_gains_share` + `st_ord.st_gains_rate` -- short-term capital
   gains split out of the own-base gains class and taxed at their own rate.
   Pennsylvania and Alabama set the new share explicitly to 1 so their
   treatment is unchanged.
2. `st_ded.payroll_ded_cap` -- a per-person deduction of the filer's own
   payroll and public-retirement contributions.
3. `st_exempt.medical_exempt` -- the federal Schedule A medical deduction
   allowed as a state exemption, itemizer or not.
4. `st_credits.lic_*` -- No Tax Status and the Limited Income Credit as ONE
   mechanism. Both the published 1.75x band ceiling and the
   married-filing-separately exclusion fall out of the arithmetic rather than
   needing their own parameters.

The 4% surtax reuses the existing `st_surtax.taxable_income_*` component.

## Worksheet tests added

MA-1 payroll deduction at its cap; MA-2 the TY2017 rate; MA-3a/MA-3b
short-term gains at 12% and 8.5%; MA-4 No Tax Status; MA-5 the Limited Income
Credit binding; MA-6 exemptions and the Child and Family Tax Credit; MA-7 the
surtax threshold not doubling for joint filers; MA-8 Social Security excluded
with long-term gains included; MA-9 the earned income credit at 40%.

## Research findings worth flagging

- **The age-65 exemption is $700, not $1,700.** Every year's Form 1 PDF text
  layer extracts the multiplier as "x $1,700"; rendering the same region as an
  image shows the printed figure is $700, which the line instructions confirm
  in words. A transcription taken from the text layer would be wrong in all
  nine years.
- **The dependent-care regime changed twice, and secondary sources describe
  only the last step.** TY2017-2020 gave DEDUCTIONS of up to $4,800/$9,600 and
  $3,600 per dependent; TY2021-2022 replaced them with small refundable credits
  ($240/$480 and $180/$360); TY2023 replaced those with the Child and Family
  Tax Credit. Only the last is encoded.
- **Interest and dividends are taxed at the Part B rate, not 12%.** They reach
  it through Schedule B line 38 to Form 1 line 20. Summaries that attach 12% to
  all of Part A are wrong.
- **The surtax threshold does not double for joint filers**, and Massachusetts
  has no qualifying widow(er) status at all.
- **Massachusetts bank interest is Part B (Form 1 line 5), not Part A**, and
  its $100/$200 exemption was repealed effective TY2024.
- The collectibles "50% deduction" is itself reduced by half of any excess
  short-term losses.
- The senior circuit breaker more than doubled in TY2023 ($1,200 to $2,590) by
  a statutory base doubling rather than indexation.
- From TY2024 a federal joint return forces a Massachusetts joint return
  (TIR 24-4), which removes any need for a separate-return election.

## Known differences

- **Government contributory pension exclusion not modeled**: U.S. and
  Massachusetts public contributory pension income is entirely excluded while
  another state's public pension is included at gross. Federal 1040 line 4b/5b
  does not identify the payer. Same pension-source limit as NY, MO and AL;
  clears with the Tier 1 imputation.
- **The senior circuit breaker is not modeled** -- refundable, and keyed to
  property tax paid or 25% of rent against 10% of total income. The largest
  data-blocked Massachusetts item.
- **The rental deduction is not modeled** (50% of rent, capped at $3,000 rising
  to $4,000), a Tier 1 target reaching roughly a third of Massachusetts filers.
- **The TY2017-2022 dependent-care deductions and credits are not modeled**,
  the largest year-specific gap.
- Massachusetts' own capital-loss netting, its shared $2,000 cap against
  interest and dividends, and its indefinite carryforward are not reproduced;
  the model carries federally capped net gains.
- The Child and Family Tax Credit reaches disabled and elderly qualifying
  individuals as well as children under 13; only the under-13 branch is
  encoded.
- Massachusetts public employees' retirement contributions are pre-tax
  federally but taxable in Massachusetts, and the difference is invisible on a
  federal record.
- The solar and wind energy credit exists but NO dollar cap or percentage
  appears in any 2017-2025 booklet, and Schedule EC was not retrievable, so it
  is flagged unverified rather than encoded.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the pension exclusion will dominate retiree cells;
  the circuit breaker and rental deduction will show in elderly and renter
  cells; the pre-2023 dependent-care regimes will show in family cells in the
  earlier years; and any filer with realized capital losses will diverge on the
  netting rules.

## Aggregate validation notes

- HT2 targets once weights land; the Massachusetts DOR publishes annual
  personal income tax statistics for a revenue-agency benchmark.
