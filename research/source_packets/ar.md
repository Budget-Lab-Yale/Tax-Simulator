# State Source Packet: Arkansas

State: `AR`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025
- Baseline only
- Major structural features: full own base; a published schedule in
  `rate x income - minus adjustment` form rather than a marginal ladder; a
  whole-income-table NOTCH through TY2021; a recapture tail; personal CREDITS
  in place of exemptions; and five Low Income Tax Tables that are a taxpayer
  ELECTION

## Primary sources

- Arkansas DFA Form AR1000F booklets TY2017-TY2025, plus AR3 (itemized), AR1000D
  (capital gains), AR2441 (child care) and AR1000TC
- The DFA one-page memo "<year> Indexed Tax Brackets" for every year, which is
  the authoritative machine-readable form of the schedule
- Ark. Code Ann. Title 26 Chapter 51, and Act 2 of the 2021 Second
  Extraordinary Session for standard-deduction indexation
- 70 source PDFs and 68 text extractions retained in the research folder

## Parameter inventory by file

### `ord.yaml`

- Encoded: `brackets`, `rates` and `base_amounts` for all nine years, twelve
  bands each, generated programmatically from the transcribed DFA memos
- Known approximations: the midpoint table convention; the granular
  TY2022+ recapture tail encoded as one ramp band

### `agi.yaml`

- Encoded: own base with the `ob_*` shares; `cap_gains_excl_share` 0.5;
  the $6,000 retirement exemption with no age gate; Social Security excluded;
  unemployment compensation year-keyed through its three flips
- Known approximations: the IRA branch's 59-and-a-half test; military
  retirement; the $10,000,000 full capital gain exemption

### `ded.yaml`

- Encoded: the standard deduction, flat through TY2021 and indexed from TY2022
- Known approximations: TCJA non-conformity on itemized deductions

### `exempt.yaml`

- Encoded: zero. Arkansas grants credits, not exemptions

### `credits.yaml`

- Encoded: the personal tax credits ($26 to TY2019, $29 from TY2020) for
  taxpayer, dependants, age and blindness; the child care credit at 20% of
  federal; `eitc_match` 0 as a verified negative

### `filing.yaml`

- Encoded: gross-income thresholds by status for all nine years

## Worksheet tests added

AR-1 the indexed standard deduction; AR-2a and AR-2b the whole-income table
NOTCH either side of $22,900 in TY2020; AR-3 the 50% long-term capital gain
exclusion; AR-4 Social Security exempt with the $6,000 retirement exemption;
AR-5 above the recapture tail; AR-6 the absence of a state earned income
credit; AR-7 the child care credit at 20% of federal.

## Research findings worth flagging

- **The schedule is not a ladder.** The booklet prints a dense $100-step table
  and the closed form lives in a separate DFA memo as
  `rate x income - minus adjustment`. Converting it to base amounts
  (`base = rate x bracket - adjustment`) makes the two identical; the
  conversion was done programmatically and verified back at eight published
  points across five years.
- **The personal credit is $26 only through TY2019 and $29 from TY2020.** It
  is conditionally indexed off a 2001 base of $20 and steps only in years a
  general-revenue trigger fires, which is why it moved once in nine years.
  Secondary sources quoting a flat $26 are wrong for six of the nine years.
- **The pre-2022 notch is real.** Three statutory whole-income tables selected
  by income level mean TY2020 taxable income of $22,899 owes $537.00 and
  $22,900 owes $717.29.
- **Unemployment compensation flips taxable status three times** — exempt in
  TY2017, taxable TY2018-19, exempt TY2020-21, taxable from TY2022.
- **Arkansas did not conform to TCJA's itemized changes**: 2% miscellaneous
  deductions and casualty losses survive, there is no SALT cap, the medical
  floor is 10%, and moving expenses remain deductible.
- Two probable DFA errors are recorded: the TY2022 booklet contradicts itself
  on the qualified-individuals ceiling, and the TY2025 filing threshold for
  joint filers with two or more dependants prints $28,723 where the
  low-income table implies $29,723.

## Known differences

- **The Low Income Tax Tables are not modeled, and this is the largest
  Arkansas gap.** Five dense tables, used INSTEAD of the schedule and INSTEAD
  of any deduction, zeroing tax below their thresholds — and the booklet makes
  it an explicit taxpayer election, so modelling it means computing both paths
  and taking the better. That is the generic minimum-liability election pass
  already queued for the Wisconsin Act 15 election and Alabama separate
  returns. Arkansas is the third state waiting on it.
- **Filing status 4 applies the schedule twice**, once per spouse column, with
  a per-spouse standard deduction and itemized deductions pooled then prorated
  by AGI share. Not modeled.
- The TY2022-23 Inflationary Relief credit and the TY2022+ Additional Tax
  Credit for Qualified Individuals (a $60 plateau falling $5 per $100, looked
  up per spouse) are not encoded.
- The deaf and head-of-household additional personal credits, and the $500
  developmental disabilities credit, are not model inputs.
- From TY2021 the child care credit runs on a pre-ARPA recomputation of the
  federal credit rather than the credit as claimed.

## Cross-model validation notes

- TAXSIM years 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the low-income tables will dominate every
  low-income cell, so those cells cannot clear until the election pass exists;
  TCJA non-conformity will show among itemizers; and the notch means results
  just above $22,900 of taxable income are highly sensitive to whether the
  external model reproduces it.

## Aggregate validation notes

- HT2 targets once weights land; Arkansas DFA publishes annual statistics of
  income for a revenue-agency benchmark.
