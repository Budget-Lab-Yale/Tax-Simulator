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
- Encoded 2026-10-01: `ob_cap_loss_limit` 3,000, the federal net capital
  loss limit ($1,500 per spouse column on filing status 4) from the AR1000F
  line 13 instructions. The microdata gain fields are uncapped, so before
  this a large realized loss wiped out wages in Arkansas income;
  `cap_gains_full_excl_above` $10,000,000, AR1000D line 7b (gain above it is
  exempt outright, not half-taxed; Ark. Code Ann. 26-51-815)
- Known approximations: the IRA branch's 59-and-a-half test; military
  retirement

### `ded.yaml`

- Encoded: the standard deduction, flat through TY2021 and indexed from TY2022
- Known approximations: TCJA non-conformity on itemized deductions

Filing status 4 (married filing separately on the same return, AR1000F Box 4)
is encoded in `ord.yaml` as `split_election` 1 with `split_item_by_agi_share`
1: each spouse column takes the schedule and a full single standard deduction,
and itemized deductions are pooled then prorated by each spouse's share of AGI
rounded to whole percent (Form AR3 lines 30-33). The model takes the lower of
the joint and the split liability.

### `exempt.yaml`

- Encoded: zero. Arkansas grants credits, not exemptions

### `credits.yaml`

- Encoded: the personal tax credits ($26 to TY2019, $29 from TY2020) for
  taxpayer, dependants, age and blindness; the child care credit at 20% of
  federal; `eitc_match` 0 as a verified negative
- Encoded 2026-10-01: the TY2022-23 Inflationary Relief credit as
  `stepcred1_*` ($150 / $300 joint up to $87,000 in TY2022 and $89,600 in
  TY2023, less $10 per $1,000 over, the joint table doubling every band) and
  the TY2022+ Additional Tax Credit for Qualified Individuals as `stepcred2_*`
  ($60 per spouse up to $24,300 / $25,000 / $25,800 / $26,500 for TY2022-25,
  less $5 per $100 over, looked up per spouse and so doubled for joint
  filers). Both nonrefundable, through the generic `st_step_credit`

### `filing.yaml`

- Encoded: gross-income thresholds by status for all nine years

## Worksheet tests added

AR-1 the indexed standard deduction; AR-2a and AR-2b the whole-income table
NOTCH either side of $22,900 in TY2020; AR-3 the 50% long-term capital gain
exclusion; AR-4 Social Security exempt with the $6,000 retirement exemption;
AR-5 above the recapture tail; AR-6 the absence of a state earned income
credit; AR-7 the child care credit at 20% of federal. 2026-10-01: AR-SC1 to
AR-SC4 the two stepped credits at plateau, mid-ramp, zero and joint; AR-ST1 to
AR-ST3 the filing status 4 split with AGI-share itemized proration. AR-4 and
AR-6 now include the qualified-individuals credit. AR-CL1 the $3,000 capital
loss limit; AR-CG1 and AR-CG2 the $10,000,000 gain tier either side.

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

- 2026-10-01 triage (refit vintage, filers only). Before the pass: PolicyEngine
  2021-25 0.662 / 0.283 / 0.523 / 0.641 / 0.636, TAXSIM 2017-20 0.642 / 0.629 /
  0.663 / 0.666. Encoding the two stepped credits and the filing status 4
  split took them to 0.684 / 0.722 / 0.579 / 0.682 / 0.729 and 0.723 / 0.710 /
  0.748 / 0.737 (2022 is the Inflationary Relief year PolicyEngine pays in full).
- External-model bugs found (external_model_issues.md): **T29**, TAXSIM gives the
  $6,000 retirement exemption only at 65 and over, where the AR1000F
  instructions set no age test for employer plans and 59 1/2 for IRAs (probe:
  AR AGI 40,000 at ages 59-64, 34,000 at 65); **P12**, PolicyEngine pays the
  TY2023 Inflationary Relief credit at $50 / $100 against the DFA worksheet's
  $150 / $300, and leaves the TY2023 joint step-down start at $174,000 against
  the table's $179,200. Both keyed in known_differences.csv. The P12 row keys
  on the liability effect (each credit capped at the tax left after the other
  nonrefundable credits) being at least $99.50: the single-filer gap is
  exactly $100, and PE rounds to the cent, so those records fall either side
  of the tolerance by sub-cent noise. It removes 215 of 469 aligned 2023
  records; the 2022 control (PE pays $150 / $300) is clean.
- Sizing the low-income tables: in the PolicyEngine years only about ten
  misses a year sit below $35,000 of income with our tax higher. Half the
  misses are at $100,000+, which is where the capital loss limit and the
  $10,000,000 tier were found.
- **P13**: PolicyEngine's Arkansas gross income omits partnership, S-corporation,
  estate and trust income (all on AR1000F line 19 with rents) and other income
  (line 22), so pass-through income and losses never reach Arkansas tax (probe:
  AR AGI unchanged at 100,000 with +/-50,000 of partnership income). Keyed on
  the new harness covariate `xw_pe_passthru_misc`.
- **Net operating loss: resolved.** Arkansas allows the carryforward as a
  subtraction on line 22 (Form AR-OI, "Attach form AR1000-NOL"; carrybacks not
  allowed), so our treatment stands with the federal NOL as the proxy. TAXSIM
  ignores it; keyed as an external-model-scope row.
- Still open: the low-income tables election (above).

## Aggregate validation notes

- HT2 targets once weights land; Arkansas DFA publishes annual statistics of
  income for a revenue-agency benchmark.
