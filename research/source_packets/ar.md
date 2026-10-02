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
- Encoded 2026-10-02: `ob_passthru_as_federal` 1. AR1000F line 19 takes
  partnership, S-corporation and fiduciary income "as reported on your
  federal Schedule E", i.e. after the federal passive-activity and at-risk
  limits, so the own base uses the federally allowed `part_scorp` rather than
  `part + scorp`, which net every loss (a -$3.55M S-corporation loss federal AGI
  ignores had driven one record's Arkansas income to -$3.3M). Mississippi's
  line 41 reads the same way; Alabama has no passive-loss limit and keeps 0
- Encoded 2026-10-02: `add_exempt_int` 1 with `own_state_exempt` 1, and
  `sub_us_int` 1. Arkansas exempts interest only on US obligations and on
  Arkansas and its subdivisions (booklet exempt income item 7), so other
  states' bond interest is Arkansas income; the own base had never carried
  it. The in-state share is the model-wide 75% assumption
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
the joint and the split liability. `split_credits_pooled` 1 (2026-10-02): the
two column taxes are added on line 30 BEFORE the credits come off, so one
column's unused credits offset the other's tax; summing floored column
liabilities had lost them, enough to make the joint return wrongly win for
retired couples (AR-ST4).

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
- Encoded 2026-10-02: the TY2021 child care credit on the PRE-ARPA federal
  credit (Arkansas did not adopt the 2021 expansion): an own-rate credit
  (`cdctc_style` 2) of 20% x the pre-ARPA rate, i.e. 7% sliding 0.2 point per
  $2,000 of federal AGI over $15,000 to 4%, on expenses capped at $3,000 /
  $6,000 and at the lower earner's income. New generic parameter
  `cdctc_rate_income_base` lets the slide run on federal AGI. TY2017-20 and
  TY2022+ stay 20% of the federal credit as claimed (AR-CC1, AR-CC2)

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
loss limit; AR-CG1 and AR-CG2 the $10,000,000 gain tier either side; AR-PT1 a
federally disallowed pass-through loss; AR-MI1 out-of-state bond interest; AR-LT1
to AR-LT5 the low income tables (zero band, in-table, above the ceiling, head
of household with two dependents, exempt income counted toward the ceiling).

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

- **The Low Income Tax Tables are encoded (2026-10-01)** as an elective
  whole-income table (`st_ord.alt_table_election`, `calc_st_alt_table` in
  st_tax.R): looked up on Arkansas AGI with the retirement exemption added
  back (a filer may elect out of it to qualify), taken when cheaper than the
  schedule, and only where total income including exempt Social Security and
  the excluded half of gains is within the table. All 45 tables (five a year,
  TY2017-2025) are in credit_tables.csv, extracted from the booklets'
  word coordinates by research/state_tax/scripts/ar_lit_extract.py, every
  table checked to start at zero and chain without a gap. Against
  policyengine-us's own transcription for TY2021-2025 they agree on 1,128 of
  1,129 rows; the exception is a PolicyEngine typo (TY2024 head of household
  with two or more dependents starts at $24,200, the booklet at $24,177, a $92
  row, under tolerance). The TY2017 booklet misprints one lower bound
  (21,900 for 21,901), repaired by the extractor. Not modeled: the military
  pay and military retirement disqualifications.
- The deaf and head-of-household additional personal credits, and the $500
  developmental disabilities credit, are not model inputs.

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
  allowed), so our treatment stands with the federal NOL as the proxy.
- **T30**: TAXSIM ignores `nonprop` in Arkansas AGI in both directions, so it
  drops other income and alimony as well as the NOL (probe: +$10,000 of
  nonprop leaves AR AGI unchanged; the same in otherprop adds $10,000). Keyed
  on the tax effect (amount x marginal rate > $100), which took the TAXSIM
  cells from about 0.93 to 0.94-0.955.
- PolicyEngine window, 2026-10-02 (per-record PolicyEngine reruns of the
  missed records): the crosswalk gave all pension income to the head, so
  PolicyEngine allowed one $6,000 exemption per couple where we and TAXSIM
  pool the pension against both. The PolicyEngine driver now splits pension
  income evenly between spouses (harness-wide; who owns a pension is
  unobserved). Two rows added: separate filers with care expenses (the
  federal IRC 21(e)(4) living-apart assumption, as for NJ) and the crosswalk
  deduction class (PolicyEngine never sees medical, miscellaneous or casualty
  deductions, which Arkansas still allows).
- Same day, two more PolicyEngine-window findings. **P15** (fixed upstream in
  #9615): 1.775.7 taxes 2021 unemployment benefits, which Act 154 of 2021
  exempted for 2020-21; excluding them took PE 2021 from 0.856 to 0.941.
  And a **harness** fault, not a PolicyEngine rule: PolicyEngine infers head
  and spouse by age, ignoring the dependent flag, so an adult dependent of a
  single or separate filer became a "spouse" and the Inflationary Relief
  credit (paid per head or spouse) came out at $300 instead of $150. The
  driver now sets `is_tax_unit_head` / `is_tax_unit_spouse` on every person.
- Further rows 2026-10-02: municipal interest for both models (TAXSIM has no
  exempt-interest input; PolicyEngine taxes all of it where we assume 75% is
  in-state); the TAXSIM head-of-household derivation class (as for UT, DC,
  CA), which matters in Arkansas only through the low income tables; and the
  tables' total-income test, which counts exempt Social Security and which
  neither external model applies (`st_alt_table_blocked`, a new diagnostic
  output, marks the units it refuses).
- **Open decision (2026-10-02): who owns unobserved income in the status-4
  split.** The split halves non-wage income between the columns (the
  ST_SPLIT_HALVE convention). With credits now pooled the split wins more
  often, and couples where it wins match worse in both external models
  (TAXSIM 0.83, one-earner couples 0.70-0.77; PolicyEngine, which gives all
  of it to the head, 0.22-0.79), taking PE 2023 from 0.948 to 0.926. The
  pooling is the form; the halving is an assumption. `st_split_used` marks
  the couples. Not excluded pending a decision on the convention.
- Low income tables: TAXSIM's low-income misses (our tax higher, under
  $20,000 of income, mostly heads of household) were the tables; encoded.

## Aggregate validation notes

- HT2 targets once weights land; Arkansas DFA publishes annual statistics of
  income for a revenue-agency benchmark.
