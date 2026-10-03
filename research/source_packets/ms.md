# State Source Packet: Mississippi

State: `MS`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025, with enacted rates through TY2030
- Baseline only
- Major structural features: own base that never takes a federal AGI figure;
  status-invariant brackets with a growing ZERO BRACKET; retirement income
  exempt in total; generous exemptions in place of a large standard deduction;
  no refundable credit of any kind

## Primary sources

- Mississippi DOR Form 80-105 and the Form 80-100 instruction booklet,
  TY2017-TY2025, with text extractions retained
- Miss. Code Ann. Title 27 Chapter 7, notably 27-7-9(f)(10), 27-7-15(4)(k)
  and (4)(l), 27-7-16, and 35 Miss. Code R. 3-02-07-104
- HB 531 (2022) and HB 1 (2025) for the enacted rate path

## Parameter inventory by file

### `ord.yaml`

- Encoded: `brackets` carrying the growing zero-bracket ceiling, and `rates`
  through the enacted TY2030 path
- Known approximations: the gaming winnings flat tax; TY2031+ trigger rates
  deliberately excluded

### `agi.yaml`

- Encoded: own base with the `ob_*` shares; retirement, IRA and Social
  Security shares all zero; unemployment taxable
- Known approximations: early and excess distributions, severance and
  pre-retirement deferred compensation stay taxable but are not separable;
  the 27-7-9(f)(10) Mississippi-domiciled stock gain exclusion; out-of-state
  municipal interest

### `exempt.yaml`

- Encoded: $6,000 per taxpayer ($8,000 head of family), $1,500 each for
  dependants, age 65 and blindness
- Known approximations: free allocation of the exemption on a combined return

### `ded.yaml`

- Encoded: the standard deduction, unchanged in every year
- Known approximations: the TY2017-only Pease-style limitation; gaming losses

### `credits.yaml`

- Encoded: `eitc_match` 0 as a verified negative; the child care credit at 25%
  of federal from TY2023 only
- Known approximations: the $50,000 federal-AGI cliff on that credit

### `filing.yaml`

- Encoded: $8,300 single and $16,600 married; head of family flagged
  UNVERIFIED

## Worksheet tests added

MS-1 the $10,000 zero bracket at 4.7%; MS-2a and MS-2b the zero bracket
growing into the 3% band rather than the rate being cut; MS-3 retirement
income exempt in total; MS-4 joint exemptions and dependants; MS-5 the enacted
step to 4.4%; MS-6 the absence of an earned income credit; MS-7a and MS-7b the
child care credit arriving in TY2023.

## Research findings worth flagging

- **The "3% bracket phase-out" was never a rate cut.** A zero bracket appeared
  in TY2018 and grew $1,000 a year from below until it had eaten the 3% band
  by TY2022, then jumped to $10,000 in TY2023 and took the 4% band with it.
  The rates themselves never moved. Encoding a disappearing 3% RATE would get
  every intermediate year wrong.
- **The $10,000 top-bracket threshold never moves**, in any year 2017-2030,
  and the brackets do not vary by filing status at all.
- **Retirement income is exempt in total and the exemption is mechanical.**
  The instruction is "do not report" the income, so it never enters the base
  — there is nothing to subtract. Social Security, Railroad Retirement and
  pensions, IRAs and 401(k)s from federal, state AND private systems all
  qualify.
- **Head of family carries an encoding trap**: the booklets describe an
  effective $9,500, but the extra $1,500 is the REQUIRED dependant already
  counted on the dependant line. Encoding $9,500 double-counts them.
- **Two verified negatives**: there is no earned income credit (the only
  "earned income" string in any booklet is the FEDERAL credit used as an
  eligibility test inside the Reforestation Credit), and no child care credit
  existed before TY2023 — the earlier one is an employer credit, which the
  TY2017 booklet warns about in bold.
- **No credit is refundable in any year.**
- Mississippi casino winnings carry a 3% FINAL withholding, are excluded from
  income, and the withholding is not creditable.
- A web search surfaced a "$6,000 retirement cap" figure that contradicts nine
  booklets; the research flagged it explicitly as do-not-use.

## Known differences

- **The COMBINED return is modeled from 2026-10-02** (`st_ord.combined_pool`):
  two columns on the single schedule, so a two-earner couple shelters two
  zero brackets, with the pooled exemption and deduction allocated to
  minimize tax ("may be divided between the spouses in any manner they
  choose"). Column income is own wages plus half of unobserved non-wage
  income; one spouse's wages are never split ("If only one spouse has
  income, this income may not be split"). Both external models compute two
  columns too (probes: TAXSIM 2019 $1,750 two-earner against $1,960
  one-earner on $60,000; PolicyEngine 2024 $1,099.80 against $1,569.80).
  MS-4 now expects 1,898.80 (it assumed one zero bracket); MS-CP1/CP2 pin both
  cases.
- Early and excess distributions, severance, and deferred compensation taken
  before retirement age stay taxable despite the total exemption; the model
  exempts the whole pool.
- The 27-7-9(f)(10) exclusion for gains on Mississippi-domiciled stock and
  LP/LLC interests held over a year is 100% and uncapped, and unmodellable
  from survey data.
- Out-of-state municipal interest: now added back (`add_exempt_int`, the
  model-wide 75% in-state assumption) with US-obligation interest subtracted;
  the line 43 instructions say other states' bond interest "is taxable".
- Own adjustments repeating a federal item now subtracted (lines 50-61: IRA,
  SEP/SIMPLE/Keogh, early-withdrawal penalty, self-employed health insurance,
  HSA, and half of self-employment tax via the new `st_agi.ob_adj_seca`).
  Alimony paid (federally gated by divorce date) and moving expenses (military
  only since 2018) are documented, not applied (MS-ADJ1).
- The head-of-family filing threshold is UNVERIFIED — no booklet prints one,
  and the encoded $11,400 is the arithmetic analogue of the single and married
  figures rather than a transcription.

## Cross-model validation notes

- 2026-10-02 triage (refit vintage, filers only). Start: TAXSIM 2017-20 0.847 /
  0.830 / 0.829 / 0.826, PolicyEngine 2021-25 0.866 / 0.841 / 0.800 / 0.808 /
  0.888. Our-side fixes: the combined return, own adjustments, out-of-state
  bond interest (above); the TY2023+ child care credit's $50,000 federal-AGI
  CLIFF, documented but never encoded (now the generic income cap with a zero
  amount; MS-CC1/CC2); and the federal $10,000 SALT limit on Mississippi's
  Schedule A taxes ("Per the Tax Cuts and Jobs Act, there is a $10,000
  limitation"), which the state itemized base never applied (new generic
  `st_ded.item_salt_cap`; MS-SALT1).
- External: **P18** (PolicyEngine's Mississippi AGI omits miscellaneous income,
  the Alabama P17 pattern) and **P19** (its generic `state_income_tax` does not
  net the Mississippi child care credit that `ms_income_tax` does; the harness
  now reads `ms_income_tax`). Muni rows for both models.
- **Closed 2026-10-02.** Final cells: TAXSIM 2017-20 0.953 / 0.958 / 0.969 /
  0.963; PolicyEngine 2021-24 0.962 / 0.966 / 0.953 / 0.955 (2025 0.947).
- Open: one-earner couples whose half of unobserved non-wage income uses the
  second column's zero bracket under our even-split convention, where
  PolicyEngine gives all of it to the head (candidate key tested, hits match
  0.4-0.6, not adopted).

- TAXSIM years 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the combined-return two-column treatment will
  show in every two-earner married cell; retiree cells should be clean if the
  external model also exempts retirement income in full, and sharply divergent
  if it does not; and any cell containing casino winnings will diverge because
  the 3% final tax sits outside the income tax entirely.

## Aggregate validation notes

- HT2 targets once weights land; the Mississippi DOR publishes annual reports
  for a revenue-agency benchmark.
