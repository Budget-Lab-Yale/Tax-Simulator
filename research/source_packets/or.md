# State Source Packet: Oregon

State: `OR`
Status: `done`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025, plus preliminary TY2026 rates, standard
  deduction, exemption credit and federal-subtraction cap from OR-ESTIMATE 2026
- Baseline only
- Major structural features: federal-AGI start; four brackets with the top bound
  FROZEN; a federal income tax liability subtraction whose dollar cap is itself
  cut in five AGI steps; personal exemptions delivered as a CREDIT with a hard
  income cliff; an earned income credit paying a higher rate for a child under
  three; the Oregon Kids Credit; and the kicker, which is structurally
  unmodellable in a cross-sectional model

## Primary sources

### Current forms and instructions

- Form/booklet: Form OR-40 instructions, TY2017-TY2025 (all nine downloaded and
  text-extracted)
- Publication OR-17 (the comprehensive guide) for TY2017, TY2018 and TY2025
- Form OR-ESTIMATE instructions TY2019-TY2026, which carry next-year rate
  charts, standard deductions, exemption credits and federal-subtraction caps
- Schedule OR-A (itemized deductions) forms and instructions for 2018, 2021,
  2024, 2025
- DOR landing page: oregon.gov/dor

### Statutes and technical guidance

- Rate schedule and indexation: ORS 316.037
- Standard deduction: ORS 316.695
- Federal income tax liability subtraction: ORS 316.680, 316.685, 316.695
- Social Security and Railroad Retirement: ORS 316.054
- Exemption credit: ORS 316.085
- Earned income credit: ORS 315.266
- Special Oregon medical subtraction: ORS 316.693
- Retirement income credit: ORS 316.157
- Filing requirement: ORS 316.117
- Conformity connection: ORS 316.012

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 1; `ss_full_sub_allages` (Social Security and Railroad
  Retirement wholly untaxed, no age or income test); `sub_us_int`;
  `sub_state_ref`
- Known approximations: the U.S.-obligation share of interest; the federal
  pension pre-October-1991 service subtraction; the special Oregon medical
  subtraction; out-of-state municipal interest

### `ded.yaml`

- Encoded: the indexed standard deduction by status with filing-status-mapped
  aged and blind add-ons ($1,200 single and head of household, $1,000 others)
  and the dependent-filer limitation; independent itemize election
  (`item_coupling` 0) with `salt_addback`; `fed_tax_ded` with
  `fed_tax_ded_less_excess_ptc`, `_less_ed_ref` and `_less_ptc`, plus the
  five-band `fed_tax_ded_band_upper` / `_band_cap` families on federal AGI
- Known approximations: the TY2020-2021 recovery rebate and TY2021 refundable
  child and care credit offsets to the subtraction; accrual and amended-return
  mechanics; married-filing-separately itemizing coupling

### `exempt.yaml`

- Encoded: `personal_amount` 0. Oregon has no exemption DEDUCTION at all -- its
  personal exemptions are a credit, which is why they survived TCJA untouched

### `ord.yaml`

- Encoded: `rates` (5/7/9/9.9% through TY2019, 4.75/6.75/8.75/9.9% from TY2020)
  and `brackets_single` / `brackets_joint` for all nine years plus preliminary
  TY2026. Head of household takes the JOINT widths
- Known approximations: the published chart constants are rounded and chained,
  so they can sit up to a dollar off the continuous computation

### `credits.yaml`

- Encoded: the exemption credit with its hard AGI cliff; the earned income
  credit with the new young-child rate; the Oregon Kids Credit via the
  style-3 child credit machinery
- Documented: the kicker, the working family household and dependent care
  credit, the retirement income credit, the political contribution credit, the
  ITIN-filer earned income credit

### `filing.yaml`

- Encoded: `req_type` 2 with `req_if_fed_filer`, and the under-65 gross-income
  thresholds for all nine years by status
- Known approximations: the aged and blind threshold add-ons; the separate
  qualifying-surviving-spouse column

## New generic machinery introduced for Oregon

1. `st_ded.fed_tax_ded_band_cap` -- an income-banded CAP on the federal tax
   deduction, alongside Missouri's income-banded SHARE. Both families hang off
   the same `fed_tax_ded_band_upper` bound table and each is independently
   optional, so a state can band either, both or neither.
2. `st_ded.fed_tax_ded_less_excess_ptc` -- strips the excess advance premium tax
   credit repayment back out of the base. It reaches 1040 line 22 through
   Schedule 2 Part I, but Oregon's subtraction is "limited to income tax".
   Missouri and Alabama both leave it in.
3. `st_credits.eitc_match_young` / `eitc_match_young_max_age` -- a higher earned
   income credit match where a dependent is under a stated age, distinct from
   the existing child-COUNT-keyed `eitc_match_by_kids` family.

## Worksheet tests added

- OR-1 single with the full federal tax subtraction
- OR-2 the subtraction cut by the third AGI band
- OR-3a / OR-3b the exemption credit cliff at $100,000
- OR-4 the young-child earned income credit rate and the Kids Credit
- OR-5 the Kids Credit half phased out
- OR-6 the pre-2020 rate schedule and the 8% earned income credit
- OR-7 joint AGI bands at double the single ones

## Research findings worth flagging

- **The rate cut is TY2020, not TY2019.** The TY2019 charts still read 5/7/9%.
- **The top bracket bound has never been indexed** -- $125,000 single and
  $250,000 joint in every year 2017-2026 -- while the first two bounds move
  annually.
- **OR-ESTIMATE 2025 is WRONG about the TY2025 second bracket bound.** It prints
  $11,050 / $22,100 where the correct values are $11,100 / $22,200. The
  discriminating test is the OR-40 booklet's own cumulative chart constants:
  the correct bounds reproduce the printed $4,065 at $50,000 and $10,627 at
  $125,000 exactly, and the estimate form's values give $4,067 / $3,761.
  OR-17's appendix agrees with the booklet. This matters for TY2026, whose
  values come from the same estimate form and should be replaced when the
  booklet publishes.
- **The federal EARNED INCOME CREDIT is the one federal credit that does not
  reduce Oregon's federal tax subtraction** (OR-17: "Federal income tax credits,
  except for the EITC, reduce your federal tax subtraction"). Missouri and
  Alabama both subtract it, so this is the one place Oregon's base is more
  generous. In Alabama the federal credit actually raises state tax.
- **The federal-subtraction AGI bands are frozen** at $125,000-$145,000 (single
  and separate) and $250,000-$290,000 (joint, head of household and qualifying
  surviving spouse) in every year, while the cap amounts inside them are
  indexed. From TY2024 the steps are exactly 80/60/40/20/0% of the cap, but
  earlier years round inconsistently -- TY2018 rounds 80% of $6,650 DOWN to
  $5,300 and 60% UP to $4,000 -- so the tables are transcribed literally.
- **The exemption credit is a cliff with no taper**, and it is tested on federal
  AGI rather than Oregon income.
- **Oregon's itemize election is fully independent of the federal one**, and
  because Schedule OR-A removes state and local income tax, many Oregon filers
  itemize for Oregon while taking the federal standard deduction.

## Known differences

- **The kicker is not modeled, and it is Oregon's largest gap.** The surplus
  credit is a percentage of the taxpayer's PRIOR year Oregon liability before
  credits -- 9.863% of TY2024 liability on the TY2025 return. Prior-year
  liability is not observable per record in a cross-sectional model, and the
  percentage is certified biennially. This understates Oregon refunds materially
  in every kicker year, and kicker years recur every other year. Encoding it
  needs either a lagged panel or a same-year approximation of prior-year
  liability, and the decision to do neither was deliberate rather than an
  oversight.
- **The working family household and dependent care credit is not modeled** --
  refundable, and a percentage of care expenses keyed jointly to household
  income as a share of the federal poverty guideline and to child age. The most
  valuable remaining Oregon item after the kicker.
- **The retirement income credit (ORS 316.157) is not modeled.** Its 9% rate is
  known but the eligible base, the household-income measure and the Social
  Security offset limits were not transcribed from a primary source in this
  pass. Flagged as the next Oregon encoding task.
- The federal pension pre-1991 service subtraction, the special Oregon medical
  subtraction, the political contribution credit, the ITIN-filer earned income
  credit, and the 529 and ABLE contribution items are all data-blocked or
  unobserved; each is documented in the relevant yaml.
- TY2026 values are preliminary, from a form that has already been wrong once.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the kicker will dominate any TY2018, TY2020, TY2022
  or TY2024 cell where the external model includes it and we do not -- check
  first whether TAXSIM and PolicyEngine model it at all, because if they do the
  Oregon cells cannot clear without a lagged-liability approximation. Beyond
  that, the WFHDC and retirement income credits, the unmodelled special medical
  subtraction, and the federal pension subtraction will show in older and
  lower-income cells respectively.

## Aggregate validation notes

- HT2 targets once weights land; the Oregon Department of Revenue publishes
  annual personal income tax statistics for a revenue-agency benchmark, and the
  Legislative Revenue Office publishes kicker forecasts that would help size
  that known difference.
