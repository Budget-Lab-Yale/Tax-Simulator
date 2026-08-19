# State Source Packet: Iowa

State: `IA`
Status: `done`
Last updated: `2026-08-19`

## Scope

- Tax years covered: 2017-2025, with the flat rate confirmed through TY2026
- Baseline only
- Major structural features: three regimes and four bracket counts in nine
  years; a nine-bracket ladder that does not vary by filing status; a full
  uncapped federal income tax deduction available to standard-deduction
  filers; a two-column combined return for married couples; personal
  exemption CREDITS rather than exemptions; and an alternate tax that caps
  the bill for every status except single

## Primary sources

- Iowa Code sections 422.4, 422.5, 422.5A, 422.7, 422.9, 422.10, 422.11B,
  422.12, 422.12B, 422.12C and 422.13, read in the 2017 through 2026 Code
  editions so each parameter's constancy or change is verified rather than
  assumed
- IA 1040 Expanded Instructions, TY2018-TY2025
- IDR *Iowa Individual Income Tax Annual Statistical Report*, TY2017-TY2024,
  Table 1 and Figure 1
- IDR annual bracket and deduction press releases, TY2018-TY2026, several
  retrieved through the Wayback Machine
- Form IA 6251 for 2017, 2019 and 2021, and the LSA fiscal note on HF 2317
- Session laws: SF 2417 (2018 Acts ch 1161), HF 2317 (2022 Acts ch 1002),
  2021 Acts ch 177 and SF 2442 (2024 Acts ch 1094)

## Parameter inventory by file

### `ord.yaml`

- Encoded: the nine-bracket ladder for TY2017-TY2022 with its bounds repeated
  across all statuses, the TY2023 and TY2024 ladders by status, the flat
  3.8% from TY2025, and the combined-return election with its per-column
  standard deduction
- Known approximations: the Iowa AMT; the alternate tax add-backs and
  married-separate proration; the lump-sum tax; the school district and EMS
  surtaxes; NOL disqualifiers

### `agi.yaml`

- Encoded: the start point moving from federal AGI to federal taxable income
  at TY2023; Social Security untaxed in every year; the retirement exclusion
  capped at $6,000 and $12,000 through TY2022 and uncapped from TY2023, with
  its age-55 test
- Known approximations: the disability and survivor eligibility routes;
  military retirement and survivor benefits; railroad retirement;
  nonqualified deferred compensation; the health insurance premium
  adjustment; pre-2023 NOL carryforwards

### `ded.yaml`

- Encoded: the indexed standard deduction through TY2022; the itemized
  components with Iowa income tax struck and the SALT cap disapplied; the
  uncapped federal income tax deduction with its NIIT addition and its
  refundable-credit and excess-APTC subtractions
- Known approximations: the deduction's cash basis; the federal AMT
  inference; the Iowa QBI phase-in; the Iowa-specific Schedule A
  adjustments; the IA 104 high-income limitation; the standard-deduction
  interaction with remaining income; the TY2021 charitable add-back

### `exempt.yaml`

- Encoded: zero, stated explicitly. Iowa grants credits, not exemptions

### `credits.yaml`

- Encoded: the exemption credits, with head of household doubled; the earned
  income credit at 15% refundable; the child and dependent care credit with
  its six-band schedule and its moving cutoff; and the alternate tax as a
  ceiling on tax
- Known approximations: the single-filer low-income exemption; the dependent
  filer's personal credit; the gross-versus-limited federal care credit; the
  early childhood development, tuition and textbook, and volunteer
  firefighter credits; married-separate credit mechanics; the minimum tax
  credit

### `filing.yaml`

- Encoded: the $9,000 and $13,500 net-income thresholds, unchanged and
  unindexed in every year
- Known approximations: the elderly thresholds; the net-income add-backs; the
  dependent and nonresident thresholds; the file-regardless cases

## New machinery this state required

- `st_ord.combined_sep_std_amount` — a per-column standard deduction stated
  outright, because Iowa's married-separate amount is not a clean fraction of
  its joint one ($2,210 each against $5,450 joint in TY2022)
- `st_credits.lic_thresh_aged` and `st_credits.lic_aged_min_age` — the
  elderly variant of the alternate tax and low-income exemption thresholds

The combined-separate column deduction was also generalised so that
deductions beyond the standard or itemized amount — Iowa's federal income tax
deduction above all — are prorated between the columns by income share, which
is what the form does. That change is inert for Kentucky and Delaware, whose
state deduction is only the standard or itemized amount.

## Worksheet tests added

IA-1 the nine-bracket ladder; IA-2 the uncapped federal tax deduction
reaching a standard-deduction filer; IA-3 the combined return beating joint
filing by $1,162.61; IA-4 the TY2023 federal-taxable-income base; IA-5 the
flat 3.8%; IA-6 the alternate tax binding for a joint filer near the
threshold; IA-7 its self-nullification for single filers; IA-8 and IA-9 the
retirement exclusion losing its cap; IA-10 head of household taking the
doubled exemption credit; IA-11 the earned income credit; IA-12 the care
credit at 50% of federal; IA-13a and IA-13b its cutoff moving from $45,000
to $90,000.

## Research findings worth flagging

- **Through TY2022 the nine-bracket ladder applies to every filing status.**
  There is no married rate schedule at all. What Iowa offered married couples
  instead was the COMBINED RETURN — two columns of one form, each running the
  same ladder against one spouse's income with its own standard deduction —
  which is why a two-earner couple almost always split. Bracket variation by
  filing status begins in TY2023 and ends with the flat rate in TY2025.
- **Iowa's federal income tax deduction reached STANDARD-deduction filers**,
  under Iowa Code 422.9(1) and (2)(b). Every other federal-deduction state in
  this set either caps it, confines it to itemizers, or both.
- **An erratum in a primary source.** IDR's own 2023 Statistical Report,
  Table 1, prints the third TY2023 bracket as "5.70% over $31,050" — that is
  the TY2024 indexed figure. The same report's Figure 1 and narrative, the
  2022-11-21 press release and the statute all say $30,000. **Use $30,000 for
  TY2023.** TY2023 amounts were not indexed at all; the statutory
  $6,000/$30,000/$75,000 were used as written, and TY2024 applies a 1.035
  factor.
- **The widely-cited "3.9% flat in 2026" is stale.** SF 2442 superseded it;
  the flat rate is 3.8% and IDR's 2025-10-21 release confirms 3.8% for TY2026.
- **The retirement exclusion's age test is 55, not 65**, and it is equally
  available to a disabled taxpayer or a qualifying survivor at any age. HF
  2317 did not change the test — it simply struck the cap.
- **Head of household gets the doubled $80 personal credit**, the same as a
  married couple, instruction-verified in both TY2021 and TY2023.
- **The alternate tax's rate was defined as the top marginal rate through
  TY2024** and then hard-coded by SF 2442 at 4.3% — which is *above* the 3.8%
  flat rate, so from TY2025 it only ever helps taxpayers near the thresholds.
- **The exemption credits, the low-income exemption thresholds and the filing
  thresholds have not moved in nine years** and are not indexed: $40/$80/$40/
  $20/$20 and $9,000/$13,500/$24,000/$32,000, verified identical in the 2017
  and 2026 Code editions.
- Iowa was never subject to the federal SALT cap on its own Schedule A —
  422.9(2)(l) says IRC 164(b)(6) "does not apply in computing taxable income
  for state tax purposes" — and it allowed itemizing even for a filer who took
  the federal standard deduction.
- The child care credit's top tier moved from a $45,000 cutoff to $90,000
  **retroactively to TY2021** (2021 Acts ch 177), and the credit is computed
  off the GROSS federal section 21 credit "without regard to whether or not
  the federal credit was limited by the taxpayer's federal tax liability".

## Known differences

- **The Iowa AMT is the largest unencoded feature.** It ran through TY2022 at
  6.7% (TY2017-18) and 6.4% (TY2019-22), both read directly off Form IA 6251,
  with exemptions of $26,000/$35,000/$17,500 phased out at 25% above
  $112,500/$150,000/$75,000. It was not an election. Encoding it needs an Iowa
  AMTI base built from federal AMTI plus Iowa nonconformity adjustments, which
  the model has no plumbing for, so pre-2023 tax is understated for
  high-income filers with large preference items — and the 422.11B minimum tax
  credit that went with it is likewise missing.
- **The single-filer low-income exemption** (form 41-146) is unencoded. For
  every status that may use the alternate tax the alternate ceiling is far
  tighter and governs, so only the single version is missing, and it binds in
  a narrow band just above the threshold — barely at all from TY2023, when the
  embedded federal standard deduction leaves a filer at those income levels
  with little Iowa taxable income.
- The alternate tax base has its own add-backs (the federal deduction, the
  personal exemption deduction, QBI, the pre-2023 NOL carryover and lump-sum
  distributions). The model uses federal AGI as a single stand-in for both the
  pre-2023 and post-2023 constructions, which agrees on the large items.
- Military retirement pay and 10 U.S.C. 1447 survivor benefits were fully
  excluded *in addition to* the general exclusion pre-2023, so a military
  retiree got both. Not separable from pension income.
- The Iowa QBI deduction phase-in (25%, 50%, then 75% of the federal amount
  for TY2019-TY2022), the Iowa-specific Schedule A adjustments in both
  directions, and the IA 104 high-income itemized limitation are all
  unencoded — the last because the form was not retrieved.
- The early childhood development credit (mutually exclusive with the care
  credit), the tuition and textbook credit, and the volunteer
  firefighter/reserve peace officer credit all run on spending or occupation
  that is not a model input.
- The school district and EMS surtaxes are levied as a percentage of state
  income tax and vary by district; deferred to the locality phase like the
  Maryland county piggyback.
- Iowa's separate lump-sum distribution tax, and the NOL elections that
  disqualify a spouse from the low-income exemption and the alternate tax, are
  not model inputs.

## Cross-model validation notes

- TAXSIM years 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: every two-earner married cell before TY2023 turns
  on whether the external model implements the combined return; high-income
  pre-2023 cells will diverge wherever the Iowa AMT would have bitten;
  TY2017-2022 cells turn on whether the external model allows the federal tax
  deduction to standard-deduction filers and on its cash-basis versus
  liability construction; retiree cells before TY2023 turn on the military
  add-on; and cells just above $13,500 of income turn on the alternate tax.

## Aggregate validation notes

- HT2 targets once weights land; IDR's Annual Statistical Report gives a
  revenue-agency benchmark with per-bracket detail.

## Unverified

- The Tax Calculation Worksheets (41-026) for TY2023 and TY2024 were not
  retrieved. The brackets are doubly sourced from the statute and the press
  releases, so nothing turns on it.
- The stated inflation-factor percentages for TY2018, TY2019, TY2021 and
  TY2024 were not found; the bracket amounts themselves are transcribed.
- No TY2017 instruction booklet exists on IDR's site; TY2017 parameters come
  from the Code and the statistical report.
- Whether the federal AMT counts toward the federal income tax deduction is
  inferred from the instructions' silence.
- Whether filing status 4 (married filing separately on separate returns) got
  the $9,000 or the $13,500 low-income threshold pre-2023 is genuinely
  ambiguous — the statute says $9,000 and the TY2021 instructions say
  $13,500. The encoding maps married-separate to the joint threshold for the
  alternate tax, following the instructions, and to the single threshold for
  the filing requirement, following the TY2025 instructions.
- Form IA 104's limitation parameters, the school district surtax rate tables,
  and the 422.7 additions and subtractions beyond the retirement items were
  not collected.
