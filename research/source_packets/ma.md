# State Source Packet: Massachusetts

State: `MA`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-21`

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

- Encoded: `payroll_ded_cap` $2,000 per person; the TY2017-TY2020
  dependent-care regime, both branches -- `care_exp_ded` /
  `care_exp_ded_per_dep_cap` $4,800 / `care_exp_ded_dep_limit` 2 /
  `care_exp_ded_age_limit` 12 for Form 1 line 12, and `care_hh_member_amt`
  $3,600 / `care_hh_member_age_limit` 11 for line 13, with
  `care_ded_mfs_ineligible` barring both to married filing separately. Every
  one of these goes to zero in 2021
- Known approximations: public-retirement contributions are not a model input;
  the rental deduction and the TY2021-TY2022 dependent-care *credits* are
  documented but not modelled; line 13 members qualifying by age 65-plus or by
  disability are not counted (the same unobserved-qualifier limit the Virginia
  care deduction carries)

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

5. `st_ded.care_hh_member_amt` + `st_ded.care_hh_member_age_limit` +
   `st_ded.care_ded_mfs_ineligible` (added 2026-08-21) -- a flat
   per-qualifying-member alternative to the existing Virginia-style
   care-expense deduction, taken instead of it when larger, with its own age
   test and a shared married-filing-separately bar. This is the general shape
   of Form 1 lines 12 and 13: two mutually exclusive branches over the same
   qualifying-member count, one expense-based and one flat. Virginia is
   unaffected (both new parameters default to neutral).

The 4% surtax reuses the existing `st_surtax.taxable_income_*` component, and
the line 12 branch reuses the `st_ded.care_exp_ded` family already built for
Virginia -- only the count cap is shared between the two branches, because
Form 1 caps both at two members.

## Worksheet tests added

MA-1 payroll deduction at its cap; MA-2 the TY2017 rate; MA-3a/MA-3b
short-term gains at 12% and 8.5%; MA-4 No Tax Status; MA-5 the Limited Income
Credit binding; MA-6 exemptions and the Child and Family Tax Credit; MA-7 the
surtax threshold not doubling for joint filers; MA-8 Social Security excluded
with long-term gains included; MA-9 the earned income credit at 40%; MA-10 the
$3,600 household-member deduction (line 13, no expenditure needed); MA-11 the
care-expense branch (line 12) taken over the flat one with the count cap
binding at two; MA-12 both branches barred to married filing separately;
MA-13 the regime gone from TY2021.

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
  cells; and any filer with realized capital losses will diverge on the netting
  rules.

### Triage 2026-08-21 (first cross-model pass)

Clean-subset match@$100 was flat at 0.550-0.571 across all four TAXSIM years
with no year structure at all, and we ran high on 79-84% of mismatches -- the
signature of a missing deduction rather than a rate or bracket error.

**The dominant cause was the unencoded TY2017-2020 dependent-care regime, and
the residuals identified it to the dollar.** Dividing the liability difference
by the headline rate gives the deduction TAXSIM allowed and we did not:

| dependents | 2018 modal implied base | 2020 modal implied base |
|---|---|---|
| 1 | 3,600 | 3,500 |
| 2 | 7,200 | 7,100 |
| 3 | 7,200 (count cap binds) | 7,100 |

The 2020 figures are the same statutory amounts carried at that year's 5.0%
rate rather than 5.1%, which independently confirms the mechanism. The regime
is now encoded (see `ded.yaml` above); TAXSIM reaches the line 13 branch from
the dependent ages the crosswalk hands it, and the line 12 branch from
`care_exp`.

#### Result of the fix (re-run 2026-08-21, job 22970509)

Clean match@$100 moved 0.552/0.550/0.551/0.571 -> **0.623/0.626/0.636/0.657**,
and the important part is not the level but the bias:

| dependents | match before | match after | median difference before | after |
|---|---|---|---|---|
| 0 | 0.669 | 0.669 | +$0.00 | +$0.00 |
| 1 | 0.504 | 0.624 | +$23.60 | +$0.00 |
| 2 | 0.308 | 0.550 | +$191.00 | +$0.00 |
| 3 | 0.192 | 0.535 | +$302.00 | +$0.12 |

The systematic ours-high bias on family returns is gone; what is left is
dispersion. Records with a dependent but none under 12 correctly did not move
(1,353 records in 2018, median difference +$2.78) -- the line 13 age test
working as written.

#### The residual is now the line 11 payroll deduction, and it looks like
TAXSIM's error in both directions

After the fix the dominant implied base is +/-2,000 at every dependent count.
It splits into two clean groups, and on both our reading of Form 1 is the one
the booklet supports:

**We run $102 LOW on 741 records (implied base -2,000).** 96.4% married filing
jointly, median AGI $136,874, our deduction $4,000 -- both spouses at the
$2,000 cap. TAXSIM behaves as though the cap were $2,000 per *return*. Form 1
has separate lines 11a and 11b and the booklet says "the amount you, and your
spouse if filing jointly, paid ... up to a maximum of $2,000" each, so the cap
is per person and non-transferable. **We are right; TAXSIM under-allows for
two-earner couples.**

**We run $102 HIGH on 496 records (implied base +2,000).** 89.1% receive
Social Security, 71.8% are 65 or over, median age 69, and we allow **zero** on
53.4% of them because they have no payroll tax to deduct. TAXSIM allows about
$2,000 anyway. The booklet is explicit that Medicare *premiums* withheld from
Social Security or retirement payments are NOT deductible, which is the
obvious candidate for what TAXSIM is picking up -- but **the mechanism is
inferred from the retiree signature, not probed.** The measured fact is that
TAXSIM allows a payroll-contribution deduction on records with no earnings.

#### Probe verification, 2026-08-21

Both went through the TAXSIM probe protocol and both are confirmed. Synthetic
cases were run first through the local WASM build the harness uses and then
through the live NBER server via the `taxsim35` ado; the two agree exactly.
Artifacts are in `research/state_tax/cross_model/bug_reports/` as
`t16_ma_payroll_percap_*` and `t17_ma_payroll_no_earnings_*`.

**T16 — the cap is applied per return.** TY2018 Massachusetts joint, both
spouses age 40, wages 60,000 each. Expected 5,467.20; TAXSIM returns
**5,569.20**, an implied deduction of 2,000 against the 4,000 the two separate
lines allow. Across joint shapes the implied deduction is pinned at 2,000
regardless of the second earner's own contributions:

| primary / spouse wages | implied | correct | error |
|---|---|---|---|
| 60,000 / 0 | 2,000 | 2,000 | — |
| 60,000 / 60,000 | 2,000 | 4,000 | +102.00 |
| 60,000 / 30,000 | 2,000 | 4,000 | +102.00 |
| 60,000 / 10,000 | 2,000 | 2,765 | +39.02 |

A single filer at 60,000 returns the correct 2,000, so singles are unaffected.

**T17 — and it is broader than the residual suggested.** The probe corrected
the scope. It is not "retirees with no earnings": it is a **flat $2,000
granted to any Massachusetts return with positive gross Social Security**,
*additive on top of* any legitimate wage-based deduction. Holding everything
else fixed and varying `gssi` alone:

| gross Social Security | implied deduction | 7.65% of gssi |
|---|---|---|
| 0 | 0 | — |
| 5,000 | **2,000** | 383 |
| 10,000 | **2,000** | 765 |
| 20,000 | **2,000** | 1,530 |
| 60,000 | **2,000** | 4,590 |

So it is not FICA computed on the benefit either — $5,000 of Social Security
draws the full $2,000. And a wage-earner with Social Security gets 4,000
(2,000 correct + 2,000 spurious), so working filers are hit too, not only the
retirees visible in the residual. Age is irrelevant (a 40-year-old with
pension and no `gssi` correctly gets zero).

This is the same shape as the Utah retirement-credit issue already filed (T7):
a flat state amount keyed to positive `gssi` alone. The likely source remains
Medicare premiums withheld from Social Security, which the booklet expressly
disallows — but note that even on that reading the amount should be the
premiums actually withheld, not the statutory cap.

Because T17 fires on every Massachusetts return with Social Security and T16
on every two-earner joint return, the two partly cancel on a joint two-earner
couple who also receive Social Security.

Both are now known-difference exclude rows (`gross_ss > 0` and
`filing_status == 2 & ei1 > 0 & ei2 > 0`); `ei1`/`ei2` were added to the
harness exposure covariates for the second predicate, whose population cannot
be identified from filing status alone. Re-run 2026-08-21 (job 22984034):

| cell | before | after |
|---|---|---|
| 2017 | 0.623 | **0.735** |
| 2018 | 0.626 | **0.740** |
| 2019 | 0.636 | **0.745** |
| 2020 | 0.657 | **0.737** |

The exclusions are large -- `n_clean` falls from about 12,000 to about 7,150,
so roughly 40% of the clean subset now sits behind a known-difference row.
That is defensible only because both are probe-verified defects in TAXSIM
rather than judgement calls, but it does mean Massachusetts's remaining score
is measured on a narrower population and should be read with that in mind.

**Massachusetts is still ~0.74 against a 0.95 bar with the residual
unattributed.** Neither the dependent-care fix nor the two payroll exclusions
reach it. The next step is a synthetic probe of the kind that resolved the
payroll classes, not further residual arithmetic.

**Do not read the stage-diagnosis table for Massachusetts as a liability
finding.** TAXSIM's `v36_state_taxable_income` for MA omits the line 11
payroll deduction, so 2,159 of the 2018 records carry a $2,000 taxable-income
"gap" -- and 99.7% of them match on liability. The 5,600 and 9,200 gap modes
in that table decompose exactly as 2,000 (payroll, definitional) plus 3,600 or
7,200 (dependent care, the real gap). Trust `siitax`, per the harness README
caveat.

## Aggregate validation notes

- HT2 targets once weights land; the Massachusetts DOR publishes annual
  personal income tax statistics for a revenue-agency benchmark.
