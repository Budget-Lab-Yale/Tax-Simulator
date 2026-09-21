# State Source Packet: Oregon

State: `OR`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-21`

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
- Expected mismatch reasons: the kicker, then the WFHDC and retirement income
  credits, the unmodelled special medical subtraction, and the federal pension
  subtraction in older and lower-income cells.

### Triage 2026-08-21 (first cross-model pass)

Two causes, and they separate cleanly.

**1. The kicker lands on the TY2019 return, not the even years.** The advance
note above guessed TY2018/2020/2022/2024; that is wrong, and the mechanism is
why: the kicker is credited on the *return* whose biennium produced the
surplus, computed from the PRIOR year's liability -- 17.171% of TY2018
liability appears on the TY2019 return. Among non-itemizers with liability
above $500, `diff / st_tax_pre_credit` has a mode at exactly **0.172
(n = 3,124)**, and the same ratio is exactly **0.000 in 2017, 2018 and 2020**.
So TAXSIM models the kicker only in 2019 within this window -- including *not*
modelling it for TY2017, when Oregon in fact paid a kicker. Excluding the
affected records takes the 2019 cell from 0.268 to **0.999**, which attributes
the whole of that year's gap to this one provision.

The kicker is excluded in the harness (`known_differences.csv`, OR/taxsim
2019) rather than treated as permanent: Tax-Simulator runs years in sequence
over the same tax units, so prior-year Oregon liability *is* available in a
production run even though a cross-sectional harness cell cannot see it. A
plan item tracks modelling it.

**2. Itemizers are the standard crosswalk-exposure class.** In kicker-free
2018, non-itemizers match at **0.910** and itemizers at **0.128**. Among
records both models itemize, the state itemized-deduction gap correlates with
`xw_unhanded_item` at **r = 0.975**, and only 0.2% of those records agree on
the deduction at all (gap p10 -5,610, p90 +17,152). Oregon elects
independently of the federal return and Schedule OR-A strips state income tax,
so the exposed population includes federal standard-deduction takers -- the
same shape as DC, CA, DE, NY, MN and NC. Excluded via the standard exposure
predicate.

**After both exclusions: 2017 0.891, 2018 0.904, 2019 0.999, 2020 0.640.**

### TY2020: the economic stimulus reduces the federal tax subtraction (2026-08-21)

2020 was the weak cell after the two exclusions above, and the cause is a
third, unrelated one -- an encoding gap of **ours**.

Backing TAXSIM's implied federal tax subtraction out of its own intermediates
(`v32 - v36 - max(v34, v35) - v33`; Oregon's `v33` is 0 in every year, its
exemptions being a credit) and comparing it with our `st_fed_tax_ded`:

| year | we agree | median gap (ours - theirs) |
|---|---|---|
| 2017 | 66.2% | 0 |
| 2018 | 65.4% | 0 |
| 2020 | **35.2%** | **+1,030** |

Among the 2020 mismatches only 12.7% agree and the median gap is **+1,800**,
and that gap times the 8.75% bracket rate **explains 92.9% of the liability
difference**. We were subtracting too much federal tax.

$1,800 is the TY2020 recovery rebate for a single filer ($1,200 EIP1 + $600
EIP2), and the per-cell medians confirm it -- single filers with no
dependents, the largest cell at n = 1,134, sit at a median gap of exactly
1,800. The remaining cells run below their nominal entitlements, which is what
the rebate's own AGI phase-outs produce.

**Oregon requires this and we were not doing it.** The 2020 Form OR-40
instructions, line 10 worksheet step 8: *"Enter any 2020 federal economic
stimulus payments you received in 2020 or 2021, plus your recovery rebate
credit, if any (Form 1040 or 1040-SR, line 30)"*, added at step 9 into the
total that reduces the deductible federal tax; the booklet summary states it
plainly at p.5. The 2021 booklet carries the same line for the third payment,
citing IRS Notice 1444-C.

Encoded as a new generic `st_ded.fed_tax_ded_less_rebate`, on for TY2020-2021
only, reading the model's existing `rebate` variable -- the federal baseline
already computes it at $1,800 per adult plus $1,100 per dependent for 2020 and
$1,400 for 2021, with the statutory phase-outs, so no new input was needed.
Tests OR-8 (2020 offset), OR-9 (2019 unaffected), OR-10 (2021 offset).

#### Result (re-run 2026-08-21, job 22984034)

| cell | before | after |
|---|---|---|
| 2020 TAXSIM | 0.640 | **0.918** |
| 2021 PolicyEngine | 0.712 | **0.877** |

**The 2021 PolicyEngine cell is the strongest evidence the fix is right.** It
was not the cell the fix was diagnosed on, it is a different tax year, and it
is checked against a *different external model* -- yet encoding Oregon's rule
moved it 16.5 points. Two independent models both apply the stimulus offset
and we now agree with both.

Oregon's TAXSIM window now reads 0.891 / 0.904 / 0.999 / 0.918 and the
PolicyEngine window 0.877 / 0.805 / 0.837 / 0.832. Still short of the bar in
every cell but one, but no longer with a known unencoded provision in it.

Caveat on the 2019 cell: its 0.999 rests on `n_clean` = 1,360 rather than the
~6,900 of the other years, because the kicker exclusion removes every record
with liability. It is a real result on a small residual population, not a
whole-year clearance.

Note the base is the full **entitlement** -- advance payments received *plus*
any rebate credit still claimed on line 30 -- not just the line 30 credit,
which is why the parameter reads the calculated `rebate` rather than a return
line. For most filers the payments were advanced and line 30 is zero.

Two false starts worth recording so they are not repeated. The taxable-income
gaps first looked like round multiples of 500, which was an artifact of
binning at 500 (only 25 records sit exactly on -2,000; the distribution is
continuous). And a first pass at the rebate hypothesis was wrongly dismissed
because it was tested on medians over *all* records, where the affected subset
is diluted; the signature only appears once the test is restricted to the
mismatches.

**Other states with a federal tax deduction.** Missouri's 2020 MO-1040
instructions contain no mention of stimulus, economic impact payments or the
recovery rebate credit, so its flag correctly stays off. Alabama's 2020
booklet is not in the NBER archive and Alabama's treatment is **unverified**;
its flag is off by default, which should be checked when Alabama is triaged.

## Aggregate validation notes

- HT2 targets once weights land; the Oregon Department of Revenue publishes
  annual personal income tax statistics for a revenue-agency benchmark, and the
  Legislative Revenue Office publishes kicker forecasts that would help size
  that known difference.
