---
title: "Pass-through entity taxes: proposed methodology"
role: method
workstream: state_tax
status: open
updated: 2026-11-11
sot: research/state_tax/plan.md
supersedes: [research/archive/ptet_methodology_2026-11-11_pre-reported-income.md]
superseded_by: null
---

# Pass-through entity taxes: proposed methodology

For review (JI). This is the short statement of how the model would treat
pass-through entity taxes. The argument and evidence are in
`research/state_tax/ptet/ptet_proposal.md`; the steps and checkpoints are in
`research/state_tax/plan.md` section 3.2; the law is in
`research/state_tax/ptet/ptet_state_law.csv`, with draft per-state parameter
files in `research/state_tax/ptet/params_draft/{st}/ptet.yaml`. This version
rests on decision S48 (`research/decisions_log.md`): the microdata report
pass-through income net of the PTET, and the model works back to income before
it. Section 2 explains why. The earlier version, which went the other way, is
`research/archive/ptet_methodology_2026-11-11_pre-reported-income.md`.

## 1. What is being modeled

A pass-through entity tax (PTET) is an elective state income tax that a
partnership or S corporation pays on its owners' shares of income. The entity
deducts the tax on its federal return without limit, so the owners' state tax
escapes the federal cap on the deduction for state and local taxes (SALT). The
state then makes the owners whole, with a credit against their own state income
tax or by excluding the income from it.

An electing owner's return therefore shows two things that a non-electing
owner's does not: pass-through income lower by the PTET, and an itemized
deduction for state income tax lower by the part of the owner's state tax the
entity paid. Federal revenue falls. The owner's own state liability falls by the
credit or exclusion, and the state collects the PTET instead, outside the
individual income tax.

Today the model handles this with `do_salt_workaround_baseline()`
(`src/calc/do_taxes.R`), which moves a share of itemizers' state income and
sales tax deduction into a business loss before the federal calculation, at the
same rate in every state and every year including 2017. The state calculator has
no PTET credit, which is the leading explanation for modeled New York resident
liability running 1.36 to 1.49 times the state's published figure from tax year
2021, against 1.08 to 1.17 before.

## 2. The microdata already report income net of the PTET

### What the file contains

The Tax-Data baseline this design assumes is John Ricco's generative model
(Tax-Data branch `rebuild-joint-model`, `research/rebuild/README.md`). It
generates tax-unit records and post-trains each year's file to that year's IRS
counts and amounts by income bracket. Those amounts are what taxpayers
reported. From 2018, and in volume from 2021, an electing owner's reported
partnership or S-corporation income is net of the PTET the entity paid, and the
owner's itemized state income tax is net of the tax the PTET replaced. So each
record's pass-through income and SALT are reported figures, and the netting sits
where the IRS data put it across the income distribution.

The records carry no state. The State-Weights interface splits each record
across states, fitting to IRS Historical Table 2 state totals of AGI, SALT and
the other reported items, so the split is also fit to reported amounts.

### The identity, and which side is held fixed

For a record r assigned to state s, with Y_t its pass-through income before the
PTET and Y_r its reported income:

```
Y_r = Y_t - PTET[r, s]        PTET[r, s] = elect[r, s] x rate[s] x Y_t
```

A record is split across several states, and it cannot hold both quantities
fixed in all of them. **The design holds reported income fixed**, because the
file value is a reported figure. Income before the PTET then depends on the
state:

```
Y_t[r, s] = Y_r / (1 - elect[r, s] x rate[s])
```

Two owners who report the same income, one electing in New York and one in
Texas, had different incomes before the PTET: the New York owner's was higher by
the tax the entity paid. Both have the same federal return.

### Why not hold income before the PTET fixed

The alternative treats the file value as income before the PTET, shaded
evenly by Tax-Data's aging, and subtracts a state-specific PTET from it. That is
the right reading of one particular kind of file, and it is the kind production
Tax-Data builds today: on `main`, records are 2017 PUF returns, from before any
state had a PTET, and every later year multiplies each record's pass-through
income by one national factor, the growth in average reported income per return
in SOI Table 1.4 (`src/project_puf.R`). That factor is net of the PTET from
2021, so every record's income is pulled down by the same share, about 4 to 5
percent, whether the owner elected or not and whatever the state. Undoing that
even shading with one national gross-up, then subtracting each record's own
PTET, puts the netting back on the electing owners in PTET states.

The generative baseline has no such artifact. Its records are fit year by year
to reported amounts by bracket, not aged from a pre-PTET year by one factor.
Grossing every record up would raise the income of owners who never elected and
owners in states with no PTET, and would move each bracket's total off the IRS
amount the file was post-trained to match. Holding reported income fixed
leaves the file as fit and puts the PTET where it belongs: in the state
calculation, and in counterfactuals that change who elects.

Two consequences follow, and both make the build simpler:

- **The federal baseline needs no adjustment.** The reported file is already
  the federal baseline, so `do_salt_workaround_baseline()` is removed and
  nothing replaces it on the federal side of a baseline run.
- **Federal tax in a baseline run does not depend on state.** The state-group
  computation of federal tax in section 7 is needed only in counterfactual runs,
  and only for records whose election changes.

**When to revisit.** If production Tax-Data stays on the aged 2017 file, the
reasoning above reverses for that file, and the archived version of this note
describes the matching design. Under this design an aged file keeps its even
shading on owners who did not elect, about 4 to 5 percent of pass-through income
from 2021; the validation in section 8 would show it as federal revenue too low
from non-electors and state liability too low in states without a PTET.

## 3. The PTET amount for each record and state

For each record, each state it is weighted to, and each entity type (S
corporation, partnership), where the owner elects:

```
PTET[r, s] = rate[s, year, entity] / (1 - rate[s, year, entity]) x max(0, Y_r) x available[s, year]
```

- `rate` is the state's top PTET rate. Graduated entity schedules (New York and
  four others) are recorded but not applied, because their brackets are on the
  entity's income, which the data do not carry.
- `available` is 1 only where and when a PTET is enacted. There is none in
  2017, and in 2018 to 2020 only in the handful of early states.
- The base is the income the PTET actually taxes, the owner's share before the
  PTET (decided, JI 2026-11-11).

*Example.* A record reporting $1,000,000 of S-corporation income in 2024, split
between New York and Texas. In its New York share, if it elects:
`Y_t = 1,000,000 / (1 - 0.109) = $1,122,334` and `PTET = $122,334`. In its
Texas share: `Y_t = $1,000,000`, no PTET. The federal return is the same in
both.

## 4. Who elects

A record elects in a state when its federal gain from electing exceeds a
threshold. The gain is approximately the federal marginal rate times the part
of the PTET that the SALT cap would otherwise disallow: all of it for someone
taking the standard deduction or already at the cap, less for an itemizer with
room under the cap. Take-up is then scaled by one parameter per entity type,
using each record's existing random draw for the workaround (`r.salt_workaround`),
so a record's draw does not change.

In a baseline run the gain is computed from the baseline federal calculation of
the reported file, which runs once and does not depend on the elections. The
elections then say whose reported income is net of a PTET and in which states.
They change nothing federally.

Because the rule depends on the gain, the $40,000 cap from 2025, its phase-down
and the return to $10,000 in 2030 change participation with no re-tuning, and
so does any reform to the cap.

*Example, continued.* At a 37% marginal rate and with the cap already used, the
gain is about `0.37 x 122,334 = $45,264`, so the record elects in New York,
subject to the take-up draw.

## 5. State treatment

State adjusted gross income starts from federal AGI, which for an electing
owner is net of the PTET. For each state the record is weighted to:

1. **Add the PTET back** where the record elects in that state and the state
   requires owners to add back the entity's deduction (`st_ptet.addback`), so
   the state starts from income before the PTET.
2. **Apply the state's own relief**, by `st_ptet.method`:
   - *Credit* (most states): a credit of `credit_share x PTET` (1.0 in New
     York, 0.875 in Connecticut from 2019, 0.9 in Massachusetts and Maine),
     refundable or limited to tax as the state provides.
   - *Exclusion* (AR, GA, LA, NC, OK, SC, WI): no credit; the owner excludes
     the pass-through income on which the entity paid.
   - *None* (PA, DC, DE, ND, VT, and every state before enactment): the state
     taxes the income as reported.

In a state where the record does not elect, nothing changes: reported income is
income before the PTET.

The state SALT add-back keeps reading the file's `salt_inc_sales`, which is the
reported Schedule A amount and already net of the PTET (plan section 5, item 7
stands).

*Example, continued.* New York adds the $122,334 back, taxes $1,122,334, and
allows a refundable credit of $122,334.

**For review.** In credit states where owners do not add the entity's deduction
back by law (Connecticut, Missouri), step 1 is skipped and the state taxes
reported income. For Alabama, Kentucky, Mississippi and West Virginia the
research did not settle the add-back, and the files assume none.

Not modeled: PTET paid to a state other than the owner's, and the resident
credit for it; local entity taxes (New York City); Massachusetts's second
entity tax above its surtax threshold from 2026.

## 6. Counterfactuals: when an election changes

A reform that changes the gain from electing (the SALT cap, federal rates, a
ban on the deduction, a state's PTET law) can change who elects. For each
record and state where the election changes from the baseline:

- **Stops electing.** Reported income rises to income before the PTET, by the
  PTET. The itemized state income tax rises by the state tax the owner now pays
  directly: `credit_share x PTET` in credit states; in exclusion states, the
  state tax on the formerly excluded income, which the state calculator
  supplies in state mode and which is approximated by the PTET in federal-only
  runs.
- **Starts electing.** Reported income falls by the PTET, computed on reported
  income since there was none before (`rate x Y_r`), and the itemized state
  income tax falls by the same relief amount, down to zero.

A reform that changes no election leaves the reported file unchanged.

*Example, continued.* A reform that removes the SALT cap removes the gain. The
New York share's reported income rises to $1,122,334 and its itemized state
income tax rises by $122,334; the Texas share is unchanged.

## 7. Two modes

- **State-mode runs (the reference).** The baseline federal calculation runs
  once. In a counterfactual, records whose election changes in some state need
  federal tax computed per group of states, because their reported income now
  differs by state. States are grouped into K rate buckets a year (about 12; to
  be fixed after measuring run time and memory), each bucket's row carrying the
  record's weighted-average rate within the bucket, and the results are combined
  using the record's state weights. Records whose elections do not change keep
  one federal row.
- **Federal-only runs.** The baseline is the reported file with no PTET
  calculation. A counterfactual uses a national rate by year and entity type,
  stored in the federal law files and derived from a state-mode run, to price
  each record's gain and the amount that moves when its election changes. A
  reform that changes the incentive needs its own series; the model stops with
  a clear error if it is missing.

## 8. Calibration and validation

**Calibrate** the two take-up parameters so that the baseline's elected PTET in
tax year 2024 matches the Joint Committee on Taxation's amounts moved off
individual returns: $30.9 billion for S corporations and $33.1 billion for
partnerships. In the baseline this decides only who is an electing owner and
where; it moves no federal total.

**Check, without tuning to them:**

| Check | Published | Model today |
|---|---|---|
| Federal cost, 2024: a counterfactual with no PTET available, against the baseline | $18.1 billion | $15.8 billion |
| Share of that cost in the top 1% | about 80% | to be measured on the same ranking |
| New York credits claimed | $12.44 billion | none |
| Modeled liability over the New York, California and Connecticut agency figures | should fall toward about 1.1 | 1.36-1.49, 1.31, 1.20-1.30 |

## 9. What changes in results, and what needs sign-off

- **Federal baseline revenue rises in every year** by today's workaround cost:
  $3.2 billion in 2017 and $11 to 16 billion a year from 2018. The reported data
  already carry the PTET, so today's function counts it a second time. This
  needs John Ricco's agreement, and a check of the baseline against CBO
  receipts.
- **Federal cost of the PTET** becomes a counterfactual quantity (no PTET
  against the baseline), checked against the Joint Committee on Taxation's
  $18.1 billion.
- **State liability** falls in credit and exclusion states from the year of
  enactment. Modeled state individual income tax then excludes the entity tax,
  so comparisons with Census collections, which include it, need the entity tax
  shown alongside.

## 10. Where this stands

Decided (JI): the calibration source; the two modes and the rate buckets; the
top rate; enacted law only; a series per reform; the base; reported income held
fixed across a record's states, assuming the generative Tax-Data baseline
(2026-11-11, S48).

Open:
- The number of buckets and whether collapsed per-record flags may be fractions
  (after measurement).
- The sign-off package.
- The add-back handling in section 5.
- The law items still on secondary sources, listed in
  `research/state_tax/ptet/params_draft/README.md`.
- The Joint Committee on Taxation's 85% individual share of partnership PTET:
  it converts entity totals to individual totals, so it belongs in the $33.1
  billion target, which already applies it, and not in each record's amount,
  whose base is already the owner's own share.
- Whether the gain in section 4 should net the qualified business income
  deduction (the PTET also lowers it, so taxable income falls by about 0.8 of
  the PTET) and the state cost where the credit is below 100%.
- The timing of the generative Tax-Data baseline, which this design depends on.

Built so far, by the note's previous version: a test pinning the current
function and its 2017 and 2024 totals; a reordering of the adjustment,
verified to leave baseline totals unchanged; the state PTET parameters, parsed
and not yet read by any calculator. None of these was on GitHub as of
2026-11-11; the parameter files there are the drafts in `params_draft/`.

## Revision history

- **2026-11-11** — Rewritten for reported income held fixed (S48): the
  federal baseline takes no PTET adjustment, income before the PTET is
  inferred state by state, the state side adds back only where the record
  elects, and counterfactuals move reported income when an election changes.
  Section 2 records why the true-income design was set aside. Previous version
  archived.
- **2026-11-11** — written for review.
