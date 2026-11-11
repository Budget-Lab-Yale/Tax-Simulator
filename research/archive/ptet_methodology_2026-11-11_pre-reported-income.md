---
title: "Pass-through entity taxes: proposed methodology"
role: method
workstream: state_tax
status: superseded
updated: 2026-11-11
sot: research/state_tax/plan.md
supersedes: []
superseded_by: research/state_tax/ptet/ptet_methodology.md
---

# Pass-through entity taxes: proposed methodology

For review (JI). This is the short statement of how the model would treat
pass-through entity taxes. The argument and evidence are in
`research/state_tax/ptet/ptet_proposal.md`; the steps and checkpoints are in
`research/state_tax/plan.md` section 3.2; the law is in
`research/state_tax/ptet/ptet_state_law.csv` and the per-state files
`config/scenarios/tax_law_state/baseline/{st}/ptet.yaml`.

## 1. What is being modeled

A pass-through entity tax (PTET) is an elective state income tax that a
partnership or S corporation pays on its owners' shares of income. The entity
deducts the tax on its federal return without limit, so the owners' state tax
escapes the federal cap on the deduction for state and local taxes (SALT). The
state then makes the owners whole, with a credit against their own state income
tax or by excluding the income from it.

Two effects follow, and the model should have both:

- **Federal.** Owners report less pass-through income and less itemized state
  tax. Federal revenue falls.
- **State.** Owners' own state liability falls by the credit or exclusion. The
  state collects the entity tax instead, outside the individual income tax.

Today the model has an approximation of the first and nothing for the second.
`do_salt_workaround_baseline()` moves a share of itemizers' state income and
sales tax deduction into a business loss, at the same rate in every state and
every year including 2017. The state calculator has no PTET credit, which is
the leading explanation for modeled New York resident liability running 1.36 to
1.49 times the state's published figure from tax year 2021, against 1.08 to
1.17 before.

## 2. One amount per record, used by both calculators

For each record, each state it is weighted to, and each entity type (S
corporation, partnership):

```
ptet = rate[state, year, entity] x max(0, pass-through income) x available[state, year]
```

- `rate` is the state's top PTET rate. Graduated entity schedules (New York and
  four others) are recorded but not applied, because their brackets are on the
  entity's income, which the data do not carry.
- `available` is 1 only where and when a PTET is enacted. There is none in
  2017, and in 2018 to 2020 only in the handful of early states.
- The partnership amount is multiplied by 0.85, the share of partnership PTET
  that benefits individuals in the Joint Committee on Taxation paper (Comey,
  Gallagher and Gorman 2025; `docs/papers/`).

The base is the income the PTET actually taxes (decided, JI 2026-11-11), not
the itemized deduction as today. Sales tax can no longer be moved, owners who
take the standard deduction are included, and the amount varies with each
state's rate and enactment year.

*Example.* A New York resident with $1,000,000 of S-corporation income in 2024:
`ptet = 0.109 x 1,000,000 = $109,000`.

## 3. Who elects

A record elects in a state when its federal gain from electing exceeds a
threshold. The gain is approximately the federal marginal rate times the part
of the PTET that the SALT cap would otherwise disallow: all of it for someone
taking the standard deduction or already at the cap, less for an itemizer with
room under the cap. Take-up is then scaled by one parameter per entity type,
using each record's existing random draw for the workaround, so a record's
draw does not change.

Because the rule depends on the gain, the $40,000 cap from 2025, its phase-down
and the return to $10,000 in 2030 change participation with no re-tuning, and
so does any reform to the cap.

*Example, continued.* At a 37% marginal rate and with the cap already used, the
gain is about `0.37 x 109,000 = $40,330`, so the record elects subject to the
take-up draw.

## 4. Federal treatment

The elected amount is entered as an active business loss for the entity type,
as today, and the itemizable state income tax falls by the same amount, down
to zero. Expanded income, used for distribution tables, adds the amount back.
(One existing wrinkle carries over and is now pinned by a test: where the
amount exceeds a record's combined partnership and S-corporation income, the
add-back overstates expanded income, by $0.5 to $0.7 billion in aggregate.)

## 5. State treatment

State adjusted gross income starts from the federal figure, which is now net
of the PTET. For each state the record is weighted to:

1. **Add the federal deduction back**, so the state starts from gross
   pass-through income. This reverses the 2026-07-12 decision that the state
   add-back for state taxes reads the value after the workaround.
2. **Apply the state's own relief**, by `st_ptet.method`:
   - *Credit* (most states): a credit of `credit_share x ptet` (1.0 in New
     York, 0.875 in Connecticut from 2019, 0.9 in Massachusetts), refundable or limited to tax
     as the state provides.
   - *Exclusion* (AR, GA, LA, NC, OK, SC, WI): no credit; the owner excludes
     the pass-through income on which the entity paid.
   - *None* (PA, DC, DE, ND, VT, and every state before enactment): the state
     taxes gross income.

*Example, continued.* New York adds the $109,000 back, taxes the full
$1,000,000, and allows a refundable credit of $109,000.

**For review.** In the few credit states where owners do not add the entity's
deduction back by law (Connecticut, Missouri), step 1 would overstate state
income. The proposed handling is to subtract the state's own PTET again where
`st_ptet.addback` is 0. For Alabama, Kentucky, Mississippi and West Virginia
the research did not settle the add-back, and the files assume none.

Not modeled: PTET paid to a state other than the owner's, and the resident
credit for it; local entity taxes (New York City); Massachusetts's second
entity tax above its surtax threshold from 2026.

## 6. Two modes

The PTET depends on the state, and a federal-only run has no state.

- **State-mode runs (the reference).** Federal tax is computed once per group
  of states with similar PTET rates, and the results are combined using the
  record's state weights. States are grouped into K rate buckets a year (about
  12; to be fixed after measuring run time and memory). Each bucket's row
  carries the record's own weighted-average rate within the bucket, so the
  record's total PTET is exact; only the nonlinearity of federal tax within a
  bucket is approximated. Each state's calculation reads the federal results of
  its own bucket.
- **Federal-only runs.** Each record uses a national rate by year and entity
  type, stored in the federal law files. The series is derived from a
  state-mode run, as the rate that reproduces that run's elected amount. A
  reform that changes the incentive needs its own series; the model stops with
  a clear error if it is missing.

## 7. Calibration and validation

**Calibrate** the two take-up parameters so that tax year 2024 matches the
Joint Committee on Taxation's amounts moved off individual returns: $30.9
billion for S corporations and $33.1 billion for partnerships.

**Check, without tuning to them:**

| Check | Published | Model today |
|---|---|---|
| Federal cost, 2024 | $18.1 billion | $15.8 billion |
| Share of the cost in the top 1% | about 80% | to be measured on the same ranking |
| New York credits claimed | $12.44 billion | none |
| Modeled liability over the New York, California and Connecticut agency figures | should fall toward about 1.1 | 1.36-1.49, 1.31, 1.20-1.30 |

## 8. What changes in results, and what needs sign-off

- **Federal baseline before 2021 rises.** The workaround today costs $3.2
  billion in 2017 and about $11 to 12 billion a year in 2018 to 2020. Under
  enacted law it is zero in 2017 and small until 2021. This needs John Ricco's
  agreement.
- **From 2021** the level is recalibrated from the Tax Policy Center's $20
  billion to the Joint Committee on Taxation's figures, and the mix shifts
  toward partnerships and toward high-rate states.
- **State liability** falls in credit and exclusion states from the year of
  enactment. Modeled state individual income tax then excludes the entity tax,
  so comparisons with Census collections, which include it, need the entity tax
  shown alongside.

## 9. Where this stands

Decided (JI): the calibration source; the two modes and the rate buckets; the
top rate; enacted law only; a series per reform; moving the federal adjustment
after the cost-of-living, capital and excess-growth adjustments; the base.

Open: the number of buckets and whether collapsed per-record flags may be
fractions (after measurement); the sign-off package; the add-back handling in
section 5; the law items still on secondary sources, listed in
`research/state_tax/ptet/params_draft/README.md`.

Built so far: a test pinning the current function and its 2017 and 2024
totals; the reordering, verified to leave the baseline totals unchanged; the
state PTET parameters, parsed and not yet read by any calculator.

## Revision history

- **2026-11-11** — written for review.
