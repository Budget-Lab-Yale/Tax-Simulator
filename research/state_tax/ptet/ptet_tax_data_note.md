---
title: "Pass-through entity taxes: note to John Ricco on the Tax-Data side"
role: notes
workstream: state_tax
status: open
updated: 2026-11-11
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Pass-through entity taxes: what the design assumes about Tax-Data, and what it asks

For John Ricco, from John Iselin. This note stands on its own. The full design
is `research/state_tax/ptet/ptet_methodology.md`; the build plan is
`research/state_tax/plan.md` section 3.2.

## The short version

A pass-through entity tax (PTET) is an elective state income tax that a
partnership or S corporation pays on its owners' shares. The entity deducts it
federally without limit, so the owners' state tax escapes the cap on the
deduction for state and local taxes (SALT), and the state gives the owners a
credit or an exclusion in return. An electing owner's return therefore shows
lower pass-through income and a lower itemized state income tax.

Tax-Simulator's new treatment starts from one premise: **the Tax-Data file
already reports both amounts net of the PTET, because it is fit to what
taxpayers reported.** Three things follow.

1. **The federal baseline takes no PTET adjustment.** Today
   `do_salt_workaround_baseline()` moves a share of itemized state tax into a
   business loss before the federal calculation. On a file fit to reported
   amounts that subtracts the PTET a second time, so it is removed.
2. **The state calculator attributes who elected, state by state,** and works
   back to income before the PTET (`reported / (1 - rate)`) to apply each
   state's add-back, credit or exclusion.
3. **Reported income moves only when an election changes:** in a reform, and in
   projection years as the law changes (below).

Nothing has changed in any result yet. The new treatment is built behind a
switch that defaults to the old method.

## What I need you to agree to

**The federal baseline rises when the switch is turned on**, by what the current
function costs. From a full-sample run on Tax-Data vintage `2026102315_rebuild`
(`research/state_tax/ptet/results/workaround_national.csv`), in billions of
dollars of individual income tax:

| 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 |
|---|---|---|---|---|---|---|---|---|
| 3.2 | 11.1 | 11.6 | 11.7 | 14.7 | 16.2 | 15.1 | 15.8 | 15.1 |

The 2017 figure is an outright error today: no state had a PTET. For later
years the argument is that the income targets are reported amounts. The federal
cost of the PTET (the Joint Committee on Taxation puts it at $18.1 billion for
2024) then becomes a counterfactual quantity, a run with no PTET against the
baseline, and stops being something subtracted inside the baseline. If the
baseline is reconciled to CBO receipts anywhere with the current function in
place, that reconciliation would need to be redone.

The switch will not be turned on until the model can change elections in a
counterfactual, so that a reform to the SALT cap still moves them.

## What the design assumes about the file: please confirm or correct

1. **Partnership and S-corporation income are fit to reported IRS amounts by
   income class, year by year**, through the last observed year. In the
   generative model (Tax-Data branch `rebuild-joint-model`) I understand this is
   the post-training step. In the file in production today, the aged 2017
   Public Use File, each record's pass-through income grows on one national
   factor of reported growth, so the total is net of the PTET but the netting is
   spread evenly, about 4 to 5 percent from 2021, over owners who did and did
   not elect. The design runs on either; it is right about who bears the netting
   only on the generative file.
2. **`salt_inc_sales` is the reported Schedule A amount**, so an electing
   owner's is already lower by the state tax the entity paid.
3. **Records carry no state**, and the split state weights are fit to reported
   state totals from IRS Historical Table 2.

## What this asks of the Tax-Data process

1. **Make no PTET adjustment in Tax-Data.** No gross-up to income before the
   PTET, and no election flag. The simulator needs the reported amounts and
   attributes elections itself, because the election depends on state law and
   on the scenario.
2. **Keep the fields the calculation reads, separately:** partnership and
   S-corporation income in their active, passive, loss and section 179 pieces;
   the specified-service flags `sstb_part` and `sstb_scorp`; and
   `salt_inc_sales` apart from `salt_prop` and `salt_pers`.
3. **Publish the last observed year for pass-through income and for itemized
   state tax**, in the variable guide or the vintage manifest. From the first
   projected year the simulator compares each year's elections under that
   year's law with the elections under the last observed year's law and moves
   reported income for records whose election differs. The SALT cap is $40,000
   from 2025 and returns to $10,000 in 2030, and Minnesota's and Oregon's PTETs
   end after 2027, so projected years cannot simply inherit the last observed
   year's netting.
4. **Say what projected pass-through income and itemized state tax grow on.**
   If the projection follows a forecast that already anticipates the higher
   cap, the simulator's projection-year step would count that response twice,
   and one of the two has to give way.
5. **Keep record ids stable in meaning.** The election uses the existing
   per-record random draw for the workaround, keyed on record id, so that a
   record's draw is the same in every scenario. If the generative file draws a
   fresh population each year, a record does not persist across years and that
   is fine; if records do persist, their ids should too.
6. **Consider targeting pass-through income by state in the split state
   weights.** With top PTET rates applied to 2024 pass-through income under the
   current weights, 73% of S-corporation income is in states with a PTET, close
   to the Joint Committee on Taxation's 71.7%, but 78% of partnership income is,
   against its 61.5%. Part of that gap may be where partnerships operate against
   where partners live, which the file cannot see. Part may be the weights,
   which do not target pass-through income by state as far as I know.

## Questions

1. Is assumption 1 right for the generative model, and through which year?
2. Which year will be the last observed one for pass-through income and for
   itemized state tax when the generative file becomes the production vintage,
   and when is that expected?
3. Do you agree with removing the federal adjustment, given the table above?
   Is there a receipts reconciliation that has it baked in?
4. On item 6: is partnership and S-corporation income by state a feasible
   target for the state weights?
