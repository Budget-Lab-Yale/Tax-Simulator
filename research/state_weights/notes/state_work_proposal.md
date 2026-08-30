---
title: "Proposal: the state work that follows the non-filer handoff"
role: notes
workstream: state_weights
status: open
updated: 2026-08-30
sot: research/state_weights/plan.md
supersedes: []
---

# Proposal: the state work that follows the non-filer handoff

Part (d) of the handoff (`handoff.md`). A proposal, not a plan — the plan of
record stays `plan.md` (S7: one plan per workstream). Four items, ordered by
what unblocks what.

## 1. Re-fit the split state weights on the new pool (group F)

The Phase 1 bake-off (config 7: β=1e-4 joint fit, 95.3% within 2%) was run
against the old non-filer partition. The pool changes the non-filer margin's
composition — real ages, observed sex, income receipt where DINA had zeros —
so the fit's non-filer targets and its demographic interactions should be
rebuilt before the production swap-in. Inputs ready: state anchors 2014–2022
(`residual_anchors_{year}.csv`), the pool for every year 2014–2023.

## 2. Complete the state margins for the calibration years

`nonfiler_wage_margin_{year}.csv` and `ssa_age_margin_{year}.csv` are skipped
for 2014–2016 for exactly one remaining reason: three SSA covered-earnings
workbooks (`eedata_sc14/15/16.xlsx`) that ssa.gov will not serve to this
cluster. QCEW is already backfilled. Drop the files in,
`13_verify_ssa_backfill.R` checks and registers them, and one anchor re-run
completes both products for all three years.

## 3. Name what HT2's 2022 ceiling caps, and what it does not

HT2 ends at 2022, so **state** shares end there; the **national** anchor
reaches 2023 with carried level corrections (0.59%). Proposal: state weights
for 2023+ hold the 2022 state shares against the national path — the same
carried-forward logic the level corrections use — with the assumption named
in the fit's documentation rather than discovered. Revisit when SOI publishes
HT2 2023.

## 4. Resume the per-state rollout on the settled base

The per-state law encoding (`state_tax/state_parameter_rollout.csv`) paused
while the population under it was being replaced. With the pool published,
the aging settled (S18), and the handoff basis aligned (S19), the rollout's
blocker is gone. State aggregate validation remains gated on production
state weights (item 1), per the standing protocol.
