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

## 3. Name what the HT2 ceiling caps, and what it does not

**The trigger at the end of this item has fired: SOI published HT2 TY2023 in
August 2026 and it was mirrored 2026-09.** The ceiling moves from 2022 to
2023, where Pub 1304 T1.6 already sits — so state shares and the national
anchor now end in the same year, and the 0.59% carried level corrections are
measurable for 2023 rather than carried.

What that changes, and what it does not: `02_build_residual_anchors.R` will
now take its MEASURED branch for TY2023 instead of the carried one, so a
TY2023 run is a different computation than it was — but nothing rebuilds on
its own, because the anchor pair is still 2017/2022 and every committed
anchor file is a 2017 or 2022 product. The open decision is whether to move
the pair to 2017/2023 (≈15 scripts' `ANCHOR_YEARS`, plus `HT2_REF_YEAR`, and
a full rebuild and re-verification of the anchors).

The original proposal, now moot for 2023 and still live for 2024+: state
weights for years past the latest HT2 hold the latest HT2 state shares
against the national path — the same carried-forward logic the level
corrections use — with the assumption named in the fit's documentation
rather than discovered.

## 4. Resume the per-state rollout on the settled base

The per-state law encoding (`state_tax/state_parameter_rollout.csv`) paused
while the population under it was being replaced. With the pool published,
the aging settled (S18), and the handoff basis aligned (S19), the rollout's
blocker is gone. State aggregate validation remains gated on production
state weights (item 1), per the standing protocol.
