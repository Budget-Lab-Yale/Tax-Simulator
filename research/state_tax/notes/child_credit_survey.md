---
title: "State child credit survey — which structures the calculator can express"
role: notes
workstream: state_tax
status: current
updated: 2026-08-19
true_as_of: 2026-08-13
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# State child credit survey — which structures the calculator can express

Last updated: `2026-08-13`

Written to answer a narrow question that kept recurring during the batch-C
rollout: the tiered-CTC selector was generalized from three tiers to n because
NM needs seven — but is a tier count the only thing that varies across the
states still to be encoded, or do some publish a credit the calculator cannot
express at all regardless of tier count? Structures, not amounts. Amounts belong
in each state's source packet.

## Encoded (9 states)

| State | Parameter shape | Notes |
|---|---|---|
| AZ | `dep_credit_style` | flat per-dependent with a per-$1k phase-out |
| CO | `ctc_style` 1/2 + `fatc_*` | AGI-tiered per-child, plus the Family Affordability credit |
| ID | `ctc_style` | flat per-child |
| IL | `ctc_pct_of_eitc` | percent of the state EITC, gated on a child under 12 |
| MD | `ctc_style` 3 | flat per child under 6 under a hard FAGI ceiling |
| MN | `cwfc_style` | combined child + working-family credit (M1CWFC) |
| NC | `ctc_style` (tiered) | TY2017 only; repealed into `st_child_ded` after |
| NY | `ctc_style` 1/2 | ESCC: federal-replica match, then flat amounts from 2025 |
| UT | `ctc_style` 3 | flat per-child, continuous phase-out |

## Not yet encoded

PolicyEngine carries a state child credit for **CA** (YCTC), **DC**, **GA**,
**NE**, **NJ**, **NM**, **OR**, **RI** and **VT**. RI's is a one-time 2022 child
tax *rebate*, which belongs to the rebate-netting class (P5 in
`cross_model/external_model_issues.md`) rather than to this family — RI is
encoded and correctly has no recurring child credit.

Of the rest, most are ordinary instances of shapes already present: a flat
per-child amount with a threshold or a linear phase-out. Four are not.

### 1. Tier count above three — NM (7 bands), NJ (6 bands)

**Resolved 2026-08-13.** The selector hard-coded three bounds, and a fourth
bound in YAML passed the parameter-name validator (it is a legal member of the
`ctc_tier{n}_bound` family) and was then silently ignored — every filer above
tier 3 credited zero. Tier count is now discovered from the columns present.
Two semantics are load-bearing and deliberately not delegated to
`st_band_index_upper`, which clamps: AGI above a state's last bound means
ineligible rather than bottom-tier, and the count is taken per row from the
non-NA bounds, so a 3-tier state sitting in a frame widened to 7 columns by
another state stays ineligible above its own third bound. A state with no
eligibility ceiling (NM's seventh band is "over $350,000") says so with a final
`.inf` bound. Tests MACH-7 / 7b / 7c.

### 2. Greater-of two credits — OK

68 O.S. 2357.43 grants the greater of 20% of the federal CDCC and 5% of the
federal CTC. `st_ctc` and `st_cdctc` are summed, so encoding both legs
overstated by `min(20%CDCC, 5%CTC)`; the OK packet's original decision was to
encode the CTC leg alone and absorb the residual on care-expense units.

**Resolved 2026-08-13** by `st_credits.ctc_cdctc_greater_of`, which zeroes the
smaller leg in `calc_st_credits()` before either feeds the OH credit ordering or
the refundable/nonrefundable split. Zeroing rather than taking a max at the
aggregation site is what keeps the reported `st_ctc`/`st_cdctc` equal to what
was claimed, so no downstream aggregation needs to know an election happened.
Tests MACH-9 / 9b.

### 3. Phase-out applied per child, not to the aggregate — VT

VT reduces **each child's** $1,000 by $20 per $1,000 of AGI over $125,000, so
the credit is `n x max(0, 1000 - reduction)`. We computed
`max(0, n x 1000 - reduction)`, which floors once for the return instead of once
per child. The two agree until the reduction exceeds one child's amount, and
then diverge in a way that matters: at AGI $185,000 the aggregate reading pays
$800 to a two-child family VT phased out entirely at $175,000.

PolicyEngine agrees with the aggregate reading and therefore disagrees with VT's
published table — worth an issues-doc entry when VT is encoded, since the
harness will show a PE-side residual there rather than an our-side one.

**Resolved 2026-08-13** by `st_credits.ctc_po_per_child`, plus
`st_credits.ctc_po_round_up` for VT's "or fraction thereof" step. The rounding
needed its own flag because NY 2025, the other style-2 state, rounds the excess
DOWN — the two choices are independent and VT needs both. Tests MACH-8 .. 8d.

### 4. Statutory cap on the qualifying-child count — OR

The Oregon Kids Credit caps the credit at five children. **No parameter needed,
and none should be added**: `st_n_dep_in()` counts the up-to-three dependent age
slots, so a cap of five can never bind on this data. Document it as a data
limitation when OR is encoded, the way MN's CWFC already does ("no child
limit -- dependent slots cap tracked children at three"). Adding a
`ctc_child_limit` parameter would be dead code that reads as coverage.

## What this means for the remaining batches

Nothing else in the unencoded set needs new child-credit machinery. NM, NJ, OK
and VT can each now be encoded exactly on this family; OR needs a
`documented_not_modeled` line rather than a parameter. The four resolutions are
generic — no state-name branches — and the rest of the suite proves them neutral
for all 28 encoded states, while MACH-7..MACH-9b prove each one works.
