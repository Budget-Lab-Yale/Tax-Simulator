---
title: "Cross-model harness — triage history"
role: evidence
workstream: state_tax
status: frozen
updated: 2026-08-19
true_as_of: 2026-08-16
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# Cross-model harness — triage history

Two narrative passes that used to sit at the foot of the harness README
(`src/tests/state/cross_model/README.md`), moved here 2026-08-19 because the
README also stated per-state *status*, and the tracker
(`research/state_tax/state_parameter_rollout.csv`) is the single status surface.

The **status claims below are as of the dates in their headings and are not
current** — the July text predates 21 further jurisdictions. What is worth
keeping is the substance: which defects the triage surfaced, which
known-difference rows exist and why, and what each fix moved. Read it as a
record of how the harness reached its current accept-list, not as a punch list.

## Status as of 2026-07-19

Status as of 2026-07-19: **done** for AK FL NV SD TX WY (stubs), TN, NH,
and IL (TAXSIM window 100% clean; PE 2022–2024 at 99.2–99.5% clean; two
residual IL records are exactly 5% of property tax — the Schedule ICR
property-tax credit is a candidate encoding gap to research).
**in_progress** (harness run, triage open) for the other 16 encoded states
(OH and UT added 2026-07-19); dominant divergence stages per state are in
the reports. Notable since the 2026-07-18 baseline: the QBI crosswalk
repair (TAXSIM now computes QBID from mapped inputs) lifted the
federal-taxable-start states CO/ND/SC by 3–5 points in 2018–2020; CO's PE
window is now fully excluded (TABOR netting), so its verdict rests on the
TAXSIM window; and neither TAXSIM nor PolicyEngine models the OH Business
Income Deduction (excluded via `st_bid > 0` predicate — with it, OH passes
the PE window at 96.6–98.7% and sits at ~91% clean vs TAXSIM, residuals
documented as JFC-proxy and retirement-credit annotate rows).

## Update 2026-08-15 (CA close-out)

Update 2026-08-15 (CA close-out): the CA triage surfaced a cross-state
calculator defect — `do_taxes.R` zeroes the `*_item_ded` components for
federal standard-deduction takers, so independent-election states could
never itemize state-only. The fix preserves as-if-itemizing
`*_item_ded_potential` columns for the state pass, and the TAXSIM
crosswalk hands them in state mode ONLY for independent-election states
(handing them to coupled/fed-gated states lets TAXSIM unpin its election
from the federal one — verified regression, VA 2019). Effects: CA's
TAXSIM window cleared the bar at 0.965–0.981 clean (from 0.61–0.73) with
the UI-subtraction fix, CalEITC age-band/gate repairs, and seven CA KD
rows; DE/NY/MN/NC gained 20–33 points (DE 0.90–0.91, NY 0.84, MN
0.78–0.88, NC 0.91–0.97, crossing the bar 2018–2020) under the standard
crosswalk-exposure exclude rows added for each; WI strengthened to
0.97–0.99. A 2026-08-16 CalEITC residual dig also fixed a table-lookup
rounding bug (fractional incomes fell between whole-dollar FTB bins and
got $0), lifting CA to TAXSIM 0.966–0.982 and PE 0.943–0.967 (2021/2023
clear; 2022/2024 miss by roughly two low-income credit-margin records
each). A 2026-08-16 hardening batch added the CA HSA addback, the CalEITC
investment-income ceiling, and the model-wide US-obligation interest
subtraction (15% share assumption; neither external model takes an
equivalent input, so an ALL-states/both-models KD row excludes records
with txbl_int > 5,000 where the assumed subtraction can break the
tolerance) — 182 cells improved ≥0.3pp under the new exclusion, 8 moved
down ≤1pp on denominator composition, and every previously-cleared state
held its bar. The same day's credit-stack close encoded California's
CDCTC (FTB 3506 stepped tiers, found via the PE residual to the dollar)
and added the PE itemizer-exposure KD row (`xw_pe_unhanded_item`) —
**CA is now `done`**: all eight cells clear the bar (TAXSIM 0.969–0.985,
PE 0.965–0.995), the first broad state to clear both windows since IL.
Coupled and fed-gated states are unchanged. The stage classifier now
counts `st_earned_credit` in the state-EITC stage (CalEITC-style credits
live there, not in `st_eitc`).
