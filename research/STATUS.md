---
title: "State income tax workstream — status"
role: status
workstream: cross-cutting
status: current
updated: 2026-11-10
sot: self
supersedes: [research/archive/STATUS_2026-10-25_pre-rewrite.md]
superseded_by: null
---

# State income tax workstream — status

What is true today, as pointers. The plan is `research/state_tax/plan.md`; settled
arguments are in `research/decisions_log.md`; per-state status is one row of
`research/state_tax/state_parameter_rollout.csv` and nowhere else. The running
log this page used to carry (2026-07 to 2026-10-22) is
`research/archive/STATUS_2026-10-25_pre-rewrite.md`.

## What is true today

**Law and calculator.** All 51 jurisdictions are encoded from primary sources for
TY2017-2025, enabled, and in `states = all`: 41 states with a broad income tax
plus DC, the narrow investment-income taxes of NH and TN, WA's capital-gains
excise and Working Families Tax Credit, and six no-tax stubs. Law lives in
`config/scenarios/tax_law_state/baseline/{st}/`, the calculator in
`src/calc/state/`, worksheet tests in `src/tests/state/test_state_calc.R`.

**Cross-model validation is complete (2026-10-22).** Every jurisdiction's
`cross_model` cell in the tracker is `done`: on records whose federal results
agree, at least 95% are within $100 of TAXSIM-35 in every year 2017-2020 and of
PolicyEngine US in every year 2021-2025.

- Scores: `research/state_tax/cross_model/results/summary.csv`.
- Every remaining difference is a row of
  `src/tests/state/cross_model/known_differences.csv` (324 rows: 285 excluded,
  39 annotated), attributed to our encoding (fixed), the external model
  (written up), or an input neither side observes.
- External-model findings, TAXSIM T1-T81 and PolicyEngine P1-P61:
  `research/state_tax/cross_model/external_model_issues.md`. Not yet sent.
- How each state closed: `research/state_tax/cross_model/triage_history.md` and
  the source packets.

**State weights are production weights.** The default `State-Weights` vintage is
Tax-Data's production fit `2026102315_rebuild` (fitted TY2017-2023, later years
carried on the TY2023 shares), read through the interface. A state-mode row must
pin Tax-Data vintage `2026102315_rebuild`. The producer and its plan are in
Tax-Data; `research/state_weights/MOVED.md` records the split.

**State-mode runs and gap diagnostics exist (2026-10-26).** Full-sample
`states = all` baseline runs for TY2017-2025 are on the production root. The
current one is `sw_states_nfgate` (runscript
`config/runscripts/tests/states_all_components.csv`, after the fix that a federal
non-filer files no state return). `research/state_tax/aggregate/README.md`
describes the comparison to published figures. Headlines from that run:

- **Against Census state tax collections** (individual income tax, 43 states
  with a series): the national ratio of modeled liability to collections is
  0.97-1.02 for TY2017-2022, 1.06 for TY2023 and 1.09 for TY2024. In TY2022, 25
  of 43 states are within 10%. Low: ND, DE, NH. High: NJ, CT, MD, and from
  2023 CO and NE.
- **Against state agency tables by income class** (resident liability): NY is
  1.08-1.17 times the Department of Taxation and Finance figure for TY2017-2020
  and 1.36-1.49 for TY2021-2024; CA is 1.04-1.11 times the Franchise Tax Board
  figure through TY2020 and 1.31 in TY2022; CT is 1.20-1.30 for TY2021-2024. The
  jump sits above $2M of state AGI and coincides with the credits for
  pass-through entity taxes, which the model does not have.

These are diagnostics, not estimates: collections include nonresidents and
timing, and the largest gaps have a named cause that is not yet modeled.

**Data gaps are one registry.** `research/state_tax/tax_data_gaps.csv` (614 rows)
lists every state provision the pinned Tax-Data vintage cannot compute as
written. A blocked provision is encoded against an optional input and switches
on when a producer publishes it (`research/state_tax/supplemental_inputs_interface.md`,
decision S43).

## What is open

In order; details and checkpoints are in `research/state_tax/plan.md` section 3.

1. **Documentation** — this rewrite, the plan, the indexes, and the convention
   checker back to zero findings.
2. **Pass-through entity taxes (PTET)** — the state-level workaround to the
   federal cap on the state and local tax deduction. Today a federal-only
   function approximates it and the state calculator has nothing. Proposal:
   `research/state_tax/ptet/ptet_proposal.md`; law table:
   `research/state_tax/ptet/ptet_state_law.csv`; draft parameters:
   `research/state_tax/ptet/params_draft/README.md`. Nothing is implemented.
3. **TY2026 state law** — seven states have known unencoded provisions (SC, ND,
   DC, RI, NM, MT, CT), and most states the tracker calls `carry-forward` hold
   2025 dollar amounts fixed in 2026 rather than indexing them.

**Waiting, not scheduled:** the data-gap imputations (rent and property tax, the
pension source split, household resources); JI's review of the circuit-breaker
provisions (decision S46); sending the TAXSIM and PolicyEngine reports; CO and WI
AMT documentation; CA miscellaneous deductions and AMT; the Oregon kicker;
interstate migration; local income taxes; coupled federal-state iteration. Each
is listed in the plan's section 7.

**Merging to `main`.** This branch (`state-gap-diagnostics`, on top of
`cross-model-refit-filers`) is 333 commits behind `origin/main` as of
2026-11-10, and `main` has since moved state law to
`config/scenarios/tax_law/default/state/{st}/`. The merge is therefore a path
migration, not the clean trial merge recorded on 2026-10-22.

## Calls JI may want to revisit

- Michigan's TY2022 earned income credit is encoded at the statutory 30% (PA 4
  of 2023, retroactive), though the extra 24 points were paid as separate
  checks; PolicyEngine uses the 6% printed on the return (issue P61).
- One-time state payments that an external model books to a tax year (New York's
  2022 supplemental credits, Maine's 2025 affordability payment) are excluded
  from the comparison rather than modeled (decision S46).
- Wisconsin's 2025 Act 15 retirement-income election is documented in the WI
  source packet and not encoded.

## Where history lives

| For | Read |
|---|---|
| The running log through 2026-10-25, including each encoding batch | `research/archive/STATUS_2026-10-25_pre-rewrite.md` |
| The plan as it stood through the build (phases 0-6, as-built design) | `research/archive/state_tax_plan_2026-10-22_pre-rewrite.md` |
| Settled decisions (S-series) | `research/decisions_log.md` |
| Cross-model triage, state by state | `research/state_tax/cross_model/triage_history.md` |
| The non-filer and state-weights work | Tax-Data, via `research/state_weights/MOVED.md` |
