---
title: "State Parameter Workflow"
role: procedure
workstream: state_tax
status: current
updated: 2026-08-19
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# State Parameter Workflow

Purpose: keep state-parameter research, documentation, and record-level validation
moving in parallel with the state-weights work. We should not wait for Phase 6 to
start learning state law.

## What can happen before weights land

Everything below is weights-independent and should proceed now:

1. Build a source packet for the state.
2. Encode baseline YAML parameters under `config/scenarios/tax_law_state/baseline/{st}/`.
3. Add or extend worksheet-style unit tests in `src/tests/state/test_state_calc.R`.
4. Write known-differences notes for data limits, projected years, and omitted features.
5. Run record-level cross-model checks against TAXSIM and, where useful, PolicyEngine.

Only the last validation layer is weights-dependent:

6. Aggregate validation against SOI HT2 and state revenue-agency totals.

That means a state's parameter packet can be mostly complete before the weights are
ready. The tracker should show those states as `aggregate = blocked_weights`, not
`todo`.

## Required artifacts per state

For every jurisdiction we model, maintain the following:

1. A source packet using [TEMPLATE.md](./source_packets/TEMPLATE.md).
2. A baseline YAML directory with the six standard files:
   `agi.yaml`, `credits.yaml`, `ded.yaml`, `exempt.yaml`, `filing.yaml`, `ord.yaml`.
3. Inline `reference:` citations on every encoded subparameter.
4. At least one worksheet-style unit test covering each nontrivial feature family
   introduced by the state.
5. A short known-differences note in the source packet covering omitted provisions,
   approximations, and projected future years.
6. Tracker updates in
   [state_parameter_rollout.csv](./state_parameter_rollout.csv).

## Source order

Always work from sources in this order:

1. Current-year state forms and instructions from the state DOR.
2. Historical forms/instructions from the NBER historical archive and state prior-year pages.
3. Statutes and DOR technical bulletins for indexing rules, conformity rules, and phase-ins.
4. Secondary checks only after transcription:
   Tax Foundation, TPC, ITEP, PolicyEngine, TAXSIM.

The form is the operational truth. Statutes explain the rule that generates future
values. Secondary sources are cross-checks, not authority.

## Encoding rules

1. Anchor every year-keyed `value:` series at `2017` or earlier.
2. Put source citations in `reference:` on every subparameter.
3. Omit feature-not-present parameters where possible; `ensure_st_params()` supplies neutral defaults.
4. Comment clearly when a future-year value is projected rather than directly transcribed.
5. Keep a state's structure in the standard six files unless a reusable generic
   calculator component needs an auxiliary file. Document that component in the
   source packet; do not create state-specific calculation modules for a
   parameter-only difference.

## Generic component ledger

Update this ledger whenever state research exposes a reusable rule. It is a guard
against accumulating state-name branches in the calculator.

1. Federal-EITC matches must encode refundability separately from the match share.
2. Family-size credits that reduce preliminary tax use generic per-family-size
   income-bound and credit-share tables (Kentucky).
3. Exemption credits use generic personal/aged/blind/dependent amounts with a
   per-credit step phaseout (California).
4. Independent earned-income credits use either a generic child-count triangular
   schedule or a versioned row-based lookup table keyed by state, year, inclusive
   income bounds, capped child count, and credit amount. Dense tables may also
   carry a generic second-lookup/safe-harbor rule; never write state-specific
   credit logic (California CalEITC).
5. Refundable young-child credits should be a generic per-return amount with an
   age gate, earned-credit gate, and parameterized phaseout. Keep exceptional
   zero-income eligibility gates as explicit generic inputs and document any
   incomplete microdata proxy (California YCTC).
6. Fixed or selective federal conformity uses `conformity_groups.yaml` and a generic
   reference-law context. Calculate that reference base on current post-behavior
   records, cache it by conformity group, and express adoption differences in a
   federal overlay; never join a prior baseline record or add a state-name branch.
   A group remains `ready: false` until its full bridge and form cases are valid,
   and federal-reform runs must fail rather than use scenario federal outputs.

## Validation gates

Use this sequence for each state:

1. `source_packet = done`: primary sources assembled, links captured, feature inventory drafted.
2. `yaml_dir = done`: baseline YAML exists and passes parser/lint checks.
3. `worksheet_tests = done`: form-worksheet cases pass locally.
4. `cross_model = done`: TAXSIM or other cross-model spot checks documented.
5. `aggregate = done`: HT2 / revenue-agency validation complete after weights arrive.

A state with endpoint carry-forwards, a placeholder table approximation, or an
unimplemented fixed-conformity mechanism must remain `in_progress` for the affected
YAML or worksheet-test gate. It is not production-ready merely because its directory
parses.

Allowed tracker values are `todo`, `in_progress`, `done`, `blocked_weights`, and `n/a`.

## Suggested working rhythm

Use the repo in two parallel loops:

1. Research loop: pick a state, fill the source packet, decide the feature inventory,
   capture known differences, update the generic component ledger when needed, and
   mark the tracker.
2. Encoding loop: add YAML, add tests, and move the state to record-level validation.

This lets us arrive at Phase 6 with a queue of partly or mostly validated states,
rather than beginning research from zero.
