---
title: "State weights + non-filer workstream"
role: index
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Workstream: split state weights and the non-filer rework

Building production state weights for the microsim, and reworking the non-filer
population they rest on. These are **one** workstream with one plan: the non-filer
rework lands *before* the Phase 1 swap-in, so the fit happens once on upgraded margins
rather than fit-then-refit.

| Role | Document |
|---|---|
| **plan** — start here | [`plan.md`](plan.md) |
| method — non-filers | [`nonfiler_residual_design.md`](nonfiler_residual_design.md) — the design of record: the *why*, and each decision with its evidence |
| method — the fit | [`state_weights_phase1_summary.md`](state_weights_phase1_summary.md) — the bake-off record and the adopted hyperparameters |
| procedure | [`nonfiler_federal_validation.md`](nonfiler_federal_validation.md) — the federal validation runbook |
| evidence | [`nonfiler_residual/04_findings.md`](nonfiler_residual/04_findings.md) — Stage D findings F1–F7, frozen; result CSVs in `nonfiler_residual/results/` |
| notes | [`notes/`](notes/) — engine root-cause record, deferred alternatives, the proposal-rewrite plan |
| scripts | [`scripts/`](scripts/) — `sweep_state_weights.R` and `validate_state_weights.R` are parts 1 and 2 of the Phase 1 comparison harness |

The narrative case for the methodology, for co-authors and outside readers, is a
Word-native document with tracked changes:
[`../docx_sources/nonfiler_proposal_jii.docx`](../docx_sources/nonfiler_proposal_jii.docx).
It is a *different document* from `nonfiler_residual_design.md`, not a render of it.

Point-in-time Word copies of the plan and status are cut with
`research/tools/render_release.R state_weights_plan` — see
[`../CONVENTIONS.md`](../CONVENTIONS.md).
