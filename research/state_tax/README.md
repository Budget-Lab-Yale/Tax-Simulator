---
title: "State-tax encoding workstream"
role: index
workstream: state_tax
status: current
updated: 2026-08-19
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Workstream: per-state income tax encoding

Encoding each jurisdiction's individual income tax from primary DOR forms — *the form
is operational truth* — back to TY2017, with worksheet tests and cross-model
validation. Encoding coverage is complete (51/51 jurisdictions); see
[`../STATUS.md`](../STATUS.md).

| Role | Document |
|---|---|
| **plan** — start here | [`plan.md`](plan.md) |
| procedure | [`state_parameter_workflow.md`](state_parameter_workflow.md) — required per-state artifacts, source order, and the validation-gate vocabulary the tracker uses |
| tracker | [`state_parameter_rollout.csv`](state_parameter_rollout.csv) — **the single per-state status surface.** Packets and reviews point here; they do not restate it. |
| review | `CODE_REVIEW_2026_07_17.md` (22 states), `STATE_ENCODING_REVIEW_2026_08_11.md` (30 states) — read their `true_as_of:` before their counts |
| evidence | [`cross_model/`](cross_model/) — per-state triage reports, filed external-model bug packets, federal-divergence record |
| notes | [`notes/`](notes/) — cross-state feature surveys, the CA cluster, deferred designs |
| scripts | [`scripts/`](scripts/) — research-maintenance only |

Per-jurisdiction primary-source packets are workstream-agnostic and live at
[`../source_packets/`](../source_packets/); the verbatim research dumps behind eight of
them are in [`../raw/`](../raw/).

The cross-model harness **code** is not here — it is `src/tests/state/cross_model/`,
because a `src/` test may not depend on a path in a tree that gets reorganized.
