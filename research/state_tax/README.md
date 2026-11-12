---
title: "State-tax encoding workstream"
role: index
workstream: state_tax
status: current
updated: 2026-11-11
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Workstream: per-state income tax encoding

Encoding each jurisdiction's individual income tax from primary DOR forms — *the form
is operational truth* — back to TY2017, with worksheet tests and cross-model
validation. Encoding and cross-model validation are complete (51/51
jurisdictions); what is next is in the plan. See [`../STATUS.md`](../STATUS.md).

| Role | Document |
|---|---|
| **plan** — start here | [`plan.md`](plan.md) |
| procedure | [`state_parameter_workflow.md`](state_parameter_workflow.md) — required per-state artifacts, source order, and the validation-gate vocabulary the tracker uses |
| tracker | [`state_parameter_rollout.csv`](state_parameter_rollout.csv) — **the single per-state status surface.** Packets and reviews point here; they do not restate it. |
| review | `CODE_REVIEW_2026_07_17.md` (22 states), `STATE_ENCODING_REVIEW_2026_08_11.md` (30 states) — read their `true_as_of:` before their counts |
| method | [`supplemental_inputs_interface.md`](supplemental_inputs_interface.md) — optional data inputs for the state calculator, published by another model (decision S43) |
| registry | [`tax_data_gaps.csv`](tax_data_gaps.csv) — every state provision the pinned Tax-Data vintage cannot compute as written; checked by `scripts/check_tax_data_gaps.R` |
| evidence | [`cross_model/`](cross_model/) — per-state triage reports, filed external-model bug packets, federal-divergence record |
| evidence | [`aggregate/`](aggregate/README.md) — gap diagnostics: modeled state liability against Census collections and NY, CA and CT agency tables |
| evidence | [`acs_test/`](acs_test/README.md) — which data gaps the ACS could fill |
| notes | [`ptet/`](ptet/ptet_proposal.md) — pass-through entity taxes: the proposal ([`ptet_proposal.md`](ptet/ptet_proposal.md)), the design ([`ptet_methodology.md`](ptet/ptet_methodology.md)), the note on the Tax-Data side ([`ptet_tax_data_note.md`](ptet/ptet_tax_data_note.md)), the state-by-year law table ([`ptet_state_law.md`](ptet/ptet_state_law.md)), the encoding record for the per-state parameters ([`params_draft/`](ptet/params_draft/README.md)) |
| notes | [`notes/`](notes/) — cross-state feature surveys, the CA cluster, deferred designs |
| scripts | [`scripts/`](scripts/) — research-maintenance only |

Per-jurisdiction primary-source packets are workstream-agnostic and live at
[`../source_packets/`](../source_packets/); the verbatim research dumps behind eight of
them are in [`../raw/`](../raw/).

The cross-model harness **code** is not here — it is `src/tests/state/cross_model/`,
because a `src/` test may not depend on a path in a tree that gets reorganized.
