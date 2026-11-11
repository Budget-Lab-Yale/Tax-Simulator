---
title: "This workstream lives in Tax-Data"
role: notes
workstream: state_weights
status: current
updated: 2026-11-10
sot: self
supersedes: []
superseded_by: null
---

# Moved to Tax-Data — copied 2026-09-11, deleted here 2026-09-12

Decision (JI, 2026-09-11): everything that *builds* a population or a weight —
the non-filer pool, the residual anchors, the split state weights — lives in
**Tax-Data**. Tax-Simulator keeps the state tax *law*
(`config/scenarios/tax_law_state/`, `research/state_tax/`, the state
calculator) and *consumes* weights through the Tax-Data interface.

| was here | is now, in Tax-Data |
|---|---|
| `research/state_weights/**` | `research/state_weights/**` — same layout, so every internal citation still resolves |
| `src/data/asec_tax_units.R`, `filing_model.R` | `src/nonfilers/` — same names |
| most of `src/data/state_weights.R` | `src/nonfilers/state_weights.R` |
| `src/tests/test_state_weights.R`, `test_asec_tax_units.R` | `src/tests/` |
| `research/{README,CONVENTIONS,decisions_log}.md`, `tools/`, `archive/` | same paths (the S-series log is maintained there now) |

**What stayed, and why it is only this.** `src/data/state_weights.R` still
exists: the jurisdiction set, the interface reader and `build_state_weights()`,
the runtime dispatcher `src/sim/run.R` calls. Nothing else in this repo ever
touched the moved code — verified before deletion by grepping every export of
the three modules across `src/`, which returned nothing outside the modules
and their own tests. That asymmetry is the same one
`research/CONVENTIONS.md` states for research scripts: a builder may read a
consumer's schema, but nothing on the run path may depend on a builder.

**The gate that had to pass first**, and did: a Tax-Data `main.R` run consumed
a pool built in Tax-Data end to end (pool vintage 2026091201, Tax-Data output
vintage 2026091119), and a Tax-Simulator baseline run consumed that output.

**What this repo reads.** Tax-Data publishes `state_weights_{year}.csv.gz`
through the `State-Weights` interface (its `src/main_state_weights.R`, since
2026-09-29), and `build_state_weights(method = 'interface')` here reads it,
checks coverage and that each record's state weights sum to its national
weight, and filters to the requested states. The default vintage is the
production fit `2026102315_rebuild` (since 2026-10-24). The `placeholder`
method, 1/53 of the national weight per jurisdiction, remains only for
reproducing old A/B runs; its state levels are not estimates.

Not moved: `research/state_tax/`, `research/source_packets/`,
`research/STATUS.md`, `research/raw/`, `research/releases/` — state law, which
stays here.
