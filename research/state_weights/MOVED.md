---
title: "This workstream lives in Tax-Data"
role: notes
workstream: state_weights
status: current
updated: 2026-09-12
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
exists, at 87 lines: the jurisdiction set and `build_state_weights()`, the
runtime dispatcher `src/sim/run.R:430` calls. Nothing else in this repo ever
touched the moved code — verified before deletion by grepping every export of
the three modules across `src/`, which returned nothing outside the modules
and their own tests. That asymmetry is the same one
`research/CONVENTIONS.md` states for research scripts: a builder may read a
consumer's schema, but nothing on the run path may depend on a builder.

**The gate that had to pass first**, and did: a Tax-Data `main.R` run consumed
a pool built in Tax-Data end to end (pool vintage 2026091201, Tax-Data output
vintage 2026091119), and a Tax-Simulator baseline run consumed that output.

**What this repo still owes.** `build_state_weights()` has only its
`placeholder` method — every jurisdiction gets 1/53 of the national weight, so
state levels are not estimates. When the Phase 1 fit lands in Tax-Data it
publishes `state_weights_{year}.csv`, and this dispatcher gains a method that
READS that file from the pinned interface. It does not regain a fitting engine.

Not moved: `research/state_tax/`, `research/source_packets/`,
`research/STATUS.md`, `research/raw/`, `research/releases/` — state law, which
stays here.
