---
title: "This workstream has moved to Tax-Data"
role: notes
workstream: state_weights
status: current
updated: 2026-09-11
sot: self
supersedes: []
superseded_by: null
---

# Moved to Tax-Data (2026-09-11)

Decision (JI, 2026-09-11): everything that *builds* a population or a weight —
the non-filer pool, the residual anchors, the split state weights — lives in
**Tax-Data**. Tax-Simulator keeps the state tax *law* (`config/scenarios/
tax_law_state/`, `research/state_tax/`, the state calculator) and *consumes*
weights through the Tax-Data interface.

Copied on 2026-09-11 to `Tax-Data` (branch `asec-nonfiler-pool`), preserving
this tree's shape so internal citations still resolve:

| here | there |
|---|---|
| `research/state_weights/**` | `research/state_weights/**` (same layout, incl. gitignored `results/`) |
| `src/data/asec_tax_units.R`, `filing_model.R`, `state_weights.R` | `src/nonfilers/` (same names; `source()` lines rewritten) |
| `research/{README,CONVENTIONS,decisions_log}.md`, `tools/`, `archive/`, `docx_sources/nonfiler_proposal_jii.docx` | same paths |

**This copy is frozen.** Edit the Tax-Data copy. The files here are kept until
the Tax-Data copy has reproduced `13_/14_/15_` byte-for-byte and one full
`main.R` run has consumed a pool built there; then this directory and the three
`src/data/` modules are deleted in one commit, and `build_state_weights()` in
`src/data/state_weights.R` is replaced by a loader that reads
`state_weights_{year}.csv` from the Tax-Data interface (still the `placeholder`
method until that file exists).

Not moved: `research/state_tax/`, `research/source_packets/`, `research/STATUS.md`,
`research/raw/`, `research/releases/` — state law, which stays here.
