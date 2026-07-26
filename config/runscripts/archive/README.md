# Archived runscripts

Runscripts in this directory are retired from the live tree and are NOT
maintained against the current runscript schema. They are kept as a record of
past analyses.

Archived 2026-07-25 as Phase 1 of the configuration-system redesign
(three-leg scenario architecture: tax_law / economy / behavior). Last commit
at which the live tree ran on the old schema: tag `cfg-p0`.

Contents:

- `public/` — historical published-analysis runscripts, archived wholesale.
  Most reference dependency vintages that no longer exist on disk; all use the
  pre-redesign schema (`dep.*` columns, `behavior` module paths, `s` /
  `wealth_financing` / `assumption.*` columns), which hard-errors after the
  redesign. To revive one, rewrite it to the new schema (see
  `config/scenarios/README.md` for the retired-column mapping) — or check out
  `cfg-p0` and run it there.
- `tests/` — dead test runscripts: 8 that use the pre-`years` schema
  (`first_year`/`last_year`, unparseable since that column was retired), 7
  that reference interfaces deleted from `interface_versions.yaml`
  (`dep.Corporate-Tax-Model`, `dep.Estate-Tax-Model`), plus `kg_5pp.csv` and
  `long_run_child_earnings.csv`, which reference behavior modules that no
  longer exist (`kg_lt/*`, `child_earnings/old_34`). The kg elasticity-grid
  sweep is expressible in the new scheme as dotted `behavior.kg_static.*`
  overrides in a fresh runscript.

Note: `config/runscripts/private/` is untracked by design (.gitignore) and is
therefore not archived here; it remains in place. Its runscripts are also
old-schema and will hit the same retirement errors if run.

## `retired_2026_07_26/`

71 runscripts retired during Phase 3c of the rebuild. Each still carries at
least one retired column (`dep.{Model}.vintage` / `.ID`, `s`,
`wealth_financing`, or the dotted `economy.interfaces.*` form from the abandoned
branch), so all of them hard-error at parse. That is intended: the author ruled
on 2026-07-26 that the only runscripts worth migrating were the OBBBA
retrospective, the top-tax batches, and the most recent Kim Clausing runs.

Several of these were development or verification tooling rather than finished
analyses, and are the ones most likely to be wanted back:

| File | What it was for |
|---|---|
| `tests/simplify_smoke_fast.csv` | the fast leg of the refactor byte-diff harness |
| `tests/kg_item1_regression.csv`, `_v2.csv` | the kg regression pair |
| `tests/sigma_recal_eta.csv` | the sigma recalibration stack |
| `tests/hidden_ledger_smoke.csv` | hidden-ledger verification |
| `tests/corp_incidence.csv`, `corp_rate_smoke.csv`, `corp_2stream_smoke.csv` | corporate channel smokes |
| `tests/mtr_extensive_tips_ot.csv` | extensive-margin MTR check |
| `tests/perf_probe.csv` | the performance probe |
| `tests/wealth_bathtub_smoke.csv`, `warren_*`, `cgcarry_bound_*` | the wealth bounding set |

Reviving one is two steps: `git mv` it back out of this folder, then replace its
retired columns with an `economy` cell naming a folder under
`config/scenarios/economy/alternatives/`. Several of the pins these files use
already have an alternative folder (`ome_20250925`, `ome_20250925_saving_*`,
`multi_module_smoke` for the older Tax-Data vintage), so the migration is often
just picking the right one. `other/config_redesign/check_runscripts.R` confirms
the result parses and resolves.
