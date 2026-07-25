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
