# Calibration staleness watch

Detects when a commit touches an input that a calibration constant
(`KG_DYN_DEFAULT_ETA`, `KG_DYN_TIMEABLE_SHARE`, `SIGMA_CONV`) depends on, so a
stale calibration can't ride into production unnoticed (the failure mode that
let σ sit 2× wrong for a week in July 2026). Built 2026-07-12.

## Pieces

| file | role |
|---|---|
| `calibration_reference.csv` | **single source of truth**: per-constant shipped value, the moment it was pinned to, last-derivation date + SHA, and the `;`-separated invalidation file list. |
| `.githooks/pre-push` | **detector** (pure git, no compute). Blocks a push whose commits touched any invalidation file — or bumped an interface `default_vintage` line — since the constant's recorded SHA, unless the row's SHA was advanced. Prints the exact measurement command; runs nothing. |
| `calibration_watch.sbatch` | **Tier A** (cheap, minutes): recomputes the kg eta long-run internal moment at the shipped eta and compares to the reference. `--seed` writes the reference from a clean state. |
| `calibration_moment_check.R` | the Tier A engine (generalized from `eta_estate_check.R`). |
| `calibration_watch_sigma.sbatch` | **Tier B** (expensive, on σ-trigger): two-leg top-ordinary ETI → solves σ* and compares to the shipped 0.16. |

## Activate (one time)

```bash
git config core.hooksPath .githooks
```

Version-controlled, so this points every clone at `.githooks/`. There is no
prior `core.hooksPath` to clobber. Bypass an intentional, calibration-neutral
push with `SKIP_CALIB_WATCH=1 git push`.

## The acknowledge-by-SHA workflow

The detector fires on `git diff <row.code_sha>..HEAD` ∩ invalidation files. It
goes **green** for a constant only once that row's `code_sha` in
`calibration_reference.csv` is advanced to a commit at/after the change. So the
discipline on any commit that changes a calibration input is:

1. Re-derive if warranted (Tier A / Tier B command the hook printed).
2. Update the constant + its `calibration_reference.csv` row (value, date,
   `code_sha` = the commit you're making, and `reference_moment` for eta via
   `calibration_watch.sbatch <baseline> --seed`).
3. Commit both together. The next push is clean.

This is DETECT-only: the watch never re-pins. Humans re-derive and sanction.

## Re-pinning eta (Tier A drift → full-sim re-pin)

If Tier A reports DRIFT (or you changed a Bellman primitive), re-pin on the full
simulator, do **not** paste the miniature's number:

1. Run `top_tax/eta_dial` at ~3 eta values straddling the expectation (see
   `other/top_tax/eta_dial/`), estate offset live, on the production Tax-Data
   vintage.
2. `sbatch other/top_tax/eta_dial/measure_efull_repin.sbatch` → `eta_repin_fit.csv`
   gives `eta* = |E_full_target| / slope`, `E_full_target = -0.6/0.238 = -2.52`.
3. Update `KG_DYN_DEFAULT_ETA` + `KG_DYN_CALIB_PROVENANCE`, then re-seed Tier A.

The old dilution-bridge calibrator (`calibrate.R`) is retired to a drift
diagnostic — it no longer produces shipped values.
