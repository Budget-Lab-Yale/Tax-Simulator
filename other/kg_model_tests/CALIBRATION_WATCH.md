# Calibration staleness watch

Detects when a commit touches an input that a calibration constant
(`KG_DYN_DEFAULT_ETA`, `KG_DYN_TIMEABLE_SHARE`, `SIGMA_CONV`, and — since the
2026-07-19 functional-form toggle — `KG_DYN_DEFAULT_ETA_LOGS`,
`KG_DYN_TIMEABLE_SHARE_LOGS`) depends on, so a stale calibration can't ride into
production unnoticed (the failure mode that let σ sit 2× wrong for a week in July
2026). Built 2026-07-12.

## Per-form anchor (levels vs logs)

The realization response has two selectable functional forms (`KG_RESPONSE_FORM
= levels | logs`; see the `KG_DYN_RESPONSE_FORM` header in
`src/sim/kg_dynamics.R`). Each is a **complete, independent calibration** to the
**same local moment** — full-model realization semi-elasticity `E_full = −2.52`
(= −0.6/0.238) long-run, and the `5.04` (= 1.2/0.238) short-run announcement
moment — but with its own `eta` and `timeable_share`. `calibration_reference.csv`
carries a row per constant per form; `calibration_moment_check.R` keys the row it
checks off `KG_RESPONSE_FORM` (`KG_DYN_DEFAULT_ETA` for levels,
`KG_DYN_DEFAULT_ETA_LOGS` for logs) and solves the Tier-A internal moment through
the live form's cost primitive automatically. Run the Tier-A watch **once per
form**:

```bash
# levels (default)
sbatch other/kg_model_tests/calibration_watch.sbatch <baseline>
# logs
KG_RESPONSE_FORM=logs sbatch --export=ALL,KG_RESPONSE_FORM=logs \
  other/kg_model_tests/calibration_watch.sbatch <baseline>
```

The Tier-A "η IS the long-run semi-elasticity" framing is levels-specific; the
logs rows carry their OWN reference moment (the internal long-run semi at the
logs `eta_tilde`, seeded after the logs pin).

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

## Re-pinning the logs form (net-of-tax)

The logs form calibrates to the **same** two moments as levels, under
`KG_RESPONSE_FORM=logs`:

1. **η̃ (long run).** eta-dial protocol with `KG_ETA_LOGS` on the grid (env
   `KG_RESPONSE_FORM=logs KG_ETA_LOGS=<val>`), 3 fresh vintages of the +5pp
   shock; invert the E_full(η̃) line at −2.52 (expect η̃* ≈ 1.9). A confirmation
   run at η̃* must land E_full within ±2% of −2.52.
2. **timeable_share_logs (short run).** Given η̃*, 1-D root-find on the full sim
   against the 5.04 announcement moment (start from 0.2542). η̃ first, then the
   share — the long-run moment is timeable-invariant (the overlay nets to zero
   under permanent shocks in both forms).
3. **Stamp.** Paste `KG_DYN_DEFAULT_ETA_LOGS` / `KG_DYN_TIMEABLE_SHARE_LOGS`,
   fill the `logs` entry in `KG_DYN_CALIB_PROVENANCE`, and update the two
   `*_LOGS` rows in `calibration_reference.csv` (value, date, `code_sha` = the
   stamping commit; seed the eta row's `reference_moment` via
   `KG_RESPONSE_FORM=logs calibration_watch.sbatch <baseline> --seed`).

The levels calibration is untouched by the toggle (the byte-identity run proves
it), so `KG_DYN_SPEC_VERSION` stays 3.
