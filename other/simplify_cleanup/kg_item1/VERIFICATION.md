# Item #1 (+ bundled 2026-06-22 #9) — applied & verified

_2026-07-01, branch `wealth`. From `other/simplify_review_codebase_2026-07-01.md`._

## What changed (behavior-preserving)
`src/sim/kg_dynamics.R` (+ one comment in `src/sim/cohort_bathtub.R`):

- **Part A** — deleted `kg_dyn_build_aging_matrix` (an exact copy of
  `build_aging_matrix` in `cohort_bathtub.R` minus a contiguity `stopifnot`);
  the two call sites (`kg_dyn_run_bathtub_pass`, `kg_dyn_run_frozen_pass`) now
  call the shared `build_aging_matrix`.
- **Part B** — the four kg state dir/path helpers (`kg_dyn_state_dir/path`,
  `kg_dyn_mech_state_dir/path`) are now thin wrappers over the shared
  `cohort_state_dir` / `cohort_state_path` (subdir/pass fixed). Output paths
  unchanged.
- **#9** — deleted the duplicated `KG_DYN_ESTATE_ASSET_VALUE_COLS` literal;
  the 3 use sites now reference the canonical `ESTATE_ASSET_COLS`
  (`src/calc/functions/tax/estate.R`), which `wealth_dynamics.R` already uses.

No function signatures, state-file contracts, or pass sequencing changed →
**no `src/slurm/*` or CLAUDE.md sync-table edits needed**. Confirmed the SLURM
path is safe: `src/slurm/common.R::reconstitute_environment` sources all of
`./src` (incl. `cohort_bathtub.R`) with the same predicate as `main.R`.

## Proof
1. **Preflight identity** (`preflight_identity.R`, job 16932579): `PARSE_OK` +
   `ALL_IDENTITIES_PASS` — `build_aging_matrix` ≡ old matrix over multiple age
   grids; `ESTATE_ASSET_COLS` ≡ old literal (content + order); all four state
   paths reproduce the old `file.path` strings.
2. **Full-sample regression, original (HEAD worktree) vs edited (main tree)**,
   runscript = baseline + 4 kg_dynamics/turnover regimes (baseline_check,
   rate_up_5pp, carryover, deemed), `pct_sample=1`, `multicore=none`, `stacked=1`.
   Pre run from a detached worktree at HEAD `a142825e8` (original code); post
   from the edited tree. Same runscript (md5-verified identical in both trees).
   - **V1 (years 2025:2028)** — jobs 16933230/16933231, compare job 16935287:
     227/227 files, 0 rds mismatches, 0 csv/data mismatches, 13 xlsx
     timestamp-only (spurious), **0 genuine** → DATA-IDENTICAL.
   - **V2 (years 2026:2029, both exit 0)** — jobs 16935077/16935078, compare
     16936778: 249/249 files (adds the distribution/heir-allocation outputs),
     0 rds, 0 csv/data, 16 xlsx timestamp-only, **0 genuine** → DATA-IDENTICAL.

The only byte-differences anywhere are `.xlsx` files, whose sole differing zip
member is `docProps/core.xml` `<dcterms:created>` (openxlsx embeds the run's
wall-clock time; the two jobs ran seconds apart). All spreadsheet DATA identical.

NB: the 2025-start runs exit 1 on a *downstream, unrelated* step —
`process_for_distribution` hard-stops because Estate-Tax-Distribution has no
`estate_tax_detail_2025.csv` (interface starts 2026) while the `deemed` scenario
has 2025 deemed tax. It fails identically on original + edited code and only
after all 227 data files are written; the 2026-start V2 avoids it entirely.

## Artifacts (full paths)
- Comparator: `other/simplify_cleanup/kg_item1/compare.R`
- V1 verdict log: `other/simplify_cleanup/kg_item1/logs/compare_16935287.out`
- V2 verdict log: `other/simplify_cleanup/kg_item1/logs/compare2_16936778.out`
- Run scripts + runscripts: `other/simplify_cleanup/kg_item1/run_*.sh`,
  `config/runscripts/tests/kg_item1_regression{,_v2}.csv`
- Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_sr_{pre,post,pre2,post2}`
