# Correctness review — post-processing, sim orchestration, data/parsing, SLURM drift (2026-07-02)

Follow-up to the 2026-07-01 calc-layer review (5 fixed bugs, `other/calc_fixes/`).
This pass covered the layers that review did NOT: `src/data/post_processing/`,
`src/sim/` orchestration, `src/data/` parsing, `src/misc/`, and main.R↔SLURM
drift. Method: 4 parallel area reviews → per-finding verification against
current code before any fix.

## Fixed this pass

| # | Where | Bug | Commit |
|---|-------|-----|--------|
| 1 | economy.R `generate_indexes` | 1970 CPI growth NA'd out at the historical splice → `kg_lt_cpi_ratio` understated ×1.0572 for gains purchased ≤1969 | 087a51623 |
| 2 | revenue.R `calc_receipts` | on-model estate delta took the `excess_growth_all_rev` factor even though `est_tax_exp` already embeds excess growth (do_excess_growth runs on-model) → double count. Delta now joins after the scaling, same exemption as the on-model wealth tax | 10e4d09d2 |
| 3 | revenue.R `calc_receipts` | baseline `totals/estate.csv` covering only a subset of scenario years silently zeroed the estate delta for uncovered years via `coalesce(..., 0)`. Now falls back to the detail rebuild, warns loudly if that fails too | 10e4d09d2 |
| 4 | distribution.R `process_for_distribution` | reform-leg `left_join` unguarded: a baseline id missing from scenario detail NA-poisons every group aggregate (no na.rm downstream); reform-only ids silently dropped. Now a hard stop with counts (trigger: incompatible vintages, e.g. pre/post sample-universe change) | 10e4d09d2 |
| 5 | summary_stats.R | `n_adults = filer * (1 * (filing_status == 2))` counted 0 adults on non-joint returns (cf. correct `1 + ...` form in rebate.R). Latent — not in `demographic_vars` — but wrong on any future addition | 10e4d09d2 |
| 6 | config_parser.R + slurm/common.R + bastian.R | RNG: CLAUDE.md documents `set.seed(globals$random_seed)` but the field never existed; SLURM workers never seeded at all (fresh processes → bastian.R's bare `sample()` non-reproducible and main≠SLURM); bastian.R also order-dependent in main.R (draws depended on stream position). Added `globals$random_seed`, seeded `reconstitute_environment()`, seeded bastian.R per the convention | 7f8063590 |
| 7 | slurm_run.sh | Phase 4 (which owns `purge_detail()`) was gated on `STACKED == 1` → `delete_detail=1, stacked=0` silently left all detail files on disk (main.R purges unconditionally). Phase 4 now submits when stacked OR delete_detail; falls back to the 3a dependency in baseline-only runs | 7f8063590 |
| 8 | slurm/aggregate.R Phase 3a | scenario list derived from probing `baseline/config.rds` on disk; staging dirs persist across runs of a vintage, so a stale baseline config shifted the array indexing and the LAST counterfactual silently never aggregated. Now derived from this run's manifest | 7f8063590 |
| 9 | slurm/worker.R | 2C/2N tasks silently fell back to `static_mtrs = NULL` when Phase 2A's per-year .rds was missing (partial staging dir) — behavior modules would mis-adjust quietly. Now a hard stop | 7f8063590 |
| 10 | slurm/setup.R | `____`-separated multi-runscript args (supported by main.R) crashed obscurely in Phase 0. Now an explicit early stop with message | 7f8063590 |
| 11 | slurm_run.sh Phase 3a | counterfactual receipts jobs raced the baseline's `totals/estate.csv` write in the parallel aggregation array and took the detail-rebuild fallback → last-bit float drift vs main.R (detail CSVs carry 15 sig digits; caught empirically: `revenues_estate_tax` 35.207 vs 35.206999999999994). Baseline aggregation now runs before the counterfactual aggregations, so the fallback fires only when it is genuinely needed | 7f8063590 |

Verification: fixes are output-neutral on the smoke config (no excess growth,
matched universes), so both entry paths were re-run on `tests/simplify_smoke`
and byte-compared against the verified pre-fix outputs (`simplify_post3`):
main.R path (vintage `bugfix_main`, byte-identical) and the full SLURM
pipeline (vintages `bugfix_slurm` pre-3a-fix, then `bugfix_slurm2` with all
fixes: BYTE-IDENTICAL to main.R, 82 non-xlsx + 10 xlsx — the pre-fix run
drifted on 8 receipts files, which is how fix 11 was found).

## Verified NON-bugs (leads closed)

- **calc_mtrs extensive-margin tips1/ot1 not decrementing the `tips`/`ot`
  aggregates** (asymmetric vs the nextdollar branch): harmless — nothing in
  the calc layer consumes the aggregates; tip/OT deductions (below_ded.R) and
  payroll (pr.R) use only per-earner tips1/tips2/ot1/ot2.
- time_burden.R `other_credits` can't go negative (`number_of_credits` in
  liab.R includes matching CTC/EITC terms).
- historical/projections year overlap: files are disjoint (≤2025 / ≥2026).
- `calc_receipts` `lag()` row order: both entry paths build totals in
  `scenario_info$years` order.
- estate heir-allocator ladder (`approx` knots, aggregate identity) traced clean.
- `get_other_taxes` stacked-baseline asymmetries are in a dead code path (both
  callers pass `baseline_id = 'baseline'`).
- tax_law.R indexation base-year anchoring matches the statutory C(t−1)/C(base)
  convention; schedule integrators traced clean (incl. negative income and
  defunct-bracket removal).

## Known-open (documented, not fixed — all PLAUSIBLE/conditional)

1. **tax_law.R `replace_defaults` pairs `i_*` fields with `indexation_defaults`
   by POSITION (map2), not name.** A reform YAML listing the four indexation
   keys in a non-canonical order silently crosses defaults (e.g. base-year
   default lands in `i_measure` → parameter silently unindexed). No current
   file violates the order. Right fix: name-keyed pairing or an order
   assertion in the parser.
2. **Macro-horizon snap-back:** sim years past the Macro-Projections horizon
   make the index chain NA and `apply_indexation` reverts the parameter to its
   raw base value (a discontinuity, not a freeze), silently.
3. **Pre-2014 `value` year keys are silently dropped** (series built on
   `2014:max(years)`, downward fill only) → all-NA series for a
   historically-keyed reform.
4. **`i_increment: NA` with non-NA direction** → value NA → row silently
   dropped by `filter(!is.na(value))`.
5. **Stacked reports mis-stack on year-window mismatch:** `value - lag(value)`
   keys on implicit row order; a scenario missing a year that others have gets
   differenced against the wrong predecessor (revenue.R stacked; 1040.R
   stacked). Guard (assert equal year sets) would be cheap.
6. **`calc_rev_est` drops scenario years absent from baseline receipts**
   (left_join keeps baseline years only) — mismatched `baseline_vintage`
   windows silently truncate revenue tables.
7. **`recode_1040_vars` labels MTR rows from runscript row 1's
   `mtr_vars`/`mtr_types`** — wrong Marginal/Average labels (or silent row
   drop) for scenarios with different MTR specs in one runscript.
8. **horizontal.R `cut()` crashes on tied weighted-percentile breaks** (≥1% of
   weighted mass at one income value) — loud, not silent.
9. **Baseline law gap (needs domain confirmation):** `char.above_limit` is 0
   from 2022 on in `baseline/char.yaml`. OBBBA (July 2025) created a permanent
   above-the-line charitable deduction ($1,000 single / $2,000 joint) for
   non-itemizers starting TY 2026 — the baseline appears to lack it while
   carrying the other OBBBA below-the-line provisions (tips/OT/senior/auto).
   If confirmed, this is a current-law baseline error, not a code bug.
