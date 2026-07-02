# Findings & recommendations — 2026-07-02 sweep

Four targeted correctness sweeps (post-processing, sim orchestration,
data/parsing, SLURM↔main.R drift) plus direct reading done while scoping the
next simplification batch, all on branch `wealth` @ `14b9baf9c`. The
data/parsing and SLURM sweeps ran to completion; the **post-processing and
sim-orchestration sweeps were stopped early** (see "Coverage" at the bottom),
so their areas are only partially cleared.

---

## 1. First priority: verify and commit the fixes already sitting in the working tree

The shared checkout contains **uncommitted fixes for six confirmed bugs** —
apparently from an earlier session, currently unverified as a set and at risk
of being lost or half-shipped. They line up exactly with what the SLURM drift
sweep re-confirmed against HEAD:

| Uncommitted change | Bug it fixes | Notes |
|---|---|---|
| `config_parser.R` (adds `random_seed` to globals) + `src/slurm/common.R` (`set.seed` in `reconstitute_environment()`) + `behavior/employment/bastian.R` (`set.seed` before `sample()`) | **SLURM workers run with an unseeded RNG.** main.R seeds in-process (`set.seed(76)` in `parse_globals`); a SLURM worker is a fresh R process that never runs it. Only live trigger: `bastian.R`'s bare `sample()` — any `employment/bastian` scenario is non-reproducible on SLURM and diverges from main.R. Silent. | Behavior-changing for bastian runs on SLURM (that's the point) |
| `slurm_run.sh` Phase-4 restructure (`P4_DEP`, `DELETE_DETAIL` gate) | **`purge_detail()` unreachable on SLURM** when `stacked=0` or baseline-only: the only Phase-4 submission was nested inside `STACKED==1`, so `delete_detail=1` silently left all per-year detail CSVs on disk. | |
| `src/slurm/setup.R` `____` guard | **Multi-runscript invocations crash the SLURM path** (main.R splits on `____`; setup.R read it verbatim). Now fails loudly with a clear message instead of a confusing read_csv error. | Capability drift, loud either way |
| `src/slurm/aggregate.R` manifest-based scenario list | **Stale staging dir on vintage reuse**: Phase 3a probed `baseline/config.rds` on disk; a leftover baseline config from an earlier run of the same vintage shifted array indexing so the **last counterfactual silently got no totals/receipts**. | |
| `src/slurm/worker.R` hard-stop on missing `year_*_static.rds` | **Silent `static_mtrs = NULL` fallback** in 2C/2N on a broken/partial staging dir — behavior modules would mis-adjust rather than fail at the obvious point. | Turns a silent wrong-answer into a loud error |
| `distribution.R` record-universe guard | Baseline/reform detail id mismatch flowed NA through `liab_delta` and poisoned every group aggregate (no `na.rm` downstream) — or silently dropped reform-only records. | Loud-error guard |
| `revenue.R` baseline-estate year-coverage fallback + **estate excess-growth reorder** | (a) A baseline `totals/estate.csv` covering only a subset of scenario years silently zeroed the estate delta for uncovered years via `coalesce()`. (b) The on-model estate **delta** was taking the `excess_growth_all_rev` factor even though it is computed on micro data that already embed `do_excess_growth` — double count. Now only the CBO level is scaled. | **(b) changes numbers** for excess-growth scenarios with estate reforms |

**Recommendation:** treat these as one "pipeline hardening" branch. Run the
cheap smoke (`tests/simplify_smoke`) via main.R **and** one small
`slurm_run.sh` run (they touch the SLURM path specifically; include one
bastian scenario to see the reseed), then commit. Note which changes are
intentionally behavior-changing (bastian-on-SLURM, revenue.R estate
excess-growth) so the commit message flags any vintage comparability caveats.
Also in the tree, unreviewed here: 1-line `summary_stats.R` change, doc edits
in `other/corporate_incidence/`.

---

## 2. Open bugs — not yet fixed anywhere

### 2a. `calc_mtrs` extensive-margin tips/OT asymmetry (NEEDS VERIFICATION — highest stakes)

`src/calc/do_taxes.R`: the **nextdollar** branch for `tips1` increments
`c('tips1','tips','wages1','wages')` (do_taxes.R:577-588), but the
**extensive** branch for `tips1`/`tips2`/`ot1`/`ot2` decrements only
`c('wages1','wages')` / `c('wages2','wages')` (do_taxes.R:655-660) — the
`tips`/`ot` **aggregate columns are left at their original values** while the
per-earner component is zeroed.

If any calculator reads the aggregate (`tips` or `ot`) rather than
`tips1+tips2`/`ot1+ot2` — the below-the-line tip/OT deductions are the obvious
candidates — then **extensive-margin MTRs for tips and OT are wrong** (the
deduction doesn't shrink when the tips disappear, understating the tax delta).
The verification agent was stopped mid-check; its last confirmed step:
`derive_vars` does *not* re-derive `tips`/`ot`, so nothing downstream repairs
the inconsistency.

**Why it matters now:** the OT-deduction scoring work uses `ot1`/`ot2` MTRs.
If any of those runscripts used `mtr_types = extensive`, the behavioral inputs
were biased. Check `config/runscripts/tests/ot_*.csv` and
`src/calc/functions/deductions/below_ded.R` (whether `tip_ded`/`ot_ded` read
the aggregate). Fix if confirmed: the extensive branch for `tips*`/`ot*`
should also decrement the `tips`/`ot` aggregate by `original_value`.

### 2b. Tax-law parser latent traps (no current config triggers any; all silent except the last)

From the completed data/parsing sweep — all verified against real YAMLs,
none currently firing (~1,000 config files scanned):

1. **Positional `map2` pairing of indexation defaults** (`tax_law.R:319-320`):
   `replace_defaults` pairs a subparam's indexation keys with
   `indexation_defaults` **by position, not name**. A file listing keys in a
   different order (or count) silently crosses defaults — e.g. `'default'`
   for `i_measure` resolving to the base-year default → silently unindexed
   parameter. *Rec: pair by name, or assert name equality.*
2. **No NA guard on the index chain** (`tax_law.R:343-346`): if sim years
   exceed the Macro-Projections horizon, `cumprod(1 + growth)` goes NA and an
   indexed parameter **snaps back to its raw nominal base value** (std
   deduction reverting toward $12,000) instead of freezing — silently.
   *Rec: `stop()` on NA index within sim years.*
3. **Pre-2014 value keys silently discarded** (`tax_law.R:210-214`): the
   year grid starts at 2014 with forward-fill only; an earlier key produces an
   all-NA series that gets dropped entirely. *Rec: assert min key ≥ 2014.*
4. **NA `i_increment` with live direction** (`tax_law.R:399-404`) yields NA
   and the row is silently dropped; vector entries like `[default, NA]` are
   never resolved by the scalar-only `replace_defaults`. *Rec: extend the
   case_when guard + vector-aware default resolution.*
5. Scalar (non-year-keyed) `i_measure` crashes loudly at `tax_law.R:334` —
   known gotcha, low priority.

### 2c. Baseline gap candidate: OBBBA above-the-line charitable deduction

`config/scenarios/tax_law/baseline/char.yaml` has `above_limit_single = 0`
from 2022 onward. OBBBA (enacted July 2025) created a **permanent
above-the-line charitable deduction for non-itemizers — $1,000 single /
$2,000 joint — effective tax year 2026**. The baseline *did* pick up the
companion OBBBA charity provision (the 0.5%-of-AGI itemized floor,
`item_floor_agi: 0.005` from 2026), which makes the missing above-the-line
piece look like an omission rather than a decision. No other parameter models
it (no non-itemizer charity param anywhere in `config/`).

**Rec:** confirm intent; if it's a gap, add the 2026+ value to
`above_limit_single` (the existing `above_limit` filing-status mapper already
gives married ×2) — it affects baseline revenue, charity-reform scores, and
whether the two-pass charitable optimization branch in `do_taxes.R` ever
fires under current law.

### 2d. Fixed and committed during the sweeps (context, no action)

- **1970 CPI splice hole** — `generate_indexes()` dropped 1970's ~5.7%
  inflation (splice-year growth NA → 0), understating `kg_lt_cpi_ratio` for
  all pre-1970 basis. Fixed in `087a51623`; verified only (cpi, 1970) changed.

---

## 3. Simplification: next batch is scoped and ready (on hold)

Yesterday's sweep doc (`other/simplify_review_codebase_2026-07-01.md`) still
has most of its menu open. The next batch is scoped and the verification
setup is built; it's paused pending go-ahead:

**Batch contents (all behavior-preserving, byte-diff verified):**
- `add_rank_groups()` — the percentile/quintile/top-share block copy-pasted
  4× (distribution.R income/AGI/net-worth + time_burden.R)
- Spec-driven `build_distribution_tables` (18 near-identical stanzas → spec
  list + map)
- Shared readers: static-detail fread idiom (6 sites),
  historical+projections splice (4 sites), VAT-offset read (3 sites),
  baseline-interface-root lookup (4 sites)
- revenue.R: single-source the 8-term receipts total (3×), receipts-long
  reader, xlsx styling helper, hoist loop-invariant reads
- 1040.R: `pivot_1040_long()` (3×), header-write loop, drop unused
  `recode_1040_vars(scenario_id)` param
- tax_law.R: single-loop subparam override; `parse_inf`/`parse_na` →
  one `walk_atomic()`
- economy.R: excess-inflation helper (2×), shared OASDI/pension wedge factor

**Two review-menu items rejected as unsafe** (worth annotating in the review
doc):
- Item 23's suggested `modifyList` for reform overrides — `modifyList` merges
  nested lists **recursively**; reform subparameters must replace **wholesale**
  (omitted field = NULL). Using it would silently inherit baseline indexation
  fields. The safe form is `tax_law[[param]][names(changes)] = changes`.
- Item 29 (calc_mtrs composite-map dedup) — the two branches genuinely
  differ, and the difference is possibly the live bug in §2a. Don't dedup
  until 2a is resolved.

**Verification setup (already in place):**
- Fresh detached worktree at HEAD: `/nfs/roberts/scratch/pi_nrs36/jar335/simplify_pre_wt`
- Pre-edit reference smoke: SLURM job **17000050**, vintage `simplify_pre3`
  (5% sample, 2025:2027, baseline + sd_bump_10k, stacked, dist year 2026).
  Gotcha discovered on the way: `tests/simplify_smoke.csv` is not tracked by
  git, so fresh worktrees need it copied in.
- Post-edit run + byte-diff scripts: `other/simplify_cleanup/`

**Caveats for whoever runs the batch:**
- Rebase on top of the §1 uncommitted fixes first — they touch
  distribution.R and revenue.R, same files as the batch.
- do_taxes.R items 27/28 (charitable two-pass dedup) have **zero smoke
  coverage**: `char.above_limit` is 0 in 2025–2027 baseline law, so the
  branch never fires. Either add a smoke scenario with an above-the-line
  charity limit (or years 2020–21), or skip those two items. (If §2c is
  fixed, 2026 baseline law exercises the branch for free.)

---

## 4. Coverage — what was and wasn't cleared

**Cleared (completed sweeps; negative results are load-bearing):**
- SLURM↔main.R sync: globals serialization/reconstitution, Phase 3a vs
  `write_pass_outputs`, Phase 3b vs `do_scenario`, worker arg passing and
  2A/2B/2N/2W/2C dispatch + DAG ordering, setup.R pre-loop replication — all
  in sync at HEAD apart from the items in §1. One stale docstring
  (`common.R:69` lists phases 1/2A/2B/2C only).
- config_parser.R, misc/utils.R, calc/utils.R (both schedule integrators:
  bracket edges, defunct brackets, inclusive bands) — no correctness bugs.
- tax_law.R indexation core (direction codes, base-year lag convention,
  defunct-bracket removal) verified against real ord/std configs.

**NOT cleared (sweeps stopped early on request):**
- Post-processing correctness (distribution.R, revenue.R, 1040.R,
  time_burden.R, horizontal.R, summary_stats.R, estate_allocator.R) — the
  hunter was killed before reporting. The §1 uncommitted guards cover two
  issues in this area, but no systematic pass finished.
- Sim orchestration (run.R pass sequencing, behavior.R elasticity math,
  estate.R, wealth.R) — killed mid-verification of §2a.

Re-running those two sweeps is cheap if wanted.
