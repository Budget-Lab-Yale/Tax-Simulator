# Simplification review — `src/` changes on `wealth`

_Generated 2026-06-22. Branch `wealth` vs `main` (merge-base `9f76878712`)._

**Scope:** 20 files, ~4.3K insertions under `src/`. Method: 24 angle reviews
(reuse / simplification / efficiency / altitude, across 6 file-group units) →
per-finding verification against the actual code → synthesis.
**34 findings survived verification** (13 dropped as false-positive or
behavior-risk), merged to **22 distinct items**.

> These are **not** correctness bugs — that's `/code-review`'s job. Every
> suggestion below is intended to be behavior-preserving. Nothing has been
> edited; this is a menu.

The three Tier-1 claims were ground-checked directly against the code:
- `src/calc/do_taxes.R:127-129` — `calc_estate()` runs **unconditionally** in
  the tax chain; the code comment itself notes "the MTR loop re-runs do_taxes on
  frames that already carry them."
- `src/main.R:23` and `src/slurm/setup.R:32` both carry
  `!startsWith(.x, 'tests/')`; `src/slurm/common.R:35` does **not** — confirmed
  divergence.

---

## The shape of it

The estate + kg_dynamics work was bolted onto `run_one_year` and `do_taxes` by
**copy-pasting whole blocks between the static and conventional passes** and
**re-declaring shared constants/definitions in each module**. The single biggest
theme: the static-vs-conventional split is handled by duplication rather than a
parameterized helper. Fixing that *reduces* the SLURM sync burden CLAUDE.md
warns about, rather than adding to it.

---

## Tier 1 — real payoff, do these first

### 1. Gate the in-chain estate calc · `src/calc/do_taxes.R:127-129` (efficiency)
`calc_estate()` — four graduated-schedule integrations + rowSums + findInterval
over the full frame — runs on *every* `do_taxes` call, including the MTR loop
(one pass per `mtr_var`, ~4–18/scenario-year) and the kg dead-leg recomputes
(`run.R:493/558/633`). All four of those callers **discard** the estate columns;
only the two real passes (`run.R:470/621`) read them. Add a defaulted
`calc_estate_flag = TRUE`, pass `FALSE` from the discard-only callers.
Byte-identical output, dozens of full-frame estate passes saved per multi-year
scenario. Contained to `do_taxes.R` + ~5 call sites; no SLURM change.

### 2. Extract the deemed-death dead-leg recompute · `src/sim/run.R:488-512` & `625-645` (simplification, high)
The kg deemed-death-gain two-leg split + dead-leg `do_taxes()` recompute + §2053
estate-deduction reprice is ~20 lines of per-record tax physics copy-pasted
near-verbatim between the static and conventional passes — the conv copy
literally comments "same logic as the static pass." Lift into one pure helper in
`kg_dynamics.R`; let each caller do the fold (which must stay after the MTR
block). Preserves the existing static/conv asymmetry (static seeds
`liab_deemed=0` and folds unconditionally under `uses_kg_mech`; conv folds only
when the deemed leg fired). Any future fix to deemed-tax mechanics then touches
one place instead of two.

### 3. The `tests/` exclusion is missing from the SLURM path · `src/slurm/common.R:35` (altitude — latent contract drift)
The "skip `src/tests/`" clause was added to `main.R:23` and `setup.R:32` but
**not** to `reconstitute_environment` in `common.R:35`, so every SLURM
worker/aggregator sources `src/tests/` files that `main.R` deliberately excludes
— breaking the "operate identically to main.R" contract and the CLAUDE.md sync
table. Harmless today (those files are function-definition-only) but latent.
Minimum fix: add `&& !startsWith(.x, 'tests/')`. Better: hoist the predicate into
one shared `source_src_files()` / `SRC_EXCLUDE_PREFIXES` used by all three sites.

---

## Tier 2 — worth doing

### 4. Extract `write_pass_outputs()` · `src/sim/run.R:224-308` + `src/slurm/aggregate.R:102-160` (simplification)
Static- and conventional-output writers are structurally identical (same
supplemental + pr/1040/1040_by_agi/estate writes + `calc_receipts`), differing
only by root path and `$static_totals` vs `$conventional_totals`. The block is
**triplicated** (third copy in SLURM Phase 3a). Extract
`write_pass_outputs(output, root, totals_accessor)`; share it with `aggregate.R`
to *cut* the documented sync burden rather than add to it.

### 5. Gate baseline/static MTR I/O on `has_behavior` · `src/slurm/worker.R:53-81` (efficiency)
Phase 2C reads the full baseline MTR set (often *all* baseline year RDS files) +
re-reads `year_{y}_static.rds$mtrs` for **every** counterfactual year — including
no-behavior CFs that take the `run.R:700-723` branch and never touch the MTR
frames. Gate both reads on
`length(config$scenario_info$behavior_modules) > 0` (config already loaded at
line 50), leaving NULL (which `run_one_year` tolerates). In the non-prebuilt
branch read only the single matching year file:
`readRDS(file.path(staging_dir,'baseline',paste0('year_',task$year,'.rds')))$mtrs`.

### 6. Merge `bathtub.R` into `frozen.R` · `src/slurm/bathtub.R` (reuse)
`bathtub.R` is a near-verbatim copy of `frozen.R`; the only real differences are
the phase string (`2B` vs `1B`) and the pre-pass fn called (`run_bathtub_pass`
vs `run_frozen_pass`, identical signatures). Collapse to one
`src/slurm/prepass.R` dispatching on a CLI phase arg —
`switch(phase, '1B'=run_frozen_pass, '2B'=run_bathtub_pass)` — mirroring how
`worker.R` already does `switch(phase, ...)`. Update `slurm_run.sh:109,161` to
pass the phase arg.

### 7. Extract `calc_scenario_mtrs()` closure · `src/sim/run.R:514-534` & `647-672` (simplification)
The `map2()` per-year MTR-tibble builder is copy-pasted verbatim between the two
passes (only `tax_units_static` vs `tax_units_conv` differ), including the
`select(-all_of(...))` projection and the `bind_cols`/`mutate(id,year)`/`relocate`
sequence. A local closure `calc_scenario_mtrs(taxed_frame, year)` over
`mtr_vars`/`mtr_types`/`return_vars`, with each pass doing its own
`left_join(by='id')`, removes ~18 duplicated lines. (The law-only site at
562-572 can share only the projection, not the full loop.)

### 8. One `parse_year_range()` helper · `src/misc/config_parser.R:239-244` vs `368-379` (reuse)
The `{start}:{end}` parse exists in two (arguably three) distinct idioms; the new
`sim_years` loop reimplements it in a more convoluted `map/map/unlist` form that
handles malformed input differently than the `get_scenario_info` copy with its
explicit `stop()`. Share one `parse_year_range(s)` helper. **Don't** fold in
`dist_years` (382-392) — it also accepts a space-delimited list, so it is not a
drop-in there.

### 9. Alias the estate asset-cols vector · `src/sim/kg_dynamics.R:158-163` (reuse)
`KG_DYN_ESTATE_ASSET_VALUE_COLS` is a byte-identical copy of `ESTATE_ASSET_COLS`
(`estate.R:23`, 14 elements, same order). Replace the literal block with the
one-line alias `KG_DYN_ESTATE_ASSET_VALUE_COLS = ESTATE_ASSET_COLS`. (Verified:
single definition, in scope under both the `main.R` and SLURM reconstitute
source orders; use sites 333/350/1621 are order-insensitive.) Stops the wealth
definition from silently drifting between liability, mortality, and kg
aggregation.

### 10. Memoize `get_estate_params()` · `src/sim/run.R:423-425` (efficiency)
Re-reads / re-parses / re-validates the same YAML and re-emits the
vintage-mismatch warning on every `run_one_year` (~22 parses for an 11-year kg
run). Memoize **keyed on the Tax-Data path** — not a bare `is.null` guard, since
`main.R:113` runs multiple scenarios with possibly different vintages in one
process:
`if (!identical(globals$estate_params_path, td_path)) { globals$estate_params <<- get_estate_params(td_path); globals$estate_params_path <<- td_path }`.

### 11. Collapse `diag_or`/`death_or` · `src/sim/kg_dynamics.R:1788-1805` (simplification)
Byte-for-byte identical default-broadcast closures differing only in which
captured list they read (`planned_diag` vs `death_diag`). One
`or_default(src, name, default)` closing over `ages_chr`.

---

## Tier 3 — low-value cleanups (a single tidy-up pass)

Real but small; batch or skip.

| # | Location | Issue → fix |
|---|----------|-------------|
| 12 | `distribution.R:132-177` | leg-detail read + `liab_deemed` default copy-pasted for both legs → local `read_leg_detail(root)` |
| 13 | `estate_allocator.R:129-141` | degenerate-case early return re-hand-writes the full diag schema → `make_diag(...)` template (silent NA-fill hazard in downstream `bind_rows`) |
| 14 | `kg_dynamics.R:621-687` | macro-projection read boilerplate duplicated across beta/cpiu loaders → `read_macro(root, cols)` |
| 15 | `sim/estate.R:106-107` vs `estate.R:140` | identical `economic_gross` rowSums in two modules → `estate_economic_gross(df)` helper next to `ESTATE_ASSET_COLS` |
| 16 | `kg_dynamics.R:712-724` | `inner$<col>[inner$age == AGE_MAX]` repeated 5× → compute `top` row once |
| 17 | `config_parser.R:157-161, 413-416` | `corp_incidence_phasein` default `10` hardcoded in two layers → coalesce once in `parse_globals` (don't just delete the guard — it drops blank-cell defaulting) |
| 18 | `kg_dynamics.R:1810-1840` | `as.character(age)` / `match()` recomputed in one mutate → hoist `age_chr` / `mix_idx` |
| 19 | `distribution.R:311-320` | inheritance-share factor recomputed twice → factor `deemed_share` once (inside `group_by`) |
| 20 | `estate_allocator.R:195-202` | `heirs_per_estate` seeded `NA` then mutate-filled → compute operands as locals, assign inline |
| 21 | `kg_dynamics.R:1767` | dead `year_idx` param (never referenced; one call site 2061) → drop from signature + call |
| 22 | `kg_dynamics.R:2411-2412` | `deemed_realized` & `taxable_deemed_stock` are the identical reduction → reference the first |

---

## Cross-cutting themes

1. **Triplicated "which `src/` files to source" predicate** (`main.R`,
   `setup.R`, `common.R`) — the `tests/` exclusion landed on only 2 of 3. →
   one shared helper. (Items 3.)
2. **Static-vs-conventional duplication in `run_one_year`** — deemed-leg
   recompute, MTR map2 builder, and pass-output writer all copy-pasted,
   parameterized only by frame/root name; pass-output writer has a third copy in
   `aggregate.R`. → pure helpers, some shared with SLURM. (Items 2, 4, 7.)
3. **Unconditional `calc_estate()` in the hot loop** — recomputed and discarded
   on every MTR pass and dead-leg recompute. → one defaulted flag. (Item 1.)
4. **Estate-asset / gross-estate definition duplicated** across calculator,
   mortality, and kg modules with nothing enforcing lockstep. → estate.R as
   single source of truth. (Items 9, 15.)
5. **Year-range parsing implemented three ways** in `config_parser.R`. → one
   helper (excluding `dist_years`). (Item 8.)
6. **Recurring micro-duplication** — small read/default or default-broadcast
   blocks re-typed once per source rather than parameterized. (Items 11, 12, 14,
   19.)

---

## What was dropped (13 findings)

Mostly suggestions to "collapse" the DSUE/no-DSUE two-branch estate calc
(intentionally nonlinear), to merge static/conv blocks that actually differ in
load-bearing ways, and reuse claims where the named "existing helper" did not
actually exist on inspection. The verifier also rejected several `kg_dynamics`
numeric-shortcut ideas as calibration risks — correctly, given the recalibration
work.

---

## Suggested order of attack

- **Start:** #1, #3, #9 — most contained, lowest-risk, no behavior change.
- **Then (verified batch):** #2, #4, #7 — higher value but touch
  `run_one_year`; run the kg_dynamics regression at `pct_sample=1` and confirm
  byte-identical totals before committing.
- **Opportunistic:** Tier 3 in one tidy-up pass.
