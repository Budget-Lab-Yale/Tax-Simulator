# Simplification review — full codebase sweep

_Generated 2026-07-01 on branch `wealth`. Companion to
`other/simplify_review_src_wealth_2026-06-22.md` (which covered only the
wealth-branch `src/` diff as of that date)._

**Scope:** everything the prior review did NOT cover — the calculator layer
(`src/calc/`), data/post-processing (`src/data/`), orchestration/misc, behavior
modules, and all code written after 2026-06-22 (`wealth_dynamics.R`,
`cohort_bathtub.R`, SLURM 2N/2W plumbing). Method: 4 parallel area sweeps →
per-finding verification against current code (all headline diffs re-run and
confirmed byte-identical where claimed).

**These are not bugs.** Everything below is intended to be behavior-preserving.

**Update (2026-07-01, later same day):** the low-hanging-fruit batch was APPLIED
and verified — items **14–19, 21, 22** (Tier 3), **#13's yaml-read hoist**
(config_parser reads `interface_versions.yaml` once), and the
**`interface_path` → `interface_paths` rename** (3 sites). Item **20**
(salt-workaround → `derive_vars`) was deliberately skipped: calling
`derive_vars()` there would recompute a dozen unrelated columns mid-pipeline,
so it is not zero-risk. Verification: pre/post smoke runs of
`tests/simplify_smoke` (baseline + sd_bump_10k, 2025:2027, 5% sample, dist year
2026, wages MTRs, stacked) via sbatch jobs 16927554/16927555 — all 56 output
files byte-identical, including xlsx sheet XML.

**Update (2026-07-02):** the post-processing batch was APPLIED and verified
byte-identical (commit a661e6c8c) — items **6, 7, 8, 9** (helpers now in
`src/data/helpers.R`), **23** (via wholesale index-assign, NOT modifyList —
modifyList merges nested lists recursively, which would break the
replace-wholesale override contract), **24–28**. Item **29** (composite_deps
lookup) was deliberately skipped: the two maps are NOT duplicates — the
extensive branch intentionally skips the tips/ot aggregates (verified
harmless: no calculator consumes them) — so a shared lookup would paper over
a real asymmetry. `tests/simplify_smoke` gained a `char_above` scenario so the
two-pass charitable branch is exercised. Verification: worktree at 087a51623
(vintage simplify_pre4) vs edited tree (simplify_post3), jobs
16998779/16999449 — 82 non-xlsx byte-identical + 10 xlsx identical
ex-timestamp. Remaining open: Tier 1 items **2** (channel guard rails),
**3** (SLURM pre-pass driver triple), **4** (build_scenario_config bundle),
Tier 2 **10–12**, **13**'s bootstrap triplication, and the 2026-06-22
leftovers (#2, #7, #8, #10, #11).

---

## Status of the 2026-06-22 review

Applied since: **#1** (calc_estate_flag gate), **#3** (tests/ exclusion in
common.R), **#4** (write_pass_outputs extraction — now shared with aggregate.R).

Applied 2026-07-01: **#9** (KG_DYN_ESTATE_ASSET_VALUE_COLS → canonical
`ESTATE_ASSET_COLS`), bundled with Tier-1 item **1** below.

Still open: **#2** (deemed dead-leg recompute extraction), **#6** (bathtub/frozen
merge — see item 3 below, it grew), **#7** (calc_scenario_mtrs closure), **#8**
(parse_year_range), **#10** (memoize get_estate_params), **#11**
(diag_or/death_or), and the Tier 3 batch.

---

## The shape of it

Five recurring patterns:

1. **cohort_bathtub.R's "generic primitives" premise is unrealized.** Every one
   of its 11 functions has exactly one consumer (wealth_dynamics.R, plus
   `read_cohort_state` in run.R). kg_dynamics.R — the intended second consumer —
   uses none of them and still carries private copies of the exact things that
   overlap (aging matrix, state-path/IO).
2. **The kg↔wealth channel pair duplicates its guard rails** (run-compat
   refusals, provenance machinery, macro spliced readers) — the highest-risk
   duplication in the repo because these are the blocks that must stay in
   lockstep by design.
3. **main.R↔slurm/setup.R entry-path duplication persists** beyond the items
   already fixed: the 4-call scenario-config bundle and the baseline-MTR loader
   are verbatim copies, and the bootstrap boilerplate is triplicated.
4. **The calculator layer has no phase-out vocabulary.** The same 3–4 arithmetic
   idioms (phase-out, phase-out share, discrete-step rounding, po_type
   rate-vs-range) are re-typed ~25 times across calculators; utils.R's two
   schedule integrators share ~45 byte-identical lines.
5. **Post-processing scripts grew by copy-paste from each other** — the same
   read-detail / read-hist+proj / percentile-cut / reshape blocks appear 3–4×
   across distribution.R, revenue.R, 1040.R, time_burden.R, horizontal.R.

---

## Tier 1 — highest payoff, do these first

### 1. Merge kg onto the cohort_bathtub primitives it was supposed to share — **APPLIED 2026-07-01 (with #9)**
_Done: (A) deleted `kg_dyn_build_aging_matrix`, both call sites now use the
shared `build_aging_matrix`; (B) the four kg state dir/path helpers are thin
wrappers over `cohort_state_dir`/`cohort_state_path` (inline saveRDS/readRDS and
their bespoke error messages left as-is — routing through the wrappers removes
the path-shape duplication); (#9) deleted the `KG_DYN_ESTATE_ASSET_VALUE_COLS`
literal, 3 sites now use canonical `ESTATE_ASSET_COLS`. No SLURM/CLAUDE.md edits
(no signature/contract/sequencing change; `common.R` sources `cohort_bathtub.R`
identically to main.R). Verified byte-identical DATA on TWO independent
full-sample kg_dynamics/turnover regressions (original-HEAD-worktree vs edited),
5 scenarios incl. baseline/baseline_check/rate_up_5pp/carryover/deemed: V1
2025:2028 = 227/227 files, V2 2026:2029 = 249/249 files (green, +distribution
outputs); 0 rds / 0 csv / 0 genuine-xlsx mismatches (only openxlsx
`docProps/core.xml` creation-timestamps differ). Evidence:
`other/simplify_cleanup/kg_item1/VERIFICATION.md`._
- `kg_dyn_build_aging_matrix` (kg_dynamics.R:560-569) is an exact copy of
  `build_aging_matrix` (cohort_bathtub.R:19-42) — verified identical bodies; the
  generic one only adds a harmless `stopifnot` contiguity guard. Delete the kg
  copy, call the generic at the two use sites (kg_dynamics.R:2021, :2143).
- kg's four state-path helpers (`kg_dyn_state_dir/path`,
  `kg_dyn_mech_state_dir/path`, kg_dynamics.R:1437-1460) plus its inline
  `dir.create`/`saveRDS`/`readRDS` (≈:1914, :2100, :2201, :2233, :2314) are the
  same `file.path(output_path, pass, 'supplemental', subdir)` shape as
  `cohort_state_dir`/`cohort_state_path`/`write_cohort_state`/`read_cohort_state`
  (cohort_bathtub.R:272-309). Point kg at them. (Only the IO plumbing dedups —
  the state payloads differ.)
- Do together with the still-open 2026-06-22 #9 (estate asset-cols alias) as one
  "kg consumes shared definitions" commit.

### 2. Extract the shared channel guard rails (kg ↔ wealth)
- **Run-compat refusals:** `wealth_dyn_check_run_compat`
  (wealth_dynamics.R:473-516) vs `kg_dyn_check_run_compat` (run.R:973-1030) —
  the `vat_active` block is **byte-identical** (verified by diff:
  wealth_dynamics.R:496-498 ≡ run.R:1003-1005), and the excess-growth and
  pct_sample guards match verbatim too. Extract
  `assert_raw_dollar_cell_compat(scenario_info, vat_price_offset, channel_label)`;
  each channel keeps only its extras (kg's mtr_vars check).
- **Provenance machinery:** `wealth_dyn_check_provenance`
  (wealth_dynamics.R:422-469) vs `kg_dyn_check_calibration_provenance`
  (kg_dynamics.R:231-302) — identical `num_mismatch` inner fn, identical
  Macro-vintage `grepl` check, near-identical STRICT-env stop/warn epilogue.
  Share `num_mismatch()` + a banner/epilogue helper; keep field comparisons
  per-channel.
- These blocks exist to guarantee the two channels behave identically under the
  same preconditions — copy-paste is the failure mode here, not just noise.

### 3. Collapse the SLURM pre-pass driver **triple** (supersedes 2026-06-22 #6)
`src/slurm/frozen.R` (66 L), `bathtub.R` (59 L), and the post-review `wealth.R`
(67 L) differ only in phase string, log label, gate fn, and pass fn; the ~40
lines of scaffolding around them are identical. One `prepass.R <staging_dir>
<phase>` with a `phase → (gate_fn, pass_fn, label)` map — the pattern worker.R
already uses — cuts ~190 lines to ~70 and shrinks the CLAUDE.md sync table by
two rows. Update `slurm_run.sh` call sites.

### 4. `build_scenario_config()` — stop duplicating the 4-call bundle main↔SLURM
run.R:37-58 (`do_scenario`) and slurm/setup.R:111-131 compute the identical
`get_vat_price_offset → get_excess_growth_offset → generate_indexes →
build_tax_law` sequence (verified: diff shows only whitespace/comments). Same
lockstep hazard write_pass_outputs already fixed for the totals path. Also fold
in the baseline-MTR loader duplicated at main.R:92-102 ≡ setup.R:154-164
(verified; only the years-vector source differs) → `load_baseline_mtrs(years)`.

### 5. utils.R: one schedule-integration engine instead of two — **APPLIED 2026-07-01**
_Done (safe version): private `integrate_schedule(df, ..., bracket_fn)` holds
the shared boilerplate; both public functions are thin wrappers with unchanged
signatures and their per-bracket bodies moved verbatim into closures. Verified
byte-identical vs HEAD via the smoke harness (job 16929759, vintage
simplify_post2). The full collapse (unconditional = conditional with y=0)
was NOT taken._
`integrate_rates_brackets` (utils.R:105-197) and
`integrate_conditional_rates_brackets` (utils.R:201-323) share ~45
**byte-identical** lines (verified: utils.R:138-173 ≡ 246-281 and 188-197 ≡
314-323, diff exit 0) — defunct-bracket removal, n_brackets logic, (n+1)th
bracket, output naming, rowSums wrapper. Extract one private
`integrate_schedule(df, ..., bracket_fn)`; both become thin wrappers. (The
unconditional version is the conditional one with y=0/x=y/inclusive=F, so full
collapse is also possible.) This is the calculator core used by
tax/pr/amt/estate/wealth/niit/ed_cred — highest-leverage single dedup in the
repo.

### 6. distribution.R: spec-drive the table assembly and share the rank-groups block
- `build_distribution_tables` (distribution.R:34-101) hand-writes 9 structurally
  identical `group_by(taxes_included, group=X) %>% calc_dist_metrics() %>%
  mutate(group_dimension=Y)` stanzas. Drive from a spec list + `map_dfr`;
  ~70 → ~15 lines.
- The `arrange → pctile → quintile case_when → top_10/5/1/01` block appears
  **four times verbatim**: income (:389-417), AGI (:420-438), net_worth
  (:447-467), and a 4th copy in time_burden.R:119-137. One
  `add_rank_groups(df, var, prefix)` puts the 0.2/0.4/0.6/0.8 and
  0.9/0.95/0.99/0.999 thresholds in exactly one place.

---

## Tier 2 — worth doing

### 7. Post-processing shared readers (one small helper file)
- `read_detail(root_or_id, yr)`: the `file.path(..., 'static/detail',
  paste0(yr,'.csv')) %>% fread() %>% tibble()` idiom is re-typed in
  distribution.R:148/200, time_burden.R:29/32, horizontal.R:30/35 (plus the
  wealth-side readers, item 12).
- `read_hist_proj(root)`: the `c('historical.csv','projections.csv') %>% map(read_csv)
  %>% bind_rows()` splice appears at economy.R:37, revenue.R:49, :294, :490 —
  and in channel-flavored form at wealth_dynamics.R:708-712 and
  kg_dynamics.R:632-638/:662-687 (2026-06-22 #14). One
  `read_macro_spliced(macro_root, cols)` covers all seven.
- `read_vat_offset(id)`: distribution.R:564, revenue.R:265, revenue.R:456.

### 8. revenue.R internal dedup
- The 8-term receipts `total` sum is spelled out at :249-256, :278-285,
  :474-481 → `total_receipts()` helper (or a named component vector + rowSums).
- The baseline/scenario legs of `calc_rev_est` share the whole
  "path → read receipts_full.csv → mutate(total) → pivot_longer" block →
  `read_receipts_long(path)`.
- openxlsx styling sequence duplicated between `calc_rev_est` (:378-417) and
  `calc_stacked_rev_est` (:563-606) → `style_rev_sheet(wb, sheet, ncol, rows)`.
- Measure-split list construction duplicated at :341-345 vs :524-535.

### 9. 1040.R reshape + header boilerplate
The `rename n_→count. / prefix amount. / pivot_longer(sep='.')` reshape is
copy-pasted 3× (:33-42, :57-67, :319-328) → `pivot_1040_long(df)`. The 12
`writeData(xy=)` header calls (:128-179) → loop over a `tribble(text, x, y)`.
Also `recode_1040_vars(df, scenario_id)` (:424) never uses `scenario_id` — drop
the param.

### 10. Calculator phase-out vocabulary (4 tiny helpers, ~25 call sites)
- `apply_phaseout(value, income, thresh, rate)` = `pmax(0, value - pmax(0,
  income - thresh) * rate)` — 9 sites (wage_subsidy.R:49/53,
  below_ded.R:84/104/119, eitc.R:127/133, ctc.R:226, agi.R:132).
- `phaseout_share(income, thresh, range)` = `pmin(1, pmax(0, income - thresh) /
  range)` — 5 sites (pe_ded.R:54, qbi_ded.R:110, rebate.R:79, ed_cred.R:79/87).
- `round_up_step(x, step)` — 8 sites (pe_ded.R:59, ctc.R:145,
  cdctc.R:108-110/137-139).
- `resolve_po_rate(po_type, rate, value, range)` — below_ded.R:83/103,
  ctc.R:149-153, rebate.R:74-78.
Then cdctc.R's young (:93-119) / old (:122-148) blocks collapse to a loop over
`c('young','old')` (verified structurally identical; old's expense base nets out
young's).

### 11. Behavior-module families are per-constant file copies
- `kg/{22,50,62,70,72,50_w_transitory_2025}.R`: identical `do_kg` bodies; only
  `e_permanent = -X/0.238` and `max_adj` (1 vs 3) differ.
- `charity/50.R` vs `100.R`: identical except `e = -0.5` vs `-1.0`.
- `ot/{france,france_1yr,france_full}.R`: diff-confirmed single differing line
  each (`phase_in_years`, `phase_in_factor`).
One parameterized module per family driven by a small params table (or parse the
constant from the basename). Judgment call — the one-file-per-assumption
convention has documentation value; if kept, at least generate them.

### 12. wealth_dynamics.R internal boilerplate + lockstep constants
- `read_detail_checked(path, need, context)`: the exists-check / fread /
  missing-columns-stop / `liab_deemed=0` default block is repeated in
  `wealth_dyn_read_convnw_detail` (:943-965) and
  `wealth_dyn_read_baseline_detail` (:969-996).
- `wealth_cap_bundle_cols(df)`: the `pure_cols`/`pt_cols` intersect pair is
  duplicated between the MTR bump (:635-638) and the applier (:1079-1082) —
  and the file's own comment (:70-71) says these two sites MUST scale exactly
  the same columns. Single-source it.
- `liab_iit_pr = liab_iit_net + liab_pr - liab_deemed` (the forcing-leg
  identity) is spelled inline 4×: wealth_dynamics.R:831, :994,
  distribution.R:185, :213 → one `during_life_iit(df)` helper.
- `economic_gross` computed twice on the same conv-no-wealth frame
  (wealth_dynamics.R:633 then run.R:794) — return it from
  `calc_cap_bundle_mtr` and reuse.

### 13. Orchestration odds and ends
- `parse_globals()` reads `interface_versions.yaml` from disk **4×**
  (config_parser.R:60, 74, 78, 209) — read once, reuse.
- Bootstrap boilerplate (package loader + source-walk predicate) triplicated
  across main.R:14-23, setup.R:22-35, common.R:24-42 → one `src/bootstrap.R`.
  common.R's own comment names the lockstep hazard.
- `run_one_year()`: the 5-slot totals list is built twice (run.R:702-706 static,
  :874-878 conv) → `get_all_totals(frame, year)`; the deemed-tax fold 3-liner is
  duplicated (:686-689 vs :852-855) → `fold_deemed_tax(df)`; `return_vars %>%
  unlist() %>% set_names(NULL)` is recomputed 4× (:622, :668, :805, :824) —
  hoist once.

---

## Tier 3 — dead code & trivia (one tidy-up pass)

| # | Location | Issue → fix |
|---|----------|-------------|
| 14 | revenue.R:323, :332 | `revenues_corp_rate` series never produced by receipts writer (:197-199) — dead match entry + label → delete |
| 15 | time_burden.R:144-160 | `calc_fixed_cost(id)` computes `first_year` then returns the constant 497.058 → named constant |
| 16 | time_burden.R:328-335 | `output$scenario = 'reform'` re-sets what the mutate on the prior line already set; whole if/else collapses to one mutate |
| 17 | cdctc.R:142-144 | `pmax(0, pmax(0, ...))` double-clamp (young block at :113-115 has the single) → drop one |
| 18 | caregiver_cred.R:69 | single-argument `pmin(x)` is a no-op → remove wrapper |
| 19 | tax.R:684-687 | invalid-`type` else branch in `calc_mtrs` references `vars`, defined only in the nextdollar branch — can't execute → replace with `stop()` |
| 20 | do_taxes.R:751-754 | `do_salt_workaround_baseline` re-derives `part`/`scorp` byte-identically to `derive_vars` (utils.R:22-25) → call the helper |
| 21 | ss.R:40 | reimplements `get_n_cols` inline → call it |
| 22 | wealth_dynamics.R:124, :300 | vestigial `transition = 'identity'` provenance field (never read) + comment referencing deleted `build_within_age_transition` → drop both |
| 23 | tax_law.R:37-41 | hand-rolled nested two-level merge → `modifyList` per param |
| 24 | tax_law.R:595-671 | `parse_inf`/`parse_na` are the same nested-walk shape with different leaf coercions → one `walk_atomic(value, fn)` |
| 25 | economy.R:167, :229 | `excess_inflation = cpi_factor/lag(cpi_factor)-1` derived twice → tiny helper |
| 26 | economy.R:443-452 | oasdi/pension wedge case_whens share 2 of 3 arms → compute shared factor once |
| 27 | do_taxes.R:97-111 | charitable above/item `do_1040` pair differs only by flag+label → map over `c(above=T, item=F)` |
| 28 | do_taxes.R:321-340 | char zero-then-restore pattern written twice with flipped condition → local closure |
| 29 | tax.R:568-591 vs :649-663 | composite-var dependency map written once per MTR type branch → one `composite_deps(var)` lookup |

---

## Bug-adjacent (not simplification — flagged for a look)

**`globals$interface_path` (singular) at revenue.R:292, :488 and
distribution.R:580.** `parse_globals` only ever creates `interface_paths`
(plural). These three sites work **only because R's `$` on lists does partial
name matching** — verified. Not a live bug, but any future `globals` key
starting with `interface_path…` would silently turn these into
`filter(ID == NULL)`. Rename to the plural (3 characters × 3 sites).

---

## Verification notes

Ground-checked directly (not just agent-reported): utils.R block identity
(diff exit 0 both blocks), aging-matrix identity, VAT-guard byte-identity
(diff exit 0), run.R↔setup.R config-bundle and MTR-loader diffs
(whitespace/comments only), `interface_path` partial-match, dead
`revenues_corp_rate`, cdctc double-pmax, caregiver no-op pmin,
time_burden redundant reassignment, interface_versions.yaml 4× read.

Explicitly NOT flagged (verified non-duplication): estate DSUE/no-DSUE two-branch
calc (intentionally nonlinear); `cohort_recurrence_step` vs
`kg_dyn_step_recurrence` (genuinely different flows — do not merge);
percentile-ranking primitives (wealth-only, no kg counterpart);
`do_behavioral_feedback`'s positional args (required by turnover.R/avoidance.R
signatures); conv-no-wealth pass in run.R (already unified under one
`is_convnw` flag — good); `scenario_uses_wealth_dynamics` (already memoized).

## Suggested order of attack

- **Start (contained, zero behavior risk):** items 14-22 trivia batch + #13
  yaml-read hoist + the `interface_path` rename.
- **Core batch (run kg regression at pct_sample=1 after):** items 1, 2, 5 —
  then 3 and 4, which also shrink the CLAUDE.md SLURM sync table.
- **Post-processing batch (compare output CSVs before/after):** items 6-9.
- **Opportunistic:** 10-12, and decide the behavior-module question (11) as a
  team convention rather than a refactor.
