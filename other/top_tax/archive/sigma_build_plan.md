# σ Build: Items i–iii of the Top-Tax Interaction Prospectus

> **BUILD COMPLETE 2026-07-08.** All steps (0–6) executed and verified; see
> `DESIGN_LOCK.md` amendment A1 for the one post-validation design change
> (σ central recalibrated 0.6 → 0.08 against a top-ETI-0.25 target after the
> validation run surfaced the anticipated stacking double-count). Deviations
> from this plan as written: smokes ran at FULL sample with a short window
> (the kg guard hard-stops `pct_sample ≠ 1`, so the 10% smoke spec was
> impossible); Step 2's per-record persistence was replaced by the cell
> tracker per lock ruling 7 (as anticipated); `run_bathtub_pass`'s external
> signature was left unchanged (σ context built internally), so
> `src/slurm/bathtub.R` needed no functional edit. Verification artifacts:
> `other/top_tax/tests/` (unit/guard tests, smoke checkers, ETI tool),
> `other/top_tax/sigma_validation_out/` (exhibit), vintages
> `202607080756` (σ smoke), `sigma_noop_{pre,post}`, `sigma_zero_{a,b}`,
> `sigma_validation`, `sigma_calib_nosigma`, `sigma_calib_confirm`.

## Context

The top-tax interaction exercise (`other/top_tax/interaction_prospectus.html`, superseding banner in
`frontier_exercise_notes_2026-07-07.md`) needs its centerpiece behavioral margin built: **σ, the
income-conversion response** — owner-managers repackaging top salary as equity appreciation when the
ordinary-vs-equity-path wedge widens. This plan covers build items **1 (wedge), 2 (perturbation run),
3 (σ module)** as re-scoped by author rulings made in this planning session (2026-07-07).

### Author rulings (supersede the prospectus where they conflict)

1. **Gain-state entry = recurrence injection, single-state ("B-pure").** Converted dollars are injected
   as a new inflow term in the kg bathtub recurrence (`delta_next`), parallel to the inheritance inflow.
   They persist as unrealized stock, realize at the holder's **age-specific** rate, and meet the death
   regime like any other gain. NO separate σ-ledger state, NO extra_R-only credit, NO separate hazard.
2. **Wedge = full Bellman extraction, mandatory kg.** τ_eq(a,t) computed exactly by a companion
   linear recursion ("expected PV tax per dollar entering the gain state") on the Bellman's own policy
   grid, per scenario, per year. The crude closed form (ρ/δ/x_regime constants) is **dead as code** —
   exposition only. **σ hard-errors if the scenario is not running kg_dynamics.** No fallbacks.
   The ρ-pinning script/runs are dead (subsumed).
3. **Per-record wedge:** `W_i(t) = mtr_wages{1,2}_i − τ_eq(age_i, t)` — ordinary leg from the record's
   own calculator-measured MTRs (per earner / per leg), equity leg from the recursion (cell-level, keyed
   by age). Forcing = ΔW_i = W_i(static reform) − W_i(baseline), mirroring other modules' MTR-frame use.
4. **Pool:** gate = (scorp_active>0 | part_active>0 | sole_prop>0) AND **taxable income ≥ top-bracket
   threshold** (filing-status-specific, from `ord.` bracket params — threshold-based, NOT MTR-based,
   because QBI etc. push measured MTRs below the statutory top rate). Base = wages + 0.75·(part_active
   + scorp_active + sole_prop) of gated records (SYZZ 0.75).
5. **No phase-in φ(t).** Memoryless annual response to the current-year wedge gap, same convention as
   entity shifting / evasion: `Δconv_i(t) = σ · (W_i(t) − W_i⁰(t)) · pool_i`.
6. **No corporate-base-vs-gain-state split parameter.** Composition is an output (reported in
   diagnostics), not an assumed dial. Sequential module order still prevents double-moves.
7. **σ asserted, not calibrated:** low 0.2 / central 0.6 / high 0.9 (% of pool per pp of wedge), env
   knob. The residualization/perturbation-calibration machinery is dead. Build item 2 is demoted to a
   **validation run**: +5pp top-ordinary leg + a CG mirror leg, 2025:2035, full stack, central σ,
   producing the 2×2 own/cross elasticity matrix vs literature brackets.
8. **Charity is IN the standard behavior stack** for validation + campaign runs.

### 2026-07-08 review rulings (author, post-codex-review — see `other/top_tax/DESIGN_LOCK.md`, which takes precedence)

- **R1 (supersedes Step 1's sketch):** τ_eq defined by FINITE DIFFERENCE against the exact
  `kg_dyn_step_recurrence` dynamics (inject $1 per (age,year) cell, measure PV tax under the actual
  event order — deaths take the full stock; realization at `r_S` incl. the RETIMED planned bucket,
  NOT the Bellman's baseline-scaled `r_exog`). The linear recursion is then implemented and tested
  cell-by-cell against the finite difference. The sketch in Step 1 is a starting point only.
- **R2:** σ stays asserted (0.2/0.6/0.9); Step 6's validation run is an INFORMAL build-time check
  (eyeball the 2×2 matrix vs literature brackets, iterate σ/pool if off) — not a formal fail gate,
  not residual calibration. Known accepted caveat: 0.6/0.9 are total-response anchors while P–P also
  runs (calibration double-count risk the check should surface).
- **R3:** elasticity bundles DROPPED for now — campaign is central-bundle only (no Tier-2 band runs;
  explorer shows central numbers only).
- **R4:** pool stays record-level as written (ruling 4 unchanged); keep the pool-composition
  diagnostic.
- **R5 (new pre-step):** harden `entity_shifting/pearce_prisinzano.R` BEFORE Step 6 validation —
  SECA companion co-scaling (evasion pattern), order/required-MTR guards, conservation diagnostic.
- **R6:** end-of-year injection stands; the finite-difference harness injects the same way so τ_eq
  is consistent with the flow's actual timing by construction. No same-year death split.
- **R7 (supersedes Step 2's persistence):** NO per-record `sigma_conversion/{year}.csv` for normal
  runs. Pre-pass persists cell-level tracker only (conv_inflow by age, pool size, mean wedge, σ
  stamp). The behavior module recomputes record conversions via the SAME shared pure function
  (inputs: static/baseline MTRs + persisted τ_eq cell table) with a hard conservation assert
  Σ(record Δconv) ≈ persisted cell inflow. Per-record dump behind an env knob (smoke/validation
  only). Also: Step 2's pre-pass input frame needs an explicit contract — gate/pool legs
  (`part_active`, `scorp_active`, `sole_prop`, wages legs) come from raw Tax-Data (already loaded
  pre-slim in `kg_dyn_load_cells_inputs`), `txbl_inc` + `mtr_*` columns via widened `read_mtr`
  column lists from static/baseline detail. Modest widening, but write the column list down before
  coding.
- **R8:** `other/top_tax/DESIGN_LOCK.md` is the live-design source of truth; prospectus σ sections
  (crude wedge, phase-in, split param, residual calibration, band readout/Tier 2) are dead.

### Key machinery facts (from exploration; line numbers = branch `wealth`)

- Behavior column is **space-delimited, executes in column order** (`src/misc/config_parser.R:390-393`,
  `src/sim/behavior.R:42-48`). Function name = `do_` + first path segment → σ needs its own folder
  (`conversion/` → `do_conversion`). NO existing runscript composes multiple modules — untested path.
- kg bathtub pre-pass: `kg_dyn_run_bathtub_pass` (`src/sim/kg_dynamics.R:1895`), recurrence
  `kg_dyn_step_recurrence` (:1118, `delta_next = delta_surv + delta_inh` at :1184), cell table
  `kg_dyn_build_cell_table` (:1777, `extra_R = r_S * pmax(dG − corp_gain_debit, −G_B)` at :1875).
  Bellman: `kg_dyn_solve_bellman` (:875) over (age, year) grid to age 119; cell tables already emit
  `r_D_B/r_D_S`, `tau_B/tau_S`, `W_B/W_S`, `c_phi`, `m` — everything the τ_eq recursion consumes.
  Structural template for threading a new per-year-by-age input: `corp_debit_by_year`
  (arg at :1903, sliced at :2128, consumed at :1865-1875; built by
  `corp_kg_state_debit_by_year`, `src/sim/corp_incidence.R:1314`, threaded in `src/sim/run.R:1008-1021`).
- Sequencing inside `run_one_year` conventional pass (`src/sim/run.R:736-810`): corp applier → wealth
  haircut → **behavior modules** → corp kg-quantity step → `do_taxes`. Static pass runs NO behavior
  (σ is conventional-only by construction — static stays law-only).
- SLURM: Phase 2B (`src/slurm/bathtub.R`) runs the bathtub pass after static 2A (whose detail files
  carry the `mtr_*` columns) and after Phase 1 baseline (baseline `mtr_*` detail). So per-record wedges
  ARE computable in the pre-pass. Per CLAUDE.md sync table: `run_bathtub_pass` contract changes →
  update `src/slurm/bathtub.R`.
- Evasion module (`config/scenarios/behavior/evasion/debacker.R`) is the template for an
  "implement-any-logic" module with SECA companion co-scaling (`sole_prop1/2`, `part_se1/2`, `farm1/2`)
  and required-MTR hard stops. `derive_vars` recombines legs inside `do_taxes`
  (`src/calc/utils.R:22-31`); `wages` is reconstructed as `wages1+wages2` at `src/calc/do_taxes.R:79`.
- `calc_mtrs` accepts any var (`src/calc/do_taxes.R:515`); `scorp_active` works as an mtr_var (plain
  bump). `mtr_part_active`/`mtr_sole_prop1` are SECA-inclusive.

---

## Build

### Step 0 — Verify Bellman grid mechanics (read-only, before any code)

Read `kg_dyn_solve_bellman` + `kg_dyn_bellman_sweep_age` closely to confirm: (a) the policy grid is
available as (age × year) with terminal/extended-age handling we can mirror; (b) where per-cell
`tau_S`, `m`, `c_phi_eff`, `beta_by_year`, `r_exog` live at recursion time. The τ_eq recursion design
below assumes backward-in-time availability of next-year continuation — confirm or adapt (e.g.,
stationary continuation beyond the sim horizon, mirroring the Bellman's own terminal assumption).

### Step 1 — τ_eq recursion (new, in kg machinery)

New function `kg_dyn_compute_tau_eq()` in `src/sim/kg_dynamics.R` (or alongside the solver):

```
T(a,t) = r_tot(a,t)·τ(a,t) ·[discounted at realization]
       + (1−r_tot)·β(t)·[ (1−m(a))·T(a+1,t+1) + m(a)·T_death(a,t) ]
T_death = c_phi_eff(a,t)·τ(a,t)      # same death-burden object the Bellman internalizes
                                      # (0 step-up, θ-weighted carryover, ~full deemed, ×(1−p_char))
```

- Inputs are the Bellman's OWN policy and primitives: `r_tot = r_exog + r_D`, cell `tau`, `m`,
  `c_phi_eff`, `beta_by_year`. Linear bookkeeping — no optimization.
- Run TWICE per year-loop: baseline policy (Pass-1 objects → `tau_eq_B(a,t)`) and scenario policy
  (Pass-2 → `tau_eq_S(a,t)`).
- Emit both as new columns on `cell_table` → they land in the state RDS +
  `kg_dynamics_age_profile.csv` automatically (additive columns; existing consumers unaffected).
- Sanity properties to assert in tests: `0 ≤ τ_eq ≤ τ_statutory`; regime ordering
  `τ_eq(step-up) < τ_eq(carryover) < τ_eq(deemed)` at equal rates; τ_eq rises with age
  (less deferral runway... note: under step-up it FALLS with age near death — assert regime-conditional
  monotonicity, not global).

### Step 2 — Conversion computation + gain-state injection (bathtub pre-pass)

New file `src/sim/sigma_conversion.R` (keeps kg_dynamics.R lean; kg calls into it):

- `scenario_uses_sigma(scenario_info)`: any `behavior_modules` starting with `conversion/`.
- `sigma_compute_conversions(tax_units, baseline_mtr_detail, static_mtr_detail, tau_eq_B, tau_eq_S,
  sigma, year)`: per record —
  - gate (ruling 4; taxable income from the static detail, threshold from the scenario's resolved
    `ord.` bracket params joined on the frame);
  - per-leg wedges: wage legs use `mtr_wages1`/`mtr_wages2`; PT legs use `mtr_part_active`,
    `mtr_sole_prop1`, `mtr_scorp_active`; equity leg = `tau_eq_{B,S}(age_i, t)` keyed on the kg age
    convention (pmax(age1,age2), 80+ topcode — match `kg_dyn_aggregate_cells`);
  - `Δconv_leg = σ · pmax(ΔW_leg, ...) · pool_leg` (sign convention: wedge can narrow ⇒ negative
    conversion allowed, clamped so no leg goes negative);
  - returns per-record per-leg conversion frame.
- Persist per year to `{scenario}/conventional/supplemental/sigma_conversion/{year}.csv`
  (id + per-leg Δconv + diagnostics). The behavior module APPLIES this file verbatim — single source
  of truth, no recompute drift.
- Aggregate to age cells → `conv_inflow_by_year` (named-by-age vectors, same shape as
  `corp_debit_by_year`).
- **Injection**: new `conv_inflow_by_year` argument threaded `run_bathtub_pass` (`src/sim/run.R:962ff`)
  → `kg_dyn_run_bathtub_pass` (:1903) → per-year slice → **`kg_dyn_step_recurrence`**: inflow enters
  `delta_next` at end of year t (participates from t+1, like the inheritance inflow). σ dollars then
  realize via `extra_R` and meet death via `deemed_factor` with zero further special-casing.
- Timing note: within the year-t loop the order is Bellman solve → τ_eq recursion → per-record
  conversions (needs τ_eq_S(·,t)) → inject at recurrence step. The Bellman policy is per-dollar
  (stock-independent), so injection creates no fixed-point problem.
- Env knob: `SIGMA_CONV` (default 0.6 central; 0.2/0.9 for sweeps), read once, stamped into the
  diagnostics.

### Step 3 — σ behavior module

`config/scenarios/behavior/conversion/sigma.R`, `do_conversion(tax_units, baseline_mtrs, static_mtrs,
scenario_info, indexes)` — evasion-module pattern:

- **Guards (hard stops):** kg_dynamics present in `behavior_modules`; pinned relative order
  `kg_dynamics/turnover → conversion/sigma → entity_shifting/* → evasion/*` for whichever are present
  (assert on `scenario_info$behavior_modules` positions); required mtr_vars present
  (`wages1 wages2 part_active sole_prop1 scorp_active` + kg's `kg_lt`); sigma_conversion file exists
  for the year.
- **Apply** the persisted per-record file: reduce `wages1`/`wages2` (and `wages` coherently — mind the
  wages-residual convention), reduce PT legs **with SECA companions co-scaled**
  (`sole_prop`+`sole_prop1/2`; `part_active`+`part_se1/2`; `scorp_active` has no companion). Nothing is
  added to record `kg_lt` — converted gains are unrealized; taxation arrives in later years through the
  cell machinery.
- Rationale docstring: forcing/object pair (wedge → payment FORM of labor comp), why order is pinned,
  why no φ, provenance of 0.2/0.6/0.9.

### Step 4 — SLURM + orchestration sync (per CLAUDE.md table)

- `src/slurm/bathtub.R` (Phase 2B): thread the new `run_bathtub_pass` inputs (baseline + static detail
  mtr reads, sigma outputs). Phase-2B deps (Phase 1 + 2A) already provide the needed detail files —
  no new phase, no manifest count changes expected; confirm `setup.R` serialization untouched.
- Sequential path: `do_scenario`/`run_bathtub_pass` wrapper in `src/sim/run.R` gains the same threading.
- `run_one_year` untouched except that `do_behavioral_feedback` now runs a 4–5 module stack (already
  supported); NO worker.R changes.

### Step 5 — Runscripts, reform YAMLs, smoke tests

- `config/runscripts/tests/multi_module_smoke.csv`: kg + entity_shifting + evasion + charity (NO σ) at
  10% sample — de-risk the never-exercised multi-module composition path FIRST, before σ lands.
- `config/runscripts/tests/sigma_smoke.csv`: baseline + a top-rate-hike scenario, behavior =
  `kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker`,
  mtr_vars = `wages1 wages2 kg_lt part_active sole_prop1 rent scorp_active` (+ charity's var when
  stacked), `wealth_financing = none` for speed. 10% sample allowed for smoke ONLY (guards mirror kg:
  σ full runs require pct_sample = 1 — reuse `kg_dyn_check_run_compat` gating since σ requires kg).
- Reform YAMLs for validation: `config/scenarios/tax_law/tests/topord_plus5/ord.yaml` (+5pp on the top
  ordinary bracket rate, full time series + indexation per /policy-config rules) and a CG mirror leg
  (rate cut sized after leg 1 reports the pool-weighted ΔW).

### Step 6 — Validation run + exhibit (build item 2, demoted)

- Two full-sample SLURM runs (2025:2035, stack incl. charity, central σ, `multicore` n/a under
  slurm_run.sh; one year past the 2026–2034 reporting window for FY-lagged legs).
- New analysis script `other/top_tax/sigma_validation.R`:
  - 2×2 matrix: (ordinary income excl. gains, gains realizations) × (τ_ord leg, τ_cg leg). Ordinary
    income concept = taxable-income-excluding-net-gains from detail files (ETI-literature concept);
    subtract the known baseline-on-baseline wages residual before interpreting deltas.
  - Compare vs brackets: SSG 0.12–0.40 (own-ordinary), Mortenson −0.24…−2.4 (ord←CG cross, expect
    below face), Dowd/Mortenson −0.8…−0.9 (own-gains), gains←ord positive/persistent/≪ +2.77.
  - Composition report: share of diverted comp → gain state vs → corp base (ruling 6's "output not
    input"), pool size, mean wedge by year.
  - Symmetry check between the two legs, stated in pool-weighted wedge units.

---

## Files touched (summary)

| File | Change |
|---|---|
| `src/sim/kg_dynamics.R` | τ_eq recursion + cell_table columns; `conv_inflow` arg in bathtub pass + recurrence |
| `src/sim/sigma_conversion.R` | NEW — gate/pool/wedge/conversion computation, persistence, cell aggregation, guards |
| `src/sim/run.R` | `run_bathtub_pass` threading (detail-mtr inputs, sigma outputs) |
| `src/slurm/bathtub.R` | mirror the threading (CLAUDE.md sync rule) |
| `config/scenarios/behavior/conversion/sigma.R` | NEW — `do_conversion` applier + guards |
| `config/runscripts/tests/{multi_module_smoke,sigma_smoke}.csv` | NEW smoke fixtures |
| `config/scenarios/tax_law/tests/topord_plus5/`, `tests/cg_mirror/` | NEW validation reform YAMLs |
| `other/top_tax/sigma_validation.R` | NEW validation/exhibit script |

Explicitly NOT touched: `src/slurm/worker.R`, `setup.R` manifests (no new phase), frozen/static pass
(σ is conventional-only), corp channel, wealth bathtub (σ composes with it automatically — lower wages
→ lower tax bill → forcing F adjusts on its own).

## Verification

1. **Order-of-operations smoke**: run `multi_module_smoke` (sbatch, 10%) — confirms the untested
   multi-module path composes before σ exists.
2. **No-op regression**: with σ absent from the behavior column, a pre-existing kg regression runscript
   must reproduce prior outputs byte-identically EXCEPT the additive τ_eq columns in kg supplemental
   files (smoke-diff harness pattern from `tests/simplify_smoke`).
3. **σ smoke** (sbatch, 10%): (a) per-record wage/PT deltas in conventional detail match the
   `sigma_conversion/{year}.csv` files exactly; (b) Σ(record Δconv) = Σ(cell `conv_inflow`) per year
   (conservation assert, also enforced in-code); (c) `SIGMA_CONV=0` ⇒ outputs identical to σ-off;
   (d) τ_eq sanity asserts (bounds, regime ordering) pass.
4. **Guard tests**: σ without kg_dynamics → hard stop; wrong module order → hard stop; missing
   mtr_vars → hard stop (evasion-style messages).
5. **Validation runs** (full sample, SLURM): elasticity matrix lands inside/near the literature
   brackets; the ord←CG cross below Mortenson face value; symmetry check within tolerance. Direction
   check on the death-regime interaction: rerun the +5pp leg under deemed-at-death → conversion
   response should shrink materially (wedge collapse working end-to-end).
6. All heavy compute via sbatch (never login-node Rscript); full paths printed for all deliverables.

## Sequencing

Step 0 (verify grid) → Step 5's `multi_module_smoke` (de-risk composition, runs while coding) →
Steps 1–2 (kg-side) → Step 3 (module) → Step 4 (SLURM sync) → Step 5 (σ smoke + no-op regression) →
Step 6 (validation runs + exhibit). Estimated: Steps 1–4 are the real build (~3 files of substance);
5–6 are fixtures and orchestration.
