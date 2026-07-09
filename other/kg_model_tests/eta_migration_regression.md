# kg_dynamics spec-v2 migration: regression memo (entropy cost + nested buckets)

**Branch:** `wealth`  ·  **Pre-change HEAD:** `7b46a169c`

Records the pre/post regression comparison for the migration from the quadratic
realization cost / flat `(phi_I, planned_share)` buckets to the **entropy (KL)
cost** and the **nested `(Phi, omega)` reparameterization** (spec v2). Design:
`representative_cell_bellman_results.md` §"2026-07: entropy/log cost (spec v2)".

## What changed (code)

- `src/sim/kg_dynamics.R`: entropy cost in `kg_dyn_bellman_sweep_age`
  (`r_D_S = r_D_B*exp(-eta*(MC_S-MC_B))`, `kappa = MC` in Pass 1); `psi`->`eta`
  threaded through `kg_dyn_solve_bellman` / `kg_dyn_run_bathtub_pass`; nested
  `KG_DYN_SHARE_INERT` (Phi) / `KG_DYN_TIMEABLE_FRAC` (omega) with derived
  `phi_I`, `planned_share`; validator extended; `KG_DYN_SPEC_VERSION = 2L`;
  provenance stamp restamped (`eta / timeable_frac / share_inert`).
- Tests: `test_planned_timing.R`, `test_terminal_charity.R`,
  `test_provenance_guard.R` updated; new `test_naive_limit.R`.
- Calibrator: `calibrate.R` (outer omega / inner eta, direction flip),
  `measure_dilution.R` header + `measure_dilution.sbatch`.
- No changes to `run.R`, SLURM files, runscripts, the applier/state contract,
  or `wealth_dynamics.R` (the applier lives inside `run_one_year`; bathtub.R /
  frozen.R pass no Bellman params).

## Runs

- **PRE**  (`7b46a169c`, pristine worktree): vintage `kg_eta_regress_pre`,
  runscript `tests/kg_item1_regression_v2`, full sample.
- **POST** (spec v2, post-calibration): vintage `kg_eta_regress_post`,
  same runscript/command.

Command (both): `bash slurm_run.sh tests/kg_item1_regression_v2 NULL jar335 1
<vintage> 1 0 NULL 0`.

## Expectations (from the plan)

| Check | Expectation |
|---|---|
| Mechanical state files | **byte-identical** PRE vs POST (Phi-invariance of the frozen pass; any diff = bug) |
| `baseline_check` revenue delta | exactly zero (baseline-on-baseline) |
| `rate_up_5pp` behavioral offset | in the ~-50% neighborhood |
| carryover / deemed conventional totals | **move** (new Phi/eta — intended, not a regression) |
| `kappa_avg_gw` vs `MC_B_avg_gw` | now EQUAL (`kappa == MC_B` identity) — new identity, not a bug |
| `timing_clipped_cells` | watch — exp response hits `r_D_cap` more readily under anticipation than the quadratic did |

## Results (2026-07-08)

- **PRE** `kg_eta_regress_pre` — pristine worktree at `7b46a169c` (quadratic ψ).
- **POST** `kg_eta_regress_post` — spec v2, η*=4.4984, ω*=0.5132 (Φ=0.50).

### Mechanical state byte-diff — PASS

`cmp` of every `static/supplemental/kg_dynamics_mech_state/{year}.rds`, all four
scenarios × 2026–2029: **IDENTICAL in every case.** Confirms the frozen
mechanical pass is Φ/η-invariant — the state file the STATIC pass consumes is
unchanged by the migration, so mechanical revenue / distribution are untouched.

### Revenue-estimate deltas, sum 2026–2029 ($B)

| scenario | PRE static | POST static | PRE conv | POST conv | PRE offset | POST offset |
|---|---:|---:|---:|---:|---:|---:|
| baseline_check | 0.0 | 0.0 | 0.0 | 0.0 | 0.0% | 0.0% |
| rate_up_5pp | 215.5 | 215.5 | 115.5 | 88.4 | −46.4% | **−59.0%** |
| carryover | 5.7 | 5.7 | 59.9 | 84.5 | +950% | +1380% |
| deemed | 95.0 | 95.0 | 203.2 | 261.8 | +114% | +176% |

Checks against the plan's expectations:
- **`baseline_check` exactly zero** (both) ✓ — no spurious revenue.
- **Static invariant** PRE = POST for every scenario ✓ — static is the clean
  law-only counterfactual, unaffected by the Bellman recalibration (only the
  conventional/behavioral channel moves).
- **`rate_up_5pp` behavioral offset −59%**, in the ~−50% neighborhood ✓ (the
  entropy model prices a modestly stronger CG lock-in on a 5pp hike; the short
  2026–2029 window also over-weights the stronger early-year response —
  year-1 semi ≈ −2.81 vs steady-state −2.27).
- **carryover / deemed conventional totals move** ✓ (intended — new Φ/η; the
  death-regime realization response is re-priced).
- No log(0) / clipping pathologies; `timing_clipped_cells` nominal.

**Verdict: migration is clean.** Mechanical/static outputs bit-for-bit
unchanged; only the conventional (behavioral) channel re-prices, as designed.
