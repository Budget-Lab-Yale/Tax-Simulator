# Planned-Bucket Timing Model: Calibration and Results

**Date:** 2026-05-13
**Branch:** `wealth`
**Output vintage:** `kg_dyn_calibrated`
**Runscript:** `config/runscripts/tests/kg_dynamics_all.csv`

This memo records the headline numbers from the first calibrated run of the
three-bucket realization timing model (fixed / ordinary-Bellman / planned).
The planned bucket handles announced-shock anticipation as a pure
conservation-of-dollars timing channel; the Bellman handles ordinary
permanent realization decisions. See `src/sim/kg_dynamics.R` for the
implementation.

---

## 1. Calibrated parameters

Set by `other/kg_model_tests/calibrate.R` against two targets:

| Parameter | Value | Target moment |
|---|---:|---|
| `KG_DYN_DEFAULT_PSI` | 26.5673 | long-run `dlog(R)/dτ = −2.52` (permanent +1pp shock, sim-year 30) |
| `KG_DYN_SHARE_PLANNED` | 0.3285 | short-run `dlog(R(t))/dτ(t+1) = +5.04` (delayed +5pp shock, year 1) |

Fixed by assumption (not calibrated):

| Parameter | Value | Role |
|---|---:|---|
| `KG_DYN_SHARE_FIXED` | 0.40 | nonresponsive realization share |
| `KG_DYN_TIMING_REF_WEDGE` | 0.05 | wedge differential at which the full planned bucket retimes |
| `KG_DYN_TIMING_WINDOW` | 1 | one-year lookahead/lookback for retiming |

Short-run target was chosen as twice the magnitude of the long-run target
(opposite sign — future-tax-up implies realize-today). Reference wedge of
0.05 means a 5pp tax differential moves 100% of the planned bucket; a 1pp
differential moves 20%; etc.

---

## 2. Delayed scenario (announce 2026, +5pp permanent from 2027)

Realization-weighted top-bracket τ shift averages ~3.65pp (the +5pp applies
only to the top LTCG rate, not the population-average rate).

| Year | R_B ($B) | R_S ($B) | %ΔR | What's happening |
|---:|---:|---:|---:|---|
| 2026 | 1605.5 | 1950.9 | **+21.5%** | announcement spike: planned bucket pulls forward from 2027 + Bellman anticipation |
| 2027 | 1543.7 | 1055.4 | **−31.6%** | payback (2026 hole) + contemporaneous response to the now-active hike |
| 2028 | 1459.2 | 1341.2 | −8.1% | steady-state permanent response, still ramping |
| 2029 | 1429.8 | 1313.5 | −8.1% | |
| 2030 | 1434.6 | 1315.3 | −8.3% | |
| 2031 | 1459.8 | 1339.0 | −8.3% | |
| 2032 | 1497.1 | 1367.7 | −8.6% | |
| 2033 | 1542.4 | 1410.3 | −8.6% | |
| 2034 | 1592.9 | 1456.2 | −8.6% | |
| 2035 | 1648.8 | 1510.2 | −8.4% | approaching the long-run target |

**Implied short-run semi-elasticity** at the announcement-year aggregate:

```
log(1950.9 / 1605.5) / 0.0365 = +5.34
```

vs. calibration target of +5.04. Small overshoot consistent with the
realization-weighted τ shift in the actual scenario (top-bracket-concentrated)
being more concentrated on planned-bucket-active cells than the uniform
synthetic shock used in calibration.

**Long-run semi-elasticity** at year 2035:

```
log(1510.2 / 1648.8) / 0.0365 = −2.40
```

vs. target −2.52. Still ramping toward steady-state (calibration is anchored
at sim-year 30; with 2026 + 9 = 2035 we're at sim-year 10 here). Consistent
with the long-run profile shown in the calibration log.

---

## 3. Temporary scenario (high 2026 only, revert 2027) — falsification test

This scenario is the smoking gun for the planned-bucket design. The pure
Bellman model has no mechanism to produce a clean pull-back + payback under
a one-year shock; the planned bucket does it naturally via dollar
conservation.

| Year | R_B ($B) | R_S ($B) | %ΔR | What's happening |
|---:|---:|---:|---:|---|
| 2026 | 1605.5 | 1133.1 | **−29.4%** | deferral: planned bucket pushes back to 2027 + contemporaneous response to the active 2026 hike |
| 2027 | 1543.7 | 1947.4 | **+26.2%** | payback spike: deferred dollars arrive in the low-tax year |
| 2028 | 1459.2 | 1460.4 | +0.1% | back to baseline (no permanent response) |
| 2029 | 1429.8 | 1431.0 | +0.1% | |
| 2030 | 1434.6 | 1435.6 | +0.1% | |
| 2031+ | … | … | ≤ +0.1% | drift to baseline |

The 2028+ row being essentially zero confirms dollars really do flow to
2027 — no leakage out of the conservation accounting.

---

## 4. Provenance

- **Code commit:** see `git log -1` on branch `wealth` at the time of run.
- **Baseline:** `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/wealth_calib_202605070813` (used for both calibration and the calibrated regression).
- **Calibration run:** `other/kg_model_tests/calibrate_11617364.out`.
- **End-to-end run:** SLURM jobs 11618371–11618377, vintage `kg_dyn_calibrated`. Full output tree at `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/kg_dyn_calibrated/`.
- **Tax-Data vintage:** 2026050315/baseline.
- **Macro-Projections vintage:** v3/2026022522/baseline (for real-rate β).

Per-scenario detail (cell-level summary CSVs) lives at
`.../<scenario>/conventional/supplemental/kg_dynamics_summary.csv`.

---

## 5. Other scenarios run (not tabulated here)

The same runscript executed the standard kg_dynamics regression set
alongside the timing scenarios:

- `baseline_check` — no-policy-change behavioral run; should match baseline.
- `rate_up_5pp`, `rate_down_5pp` — permanent ±5pp from 2026, step-up regime.
- `rate_up_carryover`, `rate_up_deemed` — +5pp paired with regime changes.
- `carryover`, `deemed` — pure regime change without rate change.
- `legacy_rate_up_5pp`, `legacy_rate_down_5pp`, `legacy_delayed`, `legacy_temp` — same scenarios run under the legacy reduced-form `kg/62` module for comparison.

Outputs available at `.../kg_dyn_calibrated/<scenario>/conventional/supplemental/`.
