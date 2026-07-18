# Cross-model state-tax validation harness

Record-level validation of the state income tax calculator (`src/calc/state/`)
against **NBER TAXSIM-35** and **PolicyEngine US** — code-review item #9
(cross-model leg) / Phase 5 of the implementation plan. This is the only test
layer that catches *research* errors (wrong parameter values from the state
forms) rather than encoding errors.

## Design

**Record × state × year, unweighted.** Stratified samples of PUF records
(post-federal-calculator) are run counterfactually through *each* state's
calculator and through the external model with that state code. State weights
are not used — this validates law encoding, not geography. Aggregate
validation stays blocked on the Phase 1 weights.

**Canonical year split** (design of record):

| Window | Benchmark | Why |
|---|---|---|
| 2017–2020 | TAXSIM-35 (`usincometaxes`, local WASM) | TAXSIM state law is actually coded through ~2020 |
| 2021–2024 | PolicyEngine US (python, pinned) | TAXSIM 2021+ uses inflated prior-year params; PE state coverage starts 2021 |

PolicyEngine is the tie-breaker for 2021+ disagreements.

**Comparison variable by state class** (vs TAXSIM `siitax` / PE
`state_income_tax`):

- Broad-IIT states (AZ CA CO CT GA IL IN KY MI NC ND NY SC VA): `liab_st_iit`
- NH/TN (Hall-type interest & dividends tax): `liab_st_narrow_iit` — TAXSIM
  has the Hall tax coded correctly for 2017–2020 (verified)
- No-tax stubs (AK FL NV SD TX WY): assert both models return 0
- WA (LTCG excise + WFTC): PolicyEngine only; TAXSIM does not model either
  (excluded via `known_differences.csv`)

Dependent filers (`dep_status == 1`) are excluded from samples in v1 (TAXSIM
mstat-8 semantics differ enough to swamp state signal).

## Running

From the repo root:

```bash
module load R/4.4.2-gfbf-2024a

# One cell
Rscript other/state_tax_research/cross_model/run_cross_model.R \
  --states IL --years 2019 --models taxsim

# Full TAXSIM window
Rscript other/state_tax_research/cross_model/run_cross_model.R \
  --states ALL --years 2017:2020 --models taxsim

# PolicyEngine window (needs the venv below; consider sbatch)
Rscript other/state_tax_research/cross_model/run_cross_model.R \
  --states ALL --years 2021:2024 --models policyengine \
  --pe-python /nfs/roberts/project/pi_nrs36/ji252/venvs/policyengine/bin/python
```

Flags: `--n` (TAXSIM sample/year, default 20000), `--n-pe` (PE sample/year,
default 1500), `--chunk-size` (WASM rows/call, default 10000),
`--force-prepare` (recompute the federal pre-pass cache).

The first run per year executes the full federal calculator on ~220k records
(minutes) and caches to `cache/fed_calc_{year}.rds`; subsequent runs reuse it.

## PolicyEngine venv (one-time)

```bash
module load Python/3.12.3-GCCcore-13.3.0
python -m venv /nfs/roberts/project/pi_nrs36/ji252/venvs/policyengine
/nfs/roberts/project/pi_nrs36/ji252/venvs/policyengine/bin/pip install \
  -r other/state_tax_research/cross_model/pe_requirements.txt
```

The venv is repo-external and not committed; the exact package version is
pinned in `pe_requirements.txt` and recorded per output row.

## Outputs

```
results/summary.csv     committed — one row per state × year × model:
                        n, match_15, match_100, share_both_zero,
                        mean/median/p90/p99 abs diff, mean signed diff
results/reports/<st>.md committed — per-state spot-check documentation
                        (the artifact behind cross_model = done)
results/raw/            gitignored — per-record comparisons + breakdowns
cache/                  gitignored — federal pre-pass caches, PE i/o
```

Match rates are reported at **$15** and **$100** tolerances (PolicyEngine's
own emulator and release-test conventions, adopted by the design of record).

### Stage-diagnosis caveat

The mismatch stage tables compare TAXSIM's state intermediates (v32–v40)
against ours. For federal-taxable-income-start states (CO, ND, SC) TAXSIM's
`v32_state_agi` is not the same concept as our `st_agi` (verified for CO:
the "gap" is mostly just the federal deduction wedge), so a "state AGI"
stage label there does NOT imply a liability error — trust `siitax` and the
match rates, and treat the stage tables as directional only. `staxbc` is
unpopulated for some states (verified for IL) and is not used.

## Known differences

`known_differences.csv` is the machine-readable list of expected
discrepancies. `action = exclude` removes a state-model-window from the match
denominator (reported separately); `action = annotate` documents an expected
divergence pattern without exclusion. Structural sources: TAXSIM's SALT
circularity and 3-round federal↔state iteration (we are one-way until Phase
7), TAXSIM's imputed sales-tax deduction, QBI inputs zeroed in the crosswalk
(fed-taxable-income states CO/ND/SC), TAXSIM 2021+ vintage lag.

## Acceptance (`cross_model = done`)

A state's tracker row (`state_parameter_rollout.csv`) flips to `done` when:

1. **match@$100 ≥ 95%** in every canonical-window cell, on the clean
   (federally aligned) subset where defined, after known-difference
   exclusions — a fully excluded cell (e.g. IL 2021 vs PE, the one-time
   rebate) drops out and the verdict rests on the remaining cells; and
2. residual mismatch clusters are explained in `results/reports/<st>.md`
   (mapped to a known-differences row, or filed as a suspected
   encoding/research bug).

States failing the bar stay `in_progress` with the report as the punch list.

Status as of 2026-07-18: **done** for AK FL NV SD TX WY (stubs), TN, NH,
and IL (TAXSIM window 100% clean; PE 2022–2024 at 99.2–99.5% clean; two
residual IL records are exactly 5% of property tax — the Schedule ICR
property-tax credit is a candidate encoding gap to research).
**in_progress** (harness run, triage open) for the other 14 encoded states;
dominant divergence stages per state are in the reports.
