# Functional-form toggle: calibration + memo runbook

Everything downstream of the code (Part A, shipped + unit-tested) for the
`levels` vs `logs` realization-response comparison. Plan:
`~/.claude/plans/serialized-sauteeing-cocoa.md`. All compute via `sbatch` /
`slurm_run.sh` (never the login node); full sample throughout.

The `logs` form is UNPINNED until Part B stamps `KG_DYN_DEFAULT_ETA_LOGS` and
`KG_DYN_TIMEABLE_SHARE_LOGS`. Any `logs` sim before then hard-stops (the eta=NA
bootstrap). Run the parts strictly in order.

## Sequencing

```
A (code + unit tests) ──► B0 ──► B1 ──► B2 ──► B3 ──► C ──► D
```

### B0 — levels byte-identity (correctness gate)
Proves the toggle left the levels path bit-identical.
```bash
bash other/kg_model_tests/form_ab/launch_byte_identity.sh
# when both jobs finish:
bash other/simplify_cleanup/compare_smoke.sh \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/form_byte_pre \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/form_byte_post
# expect: RESULT: BYTE-IDENTICAL
git worktree remove /nfs/roberts/scratch/pi_nrs36/jar335/form_byte_pre_wt
```

### B1 — pin eta_tilde (logs long-run)
```bash
bash other/kg_model_tests/form_ab/launch_eta_dial_logs.sh      # 3 x 30-yr vintages
# after all three reach Phase 3b:
sbatch other/kg_model_tests/form_ab/measure_efull_logs.sbatch  # -> eta_tilde_fit.csv
```
Take `eta_tilde_pw` (piecewise-linear inversion; the net-of-tax curve may bend,
so do NOT force the through-origin line). Confirmation run at eta_tilde*:
`launch_eta_dial_logs.sh`-style single vintage, assert E_full within +-2% of -2.52.

### B2 — pin timeable_share_logs (logs short-run)
Given eta_tilde*, hand-iterate the share (start 0.2542) against the 5.04 moment:
```bash
bash other/kg_model_tests/form_ab/launch_timeable_logs.sh <eta_tilde> 0.2542
# measure (see the printed sbatch line) -> E_full_short vs 5.04; adjust + repeat
# (~3 sims). Long-run E_full is timeable-invariant, so eta_tilde stays put.
```

### B3 — stamp
Paste `KG_DYN_DEFAULT_ETA_LOGS` + `KG_DYN_TIMEABLE_SHARE_LOGS` into
`src/sim/kg_dynamics.R`; fill the `logs` entry in `KG_DYN_CALIB_PROVENANCE`
(eta, timeable_share, date, tax_data_vintage); update the two `*_LOGS` rows in
`calibration_reference.csv` (value, date, `code_sha`, and seed the eta row's
reference_moment via `KG_RESPONSE_FORM=logs calibration_watch.sbatch <baseline>
--seed`). `KG_DYN_SPEC_VERSION` stays 3.

### C — memo runs (both forms; baseline built once, shared via baseline_vintage)
```bash
bash other/kg_model_tests/form_ab/launch_form_memo.sh   levels   # then, after 3b:
bash other/kg_model_tests/form_ab/launch_form_memo.sh   logs
bash other/kg_model_tests/form_ab/launch_form_laffer.sh levels   # then, after 3b:
bash other/kg_model_tests/form_ab/launch_form_laffer.sh logs
```

### D — memo
```bash
python3 other/kg_model_tests/form_ab/collect_form_results.py   # -> form_ab_results.json
# fill functional_form_memo.md placeholders from that JSON; sync the HTML artifact.
```

## Compute budget
~34 (Laffer) + 6 (experiments) + 4 (eta_tilde grid + confirm) + ~3 (timeable)
+ 2 (byte-identity) ≈ 49 full-sample runs, B before C. `delete_detail=1`
everywhere except B0 (which keeps detail for the byte-diff); the memo reads
`supplemental/revenue_estimates.csv` + `totals/receipts.csv`, never per-record
detail.

## Files
| file | part | role |
|---|---|---|
| `byte_identity.sbatch`, `launch_byte_identity.sh` | B0 | levels bit-identity pre/post worktree run |
| `launch_eta_dial_logs.sh` | B1 | 3 logs eta-dial vintages (KG_ETA_LOGS grid) |
| `measure_efull_logs.R`, `.sbatch` | B1 | measure E_full, invert for eta_tilde* |
| `launch_timeable_logs.sh`, `measure_shortrun_logs.R` | B2 | short-run share pin |
| `launch_form_memo.sh` | C1 | three experiments x two forms |
| `launch_form_laffer.sh` | C2 | 17-cell Laffer x two forms |
| `collect_form_results.py` | D | run outputs -> form_ab_results.json + tables |
| `functional_form_memo.md` | D | the memo (source of record) |
