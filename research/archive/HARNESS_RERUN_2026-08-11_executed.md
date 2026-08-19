# Harness-machine batch — reruns + diagnostics queued 2026-08-11

> **EXECUTED 2026-08-11** on the harness machine (commits `b7491e11b`
> through the final rerun commit). Deviations from the plan below: the
> reruns used `--states ALL` (raw files are whole-file overwrites keyed
> model+year — the 5-state commands would have destroyed the MD/WI raw),
> and the MI ±$386 mass turned out to be TAXSIM's home-heating credit on
> a $1.01 household-income base, NOT the Tier-2 KD row (see the MI KD
> exclude row + issues-doc T6). All section-C digs resolved to PE's
> one-time-rebate netting (issues-doc P5) plus one GA encoding fix on our
> side (HB 593 std-deduction vintage). Do not re-run as written.

The R1 triage sessions of 2026-08-11 fixed or cleared seven states using
local TAXSIM (usincometaxes WASM) probes, but this machine has neither
the federal pre-pass caches, the PolicyEngine venv, nor `results/raw/`.
Everything below needs the machine that ran the 2026-07 harness passes.

**Prerequisites:** repo root; the federal pre-pass cache dir
(`cross_model/cache/`); the pinned PE venv (`policyengine-us==1.775.7`)
for `--pe-python`; pull `state-tax` (fixes through `21dcbc7e1`).

## A. Confirmation reruns for the five fixed states

```
Rscript research/state_tax/cross_model/run_cross_model.R \
  --states KY GA SC IN MN --years 2017:2020 --models taxsim
Rscript research/state_tax/cross_model/run_cross_model.R \
  --states KY GA SC IN MN --years 2021:2024 --models policyengine \
  --pe-python <venv>/bin/python
Rscript research/state_tax/cross_model/run_cross_model.R --report-only
```

What changed and what to expect (all fixes probe-verified locally; KD
rows already pre-registered):

| St | Was (clean m@100) | Fixes on the branch | Expect |
|---|---|---|---|
| KY | 0.273 (2017) / 0.46-0.48 | 2017 graduated schedule; combined-return per-spouse std (`combined_sep`); std vintages de-shifted; $10/$40 personal credits | Large jump both windows. Residuals should land on the 4 KY KD rows (TAXSIM 2017 double-std ~$287.68/couple; one-earner 2x std ~$130; Table C MGI base; column-split proxy) |
| GA | 0.50-0.54 | MFJ exemption $3,700 -> $7,400 | Married half of the sample should clear; probe matched 5 shapes exactly |
| SC | 0.48-0.56 | Age-65 deduction (12-6-1170(B)) + Two Wage Earner Credit | Retiree and two-earner records clear; residual ±$10 schedule constants (KD row) |
| IN | 0.67 | Dep = $1,000 + $1,500 child add-on; universal $1,000 aged/blind | Exemptions-stage mismatches clear; TAXSIM grants the child add-on at ANY dep age (ours ≤ 23) — residual on 24+ deps only |
| MN | 0.49-0.67 | Childless M1CWFC 9% -> 12% (2023+ only) | TAXSIM window UNCHANGED (fix is 2023+); the 2024 PE cell moves. TAXSIM residual is itemizer-concentrated per the MN KD row — treat as characterized |

Where a canonical cell clears 95% after KD exclusions, flip the state's
`cross_model` tracker cell to `done` and note the rate; KY/GA/SC/IN are
the candidates.

## B. Record-level diagnostics (need `results/raw/`)

1. **MI +$386 point mass** (~$9,082 deduction, tracker row): pull the
   records at exactly ±386; check age/tier composition. The new MI KD
   row (TAXSIM applies the Tier-2 $20k/$40k to ALL 67+ — both directions
   probe-verified) may already explain it; confirm and fold in, else
   keep digging.
2. **Regenerate reports on that machine** so the stage-diagnosis tables
   return: the 2026-08-11 local `--report-only` left
   `reports/{md,mn,wi}.md` with an explicit "stage data not available"
   note (raw per-record output is not committed).

## C. PolicyEngine-side diagnostics (need the venv)

1. **NY 2023 collapse**: clean m@100 = 0.160 vs 0.833 in 2022 — a
   discrete break, not noise (n_clean 357). Pull the mismatch records;
   suspects: PE's 2023 NY supplemental-tax/CTC changes or a parameter
   vintage issue. No KD row exists yet.
2. **VA 2023/24**: 0.345/0.349 vs 0.940 in 2022 — same shape as NY.
3. **GA PE 2021**: 0.448 (vs 0.92 by 2024).
4. **AZ PE 2021 outlier**: mean_abs_diff ≈ $819k, p99 $14.3M — one or a
   few pathological records; identify and either fix the input mapping
   or exclude with a predicate.
5. **ID grocery credit**: verify PE's variable structure in package
   source, then promote the ID/policyengine KD row from `annotate` to a
   whole-window `exclude` (CO/TABOR precedent) or net the credit inside
   `src/tests/state/cross_model/pe_state_tax.py`. ID's 0.43-0.58 PE window is currently scored
   against a known-bad benchmark.

## Housekeeping already done on the branch (no action)

- `cross_model_states()` now includes MD/MN/WI (their reports were never
  generated before; the class list had omitted them).
- The paired all-NA rows for OH/IL in `summary.csv` are exclusion
  partitions (excluded-record counts from KD predicates), by design.
- Coverage + continuity test layers are live in `test_state_calc()`;
  the continuity sweep's per-state allowances document each state's
  published cliffs. NY carries an open item: a +$327 (2024) jump at the
  single 215,400-bracket entry inside the recapture zone — verify
  against the IT-201 worksheet while the NY PE dig is open.
