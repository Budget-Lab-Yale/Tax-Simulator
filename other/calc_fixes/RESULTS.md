# Calc-layer bug fixes — measured impact (2026-07-01)

Five confirmed bugs from `other/calc_functions_review_2026-07-01.md` were fixed,
unit-tested, and quantified against full-sample (pct_sample = 1) baseline runs,
years 2025:2027, on branch `wealth`. Each fix is its own commit so impacts were
attributed by diffing *consecutive cumulative* runs (each marginal delta
isolates exactly one fix):

| variant | commit      | adds fix |
|---------|-------------|----------|
| c0      | `2a34b1a32` | (pre-fix reference) |
| c1      | `178508678` | #4 payroll ee/er split (revenue-neutral; folded into c2's diff) |
| c2      | `62be3e4a1` | #6 magi_ss tax-exempt interest |
| c3      | `f4ba6f0ea` | #5 1250/collectibles stacking |
| c4      | `0540f1cf2` | #3 CDCTC earned-income cap |
| c5      | `205ee8e15` | #8 dependent standard deduction bonus |

All 18 synthetic-record unit tests pass (`tests_unit.R`, log
`logs/unit_16953048.out`). Model outputs under
`/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/calcfix_{c0,c2,c3,c4,c5,c0x,c5x}`.

## How much each bug was distorting the model

**Positive = the bug was understating revenue.** "Affected units" = tax units
whose net income tax changed, 2026, weighted.

| Fix | What was wrong | Baseline revenue error ($B/yr, 2025/26/27) | Affected units | Mean per affected |
|---|---|---|---|---|
| **#5** 1250/collectibles dropped from base (`tax.R`) | Special-rate gain silently untaxed whenever plain preferred income > ordinary income | **+8.08 / +8.37 / +7.83** | 395K | +$21,185 |
| **#6** magi_ss omitted tax-exempt interest (`agi.R`) | SS benefit taxability understated for muni holders (+$2.1–2.3B/yr taxable SS) | **+0.123 / +0.119 / +0.131** | 456K | +$261 |
| **#3** CDCTC earned-income cap doubled (`cdctc.R`) | Units with young + old dependents got up to 2× the statutory expense cap | **+0.0027 / +0.0039 / +0.0037** | 14K | +$276 |
| **#8** dependent std ded lost age/blind bonus (`std_ded.R`) | Low-earning elderly/blind claimed dependents overtaxed | **−0.0012 / −0.0013 / −0.0014** | 9K | −$151 |
| **#4** `liab_pr_ee` omitted Additional Medicare Tax (`pr.R`) | ee/er breakdown didn't reconcile with `liab_pr` | **0** (breakdown only: `liab_pr_ee` was understated **$15.9 / $17.4 / $19.0B/yr**, ~2.1%) | 9.2M pay AMT | — |

Detail on the headline item, **#5**: baseline `liab_1250` was $2.34B and should
be $8.56B (2026); `liab_collect` $0.46B → $2.45B. Roughly **72% of Section 1250
recapture revenue and 81% of collectibles-rate revenue was missing** — the
dropped population is depreciation-recapture sellers/collectibles sellers whose
portfolio income exceeds ordinary income, which is the *typical* profile for
those gains. Cumulative baseline understatement is ~$80B over a 10-year window.

Fix #4 verification: post-fix `liab_pr_ee + liab_pr_er = liab_pr` exactly;
`liab_pr`, `liab_pr_er`, and `liab_add_med` all byte-unchanged (the fix moved
no revenue, it repaired the employee-side aggregate consumed by
`summary_stats.R` and `config_parser.R`).

## Effect on reform scores (pre-fix c0 vs post-all c5)

| Scenario | What it probes | Score change from fixes |
|---|---|---|
| `tests/kg_top_5pp` (top LTCG 20→25%) | #5 | ≤ $0.4M/yr (≤0.001%) — **unaffected** |
| `tests/sd_bump_10k` (+$10K std ded) | #8 | −$68M (2026) / −$86M (2027) on a $169B/$226B cost (~0.04%) |
| `tests/ex_cdctc` (CDCTC repeal, i.e. the tax expenditure) | #3 | tax expenditure was inflated $2.2M/$3.6M/$3.7M |

Takeaway: these bugs distorted **levels** far more than **deltas** — published
scores for rate-style reforms are essentially unaffected. Scores that would have
been materially wrong pre-fix: anything directly repricing 1250/collectibles
(e.g. changing `pref.unrecapture_rate`/`collectibles_rate` — the affected
population's marginal gain dollars weren't in the base at all), SS-taxability
reforms (muni add-back missing), and CDCTC reforms for two-bucket families.

## Notes / follow-ups

- `tests/cdctc` turned out to be baseline-equivalent (zero-delta config); the
  CDCTC probe above uses `tests/ex_cdctc` instead.
- Baseline `liab_iit_net` is now ~+$8.5B/yr higher. **Any byte-comparison
  against pre-fix vintages (incl. the kg regression harness reference runs)
  must re-run its reference at ≥ `205ee8e15`.**
- Not picked up (per review triage): #2 CTC sequential-phaseout no-op (only
  live in `booker_repeal_*` test configs), #1/#9 dead-lever documentation, #10
  EITC joint-earnings question (ask author), doc-only nits.

## Artifacts

- Fix commits: `178508678`, `62be3e4a1`, `f4ba6f0ea`, `0540f1cf2`, `205ee8e15`
- Unit tests: `other/calc_fixes/tests_unit.R` (+ `run_tests_unit.sh`)
- Attribution runs: `other/calc_fixes/run_sim_{c0,c2,c3,c4,c5,c0x,c5x}.sh`,
  runscripts `config/runscripts/tests/calc_fixes_{baseline,reforms,excdctc}.csv`
- Comparison: `other/calc_fixes/compare.R`; outputs `levels_by_variant.csv`,
  `marginal_deltas.csv`, `reform_cmp_*.csv`; log `logs/compare_16953534.out`
