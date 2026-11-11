---
title: "State gap diagnostics: modeled state income tax against published figures"
role: evidence
workstream: state_tax
status: current
updated: 2026-11-10
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# State gap diagnostics

Compares modeled state income tax liability, from a full-sample state-mode
baseline run, with two kinds of published figure: Census state tax collections
by state and year, and state revenue agency tables by income class. The purpose
is to locate where the model and the published figures differ, by state, year
and income class. It does not size a gap precisely, and nothing here is a
revenue estimate.

## Scripts

| Script | What it does |
|---|---|
| `research/state_tax/aggregate/01_gap_slices.R` | Model side. Reads a run written with `state_detail = 2` (the long record-by-state component file) and writes weighted state totals by state, federal AGI class and one taxpayer characteristic at a time, plus totals by modeled state-AGI class for the agency comparison. |
| `research/state_tax/aggregate/02_gap_report.R` | Comparison. Part A: modeled liability against Census collections by state and year. Part B: modeled resident totals against agency tables by state-AGI class and measure. |
| `research/state_tax/aggregate/gap_bins.R` | The income-class edges both scripts share. |

**Benchmarks** come from the State-Ind repo (`Budget-Lab-Yale/State-Ind`), read
from the shared production root at
`raw_data/State-Ind/aligned/state_pit_benchmarks.csv`:

- Census Annual Survey of State Government Tax Collections, item T40 (individual
  income tax). Tax year t is compared with the mean of state fiscal years t and
  t+1. 43 states have a series; DC does not.
- New York Department of Taxation and Finance (full-year residents, TY2017-2024),
  California Franchise Tax Board table B-4A (residents, TY2017-2022) and
  Connecticut Department of Revenue Services (residents, TY2021-2024).

**Liability concept by agency.** New York's published liability is net of
refundable credits, so it is compared with `liab_st_iit`. California's and
Connecticut's are before refundable credits, so the model adds them back.

## Runs

All are baseline, `states = all`, TY2017-2025, full sample, on Tax-Data and
State-Weights vintage `2026102315_rebuild`, written to the production root under
`model_data/Tax-Simulator/v1/`.

| Run | What it is |
|---|---|
| `sw_states_components` | First run with the component file (runscript `config/runscripts/tests/states_all_components.csv`). |
| `sw_states_nfgate` | The same after commit `346e0b29e`: a federal non-filer files no state return. **Current.** |

## Headline results (`sw_states_nfgate`)

**Against Census collections.** The ratio of modeled liability to collections,
summed over the 43 states:

| TY2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|---|---|---|---|---|---|---|---|
| 0.98 | 0.99 | 1.01 | 0.97 | 1.02 | 0.99 | 1.06 | 1.09 |

The non-filer fix lowered these by about 3 points in most years (1.02 to 0.99
in TY2022). By state, 31 of 43 are within 10% in TY2019 and 25 in TY2022.
Low: DE (0.73-0.83 in every year), NH (0.62-0.86), ND (0.60-0.76 from 2021,
0.81-0.84 before), IN (0.67-0.79 through 2021, 0.92-0.94 after). High: NJ
(1.14-1.38 in every year), CT (1.12-1.19 from 2019 apart from 1.07 in 2020), MD
(1.12-1.22 from 2021), and from 2023 CO (1.39, 1.19) and NE (1.24, 1.43). Collections
include nonresidents, entity-level tax payments and timing, so a ratio away from
1 is a place to look, not an error of that size. The state-by-year grid is
`research/state_tax/aggregate/results/census_ratio_sw_states_nfgate.csv`, with a
heatmap beside it.

**Against agency tables**, model divided by agency, all classes summed:

| State | Measure | TY2017-2020 | TY2021 onward |
|---|---|---|---|
| NY | state AGI | 1.02-1.08 | 1.03-1.08 |
| NY | tax before credits | 1.02-1.09 | 1.03-1.11 |
| NY | liability | 1.08-1.17 | 1.36-1.49 |
| CA | state AGI | 1.06-1.09 | 1.06-1.11 |
| CA | liability | 1.04-1.11 | 1.14 (2021), 1.31 (2022) |
| CT | tax before credits | not published | 1.01-1.07 |
| CT | liability | not published | 1.20-1.30 |

New York's income and tax before credits match throughout; liability breaks away
in TY2021, when the credit for the pass-through entity tax arrives, and the break
sits above $2M of state AGI. That is the evidence behind section 3.2 of
`research/state_tax/plan.md` and `research/state_tax/ptet/ptet_proposal.md`.
Modeled return counts also run 1.22-1.25 times the NY and CA figures in TY2020
and TY2021 against 1.03-1.10 in other years; this is not yet explained. The full
grid by class is
`research/state_tax/aggregate/results/agency_ratio_sw_states_nfgate.csv`.

## How to regenerate

From the repo root, after a state-mode run with `state_detail = 2`. The first
script reads about 20 million component rows a year, so run it as a batch job,
not on the login node.

```bash
module load R/4.4.2-gfbf-2024a
Rscript research/state_tax/aggregate/01_gap_slices.R <run> 2017:2025
Rscript research/state_tax/aggregate/02_gap_report.R <run>
```

Outputs land in `research/state_tax/aggregate/results/`. The slice file
(`gap_slices_<run>.csv.gz`) is gitignored; the ratio tables and heatmap are
tracked.

## Revision history

- **2026-11-10** — written, from the 2026-10-26 runs.
