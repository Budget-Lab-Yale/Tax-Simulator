---
title: "Pass-through entity taxes (PTET): a proposed update to the federal and state models"
role: notes
workstream: state_tax
status: open
updated: 2026-11-10
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Pass-through entity taxes (PTET): a proposed update to the federal and state models

**For:** John Ricco. **From:** John Iselin. **Status:** draft for discussion; nothing here is implemented.

## Summary

States let a partnership or S corporation elect to pay its owners' state income tax at the entity level. That is a pass-through entity tax (PTET). The entity deducts the tax federally without limit, so the owners escape the $10,000 SALT cap. SALT is the itemized deduction for state and local taxes, capped since 2018. Owners then get a state credit, or an income exclusion, for the tax the entity paid.

Tax-Simulator models one half of this today, and only roughly. The state calculator models none of it.

The joint committee paper by Comey, Gallagher and Gorman (JCT, October 2025) now gives a credible national calibration. A state-by-state law table built for this proposal gives the state side.

I propose:
- **One PTET amount per record,** built once from state PTET law and an election rule based on each owner's federal gain. Both the federal and the state calculators read it.
- **Calibration** to the JCT TY2024 figures: $64.0B of SALT moved off individual returns, a $18.1B federal cost, and about 80% of the benefit in the top 1%.
- **Validation** against published state PTET credit totals: NY, CA, CT, NYC and MA.

The change fixes three things in the federal baseline:
- It credits the workaround before any state allowed it, back to 2017.
- It applies the workaround in states with no PTET.
- It cannot respond to the 2025 cap changes.

It also closes most of the post-2021 overshoot in modeled NY and CA state liability.

## 1. Why now: two independent signals

**State side.** We now compare modeled resident state income tax with state agency tables by income class. Those tables live in the new State-Ind repo; the comparison is `research/state_tax/aggregate/02_gap_report.R` on run `sw_states_nfgate`.

Before 2021 the model is close:
- NY liability runs 1.08–1.17 times the NY Department of Taxation and Finance (DTF) figure for full-year residents.
- CA runs 1.04–1.11 times the Franchise Tax Board (FTB) figure.

From 2021 the gap jumps:
- NY: 1.36–1.49.
- CA: 1.31 in 2022.
- CT: 1.20–1.30.

The jump sits almost entirely above $2M of state AGI. There, modeled NY liability is 1.6–2.2 times the agency figure, while state AGI in the same classes is within 0.95–1.14. NY's tax before credits matches within about 10% throughout. The wedge is credits: NY's credits for full-year residents jump from $4.8B (TY2020) to $14.8B (TY2021), when the PTET credit arrives. The model has no PTET credit.

**Federal side.** Section 2 shows that the current workaround moves roughly the right 2024 amount to roughly the right people, but in the wrong years and the wrong states.

## 2. What the model does today

### Federal: `do_salt_workaround_baseline()`

The function is in `src/calc/do_taxes.R`, added December 2023 in commit `184ca482e`. It is applied to baseline data in `src/sim/run.R` before `do_taxes()`.

- **Switch.** It is on whenever the SALT cap is finite or the AMT exemption is finite. The AMT condition makes it on in every year, including 2017.
- **Amount.** Each record's itemized state income-or-sales tax (`salt_inc_sales`) is split by its partnership and S-corp shares of gross income.
- **Participation.** A per-record draw (`r.salt_workaround`) against 75% (`item.salt_workaround_allowed_sstb` / `_non_sstb`) decides who moves it. Partnerships are gated by `salt_workaround_allowed_part = 1`.
- **Effect.** The moved amount leaves `salt_inc_sales` and is added to partnership or S-corp active losses. Pass-through income falls, and the moved SALT stops counting against the cap.
- **Calibration.** The code says it was "calibrated to hit $20B annual estimate from TPC". That figure is a federal revenue cost.

What it moves, from `research/state_tax/ptet/01_quantify_workaround.R` (Tax-Data `2026102315_rebuild`, full sample). The federal cost is the change in `liab_iit` with the workaround on versus at zero participation.

| Tax year | SALT moved ($B) | of which S-corp | of which partnership | Participants (M) | Federal cost ($B) |
|---|---|---|---|---|---|
| 2017 | 36.1 | 19.3 | 16.8 | 3.18 | 3.2 |
| 2018 | 37.7 | 20.1 | 17.6 | 3.21 | 11.1 |
| 2019 | 39.5 | 21.1 | 18.4 | 3.25 | 11.6 |
| 2020 | 39.3 | 21.2 | 18.2 | 3.16 | 11.7 |
| 2021 | 48.2 | 26.4 | 21.7 | 3.08 | 14.7 |
| 2022 | 53.2 | 28.6 | 24.6 | 3.25 | 16.2 |
| 2023 | 49.7 | 26.5 | 23.2 | 3.26 | 15.1 |
| 2024 | 51.8 | 27.9 | 24.0 | 3.28 | 15.8 |
| 2025 | 53.0 | 28.6 | 24.5 | 3.31 | 15.1 |

Against JCT for TY2024:

| | Model | JCT | Model / JCT |
|---|---|---|---|
| SALT moved | $51.8B | $64.0B | 0.81 |
| S-corp | $27.9B | $30.9B | 0.90 |
| Partnership | $24.0B | $33.1B | 0.73 |
| Federal cost | $15.8B | $18.1B (13.1–19.7) | 0.87 |
| Cost share, top 1% | 84% | ~80% | |
| Cost share, top 0.1% | 50% | 42% | |

What is wrong:
1. **Timing.** About $38B a year is moved in 2018–2020, when 4–9 states had a PTET. In 2017 there was no cap and no PTET, yet $36B is moved at a $3.2B cost through the AMT branch.
2. **Geography.** About 11% of the 2024 amount ($5.5B) lands, under the state weights, in states with no PTET or no income tax. That includes FL ($1.7B, which is sales tax being moved) and PA ($1.1B).
3. **Level and mix.** The total is 19% low, and partnerships are 27% low.
4. **No behavior in projection.** Participation is a fixed draw. It does not respond to the 2025 cap ($40,000 with a phase-down for modified AGI of $500,000–$600,000), the 2030 return to $10,000, or any reform.

### State: nothing

The state calculator has no PTET add-back, credit or exclusion. `docs/model_documentation.md` lists PTET as unmodeled.

Worse, the federal adjustment runs before the state calculator, so state AGI already carries the reduced pass-through income. A participating owner in a credit state is therefore taxed on income net of the entity tax, with no credit. State liability falls by only (state rate × PTET), when it should fall by about the full PTET. For owners recorded in a non-PTET state, state income is understated instead.

## 3. New evidence

**Comey, Gallagher and Gorman (JCT, October 2025), "SALT Cap Workarounds."**
- **Method.** A stacked triple difference on IRS Statistics of Income business returns, 2014–2022. It compares "taxes and licenses" deductions of pass-throughs against C-corps, in PTET against non-PTET states, before and after enactment.
- **Effects.** PTET raised the deduction by 19.8% for S-corps and 40.5% for partnerships. A placebo on sole proprietors is null.
- **TY2024 entity deductions.** $69.8B in total: $30.9B S-corp and $39.0B partnership.
- **Benefit to individuals: $64.0B.** The partnership figure is $33.1B under an ad hoc assumption that 85% benefits individuals.
- **Federal cost: $18.1B.** The range is $13.1–19.7B across that assumption.
- **Distribution.** 80% of the cost falls on the top 1%, 94% on the top 5% and 42% on the top 0.1%.
- **Income in PTET states.** 71.7% of S-corp income and 61.5% of partnership income was in PTET states in 2024.
- **Law tables.** Table 1 and Table B.1 give every state's PTET rate, effective year and method.

**State PTET law, TY2018–2030.** The table is `research/state_tax/ptet/ptet_state_law.csv`, with notes in `ptet_state_law.md`. It covers 41 states with a broad income tax plus DC, and every row cites a source.
- **States with a PTET:** 4 (2018), 9 (2020), 19 (2021), 34 (2022), 36 (2024), 37 (2026), 35 (2030).
- **Changes since JCT:**
  - CA extended its PTET to 2030 (SB 132).
  - IL, VA and UT made theirs permanent.
  - MN was re-enacted for 2026–2027 only, and OR runs only to 2027.
  - Maine is new from 2026, with a 90% credit.
  - MA adds a 4% PTET on income subject to its millionaire surtax from 2026.
- **Credit shares below 100%:**
  - CT: 87.5% from 2019.
  - IA: 100% minus the individual rate.
  - MA and ME: 90%.
- **Income-exclusion states:** AR, GA, LA, NC, OK, SC and WI.

**Published state totals (validation).** These are mostly fiscal-year cash figures, not tax-year liability.

| State | Measure | Amount |
|---|---|---|
| NY | PTET credit, full-year residents (DTF, in State-Ind) | $12.44B in TY2024 ($15.73B all filers) |
| NY | PTET credit claimed (Office of the State Comptroller) | about $11.5B in TY2021 |
| NY | PTET collections (Office of the State Comptroller) | $16.4B in state FY2021-22 |
| NYC | City PTET credits | $1.6B in TY2022, $1.7B in TY2023 |
| CA | Department of Finance estimate of income-tax revenue lost to PTET credits | $18–19B a year, FY2021-22 to FY2023-24 |
| CT | PE tax collections | about $2.0–2.3B a year since FY2022 (the PE tax credit sits on the payment side in the DRS tables, in State-Ind) |
| MA | One-time revenue gain from the PTE excise | about $1.4B in FY2022 |

## 4. Proposal

### 4.1 One PTET amount per record, used by both calculators

Replace `do_salt_workaround_baseline()` with `calc_ptet()`. It would write two columns, `ptet_scorp` and `ptet_part`: the entity tax paid on the owner's behalf. Pass-through income is not netted in the data. Each calculator applies the amount under its own law (4.4, 4.5). This removes today's ordering problem, where the federal adjustment silently changes the state base.

### 4.2 The amount if a record elects

The entity tax is the state PTET rate times the owner's positive S-corp or partnership income, in states where a PTET is in effect that year:

- **State pass (state known).** `ptet_x = rate[s,t] × max(0, x_income) × available[s,t]`, using `ptet_state_law.csv`. The table becomes per-state YAML parameters (`ptet.available`, `ptet.method`, `ptet.rate`, `ptet.credit_share`, `ptet.addback`), which `build_state_tax_law()` already parses.
- **Federal pass (no state).** The same formula, using the record's expected rate across its states: Σ over states of `w[r,s] / w[r] × rate[s,t] × available[s,t]`, from the State-Weights split.
  - In runs without State-Weights, use a national series: the pass-through-income-weighted mean of `rate × available` by year and entity type. It is precomputed once from State-Weights and stored in the federal law YAML.
  - The paper's 71.7% / 61.5% PTET-state income shares for 2024 check this series.

This bases the amount on the income the PTET actually taxes, rather than on itemizers' `salt_inc_sales`. Three things follow:
- Sales tax can no longer be moved.
- Non-itemizers, who also gain from the deduction, are included.
- The amount scales with each state's rate and enactment year.

### 4.3 Who elects: a rule based on the federal gain, calibrated to TY2024

A record elects when its federal gain from electing exceeds a threshold, scaled by a take-up parameter calibrated by entity type.

The gain is approximately the federal marginal rate on pass-through income times the part of the PTET that would otherwise be lost to the SALT cap. That is the PTET minus the room left under the cap, plus the standard-deduction case, where all of it is gained. An exact with-and-without `do_taxes()` pass for marginal records is an option if the approximation proves too coarse.

Calibrate so TY2024 gives $30.9B of S-corp and $33.1B of partnership amounts, then check the federal cost against $18.1B and the top-1% share against about 80%.

Two optional refinements:
- **Ramp.** Take-up in a state ramps over its first two to three years, since use grew between 2021 and 2024.
- **Random draw.** The per-record draw keeps the existing `r.salt_workaround` stream, so draws stay tied to record id (`draw_by_id()`, `src/misc/rng.R`).

Because the rule depends on each owner's gain, the 2025 $40,000 cap, its phase-down and the 2030 return to $10,000 change participation without any re-tuning. So do reform scenarios that touch the cap or the PTET itself. The JCT paper notes that the 2025 cap leaves the top-1% incentive essentially intact, so participation should fall mostly in the middle.

### 4.4 Federal treatment

- The elected amount reduces partnership or S-corp income, as an active loss as today.
- It reduces `salt_inc_sales` by `min(salt_inc_sales, ptet)`, and only for its income-tax component.
- It enters expanded income as now.

Existing reform switches (`item.salt_workaround_allowed_*`) keep working as caps on take-up.

### 4.5 State treatment

State AGI starts from federal AGI, which is net of the PTET the federal pass deducted (an expected value across the record's states). So the state pass first **adds that federal deduction back in every state**, restoring gross pass-through income. It then applies its own state-specific amount from 4.2, by state and year:

- **Credit states.**
  - Allow a credit of `credit_share × ptet`.
  - Whether the credit is refundable is set per state (MA and ME refundable; others limited to tax).
  - Nearly every credit state also requires owners to add back the entity's deduction. The first step already does this.
- **Exclusion states (AR, GA, LA, NC, OK, SC, WI).** No credit. The owner excludes the pass-through income on which the entity paid the tax.
- **States without a PTET, or years before enactment.** Nothing further. The state taxes gross pass-through income.

A PA-weighted record therefore carries no PTET in PA, even if its federal expected value is positive.

**Out of scope for this step:** PTET paid to another state, and the resident's credit for it. That belongs with the general credit for taxes paid to other states (source-state work, planned Phase 7). The law table records which states grant that credit.

### 4.6 Projection to 2025–2030

- The law table runs through 2030 under current law.
- The election rule handles the cap path.
- Pass-through income grows with Tax-Data's projection.
- MN and OR drop out after 2027 unless extended.

Additional anchors as they appear:
- NY DTF TY2025 data (around fall 2027) and CT DRS data.
- JCT's score of the House anti-workaround provision (H.R. 1 as passed by the House, Sec. 112018), if it can be separated out, as a federal-cost path over the budget window.

## 5. What to expect (rough, to be confirmed by the implementation)

- **Federal cost path.** It should fall to near zero in 2017, to about $1–3B in 2018–2020 (CT, WI, retroactive CO and NE, then LA, OK, RI, MD, NJ), and rise through 2021–2022 to about $18B in 2024. The current model shows a flat $11–16B.
- **State liability.** Netting the published PTET credits from modeled liability:
  - NY TY2024: (1.394 × $47.75B − $12.44B) / $47.75B ≈ 1.13.
  - NY TY2021: (1.358 × $49.47B − $11.5B) / $49.47B ≈ 1.13.

  Both are in line with NY's ratio for tax before credits (1.06–1.11), against 1.36–1.49 today. CA TY2022 would fall from 1.31 to roughly 1.12 on the Department of Finance's ~$18B figure. That figure is fiscal-year, so this is indicative only.

## 6. Implementation outline

| Piece | Where | Notes |
|---|---|---|
| PTET law parameters | `config/scenarios/tax_law_state/baseline/{st}/ptet.yaml` | Generated from `ptet_state_law.csv`, with citations; `params_schema.yaml` gains `ptet.*` |
| National expected-rate series | `config/scenarios/tax_law/baseline/item.yaml` (or a new `ptet.yaml`) | Precomputed from State-Weights by year and entity type |
| `calc_ptet()` | `src/calc/do_taxes.R`, replacing `do_salt_workaround_baseline()` | Writes `ptet_scorp` and `ptet_part`; election rule and calibration parameters |
| Federal application | `do_taxes()` data adjustments | Income reduction and SALT reduction as in 4.4 |
| State add-back and credit or exclusion | `src/calc/state/st_agi.R`, `st_credits.R` | As in 4.5; new outputs `st_ptet_credit` and `st_ptet_addback` |
| Tests | `src/tests/state/test_state_calc.R` worksheet cases (NY credit, CT 87.5%, WI exclusion, PA none); `test_supplemental_inputs_invariance` unchanged | Plus a federal regression that zero participation reproduces the run with the workaround off |
| Validation | `research/state_tax/ptet/` | Rerun `01_quantify_workaround.R` against JCT; NY, CT and NYC credits from State-Ind; agency ratio tables before and after |

Tax-Data needs no change for the base version. The S-corp and partnership income split and the `sstb_*` flags already exist. A later improvement would be the state where each owner's entity operates, which the PUF does not carry.

## 7. Decisions for you

1. **Federal baseline change.** *Decided 2026-10-28 (JI): calibrate to JCT, replacing the TPC-based calibration, for consistency.* The new mechanism moves federal baseline revenue: lower 2017–2020, higher partnership amounts from 2021.
2. **Who benefits from partnership PTET.** *Decided 2026-10-28 (JI): follow JCT's 85% individual share and calibrate to its central figure.* The assumption is ad hoc, and its range ($13.1–19.7B of federal cost) is the main source of uncertainty.
3. **Base for the amount.** Use rate × pass-through income (proposed) or keep the itemized-SALT base. The proposal drops the itemized-SALT base.
4. **How the federal pass handles state.** Use the expected value from State-Weights when available and the national series otherwise (proposed), or the national series always, for reproducibility across runs with and without state weights.
5. **Unconfirmed law items.** These sit in `ptet_state_law.md`: whether NY and NYC cut their credits in 2026, the MS rate, whether NM gives a credit or an exclusion, and about 14 states' year-by-year rates (to follow our own state rate parameters).

## Sources

- Comey, M., B. Gallagher and W. Gorman (2025), "SALT Cap Workarounds," Joint Committee on Taxation working paper, October 2025.
- `research/state_tax/ptet/ptet_state_law.md`: statute, revenue department and secondary sources for each state.
- `research/state_tax/ptet/results/`: the current-model quantification (national, distribution, by state).
- `research/state_tax/aggregate/results/agency_ratio_sw_states_nfgate.csv`: model against agency tables.
- State-Ind repo (`Budget-Lab-Yale/State-Ind`): NY DTF, CA FTB, CT DRS and Census benchmark tables.
