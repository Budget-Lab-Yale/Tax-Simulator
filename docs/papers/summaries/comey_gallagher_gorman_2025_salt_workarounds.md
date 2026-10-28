---
title: "Comey, Gallagher & Gorman (2025), SALT Cap Workarounds — reading and model implications"
role: note
workstream: literature
status: current
updated: 2026-10-28
sot: self
supersedes: []
superseded_by: null
paper_id: comey_gallagher_gorman_2025_salt_workarounds
---

# Comey, Gallagher & Gorman (2025), *SALT Cap Workarounds*

JCT staff working paper, October 2025. PDF:
`docs/papers/pdfs/comey_gallagher_gorman_2025_salt_workarounds.pdf`. Extracted numbers:
rows of `docs/papers/parameters.csv` with this `paper_id`; per-state law table:
`docs/papers/tables/comey_gallagher_gorman_2025_salt_workarounds_ptet_laws.csv`.

## Question

How much state income tax do passthrough owners now deduct federally through state
passthrough entity taxes (PTETs), what does that cost the Treasury, and who gets it?

## Data and method

- SOI Corporate and Partnership samples, 2014–2022, about 240,000 business returns a year,
  aggregated to state × year × entity-type cells (p.8–9).
- PTET payments are not reported separately. They sit inside the "Taxes & Licenses"
  deduction line (1120-S line 12, 1065 line 14).
- Stacked triple difference (Cengiz et al. 2019 style). It compares passthroughs in PTET and
  non-PTET states, with C corps in the same state as the third difference. The regression is
  weighted by 2017 ordinary business income, with SEs clustered by state (Eq. 1, p.10–11).
- 13 treated credit-regime states in the main specification, 17 with exclusion states. Each
  stack has 4 pre-years and 2 post-years. AK and HI are dropped.
- The aggregates are converted to 2024 using CBO passthrough income growth. The fiscal cost
  uses parameters from the JCT Individual Tax Model (ITM), which is built on the 2019 SOI
  individual sample.

## Headline findings

| Finding | Value | Where |
|---|---|---|
| Effect on log Taxes & Licenses, S corps | 0.181 (0.039), +19.8% | Table B.2 A, Fig. 2 |
| Effect on log Taxes & Licenses, partnerships | 0.340 (0.064), +40.5% | Table B.2 A, Fig. 2 |
| Sole-proprietor placebo | −0.021 (0.069) | Table B.2 A |
| PTET deductions, 2024 | $30.9B S corps + $39.0B partnerships = $69.8B | Table 3 B |
| Of which benefits individuals | $64.0B | Table 3 C |
| Federal revenue cost, 2024 (static) | $18.1B (range $13.1B–$19.7B) | p.17, Table 4 |
| Share of the cost to the top 1% / 5% / 0.1% | 80% / 94% / 42% | Table 4 |
| The PTET deductions as a share of SALT disallowed by the cap ($217.5B) | 29% | p.17 |

The cost calculation (p.16–17) returns the $64.0B to individuals as passthrough income.
- QBI absorbs 8.7% of it.
- Only 1.3% of the extra SALT is deductible under the cap.
- Taxable income rises $57.4B, taxed at an average marginal rate of 31.5% on passthrough
  income, which gives $18.1B.

The partnership amount assumes 85% of partnership PTETs benefit individuals: the 39.8%
individual ownership share plus 75% of the rest. The paper calls this ad hoc. Its two bounds
produce the cost range.

OBBBA's $40k cap barely changes the result, because the phasedown between $500k and $600k
of MAGI returns the people who use PTETs to the $10k cap (p.18).

PTETs also help owners who take the standard deduction (fn 14). 36 states had a PTET by
2025, and New York City has its own (p.6). MA credits only 90% of the owner's PTET share,
and WI sets its PTET rate above its top rate (fn 13, 15).

## Implications for the model

How the model works today. `do_salt_workaround_baseline()`, at `src/calc/do_taxes.R:690`
and called from `src/sim/run.R:380`:
- splits the unit's reported `salt_inc_sales` by the partnership and S corp shares of gross
  income;
- compares a stable per-unit draw (`r.salt_workaround`, `src/misc/rng.R:133`) with a
  take-up probability of 0.75 (`config/scenarios/tax_law/baseline/item.yaml`,
  `salt_workaround_allowed_*`);
- moves the selected SALT into `part_active_loss` and `scorp_active_loss`.

The code comment says the take-up probability was "calibrated to hit $20B annual estimate
from TPC".

1. **Calibration target.** The comment does not say whether $20B is deductions or revenue.
   This paper gives a 2024 target for both: $64.0B of deductions moved off individual SALT
   (`ptet_ded_indiv_2024`) and $18.1B of revenue (`fiscal_cost_2024`). If the $20B was
   meant as deductions, the model moves about a third of what JCT finds. The benchmark below
   settles this.
2. **What gets moved.**
   - The model moves a share of the owner's reported Schedule A income or sales tax
     (2015-PUF E18400). That variable only exists for itemizers.
   - The paper's PTET is roughly PTET rate × passthrough income, paid in a PTET state.
   - As a result, owners who take the standard deduction never use the workaround in the
     model. Sales-tax electors and residents of states with no PTET (TX, FL, WA, PA, ...)
     can.
   - The model's take-up ignores state and the year each state's PTET took effect. The
     per-state table gives both.
3. **Workaround runs before 2018.** `incentive_for_workaround` is
   `!is.infinite(item.salt_limit) | !is.infinite(amt.exempt)`. `amt.exempt` is finite in
   every year, so the flag is always TRUE and the workaround runs before PTETs existed. This
   matters only for runscripts that simulate 2017 or earlier, for example
   `config/runscripts/tests/salt_workaround_test.csv`.
4. **Knobs that do nothing.**
   - `salt_workaround_allowed_part` is only checked for `== 0`.
   - S corps have no switch of their own.
   - `salt_workaround_allowed` and `salt_workaround_share` are set in several scenarios (for
     example `config/scenarios/tax_law/tests/hce/item.yaml` and
     `config/scenarios/tax_law/public/obbb/house/archive/`) but no code in `src/` reads them.
5. **Take-up ignores the cap.** It is a fixed probability. In a reform that repeals or raises
   the SALT cap, the model keeps moving SALT into the workaround even where it no longer
   saves owners anything. That biases the score of any SALT-cap reform.
6. **State tax is understated in PTET states.** This is the largest gap for the state module.
   - State AGI starts from federal `part_scorp` (`src/calc/state/st_agi.R:439`), which is net
     of the workaround, and no state module adds it back.
   - Under a credit regime (Table 2, p.27), the owner is taxed on pre-PTET income and takes a
     credit equal to the PTET. The PTET itself is state revenue.
   - So the model understates state liability by roughly the state rate times the workaround
     amount, and records no PTET revenue.
   - This is consistent with the PTET wedge in the New York pilot
     (`docs/model_documentation.md`, "PTET (pass-through entity tax) credits are
     unmodeled").
   - The decision to use post-workaround `salt_inc_sales` in state add-backs
     (`research/state_tax/plan.md`) should be reread with this in mind.
   - The per-state table is a starting list for that work. State YAML `reference:` fields
     must still cite statute, not this paper; its source is Bloomberg Tax.
7. **Self-employment tax.** PTET reduces partnership ordinary income, but the workaround
   leaves `part_se1` alone. This is a small effect on SECA.
8. **Comparable distribution.** `expanded_inc` adds the workaround back
   (`src/calc/do_taxes.R:154-159`), so the model's cost by expanded-income percentile can be
   set against Table 4. The paper ranks by JCT economic income divided by the square root of
   unit size; we do not adjust for unit size.

## Model comparison

Benchmark: `config/runscripts/tests/ptet_benchmark.csv`.
- It pairs `baseline` with `no_workaround`
  (`config/scenarios/tax_law/tests/ptet_benchmark/no_workaround/item.yaml`, both take-up
  probabilities set to 0).
- It is federal-only, covers 2023–2024, and pins Tax-Data `2026092720_filer_refit_v2`.
- `docs/papers/benchmarks/comey_gallagher_gorman_2025_salt_workarounds.R` computes the 2024
  comparison from the two detail files.

The detail files carry no workaround columns, so the script measures the workaround as the
counterfactual-minus-baseline change in `part_scorp`, matched on `id`.

**Status (2026-10-28): not yet run.** The model's dependency data lives on the cluster, so the
benchmark has to run there. Record the vintage, sample fraction and commit next to the
results.

| Quantity | Paper | Model |
|---|---|---|
| Workaround deductions moved off individual SALT ($B) | 64.0 | — |
| Federal revenue cost ($B) | 18.1 | — |
| Average MTR on the returned income | 0.315 | — |
| Cost share, top 1% / top 5% / top 0.1% | 0.80 / 0.94 / 0.42 | — |

## Follow-ups (each needs its own go-ahead)

1. Recalibrate take-up to $64.0B / $18.1B if the benchmark misses.
2. Gate eligibility by state and effective year, using the per-state table. Change what gets
   moved from the E18400 share to PTET rate × passthrough income.
3. Turn the workaround off before 2018.
4. Make take-up depend on whether the cap binds.
5. Remove the dead knobs (`salt_workaround_allowed`, `salt_workaround_share`), or wire them
   up.
6. State PTET: add back the workaround in state AGI, give owners the credit or exclusion,
   and count PTET revenue. Log it in `research/state_tax/tax_data_gaps.csv`.

## Caveats and inconsistencies in the paper

- Figure 2 prints the partnership SE as 0.047; Table B.2 prints 0.064.
- p.17 gives the taxable-income increase as $58.4B and, a few lines later, $57.4B. The
  difference is the $0.9B of extra itemized SALT, and $57.4B is the figure that reproduces
  $18.1B.
- Several cross-references print as "Table ??". The labels of Tables B.2 and B.3 do not
  match the text (B.2 has five panels, the text cites four; B.3 labels two panels "B").
- Retroactive effective years (CO and NE back to 2018, plus IA, IN, KY, VA and WV) are
  treated in the regression as starting in the later of the effective and enactment years
  (fn 23).
- The cost is static: it includes no response by taxpayers or states other than the choice
  to itemize (p.18–19).

Checks that pass: exp(0.181) − 1 = 0.198; 186.8 × (1 − 1/1.198) = 30.9;
135.2 × (1 − 1/1.405) = 39.0; 0.398 + 0.75 × 0.602 = 0.85; 39.0 × 0.85 = 33.1;
57.4 × 0.315 = 18.1. All 51 rows of the per-state table were checked against Tables 1 and B.1
by script.

## Revision history

- 2026-10-28: First reading, with parameters, per-state table and benchmark scaffold. The
  benchmark has not run yet.
