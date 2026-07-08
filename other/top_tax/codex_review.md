# Codex review: top-tax interaction prospectus and sigma plan

Date: 2026-07-08

Reviewed:

- `/home/jar335/.claude/plans/hey-read-other-top-tax-interaction-prosp-shimmying-coral.md`
- `other/top_tax/frontier_exercise_notes_2026-07-07.md`
- `other/top_tax/interaction_prospectus.html` because the markdown says the prospectus is now authoritative
- Relevant implementation paths in `src/sim`, `src/slurm`, `src/calc`, and existing behavior modules

Bottom line: the project idea is strong. The interaction matrix is a real contribution, and the sigma idea is the right missing margin to make top ordinary rates interact with capital-side reforms. But the current design is not yet implementable or publishable as written. The biggest problems are not syntax-level. They are source-of-truth drift, calibration double-counting, an under-specified `tau_eq` object, a pre-pass data contract that does not currently contain the variables sigma needs, and an overbroad economic pool that can turn the model into a mechanical top-wage-to-gain converter.

## Highest-priority findings

### P0. There is no single authoritative design anymore

The markdown banner says `interaction_prospectus.html` is authoritative, but the plan supersedes core prospectus rulings:

- Prospectus: crude closed-form wedge, residual calibration, phase-in, gain-state/corporate split parameter.
- Plan: mandatory full Bellman extraction, asserted low/central/high sigma values, no phase-in, no split parameter.

This is not harmless documentation drift. These choices change the model's identifying assumptions, implementation shape, run campaign, and validation target. A developer following the prospectus would build a materially different model than a developer following the plan.

Fix before coding: update `interaction_prospectus.html` or create a short `other/top_tax/DESIGN_LOCK.md` that explicitly states the live rulings. Mark the prospectus sections that are dead. The reviewable contract should say: wedge source, timing, calibration, pool, module order, validation runs, exact explorer outputs, and non-goals.

### P0. The exact-explorer promise contradicts the proposed run grid

The prospectus says the explorer reports revenue at low, central, and high elasticity bundles for the selected package, and that every displayed number is an exact run. The run plan only runs the full 5,184-package grid at the central bundle, with low/high runs only for a hero-lever subset.

That means either:

- non-hero packages cannot show low/high bands;
- low/high bands for non-hero packages are interpolated or inherited, contradicting the "exact run" claim;
- the actual run count is closer to central + low + high full grids, not about 5,500 runs.

Fix: decide which promise survives. The clean version is: full exact central explorer, exact low/high only for the hero-lever subset, and no low/high readout for other selected packages. If the band is shown for every dropdown package, budget and orchestrate the full low/high grids.

### P0. The new plan drops residual calibration but still relies on overlapping evidence

The prospectus correctly distinguishes mechanical double-counting from calibration double-counting. Sequential application prevents the same record dollar from being moved twice in code, but it does not make literature elasticities disjoint. The plan then says sigma is "asserted, not calibrated" and uses:

- Pearce-Prisinzano's entity-shifting semi-elasticity as the central sigma anchor;
- Mortenson's cross-rate estimates as the high sigma anchor;
- evasion, realization, entity shifting, and charity in the same stack.

That reintroduces the exact calibration double-counting problem the prospectus identified. Pearce-Prisinzano and Mortenson are reduced-form estimates from settings where multiple margins were available. If their numbers anchor sigma directly while P-P/entity, evasion, and realization are also on, the total top-end response can be too large even when every module is mechanically disjoint.

Fix: either restore residual calibration or explicitly downgrade 0.2/0.6/0.9 to a sensitivity experiment, not a calibrated model. The validation run should be a fail gate: if the full stack produces own/cross elasticities outside pre-registered bands, the central sigma cannot be used in the campaign.

### P0. The planned sigma pre-pass cannot compute its own gate/pool from current inputs

The plan asks `sigma_compute_conversions()` to compute record-level gates and pools during the kg bathtub pre-pass from baseline/static MTR detail plus `tau_eq`. Current inputs do not contain the needed variables:

- `kg_dyn_load_cells_inputs()` slims raw Tax-Data to `id`, `weight`, `kg_lt`, `age_cohort`, and `G_unit` for MTR aggregation (`src/sim/kg_dynamics.R:1647-1651`).
- `kg_dyn_load_bathtub_inputs()` reads only `mtr_kg_lt` and `mtr_kg_lt_lawonly` from static detail (`src/sim/kg_dynamics.R:1699-1756`).
- `detail_vars` include `wages1`, `wages2`, `wages`, `sole_prop`, `part_scorp`, `agi`, and `txbl_inc`, but not `part_active`, `scorp_active`, `sole_prop1`, `part_se1`, `part_se2`, or other components the plan needs for the labor-content pool and SECA-safe applier (`src/misc/config_parser.R:296-313`).

So the plan's "small threading" framing is too optimistic. Sigma needs a new pre-pass data contract: either read wider static detail, add required detail columns, read raw Tax-Data columns directly, or persist a sigma input frame in Phase 2A. Each choice changes I/O, memory, privacy footprint, and purge behavior.

Fix: define an explicit `sigma_inputs/{year}.rds` contract before building. Required columns should include id, year, weight, filing status, age convention, wages1/2, active PT legs, SECA companions, static taxable income or ordinary-income concept, baseline/static MTRs for all required legs, and resolved top-bracket thresholds.

### P0. `tau_eq` is under-derived and may not match the model's event order

The plan's recursion is only a sketch:

```text
T(a,t) = r_tot(a,t) * tau(a,t)
       + (1 - r_tot) * beta(t) * ((1 - m(a)) * T(a+1,t+1) + m(a) * T_death(a,t))
```

That is not obviously the same object as "expected PV tax per dollar entering the gain state" in the implemented kg model.

Implementation facts:

- The Bellman pass separates fixed, planned, and decision-responsive realization buckets (`src/sim/kg_dynamics.R:804-870`).
- Planned timing is built after Pass 2 (`src/sim/kg_dynamics.R:2036-2045`) and then merged into the final scenario rate in `kg_dyn_build_scenario_rate()` (`src/sim/kg_dynamics.R:1087-1109`, `2064-2074`).
- The recurrence applies survivor realization/aging and death routing in a particular order (`src/sim/kg_dynamics.R:1118-1184`).
- The persisted `cell_table` currently stores final `r_S`, `extra_R`, and death factors after the scenario-rate combination (`src/sim/kg_dynamics.R:1768-1890`).

If `tau_eq` uses `r_D` from the Bellman but the actual state uses `r_S` after fixed and planned buckets, the wedge will omit part of the policy-relevant realization rate. If the recursion's mortality timing differs from `kg_dyn_step_recurrence()`, the wedge will be internally inconsistent with the gain-state stock it drives.

Fix: define `tau_eq` as a finite-difference object first: add one dollar of unrealized gain to an age/year cell, hold policy functions fixed, run the exact recurrence/tax event order, and compute PV tax. Then implement the closed linear recursion and test it against finite differences cell-by-cell. Do not rely on an informal formula.

### P0. Conversion inflow timing is economically material and currently hand-waved

The plan injects converted compensation as `conv_inflow` into `delta_next` at the end of year t, "like inheritance inflow." But converted compensation is earned by a living owner-manager during year t. If the owner dies in year t, the new gain may be subject to deemed/carryover/step-up in that same year. If the owner survives, it should probably age into t+1 with the owner, not simply land in the current-age vector.

This matters because sigma's central mechanism is exactly the death-regime interaction. End-of-year injection delays or bypasses the first-year death-regime treatment for the new gain stock. For high-wealth old cells, this is not a rounding issue.

Fix: specify the timing convention. At minimum, the recurrence should split conversion inflow into same-year death and survivor-next-age branches, or the report should state that converted compensation enters at year-end and intentionally avoids same-year death risk. Then test the direction and magnitude under deemed-at-death.

### P1. The proposed sigma pool is too broad

Gate = active business income presence plus taxable income above the top-bracket threshold. Pool = all wages plus 75 percent of active pass-through income. This will overstate the shiftable base in several common records:

- A spouse has W-2 wages while the other spouse has a small active K-1.
- A taxpayer has high capital gains, crosses the taxable-income threshold, and has ordinary wages that are not controllable owner-manager pay.
- A taxpayer has active PT income but no realistic control over wage form.
- QBI, losses, deductions, or one-time gains move the taxpayer across the gate even though control over compensation form did not change.

The SYZZ result supports treating a large share of top pass-through profit as owner human capital; it does not justify treating all W-2 wages on any top-tax unit with active business income as convertible into capital gains.

Fix: make the wage component narrower. Options: cap wage conversion by active business labor-content income, require a stronger control proxy, split wages by earner where possible, use baseline top-status rather than reform taxable-income gate, and report the pool as a diagnostic by source. At minimum, show how much of the pool is W-2 wages versus active PT labor content.

### P1. The wedge omits destination taxes that matter for the economic story

The plan defines the equity leg as `tau_eq(age,t)` from capital-gains dynamics. That may be fine for a narrow "personal gain-state" destination. But the narrative invokes founder equity, retained business earnings, carried interest, QSBS, and owner-manager salary retention. Those routes can face very different effective tax prices:

- C-corporation retention includes entity-level corporate tax before shareholder-level gain/dividend tax.
- QSBS can make the equity path close to zero for eligible new shares.
- Partnership profits interests and carried interest are not the same as adding a dollar to generic household unrealized gains.
- S-corp compensation avoidance can change payroll taxes even if ordinary income tax treatment is not avoided.

If the wedge is ordinary wage MTR minus individual capital-gains `tau_eq`, it can overstate the incentive for routes that actually bear corporate-level tax and understate routes like QSBS.

Fix: either narrow the mechanism to "generic income converted into household gain state" and stop leaning on C-corp/founder/QSBS examples, or introduce destination-specific wedges and shares. The removed split parameter may be economically necessary even if it is hard to identify.

### P1. Existing entity shifting is not production-grade for this stacked campaign

The plan treats `entity_shifting/pearce_prisinzano.R` as a reliable disjoint layer. The current module is much rougher:

- It computes a single `amount_shifted` and pushes it into `part_active` (`config/scenarios/behavior/entity_shifting/pearce_prisinzano.R:72-80`).
- It adjusts `kg_lt` as the distribution offset (`:82-91`).
- It does not co-scale SECA companions, unlike the evasion module's care around `part_se1/2` and `sole_prop1/2`.
- It has no order guard, no per-record diagnostics, and no conservation report.
- Its comments admit dividend/gain treatment is a computational convenience and not robust to preferred-rate differentials (`:53-58`).

This is a problem because sigma's credibility depends on the other modeled legal-avoidance pieces being cleanly separated. If P-P is approximate and undocumented at the flow level, sigma validation can be contaminated.

Fix: harden P-P before sigma validation. Add SECA companion treatment, diagnostics, explicit sign conventions, and conservation totals. If the top-tax stack uses it as a calibration component, it should meet the same standard as the new sigma module.

### P1. The no-fixed-point claim is only partly true

The plan says there is no fixed point because the Bellman policy is per-dollar and stock-independent. That handles the gain-state side. It does not handle conventional-side nonlinearities:

- Conversion reduces wages/PT before `do_taxes()` in the conventional pass (`src/sim/run.R:783-810`).
- MTRs used by behavior modules are static-pass MTRs (`src/sim/run.R:617-644`, `868-892`).
- Large conversions can move records across ordinary brackets, NIIT thresholds, QBI limits, payroll tax regions, and deduction phaseouts.

Other behavior modules also use static MTRs, so this may be an acceptable first-order convention. But the plan should not sell it as exact.

Fix: call it a first-order MTR-frame approximation and run stress tests at the 50 percent top-rate and gains-at-ordinary corners. Report whether one iteration using post-conversion MTRs changes sigma flows materially.

### P1. Negative conversion needs stronger accounting rules

The plan allows wedge narrowing to generate negative conversion, clamped so no leg goes negative. That is directionally right for deviations from baseline, but it creates tricky state accounting:

- Negative gain-state inflow can push cell `dG` farther negative and hit existing clamps in `extra_R`/`deemed_factor` (`src/sim/kg_dynamics.R:1868-1878`).
- A negative flow represents "less baseline conversion into gains," but baseline conversion is not separately tracked as a sigma stock.
- If the module increases wages/PT when wedge narrows, payroll companions and derived variables must be exactly reconciled.

Fix: add annual and cumulative conservation assertions:

- weighted record dollars removed from ordinary/PT equal weighted cell gain inflow;
- negative inflow never exceeds a defined available gain-state deviation budget;
- `SIGMA_CONV=0` is byte-identical to sigma-off except additive diagnostics;
- wedge-narrowing packages increase ordinary income and reduce gain-state additions in the expected cells.

### P1. The plan underestimates SLURM and storage changes

The plan says no new phase and no manifest count changes, which is probably true. But sigma adds a new per-record supplemental output for every scenario-year and likely requires reading wider inputs during Phase 2B. Current Phase 2B resource requests are modest (`slurm_run.sh:164-169`). Full-sample kg already requires `pct_sample = 1` (`src/sim/run.R:1052-1057`).

For 5,000+ scenarios over 30-year windows, per-record sigma CSVs can become a serious storage and I/O problem. The planned `delete_detail = 1` campaign does not automatically solve this because supplemental sigma files are not detail files.

Fix: use compressed RDS or fst/parquet-style columnar storage if available; persist per-record files only for validation/debug scenarios; for the campaign, persist age-cell aggregates plus sampled diagnostics unless exact per-record audit is required. Add purge rules explicitly.

### P1. The campaign is not credible until estate, wealth, and corporate inputs are closed

The notes identify three non-sigma gaps that would dominate referee reports:

- estate tax has no behavioral response;
- wealth-tax avoidance uses deliberately extreme placeholders;
- corporate points depend on OME-scored vintages and placeholder `CORP_*` constants.

The sigma plan does not solve these. Running the full interaction matrix before these are resolved would create precise-looking numbers around known placeholder behavior.

Fix: make the run campaign conditional on:

- estate reported-base elasticity implemented or explicitly excluded with a sensitivity haircut;
- wealth avoidance re-anchored and swept;
- corporate OME points and placeholder constants replaced;
- a table listing every behavioral parameter in the low/central/high bundle.

### P1. Validation can pass while the economics is still wrong

The proposed validation matrix compares four elasticities to broad literature brackets. That is useful but too weak. Broad brackets can be passed by offsetting errors: an overbroad pool, an understated wedge, and a contaminated P-P layer can produce plausible totals for the wrong reasons.

Fix: add decomposition fail gates:

- pool size by source and percentile;
- mean and distribution of `Delta W`;
- sigma dollars removed from wages versus PT;
- same-year, 10-year, and 30-year tax recovery of converted dollars;
- share of sigma response taxed as gains, deemed at death, carryover to heirs, or never taxed under step-up;
- comparison of validation elasticities with and without entity shifting, evasion, charity, and death-regime changes.

### P2. Literature claims need a citation audit before publication

I verified two public anchors:

- Saez-Slemrod-Giertz (2012) is a JEL critical review of ETI and explicitly frames when ETI can be used as a sufficient statistic.
- Smith-Yagan-Zidar-Zwick's NBER page states that pass-through profit falls by about three-quarters after owner retirement/death and that classifying three-quarters of pass-through profit as human-capital income makes the typical top earner mostly human-capital rich.

But I did not find or verify the exact Mortenson table values, the Pearce-Prisinzano table conversion, or the DHY 2025 slide values from primary public sources in this pass. The internal files may have them, but the report should not publish exact claims like "+0.79 / -0.77", "-0.24 to -2.4", "0.631", or DHY component elasticities without a citation appendix that pins paper version, table, column, sample, income concept, and rate definition.

Fix: create `other/top_tax/literature_crosswalk.md` with one row per numeric anchor: source, version/date, table/column, estimate, standard error, tax base, rate definition, population, interpretation in this model, and double-count risk.

### P2. Presentation metrics need tighter definitions

"Leakage share = 1 - conventional/static" can be misleading when interactions raise revenue or when static is near zero. "Top-1% wealth erosion" is a financing/distribution state, not an efficiency cost. Both are useful, but they need definitions and edge-case handling.

Fix: for every explorer metric, define numerator, denominator, sign, horizon, and behavior under negative or near-zero static revenue. Label wealth erosion as balance-sheet financing or wealth decumulation, not deadweight loss.

## What to preserve

The core contribution is worth keeping:

- Use package totals against a common baseline, not stacked attribution.
- Make the interaction matrix the headline instead of an unconstrained revenue-max point.
- Keep 10-year and 30-year outputs separate.
- Use the kg Bellman/death-regime machinery to make realization behavior policy-dependent.
- Keep evasion as a leak and sigma/entity shifting as conservation margins.
- Require hard guards for module order and required MTRs.
- Treat the literature ETI as a validation target for a decomposed model, not as a generic plug-in elasticity.

## Recommended revised build order

1. Lock the live design in one document and update or demote the prospectus.
2. Write `literature_crosswalk.md` for every numeric anchor used by sigma, evasion, entity shifting, and validation.
3. Harden the existing entity-shifting module enough to be safely stacked.
4. Define `tau_eq` by finite difference against the actual kg recurrence, then implement the linear recursion and test equality.
5. Define the sigma input/output contracts before coding the module: required columns, storage format, purge policy, diagnostics, and SLURM resource expectations.
6. Build sigma with `SIGMA_CONV=0`, sign, conservation, order, and missing-MTR tests.
7. Run validation as a fail gate, not a ceremonial exhibit.
8. Only then run any full interaction-grid campaign, and only after estate, wealth-avoidance, and corporate placeholder inputs are resolved or explicitly bracketed.

## External sources checked

- Saez, Slemrod, and Giertz (2012), "The Elasticity of Taxable Income with Respect to Marginal Tax Rates: A Critical Review," AEA/JEL: https://www.aeaweb.org/articles?id=10.1257/jel.50.1.3
- Smith, Yagan, Zidar, and Zwick (2019), "Capitalists in the Twenty-First Century," NBER Working Paper 25442: https://www.nber.org/papers/w25442
