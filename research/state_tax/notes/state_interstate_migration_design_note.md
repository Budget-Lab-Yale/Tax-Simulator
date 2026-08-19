---
title: "Interstate Migration Module - Design Note"
role: notes
workstream: state_tax
status: deferred
updated: 2026-08-19
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Interstate Migration Module - Design Note

**Date:** 2026-07-13
**Status:** design note only; no implementation yet
**Purpose:** capture how an interstate migration response could fit into Tax-Simulator's
existing behavioral and state-tax architecture before we write code.

---

## 1. Current behavioral model in Tax-Simulator

The current behavioral pipeline is built around changes to tax-unit attributes after a
static policy simulation:

- `src/sim/run.R` runs a static tax calculation first, optionally computes MTRs, and
  then calls `do_behavioral_feedback()` for the conventional pass.
- `src/sim/behavior.R` loads a behavioral module from
  `config/scenarios/behavior/...` and applies it to the national tax-unit file.
- Most existing modules are MTR-based. The helper `apply_mtr_elasticity()` supports
  `"semi"`, `"arc"`, `"netoftax"`, and `"taxprice"` mappings from baseline vs static
  MTRs into percentage changes in a target variable.
- The main labor-supply example, `config/scenarios/behavior/employment/bastian.R`,
  uses changes in the net-of-tax rate, `(1 - mtr_wages)`, to model extensive-margin
  employment responses.

Two implications matter for migration:

1. The default behavioral interface is centered on **national tax-unit records** and
   changes to income or filing behavior.
2. The codebase already tolerates behavior modules that are **not purely MTR-based**.
   For example, `config/scenarios/behavior/child_earnings/34.R` works from computed
   after-tax income levels rather than only from `apply_mtr_elasticity()`.

So a migration module would be new, but not conceptually out of bounds.

---

## 2. Why migration is different from current labor-supply modules

Interstate migration is not naturally a continuous response in one income margin. It
is closer to a **discrete location choice** over states. That pushes against the
current design in two ways:

- The relevant object is not just a tax-unit row. It is an `id x state` choice set.
- The outcome is not necessarily "higher wages" or "less reported income." It is a
  change in the probability weight assigned to each state.

That distinction matters because the state-tax module currently follows the
TPC/OTA-style split-weight architecture:

- Tax units do **not** carry a literal observed state identifier.
- State analysis is produced by evaluating the same tax units under each state's law
  and aggregating with state weights that sum back to the national weight.

Because of that architecture, the natural v1 migration response is:

- **Update the state weight vector for each tax unit**, not
- assign each tax unit a single new state of residence.

This keeps federal results mechanically unchanged and matches the state-design
principles already adopted in the state-tax implementation plan.

---

## 3. ATR vs MTR

For this application, **ATR is more defensible than MTR**.

Reason:

- A worker's labor-supply decision is often modeled off the marginal reward to one
  more dollar of earnings, so MTR or net-of-tax rate is natural there.
- A household's migration decision is more plausibly driven by the overall tax burden
  attached to living in a state, that is, by expected disposable income or the average
  tax wedge, not just by the tax treatment of the next dollar.

That said, the most useful utility shifter may be **after-tax income itself**, or
`log(after-tax income)`, rather than ATR alone.

Suggested definitions for a tax unit `i` evaluated in state `s`:

- `ATI_is = expanded_inc - liab_iit_net - liab_pr_ee - liab_st_iit_s`
- `ATR_is = (liab_iit_net + liab_pr_ee + liab_st_iit_s) / max(expanded_inc, eps)`

Recommendation for v1:

- Treat **after-tax income or log after-tax income** as the core utility term.
- Keep **ATR as a diagnostic and alternative specification**, since it is intuitive and
  closely aligned with the policy story.

This avoids the pathologies ATR can create for very low or negative incomes while still
keeping the migration response tied to the total state-specific tax burden.

---

## 4. Discrete-choice sketch

The cleanest first-pass model is a conditional-logit style choice over states:

`U_is = alpha_s + beta_g * log(ATI_is) + X_i * Gamma_s + epsilon_is`

where:

- `alpha_s` = state fixed effect or destination amenity term
- `beta_g` = tax sensitivity for group `g`
- `X_i * Gamma_s` = optional interactions for demographic or income sorting
- `epsilon_is` = idiosyncratic taste shock

Then predicted state choice probabilities are:

`p_is_new = exp(U_is) / sum_j exp(U_ij)`

### Partial-adjustment version

A pure static logit can imply too much movement after a one-year tax change. A practical
way to damp that is:

`p_is_adj = (1 - lambda_g) * p_is_old + lambda_g * p_is_new`

where `lambda_g` is a group-specific adjustment speed or moving-friction parameter.

This lets us distinguish:

- long-run tax sensitivity from the literature, and
- the smaller within-year migration response that the simulator should use in annual
  budget windows.

### Important limitation of v1

Because the current state architecture uses split weights rather than an observed origin
state, a first implementation should **avoid origin-destination flow modeling**. In
other words, v1 should reweight destination probabilities, not try to build a full
migration matrix with state-pair moving costs.

If later versions carry a persistent state assignment or prior-year dominant state, we
could add explicit stay bonuses or bilateral moving frictions.

---

## 5. Literature-based parameterization strategy

This module should not start with one universal elasticity. It should start with
**heterogeneous sensitivity profiles pulled from the literature**.

Useful anchors:

- **Moretti and Wilson (AER 2017)** provide a strong upper-tail benchmark. Their state
  tax competition paper reports a long-run mobility elasticity of inventors with
  respect to the net-of-tax rate of about **1.8**.
- **Kleven, Landais, and Saez (QJE 2013)** and related superstar/foreign-worker papers
  are useful as high-mobility upper bounds, but they should not be treated as default
  parameters for the general population.
- For the broad population, the literature is typically much less responsive than the
  top-earner or superstar literature, so the baseline calibration should be
  conservative outside the upper tail.

Recommended calibration approach:

1. Calibrate `alpha_s` so the model reproduces baseline state probabilities or weights.
2. Choose `beta_g` values so the model's **implied elasticity** matches a literature
   target for that group under baseline shares and income levels.
3. Use `lambda_g` to convert long-run elasticities into annual adjustment rates.
4. Store multiple sensitivity cases rather than one "true" parameter vector.

Practical v1 grouping:

- top-income or high-skill group
- upper-middle income group
- everyone else

That is simple enough to estimate and interpret, while leaving room for richer
heterogeneity later.

---

## 6. How this fits the current state-tax architecture

The most important design point is architectural:

- the current state system works by generating state liabilities for every
  `id x state x year` combination and then aggregating with state weights.

So the migration module should sit **after** state-specific liabilities are available,
not inside the current national `behavior` hook.

### Why not reuse the existing `behavior` column?

Because the existing behavioral contract expects:

- one national tax-unit tibble, plus
- optional baseline/static MTR tables.

A migration module instead needs:

- a choice panel with one row per `id x state`,
- baseline state weights,
- state-specific tax liabilities or after-tax incomes, and
- possibly the prior-year state-weight vector.

That argues for a **separate module type**, not a small extension of the current one.

Possible future interface:

- new runscript column: `state_behavior`
- module directory: `config/scenarios/state_behavior/migration/`
- execution hook in `src/sim/run.R` after the static state panel is built

Under the current "federal once, states downstream" architecture, this module would
change **state** outcomes only. Federal tax results would stay fixed in v1.

---

## 7. Implementation sketch

One workable path:

1. Factor the current state post-processing so we can build a reusable
   `state_choice_panel` with columns like
   `id, year, state, weight_baseline, liab_st_iit, ATI, ATR`.
2. Add a new state-behavior runner that reads that panel plus module parameters and
   returns updated state weights by `id x state`.
3. Feed those updated weights into state aggregation for the conventional pass.
4. Write diagnostics alongside state totals:
   `state_population_change`, `weight_shift`, `revenue_with_migration`, and
   `revenue_without_migration`.

Candidate helper functions:

- `build_state_choice_panel()`
- `do_state_behavior_feedback()`
- `apply_state_migration_choice()`
- `summarize_state_migration()`

The module itself should return weights, not modified tax-unit income variables.

---

## 8. Preconditions and guardrails

Before this becomes operational, several constraints should be respected:

- **Do not build migration on top of placeholder state weights.** The current uniform
  placeholder split is useful for plumbing, but not for meaningful migration analysis.
- **Require a full or near-full state choice set.** A migration module should probably
  require `states = all`, because a truncated destination set will distort choice
  probabilities.
- **Keep yearly persistence explicit.** If the model is run over multiple years, the
  updated state weight vector should become the next year's baseline only when we
  intentionally want dynamic path dependence.
- **Preserve national totals mechanically.** For every tax unit, the module should
  enforce `sum_s w_is = w_i`.
- **Treat PR/OA carefully.** They are carried in the state-weight architecture, but the
  migration choice set may need a policy decision about whether they are modeled as
  destinations or kept fixed.

---

## 9. Recommended first version

The safest useful first version is:

- destination choice over states using `log(ATI)` as the main tax utility term,
- heterogeneous `beta_g` values with a conservative baseline and upper-tail
  sensitivity cases,
- partial adjustment through `lambda_g`,
- no origin-destination matrix,
- no federal feedback loop,
- and output limited to state revenue and state population-weight changes.

That gives a policy-relevant module without forcing the simulator to solve a much more
ambitious spatial equilibrium problem.

---

## 10. References to carry into implementation

- Repo behavior pipeline:
  `src/sim/run.R`, `src/sim/behavior.R`,
  `config/scenarios/behavior/employment/bastian.R`,
  `config/scenarios/behavior/child_earnings/34.R`
- State-weight architecture:
  `research/state_tax/plan.md`
- Split-weight precedent:
  TPC, *Incorporating State Analysis into the TPC's Microsimulation Model* (2016)
- Treasury architecture precedent:
  OTA Technical Paper 6 (Fisher and Lin, 2015)
- High-mobility tax response benchmark:
  Moretti and Wilson, *The Effect of State Taxes on the Geographical Location of Top
  Earners* / inventor-mobility results in AER (2017)
- Upper-bound superstar mobility evidence:
  Kleven, Landais, and Saez (2013) and related follow-on papers
