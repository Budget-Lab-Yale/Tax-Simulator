---
title: "Oregon kicker: modelling a credit keyed to prior-year liability"
role: notes
workstream: state_tax
status: open
updated: 2026-08-21
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Oregon kicker: modelling a credit keyed to prior-year liability

Drafted 2026-08-21 out of the OR cross-model triage (see
`research/source_packets/or.md` and
`research/state_tax/cross_model/results/reports/or.md`). The triage measured
the kicker precisely, excluded it from the harness, and left the provision
itself unmodelled. This note is why it should not stay that way, and what
modelling it requires.

## What the provision is

ORS 291.349. When Oregon's General Fund revenue for a biennium exceeds the
close-of-session forecast by 2% or more, the entire surplus is returned to
personal income taxpayers as a credit on the return for the biennium's second
year. The credit is a **fixed percentage of the taxpayer's own prior-year
Oregon liability before credits** — not of current-year income, and not a flat
per-filer amount.

Recent percentages, each applied to the prior year's liability:

| Return year | Percentage | Base year |
|---|---|---|
| 2019 | 17.171% | 2018 |
| 2021 | 17.341% | 2020 |
| 2023 | 44.28% | 2022 |
| 2025 | 9.863% | 2024 |

It recurs in odd years and it is large — the TY2023 kicker returned roughly
$5.6bn. It is not a rounding item in any Oregon revenue estimate.

## What the triage established

Among Oregon non-itemizers with liability above $500 in the harness sample,
`diff / st_tax_pre_credit` has a mode at **exactly 0.172 (n = 3,124)** in 2019
and **exactly 0.000** in 2017, 2018 and 2020. Three consequences:

1. TAXSIM computes the kicker from the **current-year** return, not the prior
   year's — otherwise the ratio would scatter with year-over-year income
   changes instead of sitting on a single value.
2. TAXSIM models it **only in 2019** within the TAXSIM window. It does not
   model the TY2017 kicker, which Oregon did in fact pay (5.6% of TY2016
   liability). That asymmetry is worth reporting upstream; it is filed in
   `research/state_tax/cross_model/external_model_issues.md` terms as a
   coverage gap rather than a computational error.
3. Removing the affected records takes the 2019 cell from 0.268 to **0.999**,
   so the kicker is the *entire* residual of that year once the itemizer
   crosswalk-exposure class is excluded.

## Why this is modellable, unlike the usual harness exclusion

Most known-difference rows record something the model genuinely cannot see —
rent, disability, a behavioural amount. The kicker is different. A
cross-sectional harness cell cannot see prior-year Oregon liability, but
**Tax-Simulator runs years in sequence over the same tax units**, so
`liab_st_iit` before credits for year *t-1* is available at year *t* in a
production run. The exclusion in `known_differences.csv` is a statement about
the harness, not about the model's reach.

## What implementing it requires

1. **A prior-year state liability carry.** The state calculator currently sees
   one year at a time. Something has to thread year *t-1*'s pre-credit state
   liability into year *t*. This is the substantive piece and it is
   orchestration, not tax law — which is why it belongs in the plan rather
   than in a state's YAML alone.
2. **A generic component, not an Oregon branch.** Parameterize as a credit
   equal to a rate times a prior-year liability measure, with the rate keyed by
   year and zero in non-kicker years. Any state adopting a surplus-rebate
   mechanism of this shape reuses it. Note the interaction with the existing
   one-time-rebate netting class (P5 in the triage record) — those rebates are
   flat per-filer amounts and are *not* the same mechanism.
3. **A first-simulated-year rule.** Year *t-1* does not exist for the first
   year of a run. Either require Oregon runs to start a year early (the
   convention CLAUDE.md already states for policy changes) or define an
   explicit fallback and document it as an approximation.
4. **A forward-looking rate assumption.** Past percentages are published;
   future ones depend on forecast error and are unknowable. Encoding zero for
   future years understates Oregon revenue returned to taxpayers; encoding a
   historical average asserts a surplus that may not occur. This is a
   projection decision to record explicitly, not to bury in a default.

## Sequencing and trigger

**Trigger: when Oregon output matters** — Oregon in a P1 production run, an
Oregon revenue estimate, or Oregon distributional tables. It is not needed for
the cross-model bar, which the exclusion already handles honestly.

Item 1 is the only part with reach beyond Oregon, and it is the part worth
designing before writing any of it: a prior-year state liability carry is
plausibly wanted by other lagged provisions, so it should not arrive as an
Oregon-shaped hook.
