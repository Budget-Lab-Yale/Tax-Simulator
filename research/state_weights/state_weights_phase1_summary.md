---
title: "State Split Weights — Phase 1 Bake-off Summary"
role: method
workstream: state_weights
status: current
updated: 2026-08-20
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# State Split Weights — Phase 1 Bake-off Summary

**Date:** 2026-07-19
**Companions:** `research/state_tax/plan.md` §2.1 (design),
`research/archive/state_weights_ml_alternative_2026-07-08_superseded.md` (bake-off spec and §4 harness),
`research/state_weights/notes/state_weights_fit_issues.md` (first-fit diagnosis and engine hardening),
`research/state_weights/notes/state_weights_alternatives.md` (alternatives brainstorm).
**Code:** `src/data/state_weights.R` (ingestion, assembly, engines),
`research/state_weights/scripts/sweep_state_weights.R` (hyperparameter sweep),
`research/state_weights/scripts/validate_state_weights.R` (validation battery),
`research/state_weights/scripts/build_state_weights.R` (end-to-end driver).

This document summarizes the Phase 1 weights work through the hyperparameter
sweep and validation battery: the methods, the data, the parameters tried,
the results, the recommended configuration, proposed methodological
improvements, and how our approach and results compare to the Tax Policy
Center (TPC) and Treasury OTA precedents.

---

## 1. Objective and the design invariant

Tax-Simulator computes federal tax on a national PUF-derived microdata file.
To model state income taxes, each record `i` with national weight `w_i` is
assigned **split state weights** `W[i,s] ≥ 0` over `S = 53` jurisdictions
(50 states + DC modeled; PR and OA carried as no-tax buckets) subject to the
per-record constraint

```
Σ_s W[i,s] = w_i    for every record i.
```

The constraint makes with-state and without-state model runs coherent: for
any variable, national aggregates are preserved exactly, so the weighting
method can only change *how* each record's weight is distributed
geographically — never a federal total. Both candidate engines enforce the
constraint structurally (it held to 1.1e-15 in the final full-scale runs,
asserted at write time).

## 2. Data

| Input | Source | Role |
|---|---|---|
| Tax units, TY2022 (220,897 records) | Tax-Data (production vintage), plus `agi`/`eitc` from a full-sample Tax-Simulator baseline run (`weights_2022` local vintage) | Records to be split; x-vectors for targets |
| SOI Historic Table 2 (HT2), TY2022 | IRS-Ind mirror (`shared/raw_data/IRS-Ind`, github.com/johniselin-budget-lab/IRS-Ind) | Filer-partition targets: state × AGI-stub cells |
| ACS 2022 (IPUMS extract) | `shared/raw_data/ACS/acs_common` | Non-filer partition targets (state × age band × income tier); individual-level IRS/ACS reconciliation |
| IRS EITC participation rates, TY2022 | irs.gov EITC Central (ACS–Census linkage) | Untargeted-geography covariate (see §5.3) |
| Census QWI / LODES | Census APIs (fetchers in `state_weights.R`) | Candidate demographic targets and untargeted checks (documented, not yet targeted) |

**Filer targets.** 22 HT2 series — return counts (total, single, joint, HoH),
individuals (N2), and amount+count pairs for AGI, wages, interest, dividends,
capital gains (count only, see below), SALT income/sales tax, real-estate
tax, mortgage interest, and EITC — across 10 AGI stubs × 52 areas:
**10,229 usable targets** after assembly guards. **Non-filer targets:** 1,390
ACS cells; with single-membership cells and count-only targets, the
calibration engine fits them exactly in one pass (100% within 2%, MARD 0),
so everything below concerns the filer partition.

**Share normalization (key design point).** Raw HT2/ACS levels differ from
PUF nationals (coverage, vintage), which would make level targets
infeasible under the invariant. Every target is therefore the PUF's own
national total for the (cell, series), distributed by the HT2/ACS **state
shares**: `target(st, cell, v) = PUF_total(cell, v) × share_HT2(st | cell,
v)`. National concept wedges cancel; only geography is being estimated.

**Excluded targets.** Non-positive HT2 cells are blocked at assembly
(net-negative `kg_amt` and stub-1 `agi_amt` cells flipped whole probability
columns negative in the first full-scale run — the root cause of the
invariant leak documented in `research/state_weights/notes/state_weights_fit_issues.md`). `kg_amt` is
excluded wholesale for mixed sign within cells; the `n_kg` count target is
retained. Stub-1 amount targets are skipped by the positivity guard.

## 3. Methods

### 3.1 The two engines

**Approach A — classical raking (`fit_calibration`).** Damped multiplicative
IPF seeded at the prior: for each target, rescale the (state) column of the
implicated rows by `clamp(target/predicted, [1/2, 2])`, then renormalize
rows. Hardened with a denominator floor and unfittable-target reporting.

**Approach B — differentiable reweighting (`fit_gradient`).** Each record's
split is a softmax over logits `θ[i,·] ∈ R^S`, so the invariant is
structural. Loss = Σ_t λ_t ((T̂_t − T_t)/T_t)² + β·Σ_i KL(P[i,·] ‖ P0[i,·]),
minimized by Adam on θ with analytic gradients (dependency-free; torch is
not on the cluster; verified against finite differences to 1e-10). Targets
sharing a row set (all series within a stub) are stacked into per-group
matrix operations, so a full-data step is ~10 GEMMs (300 steps ≈ 7.5 min;
the sweep added an optional cosine learning-rate schedule and warm-start
logits).

### 3.2 The structural result that reframed the bake-off

Sequential per-target IPF **cannot** satisfy ~21 series constraining the
same (stub × state) cell — a per-cell multiplier is one degree of freedom
against 21 constraints. Proof from the diagnosis: restricted to one
constraint per cell (`n_returns` only), the identical engine converges in
one iteration to 100% within 2%; on the full multi-series set it plateaus
at ~10%. The correct classical estimator for joint multi-constraint
calibration is Deville–Särndal raking / exponential tilting —
`P[i,st] ∝ P0[i,st]·exp(Σ_v λ_{st,v}·x_iv)` — which is the same
distributional family as Approach B's softmax objective.

The bake-off therefore became a **pipeline**, which is what all results
below evaluate:

1. **Counts-backbone IPF prior:** `fit_calibration` on `n_returns` only
   (valid, exact, fast) seeded at HT2 within-stub return shares;
2. **Joint fit:** `fit_gradient` from that prior over all 22 series.

## 4. Hyperparameter sweep

### 4.1 Grid

13 configurations (SLURM array, 32 GB, ~22–130 min each), varying:

| Axis | Values |
|---|---|
| Steps | 1,000 / 3,000 / 6,000 |
| Learning rate | 0.1 / 0.2, constant or cosine-annealed |
| β (KL anchor to prior) | 1e-2 / 1e-3 / 1e-4 / 1e-5 |
| λ up-weighting | ×1, or ×4 on the round-1 laggard series (SALT, real-estate tax, EITC, AGI, individuals) |

Metrics are always scored against the original (λ = 1) targets.

### 4.2 Results

| id | steps | lr | schedule | β | λ↑ | within 2% | MARD | laggards w2% | min-state Kish ESS | rows >0.99 share | min |
|---|---|---|---|---|---|---|---|---|---|---|---|
| **11** | 3000 | 0.1 | cosine | 1e-5 | 1 | **96.9** | **0.27** | 94.7 | 990 | 0.18% | 80 |
| 12 | 3000 | 0.1 | cosine | 1e-5 | 4 | 96.2 | 0.32 | 96.1 | 720 | 0.51% | 81 |
| 13 | 6000 | 0.1 | cosine | 1e-4 | 4 | 95.7 | 0.36 | 95.6 | 1010 | 0.10% | 132 |
| 8 | 3000 | 0.1 | cosine | 1e-4 | 4 | 95.3 | 0.41 | 95.1 | 790 | 0.09% | 66 |
| **7** | 3000 | 0.1 | cosine | 1e-4 | 1 | **95.3** | **0.43** | 91.6 | **1205** | **0.03%** | 66 |
| 6 | 3000 | 0.1 | cosine | 1e-3 | 4 | 89.2 | 0.87 | 88.5 | 1515 | 0.01% | 66 |
| 5 | 1000 | 0.1 | cosine | 1e-3 | 4 | 87.2 | 1.06 | 85.5 | 1442 | 0.01% | 22 |
| 2 | 3000 | 0.1 | const | 1e-3 | 1 | 86.6 | 1.10 | 77.1 | 1616 | 0.01% | 65 |
| 9 | 3000 | 0.2 | cosine | 1e-3 | 1 | 86.5 | 1.09 | 76.8 | 1619 | 0.01% | 74 |
| 4 | 3000 | 0.1 | cosine | 1e-3 | 1 | 86.4 | 1.10 | 76.7 | 1604 | 0.01% | 66 |
| 1 | 1000 | 0.1 | const | 1e-3 | 1 | 86.1 | 1.14 | 76.0 | 1594 | 22 |
| 3 | 1000 | 0.1 | cosine | 1e-3 | 1 | 85.0 | 1.24 | 73.6 | 1583 | 0.01% | 22 |
| 10 | 1000 | 0.1 | const | 1e-2 | 1 | 60.6 | 3.28 | 47.8 | 3481 | 0.00% | 26 |
| — | (300-step baseline, β=1e-3, pre-sweep) | | | | | 82.9 | 1.43 | — | — | — | 7.5 |

**Findings.**

- **β is the binding constraint, not optimization budget.** At fixed β,
  tripling steps or changing the lr schedule moves within-2% by ~1 point;
  relaxing β=1e-3 → 1e-4 moves it by ~9 points (86 → 95.3).
- **Diminishing, costly returns past β=1e-4.** β=1e-5 buys +1.6 points
  (95.3 → 96.9) while near-degenerate rows (max share > 0.99) rise 6×
  and min-state effective sample size falls 1205 → 990 (λ×4: 720).
  Doubling steps at β=1e-4 adds +0.4. **The ≥99% TPC-style bar is not
  reachable on this axis** — this is the anchor-vs-fit tradeoff, and past
  β=1e-4 it buys targeted fidelity with weight quality.
- **The misses migrate to counts.** In the best configs the historic
  problem series fit essentially perfectly (`re_tax_amt`,
  `mort_int_amt`, `int_amt`, `div_amt` at 100%), while `n_salt` (86.2%)
  and `n_returns` (88.1%) become the worst — the joint fit spends
  count-backbone fidelity to buy amounts.

## 5. Validation beyond targeted fit

### 5.1 Persistent misses — a small structural core

239 of 10,229 targeted cells (**2.3%**) miss the 2% band in *every* one of
configs {7, 8, 11, 12, 13}. They concentrate in `n_returns` (58), `n_salt`
(44), and `n_joint` (27); by stub, almost entirely at the top of the income
distribution (stubs 8–9: 156 cells) plus the negative/low-AGI stubs 1–2
(56); by state, led by CA/FL/TX. Pattern: thin PUF support against extreme
HT2 shares — a **target-consistency problem, not an optimization problem**
(no configuration can fit them; several likely conflict with other series in
the same cell). Full list: `sweep/validation/persistent_misses.csv`.

### 5.2 Untargeted held-out HT2 series — the decision metric

Five HT2 series never used in calibration, share-normalized identically to
the targeted set (per state × stub), scored for the prior alone and both
candidate configs (MARD, %):

| held-out series | HT2 code | prior | cfg 7 | cfg 11 |
|---|---|---|---|---|
| Federal income tax after credits | A06500 | 18.5 | **13.2** | 12.6 |
| Sched. C business income (count) | N00900 | 17.3 | 14.8 | 15.5 |
| Sched. C business income (amount) | A00900 | 29.8 | 30.8 | 32.7 |
| Taxable pensions (amount) | A01700 | 17.9 | **17.0** | 18.4 |
| Taxable social security (amount) | A02500 | 21.0 | **17.1** | 17.7 |
| Capital gains, positive part | A01000 | 70.0 | 61.0 | 60.0 |
| **Overall (9 series, 4,607 cells)** | | 25.3 | **22.3** | 22.7 |

**Findings.**

- The joint fit **generalizes modestly** (overall MARD 25.3 → 22.3 vs the
  prior) — it is not free-parameter memorization — but untargeted geography
  is far from targeted quality (22% vs 0.4% MARD).
- **β=1e-5 (cfg 11) is worse than β=1e-4 (cfg 7) on most held-out series**
  despite its better targeted fit: the classic overfitting signature the
  bake-off's decision rule was written for.
- The weak series are demographically distinctive — retirement income
  (pensions, SS) and business income geography — and **nothing in the
  target set carries age or self-employment structure**. This is the
  strongest argument for the demographic target expansion in §7.

### 5.3 The EITC take-up covariate (negative result worth keeping)

Tested whether IRS state EITC participation rates (TY2022 ACS-linked;
73.6% DC – 85.2% ME) explain EITC target residuals: they do not
(cor −0.03 for `eitc_amt`, p = 0.81), and re-sharing claims targets by
take-up makes them *less* consistent with the other constraints (within-2%
78% → 40% with no refit). Concept: HT2 EITC targets are claims-among-filers,
matching our full-claiming calculated EITC for PUF filers; the participation
gap is mostly non-filing, which lives in the ACS non-filer partition.
**Incidental finding:** `n_returns` signed errors correlate strongly with
take-up (−0.61 target-weighted, p < 0.001) in *every* sweep config —
filing-propensity geography leaks into the count backbone. Take-up stays in
the harness as an untargeted covariate, not a target adjustment.

### 5.4 Pilot-state liability (spec metric 5 — "the real test")

State income tax computed by our Phase 3 calculator (`src/calc/state/`) on
the full post-federal 2022 population, aggregated under each candidate
weight set (filers from the sweep fit; non-filers from the exact IPF):

| state | cfg 7 ($bn) | cfg 11 ($bn) | external benchmark ($bn) |
|---|---|---|---|
| IL | 23.10 | 23.26 | 23.26 — Census STC FY2023 net collections |
| CO | 9.59 | 9.62 | 6.78 net of ~$3.7bn FY2023 TABOR refunds → ~10.5 gross-equivalent |
| NY | 63.55 | 60.69 | 41.17 — NY DTF TY2022 **full-year-resident** liability (10.7M-return All-Filers total: 50.48) |

Benchmark caveats (documented, material): Census figures are fiscal-year
net collections, not tax-year liability; IL and NY collections embed
**pass-through entity tax credits** (PTET) that our individual calculator
deliberately excludes — for NY TY2022 the PTET offset is on the order of
$10bn+, explaining most of the apparent NY overshoot; CO's FY2023 figure
nets the TABOR refund mechanisms our liability concept excludes (the same
concept choice documented against PolicyEngine in the Phase 5 cross-model
work). Residual reading: **IL lands within ~1%** (flat tax, few concept
wedges), **CO is plausibly within ~10%** of the gross-equivalent, **NY is
overstated even after the PTET adjustment** — consistent with too much
top-income mass assigned to NY and/or unmodeled subtractions; NY is also
where the two weight sets differ most ($2.9bn, 4.7%), i.e., the weights
materially move state revenue answers. Nonresident liability (NY: $8.1bn
TY2022) is out of scope for residence-based weights — the LODES
origin-destination work is the planned Phase 7 path.

## 6. Decision

**Adopt config 7 — counts-backbone IPF prior → joint gradient fit, β = 1e-4,
λ = 1, lr 0.1 cosine, 3,000 steps — as the production candidate.**

Rationale, per the spec's decision rule (prefer the tighter-anchored fit
unless extra targeted fidelity is matched by untargeted/downstream gains):
cfg 11's +1.6 targeted points come with *worse* untargeted MARD on most
held-out series, 6× the near-degenerate rows, and 18% lower min-state ESS —
overfitting, not information. Config 7 achieves 95.3% within-2% / 0.43%
MARD targeted, the best untargeted generalization of any configuration, the
cleanest weight-quality profile, and (pilot IL) external liability accuracy
within ~1% where concept wedges are small.

Remaining before the production swap-in: prune/report the 239-cell
structural core (§5.1) the way assembly already reports unfittable targets,
wire `build_split_weights(method = "gradient")` to these hyperparameters,
and write the `state_weights_{year}.csv` interface output for all years.

> **Amended 2026-08-19 (S2) — this checklist predates the sequencing decision.**
> Three documents cite the paragraph above by line number, and it was written
> 2026-07-19, before **JI's 2026-08-16 call that the non-filer rework lands
> BEFORE the Phase 1 swap-in** so the fit happens once on upgraded margins. The
> items above are still the right items; they are **not** the next work. Read
> `research/state_weights/plan.md` first — the swap-in is its task group G, gated
> on D (Tax-Data rework), E (federal validation) and F (re-fit on the new
> margins). See `research/decisions_log.md` S2.

## 7. Paths to a better fit (proposed alternatives)

Ranked by expected value given §5:

1. **Demographic target expansion (highest priority).** The held-out
   failures (pensions, SS, business income) are exactly the dimensions the
   target set lacks. Candidates already scoped in plan §2.1 and wired as
   fetchers: QWI sex × age employment margins (residence-corrected via
   LODES RAC), ACS marital × age × wage-earner cells mapped to the PUF
   earner structure. Start as untargeted validation, promote to targets
   where fit is poor. Also directly addresses the filing-propensity leak
   (§5.3) via a filing-rate covariate in the non-filer partition.
2. **Dual-space maxent (candidate production engine, from
   `research/state_weights/notes/state_weights_alternatives.md`).** Generalized raking on the ~12k
   Lagrange multipliers (one per target) instead of 11M primal logits;
   sign-indifferent, so it can reclaim `kg_amt` and negative business
   income series the primal engines exclude; closed-form dual gradients.
   Worth building if #1 raises the target count materially (the dual
   dimension grows with targets, not records).
3. **Structural-core resolution.** For the 239 persistent misses:
   reconcile conflicting same-cell constraints at assembly (e.g., cap
   implied within-cell shares), report the remainder as unfittable with
   provenance instead of letting them pollute MARD.
4. **Sign-split calibration** for loss series ($3k-loss-cap split of
   capital gains into gain/loss components as separate non-negative
   targets) — the cross-check documented in the alternatives memo.
5. **ACS-donor matched priors.** Replace the HT2-share prior with a
   donor-matched `p(state | demographics)` prior from ACS records; the
   KL anchor then regularizes toward demographic geography rather than
   income-cell geography (complements #1).
6. **Amortized multi-year fit** (`f_φ` shared across years) once the
   single-year architecture is frozen — needed anyway for the 2017–2024
   panel, and pooling stabilizes thin cells.
7. **Replicate-weight uncertainty** (delete-a-group jackknife over PUF
   records) to put standard errors on state aggregates — required before
   publishing state revenue estimates.

## 8. Relation to TPC and OTA

Both precedents share our core design — split each record's national weight
across states so state results mechanically sum to national totals — and
differ in how `p(state | x)` is estimated and calibrated.

**Treasury OTA (TP-6, Fisher & Lin 2015).** Estimates `p(st | x)` from the
*population* IRTF via a decomposable log-linear graphical model (gRim,
closed-form MLE) over nine discretized return characteristics under a
conditional-independence assumption, then applies it to INSOLE. Validation
vs HT2 (2008): correlations 0.986–0.999 and **MARD 0.02–0.09** for most
variables, real-estate tax weakest. Comparison: our targeted MARD (0.004)
beats that range, but the honest comparison is our **untargeted** held-out
MARD (0.13–0.31 per series) against their (largely untargeted-by-
construction) 0.02–0.09 — OTA's advantage is *population* microdata (the
IRTF sees every return's state), which no amount of calibration against
published cells replicates. Their real-estate-tax weakness mirrors our
SALT/re-tax laggards: property-tax geography is poorly predicted by income
composition everywhere. We cannot use the IRTF; our closest substitute is
richer public margins (§7.1).

**TPC (2016).** Schirm–Zaslavsky constrained Poisson regression within
**nine AGI strata**, 39–51 targets per stratum from SOI state tables
(TY2011): of 22,308 targets, **0.1% missed adjusted targets by >2%**
(99.9% within-2%; our config 7: 95.3%, with a 2.3% structural core). Two
qualifications make the gap smaller than it looks: TPC calibrates
*adjusted* targets (their small-area model first reconciles targets;
analogous to our share normalization plus the pruning proposed in §7.3),
and their downstream validation vs 11 state revenue models showed **filer
counts overestimated 8–35%** and taxable income within 5% for all but two
states — our filing-threshold machinery (`st_filing.req_threshold`, Phase
3) and non-filer partition target exactly the filer-overcount problem, and
our pilot IL result (~1%) is consistent with their better states. TPC runs
Bakija's calculators downstream; we run our own Phase 3 calculators,
validated separately against TAXSIM/PolicyEngine in Phase 5.

**Where we sit methodologically.** Our pipeline is a hybrid: OTA/TPC-style
cell targets, PolicyEngine-Enhanced-CPS-style differentiable reweighting
(softmax + KL anchor, analytic gradients), with the split-constraint
enforced structurally rather than by renormalization. The Deville–Särndal
equivalence (§3.2) means our gradient engine is the classical estimator in
modern clothing — the innovation is operational (joint fit of 10k+
constraints in ~7 GEMM-minutes, warm starts, schedules), not statistical.
What we *give up* vs OTA is population microdata; what we *add* vs both is
an explicit prior-anchor dial (β) with a measured fit-vs-weight-quality
frontier, unfittable-target accounting, and a validation battery (held-out
series, take-up covariate, pilot liability) run before adoption.

## 9. Reproduction

All artifacts under `/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`
(`sweep/` for fits and metrics, `sweep/validation/` for the battery).

```bash
# from the repo root, each under sbatch (login node OOM-kills at ~7-8 GB)
Rscript research/state_weights/scripts/sweep_state_weights.R --prior     # once
Rscript research/state_weights/scripts/sweep_state_weights.R <1..13>     # array
Rscript research/state_weights/scripts/sweep_state_weights.R --collect
Rscript research/state_weights/scripts/validate_state_weights.R --misses
Rscript research/state_weights/scripts/validate_state_weights.R --untargeted
Rscript research/state_weights/scripts/validate_state_weights.R --pilot
```

External benchmarks: Census STC FY2023 flat file (T40 row); NY DTF
Personal Income Tax Filers Summary Dataset 1 (Socrata `73iw-kuxv`,
tax_year 2022, `place_of_residence = 'All Places'`); IRS EITC Central
participation table (TY2022).
