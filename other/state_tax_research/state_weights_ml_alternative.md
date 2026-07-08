# State Weights — ML Alternative and A/B Comparison

**Date:** 2026-07-08
**Companion:** `state_tax_implementation_plan.md` §2.1 (Approach A) and
`state_tax_model_research_notes.md` §4 (evidence).

Purpose: define a second, ML-based method for constructing the split state weights
so Phase 1 is a **bake-off between two approaches** rather than a bet on one. Both
produce an identical `state_weights_{year}.csv` (`id, state, weight`) and calibrate to
the same SOI Historic Table 2 (HT2) targets, so they are directly swappable and
directly comparable.

---

## 0. The shared invariant (both approaches honor it)

Let record `i` have national weight `w_i` (from Tax-Data) and let `S = 52`
jurisdictions (50 states + DC + OA). Both approaches produce a weight matrix
`W[i,s] ≥ 0` subject to the **per-record split constraint**

```
Σ_s W[i,s] = w_i        for every record i.
```

This single constraint is what makes the with-state / without-state modes coherent:
for **any** variable `x`, the national aggregate is preserved exactly, since
`Σ_i Σ_s W[i,s] x_i = Σ_i w_i x_i`. Federal totals are therefore invariant to the
weighting method — the two approaches can differ only in *how they distribute* each
record's weight across states, never in national totals.

---

## 1. Approach A — classical calibration (baseline, plan §2.1)

Recap: estimate `p(state | x)` within AGI strata from HT2 shares (OTA uses a
log-linear graphical model; TPC a constrained Poisson regression), set
`W[i,s] = p̂(s | x_i) · w_i`, then rake / GREG-calibrate within strata
(Deville–Särndal) so state-weighted totals hit HT2 targets. Interpretable, fast,
statistically well-understood, standard in official statistics. Weaknesses: rigid
stratification, difficulty hitting many targets jointly across strata, and a
restrictive functional form for `p(state | x)`.

---

## 2. Approach B — differentiable reweighting (the ML alternative)

Pose weight construction as a single optimization problem and solve it by gradient
descent — the method behind PolicyEngine's Enhanced CPS (research notes §2.4, §4).
No stratification and no separately-estimated `p(state|x)` model; all targets are
fit jointly.

### 2.1 Parameterization (bakes in the constraint)

Represent each record's split as a softmax over a logit vector `θ[i, ·] ∈ R^S`:

```
W[i,s] = w_i · softmax_s(θ[i,·]) = w_i · exp(θ[i,s]) / Σ_s' exp(θ[i,s']).
```

The softmax gives `W[i,s] ≥ 0` and `Σ_s W[i,s] = w_i` **automatically** — the split
constraint is structural, not a penalty, so it can never be violated numerically.
(This is cleaner than reweighting a single national vector: we exploit softmax to
keep the split-weight identity exact for free.)

### 2.2 Loss

```
L(θ) = Σ_t λ_t · ((T̂_t(W) − T_t) / T_t)²          # target fidelity
       + β · Σ_i KL( softmax(θ[i,·]) ‖ prior_i )     # stay-close regularizer
```

- Targets `t` are HT2 cells: (state s) × (AGI class c) × (variable v) — return
  counts and dollar amounts for AGI, wages, interest, dividends, capital gains, SALT
  income deduction, real-estate tax, mortgage interest, EITC. `T̂_t(W)` is the
  weighted total over records in class `c`. `λ_t` lets us up-weight counts vs
  amounts, or scarce cells.
- **Filers vs non-filers:** HT2 targets constrain filers only. Partition records by
  the exogenous `filer` flag and add **ACS/Census population-margin targets** (state ×
  age × income-tier) for the non-filer partition, plus a `sum(W·n_people)`-by-state
  population reconciliation target spanning both. In B this is trivial — non-filer
  cells are just more terms in the same loss; the softmax split constraint still holds
  per record. See plan §2.1 "Filers vs non-filers" for rationale and caveats.
- The KL term pulls each record's split distribution toward a **prior** and is the
  analog of PolicyEngine's dropout regularization and of classical calibration's
  "stay near the design weights." Natural prior choices: uniform, HT2-share prior, or
  **Approach A's own output** (see §2.4). `β` trades target fit against weight
  distortion; `β → ∞` recovers the prior (i.e. A), `β → 0` lets targets dominate — so
  B *nests* A as a limiting case.

### 2.3 Two parameterizations of θ (pick on the flexibility/cost axis)

- **B-dense** — `θ` is a free `N × S` logit matrix (~221k × 52 ≈ 11.5M parameters
  per year). Maximum flexibility, per-record freedom, closest to PolicyEngine's raw
  reweighting. Large but tractable in `torch`.
- **B-amortized** — `θ[i,·] = f_φ(x_i)`, a small learned function (linear, GBM, or a
  shallow net) mapping record features to `S` logits, trained end-to-end against the
  same loss. Far fewer parameters, smoother, and **generalizes to new records/years**
  (score a future year without re-solving). This is essentially an ML `p(state|x)`
  classifier trained by the downstream calibration objective instead of by supervised
  state labels (which the PUF lacks) — the sharpest intellectual contrast with A,
  which fits `p(state|x)` to marginals and calibrates separately.

Recommendation within B: prototype **B-dense** first (simplest, matches PolicyEngine,
warm-started from A), add **B-amortized** if cross-year generalization or weight
smoothness matters.

### 2.4 Warm start / prior from Approach A

Initialize `θ` (or the amortized `prior_i`) from A's `p̂(state|x)`. Then B *starts
where A ends* and only moves weights insofar as the joint target loss rewards it.
This makes the comparison fair (same information going in) and gives a principled
ensemble: A is the interpretable prior, B the target-driven refinement.

### 2.5 Tooling and compute

- **`torch` for R** (libtorch bindings) — native autograd, CPU or GPU, keeps the
  prototype in-repo and in R. Optimizer: Adam, a few hundred to few thousand
  full-batch steps per year.
- B-dense is ~11.5M params × (2017–2022 historical years) — fine on GPU, heavy but
  feasible on CPU (minutes–tens of minutes/year). B-amortized is far lighter.
- **Reproducibility:** fix the torch seed via `globals$random_seed`; log the loss
  curve and final per-target residuals. Determinism is a real difference from A
  (which is deterministic given its estimator) and must be pinned.
- New dependency (`torch`) — note the cluster module/Arrow caveats in CLAUDE.md; verify
  `torch` availability before committing to B as production.

### 2.6 Skeleton (design sketch — NOT yet runnable; HT2 data not in hand)

```r
# B-dense, one simulation year. Pseudocode-level; shapes and API to be verified.
library(torch)
set.seed(globals$random_seed); torch_manual_seed(globals$random_seed)

# X: N x P matrix of record features used to build targets (AGI-class dummies, etc.)
# A: sparse target design; target_t = sum_i A[t,i,s] * W[i,s]   (per state/class/var)
# w_nat: length-N national weights; theta0: N x S warm-start logits from Approach A

theta <- torch_tensor(theta0, requires_grad = TRUE)   # N x S
w_nat <- torch_tensor(w_nat)                           # N
opt   <- optim_adam(list(theta), lr = 0.05)

for (step in seq_len(n_steps)) {
  opt$zero_grad()
  P  <- nnf_softmax(theta, dim = 2)                    # N x S split shares
  W  <- w_nat$unsqueeze(2) * P                         # N x S weights (row-sums = w_nat)
  That <- estimate_targets(W)                          # length-T predicted totals
  fit  <- ((That - T_target) / T_target)^2 * lambda
  reg  <- beta * kl_div(P, prior)                      # stay-close-to-prior
  loss <- fit$sum() + reg
  loss$backward(); opt$step()
}
W_final <- as.matrix(w_nat$unsqueeze(2) * nnf_softmax(theta, dim = 2))
# reshape to long (id, state, weight); drop weights below a small epsilon
```

---

## 3. Why B is a real alternative (not a reskin of A)

| Dimension | A: classical calibration | B: differentiable reweighting |
|---|---|---|
| `p(state\|x)` model | explicit, restrictive (log-linear / Poisson) | none (dense) or flexible learned (amortized) |
| Target fitting | per-stratum raking/GREG | all targets jointly, one loss |
| Adding/reweighting targets | re-derive per stratum | change `λ_t`, re-run |
| Interpretability | high (known estimator theory) | lower (optimizer output) |
| Uncertainty | closed-form variance (Deville–Särndal) | none analytic; needs bootstrap |
| Extreme-weight control | bounded calibration variants | KL/entropy regularizer, direct |
| Cross-year generalization | re-estimate per year | amortized model scores new years |
| Tooling / cost | base R + `survey`; fast, deterministic | `torch`; slower, seed-dependent |
| Provenance | OTA TP-6, TPC | PolicyEngine ECPS |

They are **not mutually exclusive**: A's output is B's prior/warm start, and B with
`β→∞` reproduces A — so the bake-off also tells us whether the extra ML machinery buys
anything over the interpretable baseline.

---

## 4. Shared comparison harness

One harness, same inputs, both methods swapped in behind a common interface
(`build_state_weights(method = c("calibration","gradient"))`). Metrics:

1. **Target fidelity** — fraction of HT2 targets within 2% (TPC benchmark); MARD per
   variable. (Directly comparable to OTA's 0.02–0.09 MARD and TPC's 0.1% >2%.)
2. **Untargeted-variable validation** — hold out a subset of HT2 variables (and/or use
   state-agency figures) and measure error on them. This is the generalization test
   where A showed its real-estate-tax weakness; the most decision-relevant metric.
3. **Weight quality** — distribution of split shares; effective sample size per state
   (Kish `(Σw)²/Σw²`); count of near-zero and extreme weights; max within-record share.
4. **National invariance** — confirm `Σ_s W[i,s] = w_i` to tolerance (exact for both by
   construction) and that national aggregates are unchanged.
5. **Downstream accuracy (the real test)** — run the state calculator on the pilot
   states (IL, CO, NY) under both weight sets; compare state liability totals to state
   revenue-agency estimates and HT2 total tax. Weights exist to serve this number.
6. **Cost & reproducibility** — runtime per year; determinism; hyperparameter
   sensitivity (sweep `β`, `lr`).

**Decision rule.** Adopt A as the interpretable baseline unless B *materially* improves
untargeted-variable fidelity (metric 2) or downstream liability accuracy (metric 5)
enough to justify the `torch` dependency and loss of closed-form uncertainty. If B wins
only on targeted fidelity (metric 1) but not on 2/5, prefer A — that pattern means B is
overfitting the targets. Keep the ensemble option (A as B's prior) on the table.

---

## 5. Impact on the plan

Phase 1 becomes an **A/B bake-off**: build both behind `build_state_weights(method=)`,
run the harness, decide by §4. Adds ~1 week and a `torch` dependency check to Phase 1;
no change to downstream phases (weights format is identical either way).
