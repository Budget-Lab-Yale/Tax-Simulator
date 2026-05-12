# η-Degeneracy in the Bellman Realization Spec

**Author:** Investigation triggered by failed calibration run 2026-05-11
**Status:** Open issue with the May-2026 PDF spec (`realization_dp (2).pdf`)
**Bottom line:** Under the spec's functional-form choices, η drops out of the
realization function entirely. There is no calibration knob to fit the
elasticity target. The Bellman + (1+g) implementation in
`src/sim/kg_dynamics.R` runs cleanly but produces a model where varying η
across the full range (tested 0.5 to 64) yields **the same** implied
elasticity (-0.0407 at year 10).

---

## 1. Empirical finding

Calibration run (`calibrate_eta_11471728.out`) on full-sample baseline:

```
Coarse sweep:
  eta =   0.50  ->  eta_30 = -0.0407
  eta =   1.00  ->  eta_30 = -0.0407
  eta =   2.00  ->  eta_30 = -0.0407
  eta =   4.00  ->  eta_30 = -0.0407
  eta =   8.00  ->  eta_30 = -0.0407
  eta =  16.00  ->  eta_30 = -0.0407
  eta =  32.00  ->  eta_30 = -0.0407
  eta =  64.00  ->  eta_30 = -0.0407
Error: Grid does not bracket target. Extend eta_grid or check inputs.
```

A 1pp uniform tax-rate perturbation produces a 0.2% drop in aggregate
realizations regardless of η. The bisection cannot proceed because the
function is flat in η.

---

## 2. Why it happens — algebraic derivation

Take step-up baseline (c = 0). The Bellman recurrence with the PDF's b form:

* `b'(x) = μ − (1/η) log x`  (PDF eq 6)
* `b(x) = μx − (1/η)(x log x − x)`
* `μ = P_B + (1/η) log(x_B)`  (PDF eq 8, μ recovered from baseline FOC)
* `P = ((1+g)(1-m)β W_next − mcτ)/(1−τ) − (1/η) log(1−τ)`  (PDF eq 5, with our g extension)
* `r_D* = (1/(1−τ)) exp(η(μ − P))`  (PDF eq 7, FOC closed form)

### 2.1 b(x) simplifies cleanly at the FOC

At baseline FOC, with `μ = P_B + (1/η)log(x_B)`:

```
b(x_B) = (P_B + (1/η)log(x_B))·x_B − (1/η)(x_B·log(x_B) − x_B)
       = P_B·x_B + (1/η)·x_B·log(x_B) − (1/η)·x_B·log(x_B) + (1/η)·x_B
       = P_B·x_B + (1/η)·x_B
```

The mid terms cancel; `b(x_B) = P_B·x_B + (1/η)·x_B` exactly.

### 2.2 Substituting P_B·x_B back into the Bellman

```
W_B[a,t]  = b(x_B) + (1 − r_D_B)·(1+g)(1-m)β·W_B[a+1,t+1]
```

With `x_B = (1−τ_B)·r_D_B`:

```
P_B·x_B = ((1+g)(1-m)β·W_B[next]/(1−τ_B) − (1/η)log(1−τ_B))·x_B
        = (1+g)(1-m)β·W_B[next]·r_D_B − (1/η)·x_B·log(1−τ_B)
```

So:

```
b(x_B) = (1+g)(1-m)β·W_B[next]·r_D_B + (1/η)·x_B·(1 − log(1−τ_B))
```

Plugging into the recurrence:

```
W_B[a,t] = (1+g)(1-m)β·W_B[next]·r_D_B
         + (1/η)·x_B·(1 − log(1−τ_B))
         + (1 − r_D_B)·(1+g)(1-m)β·W_B[next]

         = (1+g)(1-m)β·W_B[next] · (r_D_B + 1 − r_D_B)
         + (1/η)·x_B·(1 − log(1−τ_B))

         = (1+g)(1-m)β·W_B[next] + (1/η)·x_B·(1 − log(1−τ_B))
```

The r_D_B-weighted and (1−r_D_B)-weighted continuation terms collapse together.
What's left is a clean linear recurrence:

$$
\boxed{
W_B[a,t] \;=\; (1+g_a)(1-m_a)\beta \cdot W_B[a+1, t+1] \;+\; \tfrac{1}{\eta}\cdot x_B[a,t]\cdot\big(1 - \log(1-\tau_B[a,t])\big)
}
$$

### 2.3 W scales exactly as 1/η

The recurrence is linear in W. With terminal `W_B[A_max+1, ·] = 0`, iterating
back gives:

$$
W_B[a,t] = \tfrac{1}{\eta} \cdot \sum_{j=0}^{A_{\max}-a}\Big(\prod_{k=0}^{j-1} \mathrm{decay}(a+k, t+k)\Big) \cdot x_B[a+j, t+j] \cdot (1-\log(1-\tau_B[a+j, t+j]))
$$

where `decay = (1+g)(1-m)β`. The summand is **η-independent**. So
`W_B[a,t] = (1/η) · C_B(a,t)` for an η-independent function C_B.

By identical algebra, in Pass 2 with the same μ recovered from baseline:
`W_S[a,t] = (1/η) · C_S(a,t)`, with C_S η-independent.

### 2.4 r_D_S/r_D_B is η-independent

Using `r_D_S = r_D_B · ((1−τ_B)/(1−τ_S)) · exp(−η(P_S − P_B))`:

```
P_S − P_B = ((1+g)(1-m)β/(1−τ_S))·W_S − ((1+g)(1-m)β/(1−τ_B))·W_B
          − (1/η)(log(1−τ_S) − log(1−τ_B))

         = (1/η)·(1+g)(1-m)β·[C_S/(1−τ_S) − C_B/(1−τ_B)]
         + (1/η)·log((1−τ_B)/(1−τ_S))

         = (1/η)·D(a,t)         (D η-independent)
```

So:

```
η·(P_S − P_B) = D(a,t)
exp(−η(P_S − P_B)) = exp(−D(a,t))
```

**The η in the exponent is exactly canceled by the 1/η in W.** The
realization function reduces to:

```
r_D_S / r_D_B = (1−τ_B)/(1−τ_S) · exp(−D(a,t))
```

which is η-independent. The implied elasticity depends only on the (1−τ)
prefactor and the η-independent W shape — no calibration handle.

---

## 3. Why the spec's "clean cancellation" is the bug

PDF §4.5 motivates the `−(1/η) log(1−τ)` term in P as:

> "a tax-rate adjustment that arises because the holder values nontax
> benefits in after-tax dollars; including it is what allows the realization
> function to take the clean exponential form (2) after differencing
> scenario from baseline."

This correction is what makes b(x) collapse to `P·x + (1/η)x`, which in turn
makes W collapse to `(1/η)·C`. The "clean cancellation" the PDF presents as
a feature is precisely what nukes η.

The prior closed-form bracket spec did **not** have this problem. There:

* `P = τ · (1 − bracket)` where bracket comes from a competing-risks PV of
  λ_T and m hazards — explicitly **η-independent**.
* `r_D_S / r_D_B = exp(−η(P_S − P_B))` — η enters once, cleanly, with no
  cancellation against P.

η was a free elasticity scalar. The bracket was a model-derived multiplier.
Two separate objects.

In the Bellman spec, η pulls double duty: it sets b's curvature **and**
scales the realization response. Recovering μ from the baseline FOC pegs
the level, and the curvature-scaling-the-level locks the response. The
two cancel.

---

## 4. Why changing the b form doesn't fix it

The natural impulse is to swap `b'(x) = μ − (1/η)log x` for something like
`b'(x) = μ − K log x` with K distinct from η. Then the FOC gives r_D in
terms of K, with η free.

But to preserve the baseline-anchored exponential form
`r_D_S = r_D_B · exp(−η(P_S − P_B))`, you'd need to pick K such that the
FOC produces the right ratio. The only K that works is K = 1/η — putting
us back where we started.

If we instead pick K independently and accept that the FOC-derived
exponential has rate 1/K (not η), we've effectively renamed η → 1/K and the
same algebra above gives `W = K · C`, leading to η = 1/K cancellation
again. The pathology is in the **structure** of substituting a log-form b
into a per-unit Bellman with FOC-recovered intercept, not in the choice
of constant.

A more invasive fix would require breaking the log form (e.g.,
`b'(x) = μ − K · x^{-1/η}`), but then the closed-form exponential ratio
`exp(−η(P_S − P_B))` no longer falls out of the FOC. The microfoundation
and the desired reduced-form parametrization become incompatible.

---

## 5. Paths forward

### 5a. Revert to the closed-form bracket P (keep (1+g))

Easiest. The prior `kg_dyn_compute_bracket` formulation has identifiable η.
We can keep the (1+g) extension to the continuation by adding the accrual
factor inside the bracket integrand (β^j s_j becomes ((1+g)β)^j s_j with
appropriate care). The "anticipation" channel is lost; we accept that as a
known approximation.

Cost: undo a lot of the Bellman scaffolding we just built. The
forward-looking τ path no longer materially affects today's decision.

### 5b. Keep the Bellman for W but decouple η from the realization function

* Use the Bellman to compute W with a **fixed** behavioral parameter
  (call it φ, ≠ η). Set φ from external evidence (e.g., the elasticity
  literature's average) and freeze it.
* Compute `P` from this W as in the spec.
* Use `r_D_S = r_D_B · exp(−η(P_S − P_B))` with η as the free scalar
  calibrated to the elasticity target.

Cost: μ no longer recovered from baseline FOC consistently; the model has
two parameters (φ for the value function, η for the response). The
"single mechanism" elegance the PDF advertises is lost. But η is
identifiable.

### 5c. Talk to John

Show him the derivation in §2 and ask whether (a) we're misreading the
spec, (b) the degeneracy is acknowledged but not flagged in the writeup,
or (c) he intended a fix we've missed.

This is the right first step before committing to 5a or 5b.

---

## 6. What's intact in the current code

The implementation in `src/sim/kg_dynamics.R` is **mechanically correct**
against the PDF spec:

* Life-table extension, extended grid, growth-rate machinery all work.
* Bellman backward induction runs cleanly; W matrices populate as
  expected.
* Bathtub recurrence on `[18,80]` runs end to end; per-year state files
  persist normally; the smoke runscript completed Phases 0–3a without
  issue. (Phase 3b failed on an unrelated `group_iqr` bug in
  `src/data/post_processing/horizontal.R`, pre-existing on the `wealth`
  branch.)

The work is salvageable. We need to decide where η lives.
