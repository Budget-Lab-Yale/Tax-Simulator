---
title: "State weights — alternative approaches (brainstorm, 2026-07-13)"
role: notes
workstream: state_weights
status: deferred
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# State weights — alternative approaches (brainstorm, 2026-07-13)

Prompted by JI: (1) we want to capture negative values (capital losses,
business losses) rather than exclude them; (2) what other ML approaches fit
this problem? Context: the joint-fit architecture works (82.9% within 2%
and descending, `research/state_weights/notes/state_weights_fit_issues.md`); this note is about what
could work better or add capability.

**Framing.** We are learning a 53-class posterior `p(state | record)` for
~207k records with NO labeled examples — only ~10k aggregate moment
constraints (HT2 cells) plus cell margins (ACS). In ML terms this is
**learning from label proportions** (LLP) / ecological inference. That
framing unlocks a specific literature and, more importantly, identifies the
dual-space formulation below as the canonical solution.

---

## 1. Negative values (capital gains/losses, business losses)

Current state: `kg_amt` (and stub-1 `agi_amt`) are excluded from
calibration because multiplicative IPF cannot chase negative totals.
Options to reclaim the signal:

**(a) Signed series in the tilting/gradient family — RECOMMENDED.** Nothing
in the joint fit requires `x >= 0` or `T >= 0`: predicted moments
`Σ w·P·x` can be negative while P stays positive, and the loss/gradient are
well-defined. Hazard: near-zero targets (gains ≈ losses in a cell) blow up
relative errors — use a robust denominator `max(|T|, τ·national_scale)`.
Implementation: an assembly flag (`calib_safe = FALSE`) that keeps signed
series out of the IPF count backbone but in the joint fit; applies to
`kg_amt`, stub-1 `agi_amt`, and future business-income series (`A00900` is
also net-of-loss).

**(b) Sign-split decomposition as a derived positive target.** HT2 only
publishes NET gains, but AGI losses are capped at $3,000/return, so
state-level losses ≈ 3000 × loss-return count. With the loss-return share
per stub estimated from the PUF (weight-independent to first order),
`gains(state) ≈ A01000(state) + 3000·share·N` yields a positive target
usable even in IPF. An assumption — use as a cross-check on (a), not
instead of it.

**(c) Status quo** (net series as untargeted validation only): acceptable
but discards real geography — capital-gains concentration (FL/NV/WY) is
precisely what state weights should capture.

## 2. Alternative / additional ML approaches

**(i) Dual-space maximum entropy (generalized raking solved in the dual) —
the standout.** Instead of 11M primal logits, solve for the Lagrange
multipliers of the moment constraints:
`P[i,s] ∝ P0[i,s]·exp(Σ_v λ_{s,v}·x_iv)` — ~12k dual parameters, CONVEX,
deterministic (no Adam, no seed), L-BFGS/Newton in seconds-to-minutes.
This is Deville–Särndal calibration / Hainmueller entropy balancing done
properly, and `e^{λx}` is sign-indifferent — **it answers both prompts at
once**. Engine B's primal freedom (per-record deviations beyond what
features explain) becomes an optional residual layer on top. Natural next
engine implementation; the existing grouped X matrices are exactly the
dual's sufficient statistics.

**(ii) Record-level priors via statistical matching to the ACS.** Replace
stub-share priors with each PUF record's empirical state distribution from
kNN/model-matched ACS donors (shared covariates: wages, age, marital
structure, dependents, SS/pension receipt). Injects covariate-driven
geography (retirement migration, wage structure) before calibration —
targets the cells the fit currently misses. The ACS extracts are already
wired in.

**(iii) Amortized classifier (spec §2.3 B-amortized, upgraded).** Fit
`f_φ(x)` — multinomial logistic (= the dual model), gradient-boosted trees,
or a small MLP trained with the bag-loss gradients we already compute — so
weights GENERALIZE across years: score 2023+ without re-solving; joint
multi-year fit with shared `f_φ` + year-specific tilts gives projection
stability.

**(iv) Uncertainty quantification via replicate weights.** Bootstrap the
target system (HT2 cells within disclosure-rounding bounds; ACS sampling
error) → ~80 replicate weight sets → standard errors on state revenue
estimates. Survey-standard; no other state model publishes uncertainty.

**(v) Noted, lower priority.** Sinkhorn/optimal-transport formulations
(equivalent to (i) under entropy regularization); LLP-specific methods
(∝SVM, DLLP — our bag loss already is DLLP's); EM-style discrete state
imputation (TPC; dominated by keeping the soft posterior); synthetic-data
fusion (PolicyEngine enhanced-microdata paradigm — a different project,
not a weights method).

## Suggested sequencing

1. Implement the dual maxent engine (i) with signed-series support (1a) —
   candidate to replace the primal gradient fit as the production method;
   compare on the §4 harness alongside prior-only IPF.
2. ACS-donor priors (ii) as the next signal injection; re-run the harness.
3. Amortized multi-year fit (iii) when projection-year weights matter.
4. Replicate-weight UQ (iv) once the production method is chosen.
