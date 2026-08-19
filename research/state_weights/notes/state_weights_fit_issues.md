# State weights — issues from the first full-scale fit (2026-07-13)

Status notes from the first end-to-end run of the Phase 1 pipeline on real
data: 2022 PUF (220,897 records; agi/eitc joined from a full-sample baseline
run) + IRS-GEO HT2 + ACS 2022, assembled by `build_weight_inputs()` and
fitted with `fit_calibration()` via
`research/state_weights/scripts/build_state_weights.R`. Commit under test:
`55ee03ec2`.

## What worked

- The full pipeline runs in ~4.5 minutes end to end and writes an 11.6M-row
  `state_weights_2022.csv`.
- Target assembly produced 10,759 filer targets (22 series × 10 stubs × 52
  areas, with shared row/x vectors) and 1,390 non-filer targets (35 ACS
  cells × states), with the share-normalization design (targets = PUF
  national totals distributed by HT2/ACS state shares) making every target
  feasible in principle.
- **Non-filer partition fit exactly**: 100% of targets within 2%, MARD 0.
  With single-membership cells and count-only targets, IPF is exact in one
  pass — as designed.
- Fitted state shares are sane (CA 15.0%, TX 9.6%, FL 6.9%, NY 6.2% of
  national weight), i.e., real geographic signal vs the uniform placeholder.

## Issue 1 — `fit_calibration()` does not converge on the filer partition

**Symptoms** (from the run log):

- Only **9.1% of filer targets within 2%** (TPC benchmark: ≥99%); median
  relative error 25.7%, MARD 41.7%.
- The convergence metric is **frozen**: `max|f-1| = 157.3475` at iterations
  10 through 50, bit-identical. Some target receives the same ~158×
  multiplier every iteration; the end-of-iteration row renormalization
  undoes exactly what the rescale did, and the loop cycles without
  progress.

**Leading hypothesis (unverified)**: mixed-sign x-vectors. Amount series can
be negative for records inside positive-AGI stubs — `kg_amt` (capital
losses up to −$3,000) is the prime suspect, and `agi_amt` mixes signs in
stub 1. A state's predicted total `Σ w·P·x` can then sit arbitrarily close
to zero, making `f = target/That` explode; multiplicative IPF assumes
nonnegative x. The engine's only guard (`That > 0`) silently skips
negative-That targets (so stub-1 `agi_amt` never binds) but does nothing
for near-zero or sign-flipping cells.

**Contributing factor**: even for well-behaved targets, one unclamped
pathological rescale per iteration distorts the shared rows for all other
targets on those records, and the renormalization spreads the distortion
across states — plausibly why the overall hit rate is so poor rather than
just one bad series.

**Planned fixes** (engine hardening, in order):
1. Damped IPF: clamp the per-iteration factor to a band (e.g. [1/2, 2]) so
   no single target can blow up a pass.
2. Denominator floor: skip a target in an iteration when |That| is tiny
   relative to |target| (record it as unfittable rather than oscillating).
3. Mixed-sign policy: for amount series with sign-mixed x within a stub,
   either calibrate on the positive part only or drop the amount target for
   that stub (keep the count target), documented per series.
4. Re-run and report the per-series hit-rate table so residual misses are
   attributable.

## Issue 2 — split-weight invariant shows a 5.4% max row error

**Symptom**: `max |Σ_st w_ist − w_i| / w_i = 5.38e-02` over records. The
invariant is supposed to hold **by construction** (the engine renormalizes
row sums to 1 every iteration, and W = w·P), so any nonzero error beyond
float noise means a defect, not a tolerance question.

**Hypotheses (unverified, to be root-caused before any fix)**:
- A zero row-sum somewhere in `P` (e.g., a record whose prior row is all
  zeros, or driven to zero by the runaway rescale of Issue 1) turning the
  renormalization into 0/0 = NaN, with NaN weights then dropped by the
  `weight > 0` filter in `build_split_weights()` — silently removing mass
  from those rows.
- Less likely: an indexing defect in the diagnostics' dcast/merge rather
  than in the weights themselves. The check should be recomputed directly
  on the long table before trusting either conclusion.

**Resolution bar**: the invariant must come back exact (float epsilon). If
records genuinely cannot be assigned (empty priors), they must keep their
national weight under the prior rather than losing mass.

## Also observed (not defects)

- 331 ACS units dropped for unresolved heads (known, logged, negligible).
- The v0 ACS filing model yields 148.2M filer units vs HT2's 159.7M actual
  2022 returns (~7% short) — the known Phase 1 tuning item, unrelated to the
  engine issues above. **Superseded as the reconciliation basis
  (2026-07-13)**: per JI, reconciliation now runs at the individual level
  (`compare_individuals_acs_irs()`), which is model-free. 2022: married
  adults 112.6M IRS / 131.6M ACS (85.6%), single adults 100.5M / 129.5M
  (77.6%), children/dependents 78.9M / 72.2M (109.2%, expected sign from
  the documented construction gap); state coverage spreads 74.5–95.3%
  (married) and 64.5–86.3% (single) — a strong geographic gradient
  confirming non-filer geography needs its own targeting.
- `fit_gradient()` has not yet been run at full scale; its per-target loop
  (10.8k targets × 500 steps) will need vectorization (group targets by
  stub into matrix ops) before the A/B bake-off is practical.

## Next steps

1. Root-cause Issue 2 first (it may be a downstream casualty of Issue 1's
   runaway factor, but must be proven, not assumed).
2. Engine hardening per Issue 1; re-run; require: exact invariant, filer
   hit rate reported per series, runtime.
3. Vectorize `fit_gradient()` and run the A/B comparison harness
   (`research/archive/state_weights_ml_alternative.md` §4) once A is stable.

---

## Resolution (2026-07-13, same day)

**Issue 2 — CLOSED.** Root cause proven by direct diagnosis: the engine's
renormalized rows always summed to exactly 1 (no NaN/zero rows; prior had
zero dead cells). The leak came from **negative targets**: several HT2
cells are net-negative (`kg_amt` in low stubs for AZ/MA/NV/RI; `agi_amt`
stub 1), and `f = target/That < 0` flipped whole (stub, state) columns of P
negative — negative weights were then silently dropped by the `weight > 0`
filter. Fixes: assembly now blocks non-positive targets and excludes
`kg_amt` from the calibration set (sign-mixed x; `n_kg` retained; the
gradient engine can reclaim it — no positivity constraint); targets carry
(series, stub, state) metadata; `build_split_weights()` asserts
`P >= 0` and exact row sums. **Verified: invariant max error 1.1e-15.**
The same negative factors also explain the poisoned zero-That columns, and
the frozen ×157/×533 factors were OA/PR extreme cells (now damped:
`f_max = 2` clamp + denominator floor + unfittable reporting).

**Issue 1 — root-caused as STRUCTURAL, not a bug.** With the pathologies
removed, the hardened engine still plateaus (9.8% within 2%, uniform
~30-40% MARD across broad-support series, while concentrated-support series
fit well: `eitc_amt` 93%, `mort_int_amt` 56%). Diagnosis: ~21 series impose
constraints on the SAME (stub × state) cell, and a per-cell multiplier
cannot satisfy 21 different factors — sequential per-target IPF is valid
only with one constraint per cell. **Proof**: restricted to `n_returns`
(one constraint per cell), the identical engine converges in ONE iteration
to 100% within 2%, MARD 0.000%.

**Architectural conclusion.** The correct classical estimator for
multi-constraint calibration is Deville–Särndal raking / exponential
tilting — `P[i,st] ∝ P0[i,st]·exp(Σ_v λ_{st,v} x_iv)` — which uses
record-level x-heterogeneity to absorb cross-series differences, and is
mathematically the same family as engine B's softmax objective. The
bake-off is therefore reframed: **count-backbone IPF (valid, exact, fast)
builds the prior; the vectorized gradient engine performs the joint
multi-series fit.** "A vs B" becomes prior-only vs joint-fit, which is the
comparison that was always economically meaningful.

Remaining work: vectorize `fit_gradient()` (group targets by (stub, series)
into matrix ops) and run the full harness.

## Step 5 result (2026-07-13): vectorized joint fit validates the architecture

`fit_gradient()` vectorized: targets sharing a rows set (all series within a
stub) stack into per-group X/T/λ matrices, so each Adam step is ~10 GEMMs
instead of ~10,700 scatter updates. Gradient verified against finite
differences (1e-10) on both the singleton and grouped paths.

Full 2022 joint fit — counts-backbone IPF prior → 300 Adam steps
(β = 1e-3, lr = 0.1), run under sbatch (48G; the login node OOM-kills at
~7 GB RSS — compute jobs are the documented route):

- **within 2%: 82.9% | MARD 1.43%** (vs 9.8% / 37.5% for multi-series IPF
  on identical targets), runtime 7.5 min, exact row sums.
- Loss still descending at exit (fit loss 460 → 37); residual misses
  concentrate in SALT/real-estate-tax cells (61–67% within 2%) and the
  EITC/AGI families (73–77%). Closing the gap to the ≥99% acceptance bar
  is an optimization-budget matter — more steps, lr schedule, per-series
  λ up-weighting — i.e., harness tuning, not architecture.

Status: engines DONE. Next: the §4 comparison harness (β/lr sweep,
untargeted validation incl. the QWI/ACS demographic cells, downstream
pilot-state liability), then the weights writer swap-in.
