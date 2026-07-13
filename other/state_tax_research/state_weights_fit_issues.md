# State weights — issues from the first full-scale fit (2026-07-13)

Status notes from the first end-to-end run of the Phase 1 pipeline on real
data: 2022 PUF (220,897 records; agi/eitc joined from a full-sample baseline
run) + IRS-GEO HT2 + ACS 2022, assembled by `build_weight_inputs()` and
fitted with `fit_calibration()` via
`other/state_tax_research/build_state_weights.R`. Commit under test:
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
  2022 returns (~7% short) — the known Phase 1 tuning item for the
  reconciliation check, unrelated to the engine issues above.
- `fit_gradient()` has not yet been run at full scale; its per-target loop
  (10.8k targets × 500 steps) will need vectorization (group targets by
  stub into matrix ops) before the A/B bake-off is practical.

## Next steps

1. Root-cause Issue 2 first (it may be a downstream casualty of Issue 1's
   runaway factor, but must be proven, not assumed).
2. Engine hardening per Issue 1; re-run; require: exact invariant, filer
   hit rate reported per series, runtime.
3. Vectorize `fit_gradient()` and run the A/B comparison harness
   (`state_weights_ml_alternative.md` §4) once A is stable.
