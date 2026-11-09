# η calibration upgrade: response-surface pinning + automated staleness watch

*2026-07-12, author-approved direction (items 1 and 2 of the calibration
discussion; items 3 [large-shock curvature moment] and 4 [published η band]
were considered and declined). Companion facts: the estate-margins build
measured a +3.7% η drift (re-pin target ≈ 2.4901), and σ went stale twice in
one week without anyone noticing until a protocol re-run tripped over it.*

---

## 1. Pin η on the full simulator directly (retire the dilution factor)

**Problem.** `calibrate.R` bisects η on a miniature (cell-level Bellman +
bathtub, no per-record tax calculator), then bridges to the real model with
hand-carried dilution constants (`KG_DYN_DILUTION_LONG/SHORT`) that must be
re-measured every iteration and are still flagged in the file as v2 priors.
The short-run dilution was already too unstable to use — the timeable share
had to be pinned by full-sim root-find. η should follow.

**What already exists** (`other/top_tax/eta_dial/`, in-flight):

- Four full-sim vintages at η ∈ {1.2, 2.3992, 3.6, 4.8} (30-year, full sample).
- `measure_efull_by_eta.R` — one consistent full-sim elasticity convention:
  `E_full(η) = log(R_shock/R_base) / dτ_rw` at sim-year 30, shock = `s_cg_r25`,
  conv-no-wealth leg. Output `efull_by_eta.csv`.
- The measured surface is nearly **linear in η** (−1.33, −2.58, −3.86, −5.16),
  exactly what the single-pool entropy model predicts — so 3–4 points pin the
  curve tightly and inversion is trivial.
- `fit_eta_dial.py` — exponential fit + leave-out-central validation
  (predicts the central vintage to ~1–3% of static; good enough).

**What remains to make this THE calibration path:**

1. **Re-measure the grid on current code.** The four existing vintages are
   pre-estate-offset. Re-run 2–3 grid points (central + one bracket each
   side is enough given linearity) with the estate offset live. Expect the
   whole surface to flatten ~3.7% (the `eta_estate_check` drift).
2. **State the target on the E_full convention** and invert the fitted line
   for η*. The nominal literature target is −0.62/0.238 = −2.52; the current
   central measures −2.58 on this convention pre-offset, so the re-pin should
   land near the internal-moment prediction of ≈ 2.49.
3. **Ship it**: update `KG_DYN_DEFAULT_ETA`, run the kg regression suite +
   one A/B, flag pre-repin kg vintages non-comparable (spec-v3 migration
   protocol). Record the fitted line's coefficients next to the constant so
   the NEXT re-pin is arithmetic, not archaeology.
4. **Demote `calibrate.R`** to a fast diagnostic (the internal moment is
   still the right cheap drift detector — see §2); delete the dilution
   constants and their re-measurement loop from the calibration story.

Cost: 2–3 pipeline runs (~1 hr each on current cluster throughput), once.
Marginal cost of every future re-pin: 1–2 grid-point refreshes.

---

## 2. Automated staleness watch (the thing that would have caught σ)

**Problem.** The calibration constants are residuals conditional on the
behavior stack, and the re-derivation rules live in comments humans don't
re-read. Two σ staleness triggers fired this week (entity-shifting τ_eq
repricing, evasion cross-base fix) and σ sat 2× wrong until an unrelated
protocol run exposed it.

**Design — one sbatch, two tiers, fail loud:**

- **Tier A (cheap, run on every watch tick): internal-moment drift check.**
  Generalize `eta_estate_check.R`: recompute the calibrator's long-run
  internal moment at the SHIPPED η, with the full current structure threaded
  (estate e, wealth-carry h when present), against a checked-in reference
  value (`calibration_reference.csv`: moment name, value, date, code SHA).
  Alert on >1% drift. Runs in minutes on an existing ≥30-yr baseline.
- **Tier B (expensive, run only when triggered): σ leg re-derivation.**
  Trigger = any commit touching the σ staleness list
  (`pearce_prisinzano.R`, `debacker.R`, charity elasticity, the pool/gate in
  `sigma_conversion.R`, Tax-Data vintage) since the last recorded derivation
  SHA. Action = submit the two `sigma_recal_eta` ETI legs + solve σ*
  (the exact pipeline scripted this week:
  `sigma_recal_estate_eti.sbatch` pattern). Alert if |σ* − shipped| > 0.01.
- **Wiring:** a weekly cron (or `/schedule` routine) that (i) diffs HEAD
  against the SHAs recorded in `calibration_reference.csv`, (ii) runs Tier A
  always, Tier B on trigger, (iii) writes a one-line PASS/DRIFT report and
  exits nonzero on drift so the failure is visible. No auto-repinning —
  the watch DETECTS; humans re-derive and sanction, exactly as this week.

**Reference file to create** (single source of truth the watch reads):

| constant | shipped | reference moment | derived | code SHA |
|---|---|---|---|---|
| KG_DYN_DEFAULT_ETA | 2.3992 (re-pin → ≈2.49 pending) | internal long-run semi | 2026-07-09 | a4bbac590 |
| KG_DYN_TIMEABLE_SHARE | 0.2542 | full-sim short-run semi | 2026-07-09 | a4bbac590 |
| SIGMA_CONV | 0.16 | top-ordinary ETI 0.25 | 2026-07-12 | (estate build) |

Cost: Tier A ≈ 5 min compute/week; Tier B ≈ 2 pipeline runs, only when the
staleness list actually changes — i.e. exactly when it's needed.

---

**Suggested order:** finish §1 first (it re-pins η and produces the fitted
line), then stand up §2 with the re-pinned values as the reference row —
starting the watch from a known-clean state.
