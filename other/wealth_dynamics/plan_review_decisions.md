# Wealth Bathtub plan review — decision log

Walking through every open item from the 2026-06-24 adversarial review of
`purrfect-weaving-toucan.md`. One decision at a time. This file is updated as we go
so disconnects don't lose progress.

Status legend: ⬜ not yet discussed · 🔵 in progress · ✅ decided · ⏭️ deferred to v2

## Order of march

### Tier 1 — moves the headline number
- ✅ D1  F5   — mortality double-count in drain-at-death (convention for P)
- ✅ D2  F24B — wealth-tax↔capital-income interaction: BUILD IN V1
- ✅ D3  F32  — r_total = per-year data path; sweep = opt-in one-time knob
- ✅ D4  F1   — relabel headline; closed-form-from-actual-params test

### Tier 2 — build blockers / engineering decisions
- ✅ D5  F21  — pre-pass reads from detail; add economic_gross (1 col, non-baseline)
- ✅ D6  F15  — WEALTH_CAP_FLOWS pinned (economy.R raw list, flat 0.2; +1250/collect; kg_lt_basis)
- ✅ D7  F16/F17 — clamp y≥0; co-bump earner splits; loss-net pairs; |F|≥ε·gross
- ✅ D8  F38  — inherit kg refusals (VAT/excess-growth/pct_sample≠1 hard-stop)
- ✅ D9  F37  — pre-pass afterok on BOTH 2A and Phase 1 (+coverage check)
- ✅ D10 F23/F35/F40 — detail-source contract; N_PHASE2W counter+guard; no 3b change

### Tier 3 — spec/math exposition (recurrence right, prose wrong)
- ✅ D11 F2   — convolution = intuition only; recurrence is production path; pin conventions
- ✅ D12 F7   — build M abstractly (any DS matrix); prose+Sinkhorn+mass-cons test under M≠I
- ✅ D13 F13  — y=F/gross; cell-aggregate ratio ΣF/Σgross; fix labels + placeholders table
- ✅ D14 F12  — stopifnot(0<G<1+r) + defensive ratio handling on cell yield
- ✅ D15 F8   — RESOLVED w/ D12: memoryless = uniform 1/100 matrix (auto doubly-stochastic)

### Tier 4 — cells / allocation / calibration
- ✅ D16 F28  — adopt kg key: if_else(fs==2, pmax(age1,age2), age1), pre-topcode
- ✅ D17 F29  — exclude neg/zero-NW (s_eff=0); floored denom+guard; f-guard; conservation check
- ✅ D18 F33  — document symmetric s as v1 assumption; s_hike/s_cut config knob (default equal)
- ✅ D19 F30  — reword top-bin prose only (concentration intended); no diagnostic; m N/A (D1)

### Tier 5 — estate / distribution / kg coupling
- ✅ D20 F25  — revenue-only reporting + reconciliation identity (distribution=static world)
- ✅ D21 F26  — accept+document ~f̄≈1% bias; verification MEASURES residual (exact fix=v2)
- ✅ D22 F11  — CONVENTIONAL ΔT⁰ (wealth-excluding); adds conv-no-wealth pass
- ✅ D23 F27/F43/F18 — gift add-back erodes; deduction consistency check; τ on conv-no-wealth frame; pr=F SECA doc

### Tier 6 — lower priority (mostly document-and-bound)
- ✅ D24 F4   — state end-of-year timing convention + unit test
- ✅ D25 F3   — 80+ note: recurrence handles self-loop; closed form exact only age<80
- ✅ D26 F14  — large-cut τ curvature caveat (cumulative+direction-aware); optional 1-time recheck
- ✅ D27 F19  — RESOLVED by D12/D17; add Σ D_alloc = P per-cell sentence
- ✅ D28 F31  — clamp-bind frequency/max|f| diagnostic; state spillover convention
- ✅ D29 F39  — DISSOLVED by ARCH redesign (wealth is a fixed pre-step, not a listed module)
- ✅ D30 F44  — mixed-sign superposition unit test (f bounded; +/− single-sign sum)
- ✅ D31 F45  — net_worth schema note + s=0 parity assertion (recompute already decided in D2)
- ✅ D32 F46  — hoist FY death+1 caveat to verification preamble; state measurement basis

### Tier 7 — caught in reconciliation (missed in walkthrough)
- ✅ D33 F34  — WEALTH_DYN_PROVENANCE staleness guard (mirror kg_dyn_calib_provenance)

## Decisions

### ⭐ ARCHITECTURE (decided mid-D20) — MECHANICAL CHANNEL, NOT A BEHAVIOR MODULE
The wealth bathtub is a MECHANICAL conventional-side step, not a behavior module.
- Drop config/scenarios/behavior/wealth_dynamics/financing.R + do_wealth_dynamics() hook.
- The haircut applier is called DIRECTLY as a built-in step in the conventional pass
  (before the behavior modules / before do_taxes on the conventional frame).
- `s` becomes a RUNSCRIPT COLUMN (per-scenario scalar; flat ages, symmetric in v1).
  s>0 → channel active (pre-pass + applier); absent/0 → dormant (byte-identical).
- scenario_uses_wealth_dynamics() keys off s>0 (NOT the behavior column).
- STILL conventional-only: static stays the clean law-only baseline (the measurement
  contrast). "Mechanical" = deterministic + always-applied-on-conventional, NOT static.
- Ripples: D39 dissolves (wealth runs before behavior modules structurally; kg operates
  on the haircut frame; no token-order guard). D18 age-shape/direction become dormant
  config hooks (runscript s is the only knob). config_parser parses s; SLURM Phase 2W
  gates on s>0; worker.R calls the applier as a fixed conventional step, not a module.
- kg composition: runscript sets behavior=kg_dynamics AND s=0.5 → wealth haircut first
  (mechanical), then kg behavior module on the haircut frame.

### D20 (F25) — DECIDED: revenue-only reporting (Option A)
- Conventional-only haircut (per ARCH). Distribution tables show the static (un-eroded)
  incidence; erosion reported via RECEIPTS. Rationale: haircut ~1% of wealth barely moves
  rank-order, so by-wealth distribution ~unchanged.
- Add reconciliation identity: conv estate receipts = static + Σ(conv−static).
- Pin the CLAUDE.md Σw·p·λ heir identity to STATIC totals/estate.csv post-feature.
- Distributional incidence of erosion → v2 (with heir routing). Ties to D1 heir note.

### D21 (F26) — DECIDED: accept + document + measure (Option A)
- Replace plan lines 108/341 "consistency confirmed in verification" with explicit signed
  bias: kg lock-in UNDER-allocates by ~cell-avg haircut f̄≈1% (haircut numerators / raw
  R_B,G_B denoms); deemed leg taxes a haircut per-record gain at an un-haircut
  deemed_factor. Both O(f̄), bounded, accepted v1. Rate channel (r_S/r_B) is exact.
- Rewrite kg-composition verification (plan 339-342) to MEASURE residual bias vs a known
  target, not assert zero mismatch. Exact applier correction (rescale by (1−f̄)) → v2.

### D22 (F11) — DECIDED: CONVENTIONAL ΔT⁰ (Option A)
- ΔT⁰ = (conventional-with-behavior-but-WEALTH-EXCLUDING scenario) − baseline. Economically
  correct: finance the tax ACTUALLY paid after behavioral realization response. Static
  overstates the CG headline ~1.9× (memory: $858B static vs $453B conv, 5pp gains hike).
- τ-frame sub-claim of F11 was debunked: τ is a marginal RATE entering only the small
  feedback term, so the 1.9× base gap does NOT apply to it (add one clarifying sentence).
- PIPELINE RIPPLE (update plan + SLURM D9/D10 + source D5): adds a conv-no-wealth pass.
  Chain: static(2A) → kg pre-pass(2B) → conv-no-wealth(ΔT⁰) → wealth pre-pass(2W) →
  final conv-with-wealth(2C). Phase 2W depends on conv-no-wealth scenario pass + baseline
  (Phase 1), NOT static 2A. Wealth pre-pass reads ΔT⁰ from conv-no-wealth detail.
  ~2× conventional compute for wealth+kg scenarios (accepted for accuracy).
- Resolve the 3 contradictory plan positions (lines 73 open-axis / 353 static-accepted /
  memory conventional) → all say CONVENTIONAL.

### D23 (F27/F43/F18) — DECIDED: batch
- F27 gift add-back (γ·reported_gross): ERODES with haircut (reduced-form proxy, not a
  sunk-gift ledger); document it over-shrinks base by ~γ·f·reported if gifts deemed sunk.
- F27 estate_income_tax_ded reprices on haircut frame: 3rd-order; ADD a consistency check
  to the kg-composition verification (currently unchecked).
- F27 joint both-die mistiming: already fixed by D16 (max(age1,age2)).
- F43 τ frame: compute mtr_cap_bundle on the SAME frame as ΔT⁰ (conv-no-wealth, per D22);
  do NOT add the death-time deemed margin to τ (double-count). Low-stakes (τ only feedback).
- F18 pr=F: drop the unqualified "capital income is not wage-based"; document that the
  pass-through 0.2 slice's SECA response is omitted (<1%, top-tail SE above OASDI cap).

### D24–D32 (Tier 6) — DECIDED: batch (document/test/convention)
- D24 (F4): state end-of-year inflow timing (carry grows by G; arrival-year inflow at face
  value); unit test one-time ΔT⁰ → P=s·ΔT⁰ that year, G·s·ΔT⁰ next.
- D25 (F3): note closed-form age-pair kernel exact only for age<80; 80+ self-loop handled
  by the sequential recurrence (already the production path per D11/D3).
- D26 (F14): document large-cut τ-curvature caveat (cumulative + direction-aware, ~1-2%,
  τ only in feedback); optional ONE-TIME re-measure of τ at eroded composition (not standing).
- D27 (F19): RESOLVED by D12 (mass-cons invariant) + D17 (Σ D_alloc=P check). Add one
  sentence: cell-ΣNW denominator on the same cross-section the haircut applies to.
- D28 (F31): add clamp-bind frequency + max|f| diagnostic to one-time long-horizon check;
  state spillover convention (redistribute unabsorbed deficit in-cell OR document dropped +
  rely on ≈ conservation identity).
- D29 (F39): DISSOLVED by ARCH (wealth = fixed pre-step, not a listed behavior module).
- D30 (F44): add mixed-sign superposition unit test (sign-flipping ΔT⁰ = sum of ± single-
  sign convolutions; f stays bounded). Toy-cell test, no design change.
- D31 (F45): schema note (CLAUDE.md + config_parser) that conventional net_worth is
  post-haircut while static is un-haircut; parity assertion conv==static when s=0. (Applier
  recompute of net_worth already decided in D2.)
- D32 (F46): hoist "run one year past window (estate/wealth FY death+1 lagged)" to a
  Verification preamble governing all estate-touching checks; state measurement basis
  (FY-receipts checks need +1yr; CY/detail-based checks don't).

### D33 (F34) — DECIDED: provenance staleness guard
- WEALTH_DYN_PROVENANCE = structured stamp (macro_vintage r_total derived from, the r_total
  path, s, fmax, spec_version). wealth_dyn_check_provenance(scenario_info) grepl-compares
  live Macro-Projections vintage vs stamp; warn, WEALTH_STRICT_CALIB=1 → stop. Called before
  every wealth pre-pass. Mirrors kg_dyn_check_calibration_provenance (kg_dynamics.R:216-299).
  Default macro vintage is pinned, so the guard catches per-runscript dep.Macro-Projections
  .vintage overrides. (Missed in the live walkthrough; included as recommended.)

## STATUS: all 33 decision points handled. Plan rewritten (purrfect-weaving-toucan.md, 2026-06-24).
## Next: update memory.

### D1 (F5) — DECIDED: Option A
P = deficit per still-living record. Drop (1−m) from the carry-forward; deaths handled
only via per-record estate_m at aggregation. Kernel line 122 → Π[G]. Make the
mass-conservation toy a REQUIRED numeric unit test.
Note (heir question): v1 does NOT propagate the smaller inheritance to heirs' capital;
heir routing is v2; repeated cross-section can't track lineages. Orthogonal to A-vs-B.
Revisit heir-side visibility at D20 (F25).

### D2 (F24B) — DECIDED: BUILD IN V1
Wealth tax participates as a first-order interaction. Implementation:
- Kernel: G = (1+r_total) − s·(τ·y + τ_w), where τ_w = marginal wealth-tax rate
  (native "tax per $ of wealth" units; NO divide-by-r needed; do NOT route through
  taxable yield y or you under-damp by y/r).
- Forcing: ΔT⁰ += Δliab_wealth (dollars, un-eroded base → stays exogenous, no new
  circularity / fixed point).
- Applier: recompute stored net_worth from scaled value.* (F24 Channel A; cf
  avoidance.R:90-92) so calc_wealth prices the eroded base.
- τ_w: statutory rate above exemption (flat) OR wealth-MTR for graduated schedules.
PREREQ (tracked): merge wealth-tax-base branch → wealth branch.

### D3 (F32) — DECIDED: per-year data path + opt-in sensitivity knob
- Central r_total(t) = nominal GDP/capita growth read per-year from Macro-Projections
  (4.00→3.74→3.57→~3.55), NOT the flat 2027 peak. Evaluate via sequential
  present/survivor recurrence (drops the time-invariant closed-form convolution; no
  real cost — calc stays year-parallel, and kernel isn't time-invariant anyway, see F2/D11).
- NO permanent multi-run sweep. Instead expose a config param in
  wealth_financing_params.yaml that deltas the assumed nominal-growth path
  (default = no change → production runs untouched/single). Use it for a ONE-TIME
  external sensitivity test after the build. Update cohort_wealth_growth.R to report
  the forward-path series, not the single 2027 ratio.

### D4 (F1) — DECIDED (delegated): relabel + closed-form test
- Prose: headline ~7%/~2% at realistic y≈0.025; "12%" labeled the y=r upper bound.
- Test: assert run matches the closed-form reduction from each cell's actual
  y, τ, τ_w, r(t); keep tax_feedback=OFF byte-for-byte check. (Not a number-mover;
  G stays default — strictly dominates plain one-shot at any y.)

### D5 (F21) — DECIDED: pre-pass single-source from detail (Option B, lean)
- Pre-pass reads everything from detail CSVs. Persist economic_gross as ONE derived
  column on NON-BASELINE static detail (+ any WEALTH_CAP_FLOWS cols not already in
  detail). Baseline detail untouched → dormancy preserved automatically.
- In-memory applier computes economic_gross = rowSums(asset cols) on the fly and
  scales the 14 value.*/basis.* directly (no detail dependency for those).
- Does NOT resolve VAT/excess-growth unit question (wealth cols are raw-dollar) → D8.

### D6 (F15) — DECIDED: pinned canonical list
- Pure-capital (w=1.0): txbl_int, exempt_int, div_ord, div_pref, kg_st, kg_lt,
  kg_1250, kg_collect (ADDED), + rent/rent_loss & estate/estate_loss net pairs.
- Scale kg_lt_basis (NOT the derived kg_lt_infl_adj).
- Pass-through slice (w=0.2): economy.R:287-289 RAW disaggregated list
  {sole_prop, part_active/passive/_loss/179, scorp_active/passive/_loss/179, farm},
  flat 0.2 partition.
- SAME list drives the MTR bump (τ) and the applier haircut (single source of truth).

### D7 (F16/F17) — DECIDED: clamp + 3 mechanical fixes
- F<0 (net-capital-loss) records: clamp y = max(F/gross, 0) → φ≥0; record stays in
  cell, contributes zero feedback.
- Earner-split: co-bump part_se1/2, sole_prop1/2, farm1/2 with their aggregates by
  reusing calc_mtrs's composite-expansion (so SECA/NIIT frame stays consistent).
- Loss columns: scale each loss-net pair (rent/rent_loss, estate/estate_loss) together;
  +F bump must raise taxable capital income.
- Floor: |F| ≥ ε·gross (fraction of gross, not absolute $).
- (record-level F/gross vs cell-aggregate yield robustness → folded into D13.)

### D8 (F38) — DECIDED: inherit kg refusal set
- New wealth_dyn_check_run_compat() mirrors kg_dyn_check_run_compat (run.R:863-920):
  hard-stop if pct_sample≠1, VAT active, or excess-growth active. Called at top of
  run_wealth_bathtub_pass() AND src/slurm/wealth.R worker. Rationale: raw-dollar
  wealth vs adjusted ΔT⁰ unit mismatch; 63×100 cells sparser than kg's 63 → subsample
  noise worst in top cells. v2 could lift this (Option B) by reading post-adjustment.

### D9+D10 (F37/F23/F35/F40) — DECIDED: SLURM wiring (all confirmed)
- D9: wealth pre-pass sbatch afterok on BOTH 2A (scenario static) and Phase 1
  (baseline) when has_baseline; skip P1 dep when baseline_vintage supplied. Add
  defensive column/year-coverage check on the baseline read (hard-error vs partial).
- D10a (F23): pre-pass reads from detail (per D5) — document the data-source contract.
- D10b (F35): setup.R emits N_PHASE2W=count; slurm_run.sh guards submit with
  if [ "$N_PHASE2W" -gt 0 ].
- D10c (F40): no aggregate.R Phase 3b change (no do_scenario rollup); state explicitly,
  add sync row only if a wealth summary is later added.

### D12+D15 (F7/F8) — DECIDED: abstract doubly-stochastic M
- Build recurrence to consume ANY 100x100 within-age percentile transition matrix M
  (identity=persistence now; uniform 1/100=memoryless; PSID later — all just inputs).
- General-M apply in percentile-index space IS the re-binning; equal-headcount
  percentiles ⇒ dollar mass conserved by construction (rows sum to 1, grows by G).
  No separate re-binning subsystem needed. D1 already removed mortality from recurrence.
- Residual: (1) fix "consistency automatic" prose → precise mass-conservation statement;
  (2) keep Sinkhorn rake to doubly-stochastic; (3) add mass-conservation unit test under
  a NON-identity M (uniform), not just M=I.
- F8: memoryless = uniform 1/100 matrix, automatically doubly-stochastic (valid input).

### D13 (F13) — DECIDED: y=F/gross, cell-aggregate ratio
- y_cell = (Σ_{i∈cell} F_i) / (Σ_{i∈cell} gross_i), clamped ≥0 (per D7). Robust to
  single-record blowups in sparse top cells.
- Confirm denominator = gross (NOT net worth); fix prose mislabels (lines 152/216/223)
  and placeholders row 7 ("÷ net worth" → "÷ gross assets"). Income feedback uses gross
  (F/gross); wealth-tax feedback (D2) uses net worth (τ_w·1) — each correct for its base.

### D11+D14 (F2/F12) — DECIDED: exposition + guard
- D11: relabel convolution section as intuition/parallelism only; production path is the
  sequential per-year recurrence (D3). Pin: G at destination age; arrival-year inflow
  factor = 1.0 (edges, t−τ G-factors). (1−m) already gone from recurrence via D1.
- D14: stopifnot(0 < G < 1+r) per cell after G formed; if_else(denom>0, ., 0) on the
  cell yield ratio. (φ≥0 from D7 + cell-aggregate from D13 make this safe in normal op.)

### D16 (F28) — DECIDED: kg cohort key
- age_cohort = if_else(filing_status==2, pmax(age1,age2), age1), applied before the 80+
  topcode, in BOTH pre-pass cell assignment and the applier. Matches kg_dynamics.R:404,
  distribution.R:173, the both-die estate_m, and prior design notes.

### D17 (F29) — DECIDED: exclude neg/zero-NW (s_eff=0)
- Ranking drops net_worth<0 (reuse distribution.R:443-444 <0→NA). Neg/zero-NW records
  are in no cell → their ΔT⁰ is excluded from the forcing = financed from consumption
  (s_eff=0; no positive wealth to draw, no taxable estate). Conservation holds.
- Cell denominator = Σ floored(NW,0); guard: if Σ ≤ ε → D_alloc=0 for cell.
- f-guard: economic_gross ≤ ε → f=0 (kills D_alloc/0 NaN).
- Verification: Σ_i w_i·D_alloc_i ≈ Σ_cell P per (year, leg).
- Soften the "clamp rarely binds" prose (it can bind for low-NW high-income payers).

### D18 (F33) — DECIDED: document symmetric; splittable knob
- Accept symmetric s as an explicit v1 simplification (locally-symmetric MPC). Make s a
  direction-splittable config param s_hike/s_cut in wealth_financing_params.yaml,
  default both = 0.5. Plan note: linear convolution mechanically enforces symmetry, so
  any hike/cut asymmetry REQUIRES a direction-conditional s (not just a magnitude change).
- Asymmetry = v2 swap path; one-time external test only if wanted (cf D3). No standing run.
