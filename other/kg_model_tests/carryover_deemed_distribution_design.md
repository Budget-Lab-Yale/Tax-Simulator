# Mechanical vs. behavioral decomposition of carryover & deemed realization, and getting the mechanical piece into distribution tables

_Design note, updated June 2026. The frozen mechanical pass (§4) is
**implemented and validated**; the distribution-layer heir reattribution and
the recalibration (§6) are outstanding. Supersedes the pre-implementation
draft of this document._

## 1. The problem (unchanged)

Distribution tables are a **static** object (`distribution.R` reads
`static/detail` for both baseline and reform), but deemed realization and
carryover basis were implemented entirely through the behavioral machinery
(`src/sim/kg_dynamics.R`, conventional pass only). Under a pure regime reform
the static reform detail was byte-identical to baseline → `liab_delta = 0` →
the distribution table showed nothing, and static revenue was ~$0 — even
though both policies have a genuinely mechanical component:

- **Carryover, mechanical:** heirs' realizations of inherited gains carry a
  lower (carried-over) basis, so the same sales produce more taxable gain,
  holding realization behavior fixed.
- **Deemed, mechanical:** the deemed tax on the baseline gain stock at death,
  holding inter-vivos realization at baseline.

## 2. Key conceptual findings (revised where marked)

2.1 **Deemed is a flow (death-year) event; carryover is a stock (realization)
event.** Unchanged.

2.2 **Deemed must not be allocated to anyone in the revenue machinery**
(taxed once on the decedent; basis steps up). Unchanged. **REVISED:** the
original draft claimed deemed needed "zero machinery change" — wrong. The
decedent's deemed tax existed only in *conventional* detail, which the
distribution layer never reads. The frozen pass (§4) is what carries deemed's
mechanical tax into static detail; under frozen realization it reduces to
exactly the mechanical definition (dG = 0, deemed_factor = 1, decedent term
only). One mechanism serves both regimes; only the heir *relabel* remains a
distribution-layer workstream (§6.1).

2.3 **Carryover is the only routed-stock concern.** Unchanged.

2.4 **The bathtub's blended dG commingles mechanical and behavioral;** a
frozen-realization pass (r_S ≡ r_B, regime on) isolates the mechanical part.
Implemented as specified. Empirical signature confirmed: mechanical routed
realizations exceed the conventional blend ($51.5B vs $38.0B of gains in
2035) because the behavioral response under carryover is an *unlock* that
drains the routed stock before heirs sell it.

2.5 **The age cross-section is already correct** via the aging matrix; only
the within-cell weight was at issue. Confirmed in output: the burden peaks in
the 50–64/65+ cells and ramps with the stock.

2.6 **No true decedent→heir linkage exists** in either model; heir targeting
is marginal matching. Unchanged.

2.7 **The within-cell allocation** was kg_lt-proportional (targets active
traders). **REVISED/RESOLVED:** now parameterized and defaulted to a 50-50
blend (§5).

## 3. What was implemented (June 2026, branch `wealth`)

### 3.1 Frozen mechanical pass

- `kg_dyn_run_frozen_pass` (`src/sim/kg_dynamics.R`): the bathtub recurrence
  with r_S ≡ r_B and the scenario regime mix; **no Bellman, no tau, no
  planned timing**. Writes per-year state
  (`static/supplemental/kg_dynamics_mech_state/{t}.rds`, same
  `list(regime, cell_table)` contract as the conventional state) plus
  `inputs_cache.rds` (the Tax-Data cell sweep), which the conventional
  bathtub reuses to avoid a second sweep.
- Step-up regimes are a structural no-op (`delta_route = delta_realize = 0`),
  so baseline-equivalent scenarios are unaffected.
- Orchestration: `run_frozen_pass` (`src/sim/run.R`). `do_scenario` is now a
  **4-step** for kg scenarios: frozen → static → bathtub → conventional.
  SLURM: new **Phase 1B** (`src/slurm/frozen.R`, manifest phase `'1B'`,
  one job per scenario, runs alongside Phase 1; Phase 2A depends on it).

### 3.2 Static-pass injection

In `run_one_year`'s static pass, kg scenarios apply the mechanical state to
records *before* `do_taxes` via `kg_dyn_apply_mech_to_records` — the same
applier the conventional behavior module uses, so the allocation rule and the
decedent RNG draw are shared and `conventional − static` decomposes record by
record. New static-detail columns:

- `kg_lockin` — the record's share of the cell's realized routed stock
  (pure carryover realization in the mechanical pass).
- `kg_deemed` — expected deemed death gains, `m_household × kg_deemed_full`
  (post-avoidance, §121-net).
- `liab_deemed` — expected tax on deemed gains. **REVISED (June 2026):** the
  original implementation drew a binary decedent flag from a per-record
  uniform (`r.behavior1`) fixed across years; with ~40% of expected death
  gains in the top 200 records, that put ±~50% persistent sampling error on
  deemed revenue (discovered comparing Tax-Data vintages: a −44% "change"
  was draw luck). Now computed as the exact decedent/survivor expectation
  without row duplication: `liab_deemed = m × [T(y + kg_deemed_full) −
  T(y)]`, where the dead leg is a second full-frame `do_taxes` with the full
  death gain on the return. Deemed gains never enter `kg_lt`, so MTRs and
  tau are pure inter-vivos margins (the deemed-decedent tau exclusion is
  gone); `liab_deemed` is folded into `liab_iit_net` after MTRs. Record-
  level nonlinearity (brackets, NIIT, §121/avoidance kinks) is fully
  preserved — no Jensen bias, no draw variance. Same two-leg recompute now
  also runs in the conventional pass. **Implementation constraint:** calc
  functions index `globals$random_numbers` positionally (e.g. the EITC
  pre-cert draw), so both legs must run on the full frame, never a row
  subset. Because deemed bypasses `kg_lt`, the fold must also reach the
  receipts inputs by hand: `liab_iit_net`, `liab_iit`, and
  `pmt_iit_nonwithheld` (receipts read `pmt_*`; `pmt_iit` itself is dropped
  by `remit_taxes`). Validation (vintages `kg_mech_50_frac` /
  `kg_td0609_frac`): 10yr deemed static $339.9B (Tax-Data 2026050315) vs
  $318.7B (2026060918) — the true cross-vintage effect is −6%, vs the
  binary draw's spurious −44% ($465B vs $260B); conventional $758B/$753B;
  smooth monotone year profiles; carryover byte-identical to binary;
  distribution ties, `iit_pr ≈ 0`, and heir profiles all pass. Tax-Data
  `default_vintage` incremented to `2026060918` on the strength of this.
  Pipeline bug hit along the way (pre-existing) and FIXED in
  `config_parser.R`: a `scenario_id` subset used to drop the baseline
  runscript row, leaving no `ID='baseline'` rows in
  `globals$interface_paths` and crashing `get_other_taxes()` in Phase 3b.
  The subset now always retains the baseline row (baseline *execution* is
  governed solely by `baseline_vintage` in `main.R`/`setup.R`, so nothing
  re-runs) and an unknown `scenario_id` fails with a clear message.

Static MTRs are computed **post-injection** (decision: accepted), with one
carve-out: records with `kg_deemed > 0` are excluded from the reform-side
cell-tau aggregation in `kg_dyn_load_bathtub_inputs` — a death-year spike
MTR is not an inter-vivos margin. Measured conventional drift from
post-injection tau: ≈0% (carryover R), −1.9% (carryover G), −0.04% (deemed).

Consequence (intended): static revenue for carryover/deemed reforms now
reports the mechanical estimate instead of ~$0, and distribution tables
populate. `build_horizontal_table` was made robust to reform static detail
whose `expanded_inc` differs from baseline (first time that's possible).

### 3.3 Within-cell allocation knob

`KG_DYN_APPLIER_ALLOCATION` (env var `KG_APPLIER_ALLOCATION`): `'R'` = 0
(kg_lt share, historical), `'G'` = 1 (G_unit share), or numeric α ∈ [0,1] —
`(1−α)·R_share + α·G_share`. Both component shares sum to 1 within an age
cell, so any blend does. The recurrence's death-channel `m_eff` stays on
`KG_DYN_DG_ALLOCATION = 'G'` deliberately: whose dollars *die* follows
holdings; the knob controls whose dollars get *sold*.

**Default: 0.5** (adopted June 2026). Rationale: the realized carryover
dollar needs a holder (G) who sells (R); R alone over-concentrates on active
traders, G alone over-attributes to never-inheriting self-made holders.

## 4. Validation results (vintages `kg_mech_R` / `kg_mech_G` / `kg_mech_50`,
runscript `tests/kg_alloc_compare`, baseline `sec121_carryover`)

All invariants passed:

- Carryover year-1 `kg_lockin` exactly $0 (dG enters lagged).
- Aggregate `kg_lockin` identical across allocation arms (the knob moves
  who-within-cell, never the cell total).
- Deemed outputs **byte-identical** across arms (frozen deemed has no
  `extra_R`); deemed static = $465.4B (2025–35) in all three.
- Distribution `net_change` ties to the static revenue line to rounding.

Headline magnitudes (2025–35 window):

| | R | 0.5 | G |
|---|---|---|---|
| Carryover mechanical revenue | $55.9B | $48.0B | $40.4B |
| ETR on routed gains (2035) | ~20% | ~17% | ~15% |
| Top 0.1% share of 2035 carryover burden | 58% | 41% | 18% |

Deemed mechanical ($465B) is ~53% of deemed conventional ($877B); the
remainder is the behavioral unlock — a decomposition not previously
observable. The deemed distribution table currently shows **decedent
incidence** (98% in 65+ rows, 0.11% of units affected at ~$197k average) and
a known ranking artifact (modest-income retirees with large embedded gains
land in middle quintiles) — both expected pre-reattribution.

## 5. Known limitations we are explicitly accepting (unchanged from draft)

- Point-in-time incidence of an intertemporal policy is not fully resolvable.
- Realized vs. accrued income inconsistency: liabilities reflect injected
  gains; ranking/ATI denominators use baseline income. Worst for deemed
  pre-reattribution.
- Two SCF-derived heir models in series (omega age marginal; estate model for
  within-age targeting), not one joint model.
- `KG_DYN_DEEMED_AVOIDANCE` (0.25) remains an applier-only value discount,
  flagged for concordance with the estate model's pass-through discount.

## 6. Outstanding work

1. **Deemed heir reattribution in `distribution.R`.** ~~Strip `liab_deemed`
   from decedent records and reallocate revenue-neutrally to heirs via the
   existing `p_inheritance` copy-split.~~ **DONE (June 2026).** As built:
   `liab_deemed` is stripped from both legs' `liab_iit_pr` in all variants;
   the pooled total is reallocated to heir copies proportional to baseline
   `inheritance × weight` (flat tax per inheritance dollar — implicitly a
   uniform gain/estate ratio, since the estate model imputes no gains), and
   enters only the death-inclusive presentations, like the estate tax. No
   income is reattributed to heirs. The `taxes_included` variants were
   renamed `iit_pr_estate*` → `iit_pr_death*`. Consequences, validated on
   re-run `kg_mech_50` tables (originals saved as
   `distribution_pre_reattribution.csv`): the deemed `iit_pr` variant is now
   ~0 by construction (deemed lives only in death variants, mirroring estate
   tax); the 65+ decedent lump dissolved (98% → ~52% of 2035 burden) into an
   heir-age profile; the middle-quintile ranking artifact dissolved (Q3:
   $18.1B → $0.4B in 2035); top 1% now bears ~64% of the 2035 deemed burden;
   reattribution is exactly revenue-neutral and the death-variant total
   still ties to static revenue with the same pre-existing ~0.8% CY/FY
   rounding wedge. Fails loudly if deemed tax is present but the
   Estate-Tax-Distribution join is missing or produces NA `p_inheritance`.
   Carryover tables are byte-equivalent pre/post (no deemed). Verified the
   estate detail file ids match Tax-Data vintage `2026050315` exactly
   (220,896 = 220,896, zero mismatches).
2. **Cumulative inheritance-eligibility weight.** The better-grounded
   replacement for the 50-50 blend's G half: from the estate model's
   `p_inheritance` by age, build P(inherited at least once since policy
   start) per cohort — the eligibility screen for holding carried-over basis
   (a never-inheritor cannot owe carryover tax). It starts at the year-1 flow
   and broadens over time, matching the stock's composition; no extra
   accumulator needed in the recurrence (the original draft's flow/stock
   second accumulator is judged unnecessary — `extra_R` is zero in year 1
   anyway because dG enters lagged). Expected to pull concentration up from
   pure G (large inheritances are top-heavy).
3. **Recalibration.** psi / `KG_DYN_SHARE_PLANNED` / dilution factors are
   anchored to the historical `'R'` applier rule and pre-injection tau.
   Re-measure dilutions and re-run `other/kg_model_tests/calibrate.R` under
   the 0.5 default and post-injection tau. Measured drift is small
   (≤2% conventional), so this is hygiene, not a fire.
4. **Mechanical-pass diagnostics** (optional): extend `kg_dyn_build_summary`
   to also roll up the mech state files (currently conventional-only).

## 7. Implementation touchpoints (as built)

- `src/sim/kg_dynamics.R` — frozen pass, mech state helpers, applier
  (allocation blend + `kg_lockin`/`kg_deemed` stamping), cells/tau loader
  split with inputs cache, per-year regime resolver, deemed-decedent tau
  exclusion.
- `src/sim/run.R` — `run_frozen_pass`, shared compat guards
  (`kg_dyn_check_run_compat`), 4-step `do_scenario`, static injection +
  `liab_deemed` in `run_one_year`, detail writes.
- `src/slurm/frozen.R` (new Phase 1B), `src/slurm/setup.R` (manifest),
  `slurm_run.sh` (phase wiring; 2A depends on 1 and 1B).
- `src/data/post_processing/horizontal.R` — baseline-income-anchor filter.
- `src/data/post_processing/distribution.R` — untouched so far; workstream
  §6.1 lands here.
