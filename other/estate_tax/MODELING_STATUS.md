# Estate Tax Modeling — Status & Orientation

> **SUPERSEDED 2026-06-10:** current state lives in
> `new_estate_modeling_thoughts.md` (§10h = interim accepted state and
> improvement agenda; fitted params in `estate_valuation_params.yaml`;
> canonical code = `estate_module.R` / `estate_diagnostic.R` /
> `calibrate_estate_v2.R`). This file describes the pre-locked-spec
> calibration and is kept for history.

_Last updated: 2026-06-09. Working branch: `wealth`. Scope: the standalone
on-model estate tax calibration in `other/estate_tax/`. Nothing here changes the
simulator's tax calculation yet._

_See **§10** for the latest work (level constraint, gift add-back, and the count
diagnostic that reframes the top-bin undershoot). §§1–9 are the prior state._

## 1. What we're building

A reduced-form estate tax calculator that runs on Tax-Data tax units (which have
imputed **wealth** and **mortality** but no estate-return data) and reproduces
(a) the IRS SOI estate-tax tables and (b) CBO/JCT revenue. The calibrated knobs
bridge "economic wealth in the data" to "estate tax paid."

### Per-record pipeline (`apply_candidate` in `calibrate_estate_tax.R`)

```
economic_gross   = sum of 14 value.* wealth columns
reported_gross   = economic_gross x r(size) x [1 + (rho_pt - 1) * s_pt]
taxable_estate   = reported_gross x t(reported_gross)
estate_tax       = max(0, tentative(taxable_estate) - tentative(exemption))   [unified credit]
                   with a DSUE/portability blend on the exemption
expected_revenue = sum over records of  weight x mortality x estate_tax
mortality        = q_death1 (single) or q_death1*q_death2 (joint)
deaths -> receipts: receipt_year = death_year + 1 (≈9-month 706 due date)
```

- **`r(size)`** — reporting factor (the bridge). The only thing truly optimized.
- **`rho_pt`** — pass-through (closely-held business) reporting factor RELATIVE
  to all other assets; `s_pt` = pass-through share of gross. Captures §2031
  valuation discounts (minority/marketability) that concentrate in business.
- **`t(.)`** — taxable fraction (gross→taxable estate), PRE-FIT from SOI ratios,
  not optimized. DSUE table likewise pre-fit from SOI.

## 2. How calibration works

Two target families, two roles:

- **SOI → SHAPE.** Within-year bin shares (count, gross, taxable estate, net
  tax) across the 3 modelable size bins ($10–20M, $20–50M, $50M+). NOT dollar
  levels — levels in any single year are dominated by asset-price swings and
  top-tail lumpiness.
- **CBO/JCT → LEVEL.** CBO baseline estate+gift receipts (×0.9 gift haircut) and
  the JCT OBBBA-vs-sunset delta, as 10-year aggregates.

Objective = `SOI_SHARE_SCALE * mean-over-years( weighted (model_share -
target_share)^2 )` + `sum( 4 * (score rel. error)^2 )`. Reporting form chosen by
a 5×2 tournament (constant / log_linear / log_quadratic / bounded_log_quadratic
/ bin_lookup) × (bin_lookup / smooth_logit taxable fraction).

### The bracket-creep correction (important, recent)

The $10/$20/$50M bins are **nominal and fixed**, but wealth grows, so a fixed bin
is a different real slice each year. We hold the model at its one wealth year
(`SOI_MODEL_BASE_YEAR = 2022`) and, for each SOI death year, re-bin that base-year
wealth at thresholds scaled by the **FRED household net-worth deflator**
(`NW_2022 / NW_year`, annual average, `TNWBSHNO`). This reads the model's shape at
each year's real-equivalent cut points; averaging across 2018–2022 denoises the
lumpy top. Deflators: 2018 ×1.38, 2019 ×1.30, 2020 ×1.22, 2021 ×1.01, 2022 ×1.00.

### Key calibration decisions made

- Calibrate against the **taxable** universe (`tax_status == 'taxable'`), not all
  filed returns — the model only represents estates that owe tax. Nontaxable
  filers (marital-deduction first deaths, portability-only elections) come from
  mechanisms we don't model.
- Baseline = **OBBBA** ($15M exemption). The CBO level target is matched to the
  $15M scenario; the pre-OBBBA $7.2M sunset survives ONLY as the JCT delta
  counterfactual.
- 10% fixed gift-tax haircut on the estate+gift score targets.

## 3. Where the calibration landed (provisional)

Latest full run (billionaire vintage `2026052823`, shape-matching objective):

- **Reporting factor r ≈ 1.0, ~flat across size.** The earlier r>1 "hump"
  (r≈2.1 at $100–200M) is GONE — it was an artifact of calibrating to the 2022
  level spike. Confirmed by the smooth forms all landing at a,b,c ≈ 0.
- **rho_pt ≈ 0.60** — a ~40% pass-through valuation discount. Stable across every
  functional form. Implied reporting rates now sit at ~1.0 for liquid estates,
  ~0.6 for fully-business — a genuine valuation discount, ≤1 everywhere.
- **Score fit:** CBO baseline −6.2% (cumulative), JCT delta −3.0%. Within tolerance.
- **SOI shape fit:** gross and taxable-estate shares near-exact; net-tax and
  count slightly over-concentrated at the top.
- **Cross-model check:** rho_pt≈0.60 ≈ the deemed-realization keep≈0.66 found
  independently in the kg_dynamics work. Two analyses converge on the same ~⅓–40%
  closely-held discount.

**Recommended spec:** simple **log_linear (or constant) r × rho_pt≈0.60** — drop
the flexible curve; it's no longer needed and was overfitting the 2022 spike.

## 4. The out-of-sample failure we just found (READ THIS)

Running the calibrated model on **death year 2022 at the actual $12.06M
exemption** (an honest out-of-sample check) reveals a **level undershoot that the
shape/score fit masked**:

| bin | mdl count | SOI count | mdl tax $B | SOI tax $B |
|---|---|---|---|---|
| under 10M | 0 | 274 | 0.0 | 0.3 |
| 10–20M | 829 | 1,527 | 0.4 | 2.6 |
| 20–50M | 899 | 1,269 | 4.0 | 7.7 |
| 50M+ | 552 | 620 | 12.9 | 33.8 |
| **TOTAL** | **2,280** | **3,690** | **17.3** | **44.4** |

Count −38%, tax −61%, avg tax/return LOWER in every bin. The undershoot is
**bottom-heavy** (10–20M −46%, under-10M = literally zero), NOT top-tail. Two causes:

1. **No lifetime-gift add-back — now the #1 fix.** Estate tax base is
   `taxable_estate + adjusted_taxable_gifts`. We omit gifts. Gifts are what push
   sub-$20M estates over the exemption, so their absence zeroes out under-10M and
   halves 10–20M. SOI 10–20M estates carry ~$1.8M gifts each.
2. **2022 was a ~+35% spike year** (asset-price peak + lumpy deaths). We
   deliberately don't chase it (shape/denoised), so part of the tax gap is "by
   design." But the count gap is real and robust (counts aren't lumpy).

**Caveat on earlier optimism:** shares matching + cumulative CBO fit (−6%, with
the 0.9 gift-haircut slack) HID a level undershoot. Structurally the model is
clean (flat r, sensible rho_pt); on levels it is biased low, mostly from the
missing gift base.

## 5. Counts: history vs projection (current law, OBBBA $15M indexed)

| | taxable returns |
|---|---|
| SOI 2018–2022 (actual) | 2,570 / 1,275 / 2,584 / 3,170 / 3,690 |
| Model FY2026 → FY2035 | ~1,970 → ~4,060 (≈ +106%, 8%/yr) |

Projection roughly doubles over the decade — wealth grows ~6%/yr while the
exemption indexes ~2%/yr (chained CPI), so estates clear the bar faster
(bracket creep). NOTE these are biased low per §4.

## 6. Upstream data issue (Tax-Data, separate repo)

Population-weighted, the **$100M–$1B band is under-sampled**: output captures only
~68% of the SCF's population and ~64% of its wealth there. Tax-Data enumerates
$1B+ from Forbes (weight 1 each) but DRF-samples $100M–$1B from a thinned donor
pool, and the Forbes-splice reweighting cannibalizes that band further. BUT: this
is a minority of the death-weighted gap (~$13B), because billionaires barely die
(expected-value) and the aggregate top wealth is ~conserved. **The bigger levers
are the gift add-back and overall level, not the billionaire tail.** User is
adding billionaires/top-tail fixes upstream; re-pin rho_pt after.

## 7. Open items / next steps (priority order)

1. **Gift add-back** — single scalar `base = taxable_estate + gamma*reported_gross`,
   gamma≈0.10 from SOI `adjusted_taxable_gifts/gross`. Directly creates the
   missing small-bin taxable returns. Re-run the §4 2022 check to measure closure.
2. **Investigate the level undershoot** beyond gifts (is $10–50M wealth
   under-populated death-weighted? mortality? reporting/taxable-fraction).
3. **Upstream $100M–$1B sampling fix** (Tax-Data), then re-pin rho_pt.
4. **Lock the form** to log_linear/constant once the above settle.
5. Decide whether to bump `interface_versions.yaml` default vintage to
   `2026052823` — for estate purposes it's a mild regression in the $100M–1B
   band, so HOLD until upstream fix.

## 8. Files

- `calibrate_estate_tax.R` — the calibration (sourceable; set
  `ESTATE_CALIBRATE_NO_MAIN=1` to source for functions without running main).
- `estate_tax_filed_2019_2023.csv` — cleaned SOI tables (death years 2018–2022).
- `score_targets_estate_gift.csv` — CBO/JCT targets.
- `count_taxable_returns.R`, `diag_2022_count.R`, `scf_toptail_compare.R` —
  diagnostics used in this analysis.
- Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration/`
  (`estate_calibration_{parameters,moments,pareto,diagnostics}`).
- sbatch wrappers in repo root: `estate_calibrate.sbatch` (full run, points at
  vintage `2026052823`).

## 9. Status of the code

**All of the above is uncommitted in the working tree** (shape-matching refactor,
FRED deflators, new objective, diagnostics). `origin/wealth` still has the older
level-matching version. The separate **kg_dynamics deemed-realization** work
(per-record value-based avoidance, 25% default) IS committed and pushed.

## 10. Update 2026-06-09 — level constraint, gift add-back, and the count reframe

Three changes to `calibrate_estate_tax.R` (all uncommitted), plus a finding that
redirects strategy on counts.

### 10a. Objective was secretly shape-only; rebuilt as a LEVEL CONSTRAINT

The shape-matching refactor (§2) silently broke the level fit. The objective summed
two blocks in **different rulers**: SOI shape in absolute share-points (a Brier
score on a normalized distribution) and CBO/JCT level in relative %. The old
`SOI_SHARE_SCALE = 10` bridge, plus ~60 SOI terms vs 2 score terms, made the split
**0.414 SOI / 0.019 score** — i.e. the optimizer spent ~96% of effort on shape and
~4% on level. The −6% CBO fit was *flattering*: level was barely in the loss.
(The committed level-matching version did NOT have this problem — it scored SOI in
`log_rel_error`, same ruler as the score block. The refactor to *shares* forced the
ruler change, since relative error on small shares blows up.)

Fix: shape is now the primary objective at natural scale (`SOI_SHARE_SCALE = 1`);
each cumulative score target enters as a **one-sided deadband penalty** — zero while
`|rel error| <= LEVEL_TOL` (3%), then `LEVEL_LAMBDA * excess^2` (λ=200). Keeps each
block in its correct ruler instead of a hand-tuned exchange rate. Confirmed binding
in a smoke run (winner has `score = 0`, i.e. inside the band; violators penalized).
**Implication: don't trust the old −6%/−3% score fit as "near-optimal on level" — it
was never optimized. Re-running under the constraint is the honest level fit.**

### 10b. Gift add-back implemented as a flat scalar — and it can't fix bottom counts

`apply_candidate` now builds `estate_base = taxable_estate + gamma * reported_gross`,
used by both the liability AND the `taxable` flag. `gamma` is pre-fit from SOI
`adjusted_taxable_gifts/gross` on the modelable bins (excludes under_10m, whose 1.45x
ratio is a selection artifact); it lands at **gamma ≈ 0.108**. Threaded through the
call chain like `taxable_fits`/`dsue_table`.

**Empirical result (confirms the prior worry): a flat gamma does NOTHING for the
under-10M bin** — it stays at 0 modeled returns. A 0.108 add-back on a $7M estate is
~$0.76M, nowhere near clearing the $12M exemption, but the 274 real under-10M taxable
returns each carry ~$9.9M of gifts (gifts > their own estate). A population-mean
gamma physically cannot reproduce them. 10–20M *did* improve (−46% → −27%). If the
bottom ever matters, the fix is a **heterogeneous/skewed gift model**, not a bigger
gamma. (Code carries this caveat inline.)

### 10c. THE COUNT REFRAME — top-bin undershoot is classification, not data/mortality

New diagnostic (`estate_calibration_counts.csv` + console table) reports, per bin:
`raw_pop_count` (death-weighted decedents by **economic** gross, no reporting factor,
no taxable filter — **fit-independent**), modeled taxable count, SOI count, and
coverage. Death year 2022:

| bin | raw_pop (data) | model taxable | SOI | coverage |
|---|---|---|---|---|
| 20m_50m | 1,026 | 720 | 1,269 | **81%** |
| 50m_plus | 569 | 274 | 620 | **92%** |

This overturns two assumptions:

- **Mortality is exonerated.** The data holds 569 death-weighted >$50M decedents =
  92% of SOI's 620. Deaths are present. A mortality scalar would push raw_pop *past*
  620 and is also backwards (wealthy die *less*). Do not touch mortality.
- **Coverage is mostly exonerated at the count level.** The §6 ~68% was $100M–$1B
  *wealth*-weighted; the death-weighted *count* coverage for the whole 50M+ bin is
  ~92% (20–50M is ~81%). Not the bottleneck.
- **The real culprit: the model demotes decedents it already has.** 569 raw → 274
  taxable in 50M+. ~295 top estates get pushed *below* the $50M reported threshold by
  the reporting factor (r≤1, and especially the rho_pt≈0.6 business discount), landing
  in 20–50M. It's a **classification effect**, parameter-controllable — so a count
  target IS a legitimate lever (unlike mortality).

**The tension to design around:** the *same* rho_pt discount that fits dollars and
shares is what demotes the top counts. One knob, three jobs (dollars, shares, counts)
in conflict. Cranking the discount down to hold counts in the top bin inflates dollars
(now constrained) and shifts shares. The reduced form may not satisfy all three with a
single rho_pt — may need to decouple the discount used for *binning* vs *dollars*.

### 10d. Next step (proposed, not yet built)

Add **taxable counts by bin as a third constrained block** (same deadband as level),
targeting the **top two bins only** (user does not care about the bottom 274). Re-run
the tournament and let it *show the trade* — how much shape/level degrade to buy back
top counts — which decides whether a single rho_pt suffices or the discount must be
decoupled. Bottom-bin counts are explicitly out of scope.

### 10e. Code/run status

Edits in `calibrate_estate_tax.R` (gift threading, deadband objective, count
diagnostic) and `diag_2022_count.R` (passes gamma). New sbatch wrappers in repo root:
`estate_calibrate_smoke.sbatch` (`--quick`, writes `..._smoke`) and
`estate_calibrate_giftlevel.sbatch` (full, writes `..._giftlevel` so the pre-change
fit at `..._calibration/` is preserved for comparison). Full run was queued
(job 14067015) for converged `model_count`; coverage/raw_pop numbers above are
already final (fit-independent). Still all uncommitted.
