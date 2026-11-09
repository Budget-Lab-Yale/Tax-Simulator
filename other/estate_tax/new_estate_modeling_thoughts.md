# New Estate Tax Modeling — Thinking From First Principles

_Started 2026-06-09. A deliberate ground-up rethink of the estate tax model,
parallel to (not built on) the prior `MODELING_STATUS.md`. The goal here was to
build concrete intuition about the data and the tax mechanics BEFORE calibrating,
so we don't fit structural parameters to noise or data bugs._

---

## 1. The mechanics: who actually pays estate tax (and what it means for us)

The unlimited marital deduction (IRC §2056) means the **first death of a married
couple is a non-event for estate tax** — everything passing to the surviving
spouse is deducted, taxable estate → 0. The tax base is **deferred to the second
death**. SOI confirms this starkly (death year 2022, all sizes):

| status | returns | % with spousal bequest | spousal bequest / gross | net tax |
|---|--:|--:|--:|--:|
| taxable | 3,170 | **15%** | **13%** | $22.5B |
| nontaxable | 4,960 | **73%** | **59%** | $0 |

So the **taxable universe ≈ last-to-die + never-married + divorced**. Three
consequences for the model:

1. "Model the nonjoint returns" really means "model the unit that exists at the
   *last* death." A widow holding combined marital wealth is **already a single
   tax unit** in the Tax-Data cross-section — she's captured via her own `q1`.
2. The exemption is heterogeneous: ~1× for true singles, up to **2× for the
   last-to-die of a couple** via portability (DSUE). But DSUE is a minority
   channel — only ~11–14% of payers claim it, averaging ~$3–4M (not a full
   doubling).
3. Married couples (both alive) generate a taxable event only at the **second
   death**, approximated cross-sectionally by `q1·q2`. This is tiny, and that's
   correct — the deferral means living couples rarely produce a current-year
   estate tax event.

### The governing consistency constraint

Do **not** model the second death twice. The cross-sectional engine handles it via
(a) couples taxed at `q1·q2` (2× exemption) + (b) widows-already-single taxed at
`q1` (single exemption + DSUE). These are **disjoint populations** in the data. If
we ever simulated a couple's first death → created a widow → taxed her later, we'd
double-count against the widows already present. We use the cross-sectional path.

---

## 2. The locked reduced-form spec

For each record, per year, gross = valuation-discount machinery `f(assets)` (kept),
debts taken from the actual Tax-Data debt columns (NOT netted into gross):

**Joint returns** (`filing_status==2 & q2>0`), both-die event:
```
gross    = f(assets)
taxable  = max(gross - debts - f_ded(size)*gross, 0)
tax      = max(taxable - 2*exemption, 0) * rate
E[.]     = weight * q1 * q2 * tax
```

**Single returns** (everyone else), single-death event with DSUE blend:
```
gross    = f(assets)
taxable  = max(gross - debts - f_ded(size)*gross, 0)
dsue     = f_dsue(size) * gross
liab_wo  = max(taxable           - exemption, 0) * rate     # two FULL calcs,
liab_w   = max(taxable - dsue     - exemption, 0) * rate     # each through the kink
E[liab]  = p_dsue*liab_w + (1-p_dsue)*liab_wo
E[.]     = weight * q1 * E[liab]
```

Key design decisions:
- **`rate` = 0.40** is ~exact: at these estate sizes the graduated 18–40% schedule
  lives entirely below the exemption and is erased by the unified credit, so tax is
  flat 40% above the exemption.
- **DSUE blend = two complete liability calculations, probability-weighted** — NOT
  an expected DSUE inside one calc. The `pmax` kink is nonlinear; smearing it
  misprices everyone near the threshold. (dsue in taxable-estate vs added to the
  exemption is algebraically identical given the flat rate.)
- **`f_ded` is NON-debt deductions** (`= (total_allowable_deductions − debt)/gross`
  from SOI taxable), because debt is now subtracted explicitly. It MUST be
  size-varying: taxable/gross runs ~90% at $10–20M down to ~53% at $50M+, driven by
  charitable (and residual marital) deductions concentrating at the top. A flat mean
  would over-tax the top ~2×.
- **Revenue = expected value** (no RNG). **Distribution = a single decedent
  sub-record** with `weight' = weight × mortality` carrying the tax (the survivor
  fraction carries none). Only the taxable configuration needs materializing — no
  4-state split.

`f_ded`, `f_dsue`, `p_dsue` are **estimated from SOI** (assumed, not calibrated).
The **only** thing calibrated is the valuation/reporting factor `f(assets)`.

---

## 3. First-order, UNCALIBRATED diagnostic (`r=1`)

Scripts: `first_order_shape.py` (single year) and `first_order_shape_multiyear.py`
(deflated multi-year). Source = Tax-Data death year 2022; target = SOI death year
2022 (= filing 2023); `r=1`, no valuation discount; `f_ded/f_dsue/p_dsue` from SOI.

### 3a. Single-year (2022)
```
       bin |  mdl_cnt  soi_cnt  cnt_err | mdl_tax$B soi_tax$B  tax_err
   10m_20m |     3261     1527   +114% |       4.8       2.6    +84%
   20m_50m |      961     1269    -24% |       4.1       7.7    -47%
  50m_plus |      557      620    -10% |      17.7      33.8    -48%
     TOTAL |     4779     3690    +30% |      26.6      44.4    -40%
```

### 3b. Multi-year, 2022 wealth deflated to each death year (FRED net worth)

2022 is anomalous on DOLLARS only. Tax error by year: +9% / +47% / +8% / +13% /
**−40%** (2018→2022). SOI net tax ran 13.2/9.3/18.4/22.5/**44.4** $B — 2022 doubled
off 2021, a lumpy top-tail spike. On the 5-year average the **dollar level is off
just −7%**. The **count** overshoot is robust every year (avg +74%). Denoised by bin:

```
       bin | mdl_cnt soi_cnt cnt_err | tax_err
   10m_20m |   3391    1123   +202%  |  +75%
   20m_50m |    828     849     -3%  |  -14%
  50m_plus |    394     377     +5%  |  -14%
```

**Takeaways:** (1) calibrate to the multi-year average, never to 2022 alone;
(2) counts at 20–50M and 50M+ are already right once denoised — the **§10c
"one knob, conflicting jobs" tension was largely a 2022 artifact**; (3) the entire
robust signal is the 10–20M count overshoot; (4) joint/DSUE confirmed negligible to
revenue (joint = 82 of 4,779 taxable units).

---

## 4. The 10–20M overshoot is a DATA ARTIFACT, not economics

Discriminated valuation (H1) vs deduction-heterogeneity (H2) and found **neither** —
it's a replicated donor record.

- **H2 ruled out:** among SOI single (no-spouse) 10–20M filers, 56% are taxable;
  the nontaxable singles aren't charity-zeroed (18% claim it, 5.5% of gross). No
  hidden high-deduction single population for the mean `f_ded` to miss.
- **Not smooth H1 either:** the model's single-decedent death-weight by gross is
  Pareto-declining (density 605→327→144 per $M) then **jumps back to 607 at
  $15–20M** — an impossible discontinuity.
- **The smoking gun:** the $15–20M death-weight is dominated by records at
  **exactly $17.65M gross, age 75–80**, weights ~2,700–3,500, q1 ~0.09–0.12. The
  top 10 such records = **1,916 expected deaths = 63% of the 15–20M death-weight**,
  more than the entire real SOI taxable 10–20M universe (1,527). A donor clone with
  high weight × high mortality.

**Effect of removing it:** model 10–20M taxable 3,354 → ~1,438 vs SOI 1,527
(**within 6%**). So `r=1` essentially fits 10–20M once the clone is gone. The
reporting factor's apparent "job at the bottom" was a mirage — it was the lever the
optimizer would have used to absorb a Tax-Data bug.

> **Strategy implication:** the reporting-factor calibration should discipline the
> **top tail dollars** (modest −14% residual, plausibly §6 wealth coverage), NOT the
> bottom. Fix/handle the donor clone upstream (or down-weight in the estate module)
> before calibrating `r`.

(See also memory: `estate_taxdata_donor_clone_artifact`.)

---

## 5. Open items / next steps

1. **(a) Fingerprint the clone** across all of Tax-Data — how many clones, does it
   recur at other round wealth values? _(results below)_
2. **(b) Re-run the shape with clones down-weighted** to confirm the bottom falls
   into line. _(results below)_
3. Decide clone handling: upstream Tax-Data imputation fix vs in-module down-weight.
4. Only then calibrate `f(assets)` against the **multi-year-averaged top-tail**.
5. Port the locked spec (§2) into the on-model module / `calibrate_estate_tax.R`,
   carrying `filing_status` through the cells (current code drops it at cell-collapse,
   so it can't condition the exemption on married/single).

## 6. Files
- `first_order_shape.py` — single-year uncalibrated diagnostic.
- `first_order_shape_multiyear.py` — FRED-deflated multi-year version.
- `clone_fingerprint.py` — (a) donor-clone detector.
- `shape_decloned.py` — (b) shape diagnostic with cluster death-weight winsorized.
- `estate_tax_filed_2019_2023.csv` — cleaned SOI tables (filing 2019–2023).
- `estate_tax_filed_2016_2023.csv` — combined cleaned SOI tables incl. pre-TCJA
  filing years 2016–2018 (built by `convert_soi_xlsx.py` from the raw
  `1[678]es01fy*.xlsx` workbooks; `xlsx_dump.py` = stdlib inspector). CANONICAL.
- `estate_module.R` — locked-spec module, R port (record-level; §10). The python
  scripts above remain as cross-checks; the R module is canonical going forward.
- `estate_diagnostic.R` — multi-year shape diagnostic on the module, with
  mortality raw|smooth and gifts on|off variants; auto-extends to pre-TCJA
  death years when the SOI csv gains earlier filing years.
- Prior state: `MODELING_STATUS.md`. Memory: `estate_taxdata_donor_clone_artifact`.

---

## 7. (a) Clone fingerprint — results (`clone_fingerprint.py`)

Donor-clone clustering is **how Tax-Data builds the entire ≥$10M population**, not a
one-off. Clustering on exact economic-gross value (death year 2022, single-equiv
death-weight Σw·q1):

- **99%** of all ≥$10M death-weight comes from records sharing an exact gross with
  ≥1 other record; **92%** from clusters of ≥5 records.
- ~**200 archetypes carry 92%** of ≥$10M death-weight; top 10 carry 54%.
- The **$17.65M / age-79 cluster is a singular outlier: 34.8% of ALL ≥$10M
  death-weight by itself** (21 records, 32,758 pop weight, 2,442 expected deaths),
  with a ~10× gap to the #2 cluster (236). Pathological because it stacks large
  population weight × old age × high-`q1`-on-the-high-weight members.

Implication: estate counts on this data are inherently hostage to a few hundred
donor records' (weight × q1). The DRF/donor-pool imputation (§6 of `MODELING_STATUS`)
is showing through. Estate-count calibration is fragile unless clones are handled.

## 8. (b) Shape with clones down-weighted — results (`shape_decloned.py`)

Cluster death-weight capped at `cap` (each member's effective weight scaled by
cap/cluster_dw). 5-year-averaged count error vs SOI, `r=1`, otherwise uncalibrated:

| variant | 10–20M | 20–50M | 50M+ | TOTAL |
|---|--:|--:|--:|--:|
| baseline | **+202%** | −3% | +5% | +74% |
| drop $17.65M | −16% | −3% | +5% | −18% |
| **cap @300** | **+11%** | −3% | +5% | **−7%** |
| cap @150 | −6% | −3% | +5% | −14% |

**Decisive:** the 10–20M overshoot was entirely the donor clones. A mild
winsorization (cap @300, just above the legitimate #2 cluster at 236) lands the
**whole count distribution within ~10% of SOI at `r=1`, with no reporting factor**.
20–50M / 50M+ are invariant across variants — the artifact is localized to 10–20M.

What remains is **dollars, not counts**: de-cloned model tax (cap@300, $B/yr) runs
1.3 vs 1.9 (10–20M), 4.5 vs 5.3 (20–50M), 12.2 vs 14.2 (50M+) — a clean ~15–25%
undershoot that IS the legitimate calibration target (top-tail wealth coverage + the
valuation factor `f(assets)`), now uncontaminated by the count bug.

### Conclusion / revised strategy
1. The reporting factor `f(assets)` has **no job at the bottom** — `r≈1` fits counts
   once clones are tamed. It is purely a **top-tail dollar** instrument.
2. **Handle the clones before calibrating.** Decide: upstream Tax-Data imputation fix
   (preferred — the $17.65M and similar archetypes are over-replicated) vs in-module
   cluster death-weight winsorization (cap ≈ 300 works as a stopgap).
3. Re-pin the dollar calibration of `f(assets)` against the multi-year-averaged
   top-tail AFTER de-cloning; the ~15–25% dollar gap is the real signal.

---

## 9. Root cause + what would actually fix it upstream (end-of-session synthesis)

### 9a. Cross-vintage + Tax-Data-side confirmation

- NOT vintage-specific: the repo-default Tax-Data `2026050315` has the same pathology
  with a *different* archetype ($20.56M / age 80 = 29.6% of ≥$10M death-weight, vs
  $17.65M in `2026052823`). The offending wealth point MOVES per vintage — any
  in-module handling must be a general exact-gross cluster cap, never a hardcoded value.
- The Tax-Data-repo review independently found the same cluster and the mechanism:
  the bin-level wealth MARGINAL is correctly calibrated to SCF (~1% on counts AND
  dollars through 50–100M). The problem is record-level granularity: above ~$10M the
  population is built from a thin donor pool — **15–20M bin = 223 distinct donors,
  ESS ~486, only ~25 elderly (age ≥72) SCF households** — replicated at high PUF
  weight with no jitter (`wealth.R:1014` single-donor draw; `:1156–1172` cell rescale
  → byte-identical clones). Estate scoring (mortality × per-record) is the one use
  that amplifies this into a spike. Their diagnostics:
  `Tax-Data src/eda/scf_vs_output_wealth.R`, `src/eda/scf_donor_pool.R`.

### 9b. Which upstream fixes work — IMPORTANT CORRECTION

The spike is **AGE/mortality concentration, not wealth concentration**: ~32,800
weight all carrying one 80-year-old's q1. Therefore:

- **k-draw weight-splitting does NOT fix it** (initially mis-claimed in-session, then
  corrected): drawing k donors per heavy record just resamples the same thin pool —
  in expectation the elderly donor accumulates the SAME total weight, so Σw·q1 is
  unchanged. It lowers row-level lumpiness/ESS-of-rows only.
- **Wealth jitter / parametric wealth smoothing does NOT fix it**: spreading the
  clones across $15–20M leaves them all age 80; the bin's death-weight is unchanged.
  (Jitter is how we *spotted* the bug; it would only hide the fingerprint.)
- Fixes must ADD age information that isn't in the pool:
  1. **Thicken the donor pool with genuinely new (wealth, age) pairs** — e.g. pool the
     2019 (and 2016) SCF waves: ~doubles the ~25 elderly 15–20M donors. Requirements:
     revalue 2019→2022 by ASSET CLASS (not one deflator, to preserve composition for
     the valuation-discount side); fold donor weights sensibly so the already-correct
     2022 marginal calibration is preserved (re-check SCF-vs-output bins after); keep
     surveyed ages as-is (the real (wealth, age) joint is the value). Mitigates
     (~25→~50 donors), doesn't fully cure.
  2. **Decouple age from the donor**: fit age|wealth (or mortality|wealth) at the top
     and re-draw age, keeping each donor's real wealth vector/composition. Smooths
     the one degenerate dimension, keeps the two good ones empirical. Can also be done
     ESTATE-SIDE (apply age-conditional mortality to the bin population instead of
     each clone's copied age) without touching the imputation at all.
- Cheap sizing test before committing to (1): count distinct elderly donors 2019 adds
  in 15–20M / 20–50M (revalued) and recompute top-1 donor share / donor-level ESS on
  the pooled pool. If top-1 falls 8.6% → ~2–3%, it buys most of the spike.

### 9c. Plain-language framing (where we landed)

- **Box A (the estate model): nearly done.** Structure settled (§1–2); counts match
  SOI at `r=1` once clones are handled (§8); the one real remaining modeling task is
  the **top-tail dollar undershoot (~15–25%)** — calibrate `f(assets)` to it.
- **Box B (the Tax-Data clone bug): an input problem that A depends on.** It must be
  handled either upstream (donor pool, 9b) or as a local patch (cluster death-weight
  winsorization, cap ≈300). Without handling it, A's counts are wrong — and worse,
  calibration would have silently absorbed the bug into the valuation-discount
  parameter (the near-miss this whole exercise avoided).
- Open decision: patch B locally and finish A now, vs fix B upstream first and
  calibrate on clean data. Either is legitimate; local patch is faster, upstream is
  cleaner and helps other users of the top tail.

---

## 10. 2026-06-10 — pooled-SCF vintage results, architecture decisions, allocator spec

### 10a. New-vintage diagnostics (Tax-Data `2026060918`, pooled 2019+2022 SCF + Forbes-splice work)

Re-ran the §3/§7 diagnostics (logs: `clone_check_newvintage_14529113.log`,
`shape_check_newvintage_14531753.log`; the three python scripts now take the
Tax-Data path as `argv[1]`). Two headline results that point in opposite directions:

**(i) The uncalibrated model now fits current law almost perfectly.** At `r=1`, no
clone handling, no gift add-back, 5-year averaged vs SOI:

| bin | cnt_err | tax_err |
|---|--:|--:|
| 10m_20m | +14% | −29% |
| 20m_50m | +15% | −0% |
| 50m_plus | +9% | −3% |
| **TOTAL** | **+0%** | **−6%** |

The +202% 10–20M count overshoot is gone AND the ~15–25% top-tail dollar
undershoot is gone (Forbes-splice work upstream, presumably). The remaining
10–20M dollar gap (−29%) is roughly the size of the missing gift add-back.

**(ii) But the clone pathology survived pooling.** New top cluster: exactly
$11.464M gross, avg age 76, 29 records, 37,638 pop weight, **2,654 E[deaths] =
32.5% of all ≥$10M death-weight** — structurally identical to the old $17.65M /
34.8% cluster. Confirms §9b: pooling thickens the wealth dimension; the spike is
age/mortality concentration and that's untouched.

**(iii) Why both are true — the landmine framing.** The new cluster sits *below*
the taxable threshold under current-law exemptions in every comparison year, so it
contaminates nothing on baseline — by luck of where it landed this vintage. Any
reform that lowers the exemption (the $7.2M sunset that defines the JCT delta
target; $3.5M proposals) slams ~2,650 phantom-ish decedents into the base in one
step. **The clone problem has migrated from a baseline-calibration problem to a
reform-robustness problem** — invisible in baseline diagnostics, detonates
off-baseline. (`shape_decloned.py` variants now no-op: they were keyed to the old
cluster's location/cap; any guard must be generic, never value-hardcoded.)

### 10b. Does the JCT counterfactual discipline the unobserved region? (debate, resolution)

Position considered: the JCT sunset delta (~$20–33B/yr) is itself a low-exemption
counterfactual, so calibrating to it disciplines the $7–15M band we never observe
in current-law SOI. TRUE as far as it goes — it anchors the *aggregate* and makes
sunset-family scores JCT-consistent by construction. But anchoring ≠ repairing:

1. **One number per year vs a point mass.** The aggregate can be right while the
   mass sits at the wrong location → revenue-vs-exemption has a cliff at ~$11M.
   Any interior reform ($10M, $12M) and any exemption sweep shows the seam.
2. **Magnitude forces an interaction with calibration.** If ~half the cluster's
   death-weight is excess vs a smooth age|wealth distribution: ~1,300 phantom
   deaths/yr × ~$1.2M tax at the sunset exemption ≈ $1.5–3B/yr ≈ **5–15% of the
   JCT delta**, vs a 3% deadband. Either a flexible r(size) silently bends to
   absorb it (the §4-style near-miss, now invisible on baseline) or a flat r makes
   the constraint infeasible (loud, but you still then have to fix the data).
3. **Counts contaminate distribution.** "N estates / N heirs affected" is a
   headline output; phantom estates at $11.5M become a phantom spike of taxed
   inheritances in the $4–8M range under low-exemption reforms.

**Resolution = an experiment, not a judgment call.** Extend the SOI tables back to
pre-TCJA death years (≤2017, $5.49M exemption) — *observed counts* exactly in the
band where the cluster lives. Then run the first-order count diagnostic at the
2017 exemption twice: raw mortality vs the smoothed-mortality fix (below). If raw
passes, the cluster is benign in practice; if raw fails and smoothed passes, the
fix earns its place. Either way pre-TCJA counts become honest out-of-sample
validation instead of something calibration absorbs. Practical notes for the
extension: stay within filing years ~2014–2018 (portability fully phased, clean
filing→death mapping); lean on counts not dollars; FRED NW deflator extends back
trivially; run with the gift add-back ON (gifts are a larger share of the unified
base at low exemptions); older tables have a 5–10M bin, which is exactly the point.

**The smoothed-mortality fix (estate-side §9b option 2, concretized):** WLS fit of
household mortality m on a low-df natural spline of log(economic gross), weights =
population weight, fit separately for joint (m = q1·q2) and non-joint (m = q1)
records over gross > $1M; replace each record's m with m̂(w). Low df is the point:
a stiff global fit is informed by the whole top tail's wealth-mortality gradient
and dilutes any one cluster's leverage; an intercept-included WLS preserves total
death-weight over the fit range exactly (residuals orthogonal to intercept), so
aggregate mortality is untouched — only the degenerate age-copy dimension dies.

### 10c. Parameterization decisions (locked)

- Estate tax law becomes a standard tax-law parameter file (`estate.yaml`):
  exemption value series with indexation, graduated `brackets`/`rates` schedule
  (NOT a hardcoded flat 40% — reform proposals are 45/50/55/65% schedules),
  portability switch. Rides the existing override machinery; stacks with income
  reforms in one scenario. (Current script constants were calibration-only.)
- **Measurement ≠ law.** The calibrated bridge parameters (r, rho_pt, f_ded curve,
  DSUE table, gift gamma) are NOT tax law and must not be reform-overridable. They
  live as frozen resources the module reads (like kg_dynamics' life tables).
  Calibration becomes a one-time script that writes that resource; the on-model
  module never optimizes anything.

### 10d. Heir distribution: rank-matching allocator (replaces upstream liability, keeps its inheritances)

Contract today (`distribution.R:179–251`): per-id files `(id, p_inheritance,
inheritance, estate_tax_liability)`; heir/non-heir copy split on p; estate liab
enters the death-inclusive presentations. The old upstream liability is highly
structured — only 1,655 of 220,896 ids carry tax, starting at inheritance ≈$5.4M,
tax/inheritance monotone → 0.39 — i.e. each estate's marginal tax passed through
to its heirs. The `liab_deemed_heir`-style proportional smear would destroy that
concentration; do NOT reuse it for the estate tax itself.

**Design.** Per (scenario, year):
- Decedent side (estate module, expected-value): taxable records j with
  death-weight d_j = w_j·m_j, tax T_j under the scenario's estate.yaml,
  distributable estate n_j (reported gross − debts − non-debt deductions − ...).
  Bequest mass b_j = d_j·n_j, tax mass τ_j = d_j·T_j.
- Heir side (Estate-Tax-Distribution interface, **baseline only**): ids h with
  p_h, x_h, sim weight w_h. Inheritance mass μ_h = w_h·p_h·x_h.
- **Convention: x_h is the GROSS-of-estate-tax share.** Old file supports this
  (max tax/inheritance = 0.392 < 0.40; a net convention would allow up to 0.67).
  ⇒ `inheritance_reform ≡ inheritance` in static runs; reforms differ only in the
  liability column; the scenario-specific upstream file requirement disappears.
  (Confirm convention with the 2025092512 vintage's author.)
- **Algorithm.** Sort taxed estates by n_j desc, heirs by x_h desc. Walk both
  ladders top-down matching cumulative DOLLAR mass: estate 1's b_1 claims the top
  b_1 dollars of inheritance mass at rate T_1/n_1, estate 2 the next b_2 at
  T_2/n_2, … until the estate ladder is exhausted. Straddling heirs get the
  mass-weighted blend; everyone below the last matched dollar gets λ=0.
  λ_h = x_h × (blended rate).
- Properties by construction: aggregate identity Σw_h·p_h·λ_h = Σd_j·T_j per
  year/scenario; endogenous taxed-inheritance cutoff x*; λ/x monotone → top
  statutory rate; exemption ↑ peels tax off the smallest taxed inheritances first
  (right margin); heirs-per-estate handled implicitly via dollar-mass matching
  (a $50M estate's bequest mass claims many heirs — never needs one $48M heir).
  Identifying assumption = rank alignment (bigger inheritances ← bigger estates).
- Plumbing: module writes `estate_tax_detail_{t}.csv` in the existing 4-column
  schema (copy p/x from baseline interface, replace liability) for baseline AND
  every scenario ⇒ distribution.R needs only a pathing decision. Heirs bear tax
  in death year t (matches income inclusion); revenue series books receipts t+1.
- Diagnostics to ship: mass balance (taxed bequest mass vs available top-tail
  inheritance mass — thin tails make the walk dig deeper, report don't absorb);
  implied heirs-per-taxable-estate; baseline profile overlay vs old upstream file
  (shape, not level — its exemption assumptions predate OBBBA); cutoff and
  taxed-heir count by scenario.
- Flagged, deferrable: unify `liab_deemed_heir` under the same rank match later;
  estate×deemed base interaction in combined reforms unmodeled.

### 10e. R port built and validated; first variant results (2026-06-10, log `estate_diag_port_14550941.log`)

`estate_module.R` (locked spec, record-level) + `estate_diagnostic.R` are live.
The raw-mortality / no-gift variant reproduces `first_order_shape_multiyear.py`
**to the digit** on vintage `2026060918` (counts 2267/2328/2419/3270/3057, tax
15.7/14.1/20.4/25.1/26.4 $B, avg +0% / −6%) — port certified; the R module is
canonical from here.

Two estimation traps found and fixed during the port (both now documented in
`soi_inputs()`):
- **SOI blanks** (disclosure suppression: 50m_plus DSUE fields, filing years
  2021–23) must coerce to 0 — NA-poisoning silently zeroed top-bin tax.
- **Gift-gamma selection trap:** per-bin gamma from the taxable universe is
  upward-biased wherever the exemption cuts into the bin (taxable BECAUSE of
  gifts): gifts/gross = 0.33–1.45 (under_10m), 0.16–0.21 (10m_20m), vs ~0.08
  (50m_plus). A per-bin gamma creates thousands of phantom payers (+1337%
  under-10M counts). Fix: gamma pooled over bins with lo ≥ 1.5×exemption
  (≈0.07–0.11/yr ≈ the old flat 0.108), applied as one scalar. The pooling rule
  adapts automatically pre-TCJA (pools from 10m_20m up at a $5.49M exemption).

5-year-avg results by variant (total cnt_err / tax_err; bins in the log):

| variant | counts | tax |
|---|--:|--:|
| raw, gifts off (parity) | +0% | −6% |
| raw, gifts ON | +14% | +16% |
| smooth m, gifts off | +21% | −10% |
| smooth m, gifts ON | +33% | +12% |

Readings: (1) **gifts close the 10–20M dollar gap** (−29% → +14%) but push
counts (+45% in 10–20M) and top dollars (+16/+21%) into mild overshoot — with
the gift base in, a reporting factor slightly BELOW 1 (or rho_pt < 1) becomes a
defensible instrument again; calibration has a real, clean job. (2) **Smoothed
mortality degrades the current-law baseline fit** (+20pp counts) — raw data is
already right where it's observable; smoothing's only potential virtue is
robustness at the artifact location, which is exactly what the pre-TCJA test
will adjudicate. Smoothing band is (1M, 1B]: $1B+ records are Forbes-enumerated
with real ages and stay raw (extrapolating the spline into the billionaire tail
visibly distorted top-bin tax). (3) Consider a third contender for the §10b
experiment: the generic exact-gross **cluster death-weight cap**, which is
surgical where smoothing is global (cap threshold must be re-derived per
vintage — new #2 cluster is 559 E[deaths] vs old 236).

### 10f. THE PRE-TCJA EXPERIMENT — run and decided (2026-06-10, log `estate_diag_port_14560217.log`)

User supplied raw IRS SOI Table 1 workbooks for filing years 2016–2018 (deaths
2015–2017, $5.43–5.49M exemptions, full 5-bin split incl. 5–10M).
`convert_soi_xlsx.py` parses them into the cleaned schema →
**`estate_tax_filed_2016_2023.csv`** (combined file; amounts ×1000 to dollars;
'd'-suppressed cells blank; totals match published SOI: net tax $18.3/19.9/20.2B,
filing 2016/17/18). `estate_diagnostic.R` now defaults to it and reports
pre-/post-TCJA bin panels separately. Death years in scope: 2015–2022.

**Verdict 1 — the clone excess is REAL, confirmed by observed data.** Raw
mortality fails the low-exemption count test exactly where the clones predict.
Deflated to 2015–17, the $11.46M and $10.14M mega-clusters land at $6–8M —
inside the newly observable 5m_10m bin, above the old exemption:

| pre-TCJA bin (3-yr avg, r=1, gifts off) | raw cnt_err | raw tax_err |
|---|--:|--:|
| 5m_10m | **+140%** | +52% |
| 10m_20m | +8% | −13% |
| 20m_50m | +27% | +15% |
| 50m_plus | −17% | +1% |

Localized spike, healthy neighbors ⇒ clone signature, not deflator bias (a
too-fat deflated top tail would not spare 10m_20m at +8%). The two clusters'
death-weight (2,654 + 559) ≈ 94% of the bin's raw excess.

**Verdict 2 — global mortality smoothing REJECTED.** It half-fixes the sick bin
(+140%→+67%) but corrupts every healthy one (10m_20m +8→+42%, 20m_50m +27→+49%,
50m_plus −17→−32%): the stiff spline destroys genuine wealth–mortality signal
everywhere to dilute one point.

**Verdict 3 — smooth-RELATIVE cluster cap REJECTED.** Capping clusters whose
death-weight exceeds k× their smooth-implied value flags ~500–700 clusters and
biases the top tail down hard (50m_plus −43% at k=1.5, −55% at k=1.0). Cause:
the smooth fit is a conditional MEAN, legit elderly-skewed clusters sit above it
half the time, and capping only the above-mean side is asymmetric winsorization
— worst at the top, where the true age–wealth correlation is strongest.

**Verdict 4 — ABSOLUTE cluster death-weight cap (§8 rule, cap=300) WINS.**
Only the pathological mega-clusters can trip an absolute threshold: exactly 3
flagged ($11.46M: 2654→300; $10.14M: 559→300; $6.80M: 480→300), everything else
untouched:

| pre-TCJA bin (3-yr avg, abs-cap 300, gifts off) | cnt_err | tax_err |
|---|--:|--:|
| 5m_10m | **+34%** | −13% |
| 10m_20m | +8% | −13% |
| 20m_50m | +27% | +15% |
| 50m_plus | −17% | +1% |

Pre-TCJA yearly count totals fall from +54/+61/+61% to **+4/+10/+14%**; the
post-TCJA baseline is bit-identical to raw (capped clusters sit below current-law
exemptions). 8-year average: counts +5%, dollars −4%. Cap sensitivity: 600
leaves 5m_10m at +56% — 300 (just above the legit cluster scale, #4 ≈ 240) is
the right neighborhood. The RULE is generic (never keyed to a wealth value);
the cap VALUE is re-derived per vintage from the cluster-size distribution.

**Open residuals, named:** (i) 5m_10m +34% — mix of residual cluster excess at
the cap, smaller un-flagged clones, deflator approximation over 7 years, and the
mean-`f_ded` model making ~all single 5–10M estates taxable when SOI shows a
nontaxable single population (charity heterogeneity). Matters for $3.5M-class
reforms; largely irrelevant at sunset ($7M+) exemptions where 10m_20m+ are
clean. (ii) under_5m −100% (546 SOI returns/yr) — gift-dominated payers, out of
scope by the §10b gamma decision. (iii) gifts-on overshoots counts at the
marginal bin everywhere (+54% pre / +45% post at the bin straddling the
exemption): a population-mean gamma adds gifts to everyone when real gifts
concentrate in few estates — heterogeneous gift incidence is the eventual fix
if marginal-bin counts ever matter.

**Decision: mortality = raw + absolute cluster cap (cap≈300, re-derived per
vintage) is the locked clone guard for reform scoring.** Calibration of r /
rho_pt proceeds on this basis.

### 10g. First (r, rho_pt) calibration on the locked spec — INSTRUCTIVE FAILURE (2026-06-10, `estate_cal_v2_14569716.log`)

`calibrate_estate_v2.R` = old script's forward-receipts scaffolding (projected
Tax-Data death years 2025–34, chained-CPI exemption paths, OBBBA $15M vs sunset
$7.2M from 2026, receipts = death year + 1) + the new module's liability calc,
cap 300 + pooled gifts ON. Objective: post-TCJA SOI count/tax bin shares
(2018–22, deflator machinery) + CBO and JCT 10-yr cumulatives in ±3% deadbands
(λ=200). Pre-TCJA 2015–17 held out.

**Uncalibrated reference (r=1, rho_pt=1): shape 0.0025, CBO cum +59.9%, JCT
delta cum +8.2%.** Fit converged to **r = 0.952, rho_pt = 0.300 — a corner**
(rho at its lower bound, economically absurd at a 70% discount), with BOTH
deadbands still violated (CBO +7.5%, JCT −10.3%) and the held-out top tail
destroyed (pre-TCJA 50m_plus counts −57%, tax −39%; post-TCJA 50m_plus tax
−3% → −37%).

**Diagnosis — three masters, one knob.** The fitted model matches CBO almost
exactly in FY2026–28 then outgrows it: model receipts grow ~5.2%/yr (projected
wealth growth + bracket creep vs ~2%/yr exemption indexation) vs CBO's ~3%/yr
path. The CBO gap is a back-loaded SLOPE disagreement, not a level one. The
same haircut that buys the CBO level down shrinks the sunset delta below JCT's
(+8.2% → −10.3%). So: historical SOI dollars want r≈1; CBO's path wants the
model smaller/slower; JCT's delta wants it bigger in the $7–15M band. A flat
(r, rho_pt) cannot serve all three; forcing it absorbs a growth-path
disagreement into structural valuation parameters and poisons the observed fit
— the same calibration-absorbs-what-it-shouldn't failure mode as the clones,
caught this time by the deadband design (loud corner + violated constraints
instead of a silent bad fit).

**Resolution options (open, user to weigh in):**
(a) calibrate (r, rho_pt) to SOI only — shape + multi-year observed levels,
pre-TCJA validation — keep the JCT DELTA as the one binding external constraint
(reform-scoring anchor, nearly in-band at r≈1), and report the CBO comparison
as a disclosed growth-assumption difference; (b) same plus an explicit, named
forward-path wedge (avoidance/effective-base growth) calibrated to CBO's slope
— keeps measurement vs projection assumptions in separate parameters; (c) widen
the CBO band to ±10–15% with economically bounded rho_pt ≥ 0.5 and accept a
mild compromise everywhere. NOT acceptable: shipping r=0.952 / rho_pt=0.30.

**v2 run (SOI-anchored + CBO front window, `estate_cal_v2_14571339.log`):**
objective = shape + deadbands on {post-TCJA 5-yr SOI tax level, JCT delta,
CBO FY2026-28}, rho_pt bounded ≥ 0.5. Fitted **r = 0.951, rho_pt = 0.612** —
interior and economically right (~39% pass-through discount ≈ the old
calibration's 0.60 ≈ kg_dynamics keep 0.66). Deadbands still all violated
(SOI level −12.1%, CBO-3yr +12.9%, JCT −6.5%; CBO decade +25.3% reported).
BUT the per-year fit at these params is the real news: pre-TCJA (held out)
−15/−7/+1%; post-TCJA normal years +10/+4/+4% — **all six normal observed
years within ±15%, most within single digits**. The two failures are the
anomalous SOI years: 2019 (+38%; SOI $9.3B is anomalously low) and 2022
(−46%; the $44.4B spike). Decomposition of the apparent infeasibility:
(1) the 2019/2022 anomalies sit INSIDE the SOI level target — an
expected-value model cannot reproduce single-year mega-death lumpiness, so
level targets should exclude them (kept as diagnostics); the −12.1% "miss"
is mostly manufactured by 2022. (2) CBO front window +12.9% is modest and
partly liability-vs-cash timing (we book full liability at death+1; cash
arrives via extensions/§6166 installments) + the crude 0.9 gift haircut.
(3) The growth SLOPE gap is real and structural: model receipts grow
5.1%/yr (boomer mortality wave × wealth path × 2%/yr exemption indexation)
vs CBO's 3.0%/yr — not addressable by valuation params; needs a
demographics-vs-wealth decomposition and reconciliation against CBO's
published methodology. Residual to watch: 50m_plus tax −16/−18% at fitted
rho — the haircut may overcorrect the very top (fit fine at r=1),
suggesting the discount belongs more to 10–50M than uniform rho_pt.
NEXT: rerun with SOI level over normal years only (drop 2019, 2022), CBO
front window reported or widened, JCT kept; then the growth-gap research
task. (Earlier r=1 framing of "chronic ~+40% overshoot on normal years"
applies at r=1; the fitted haircut resolves it — the two statements are
consistent: valuation discounts are real and the haircut is their measured
size.)

### 10h. INTERIM ACCEPTED STATE (2026-06-10) + future improvements

**Accepted for now (user sign-off):** the locked-spec module with
- mortality = raw + **absolute cluster death-weight cap 300** (3 clusters);
- **gift add-back ON** (gamma pooled from bins with lo ≥ 1.5× exemption, ~0.10);
- **r = 0.951, rho_pt = 0.612** (`estate_cal_v2_14571339.log`; frozen in
  `estate_valuation_params.yaml`).

What this buys: all six normal observed SOI years within ±15% on tax (most
single digits), both eras, pre-TCJA held out; counts within ±10% era-avg; JCT
sunset delta −6.5%; CBO front window +12.9%. Known residuals, accepted with
eyes open: the 2019/2022 SOI anomalies (expected-value model, by design);
50m_plus tax ~−17% (rho haircut overcorrects the very top); CBO 10-yr +25.3%
(growth-slope disagreement, see below); under-exemption marginal-bin counts
+20-35% at low exemptions ($3.5M-class reforms only).

**Improvement agenda, priority-ordered:**
1. **Target design:** exclude anomalous SOI years (2019 low, 2022 spike) from
   level targets (keep as diagnostics) or use medians; rerun the fit — likely
   near-feasible interior solution, tighter than the accepted compromise.
2. **Growth-slope reconciliation vs CBO (the big one):** model receipts grow
   5.1%/yr vs CBO 3.0%/yr. Decompose model growth into demographics (boomer
   death-weight wave), wealth-per-decedent, and bracket creep; compare against
   CBO's published estate methodology and recent actual receipts. Decide:
   adopt ours (documented), adopt theirs (named adjustment parameter), or band.
3. **Gift modeling:** pooled gamma still inherits taxable-universe selection;
   estimate from population-level gift flows (SOI gift-tax statistics / SCF
   inter-vivos transfers) and consider heterogeneous incidence (few estates
   carry large gifts) — fixes marginal-bin counts and the under-$5M zero.
4. **Nontaxable margin:** the model makes ~every single filer above threshold
   taxable; SOI shows ~44% of single 10-20M filers are NONtaxable (charity-
   heavy). The csv's nontaxable rows support modeling a taxable-share margin
   (or explicit charitable-deduction heterogeneity). Would cut counts/tax at
   the margin and could substitute for part of the r haircut.
5. **Top-tail valuation structure:** if 50m_plus −17% persists after 1-4,
   make the discount size-dependent (belongs more to 10-50M than uniform
   rho_pt) — but re-derive, don't hand-tune.
6. **Cash-vs-liability timing:** receipts = death+1 full liability; reality
   spreads via extensions/§6166 installments. Build a receipts-timing layer if
   CBO front-window comparability ever needs to be tighter than ~±10%.
7. **Retire the cluster cap upstream:** Tax-Data age re-draw for replicated
   donors (age|wealth at the top) eliminates the artifact at the source; the
   cap then triggers on nothing and can be deleted.
8. **Then the goal-2 build:** rank-matching heir allocator on the decedent
   distribution (spec in §10d) + estate.yaml parameterization (§10c) for
   on-model scenario stacking.

### 10i. Sequencing / division of labor

1. (user) Extend SOI tables to pre-TCJA death years (~2013–2017).
2. (build) Locked-spec R module — record-level, filing_status carried, explicit
   debts (6 Tax-Data debt cols), per-bin f_ded/p_dsue/f_dsue per death year,
   bin-level gift gamma from SOI `adjusted_taxable_gifts`, raw|smooth mortality
   toggle — plus a diagnostic runner replicating `first_order_shape_multiyear.py`
   that auto-extends when the SOI csv gains pre-2018 rows. Port validated against
   the python numbers on `2026060918`.
3. Run the 10b experiment the moment tables land → decide the mortality fix.
4. Calibrate (at most r, rho_pt; r≈1 likely a *certification*, not a fit) under
   the deadband constraint; freeze resources.
5. Build the rank-matching allocator + writers; validate vs old upstream file.
6. On-model integration (estate.yaml, revenue stacking, run_one_year/liab_deemed
   precedent) last; not gating goals 1–2.

## 11. 2026-06-10 — STAGE 1 ON-MODEL INTEGRATION (revenue side) BUILT

The interim accepted state (§10h) is now wired into the simulator proper.
Architecture decided with user: NO pre-pass (each death year is independent,
expected-value — unlike kg_dynamics there is no cross-year state), straight
per-record calculation in the normal static/conventional passes, mortality
applied as a weight at aggregation.

**What landed where:**

- `config/scenarios/tax_law/baseline/estate.yaml` — LAW: exemption (statutory
  history 2014–2025, OBBBA $15M 2026, chained-CPI base 2025, round down $10k),
  §2001(c) 12-bracket schedule (calculator reads however many elements a reform
  supplies), portability switch. Reform-overridable like any parameter.
- `config/estate/estate_valuation_params.yaml` — MEASUREMENT (never
  reform-overridable): r = 0.951, rho_pt = 0.612, cluster cap 300, gamma
  0.0914 (single scalar — the pooling guard is a property of the historical
  SOI data, not of a reform's exemption, so re-pooling per reform would pool
  from selection-polluted bins), per-bin f_ded/p_dsue/f_dsue at death year
  2022. GENERATED by `write_frozen_params.R` (sbatch); pinned to Tax-Data
  vintage with a loud warning on mismatch. The sim never reads the SOI csv.
- `src/calc/functions/tax/estate.R::calc_estate()` — pure, weight- and
  mortality-free calculator in the house calc-function idiom (reuses
  `integrate_rates_brackets`). Outputs per record: `liab_estate_nodsue`,
  `liab_estate_dsue` (two FULL calcs through the kink), `estate_p_dsue`,
  `estate_distributable` (stage-2 allocator input).
- `src/sim/estate.R` — params loader + vintage guard; `calc_estate_mortality`
  (q1·q2 joint both-die / q1 single + absolute cluster cap — the one
  population-level step, hence outside the calculator); `get_estate_totals`
  (E[tax] and E[returns] with per-branch indicator blending, since
  1(E[T]>0) ≠ E[1(T>0)] at the kink) + detail-rebuild variant.
- `run_one_year()` — computes estate columns once per year after the tax-law
  join, outside the MTR loop, before the static/conventional split; 5 new
  detail columns (detail_vars). Totals gain `totals/estate.csv`.
- `calc_receipts()` — `revenues_estate_tax` = CBO LEVEL + on-model DELTA
  (scenario − model-baseline), booked FY death+1 (§706 9-month due date).
  Baseline receipts stay CBO-anchored: the model's +25% growth-slope
  disagreement (§10g) never ships; only reform deltas (the JCT-validated
  quantity) flow. Off-model estate delta superseded/dropped. Baseline leg
  falls back to rebuilding from baseline detail when totals/estate.csv isn't
  written yet (SLURM Phase 3a is a parallel array — race otherwise).
- SLURM Phase 3a (`aggregate.R`) mirrors the run_sim totals/receipts changes.
- Test scenario `tests/estate_sunset` ($7.2M 2026) + runscript
  `config/runscripts/tests/estate_sunset.csv` (2025:2034).

**Validation:** port parity vs the canonical module (death year 2022,
$12.06M exemption, fitted params): tax and counts match to ±0.0001%
(`estate_parity_*.log`, frozen-yaml rounding); cluster cap fires on exactly
the 3 known clone clusters. Full-pipeline baseline + sunset runs vs the
calibrator's OBBBA receipts / JCT delta: see logs referenced in repo root
(estate_smoke_*, slurm validation run).

**Stage 2 (BUILT — see §12):** §10d rank-matching heir allocator +
distribution.R rewiring. Still open: §10h improvement agenda 1–7.

### 11a. THE SAMPLE-UNIVERSE BUG the estate level exposed (2026-06-10, fixed)

First full-sample validation run: sunset deltas matched the calibrator
(FY2027 +18.5 vs +17.4) but the BASELINE LEVEL came in ~30% low (CY2025
$24.3B vs $34.8B on identical records/params). Cause: `parse_globals` built
`sample_ids` from the **2017** tax_units file; `run_one_year` filters every
year to that id set. On vintage 2026060918, projection-year files carry
**935 records whose ids don't exist in 2017 — all weight-1, all gross >
$50M, $8.2T of wealth, ~14.5 expected deaths/yr** (the new top-tail/Forbes
enumeration). The sim silently dropped them from every year of every
scenario. The sunset delta survived because it lives in the $7–15M band
(fully inside the 2017 universe) — a correct delta hiding a wrong level,
the same masking pattern as MODELING_STATUS §4.

Fix (model-wide, not estate-specific): `sample_ids` = union of ids over all
simulation years; `random_numbers` keyed by id and LEFT-JOINED per year in
run_one_year (positional bind_cols breaks once the per-year universe
varies); eitc.R's pre-certification draw switched from positional
`globals$random_numbers$` indexing to the joined column. Consequences to
know: (i) RNG realizations shift (same seed, different id→draw mapping) —
EITC-precert/CDCTC-takeup/bus-loss-style stochastic outputs move within
noise vs old runs; (ii) kg_dynamics cell aggregates now include the
top-tail records (they filter raw Tax-Data by the same sample_ids);
(iii) income-tax aggregates pick up the 935 records (weight-1 each —
negligible there, decisive for estate).

## 12. 2026-06-10 — STAGE 2 BUILT: rank-matching heir allocator + distribution.R rewiring

The §10d allocator is implemented (`src/data/post_processing/estate_allocator.R`,
`allocate_estate_to_heirs()`) and wired into `process_for_distribution()`.
Estate reforms now show distributional effects. Design decisions locked with
user (this session), where they refine or supersede §10d:

1. **DSUE branch split in the ladder.** Each single record contributes up to
   two decedent-ladder entries — (d·p, T_dsue) and (d·(1−p), T_nodsue), taxed
   branches only — NOT an expected-blend T̄. Blending dilutes rates for records
   straddling the unified-credit kink and flattens the λ/x profile near the
   cutoff (same logic as the per-branch indicator blend in
   `get_estate_totals()`). Married records have p_dsue = 0 and fall out of the
   branch machinery with no special case.
2. **On-the-fly, both legs, no file dependency.** The allocator is a pure
   function of (leg detail year-file, baseline heir p/x). It runs inside
   `process_for_distribution()` for the baseline AND reform legs — so there is
   no cross-scenario file race in the SLURM Phase 3b array, and stacked-table
   legs (baseline_id = preceding scenario) work for free. Each scenario still
   writes its own `estate_tax_detail_{t}.csv` (4-column upstream schema) plus
   `estate_allocator_diag_{t}.csv` to `static/supplemental/` for inspection.
   The scenario-specific upstream interface requirement is GONE: the baseline
   Estate-Tax-Distribution file is the only upstream input, and its liability
   column is ignored.
3. **x is GROSS of estate tax — built as an assumption, not yet confirmed.**
   Evidence stands (max tax/inheritance = 0.392 < 0.40 in the old upstream
   file; a net convention would allow 0.67). `inheritance_reform ≡ inheritance`.
   The per-year diag file reports max λ/x; still TODO: confirm convention with
   the 2025092512 vintage's author.
4. **Model liability replaces upstream EVERYWHERE** — baseline leg and all
   scenarios, including non-estate reforms. Death-inclusive presentation
   LEVELS shift from the upstream file's pre-OBBBA assumptions to
   model-baseline λ; deltas for non-estate reforms are unchanged.
5. **λ/x may exceed the statutory top rate, by design.** The gift add-back
   (base = n + γ·reported) makes T/n exceed the top rate for gift-heavy
   estates; that tax belongs to transfers heirs effectively received earlier,
   and folding it into the death-time rate preserves the aggregate identity
   Σw·p·λ = Σd·T exactly (which also ties each leg to `totals/estate.csv`
   est_tax_exp — a free cross-check, asserted in code).
6. **Heir-ladder exhaustion is a hard error.** If a reform's taxed bequest
   mass exceeds total heir inheritance mass, the allocator stops — incidence
   is never fabricated by scaling rates up. Related: taxed estates with ZERO
   distributable value (debts wipe the estate but the gift add-back alone
   exceeds the exemption) carry tax mass with no bequest mass; they are
   dropped from the ladder with a warning and reported in the diag file.
7. **dist_years only.** The allocator runs inside the distribution year loop;
   no whole-sim plumbing.
8. **Deemed realization keeps the proportional smear — PERMANENTLY, by
   design (user ruling; supersedes §10d's "flagged, deferrable" unification).**
   Deemed realization has no exemption threshold — it applies to all transfers
   at death — so proportional-to-inheritance incidence is conceptually correct
   for it. The rank match exists because the estate tax is threshold'd. There
   is no inconsistency to unify away.

Diagnostics shipped per (leg, year): bequest/heir/tax masses, allocated tax +
identity residual, dropped zero-distributable tax, endogenous cutoff x*,
taxed-heir count (raw and weighted), expected taxed estates,
heirs-per-taxable-estate, max λ/x, and λ mass on dependent-return heirs (these
are filtered from the microdata by dep_status == 0 — expected ≈ 0, reported so
a violation would surface).

Unit tests: `other/estate_tax/test_allocator.R` (+ `test_allocator.sbatch`) —
single-estate/many-heir rates, straddling-heir blend, branch split at the kink,
exhaustion error, 1e-10 identity on random ladders, shuffle invariance,
zero-distributable handling. Full-pipeline validation: `tests/estate_sunset`
with dist_years 2026:2027 at pct_sample = 1.

Still open after stage 2: §10h improvement agenda 1–7, and the gross-x
convention confirmation (item 3 above).

### 12a. TODO — estate-splitting heterogeneity (imperfect rank matching)

**The limitation (user-flagged, 2026-06-10).** The allocator is PERFECT
assortative matching: a sharp endogenous cutoff x* below which no heir bears
any estate tax (baseline 2026: zero below $11.2M). Reality has splitting
heterogeneity — a $20M estate splitting 4 ways puts real tax on four $5M
inheritances, while an intact $12M inheritance from a $12M (untaxed) estate
carries none. The true E[rate | x] is a fuzzy declining envelope, not a step.
Rank matching misassigns in both directions inside the top tail: intact heirs
just above x* get taxed with certainty (their actual estates may be exempt),
split heirs below x* get zero (their actual estates may be taxed).

**Why it ships anyway:** the aggregate identity is exact regardless; the
threshold response direction is right; and published tables cut on income
INCLUDING the inheritance, so both the misassigned-to and misassigned-from
heirs sit in the top 1% (mostly top 0.1%) — the error shuffles tax within
reported groups, not across them. It is FIRST-order only for claims at the
inheritance level inside the top tail ("share of sub-$5M inheritances facing
estate tax" is zero by construction).

**Empirical finding — the old upstream file has NO splitting variance either**
(fingerprinted 2026-06-10, vintage 2025092512, year 2026): among its 1,655
taxed heirs there are 1,655 DISTINCT tax/inheritance ratios (proportional
within-estate splits would repeat T_j/n_j across co-heirs — zero repeats ⇒
one heir per estate) and ZERO ratio-vs-x monotonicity violations in 1,654
adjacent pairs, with the ratio ramping smoothly 0.0003 → 0.3923 from x =
$5.36M up. That ramp is the estate tax schedule's own average-rate curve
evaluated at x: upstream "heirs" appear to be estates relabeled (inheritance ≈
distributable value, liability = schedule(x)). So the sharp-cutoff property
predates stage 2; the rank match reproduced (and slightly relaxed — implied
heirs-per-estate 0.89–1.33 vs their exact 1.00) the upstream structural class.
Corollary: the old file's softer marginal-bin rate (0.032 vs our 0.140 at
$5–7.5M, sunset leg) is a CONVENTION difference, not extra realism — their
heir rate is x's own schedule position; ours is the matched estate's average
rate pushed down a thinner heir ladder.

**The fix, when wanted:** imperfect assortative matching. Impose rank
correlation ρ < 1 (equivalently a heirs-per-estate / split-count
distribution) between estate size and inheritance size and spread each
estate's FIXED tax mass over the heir ladder per that copula instead of a
contiguous block. Aggregate identity still holds by construction; the cutoff
smears into a declining P(taxed | x) curve; λ/x stops being deterministic at
the boundary. The binding constraint is the PARAMETER, not the algorithm —
candidate sources, in order of preference:
  1. the upstream Estate-Tax-Distribution model's actual estate→heir
     structure or assumptions (ask the 2025092512 vintage's author — same
     person who owes us the gross-of-tax convention confirmation, §12 item 3;
     one conversation, two open items);
  2. SCF inheritance module (top-tail inheritance amounts vs estate-size
     data → feasible rank-correlation estimate);
  3. crude calibration to ~2–3 heirs per taxable estate from Form 706
     filing patterns.
Diagnostics already in place (heirs_per_estate, cutoff_x, max λ/x) give the
before/after comparison for free when this lands.
