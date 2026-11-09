# Accrual Taxation of Capital Gains — Design Memo (v0)

Status: DESIGN / pre-build. Target variant (per JR, 2026-07-11): **hybrid — MTM on
marketable assets, deferral (retrospective interest) charge on illiquid assets**.
Audience: whoever builds `calc_accrual()`. Companion to the estate/kg/corp
design docs.

---

## 0. The question this memo answers

"We have accruals, we have a kg dynamics module, can we just tax it?" Not by
taxing the `accruals.*` column — that's the wrong object and it double-counts.
The **deferral (stock-event) tier is buildable and consistent**. But **true
annual mark-to-market is in genuine tension with the data structure**, and the
holding-period problem turns into an *input* the deferral tier consumes.

> **v1 CORRECTION (2026-07-11, JR).** Tax-Data is a **repeated representative
> cross-section, not a panel** — you cannot track a record over time; that is
> exactly why kg/wealth use age-**cohort** machinery. This invalidates the v0
> §3.1 "near-stateless MTM = `value(t)−value(t−1)` per record" finding: there is
> no per-record value path to difference. `kg_dyn_step_recurrence` confirms the
> pattern — it propagates only the policy-induced **delta** `delta_G` through
> cohort cells and **re-reads the baseline gain stock `G_B` fresh from each
> year's cross-section**. Sections below are rewritten accordingly. Net effect:
> the deferral tier is the workhorse; literal annual MTM is the hard part and is
> reframed in §3.

## 0.5 Governing constraint: only STOCK events are clean

The repeated cross-section can represent, per year, the **cross-sectional gain
STOCK** `G = value − basis` and rate-based events on it (realization at rate `r`,
death at rate `m`). It CANNOT observe a record's **accrual FLOW** (Δvalue),
because the "same" record next year is a different sampled person.

Consequences:
- **Deemed-at-death and realization taxes are clean** — they hit the stock at a
  within-year event, no flow, no panel. That is why the existing deemed regime is
  consistent.
- **A deferral/interest charge is clean** — also a within-year stock event
  (§4), for BOTH liquid and illiquid assets.
- **Annual MTM is intrinsically a flow** and has no panel to difference. It can
  only be *approximated*, and every approximation reintroduces the exact
  consistency problem that prompted this memo (§3). This validates JR's original
  worry: `accruals.*` (a normal-return imputation) is the model's *only*
  accrual-flow object precisely because you can't difference a cross-section, and
  it does not reconcile with the independently-imputed gain-stock levels.

---

## 1. The three objects, and why `accruals.*` is the wrong one to tax

Per-record, per-year, all from the Tax-Data interface:

| object | definition | current use |
|---|---|---|
| `value.{k}` / `basis.{k}` | projected balance sheet | kg gain stock, estate, wealth |
| `gain.{k} = max(0, value−basis)` | unrealized-gain **stock** | `kg_dynamics`, `estate` |
| `accruals.{k}` | `value.{k} × r_hist,k`, a normal-return **flow** proxy | HS income def only (`distribution.R:221`) |

Asset classes (k): `equities`, `pass_throughs`, `primary_home`, `other_home`,
`re_fund`.

`accruals.{k}` is a smoothed steady-state yield: always positive, no volatility,
no loss years. Taxing it annually is **not** an accrual tax — over a holding
period `Σ (value·r_hist) ≠` terminal gain, and there is no true-up at sale. It
would be a soft imputed-yield tax that never reconciles to realized gains.

**The real consistency trap is basis, not the rate.** `basis.{k}` is a static
projected figure; nothing steps it up as gains accrue. The model *already* taxes
`G = value − basis` at realization (kg realization rate) and at death (deemed
regime). Layer an annual accrual tax on top **without resetting basis** and every
dollar is taxed twice — once accruing, once when `G` realizes/deems.

Corollary that drives the whole design: **a coherent accrual tax must be
basis-consistent with the existing realization/deemed machinery.** MTM achieves
this by resetting basis to value each year (residual `G → 0`, so kg/estate
collect ~nothing extra — automatic no-double-count). Deferral achieves it by
*not* accruing during life and instead surcharging the existing realization/death
tax for the time value of deferral.

---

## 2. What's already built that we reuse

- **Deemed realization at death IS an accrual tax** — the "accrue, collect at the
  terminal date, no interest" variant. `pref.kg_death_regime_{k}=2` + `estate`,
  with holder-internalized burden `c_phi` in the Bellman and a valuation haircut
  (`KG_DYN_DEEMED_AVOIDANCE`). Consistent because it fires once, on the actual
  accumulated `G`.
- **Per-class regime resolution** (`kg_dyn_build_regime_mix`) already lets each
  asset class carry a different treatment. The hybrid adds a *lifetime* regime
  axis alongside the existing *death* regime axis.
- **`kg_dynamics` realization rate** tells us WHEN illiquid gains realize →
  directly feeds the deferral-charge holding math.
- **Wealth bathtub** (`wealth_dynamics.R`) already compounds a during-life tax
  into wealth and drains it to the estate base — the accrual tax is just another
  `ΔT⁰` term in its forcing `F = ΔT⁰ − ΔY_exog`.
- **`calc_wealth`** is the template for `calc_accrual()`: pure, per-record,
  weight-free, reform-overridable YAML.

---

## 3. Tier A — "MTM" on marketable assets (`equities`, `re_fund`)

There is no per-record `value(t)−value(t−1)` to tax (§0.5). Annual accrual must be
represented some other way. Three routes, worst to best:

### 3.1 (rejected) Tax an imputed accrual flow

Tax `τ · (r_hist,k · value_k)` — i.e. tax `accruals.*` — annually. **Rejected.**
`r_hist·value` (a normal-return flow) and `value−basis` (the stock) are
independently imputed in Tax-Data and do not reconcile: over a holding period the
imputed flow does not integrate to the observed terminal gain, so a reform-basis
ledger built from it drifts (can exceed value or go negative), and it double-counts
against the kg realization + deemed machinery, which taxes the stock. This is
precisely the multi-year inconsistency JR flagged. Do not build this.

### 3.2 (viable) MTM as a cohort reform-basis bathtub

Represent the reform's basis step-up as a **cohort-cell state**, in the
`wealth_dynamics`/kg mold: track, per (age cohort × asset class), the cumulative
already-marked gain `B_reform(a,k,t)` under the reform. Each year:
`accrual base = clip(G_B(a,k,t) − aged B_reform, ≥0)`, tax it, add it to
`B_reform`, age the cell forward (mortality, aging, inheritance) with the kg law
of motion, and **allocate the cell-level `B_reform` back onto records** via a
kg-applier / estate-allocator-style rule (the cross-section it lands on next year
is a different sample). Stateful — a new cohort bathtub + state files + an
allocator + SLURM phase (wealth_dynamics-class, NOT corp_incidence-class). The
consistency win of a per-record basis reset (residual `G→0` at death ⇒ no
double-count) survives only in cohort-aggregate expectation, and the flow must
still be pinned to the model's gain-stock representation (`G_B`), not `r_hist`, to
stay data-consistent. This is the honest "real MTM" build and it is expensive.

### 3.3 (recommended) MTM as short-period deferral — collapse Tier A onto Tier B

The cleanest data-consistent move: represent liquid assets NOT with a literal
annual flow but with a **very short imputed holding period / near-annual
constructive realization** under the Tier B deferral machinery (§4). Liquid assets
turn over fast, so `H_k` (§4.1) is already small for them; MTM is the `H → ~1`
limit of a deferral charge. This keeps everything on **stock events the
cross-section can support**, needs no new cohort bathtub, no allocator, no SLURM
phase, and degrades gracefully between "pure MTM" (short H) and "realization +
interest" (data-implied H). The economic difference from literal MTM is the
timing of within-holding-period appreciation, which the repeated cross-section
cannot resolve anyway. **Recommendation: build Tier A this way; reserve §3.2 for a
later, separately-scoped "true MTM" extension if a client demands literal
mark-to-market.**

### 3.4 Losses and the volatility worry

Under §3.3, losses are handled the same way realizations already net within the kg
stock; there is no separate annual loss-carryforward ledger to persist. (Under the
rejected §3.1 and the heavy §3.2, asymmetric losses are an extra state variable and
the volatility JR flagged bites on the timing of offsets.)

---

## 4. Tier B — deferral charge on illiquid assets (`pass_throughs`, homes)

No annual accrual. Gains are taxed at the existing realization event (kg rate) or
at death (deemed regime), **plus an interest surcharge** for the deferral period
(Auerbach retrospective / lookback).

### 4.1 Holding period — the flagged gap, resolved by the flagged object

The model tracks no acquisition date. But impute the average holding period from
data already present, using — pleasingly — the very `accruals.*` rate that was the
wrong thing to *tax*:

```
r_hist,k = accruals_k / value_k                        # implied historical return
H_k      = ln(value_k / basis_k) / ln(1 + r_hist,k)    # implied holding period
```

So `accruals.*` is the *right* object for holding-period imputation even though
it's the wrong object for the accrual base. Guard the degenerate cases
(`basis ≥ value`, `r_hist ≤ 0`, `value ≤ 0`) with an assumed floor/cap H.

### 4.2 The charge

At a realization/death event on gain `G_k`:

```
liab += τ · G_k · [ (1 + i)^{H_k} − 1 ] / (i · H_k)    # or the simpler (1+i)^H−1 markup;
                                                        #   pick the lookback convention in FORMAL_MODEL
```

where `i` is the statutory deferral rate (YAML). This **rides the existing kg
realization + deemed machinery**, adding only a per-record multiplier — cheap.
Stateless (H from current-year value/basis/accruals).

---

## 5. Consistency with the existing behavioral stack

- **kg realization response must be neutralized on Tier A assets.** Under MTM
  there is no lock-in — gains are taxed regardless of sale — so `kg_dynamics` must
  NOT also model a discretionary realization response on marked assets. The
  per-class regime already lets us zero this out for `equities`/`re_fund` in an
  MTM scenario. (Tier B assets keep their kg realization response; the deferral
  charge is a level surcharge on top.) **This is a required consistency edit, not
  optional.**
- **Wealth bathtub:** the MTM/deferral liability is a during-life tax → enters
  `ΔT⁰`, drains wealth, feeds the estate base. Composes with the wealth channel
  for free.
- **Estate/deemed no-double-count:** under the recommended §3.3 collapse, Tier A
  IS a Tier B deferral event, so a marked/realized gain leaves the stock the same
  way a realization does and the deemed-at-death path sees only the un-marked
  residual — no double-count, by the same accounting as an ordinary realization.
  (Under the heavy §3.2, this holds only in cohort-aggregate expectation and must
  be checked via the conservation diagnostic, not assumed per-record.)
- **Thresholds:** Wyden-style applicability (e.g. >$1B net worth or >$100M income
  for 3 yrs) is trivially available — `net_worth` is a materialized column and
  income is computed. A YAML threshold gates the whole calc per record.

---

## 6. Where it plugs in (build surface)

1. **`config/scenarios/tax_law/baseline/accrual.yaml`** — reform-overridable, all
   dormant at baseline. Params: per-class treatment `{none, mtm, deferral}`;
   `τ_mtm`; deferral rate `i`; built-in-gain phase-in schedule; loss treatment;
   net-worth/income applicability threshold.
2. **`src/calc/functions/tax/accrual.R` → `calc_accrual()`** — pure, weight-free,
   per-record. Mirrors `calc_wealth`. Produces `liab_accrual` (+ detail columns:
   `accrual_base_mtm`, `deferral_charge`, imputed `H` for diagnostics).
3. **`do_taxes.R`** — call `calc_accrual()`, fold `liab_accrual` into total
   liability and into the `ΔT⁰` bundle the wealth bathtub reads.
4. **`kg_dynamics` regime glue** — suppress the realization response on MTM-tier
   classes when a scenario marks them (§5).
5. **Receipts/totals** — new `liab_accrual` column flows through aggregation like
   `liab_wealth` did.

### SLURM impact
- **Recommended path (§3.3 + §4): none.** Both tiers are stock-based, within-year,
  stateless (H imputed from current-year value/basis/accruals); `calc_accrual()`
  lives inside `run_one_year()`/`do_taxes` — the "safe changes" category in
  CLAUDE.md, like `corp_incidence`.
- **Heavy path (§3.2 true MTM): full wealth_dynamics-class add** — new cohort
  bathtub pass, per-year state files, a record allocator, and the SLURM sync table
  entries (new phase, setup manifest, worker dispatch, `reconstitute_environment`).
  Do not take this on without a client requirement for literal mark-to-market.

---

## 7. Open decisions before build

- [ ] **Tier A route: recommended §3.3 (short-period deferral) vs. heavy §3.2
      (true cohort MTM bathtub).** This is now the #1 decision — it sets the entire
      build size (stateless calc vs. new SLURM phase). Record-id stability is NO
      LONGER the question; there is no panel, full stop.
- [ ] Deferral-charge formula convention (simple `(1+i)^H−1` vs. annuitized) —
      pin in a short FORMAL_MODEL section. This is the core of both tiers under §3.3.
- [ ] Liquid-asset `H` under §3.3: imputed from `accruals.*` (data-implied
      turnover) vs. a hard `H=1` cap (literal-MTM proxy) vs. a policy floor.
- [ ] Built-in-gain phase-in at enactment: one-time recognition vs. N-year
      schedule (a within-year stock event either way, so cross-section-friendly).
- [ ] Which rate applies (ordinary vs. preferential), and time-variation.
- [ ] Class→tier assignment: default `equities,re_fund → short-H`;
      `pass_throughs,primary_home,other_home → data-H deferral`. Confirm homes
      aren't simply exempt (most proposals exempt primary residence under a cap —
      §121 already modeled).

---

## 8. Bottom line

The repeated cross-section (no panel) is the governing fact: only STOCK events —
realization, death, a deferral/interest charge — are internally consistent, and
literal annual mark-to-market has no per-record flow to tax. So:

- The **deferral tier (§4) is the workhorse** and is clean for both liquid and
  illiquid assets: stock-based, within-year, stateless, `corp_incidence`-class.
- **Recommended Tier A (§3.3): collapse "MTM" onto a short imputed holding period**
  in the same deferral machinery. Pure MTM is the `H→1` limit; the timing detail
  it drops is unresolvable in the data anyway.
- True cohort-bathtub MTM (§3.2) is a `wealth_dynamics`-class build — defer unless
  a client demands literal mark-to-market.

On JR's two original worries: both are **vindicated, not sidestepped**. The
historical-average accrual rate genuinely cannot be reconciled with the gain-stock
levels over a multi-year horizon (§3.1 rejected) — which is exactly why we route
around annual flows entirely and tax stocks at events. And the holding period,
untracked, is what the deferral charge needs — imputed from `accruals.*` (§4.1),
turning the object we can't tax into the object that makes the whole design work.
