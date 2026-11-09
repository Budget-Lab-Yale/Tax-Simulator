# Behavioral responses in the top-tax project: a methodology

*Budget Lab at Yale — Tax-Simulator. Draft skeleton, 2026-07-15.*

> **Status of this draft.** This is the agreed structure with **one channel
> (capital-gains realization, §4.1) written end-to-end** as the template
> exemplar and **one thin channel (wealth-carry, §4.2) drafted** to show the
> template leveling an under-documented mechanism. Sections 1–3 (the interaction
> story), the standardized channel template (§4.0), and the limitations register
> (§5) are written. The remaining channels (§4.3–§4.11) are titled skeletons
> under the shared template, to be drafted to the same depth. Appendix A carries
> the technical derivations; §A.1 (realization) is written.
>
> Formatting is LaTeX-clean (numbered sections, `$$` display math, a references
> list) so this Markdown converts to a PDF paper without rework.

---

## Contents

- **Part I — Main body**
  - §1 The research question: why interactions, not isolated responses
  - §2 How the model represents behavior
  - §3 The interaction architecture
  - §4 The behavioral channels
  - §5 Limitations and the known-gaps register
  - §6 References
- **Part II — Technical appendix**
  - §A Channel derivations and calibration
  - §B Parameter and provenance table

---

# Part I — Main body

## 1. The research question: why interactions, not isolated responses

The project asks a single question: **how much additional revenue is actually
available at the top of the income and wealth distribution?** Progressive tax
agendas propose to reach the same small group through several statutes at once —
top ordinary rates, capital-gains rates, the treatment of gains at death, the
estate tax, a wealth tax, the corporate tax, QBI repeal, NIIT expansion. Scoring
each in isolation and summing **overstates** the combined capacity, for two
distinct reasons that this document is organized around:

1. **Mechanical base overlap.** The same dollar of wealth is capital income
   during life, an estate at death, a wealth-tax base in between, and corporate
   profit upstream. A dollar taxed away by one statute is not available to
   another. Summing standalone scores double-counts it.
2. **Behavioral re-routing.** Taxpayers respond — by retiming realizations,
   converting labor income into deferred capital gains, shifting income across
   the corporate/pass-through boundary, concealing assets, and financing tax
   payments out of saving. These responses move dollars *between* bases and
   *out* of the taxable universe, and a response provoked by one lever changes
   the base every other lever stands on.

The methodological commitment is to close these loops on **actual record-level
balance sheets** rather than through a single assumed aggregate elasticity. Each
behavioral channel below is a structural response measured against the microdata;
the value of the exercise is precisely the **interaction** among them (§3), which
is why the deliverable reports conventional-as-a-share-of-static and a
destination ledger rather than a headline elasticity.

A note on what "behavioral" means here. We document the full set of *responses*,
but two of them (the wealth-financing bathtub, §4.9, and corporate incidence,
§4.10) are **mechanical conventional-side channels**, not elasticity-driven
taxpayer choices. They belong in a behavioral-methodology document because they
transmit a policy's effect from one base to another, which is the interaction the
project measures; we flag their mechanical nature explicitly (§2, §4.0).

## 2. How the model represents behavior

### 2.1 Static versus conventional

Every non-baseline scenario is run twice. The **static** pass holds all taxpayer
inputs fixed at baseline levels and recomputes tax under the reform — the clean
law-only counterfactual. The **conventional** pass turns the behavioral channels
on. The project's central quantities are differences between the two: the
*static* score is the mechanical intent, the *conventional* score is what
survives behavior, and the gap is where the methodology lives.

The static pass is deliberately kept a pure law-only object. Interactions surface
as `static − conventional` deltas reported through receipts; distribution tables
remain static-sourced by house convention.

### 2.2 Two channel types

| Type | Mechanism | Examples | Where it runs |
|---|---|---|---|
| **Behavior module** | An R file `config/scenarios/behavior/{var}/{name}.R` with a `do_{var}()` hook, dispatched on the behavioral pass; modifies tax-unit inputs, then taxes are recomputed. | kg realization, σ conversion, entity shifting, evasion, wealth avoidance, charity, employment | Behavioral pass, in a **pinned order** (§2.3) |
| **Mechanical conventional-side applier** | A fixed step invoked directly at the head of the conventional pass in `src/sim/run.R`; no `do_*` hook, no elasticity dispatch. | wealth bathtub, corporate incidence | Head of the conventional pass, *before* the behavior modules |

### 2.3 Run order and the no-double-counting discipline

Because several channels touch overlapping bases, the order in which they run is
part of the specification, not an implementation detail. The pinned conventional
sequence is:

$$
\text{corp incidence} \rightarrow \text{wealth haircut} \rightarrow
\underbrace{\text{kg} \rightarrow \sigma\text{ conversion} \rightarrow
\text{entity shifting} \rightarrow \text{evasion}}_{\text{behavior modules, fixed order}}
\rightarrow \texttt{do\_taxes}
$$

The behavior-module order is asserted at runtime so that **no dollar is moved
twice**: realization decisions are set before conversion prices them, conversion
before the entity margin re-routes what remains, and the reporting channels
(evasion, avoidance) run last so that no real-side machinery reads a
concealment-modified frame (§3.2).

## 3. The interaction architecture

Three cross-cutting structures make the channels interact coherently. They are
the reason this is a *top-tax interaction* methodology and not a list of
elasticities.

### 3.1 Shared cross-base objects

Several channels are priced off the **same** internal objects rather than
independent assumptions:

- **$\tau_{eq}(a,t)$** — the present-value tax per dollar entering the
  unrealized-gain state, produced by the realization machinery (§4.1). It prices
  both the σ conversion exit (the founder-equity path, §4.4) and, in the roadmap,
  the entity-shifting deferral value (§4.5).
- **$F$ — the death-forgiveness value** of an unrealized gain, which depends on
  the realization-at-death regime (step-up / carryover / deemed) *and* on estate
  exposure (§4.3). It links realization behavior to the estate tax.
- **$h = \tau_w \cdot \tau_{cg}$ — the wealth-tax carry** on deferred gains
  (§4.2), which couples a wealth tax into the realize-vs-hold decision.

Because these objects are shared, a change to one lever propagates into others
*through the model's own state*, not through a hand-set cross-elasticity.

### 3.2 The reporting-vs-real firewall

The governing principle is: **cash is reported; balance sheets are real.** A
*reporting* response (evasion, wealth avoidance, estate concealment) may affect
exactly one real-side object — the cash tax actually paid (and hence the
bathtub's financing flow, §4.9) — and nothing else. It must never touch the
economic balance sheet (`value.*`), the gain stock, cohort cell assignment, heir
inheritances, or corporate exposure. Conversely, *real* responses (corporate
markdown, wealth haircut, realization, conversion, entity shifting) **should**
flow into every base, including reported ones.

The architecture enforces this by construction: every real-side consumer reads
raw Tax-Data, static detail, or an explicitly preserved raw column — never the
behavior-modified conventional frame — while reporting modules write only to
tax-computation inputs and documented isolation points (the materialized
`net_worth` column, the `estate_concealed_frac` input to `calc_estate`). This is
why the reporting modules are pinned last in the run order (§2.3). The audit
establishing the firewall is `reporting_vs_real_audit.md`; each channel below
declares itself **REAL** or **REPORTING** in its template.

### 3.3 How the ETI is composed (and re-residualized)

The top-bracket **elasticity of taxable income** is not a single assumed
parameter. It is *composed* from three behavior modules — income conversion (σ,
§4.4), entity shifting (§4.5), and evasion (§4.6) — plus charity (§4.7). One of
them, σ, is **calibrated as a residual**: σ is set so that the modeled top-ordinary
ETI matches an external target (≈ 0.25), *conditional on the rest of the stack*.

The consequence, which the methodology must state plainly, is that **any change
to an in-bundle margin forces σ to be re-derived.** Margins that live *outside*
the ordinary-rate ETI bundle — the wealth and estate terms in the realization
Bellman, the estate own-rate response, capital-gains realization itself — do not
disturb σ. This distinction (in-bundle vs out-of-bundle) governs both calibration
maintenance and the sequencing of any future refinement, and it is tracked by an
automated staleness watch (§5, Appendix §B).

## 4. The behavioral channels

### 4.0 The standardized template

Every channel is documented under the same seven headers. Medium-depth content
lives here in the main body; fuller derivations go to Appendix A.

1. **What it captures** — the economic response in one or two sentences.
2. **Mechanism** — how the model produces it.
3. **Type & location** — behavior module vs mechanical; REAL vs REPORTING; code
   path.
4. **Parameters & values** — the pinned numbers and their override knobs.
5. **Calibration & evidence** — how the parameters are set; the literature anchor.
6. **Cross-base interactions** — which other channels/bases it touches, and how.
7. **Limitations & approximations** — what it does not capture (feeds §5).

Channels are grouped into five families:

- **A. Realization & timing at death:** §4.1 realization Bellman · §4.2
  wealth-carry · §4.3 estate death-value offset
- **B. ETI / base-shifting:** §4.4 income conversion (σ) · §4.5 entity shifting ·
  §4.6 evasion
- **C. Wealth & estate own-rate:** §4.7 wealth avoidance + hidden ledger · §4.8
  estate own-rate (Kopczuk–Slemrod)
- **D. Saving-financing:** §4.9 the wealth bathtub
- **E. Corporate & first-order-labor:** §4.10 corporate incidence · §4.11 charity,
  employment, migration

---

### Family A — Realization and timing at death

#### 4.1 Capital-gains realization (the entropy Bellman) — *exemplar, fully drafted*

**What it captures.** How much of a mechanical capital-gains rate increase
survives once realizations respond — investors defer gains when rates rise, and
the value of deferring depends on what happens to the gain at death. Both the
long-run level of realizations and the short-run retiming around a rate change
are modeled.

**Mechanism.** Investors are grouped into representative cells (age cohort ×
within-age net-worth percentile). For each cell the model solves a dynamic
realize-vs-hold problem — a "bathtub" recurrence over the gain stock — under an
**entropy realization cost**. Each dollar of unrealized gain has a non-tax reason
to be sold (a liquidity, rebalancing, or consumption motive) drawn from an
exponential reservation-benefit distribution; the holder realizes the dollar if
that benefit exceeds the tax wedge of realizing now versus deferring. The
survival function of that distribution *is* the realization rate. The resulting
first-order condition has a clean closed form: the discretionary realization rate
in a scenario is the baseline rate scaled by the exponential of the change in the
marginal cost of realizing,

$$
r_D^{\,j} \;=\; r_D^{\,B}\,\exp\!\big(-\eta\,(MC^{\,j}-MC^{\,B})\big),
\qquad \text{clipped to } [0,1],
$$

where the marginal cost of holding rather than realizing,

$$
MC \;=\; \tau \;+\; \beta\,(1-m)\,W_{\text{next}} \;+\; \beta\,m\,F,
\qquad F=(1-c_\phi)\,\tau,
$$

carries the current-year rate $\tau$, the discounted continuation value
$W_{\text{next}}$ of an unrealized dollar (survival probability $1-m$), and the
discounted **death-forgiveness value** $F$ (mortality $m$). The regime enters
through $c_\phi$ — the share of the gain's tax burden the holder internalizes at
death: $c_\phi = 0$ under step-up (death forgives the gain entirely, $F=\tau$),
$c_\phi = 1$ under deemed realization (death triggers full tax, $F=0$), and an
intermediate value under carryover.

Because $d\ln r_D / dMC = -\eta$ **everywhere on the pool**, $\eta$ *is* the
long-run capital-gains realization semi-elasticity directly — there is no
inert-floor or responsive-half deflation (this is the spec-v3 single-pool
property; see §A.1 for the v2→v3 collapse). A separate short-run **timing
overlay** lets a calibrated fraction of realizations retime by ±1 year toward the
lowest-wedge year; it nets to zero under a uniform permanent shock, so it leaves
the long-run response untouched.

**Type & location.** Behavior module — the applier `do_kg_dynamics`
(`config/scenarios/behavior/kg_dynamics/turnover.R`) is a pure allocator that
reads per-year cell state and translates it to per-record `kg_lt` adjustments.
The state is produced by a pre-pass (`kg_dyn_run_bathtub_pass`) in
`src/sim/kg_dynamics.R`; shared cohort primitives are in
`src/sim/cohort_bathtub.R`. **REAL** side (writes `kg_lt` and the deemed
columns).

**Parameters & values.**

| Parameter | Value | Meaning | Override |
|---|---|---|---|
| $\eta$ (`KG_DYN_DEFAULT_ETA`) | **2.4825** | long-run CG realization semi-elasticity | `KG_ETA` |
| timeable share (`KG_DYN_TIMEABLE_SHARE`) | **0.2542** | fraction of realizations that retimes ±1 yr | `KG_TIMEABLE_SHARE` |
| deemed-avoidance haircut (`KG_DYN_DEEMED_AVOIDANCE`) | 0.25 | valuation/noncompliance discount on deemed realizations (data calibration, not law) | `KG_DEEMED_AVOIDANCE` |
| $c_\phi$ | 0 / $\theta$ / 1 | regime death-burden share (step-up / carryover / deemed) | — |
| spec version | 3 (single pool) | — | — |

**Calibration & evidence.** $\eta$ is pinned **directly on the full simulator**
(2026-07-12). The `eta_dial` harness measures the realized full-sim elasticity
$E_{\text{full}}(\eta)=d\log R / d\tau_{rw}$ at simulation-year 30 across a small
grid of $\eta$ values; the surface is linear through the origin, so a few points
pin it. Inverting the fitted line for the literature target
$E_{\text{full}}^{\ast} = -0.6/0.238 = -2.52$ (a top-rate realization elasticity
over the top-rate divisor; Dowd–McClelland–Muthitacharoen and the JCT/CBO
tradition) at slope 1.0155 gives $\eta^{\ast}=2.4825$. The fitted slope and grid
are recorded in `other/top_tax/eta_dial/eta_repin_fit.csv`, so the next re-pin is
arithmetic rather than archaeological. The timeable share is calibrated
separately against the short-run announcement moment given $\eta$ (pinned by a
direct full-sim root-find, because the bathtub dilution is unstable in that
parameter). The older miniature bisection calibrator
(`other/kg_model_tests/calibrate.R`) is retired to a drift diagnostic. Full
procedure in §A.1.

**Cross-base interactions.** This is the most connected channel in the model.
- It produces $\tau_{eq}$ and $F$ (§3.1), which price σ conversion (§4.4) and, in
  the roadmap, entity shifting (§4.5).
- The **death regime** ($c_\phi$) links realization to the estate tax: closing
  step-up (deemed) collapses the deferral shelter and raises realizations.
- The **wealth-carry** term $h$ (§4.2) and the **estate death-value offset**
  (§4.3) both enter $MC$ and $F$, coupling a wealth tax and the estate rate into
  the realization decision.

**Limitations & approximations.**
- The five tracked wealth classes are collapsed into a **single asset bucket**;
  per-asset-class disaggregation is on the roadmap.
- The literal revenue-maximizing grid still argmaxes at the +25pp boundary (peak
  ≈ 44–45% statutory); the grid should be extended to +30/35pp to capture the
  turn-down.
- The deemed-avoidance haircut (0.25) is a JCT-consistent data calibration, not a
  behavioral response; it should eventually be concorded with the estate-side
  per-asset-class valuation discount.

---

#### 4.2 Wealth-carry ($h = \tau_w \cdot \tau_{cg}$) — *thin channel, drafted to template*

**What it captures.** A wealth tax makes deferring a capital gain *more*
expensive, because the unpaid capital-gains tax on an unrealized gain remains in
the wealth-tax base and is taxed at $\tau_w$ every year it is deferred. This
"unlocks" realizations in wealth-tax cells that the realization Bellman would
otherwise leave deferred — and, symmetrically, makes the σ founder-equity
deferral path (§4.4) worth less.

**Mechanism.** A per-year carrying cost $h = \tau_w \cdot \tau_{cg}$ is added to
the marginal cost of holding in the realization Bellman (§4.1) and to the
$\tau_{eq}$ recursion. It is computed at the **record level and then aggregated**
— never as a product of separately-averaged rates — because $\tau_w$ and
$\tau_{cg}$ are positively correlated at the top, so a product of means would
understate it. The term rides the survivor branch only (in-year realizers and
decedents pay no carry that year).

**Type & location.** A structural term inside the kg behavior module
(`src/sim/kg_dynamics.R`, cell-carry aggregation). **REAL** side.

**Parameters & values.** No free parameter — $h$ is a product of two marginal
rates already in the model ($\tau_w$ from `mtr_net_worth`, $\tau_{cg}$ from
`mtr_kg_lt`). Under current law $\tau_w = 0$ for essentially all cells, so
$h = 0$ is a verified **bitwise no-op** — which is why adding the term did not
bump the spec version or invalidate the $\eta$ calibration (calibrated under
$\tau_w = 0$, reproduced exactly).

**Calibration & evidence.** No own calibration; it is a mechanical consequence of
the deferral arithmetic (per-year deferral benefit $\approx r_{\text{real}}\tau$;
a 1% wealth tax erodes roughly half of it at $r_{\text{real}}\approx 2\%$).
Evidence anchor is the deferred-liability logic itself rather than an estimated
elasticity.

**Cross-base interactions.** Couples the **wealth-tax lever** into
**capital-gains realization** and into the **σ conversion price** — exactly the
wealth × capital-gains interaction the atlas conditioning panels feature. Built
and ruled 2026-07-12 (T1–T5 green; +$127B conventional on a 3% wealth dial over
2027–2036).

**Limitations & approximations.** The response is smeared at the **age-cell**
level rather than record level, which by Jensen's inequality *understates* the
record-level response (conservative, never overstates) — measured at −8.5% to
−33% depending on the corner, largest at aggressive revmax corners. The
exposed/unexposed two-state split that closes the gap to ≤1.5% was considered and
**declined** as not worth the extra state; the understatement is disclosed and
should be flagged when quoting wealth × capital-gains corners.

---

#### 4.3 Estate death-value offset — *skeleton*

- **What it captures.** For an estate-taxable decedent, capital-gains/deemed tax
  paid at or before death shrinks the taxable estate, so the effective CG rate
  near death is $\approx \tau_{cg}(1-\tau_e)$ — a discount that makes the model
  *stop* overstating lock-in for estate-exposed cells when the deemed/estate
  switches are on. *(Draft pending — build report:
  `other/top_tax/estate_margins_build_report.html`.)*
- **Mechanism.** *(pending)* Gain-weighted cell aggregation of a switch-gated
  marginal estate rate `mtr_estate_ded` entering $F$ and $MC$ in the Bellman;
  baseline death value $F_B = \tau_B(1-e_B)$, leg-paired.
- **Type & location.** Inside the kg behavior module (`src/sim/kg_dynamics.R`).
  **REAL**.
- **Parameters & values.** *(pending)* Estate-exposure term $e$ from
  `ESTATE_ASSET_COLS`.
- **Calibration & evidence.** *(pending)*
- **Cross-base interactions.** Links the **estate rate** into
  **capital-gains realization** (the crown-pair CG × death-regime panel).
- **Limitations.** *(pending)*

---

### Family B — ETI / base-shifting

#### 4.4 Income conversion (σ) — *skeleton*

- **What it captures.** Top earners converting labor/ordinary income into the
  deferred unrealized-gain state (the founder-equity / carried-interest path), a
  core component of the top-ordinary ETI. *(Draft pending.)*
- **Mechanism.** *(pending)* Wedge $W_i = (\text{own-leg MTR}) - \tau_{eq}(a)$;
  pool = all wages + 0.75·active pass-through (labor-content share); equity leg
  priced by the kg $\tau_{eq}$ recursion.
- **Type & location.** Behavior module `do_conversion`
  (`config/scenarios/behavior/conversion/sigma.R`; shared function in
  `src/sim/sigma_conversion.R`). **REAL**.
- **Parameters & values.** $\sigma = 0.16$ (`SIGMA_CONV`, re-derived 2026-07-12;
  supersedes 0.08).
- **Calibration & evidence.** **Residual** — set so modeled top-ordinary ETI ≈
  0.25 conditional on the stack (§3.3). Provenance inline
  (`SIGMA_CALIB_PROVENANCE`); archived derivation in
  `other/top_tax/archive/{sigma_explainer.md,DESIGN_LOCK.md}`.
- **Cross-base interactions.** Consumes $\tau_{eq}$ (§4.1); in the ETI bundle → a
  σ re-residualization is triggered by any change to §4.5/§4.6/§4.7.
- **Limitations.** τ_eq prices the equity path as corporate-tax-free (misses the
  corporate layer on retained earnings, gap §2.3 of the margins assessment).

#### 4.5 Entity shifting (C ↔ pass-through) — *skeleton*

- **What it captures.** Business income relocating across the
  corporate/pass-through boundary in response to the corporate-vs-individual rate
  differential (Pearce–Prisinzano). *(Draft pending.)*
- **Type & location.** Behavior module `do_entity_shifting`
  (`config/scenarios/behavior/entity_shifting/pearce_prisinzano.R`). **REAL**.
- **Parameters & values.** Shifting semi-elasticity — **β = 0.25 stub** (known
  gap: should be derived from the model's own deferral value $\tau_{eq}/\tau$;
  see §5 and margins assessment §1.3).
- **Calibration & evidence / interactions / limitations.** *(pending; the β stub
  is the headline limitation — kills death-regime responsiveness of the shelter.)*

#### 4.6 Evasion / noncompliance (DHY) — *skeleton*

- **What it captures.** Under-reporting of self-employment, partnership/S-corp,
  and rental income that rises with the net-of-tax rate; the "leak" component of
  the ETI. *(Draft pending.)*
- **Type & location.** Behavior module `do_evasion`
  (`config/scenarios/behavior/evasion/debacker.R`). **REPORTING**.
- **Parameters & values.** Net-of-tax elasticities: Sch C/F 0.046; partnership +
  S-corp 0.052; rent 0.040. Wages/interest/dividends: no response (information
  reporting). Top-end multiplier sweepable.
- **Calibration & evidence.** DeBacker–Heim–Yuskavage; inline `EVASION_PROVENANCE`.
- **Cross-base interactions.** In the ETI bundle (σ trigger); feeds the
  hidden-ledger link (§4.7) via persisted `evasion_g_*`.
- **Limitations.** Positive legs only; overstated-loss margin and top-graded
  multiplier are deferred (margins §2.2).

---

### Family C — Wealth and estate own-rate

#### 4.7 Wealth avoidance + hidden ledger — *skeleton*

- **What it captures.** Reduction in *reported* taxable wealth under a wealth tax
  (legal valuation gaming + concealment), with concealment made **consistent
  across bases** — a hidden dollar escapes income, capital-gains, and estate tax
  too. *(Draft pending.)*
- **Type & location.** Behavior module `do_wealth`
  (`config/scenarios/behavior/wealth/avoidance.R`). **REPORTING** (writes only
  the `net_worth` isolation point + `estate_concealed_frac`).
- **Parameters & values.** Semi-elasticities: marketable −7, closely-held −17.
  Hidden-ledger concealment shares $\chi$: public 1.0, private 0.5.
- **Calibration & evidence.** Author-accepted centrals (2026-07-08); seeded from
  the standalone Wealth-Tax-Simulator. Design: `other/top_tax/hidden_ledger_design.md`;
  firewall: `reporting_vs_real_audit.md`.
- **Cross-base interactions.** Shrinks reported income + estate bases model-wide
  (concealment); the reverse evasion→wealth link reads §4.6.
- **Limitations.** Migration/expatriation is *subsumed* here as a ceiling (§4.11).

#### 4.8 Estate own-rate (Kopczuk–Slemrod) — *skeleton*

- **What it captures.** Reduction in *reported* gross estate in response to a
  change in the net-of-estate-tax rate. *(Draft pending.)*
- **Type & location.** Part of the `do_wealth` module
  (`config/scenarios/behavior/wealth/avoidance.R`, `ESTATE_AVOID_PROVENANCE`).
  **REPORTING** (via `estate_concealed_frac`).
- **Parameters & values.** $\varepsilon = 0.16$ (`ESTATE_REPORT_EPS`; KS band
  0.10–0.22). Net-of-tax power form
  $\text{retained}=((1-\tau_S)/(1-\tau_B))^{\varepsilon}$, keyed on the *change*
  in `mtr_estate` (level avoidance is already in the frozen valuation bridge).
- **Calibration & evidence.** Kopczuk–Slemrod (2001); inline only, no standalone
  memo (a doc gap, §5).
- **Cross-base interactions / limitations.** No CHI on the own-base response;
  charitable-bequest rate response not yet built (margins §1.2b).

---

### Family D — Saving-financing

#### 4.9 The wealth bathtub — *skeleton*

- **What it captures.** A share $s = 1-\text{MPC}$ of the net above-baseline
  during-life tax is financed out of **wealth** rather than consumption,
  compounds over time, and drains into the estate and capital-income bases at
  death — the channel through which a during-life tax quietly erodes future
  wealth-based revenue. *(Draft pending.)*
- **Mechanism.** *(pending)* Cohort recurrence with kernel
  $G(a,p,t)=(1+r_{\text{total}}(t))-s(\tau y+\tau_w)$; forcing
  $F=\Delta T^0-\Delta Y_{\text{exog}}$ (conventional, wealth-excluding).
- **Type & location.** **MECHANICAL** conventional-side applier
  (`src/sim/wealth_dynamics.R`, `src/sim/cohort_bathtub.R`) — *not* a behavior
  module; runs at the head of the conventional pass. **REAL**.
- **Parameters & values.** $s(\text{age},\text{nw pctile})$ profile (calibrated
  default, ≈ 0.1 bottom → 0.80 top), transition matrix $M$; `fmax=0.9`;
  $r_{\text{total}}$ = nominal GDP/capita growth. Config
  `config/wealth/wealth_financing_params.yaml` + profile folders.
- **Calibration & evidence.** `other/wealth_dynamics/default_s_calibration.md`
  (persistent-flow anchor); decision log `plan_review_decisions.md` (D1–D33).
- **Cross-base interactions.** The central *interaction* channel: transmits
  income/CG/wealth-tax paid during life into the estate and capital-income bases.
- **Limitations.** It is *financing*, not a real saving-response elasticity (that
  is deliberately excluded, §4.11); requires a dedicated conv-no-wealth pass
  (≈2× compute).

---

### Family E — Corporate and first-order-labor margins

#### 4.10 Corporate incidence — *skeleton*

- **What it captures.** How a corporate-rate change lands on individual records —
  flow cuts (dividends/interest/rent/PT), an equity markdown on exposed stocks,
  capital-gains adjustment, bathtub dissaving, and an endogenous individual-tax
  offset. *(Draft pending — the most complete existing methodology set:
  `other/corporate_incidence/CONSIDERATIONS.md` (D1–D18) +
  `FORMAL_MODEL.md` (P1–P14).)*
- **Type & location.** **MECHANICAL**, conventional-side, revenue-side
  (`src/sim/corp_incidence.R`); fail-closed activation on `corporate_meta.yaml`.
  **REAL**.
- **Parameters & values.** Hardcoded `CORP_*` constants (several **Phase-0c
  placeholders**, disclosed): ω_div 0.85, ω_kg 0.50, σ_N 0.375, κ 0.40, δ_NIPA
  0.057, ERP 0.05, etc.
- **Calibration & evidence.** *(pending; see the two-doc single source of truth.)*
- **Cross-base interactions.** Generalizes the bathtub forcing to
  $F=\Delta T^0-\Delta Y_{\text{exog}}$ via `corp_dY_exog`; the endogenous offset
  makes the corporate row **not stacking-order-invariant** (disclose).
- **Limitations.** CORP_* placeholders gate the corporate card's honesty label;
  ETR corp-alloc stock exposures share the placeholder status (shown as a
  convention band).

#### 4.11 Charity, employment, migration — *skeleton*

- **Charity (intensive).** `do_charity` (`config/scenarios/behavior/charity/`),
  tax-price elasticity −0.5 on `char_cash`. **Missing:** the appreciated-asset /
  CG-rate giving-price margin (`char_noncash`), a documented gap (margins §2.1).
  In the ETI bundle (σ trigger).
- **Employment (extensive).** `do_employment`
  (`config/scenarios/behavior/employment/bastian.R`), Bastian (2023)
  extensive-margin wage elasticities. First-order for the top-tax question but
  documented for completeness.
- **Migration / expatriation.** **No standalone channel** — subsumed as a
  *ceiling* inside the wealth-avoidance semi-elasticity (§4.7) and disclosed as
  such on the wealth card.
- **Deliberately excluded (state and keep stating):** real labor supply, a real
  rate-of-return saving elasticity (the bathtub is financing, not saving), and
  explicit migration — all excluded by scoring convention.

---

## 5. Limitations and the known-gaps register

This section inherits the candor of `margins_gap_assessment.md` and is
first-class, not a footnote. Two organizing facts frame it: (i) the realization
Bellman prices off a single tax rate plus the shared $F$/$h$/estate terms, so
mispriced cross-lever content shows up as *missing* interaction rather than as
noise; (ii) σ is a residual, so in-bundle gaps carry a re-calibration cost that
out-of-bundle gaps do not (§3.3).

| # | Gap | Where | Direction / size | Status |
|---|---|---|---|---|
| 1 | Entity-shifting **β = 0.25 stub** | §4.5 | shelter is death-regime-invariant; contradicts the model's own deferral logic | **open**, in-bundle (σ re-derive on fix) |
| 2 | **CORP_\* placeholders** (Phase-0c) | §4.10 | corporate card honesty label; ETR band | **disclosed placeholder** |
| 3 | Wealth-carry **age-cell smear** understatement | §4.2 | −8.5% to −33%, conservative (never overstates) | **ruled accepted**, disclose at CG×wealth corners |
| 4 | Appreciated-asset **charitable giving** margin missing | §4.11 | understates giving response + realization removal at high CG rates | **not built**, in-bundle |
| 5 | **Migration** not an explicit channel | §4.11 | subsumed as a ceiling in wealth avoidance | **by convention**, disclosed |
| 6 | Corporate layer absent from **σ conversion price** | §4.4 | founder-equity path priced corporate-tax-free | **not built**, in-bundle |
| 7 | Estate **own-rate**: no rate-responsive charitable bequest; single asset bucket in kg | §4.8, §4.1 | conditioning-base fidelity | **partial / roadmap** |
| 8 | Evasion: positive legs only; flat top-end multiplier | §4.6 | overstated-loss margin, top-grading deferred | **partial** |

**Calibration maintenance.** The residual parameters (σ, and η under its
conditioning assumptions) are kept current by an automated **staleness watch**: a
cheap internal-moment drift check on every tick, and a σ leg re-derivation
triggered by any commit touching the σ staleness list (entity shifting, evasion,
charity elasticity, the σ pool/gate, Tax-Data vintage). The reference values live
in `other/kg_model_tests/calibration_reference.csv`. This is part of the
methodology, not an implementation detail: it is what keeps the "conditional on
the stack" caveat honest.

## 6. References

*(To be completed — Kopczuk & Slemrod (2001); Dowd, McClelland & Muthitacharoen
(2015); DeBacker, Heim & Yuskavage; Pearce & Prisinzano (2018); Bastian (2023);
Saez–Zucman and the wealth-tax avoidance literature; JCT/CBO realization-elasticity
conventions. Full citations pending.)*

---

# Part II — Technical appendix

## A. Channel derivations and calibration

### A.1 Capital-gains realization — full derivation *(written)*

**The dynamic problem.** For a representative cell, let $r_D$ be the discretionary
realization rate on the gain pool. Each unrealized dollar carries a non-tax
reservation benefit $b \ge 0$ to selling, drawn from an exponential distribution;
the holder realizes iff $b$ exceeds the tax wedge of realizing now versus holding.
The **entropy realization cost** $C(r_D)$ is the analytic cost whose marginal is
the inverse survival function of that exponential,

$$
C'(r_D) \;=\; \tfrac{1}{\eta}\,\ln\!\big(r_D / r_D^{\,B}\big),
\qquad C'(r_D^{\,B}) = 0,
$$

so the baseline rate is the zero-cost reference. The holder chooses $r_D$ to
equate the net benefit of realizing, $\kappa - MC$, to the marginal cost
$C'(r_D)$, where $MC = \tau + \beta(1-m)W_{\text{next}} + \beta m F$ and
$\kappa$ is the reservation constant.

**Two-pass solution.**
- *Pass 1 (baseline inversion):* $C'(r_D^{\,B})=0 \Rightarrow \kappa = MC^{B}$
  exactly, interior and corner.
- *Pass 2 (scenario FOC):* $\kappa - MC^{j} = C'(r_D) \Rightarrow$

$$
r_D^{\,j} = r_D^{\,B}\,\exp\!\big(-\eta(MC^{j}-MC^{B})\big),\quad\text{clipped to }[0,1].
$$

Only the upper clip binds; $r_D^{\,B}=0$ cells stay 0. Since
$d\ln r_D/dMC = -\eta$ everywhere and the **whole pool** responds, $\eta$ is the
aggregate long-run semi-elasticity and the aggregate revenue-maximizing rate is
the naive $\approx 1/\eta$ (no inert floor pushing it outward).

**The v2 → v3 single-pool collapse.** Spec v2 split baseline realizations into a
responsive Bellman slice and an inert slice, the latter a point mass of
infinitely-forced sellers — a vestige of the old quadratic (tail-less) cost that
needed an exogenous floor. The exponential reservation-benefit spectrum already
encodes the "first dollar easier to sell than the last" heterogeneity (a thin
tail of robust sellers), so the discrete floor was redundant and it was exactly
what removed the interior aggregate revmax. v3 drops it: one pool, two margins
(level via the Bellman, timing via the overlay). The SSZZ ≈50% "untimeable"
figure moves entirely into the short-run timing channel.

**The short-run timing overlay.** A fraction $f$ (`KG_DYN_TIMEABLE_SHARE`) of all
baseline realizations retimes across ±`TIMING_WINDOW` (=1) years toward the
lowest-wedge year, with move-share $\text{clamp}(\Delta\text{wedge}/\text{ref\_wedge},0,1)$
(`ref_wedge`=0.05: 5pp moves the full bucket, 1pp moves 20%). Composed as a net
shift $r_S = r_{\text{ordinary},S} + (r_{\text{planned},S}-r_{\text{planned},B})$,
it nets to zero under a uniform permanent shock — so it is long-run invariant and
$\eta$ is pinned independently of $f$.

**Calibration procedure ($\eta$).** Retire the miniature-calibrator +
dilution-bridge path. On the full simulator, measure
$E_{\text{full}}(\eta)=\log(R_{\text{shock}}/R_{\text{base}})/\Delta\tau_{rw}$ at
sim-year 30 (shock `s_cg_r25`, conv-no-wealth leg) across a grid; the surface is
linear through the origin. Invert for
$\eta^{\ast}=|E_{\text{full}}^{\ast}|/\text{slope}$ with
$E_{\text{full}}^{\ast}=-0.6/0.238=-2.52$ and measured slope 1.0155, giving
$\eta^{\ast}=2.4825$ (grid $\eta\in\{2,2.3992,3\}\to E_{\text{full}}\in\{-2.05,-2.44,-3.03\}$;
Tax-Data vintage 2026070814). Record the line's coefficients
(`eta_repin_fit.csv`) so the next re-pin is arithmetic. **Calibration
provenance** is guarded: a stale η/timeable_share/vintage warns, and
`KG_STRICT_CALIB=1` hard-stops.

*(Appendix blocks for §4.2–§4.11 to follow at this depth.)*

## B. Parameter and provenance table

| Constant | Shipped | Reference moment | Derived | Code SHA / source |
|---|---|---|---|---|
| `KG_DYN_DEFAULT_ETA` | 2.4825 | full-sim $E_{\text{full}}=-2.52$ / slope 1.0155 | 2026-07-12 | `eta_repin_fit.csv` |
| `KG_DYN_TIMEABLE_SHARE` | 0.2542 | full-sim short-run semi | 2026-07-09 | a4bbac590 |
| `SIGMA_CONV` | 0.16 | top-ordinary ETI ≈ 0.25 | 2026-07-12 | `SIGMA_CALIB_PROVENANCE` |
| `ESTATE_REPORT_EPS` | 0.16 | KS reported-estate elasticity (band 0.10–0.22) | 2026-07-12 | `ESTATE_AVOID_PROVENANCE` |
| Evasion (SchC/PT/rent) | 0.046 / 0.052 / 0.040 | DHY net-of-tax elasticities | — | `EVASION_PROVENANCE` |
| Wealth avoidance (mkt/closely-held) | −7 / −17 | author-accepted | 2026-07-08 | Wealth-Tax-Simulator seed |
| Hidden-ledger $\chi$ (pub/priv) | 1.0 / 0.5 | concealment consistency | 2026-07-08 | `hidden_ledger_design.md` |
| Entity-shifting β | **0.25 (stub)** | — (should be $\tau_{eq}/\tau$) | — | gap, margins §1.3 |

*(The reference-moment column doubles as the staleness-watch source of truth,
Appendix cross-ref `calibration_reference.csv`.)*
