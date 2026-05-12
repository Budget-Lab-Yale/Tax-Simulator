# Representative-Cell Bellman Proposal for Capital Gains Dynamics

**Author:** Design memo following eta-degeneracy review
**Status:** Proposal for a replacement realization model; not current implementation
**Date:** 2026-05-12

This document proposes a revised capital-gains dynamics model for
`Tax-Simulator`. The goal is to keep the core insight of the existing
`kg_dynamics` architecture--a synthetic-cohort bathtub that tracks
policy-induced changes in unrealized gains--while replacing the current
closed-form Bellman realization function with a representative-cell dynamic
program in which realization rates are chosen directly.

The proposal is deliberately partial-equilibrium. It is a model of the
timing of taxable realization, not a full model of household saving,
portfolio choice, labor supply, or lifetime consumption.

---

## 1. Motivation

The current Bellman revision in `src/sim/kg_dynamics.R` was designed to
improve on a reduced-form realization elasticity by letting taxpayers
internalize future realization opportunities. Mechanically, it solves a
per-unit Bellman equation for the value of holding one dollar of unrealized
gain, then uses an exponential response rule:

$$
r^{D,S}_{a,t}
= r^{D,B}_{a,t}\exp\{-\eta(P^S_{a,t} - P^B_{a,t})\}.
$$

The accompanying degeneracy note shows that, under the current functional
form, the response parameter $\eta$ cancels from the realized response. The
model runs, but varying $\eta$ does not change the implied realization
elasticity. This is a structural problem, not a numerical calibration issue.

The deeper issue is that the current formulation asks one object to do two
jobs:

1. define the curvature of the holder's nontax realization benefit; and
2. provide a free empirical response parameter calibrated to realization
elasticities.

Recovering the cell intercept from baseline realizations then makes the
Bellman value scale mechanically with the same parameter that appears in
the exponential response. The calibration handle disappears.

This proposal takes a different route. Instead of deriving a tax-price wedge
and applying a separate closed-form response, the model solves directly for
the realization share chosen by a representative cell. Baseline and scenario
realization paths are both outputs of the dynamic program.

---

## 2. Modeling Closure

Tax-Simulator is a repeated cross-section. It observes tax units by year but
does not follow the same household longitudinally. The capital-gains module
therefore cannot honestly maintain record-level dynamic wealth histories.
The natural dynamic unit is a representative age-year cell, treated as a
synthetic cohort.

The proposed closure is:

* The underlying asset/unrealized-gain path is exogenous and anchored in the
  baseline repeated cross-sections.
* Realization behavior changes the timing of taxable recognition and creates
  policy-induced deviations from that baseline gain stock.
* New unrealized gains enter the cell stock additively through an exogenous
  baseline inflow process. They are not mechanically proportional to the
  share of old embedded gains that the cell chose not to realize.
* Earlier or later tax payments are financed through an unmodeled outside
  margin: lower consumption, lower saving in other assets, borrowing, or
  higher labor supply.
* The discount factor $\beta$ prices that outside-margin timing cost.
* The model does not feed tax payments back into the gross asset path or
  into future economy-wide saving.

Thus, the model does not say:

$$
\text{tax paid today} \Rightarrow
\text{lower modeled asset stock tomorrow}.
$$

Instead, it says:

$$
\text{tax paid today} \Rightarrow
\text{outside budget margin absorbs the payment today},
$$

while the cell's baseline asset/gain accumulation process remains anchored
to the observed or imputed baseline path. This keeps the model a
capital-gains realization-timing model rather than a full household
savings model.

---

## 3. Objects and Primitives

### 3.1 Indices

* $a$ indexes age cells.
* $t$ indexes simulation years.
* $S$ indexes a reform scenario.

The core Bellman state is $(a,t)$. A later extension may add an embedded
gain ratio state $e$ or a small number of embedded-gain-ratio bins, but the
first version should be age-year only.

### 3.2 Baseline Cell Quantities

Each cell carries baseline objects already used by the current bathtub:

* $G^B_{a,t}$: baseline unrealized taxable gain stock.
* $R^B_{a,t}$: baseline positive realized gains.
* $r^B_{a,t} = R^B_{a,t}/G^B_{a,t}$: baseline realization rate.
* $m_{a,t}$: effective cell mortality, preferably gain-weighted.
* $\tau^B_{a,t}$: baseline average capital gains marginal tax rate.
The baseline stock path may also contain an implicit exogenous additive
inflow of new unrealized gains. This residual includes asset appreciation,
new saving, portfolio composition changes, ordinary inheritance flows already
present in baseline, data imputation, and other processes that make the
repeated-cross-section stock $G^B$ change over time. It is not a compounding
return on the old embedded-gain stock, and it does not need to be computed
for the Bellman.

Scenario tax rates are $\tau^S_{a,t}$. Death-regime parameters may also
vary by scenario and year.

### 3.3 Two-Channel Realization

Total realizations split into an inelastic turnover component and a
discretionary component:

$$
r_{a,t} = \lambda^T_{a,t} + r^D_{a,t}.
$$

The turnover component captures sales that occur for relatively tax-insensitive
reasons: liquidity shocks, business exits, divorce, household moves, and other
events outside the timing model. A simple first version can retain the current
rule:

$$
\lambda^T_{a,t} = \phi_I r^B_{a,t}.
$$

The Bellman chooses only $r^D_{a,t}$, subject to
$0 \le r^D_{a,t} \le 1-\lambda^T_{a,t}$.

### 3.4 Death Regime

Death enters through the unrealized gains not realized during the current
period. The important object is the value of reaching death with one dollar
of unrealized gain still embedded. A first version should model this as a
tax-liability-forgiveness value, not as a general dynastic bequest utility.

Let $F_{a,t}$ denote the death-state forgiveness value internalized by the
current holder:

$$
F_{a,t} =
\begin{cases}
\tau_{a,t}, & \text{step-up basis},\\
(1-\theta)\tau_{a,t}, & \text{carryover basis},\\
0, & \text{deemed realization}.
\end{cases}
$$

Under step-up, death forgives the capital-gains tax liability, so the value
of holding an unrealized dollar into death is approximately $\tau$. Under
deemed realization, no liability is forgiven, so this special death value is
zero. Under carryover, the liability is not forgiven at the family level, but
the current holder may not fully internalize the heir's future tax burden.
The parameter $\theta \in [0,1]$ is the internalized share of the heir-side
future tax burden; when $\theta=1$, carryover is valued like deemed
realization for the current holder, and when $\theta=0$, it is valued like
step-up.

This $F$ parameter is not the physical stock-routing rule. It is the
holder's reduced-form valuation of tax forgiveness at death for the old
embedded gains whose realization decision is being modeled. Physical stock
routing remains in the bathtub:

* under step-up, decedent unrealized gains vanish;
* under deemed realization, decedent gains are taxed and removed;
* under carryover, decedent unrealized gains are routed to heir cells.

Because new gain inflows are additive and exogenous, they do not enter the
marginal death value of realizing one additional dollar of old embedded gain.
Those new gains are still present in the dollar stock accounting, but they
are not multiplied by the current-period realization share.

---

## 4. Representative-Cell Bellman

### 4.1 Per-Unit Normalization

The Bellman is normalized to one dollar of unrealized gain in cell $(a,t)$.
This avoids modeling the full asset and basis account. It is consistent with
the current bathtub closure: the dynamic program chooses realization shares,
and the bathtub scales those shares by the cell's dollar stock of gains.

Let $W^j_{a,t}$ be the per-dollar value of an unrealized gain in environment
$j \in \{B,S\}$, where $B$ is baseline and $S$ is a scenario.

Let $b_{a,t}(r^D;\kappa_{a,t},\psi)$ be the reduced-form nontax benefit from
discretionary realization. Here:

* $\kappa_{a,t}$ is a cell-specific benefit/intercept chosen to reproduce
  baseline realization rates.
* $\psi$ is a global or low-dimensional curvature/responsiveness parameter
  calibrated to an empirical realization elasticity target.

The benefit is not a full consumption utility function. It represents
liquidity, diversification, rebalancing, consumption needs, and other nontax
motives for realizing gains.

### 4.2 Bellman Equation

For a given environment $j$, tax path $\tau^j$, and death-regime forgiveness
path $F^j$, the representative cell solves:

$$
\boxed{
W^j_{a,t}
=
\max_{r^D \in [0,\,1-\lambda^T_{a,t}]}
\left\{
b_{a,t}(r^D;\kappa_{a,t},\psi)
- \tau^j_{a,t} r^D
+ (1-\lambda^T_{a,t}-r^D)
\left[
\beta(1-m_{a,t})W^j_{a+1,t+1}
+ \beta m_{a,t}F^j_{a,t}
\right]
\right\}.
}
$$

The terms are:

* $b_{a,t}(r^D;\kappa,\psi)$: nontax benefit of realizing now.
* $-\tau r^D$: current tax cost of discretionary realization.
* $(1-\lambda^T-r^D)\beta(1-m)W_{a+1,t+1}$: discounted continuation
  value of gains that survive and remain unrealized.
* $(1-\lambda^T-r^D)\beta m F$: discounted value of tax-liability
  forgiveness if the holder dies before realizing.

There is no multiplicative $(1+g)$ term in the Bellman. The baseline
gain-stock residual is treated as an additive inflow of new gains, not as a
return on the old embedded gains that remain after current-period
realizations. Since that inflow is not caused by today's marginal realization
decision, it does not enter the first-order condition.

This positive death branch is what generates the standard step-up lock-in
result. Under step-up, $F=\tau$, so higher mortality increases the value of
holding unrealized gains. Under deemed realization, $F=0$, so that
step-up-specific death option disappears.

The Bellman is solved by backward induction over age and year. At the
terminal age, continuation value is zero or set by a terminal condition. At
the terminal simulation year, the same stationary-tail convention used by
the current implementation can be retained.

### 4.3 First-Order Condition

For an interior solution, the optimal discretionary realization rate satisfies:

$$
\frac{\partial b_{a,t}(r^D;\kappa,\psi)}{\partial r^D}
=
\tau^j_{a,t}
+ \beta(1-m_{a,t})W^j_{a+1,t+1}
+ \beta m_{a,t}F^j_{a,t}.
$$

The right-hand side is the marginal cost of realizing one more dollar of
unrealized gain today:

* current tax paid now;
* continuation value forgone by reducing future unrealized gains;
* death-state forgiveness value forgone if the holder would otherwise die
  with the gain unrealized.

This formulation intentionally does not derive an exponential response rule.
The Bellman itself supplies $r^{D,B}_{a,t}$ and $r^{D,S}_{a,t}$.

### 4.4 Candidate Benefit Function

A convenient first functional form is a concave benefit with cell intercept
and global curvature, such as:

$$
b_{a,t}(r;\kappa_{a,t},\psi)
=
\kappa_{a,t}r - \frac{\psi}{2}r^2.
$$

Then:

$$
b'_{a,t}(r;\kappa,\psi) = \kappa_{a,t} - \psi r.
$$

For an interior solution:

$$
r^{D,j}_{a,t}
=
\frac{\kappa_{a,t} - MC^j_{a,t}}{\psi},
$$

where

$$
MC^j_{a,t}
=
\tau^j_{a,t}
+ \beta(1-m_{a,t})W^j_{a+1,t+1}
+ \beta m_{a,t}F^j_{a,t}.
$$

The implementation should still solve the bounded maximization problem
directly, because corner solutions are economically meaningful:

$$
r^{D,j}_{a,t} \in [0,\,1-\lambda^T_{a,t}].
$$

The quadratic form is not mandatory. It is a transparent starting point
because one parameter shifts the level of baseline realization motives
($\kappa$), while another controls response curvature ($\psi$).

---

## 5. Bathtub Interface

The bathtub remains the synthetic-cohort law of motion for the dollar stock
of unrealized gains. The Bellman replaces only the source of realization
rates.

For each scenario and year, the Bellman pre-pass returns:

* $r^{D,B}_{a,t}$: model-implied baseline discretionary realization rate.
* $r^{D,S}_{a,t}$: model-implied scenario discretionary realization rate.
* $r^B_{\text{model},a,t} = \lambda^T_{a,t} + r^{D,B}_{a,t}$.
* $r^S_{\text{model},a,t} = \lambda^T_{a,t} + r^{D,S}_{a,t}$.

The ratio applied to observed baseline realized gains can be:

$$
\text{rate\_factor}_{a,t}
=
\frac{r^S_{\text{model},a,t}}{r^B_{\text{model},a,t}},
$$

with safeguards for sparse cells. If calibration exactly reproduces the
observed baseline realization rate, then
$r^B_{\text{model},a,t} = r^B_{a,t}$ and this ratio matches the observed
baseline denominator.

The existing bathtub recurrence can then be reused:

$$
\Delta G_{h,t+1}
=
\sum_a A_{a\to h}(1-m^{\text{eff}}_{a,t})
\left[
(1-r^S_{a,t})\Delta G_{a,t}
+ G^B_{a,t}(r^B_{a,t}-r^S_{a,t})
\right]
+ \delta_{\text{route}}
\sum_a \omega_{a\to h}m^{\text{eff}}_{a,t}(G^B_{a,t}+\Delta G_{a,t}).
$$

This recurrence is a delta equation around an exogenous baseline stock path.
One way to see the additive-inflow closure is to write the full baseline
stock equation as:

$$
G^B_{h,t+1}
=
\sum_a A_{a\to h}(1-m^{\text{eff}}_{a,t})(1-r^B_{a,t})G^B_{a,t}
+ \delta^B_{\text{route}}
\sum_a \omega_{a\to h}m^{\text{eff}}_{a,t}G^B_{a,t}
+ I^B_{h,t+1}.
$$

The scenario equation uses the same exogenous inflow $I^B_{h,t+1}$:

$$
G^S_{h,t+1}
=
\sum_a A_{a\to h}(1-m^{\text{eff}}_{a,t})(1-r^S_{a,t})G^S_{a,t}
+ \delta^S_{\text{route}}
\sum_a \omega_{a\to h}m^{\text{eff}}_{a,t}G^S_{a,t}
+ I^B_{h,t+1}.
$$

Under the usual current-law step-up baseline, $\delta^B_{\text{route}}=0$.
Subtracting baseline from scenario then cancels the additive inflow and gives
the displayed $\Delta G$ recurrence. That is why the bathtub can track only
$\Delta G$ while still respecting the fact that new gain creation is not
mechanically reduced when old embedded gains are realized.

This cancellation is also why the additive inflow does not need to be an
explicit implementation input. More generally, if

$$
G^j_{t+1}=H^j(G^j_t,r^j_t)+I_{t+1}
$$

for $j\in\{B,S\}$ and the inflow $I_{t+1}$ is exogenous and common to
baseline and scenario, then

$$
\Delta G_{t+1}
=G^S_{t+1}-G^B_{t+1}
=H^S(G^S_t,r^S_t)-H^B(G^B_t,r^B_t).
$$

The common additive inflow drops out. It can rationalize growth in the
baseline stock path without entering the Bellman first-order condition or
the $\Delta G$ recurrence.

Under carryover, the Bellman uses the reduced-form forgiveness value
$F=(1-\theta)\tau$, while the bathtub handles physical heir routing through
$\omega$. This deliberately avoids a fully dynastic Bellman in the first
version.

---

## 6. Calibration

Calibration has two jobs:

1. match baseline realization levels by cell; and
2. match an empirical aggregate realization elasticity.

These should be assigned to different parameters.

### 6.1 Baseline Inversion

For each cell, recover $\kappa_{a,t}$ so that the baseline Bellman reproduces
the observed discretionary baseline realization rate:

$$
r^{D,B}_{a,t} = r^B_{a,t} - \lambda^T_{a,t}.
$$

With the quadratic benefit and an interior solution, this inversion is:

$$
\kappa_{a,t}
=
MC^B_{a,t} + \psi r^{D,B}_{a,t}.
$$

Because $MC^B_{a,t}$ depends on $W^B_{a+1,t+1}$, which itself depends on
future $\kappa$ values, the baseline inversion should run backward over
age/year alongside the Bellman solve. In practice:

1. start from terminal values;
2. compute continuation value;
3. choose $\kappa_{a,t}$ to make observed $r^{D,B}_{a,t}$ optimal;
4. compute $W^B_{a,t}$ at that observed choice;
5. move backward.

This is analogous to recovering cell fixed effects. The $\kappa_{a,t}$
objects are nuisance calibration parameters, not the main economic
elasticity parameter.

The recovered $\kappa_{a,t}$ values are baseline-specific. They are tied to
the baseline realization path, baseline tax-rate path, mortality inputs,
additive-inflow rule, turnover share, and the candidate $\psi$. If the baseline data
vintage or baseline tax path changes, $\kappa$ must be recovered again before
scenario solves. Operationally, $\kappa$ recovery should live inside the
same pre-pass that builds the scenario's bathtub state, not as a permanent
one-time artifact.

### 6.2 Elasticity Calibration

Given $\kappa_{a,t}$ and fixed parameters, apply a small tax perturbation
under current-law step-up:

$$
\tau^S_{a,t} = \tau^B_{a,t} + 0.01.
$$

Solve the scenario Bellman, run the bathtub, and compute an aggregate
elasticity target:

$$
\epsilon_{\text{model}}
=
\frac{\Delta \log R}{\Delta \log(1-\tau)}
$$

or the simulator's preferred semi-elasticity convention. Tune $\psi$ using a
full-model root find until the model matches the literature target, such as
a permanent realization elasticity around $-0.6$.

The response need not scale linearly as $1/\psi$. Locally, the first-order
condition suggests that larger $\psi$ flattens the realization response, but
the marginal cost also includes next-period $W$, and $W$ depends on future
choices that are themselves governed by $\psi$. The calibration routine
should therefore use robust bracketing and diagnostics rather than assuming
monotone-linear behavior.

This is identified because:

* $\kappa_{a,t}$ pins baseline realization levels;
* $\psi$ controls responsiveness around the baseline;
* $\beta$, $\theta$, and $\phi_I$ are fixed or sensitivity parameters.

The key design rule is that $\psi$ must affect scenario responses after
baseline inversion. It must not cancel out of $r^S/r^B$ as $\eta$ does in
the current log-benefit Bellman.

The elasticity calibration is local to a small tax-rate perturbation under
current-law step-up. Large regime changes, such as step-up to deemed
realization, shift the death branch by much more than a marginal tax-rate
shock. Responses to those reforms are therefore extrapolations through the
chosen benefit function and calibrated $\psi$, not directly identified by
the 1pp elasticity target.

### 6.3 Parameter Posture

A first implementation should treat:

* $\beta$: fixed from an annual discount rate assumption, with sensitivity.
* $\phi_I$: fixed turnover share, initially retaining the current value.
* $\theta$: share of heir-side future tax burden internalized under carryover,
  reported as a sensitivity.
* $\psi$: calibrated to the aggregate realization elasticity target.
* $\kappa_{a,t}$: cell-level baseline fit parameters, recovered for each
  baseline/scenario pre-pass rather than reused across baseline vintages.

---

## 7. Algorithm Sketch

For each scenario:

1. Build baseline age-year cell inputs:
   $G^B$, $R^B$, $r^B$, $m$, $\tau^B$, and $\tau^S$.
2. Set turnover $\lambda^T = \phi_I r^B$ and discretionary baseline target
   $r^{D,B} = r^B-\lambda^T$.
3. For a candidate $\psi$, run baseline backward induction:
   recover $\kappa_{a,t}$ and compute $W^B_{a,t}$ while forcing
   $r^{D,B}_{a,t}$ to match the observed baseline.
4. Run scenario backward induction:
   use fixed $\kappa_{a,t}$ and $\psi$ to solve for $r^{D,S}_{a,t}$.
5. Construct total rates:
   $r^B_{\text{model}} = \lambda^T+r^{D,B}$ and
   $r^S_{\text{model}} = \lambda^T+r^{D,S}$.
6. Feed $r^S_{\text{model}}$ into the bathtub recurrence.
7. Persist per-year state files with the same downstream contract as the
   current `kg_dynamics` behavior module: rate factors, extra realizations,
   deemed factors, and diagnostic columns.

For calibration, wrap the above in an outer loop over $\psi$ and target the
chosen aggregate elasticity.

---

## 8. What This Buys

Relative to the current model, this proposal provides:

* **Endogenous baseline and scenario realization paths.** The Bellman chooses
  realization shares directly; the reform response is not imposed through an
  external exponential ratio.
* **Dynamic consistency.** The choice to defer today changes the stock that
  enters future realization decisions through the bathtub.
* **Explicit death treatment.** Step-up creates a positive tax-forgiveness
  value of dying with unrealized gains; deemed realization removes that
  value; carryover sits between them depending on $\theta$. Physical stock
  routing remains in the bathtub.
* **Identifiable calibration.** Cell intercepts match baseline levels; a
  separate curvature parameter targets realization responsiveness.
* **Compatibility with repeated cross-sections.** Dynamic state remains at
  the representative-cell level, not the record level.

It does not provide:

* a full household savings model;
* endogenous asset allocation or portfolio returns;
* record-level longitudinal wealth histories;
* a fully dynastic model of heirs' future realization choices.

---

## 9. Open Design Choices

Several choices should be settled before implementation:

* Whether the first version should include an embedded-gain-ratio state or
  stay strictly age-year.
* Whether future versions should introduce a separate carried-gain return
  parameter. The recommended first version treats baseline stock growth as an
  additive inflow and omits a multiplicative continuation-growth factor.
* Whether the quadratic benefit function is sufficient, or whether a
  different concave form is needed to improve corner behavior.
* Whether the elasticity target should be a permanent elasticity, a
  semi-elasticity, or a profile across years.
* Whether carryover's $\theta$ should be fixed, calibrated, or reported only
  as sensitivity.
* Whether to add a separate pure bequest-value parameter $\zeta$ later. The
  recommended first version omits it and uses tax-liability forgiveness $F$
  as the death branch.

The recommended first prototype is deliberately minimal:

$$
\text{state} = (a,t), \qquad
\text{control} = r^D, \qquad
b(r)=\kappa r-\frac{\psi}{2}r^2.
$$

That prototype is sufficient to test whether a direct-control
representative-cell Bellman produces sensible realization paths and avoids
the eta-degeneracy problem.
