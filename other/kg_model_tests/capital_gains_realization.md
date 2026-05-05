# A Law of Motion for Policy-Induced Changes in Unrealized Capital Gains

**Author:** John Ricco
**Status:** Design specification
**Date:** 2026-05-02

---

## 1. Objective and Scope

This document specifies a principled accounting system for tracking the
policy-induced change in unrealized capital gains in a repeated–cross-section
microsimulation. The system supports revenue and behavioral analysis of
proposals that alter (i) the capital gains tax rate and (ii) the treatment of
unrealized gains at death.

The simulation contains cross-sectional records indexed by year and age. It
does not follow individuals longitudinally. Nevertheless, the stock of
unrealized capital gains evolves dynamically: gains held by a person aged $a$
in year $t$ shape what is held by that same person aged $a+1$ in year $t+1$.
The accounting system tracks this dynamic at the age × year level by
constructing a state variable that propagates across age cells and across
years, treating each (age, year) cell as a synthetic cohort.

### Scope

In scope:

* A law of motion for the policy-induced delta in unrealized gains, indexed
  by age, year, and asset class.
* A multiplicative realization-rate function with age, asset, regime, and
  tax-rate dependence.
* A regime taxonomy that supports step-up basis, carryover basis, and deemed
  realization at death, formulated through generic regime hooks so that
  additional regimes can be added without rederiving the recurrence.
* A ratio-based empirical anchoring that guarantees baseline reproduction.
* A calibration posture that distinguishes parameters drawn from data,
  calibrated to literature targets, and held as tunable assumptions.

Out of scope:

* The Haig-Simons income measure and accrual-based distributional analysis.
* Mark-to-market regimes that tax accruals annually.
* Charitable dispositions of appreciated assets, which bleed the unrealized
  stock without going through the realization channel.
* Forward-looking expectations of future tax rates; the model is myopic.

These are acknowledged in §9 as known gaps.

---

## 2. Notation and Primitives

### 2.1 Indices

* $a \in \{a_{\min}, \ldots, a_{\max}\}$ — age of the tax unit; the topcoded
  cell $a_{\max}$ collects all ages at and above the topcode.
* $t$ — year of the simulation.
* $k \in \mathcal{K} = \{\text{equities},\ \text{pass\_throughs},\
  \text{primary\_home},\ \text{other\_home},\ \text{re\_fund}\}$ — asset
  class for which a basis exists and unrealized gains can be tracked.

### 2.2 Stocks and flows from data (baseline)

Each $(a, t, k)$ cell carries the following baseline quantities, taken from
the simulator's baseline pass:

* $G^B_{a,t,k}$ — baseline unrealized taxable gain stock for the cell, defined
  as $\sum_i \max(0,\ V_{i,k} - B_{i,k})$ summed over tax units $i$ in the
  cell.
* $r^B_{a,t,k}$ — baseline realization rate for the cell, defined as the
  ratio of baseline realized gains to $G^B_{a,t,k}$.
* $m_{a,t}$ — mortality rate. Probability that a tax unit at $(a, t)$ ceases
  to exist as such by year $t+1$ (for joint filers, the probability both
  spouses die; widow continuation is handled by the cross-section's own
  re-imputation in subsequent years).

### 2.3 Allocation operators

* $A_{a \to h, t}$ — aging transition. Share of cell-$(a, t)$ survivors
  reassigned to age $h$ in year $t+1$. Default: $A_{a \to a+1, t} = 1$ for
  $a < a_{\max}$; topcode loop $A_{a_{\max} \to a_{\max}, t} = 1$. Generalizes
  cleanly if a future variant uses fractional aging or finer age groups.
* $\omega_{a \to h, t}$ — heir-allocation matrix. Share of carryover gains
  from decedents at age $a$ assigned to heirs at age $h$ in year $t+1$.
  Required to be row-stochastic in $h$:

  $$
  \sum_h \omega_{a \to h, t} = 1 \quad \text{for every } a.
  $$

  $\omega$ is supplied by the estate module and is regime-indifferent in its
  construction; its effect on the bathtub is gated by the regime hooks
  introduced in §3.3.

### 2.4 Policy primitives

* $\tau_{a,t}$ — capital gains marginal tax rate applicable to a cell.
* $c(\phi) \in [0, 1]$ — *post-death tax burden share*. The fraction of the
  embedded capital-gains tax that survives the holder's death and is
  internalized in their pre-death realization decision. Canonical values:
  $c = 0$ under step-up (the gain is forgiven), $c = 1$ under deemed
  realization (the full tax is paid at death), $c \in (0, 1)$ under
  carryover (the heir eventually pays, scaled by the holder's bequest
  motive). $c$ enters the holder's lifetime lock-in calculation in §4. The
  downstream *routing* of decedent gains at the death event is handled
  separately by regime hooks $(\delta_{\text{vanish}},
  \delta_{\text{route}}, \delta_{\text{realize}})$ defined in §3.3.

### 2.5 Reform-side stock and the policy-induced delta

Under a reform scenario, the unrealized gain stock evolves to $G^S_{a,t,k}$
and the realization rate to $r^S_{a,t,k}$. The state variable of interest is
the **policy-induced delta**:

$$
\Delta G_{a,t,k} \;=\; G^S_{a,t,k} - G^B_{a,t,k}.
$$

By construction, $\Delta G_{a,t,k} = 0$ in the first year of any scenario;
the delta accumulates over time as the reform diverges from baseline.
Accruals on existing assets are assumed policy-invariant across scenarios in
this document, so they difference out and do not appear in the recurrence
for $\Delta G$. (See §9.)

---

## 3. Law of Motion

### 3.1 Setup

The recurrence for $\Delta G_{h, t+1, k}$ is built by writing the reform-side
recurrence for $G^S_{h, t+1, k}$ and the baseline-side recurrence for
$G^B_{h, t+1, k}$ and differencing. Each year's stock at destination cell
$(h, t+1, k)$ is composed of three sources:

1. **Survivors who aged into $h$.** Tax units at $(a, t)$ who survived to
   year $t+1$ and were reassigned to age $h$ via $A$.
2. **Decedents whose gains transferred to heirs at $h$.** Tax units at
   $(a, t)$ who died, with their gains routed to age $h$ via $\omega$ — but
   only under regimes where carryover treatment applies.
3. **Accruals on the surviving stock.** Treated as policy-invariant in this
   model (see §9) and therefore not contributing to $\Delta G$.

We treat the three components in order.

### 3.2 Survivor Flow

Consider the population at $(a, t)$. A fraction $1 - m_{a,t}$ survives to
year $t+1$. Of this surviving fraction, a share $r^S_{a,t,k}$ realizes their
gains during year $t$, leaving a fraction $1 - r^S_{a,t,k}$ of the surviving
stock to roll forward. The stock that arrives at destination cell $h$ as
survivors from age $a$ is

$$
G^{S, \text{surv}}_{h, t+1, k}
\;=\;
\sum_a A_{a \to h, t}\,(1 - m_{a,t})\,(1 - r^S_{a,t,k})\,G^S_{a,t,k}.
$$

The corresponding baseline-side expression replaces $r^S$ with $r^B$ and
$G^S$ with $G^B$. Differencing and substituting $G^S_{a,t,k} = G^B_{a,t,k} +
\Delta G_{a,t,k}$:

$$
\Delta G^{\text{surv}}_{h, t+1, k}
\;=\;
\sum_a A_{a \to h, t}\,(1 - m_{a,t})\!\left[
(1 - r^S_{a,t,k})\,G^S_{a,t,k} - (1 - r^B_{a,t,k})\,G^B_{a,t,k}
\right].
$$

Expand the bracket using $G^S = G^B + \Delta G$:

$$
\begin{aligned}
(1 - r^S)\,G^S - (1 - r^B)\,G^B
&= (1 - r^S)(G^B + \Delta G) - (1 - r^B)\,G^B \\
&= (1 - r^S)\,\Delta G + \big[(1 - r^S) - (1 - r^B)\big]\,G^B \\
&= (1 - r^S)\,\Delta G + (r^B - r^S)\,G^B.
\end{aligned}
$$

Therefore:

$$
\boxed{
\Delta G^{\text{surv}}_{h, t+1, k}
\;=\;
\sum_a A_{a \to h, t}\,(1 - m_{a,t})\!\left[
(1 - r^S_{a,t,k})\,\Delta G_{a,t,k}
+ G^B_{a,t,k}\,(r^B_{a,t,k} - r^S_{a,t,k})
\right].
}
$$

The first term inside the bracket carries the existing policy-induced delta
forward, decayed by mortality and by realizations under the reform rate. The
second term is *new* delta generated this year by differential realization
behavior on baseline-stock: when $r^S < r^B$ the reform locks in additional
gains, when $r^S > r^B$ the reform churns through baseline stock faster and
shrinks future $\Delta G$.

### 3.3 Death and Inheritance Flow with Regime Hooks

We now treat the disposition of the gain stock held by decedents at $(a, t)$,
$m_{a,t}\,G^S_{a,t,k}$, and its baseline counterpart $m_{a,t}\,G^B_{a,t,k}$
(which always faces step-up — the baseline is current-law). Three policy
regimes can apply on the reform side, distinguished by where decedent gains
go:

| Regime | Decedent gain stock under reform |
|---|---|
| step_up | Vanishes (gain is forgiven at death; no transfer, no realization). |
| carryover | Transfers to heirs with carryover basis; realized over time as heirs themselves realize. |
| deemed_realization | Forcibly realized on the decedent's final return at rate $\tau$; revenue accrues at death; nothing transfers to heirs. |

We capture these three cases by a routing triple
$(\delta_{\text{vanish}},\ \delta_{\text{route}},\ \delta_{\text{realize}})$
constrained to satisfy

$$
\delta_{\text{vanish}} + \delta_{\text{route}} + \delta_{\text{realize}} = 1,
\qquad \delta_{\text{vanish}}, \delta_{\text{route}}, \delta_{\text{realize}} \in [0, 1],
$$

with the canonical instantiations:

| Regime | $\delta_{\text{vanish}}$ | $\delta_{\text{route}}$ | $\delta_{\text{realize}}$ |
|---|---|---|---|
| step_up | 1 | 0 | 0 |
| carryover | 0 | 1 | 0 |
| deemed_realization | 0 | 0 | 1 |

The triple framing accommodates extension to mixed regimes (e.g., partial
step-up with a carryover allowance above a threshold) as a future refinement
without modifying the recurrence.

#### 3.3.1 Stock contribution

Of the decedent gain stock, only the $\delta_{\text{route}}$ portion enters
the next-period stock at heir cohorts. The reform-side contribution to
$G^S_{h, t+1, k}$ from decedents is

$$
G^{S, \text{death}}_{h, t+1, k}
\;=\; \delta_{\text{route}}
\sum_a \omega_{a \to h, t}\,m_{a,t}\,G^S_{a,t,k}.
$$

The baseline-side contribution is zero, because baseline is step-up and
$\delta^B_{\text{route}} = 0$. Therefore:

$$
\boxed{
\Delta G^{\text{death}}_{h, t+1, k}
\;=\;
\delta_{\text{route}}
\sum_a \omega_{a \to h, t}\,m_{a,t}\,\big(G^B_{a,t,k} + \Delta G_{a,t,k}\big).
}
$$

This term has two parts. The first, $\delta_{\text{route}} \sum_a \omega \cdot
m \cdot G^B$, is exogenous forcing: it is a level effect that arises purely
from the regime change relative to current law's step-up, even with zero
behavioral response. The second, $\delta_{\text{route}} \sum_a \omega \cdot
m \cdot \Delta G$, is endogenous: it propagates whatever policy-induced delta
was already in the decedent's cell to heir cells.

**Within-cell derivation of the death stock.** The expressions above use
$m_{a,t}$ as a cell-level scalar, multiplying cell-total stock. That form
silently assumes $\mathrm{Cov}(m, G_\text{unit} \mid a, t) = 0$ — that
within an age cell, individual mortality and individual gain stock are
independent. This is empirically false: the wealth-mortality gradient
makes the covariance large and negative (within any age cohort, the
wealthier subpopulation both carries more accumulated gains and dies
less). Using cell-mean $m$ times cell-total $G^B$ therefore overstates
the true decedent stock contribution materially — by roughly 2.7× at
the gain-weighted aggregate in our data.

The correct calculation is per-record. The cell's decedent stock
contribution is

$$
D_{a,t} \;=\; \sum_i w_i\, m_i\, \big(G_{\text{unit},i} + dG_i\big),
$$

where $i$ indexes the tax units in the cell, $w_i$ is the weight,
$m_i$ is per-record household mortality, $G_{\text{unit},i}$ is per-record
unrealized gain stock, and $dG_i$ is each record's share of the cell's
policy-induced delta. Materializing per-record state in the recurrence
is unnecessary: under the assumption that $dG$ within a cell is
allocated proportional to $G_\text{unit}$ (each holder's share of the
cell's gain stock — consistent with the distribution rule in §7.3),
the sum collapses analytically:

$$
\begin{aligned}
D_{a,t}
&= \sum_i w_i\, m_i\, G_{\text{unit},i}\cdot\Big(1 + \tfrac{dG_a}{G^B_a}\Big) \\
&= \Big(\sum_i w_i\, m_i\, G_{\text{unit},i}\Big) \cdot \frac{G^B_{a,t} + \Delta G_{a,t,k}}{G^B_{a,t}} \\
&= m^{\text{eff}}_{a,t}\cdot\big(G^B_{a,t,k} + \Delta G_{a,t,k}\big),
\end{aligned}
$$

where the effective cell mortality is the gain-weighted death rate

$$
m^{\text{eff}}_{a,t} \;\equiv\; \frac{\sum_i w_i\, m_i\, G_{\text{unit},i}}{G^B_{a,t}}.
$$

This $m^{\text{eff}}$ is *not* an approximation — given the
$G$-proportional allocation of $dG$, it is exactly the per-record sum.
The same algebra applied to the survivor channel gives $(1 - m^{\text{eff}})$
multiplying the cell's surviving stock. Implementations should therefore
substitute $m^{\text{eff}}$ for $m$ wherever $m$ multiplies cell-aggregate
stock in §3.2–3.3, with $m^{\text{eff}}$ computed once per cell from the
microdata as $\sum_i w_i\, m_i\, G_{\text{unit},i} / G^B_{a,t}$.

**Choice of allocation rule.** The G-proportional rule above is one of
several reasonable choices. An alternative is to allocate $dG$
proportional to positive realized gains:

$$
dG_i^{(R)} \;=\; dG_a \cdot \frac{\max(kg_{\text{lt},i}, 0)}{R^B_a}
\quad\Rightarrow\quad
m^{\text{eff,R}}_a \;=\; \frac{\sum_i w_i\, m_i\, \max(kg_{\text{lt},i}, 0)}{R^B_a},
$$

which corresponds to the lock-in story (deferred realizations stay with
the records that were going to realize the most). The choice between
$G$ and $R$ rules is implementation-controlled rather than spec-fixed —
the recurrence form $D = m^{\text{eff}}\cdot(G^B + \Delta G)$ is the same
under either rule, with only the aggregate column changing. The simulator
defaults to the $G$ rule but exposes both via a configuration constant
(`KG_DYN_DG_ALLOCATION` in `src/sim/kg_dynamics.R`); under $R$, the
recurrence falls back to the $G$ rate when $R^B_a = 0$ (e.g., young
heir cohorts under carryover with no historical realization activity).

**Practical impact.** Step-up scenarios are unaffected: when
$\delta_\text{route}=0$ the death channel is shut off, and the
$(1-m)$ vs $(1-m^\text{eff})$ misallocation in the survivor channel only
shifts stock between "vanish at death" and "stay in the population,"
which are observationally equivalent under step-up. Carryover and
deemed-realization scenarios change materially: per-record accounting
removes the wealth-mortality bias that would otherwise inflate decedent
stock by ~2-3×, with proportional reductions in carryover inheritance
flow and deemed-realization revenue.

#### 3.3.2 Revenue contribution

The $\delta_{\text{realize}}$ portion of decedent gain stock is forcibly
realized at death and contributes to revenue. Define the deemed-realization
revenue accumulator:

$$
\boxed{
R^{\text{death}}_{t, k}
\;=\;
\delta_{\text{realize}}
\sum_a m_{a,t}\,\big(G^B_{a,t,k} + \Delta G_{a,t,k}\big)\,\overline{\tau}_{a,t},
}
$$

where $\overline{\tau}_{a,t}$ is the cell-average capital gains MTR, possibly
adjusted for any deemed-realization-specific rate or exemption.
$R^{\text{death}}$ is a parallel revenue stream, summed across asset classes
and ages, that adds to the standard $\Delta R$ revenue developed in §8.
Because baseline is step-up, baseline death-event capital-gains revenue is
zero by construction; the level $R^{\text{death}}_{t,k}$ is therefore also
the reform-vs-baseline delta.

The $\delta_{\text{vanish}}$ portion is what gives us no contribution to
either next-period stock or revenue: gains forgiven at death exit the system.

### 3.4 Aging Transition Matrix and Topcoding

The aging matrix $A_{a \to h, t}$ allows the recurrence to be written
uniformly across regular and topcoded ages. For a model with one-year age
groups and a topcode at $a_{\max}$:

$$
A_{a \to h, t} \;=\;
\begin{cases}
1 & \text{if } a < a_{\max} \text{ and } h = a + 1, \\
1 & \text{if } a = a_{\max} \text{ and } h = a_{\max}, \\
0 & \text{otherwise}.
\end{cases}
$$

This convention has two consequences. First, survivors at the topcode
remain at the topcode; their delta accumulates within the topcode rather
than aging out of the system. Second, the youngest age group $a_{\min}$ has
no aged-survivor inflow and therefore $\Delta G_{a_{\min}, t+1, k}$ has no
survivor contribution — only inheritance flow under carryover. Both
boundary cases are handled mechanically by the matrix without requiring
special-case logic in the recurrence.

### 3.5 Full Recurrence

Combining the survivor and death-event flows:

$$
\boxed{
\begin{aligned}
\Delta G_{h, t+1, k}
\;=\;&
\sum_a A_{a \to h, t}\,(1 - m_{a,t})\!\left[
(1 - r^S_{a,t,k})\,\Delta G_{a,t,k}
+ G^B_{a,t,k}\,(r^B_{a,t,k} - r^S_{a,t,k})
\right] \\
&+\; \delta_{\text{route}}
\sum_a \omega_{a \to h, t}\,m_{a,t}\,(G^B_{a,t,k} + \Delta G_{a,t,k}).
\end{aligned}
}
$$

This is the central law of motion for the policy-induced delta in unrealized
capital gains. It is linear in $\Delta G$, applies cell-by-cell with no
within-year recursion (given $\Delta G_{a,t,k}$ for all cells, the year-
$(t+1)$ values for all cells follow from a single pass), and reduces to zero
under reform-equals-baseline (since $r^S = r^B$ and $\delta_{\text{route}} =
0$ collapses both terms).

### 3.6 Aggregate (Vector) Form

Stacking $\Delta G_{a, t, k}$ over $a$ into a column vector $\boldsymbol{\Delta
G}_{t, k} \in \mathbb{R}^{|a|}$, define:

* The diagonal "own-cell decay" matrix
  $D_t^{(k)} \in \mathbb{R}^{|a| \times |a|}$ with entries
  $D_t^{(k)}[a,a] = (1 - m_{a,t})(1 - r^S_{a,t,k})$.
* The aging matrix $A_t \in \mathbb{R}^{|a| \times |a|}$ acting on the
  destination index.
* The heir-routing matrix $W_t \in \mathbb{R}^{|a| \times |a|}$ with entries
  $W_t[a, h] = \omega_{a \to h, t}\,m_{a,t}$.
* The exogenous forcing vector
  $\boldsymbol{f}_{t}^{(k)} \in \mathbb{R}^{|a|}$ with entries
  $f^{(k)}_{t,h} = \sum_a A_{a \to h, t}\,(1 - m_{a,t})\,G^B_{a,t,k}\,(r^B_{a,t,k}
  - r^S_{a,t,k}) + \delta_{\text{route}} \sum_a W_t[a, h]\,G^B_{a,t,k}$.

Then the recurrence reads compactly:

$$
\boldsymbol{\Delta G}_{t+1, k}
\;=\;
\big(A_t^\top D_t^{(k)} + \delta_{\text{route}} W_t^\top\big)\,\boldsymbol{\Delta G}_{t, k}
+ \boldsymbol{f}_{t}^{(k)}.
$$

This is a linear time-varying system. The transition operator
$A_t^\top D_t^{(k)} + \delta_{\text{route}} W_t^\top$ is dissipative under
step-up ($\delta_{\text{route}} = 0$, since the diagonal decay multipliers
are strictly less than one) and quasi-conservative under carryover (the
heir-routing term redistributes magnitude between cells). Sensitivity of
$\boldsymbol{\Delta G}$ to parameter perturbations propagates linearly.

### 3.7 Conservation Properties

Three conservation statements follow directly from the recurrence:

1. **Carryover conserves routed magnitude at the transfer event.** Under
   $\delta_{\text{route}} = 1$, the total decedent contribution $\sum_a
   m_{a,t}\,(G^B_{a,t,k} + \Delta G_{a,t,k})$ is exactly the total
   inheritance inflow $\sum_h \sum_a \omega_{a \to h, t}\,m_{a,t}\,(G^B +
   \Delta G)$, by row-stochasticity of $\omega$. The death-event transfer
   itself is conservative; aggregate $\Delta G$ over the full simulation
   horizon still evolves through survivor decay $(1 - m)(1 - r^S)$ and
   differential realizations $(r^B - r^S)\,G^B$, so this is not a statement
   about long-run conservation of $\Delta G$.
2. **Deemed converts disposed Δ into revenue.** Under $\delta_{\text{realize}}
   = 1$, the decedent stock contributes nothing to next-period $\Delta G$
   but contributes $R^{\text{death}}$ to revenue. The two channels are
   complementary: stock disposed equals revenue raised, modulo the rate.
3. **Step-up dissipates Δ.** Under $\delta_{\text{vanish}} = 1$, decedent
   stock leaves the system without replacement. The bathtub slowly leaks
   through mortality even with no realization activity.

---

## 4. Realization Rate Function

The reform-side realization rate is derived from a **representative-agent
choice problem**: in each period the holder decides what fraction of their
embedded gain to realize, trading off non-tax benefits of liquidity against
the *effective tax price* of realizing now versus deferring. The regime
(step-up, carryover, deemed) enters through a single object — the effective
tax price $P$ — which propagates simultaneously into the *level* of
realizations and into their sensitivity to the tax rate. This eliminates
the need for separate "level" and "elasticity" parameters and ties their
values to the same underlying mechanism.

### 4.1 The agent's choice problem

Normalize to \$1 of unrealized gain. A representative holder in cell
$(a, t, k)$ picks a realization fraction $r \in [0, 1]$ to maximize:

$$
\max_{r \in [0,1]} \; b_{a,k}(r) \;-\; P_{a,t,k} \cdot r,
$$

where $b_{a,k}(r)$ is a concave non-tax benefit function (capturing
liquidity needs, rebalancing motives, and other reasons to realize that are
unrelated to taxes), and $P_{a,t,k}$ is the effective tax price of realizing
now rather than holding (developed in §4.2).

For tractability we choose the marginal-benefit form
$b'(r) = \mu_{a,k} - (1/\eta_k) \log r$, which gives the FOC
$\log r^* = \eta_k (\mu_{a,k} - P)$. The cell-specific intercept
$\mu_{a,k}$ absorbs all non-tax heterogeneity in realization motives and is
implicitly calibrated by the baseline anchoring (§5). The free parameter is
$\eta_k$, the curvature of the marginal-benefit curve, which controls how
sensitively the holder reshuffles realizations in response to changes in
the tax price.

Differencing across baseline and reform and exponentiating yields the
**baseline-anchored realization rate**:

$$
\boxed{
r^S_{a,t,k}
\;=\;
r^B_{a,t,k} \cdot
\exp\!\big(\!-\eta_k \cdot (P^S_{a,t,k} - P^B_{a,t,k})\big).
}
$$

When the reform coincides with baseline ($P^S = P^B$), the exponential
collapses to one and $r^S = r^B$ exactly, guaranteeing $\Delta G \equiv 0$
when no policy change is in effect.

### 4.2 Effective tax price

If the holder sells today, they pay $\tau_t$ per dollar of gain. If they
hold, they may sell voluntarily later, die before realizing, have heirs
inherit a tax burden under carryover, or face a deemed realization at
death. The effective tax price is the wedge between paying-now and the
present value of the expected future tax payment if they hold:

$$
\boxed{
P_{a,t,k}(c)
\;=\;
\tau_t
\;-\;
\sum_{j=1}^{\infty}
\beta^j\, s_{a,k,j}\, \tau_{t+j}
\;-\;
\sum_{j=1}^{\infty}
\beta^j\, d_{a,j}\, c\, \tau_{t+j},
}
$$

where $j$ indexes future years and:

* $s_{a,k,j}$ — probability that the asset is voluntarily realized in year
  $t+j$, conditional on the holder still holding at $t$. Built from the
  competing-risks recursion in §4.3.
* $d_{a,j}$ — probability that the holder dies in year $t+j$ before
  voluntarily realizing, again from the §4.3 recursion.
* $\beta$ — annual discount factor (≈ 0.96).
* $\tau_{t+j}$ — the path of expected future tax rates. Under naive
  expectations, $\tau_{t+j} = \tau_t$ for all $j$; under announced rate
  changes, $\tau_{t+j}$ tracks the announced path.
* $c \in [0, 1]$ — the regime hook from §2.4. The fraction of the
  embedded tax burden that survives the holder's death and is internalized
  in their lifetime decision.

The sums are written formally to infinity; the integrand decays
geometrically at rate $\beta \cdot (1 - \lambda_r - m) \approx 0.82$ per
year, so terms are below float precision after a few hundred years. The
implementation truncates at 200 years, which is well into the irrelevant
tail and exposes no tunable parameter — $\beta$ alone governs how quickly
future events are discounted away.

The first sum is the discounted expected tax from voluntary future
realization. The second sum is the discounted expected tax from the death
state, scaled by the regime's burden-share $c$. Under step-up ($c = 0$) the
death state contributes nothing — gains are forgiven. Under deemed
realization ($c = 1$) the full tax is paid at death. Under carryover, $c$
captures the holder's bequest motive: $c = 1$ means the heir's eventual tax
is fully internalized; $c = 0$ collapses to step-up.

Under constant rate expectations ($\tau_{t+j} = \tau_t$), $P$ factors
cleanly:

$$
P_{a,t,k}(c) \;=\; \tau_t \cdot (1 - M_{a,k}(c)), \quad
M_{a,k}(c) \;=\; \sum_{j=1}^{\infty} \beta^j s_{a,k,j} + c \sum_{j=1}^{\infty} \beta^j d_{a,j}.
$$

The dimensionless quantity $1 - M_{a,k}(c)$ is the **bracket**. It scales
both the level of $P$ and the rate sensitivity (§4.4).

### 4.3 Hazard sequences from competing risks

The probabilities $s_j$ and $d_j$ come from a discrete-time competing-risks
process. Each year, conditional on still holding, the holder either
voluntarily realizes (with hazard $\lambda_{r,k}$, the per-period
realization hazard for asset class $k$), dies (with cell-mortality
$m_{a,t}$), or holds another year. Stacking forward:

$$
S_{a, j} \;=\; \prod_{i=0}^{j-1} \big(1 - \lambda_{r, k} - m_{a+i, t+i}\big),
\qquad S_{a,0} = 1,
$$

is the probability that the asset is *still being held* at the start of
year $t + j$. Then:

$$
s_{a,k,j} \;=\; S_{a, j} \cdot \lambda_{r,k},
\qquad
d_{a,j} \;=\; S_{a, j} \cdot m_{a+j, t+j}.
$$

The indexing pairs $s_j$ and $d_j$ with the bracket-sum factors $\beta^j$
and $\tau_{t+j}$ in §4.2: realizing or dying *in year $t+j$* requires the
holder to have survived from year $t$ through the start of year $t+j$
(probability $S_{a,j}$, $j$ years of competing-risks decay), and then to
realize / die during year $t+j$ at age $a+j$.

For ages above the topcode $a_{\max}$, $m$ is held fixed at $m_{a_{\max}}$.
The mortality path $m_{a+i, t+i}$ is read from the simulator's per-year
cell aggregates or from a fixed life table.

The realization hazard $\lambda_{r,k}$ is the only *new* parameter
introduced by the microfoundation and is calibrated from observed baseline
realizations (§6.4).

### 4.4 Properties

**Level effect: removing step-up raises the level of realizations.** Going
from $c = 0$ (step-up) to $c > 0$ (carryover, deemed) makes the second sum
in §4.2 nonzero, *raising* expected future tax burden, *lowering* $P$, and
therefore *raising* $r^S$. The size of the increase scales with mortality —
holders with high $m$ see the biggest reduction in $P$ because the death
state contributes most to their expected future tax. Young holders see
nearly no change. This reproduces the "elderly lock-in unwinds when
step-up is removed" property without a separate $\psi$ parameter.

**Sensitivity effect: removing step-up reduces tax-rate elasticity.**
Differentiating $P$ with respect to $\tau$:

$$
\frac{\partial P_{a,t,k}}{\partial \tau_t}
\;=\; 1 - \sum_j \beta^j s_j \frac{\tau_{t+j}}{\tau_t} - c \sum_j \beta^j d_j \frac{\tau_{t+j}}{\tau_t}
\;=\; 1 - M_{a,k}(c) \quad \text{(under constant rate expectations).}
$$

The semi-elasticity of $r^S$ with respect to $\tau$ is therefore:

$$
\boxed{
\frac{\partial \log r^S}{\partial \tau_t}
\;=\;
-\eta_k \cdot (1 - M_{a,k}(c)).
}
$$

The same bracket $1 - M_{a,k}(c)$ that sets the level of $P$ also sets the
slope. Under deemed ($c = 1$) the bracket is small (most of a rate hike is
"passed through" into expected future tax burden, neutralizing the wedge
change), so the rate elasticity is small. Under step-up ($c = 0$) the
bracket is larger (the rate-hike-induced increase in expected future tax is
weighted only by the survival probability $1 - M_{\text{die}}$, since the
death state pays nothing), so the rate elasticity is larger. **The level
bump and the elasticity reduction are two faces of a single mechanism: the
bracket.**

**Forward-looking transitions emerge naturally.** If a rate change is
announced for year $t^*$, the sums in §4.2 use the announced path
$\tau_{t+j}$, which causes $P$ — and therefore $r^S$ — to respond *before*
the change takes effect. Anticipation behavior is captured without
modifying the recurrence. The model remains myopic about *unannounced*
future changes, but accommodates expected paths.

**Transitory release on rate cuts emerges from the dynamics, not the rate
function.** Identical mechanism to the previous reduced-form: at the
moment of a rate cut, accumulated $\Delta G > 0$ from prior lock-in is hit
with a rising $r^S$, producing a surge of realizations beyond the steady-
state response. The bracket framework does not change this; it is a
property of how $\Delta G$ is multiplied through the §3.5 recurrence.

### 4.5 Connection to reduced-form realization equations

Setting $P = P^B + \tilde\varepsilon \cdot (\tau - \tau^B)$ and ignoring
the second-sum dependence on $\tau$ recovers the log-linear form
$\log r^S = \log r^B - \tilde\varepsilon \cdot \eta \cdot (\tau^S - \tau^B)$
that Burman-Randolph (1994), Dowd-McClelland-Muthitacharoen (2015), and
similar reduced-form realization equations adopt. The microfoundation
makes explicit that the literature's "semi-elasticity" $\tilde\varepsilon$
implicitly bundles together the agent's behavioral curvature ($\eta$), the
horizon over which they expect to hold ($\beta$ raised to the
expected-time-to-realization), and the regime ($c$). Decomposing into
$(\eta, \beta, \lambda_r, c)$ exposes the implicit assumptions and lets the
analyst vary them independently.

---

## 5. Empirical Anchoring

### 5.1 The ratio formulation

The realization function $r$ specified in §4 is a structural object. In
application, the model must be anchored so that it reproduces observed
baseline realizations exactly when there is no reform; calibration
imperfections in $r$ should affect *differences* between scenarios but not
baseline levels. We achieve this by applying $r$ via a ratio to the
observed baseline:

$$
\boxed{
\text{realizations}^S(a, t, k)
\;=\;
\text{realizations}^B_{\text{obs}}(a, t, k) \cdot
\frac{r^S_{a,t,k}\,G^S_{a,t,k}}{r^B_{a,t,k}\,G^B_{a,t,k}},
}
$$

where $\text{realizations}^B_{\text{obs}}$ is the simulator's baseline
realized-gain output for the cell.

When the scenario is itself baseline, $r^S = r^B$ and $G^S = G^B$, the ratio
collapses to one, and $\text{realizations}^S = \text{realizations}^B_{\text{obs}}$
identically. With the baseline-anchored form of $r^S$ in §4.1, this is
automatic — no additional miscalibration of $r$ is possible to break it.

**Sparse-cell guard.** The ratio is undefined when $r^B_{a,t,k}\,G^B_{a,t,k}
= 0$. In such cells (typically young heir cohorts that have just received
carryover-transferred gains but have no historical realization activity), the
ratio formula is replaced by a fallback rule: apply $r^S \cdot G^S$ directly,
or pool $r^B$ across age within the asset class, depending on which fallback
the simulator's sparse-cell convention dictates. This is the same hierarchy
used by the within-cell allocator in §7.3.

### 5.2 Equivalence under perfect calibration

Under perfect calibration of $r$, the ratio formulation is mathematically
equivalent to applying $r^S$ directly to $G^S$. The two differ only when
$r^B$ as computed by the structural formula does not equal the observed
baseline rate. The ratio formulation is robust to this discrepancy; the
direct-level form is not.

### 5.3 Reform-induced delta in realizations

The flow object that feeds revenue is

$$
\Delta R_{a, t, k}
\;=\;
\text{realizations}^S(a, t, k) - \text{realizations}^B_{\text{obs}}(a, t, k).
$$

Algebraically:

$$
\Delta R_{a, t, k}
\;=\;
\text{realizations}^B_{\text{obs}}(a, t, k) \left[
\frac{r^S\,(G^B + \Delta G)}{r^B\,G^B} - 1
\right].
$$

This is the cell-aggregate $\Delta R$. Distribution to individual tax units
within the cell follows the convention in §7.3.

---

## 6. Calibration

The microfoundation in §4 introduces five parameter groups, each with a
distinct identification strategy.

### 6.1 Baseline rates $r^B_{a,t,k}$ — observed

The baseline realization rate by age, year, and asset class is taken
directly from the simulator's baseline pass. It is not estimated; it is
not a free parameter. The simulator's baseline aggregates realized gains
by age and asset class and divides by the cell's baseline gain stock to
produce a rate profile.

Baseline reproduction is built into the realization function via the
exponential anchoring in §4.1: when reform equals baseline, $P^S = P^B$
and $r^S = r^B$ exactly, regardless of how $\eta_k$ or any other
parameter is calibrated.

### 6.2 Behavioral curvature $\eta_k$ — calibrated

The curvature of the marginal-benefit-of-realizing curve, $\eta_k$,
controls how strongly $r^S$ responds to changes in the effective tax
price. It is calibrated so that the model's implied aggregate realization
elasticity matches a literature target under current law (step-up).

**Procedure.** Define the implied semi-elasticity at the baseline rate:

$$
\frac{\partial \log \sum_{a,t} \text{realizations}^S(a,t,k)}{\partial \tau}
\bigg|_{\tau = \tau_0,\, c = 0}
\;=\; -\eta_k \cdot (1 - M_{a,k}(0))_{\text{aggregated over a, t}}.
$$

Choose a target value from the realization elasticity literature
(typically η ≈ −0.5 to −1.0 for equities; smaller magnitudes for less
liquid assets) and solve for the unique $\eta_k$ that produces it under
$c = 0$. The bracket $1 - M_{a,k}(0)$ depends on the cell's mortality path
and the realization hazard $\lambda_{r,k}$ (§6.4), both of which are pinned
down before $\eta_k$ is calibrated.

Existence and uniqueness are straightforward: the implied elasticity is
linear in $\eta_k$ holding the bracket fixed.

### 6.3 Discount factor $\beta$ — empirical

The discount factor $\beta$ is read from financial-economics estimates of
household discount rates in the lock-in literature, typically $\beta \in
[0.95, 0.97]$ corresponding to a 3–5% annual discount rate. Auerbach (1991)
and Constantinides (1983) use $\beta \approx 0.96$. The doc adopts
$\beta = 0.96$ as the default and treats it as common across asset classes.

For greater rigor, $\beta$ could be made asset-specific (housing carries
a different effective opportunity cost than equities) or risk-adjusted; the
minimal model does not pursue this.

### 6.4 Realization hazard $\lambda_{r,k}$ — calibrated from data

The voluntary realization hazard for asset class $k$ is calibrated so that
the §4.3 competing-risks recursion reproduces the observed aggregate
realization rate at baseline. Concretely:

$$
\lambda_{r,k}^{\text{calibrated}}
\;=\;
\frac{\sum_{a,t} R^B_{a,t,k}}{\sum_{a,t} G^B_{a,t,k}},
$$

i.e., the asset-class-aggregate ratio of realized gains to unrealized
stock at baseline. This is a single number per asset class (not per cell)
and serves as the "average" hazard the holder uses to forecast their own
future realization timing. Typical values: $\lambda_r \approx 0.05$ for
liquid equities, $0.02$ for housing.

This is the simplest possible calibration. Refinements could vary
$\lambda_{r,k}$ by age or wealth, but at the cost of losing the
parsimony.

### 6.5 Regime burden share $c$ — judgmental, regime-specific

The regime hook $c \in [0, 1]$ is set by policy:

| Regime | $c$ |
|---|---|
| step_up | 0 |
| carryover | $\theta \in (0, 1]$ — bequest-motive parameter |
| deemed_realization | 1 |

Step-up and deemed realization pin $c$ at the endpoints; carryover requires
the bequest-motive parameter $\theta$. We treat $\theta$ as a tunable
assumption — there is no domestic empirical anchor because carryover has
never been enacted in the modern U.S. — with default $\theta = 0.5$ (the
holder internalizes half of the heir's eventual tax burden). Sensitivity
to $\theta$ should be reported at $\pm 0.25$ around the default.

Note that $\theta$ is the *only* tunable parameter for which the model has
no empirical anchor. The previous reduced-form spec had two ($\varepsilon$
calibrated; $\psi$ tunable). The microfoundation pins down the analog of
$\psi$ via the bracket calculation, leaving only $\theta$ as a knob — a
substantial improvement in identification.

### 6.6 Inheritance allocation matrix $\omega$ — exogenous

Unchanged from the reduced-form spec: $\omega_{a \to h, t}$ is supplied
by the estate module and treated here as exogenous, with the only
constraint being row-stochasticity in $h$. The estate module determines
the heir age distribution conditional on decedent age. $\omega$ is
regime-indifferent in its construction; its effect on the bathtub is
gated by the regime hook $\delta_{\text{route}}$ in §3.3.

### 6.7 Summary of identification

| Parameter | Cardinality | Identification |
|---|---|---|
| $r_B(a, k)$ | $\|a\| \cdot \|\mathcal{K}\|$ | Observed from simulator baseline |
| $m(a, t)$ | $\|a\|$ | Observed from cell aggregates / life table |
| $\eta_k$ | $\|\mathcal{K}\| = 5$ | Calibrated to step-up elasticity target |
| $\lambda_{r,k}$ | $\|\mathcal{K}\| = 5$ | Calibrated from observed $R^B / G^B$ |
| $\beta$ | 1 | Empirical default ≈ 0.96 |
| $\theta$ (bequest motive) | 1 (or per-$k$) | Tunable, default 0.5, sensitivity panel |
| $\omega$ | exogenous | From estate module |

Total tunable-or-calibrated parameters: $2|\mathcal{K}| + 2 = 12$ for five
asset classes (five $\eta_k$ + five $\lambda_{r,k}$ + $\beta$ + $\theta$).
Of these, ten are *calibrated* against observed quantities ($\eta_k$ from
elasticity targets, $\lambda_{r,k}$ from baseline data); only $\beta$ and
$\theta$ are judgmental.

In comparison, the reduced-form spec had ten free parameters (five
$\varepsilon_k$ + five $\psi_k$), of which five were calibrated and five
were tunable. The microfoundation has slightly more parameters but a
much higher fraction of them are anchored to observable quantities, and
the level-vs-elasticity tradeoff that motivated separate $(\varepsilon,
\psi)$ in the reduced form is now an internally consistent consequence of
a single mechanism.

---

## 7. Conventions and Boundaries

### 7.1 Joint-filer cohort assignment

Joint-filer households are assigned a cohort using the older spouse's age:

$$
a_{\text{joint}} \;=\; \max(a_1, a_2).
$$

This is the natural anchor: the older spouse is more likely to be near
end-of-life, which is when the step-up benefit binds most strongly, and is
typically the asset-side anchor in joint holdings. The household survival
probability is

$$
m_{\text{joint}, t} \;=\; m_{1, t} \cdot m_{2, t},
$$

i.e., the household ceases to exist as a joint-filer cell only when both
spouses die in the same year. Widow continuation is handled by the
cross-section's own re-imputation in subsequent years; the surviving
spouse appears in their own age cell with their own assets.

### 7.2 Boundary cells

* **Youngest cohort.** Tax units at $a_{\min}$ have no aged-survivor inflow
  by construction. Their $\Delta G$ accumulates only through inheritance
  flow under carryover. Realistically, $a_{\min} = 18$ or $20$ in U.S.
  microsim contexts, by which age inherited gains become possible.
* **Topcoded cohort.** At $a_{\max}$, the aging matrix $A_{a_{\max} \to
  a_{\max}, t} = 1$ retains survivors within the topcode. Mortality at the
  topcode is high and effectively dominates the cell's survivor decay, but
  the recurrence remains well-defined and stable as long as $0 \leq m \leq
  1$.

### 7.3 Within-cell distribution to tax units

The recurrence and the realization function operate on cell aggregates. The
distribution of cell-aggregate $\Delta R_{a, t, k}$ to individual tax units
within the cell uses the following rule:

1. **Pro-rata to baseline realizations.** If the cell has nonzero baseline
   realized gains, each tax unit's share of $\Delta R$ equals its share of
   the cell's baseline realizations.
2. **Pro-rata to baseline gain stock.** Fall back to the share of $G^B$
   when the cell has zero baseline realizations (e.g., a young cell that
   has just received carryover-transferred gains but has historically not
   sold).
3. **Skip.** If both baseline realizations and $G^B$ are zero in the cell,
   no realizations are distributed; the cell contributes nothing to revenue
   that year.

The fallback hierarchy is needed in carryover scenarios because young heir
cohorts can accumulate $\Delta G$ before they have any baseline realization
activity to anchor against.

---

## 8. Revenue and Downstream Use

### 8.1 Ongoing realization revenue

For each $(a, t, k)$, the cell-level revenue contributions under reform and
baseline are

$$
T^S_{a, t, k} \;=\; \tau^S_{a, t} \cdot R^S_{a, t, k},
\qquad
T^B_{a, t, k} \;=\; \tau^B_{a, t} \cdot R^B_{a, t, k},
$$

where $R^S \equiv \text{realizations}^S$ from §5 and $R^B$ is observed
baseline realizations. The reform-vs-baseline delta is

$$
\boxed{
\Delta T_{a, t, k}
\;=\;
\tau^S_{a, t}\,R^S_{a, t, k} - \tau^B_{a, t}\,R^B_{a, t, k}.
}
$$

This general form is necessary because policy reforms typically change
both the rate $\tau$ and the realization base $R$. When the rate is
unchanged ($\tau^S = \tau^B$), the expression simplifies to $\Delta T =
\tau \cdot \Delta R$; when only the rate changes ($R^S = R^B$), it
simplifies to $\Delta T = (\tau^S - \tau^B)\,R$. Mixed reforms exhibit
both channels and the cross-term cannot be separated cleanly into rate
and base effects.

### 8.2 Death-event revenue under deemed realization

Under deemed realization, the additional revenue stream is

$$
R^{\text{death}}_{t, k}
\;=\;
\delta_{\text{realize}}
\sum_a m_{a,t}\,(G^B_{a,t,k} + \Delta G_{a,t,k})\,\overline{\tau}_{a,t},
$$

as in §3.3.2. This is computed in parallel to the standard $\Delta T$ stream
and aggregated to the year-asset level.

### 8.3 Total revenue impact

The total reform-vs-baseline revenue impact for year $t$ is

$$
\Delta T_t
\;=\;
\sum_{a, k} \Delta T_{a, t, k} + \sum_k R^{\text{death}}_{t, k},
$$

with the first term active under all regimes and the second active only
under deemed realization. This is the object that integrates into the
simulator's existing revenue pipeline.

---

## 9. Acknowledged Gaps

The model leaves several phenomena outside its scope. Each is documented
here to make the modeling choices explicit.

* **Charitable dispositions of appreciated assets.** Donating appreciated
  stock allows the donor to deduct fair market value without realizing the
  embedded gain. This bleeds the unrealized stock without going through the
  realization rate function. The model treats this as a calibration
  adjustment to $G^B$ (downward bias) rather than an explicit channel.
* **Forward-looking expectations.** The §4 framework accommodates
  *announced* future rate paths: $\tau_{t+j}$ in the effective-tax-price
  formula can be set to a known forward path, in which case anticipation
  effects emerge naturally (holders pre-realize before announced hikes,
  defer before announced cuts). What remains out of scope is rational
  expectations of *unannounced* future changes — i.e., the model does not
  endogenously forecast future tax-policy uncertainty. Under naive
  expectations $\tau_{t+j} = \tau_t$, the model collapses back to a
  myopic-but-multi-period forward-looking formulation. Full rational
  expectations would require a Bellman solve over the value of holding,
  which the doc deliberately avoids.
* **Mark-to-market regimes.** Annual taxation of accruals collapses
  $\Delta G$ to near-zero by construction (all appreciation is realized
  contemporaneously). The bathtub plumbing remains valid but the channel
  through which revenue accrues differs fundamentally. Out of scope here.
* **Accruals as a policy-dependent flow.** This document treats accruals on
  existing assets as policy-invariant. Wealth feedback effects (deemed
  realization shrinks estates, smaller estates accrue smaller future gains)
  are second-order and not modeled. Revenue from the deemed channel is
  captured directly; the wealth-feedback channel is acknowledged and
  deferred.
* **Sparse-cell calibration.** Some $(a, k)$ cells have thin baseline
  observations (e.g., young cohorts holding pass-through stakes). The
  ratio anchoring still applies but $r_B$ may be noisy. Default fallback is
  to pool $r_B$ across age within an asset class when sample size in a cell
  falls below a threshold.

---

## 10. Implementation Notes

The implementation maps the formalism to the simulator's existing
architecture. This section is light on detail by intent; it points to the
correct hook points and patterns rather than specifying code.

### 10.1 Hook points

* **Per-year cross-sectional state.** The state vector $\boldsymbol{\Delta
  G}_{t, k}$ for each scenario × asset class is persisted to disk between
  years, mirroring the disk-based across-year state pattern already used
  elsewhere in the simulator for behavioral feedback that requires
  prior-year context.
* **Conventional pass only.** The bathtub state evolves only under the
  conventional simulation pass. The static pass holds inputs at baseline
  and produces $\Delta G \equiv 0$ by construction.
* **Year sequencing.** Year-level parallelization is incompatible with the
  recurrence: year $t+1$ requires year $t$'s state. Simulations using this
  module must run years sequentially within a scenario; parallelization
  across scenarios remains available.

### 10.2 Configuration

* **Tax-law parameters.** The regime burden share $c$ and the routing
  triple $(\delta_{\text{vanish}}, \delta_{\text{route}}, \delta_{\text{realize}})$
  are policy choices and live in the tax-law YAML hierarchy alongside the
  rate schedule. A new file (e.g., `death.yaml`) is the natural home.
* **Behavioral parameters.** $\eta_k$, $\beta$, $\lambda_{r,k}$, and
  $\theta$ (bequest motive) are calibration constants, not policy
  choices. They live in a separate behavioral-parameters file outside
  the tax-law tree, alongside other model fixtures such as mortality
  tables.
* **Pre-computed brackets.** The bracket $1 - M_{a,k}(c)$ depends only on
  the life-table mortality path, $\lambda_{r,k}$, $\beta$, and $c$ — not
  on the simulation year or on $\Delta G$. It is computed once at
  scenario startup as a per-cell, per-regime constant and reused across
  all years.
* **Inheritance allocation matrix.** $\omega$ is supplied by the estate
  module's per-year output, extended to carry decedent-age and heir-age
  cohort markers and per-asset transfer amounts.

### 10.3 Pseudocode

```
# at scenario startup
for each asset class k, each age a:
    bracket_B[a,k] = compute_bracket(a, c=0,           m_path, λ_r[k], β)
    bracket_S[a,k] = compute_bracket(a, c=c_reform,    m_path, λ_r[k], β)
    P_B[a,k]       = τ_B · (1 - bracket_B[a,k])
    P_S[a,k]       = τ_S · (1 - bracket_S[a,k])

for each year t in simulation_horizon:
    Δ_prev = load_cohort_state(scenario_id, t)        # zero in first year

    for each asset class k:
        r_S[a,t,k] = r_B[a,t,k] · exp( -η[k] · (P_S[a,k] - P_B[a,k]) )

        for each destination age h:
            survivor = Σ_a A[a→h] · (1 − m[a,t])
                         · [ (1 − r_S[a,t,k]) · Δ_prev[a,k]
                             + G_B[a,t,k] · (r_B[a,t,k] − r_S[a,t,k]) ]
            inherit  = δ_route · Σ_a ω[a→h,t] · m[a,t]
                         · (G_B[a,t,k] + Δ_prev[a,k])
            Δ_next[h,k] = survivor + inherit

        if δ_realize > 0:
            R_death[t,k] = δ_realize · Σ_a m[a,t]
                             · (G_B[a,t,k] + Δ_prev[a,k]) · τ̄[a,t]

    realizations_S = baseline_obs · (r_S · G_S) / (r_B · G_B)
    distribute_within_cell(realizations_S − baseline_obs)   # pro-rata
    ΔT[t] = Σ_a,k τ[a,t] · ΔR[a,t,k] + Σ_k R_death[t,k]

    persist_cohort_state(scenario_id, t+1, Δ_next)
```

### 10.4 Migration from existing aggregate-elasticity modules

The existing capital-gains behavioral feedback modules apply a single
aggregate elasticity to realized gains uniformly. This framework subsumes
those modules: the cohort × asset specification, the regime-conditional
death routing, and the empirical anchoring to baseline together produce a
strictly richer and more disciplined model of realization behavior.
Activation of this framework retires the legacy aggregate modules. Existing
runscripts that toggled the legacy modules continue to function under
default settings ($c = 0$, i.e., step-up baseline) but should be reviewed
for consistency with the calibrated $\eta_k$, $\beta$, $\lambda_{r,k}$
parameter set.
