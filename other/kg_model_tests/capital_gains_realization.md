# A Law of Motion for Policy-Induced Changes in Unrealized Capital Gains

**Author:** John Ricco
**Status:** Design specification (Bellman revision with exogenous-accrual extension)
**Date:** 2026-05-11

This document is the canonical specification for the kg_dynamics behavioral
module (`src/sim/kg_dynamics.R`). For the full discussion of the dynamic-
programming formulation that motivates the realization function, see the
companion PDF `realization_dp (2).pdf` in this folder.

---

## 1. Objective and Scope

A principled accounting system for tracking the policy-induced change in
unrealized capital gains in a repeated-cross-section microsimulation. The
system supports revenue and behavioral analysis of proposals that alter (i)
the capital gains tax rate and (ii) the treatment of unrealized gains at
death.

The simulation contains cross-sectional records indexed by year and age. It
does not follow individuals longitudinally. Nevertheless, the stock of
unrealized capital gains evolves dynamically: gains held by a person aged
$a$ in year $t$ shape what is held by that same person aged $a+1$ in year
$t+1$. The accounting system tracks this dynamic at the age × year level by
constructing a state variable that propagates across age cells and years,
treating each (age, year) cell as a synthetic cohort.

In scope:

* A law of motion for the policy-induced delta in unrealized gains, indexed
  by age, year, and asset class.
* A two-channel realization model splitting forced turnover from
  discretionary realization, with the discretionary rate derived from a
  per-unit Bellman equation for the holder's value of unrealized gain.
* An exogenous-accrual extension that scales the Bellman's continuation
  value by $(1+g_{a,t})$, where $g_{a,t}$ is the implied per-cell accrual
  rate from baseline data.
* A regime taxonomy that supports step-up basis, carryover basis, and
  deemed realization at death, formulated through generic regime hooks so
  that additional regimes can be added without rederiving the recurrence.
* A ratio-based empirical anchoring that guarantees baseline reproduction.
* A calibration posture that distinguishes parameters drawn from data,
  calibrated to literature targets, and held as tunable assumptions.

Out of scope:

* The Haig-Simons income measure and accrual-based distributional analysis.
* Mark-to-market regimes that tax accruals annually.
* Charitable dispositions of appreciated assets.
* Wealth feedback from a deemed-realization regime onto future accruals.

---

## 2. Notation and Primitives

### 2.1 Indices

* $a \in \{a_{\min}, \ldots, a_{\max}\}$ — age of the tax unit. The bathtub
  recurrence runs on $a \in [18, 80]$ with the topcoded cell at $a_{\max} =
  80$. The Bellman backward induction runs on an extended grid $a \in
  [18, 119]$ with terminal $W[120, \cdot] = 0$; the extended ages [81, 119]
  exist only for the Bellman.
* $t$ — year of the simulation.
* $k$ — asset class. The current implementation collapses the five tracked
  classes into a single bucket.

### 2.2 Stocks and flows from data (baseline)

Each $(a, t)$ cell carries:

* $G^B_{a,t}$ — baseline unrealized taxable gain stock, $\sum_i \max(0,
  V_{i,k} - B_{i,k})$ summed over tax units $i$ in the cell.
* $r^B_{a,t}$ — baseline realization rate, $R^B_{a,t} / G^B_{a,t}$.
* $m_{a,t}$ — cell-aggregate mortality (joint-filer households use $m_1
  \cdot m_2$). For ages 81-119, $m$ comes from the Trustees Report
  PerLifeTables Alternative-2 projections, 50/50 male/female blended.

### 2.3 Allocation operators

* $A_{a \to h, t}$ — aging transition. $A_{a \to a+1, t} = 1$ for $a < 80$;
  $A_{80 \to 80, t} = 1$ topcode loop.
* $\omega_{a \to h, t}$ — heir-allocation matrix, row-stochastic in $h$.
  Currently a Gaussian centered at $a - 30$ with $\sigma = 5$; the estate
  module hookup is on the roadmap.

### 2.4 Policy primitives

* $\tau_{a,t}$ — cell-average capital gains marginal tax rate, realization-
  weighted from per-record `mtr_kg_lt` in static detail.
* $c \in [0, 1]$ — death-state burden share. $c = 0$ under step-up, $c =
  \theta$ under carryover (the bequest motive), $c = 1$ under deemed
  realization.
* $(\delta_{\text{vanish}}, \delta_{\text{route}}, \delta_{\text{realize}})$
  — routing triple at death; sums to 1. Step-up: $(1,0,0)$; carryover:
  $(0,1,0)$; deemed: $(0,0,1)$.

### 2.5 Reform-side state

$\Delta G_{a,t} = G^S_{a,t} - G^B_{a,t}$, with $\Delta G_{a,t} = 0$ in
the first year of any scenario.

---

## 3. Law of Motion

Same recurrence as the prior spec — the Bellman replaces how $r^S_{a,t}$
is computed but does not change the stock dynamics.

$$
\boxed{
\begin{aligned}
\Delta G_{h, t+1}
\;=\;&
\sum_a A_{a \to h, t}\,(1 - m^{\text{eff}}_{a,t})\!\left[
(1 - r^S_{a,t})\,\Delta G_{a,t}
+ G^B_{a,t}\,(r^B_{a,t} - r^S_{a,t})
\right] \\
&+\; \delta_{\text{route}}
\sum_a \omega_{a \to h, t}\,m^{\text{eff}}_{a,t}\,(G^B_{a,t} + \Delta G_{a,t}).
\end{aligned}
}
$$

**Within-cell effective mortality** $m^{\text{eff}}$ corrects for the
wealth-mortality covariance (wealthier holders carry more $G$ AND die
less). Under the G-proportional allocation rule (default,
`KG_DYN_DG_ALLOCATION = 'G'`):

$$
m^{\text{eff}}_{a,t} \;=\; \frac{\sum_i w_i\, m_i\, G_{\text{unit},i}}{\sum_i w_i\, G_{\text{unit},i}}.
$$

This is per-record-correct under the assumption that $dG$ within a cell is
allocated proportional to $G_\text{unit}$. The R-rule (allocation
proportional to realized gains) is also supported with a fallback to G when
$R^B = 0$.

**Deemed-realization revenue stream** (parallel to the standard
$\Delta T$):

$$
R^{\text{death}}_{t}
\;=\;
\delta_{\text{realize}}
\sum_a m^{\text{eff}}_{a,t}\,(G^B_{a,t} + \Delta G_{a,t})\,\overline{\tau}_{a,t}.
$$

---

## 4. Realization Function: Bellman Formulation

The reform-side discretionary realization rate $r^{D,S}_{a,t}$ is derived
from a per-unit Bellman equation for the value $W_{a,t}$ of holding one
dollar of unrealized gain. The Bellman correctly internalizes the holder's
own future discretionary choices, eliminating the internal inconsistency
of the prior closed-form bracket spec (which assumed no future
discretionary realizations).

### 4.1 Two-channel decomposition

Total realizations split into a policy-invariant turnover hazard and a
discretionary tax-responsive rate:

$$
r_{a,t} \;=\; \lambda^T_{a,t} + r^D_{a,t}, \qquad
\lambda^T_{a,t} \;=\; \phi_I \cdot r^B_{a,t}, \qquad \phi_I = 0.4.
$$

### 4.2 Bellman equation (per unit of unrealized gain)

$$
W_{a,t} = \max_{r^D \in [0, 1-\lambda^T_{a,t}]}
\Big\{\,b_{a,t}((1-\tau_{a,t})r^D) \;-\; (1-\lambda^T_{a,t}-r^D)\,m_{a,t}\,c\,\tau_{a,t}
\;+\; (1-\lambda^T_{a,t}-r^D)\,(1+g_{a,t})\,(1-m_{a,t})\,\beta\,W_{a+1,t+1}\,\Big\}
$$

The first term is the flow nontax benefit from after-tax discretionary
realization. The second is the death-state tax cost on the surviving
stock. The third is the discounted continuation value — with **the
$(1+g_{a,t})$ factor capturing exogenous accrual** on the dollar of gain
that rolls forward (see §5).

### 4.3 Marginal-benefit form and effective tax price

Choosing the marginal-benefit form $b'_{a,t}(x) = \mu_{a,t} - (1/\eta)
\log x$, where $x = (1-\tau_{a,t}) r^D$, gives the FOC:

$$
r^{D*}_{a,t} \;=\; \frac{1}{1-\tau_{a,t}}\exp\!\big(\eta\,(\mu_{a,t} - P_{a,t})\big),
$$

with the **effective tax price**

$$
\boxed{
P_{a,t} \;=\; \frac{(1+g_{a,t})(1-m_{a,t})\,\beta\,W_{a+1,t+1} \;-\; m_{a,t}\,c\,\tau_{a,t}}{1-\tau_{a,t}}
\;-\; \frac{1}{\eta}\log(1-\tau_{a,t}).
}
$$

The intercept $\mu_{a,t}$ cancels when ratioing scenario to baseline, and
the realization rate reduces to the baseline-anchored form

$$
\boxed{
r^{D,S}_{a,t} \;=\; r^{D,B}_{a,t}\cdot\exp\!\big(-\eta\,(P^S_{a,t} - P^B_{a,t})\big).
}
$$

### 4.4 Backward induction

The Bellman is solved on a finite age grid $[a_{\min}, A_{\max}^{\text{Bell}} =
119]$ with terminal $W[A_{\max}^{\text{Bell}}+1, t+1] = 0$. The recursion
proceeds outer-loop backward in time, inner-loop backward in age. The
terminal year column is seeded by a stationary backward sweep at $t_{\max}$
using year-$t_{\max}$ primitives extended forward; subsequent columns
march backward using the previously-computed $W[\cdot, t+1]$.

**Pass 1 (baseline).** $c_\phi^B = 0$ under current-law step-up. The
algorithm recovers $\mu_{a,t}$ from the baseline FOC:

$$
\mu_{a,t} \;=\; P^B_{a,t} + \frac{1}{\eta}\log\!\big((1-\tau^B_{a,t})\,r^{D,B}_{a,t}\big),
$$

with a floor $r^{D,B} \ge 10^{-6}$ to keep the log well-defined in sparse
cells.

**Pass 2 (scenario).** With $\mu_{a,t}$ from Pass 1, compute $P^S_{a,t}$
using scenario $\tau^S$, $c_\phi^S$ (which may vary by year), and the same
$g_{a,t}$. Apply the closed-form FOC for $r^{D,S}_{a,t}$, clip to $[0,
1-\lambda^T_{a,t}]$, and propagate $W^S_{a,t}$ backward.

**Implementation.** Codified in `kg_dyn_solve_bellman_baseline` and
`kg_dyn_solve_bellman_scenario` in `src/sim/kg_dynamics.R`. Both call
`kg_dyn_bellman_sweep_age` for the inner age-backward sweep.

### 4.5 Forward-looking properties

Because Pass 2 evaluates $W[a+1, t+1]$ using *actual* future-year
$\tau^S_{t+j}$, the holder anticipates announced rate paths. An announced
hike in year $t^*$ causes $r^{D,S}$ to drop in years $t < t^*$ (deferral)
and surge in year $t^*$ (release of accumulated lock-in). The model
remains myopic about unannounced changes.

---

## 5. Exogenous Accrual Extension

### 5.1 Motivation

The PDF Bellman implicitly assumes today's $1 of unrealized gain rolls
forward as $1 of unrealized gain tomorrow (minus what got realized). In
reality the underlying asset appreciates, so the surviving dollar becomes
$(1+g)$ dollars of unrealized gain at $(a+1, t+1)$. Without this factor,
$W$ understates the continuation value of holding when assets are growing,
and the agent's deferral incentive is understated.

### 5.2 Implied accrual rate from baseline

From the cell-aggregate accounting identity under step-up baseline,

$$
G^B_{a+1, t+1} \;\approx\; (1-m_{a,t})(1-r^B_{a,t})\,G^B_{a,t}\,(1+g_{a,t}),
$$

solving for $g$:

$$
\boxed{
1 + g_{a,t} \;=\; \frac{G^B_{a+1, t+1}}{(1-m_{a,t})(1-r^B_{a,t})\,G^B_{a,t}}.
}
$$

This is the **implied per-survivor accrual rate** that makes the Bellman's
continuation internally consistent with the baseline $G$ path.

### 5.3 Noise handling

Empirically (`baseline_check` against full-sample baseline 2026-2035) the
per-cell estimator is noisier than expected: ~13% of cells (5%
G-weighted) show $|g| > 0.5$, concentrated at specific ages (19, 22, 25,
30, 64, 79) where $G^B$ systematically jumps year over year due to
demographic churn (new asset acquisition, marital-status re-imputation,
cohort entry/exit) rather than asset appreciation.

The implementation supports four rules for treating this noise, via
`KG_DYN_G_RULE`:

| Rule | Description |
|---|---|
| `per_cell_cap` (default) | Clamp raw $g(a,t)$ to $[-0.5, +0.5]$ |
| `per_cell_smoothed` | Use raw $g$ where $|g| \le 0.5$; replace cap-hits with G-weighted mean of valid cells |
| `per_age` | Single $g(a)$ profile per age (G-weighted average across years, after dropping extreme cells) |
| `scalar` | One G-weighted scalar across the whole model |

The default `per_cell_cap` keeps fidelity to baseline where the data is
trustworthy and bounds the pathological cells without smoothing.
Sensitivity to the rule should be reported with new scenarios.

---

## 6. Empirical Anchoring

### 6.1 Ratio formulation

Realizations are applied as a ratio to observed baseline so calibration
imperfections in $r$ don't break baseline reproduction:

$$
\text{realizations}^S(a, t)
\;=\;
\text{realizations}^B_{\text{obs}}(a, t) \cdot
\frac{r^S_{a,t}\,G^S_{a,t}}{r^B_{a,t}\,G^B_{a,t}}.
$$

When reform equals baseline ($P^S = P^B$, so $r^S = r^B$ and $G^S = G^B$),
the ratio collapses to one identically.

### 6.2 Reform-induced delta in realizations

$$
\Delta R_{a, t}
\;=\;
\text{realizations}^B_{\text{obs}}(a, t) \left[
\frac{r^S\,(G^B + \Delta G)}{r^B\,G^B} - 1
\right].
$$

Per-record distribution via the convention in §8.3.

---

## 7. Calibration

| Parameter | Cardinality | Identification |
|---|---|---|
| $r^B_{a,t}$ | $\|a\| \cdot \|t\|$ | Observed from simulator baseline |
| $m_{a,t}$ | $\|a\| \cdot \|t\|$ | Per-record household mortality for [18,80]; PerLifeTables for [81,119] |
| $g_{a,t}$ | $\|a\| \cdot \|t\|$ | Implied from baseline G path (§5.2), per `KG_DYN_G_RULE` |
| $\eta$ | 1 | Calibrated to elasticity target (-0.62 at year 30) under step-up via `other/kg_model_tests/calibrate_eta.R` |
| $\phi_I$ | 1 | $\phi_I = 0.4$ (asset-aggregate turnover share) |
| $\beta$ | 1 | $\beta = 0.96$ |
| $\theta$ (bequest motive) | 1 | Tunable, default 0.5, sensitivity panel |
| $\omega$ | exogenous | From estate module (placeholder Gaussian for now) |

The response parameter $\eta$ is calibrated to match a target aggregate
semi-elasticity under current law (step-up). The procedure:

1. Build baseline cell aggregates and cell-MTRs from full-sample
   baseline static detail.
2. Construct a uniform 1pp $\tau$ perturbation.
3. For a candidate $\eta$, run the Bellman pre-pass and bathtub
   recurrence for 30 years; compute year-30 implied elasticity.
4. Bisect $\eta$ until elasticity hits the target.

Because the Bellman is more responsive to $\tau$ than the prior closed-
form bracket (anticipation propagates through future $r^{D*}$), the
calibrated $\eta$ under the Bellman differs from the prior value. See
`KG_DYN_DEFAULT_ETA` in `src/sim/kg_dynamics.R` for the current default.

---

## 8. Conventions and Boundaries

### 8.1 Joint-filer cohort assignment

$a_{\text{joint}} = \max(a_1, a_2)$. The older spouse anchors the cohort
because the step-up benefit binds most strongly near end-of-life.
Household survival is $m_{\text{joint}} = m_1 \cdot m_2$.

### 8.2 Boundary cells

* **Youngest cohort.** Tax units at $a_{\min} = 18$ have no aged-survivor
  inflow; their $\Delta G$ accumulates only through inheritance flow
  under carryover.
* **Bathtub topcode (80).** Survivors at age 80 remain at 80 ($A_{80 \to
  80} = 1$). Cell aggregates pool all 80+ tax units; $m_{80, t}$ is a
  weight-averaged 80+ household death rate.
* **Bellman extension (81-119).** No simulator data; $G^B = R^B = r^B =
  0$ by construction. Only $m_{a,t}$ is populated, from PerLifeTables.
  The extended grid exists so that the Bellman backward induction has a
  true terminal condition driven by mortality.

### 8.3 Within-cell distribution to tax units

Cell-aggregate $\Delta R_{a, t}$ distributes to individual tax units as:

1. **Pro-rata to baseline realizations** if the cell has $R^B > 0$.
2. **Pro-rata to baseline gain stock** if $R^B = 0$ but $G^B > 0$.
3. **Skip** otherwise.

---

## 9. Revenue and Downstream Use

### 9.1 Ongoing realization revenue

$$
\Delta T_{a, t}
\;=\;
\tau^S_{a, t}\,R^S_{a, t} - \tau^B_{a, t}\,R^B_{a, t}.
$$

### 9.2 Death-event revenue under deemed realization

$$
R^{\text{death}}_{t}
\;=\;
\delta_{\text{realize}}
\sum_a m^{\text{eff}}_{a,t}\,(G^B_{a,t} + \Delta G_{a,t})\,\overline{\tau}_{a,t}.
$$

### 9.3 Total revenue impact

$$
\Delta T_t \;=\; \sum_a \Delta T_{a, t} \;+\; R^{\text{death}}_{t}.
$$

---

## 10. Implementation Notes

### 10.1 Hook points

* **Per-year cross-sectional state.** Bathtub state files persist to disk
  under `{scenario_output}/conventional/supplemental/kg_dynamics_state/`,
  one file per year.
* **Conventional pass only.** The bathtub state evolves only under the
  conventional simulation pass. The static pass holds inputs at baseline
  and produces $\Delta G \equiv 0$ by construction.
* **Year sequencing.** Year-level parallelization is incompatible with
  the bathtub recurrence: year $t+1$ requires year $t$'s state. Scenarios
  using this module must run years sequentially within a scenario;
  parallelization across scenarios remains available.
* **Bellman pre-pass.** Runs once per scenario at the start of
  `kg_dyn_run_bathtub_pass`. Both passes solve in a few hundred ms total.

### 10.2 Configuration

* **Tax-law parameters.** Regime hooks ($c$, routing triple) live in the
  tax-law YAML hierarchy (`pref.kg_death_regime`,
  `pref.kg_bequest_motive`).
* **Behavioral parameters.** $\eta$, $\beta$, $\phi_I$ are constants in
  `src/sim/kg_dynamics.R`. The g-handling rule `KG_DYN_G_RULE` is a
  constant in the same file.
* **Life table.** Ages 18-80 from per-year simulator cell aggregates;
  ages 81-119 from `resources/PerLifeTables_*_Alt2_TR2024.csv` (Trustees
  Report Alternative-2 projections, 50/50 male/female blended).
* **Inheritance allocation matrix.** $\omega$ is currently a placeholder
  Gaussian centered at $a - 30$ with $\sigma = 5$. Estate module hookup
  is on the roadmap.

### 10.3 Output

Per scenario, two diagnostic CSVs are written:

* `kg_dynamics_age_profile.csv` — long-format (year × age) cell table
  with $G^B$, $R^B$, $r^B$, $r^S$, $\lambda^T$, $r^{V,B}$, $r^{V,S}$,
  $m$, $dG$, $\tau^B$, $\tau^S$, $W^B$, $W^S$, $P^B$, $P^S$, $\mu$,
  $r^{D,B}$, $r^{D,S}$, $g$.
* `kg_dynamics_summary.csv` — year-level rollup with G-weighted averages,
  channel decomposition, decedent stock, and implied year-by-year
  semi-elasticity.

Two .rds artifacts also live in the state directory:

* `life_table_extension.rds` — PerLifeTables blended q(x) for [81, 119]
  × scenario years.
* `g_matrix.rds` — implied accrual rate matrix used by the Bellman.

---

## 11. Acknowledged Gaps

* **Charitable dispositions of appreciated assets.** Treated as a
  calibration adjustment to $G^B$ rather than an explicit channel.
* **Forward-looking expectations of unannounced changes.** The model
  uses the announced $\tau$ path but does not endogenously forecast
  uncertainty.
* **Mark-to-market regimes.** Out of scope.
* **Asset-class disaggregation.** Currently one bucket; the spec
  supports five.
* **Wealth feedback from deemed realization.** Not modeled; revenue
  from the deemed channel is captured directly but the wealth-shrink
  feedback channel is acknowledged and deferred.
* **Sparse-cell baseline rates.** Cells with $G^B > 0$ but $R^B = 0$
  inherit a pooled $r^B$ across the asset class.
