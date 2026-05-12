# Capital Gains Realization Behavior: Brainstorm on Forward-Looking Extensions

> **Status note (2026-05-11):** This brainstorm predates the Bellman + (1+g)
> reformulation in `capital_gains_realization.md` and `realization_dp (2).pdf`.
> References to `kg_dyn_compute_bracket` and the closed-form "bracket"
> $1 - M(c)$ describe a function and object that no longer exist in
> `src/sim/kg_dynamics.R`; the analogous role is now played by the Bellman-
> derived $P_{a,t}$ in `kg_dyn_solve_bellman_baseline` /
> `kg_dyn_solve_bellman_scenario`. The substantive ideas in the menu below
> (threshold avoidance, intertemporal smoothing, income-process feedback,
> etc.) are still candidates for future extensions, but the implementation
> hooks would target the Bellman pre-pass rather than the old bracket cache.

A brainstorm on functional-form extensions to the kg_dynamics behavior
module, with specific reference to threshold-avoidance ("$1M chunking") and
intertemporal smoothing. Written as a companion to
`capital_gains_realization.md` (the existing spec), not a replacement.

Audience: model developer. Math is informal; the goal is to lay out a menu
of options with enough specification that we can pick one and write a
proper spec.

---

## 0. TL;DR

The current model is **less myopic than the prompt suggests** — the bracket
$1 - M_{a,k}(c)$ already integrates expected future tax payments over the
holder's residual life, with discount factor $\beta$ and competing-risks
hazard $\lambda_r + m$. So when we say "myopic," what we actually mean is
narrower:

1. The cell's own tax rate $\tau_{a,t}$ is treated as **constant in
   expectation** along the horizon (modulo announced rate paths). There is
   no income-process feedback into the bracket — the cell never anticipates
   "next year my income mean-reverts and I'll cross a threshold."
2. The realization rate is a **smooth exponential** in
   $P^S - P^B$ — by construction it cannot produce kinks, notches, or
   bunching. A $1M-threshold reform looks the same as a uniform rate hike,
   modulated only through the realization-weighted $\tau$.
3. **Cells are age cohorts only** (`age_cohort` 18–80, asset classes
   collapsed). Two taxpayers with very different distance to a $1M
   threshold but the same age get the same realization rate.
4. The agent maximizes a **continuous one-shot rate**, not a
   period-by-period quantity decision $G_t \in [0, g_t]$. There is no
   "release some now, save the rest for next year."

Everything below is about closing one or more of those four gaps. Six
candidate forms are ranked by intrusiveness; the recommended phased
implementation appears in §6.

---

## 1. What the current model actually does

The kg_dynamics machinery (see `src/sim/kg_dynamics.R` and the
behavior module at `config/scenarios/behavior/kg_dynamics/eta06.R`) has
three pieces of forward-looking content already baked in:

**(a) The bracket is a forward integral.** From §4.2 of the spec:

$$P_{a,t}(c) = \tau_t - \sum_{j=1}^{H} \beta^j s_{a,j}\, \tau_{t+j}
              - c \sum_{j=1}^{H} \beta^j d_{a,j}\, \tau_{t+j}$$

with hazard sequences from competing risks
$s_j = S_j \lambda_r$ (voluntary realization) and
$d_j = S_j m_{a+j}$ (death-without-realization), survival
$S_j = \prod_{k<j}(1 - \lambda_r - m_{a+k})$.

This is structurally identical to a Constantinides (1983) / Auerbach
(1991) optimal-stopping bracket with discrete-time approximation. It
explicitly tells the holder: "if I realize today I pay $\tau_t$; if I hold
I face $\tau_{t+j}$ later weighted by the probability I voluntarily
realize then ($s_j$) plus the probability of death ($d_j$) times the
post-death burden share $c$." The tau path
$\{\tau_{t+j}\}$ can be fed an announced reform (the implementation
exposes a `tau_ratio` argument, see `kg_dyn_compute_bracket` at
`src/sim/kg_dynamics.R:202`), so the architecture *already supports*
deterministic announced rate paths. We do not use this hook in production
runs but it is there.

**(b) Mortality drives a real intertemporal margin.** Step-up at death
collapses the embedded gain to zero. The bracket internalizes that
holders facing higher $m$ defer more aggressively because the $c\sum\beta^j d_j \tau_{t+j}$
term collapses (under step-up $c=0$). This is the lock-in we observe in
older cohorts in calibration.

**(c) The realization rate is anchored to a baseline observed rate.**
$r^S_{a,t,k} = r^B_{a,t,k} \cdot \exp(-\eta_k (P^S - P^B))$
guarantees zero baseline-on-baseline delta and ties elasticity to level
through the same bracket. This is elegant but constrains the functional
form: log-deviations in the realization rate are linear in the price
wedge $P^S - P^B$. There is no curvature in $\eta$.

**What the model does not contain.** The cell's own income $y_{-i,t}$ does
not enter anywhere. There is no within-cell heterogeneity in optimal
realization timing. The bracket is computed at the cell-mean $\tau$, so a
holder right at the threshold is treated identically to a holder $200k
above or below.

This is why the user is right to call it "representative agent per cell" —
the per-cell $\tau$ is an aggregation that washes out exactly the
information we need to model threshold behavior.

---

## 2. The behaviors we want to capture

Before proposing forms, let's enumerate the specific empirical regularities
the literature documents, mapped to the user's question:

| # | Behavior | Empirical anchor | Currently captured? |
|---|---|---|---|
| 1 | Permanent-rate elasticity ≈ −0.5 to −0.7 | Burman & Randolph 1994; Dowd-McClelland-Muthitacharoen 2015 | Yes, via η calibration |
| 2 | Transitory elasticity ≈ −1.0 to −6.4 | Burman & Randolph 1994; Agersnap-Zidar 2021 (1-yr horizon) | Partially (announced paths only) |
| 3 | Pre-announcement realization spike | Auerbach-Burman-Siegel 2000; Hines-Schaffa 2023 | Yes, if we feed the announced path |
| 4 | Lock-in from step-up | Stiglitz 1983; Constantinides 1983 | Yes, via bracket with $c=0$ |
| 5 | Heterogeneous elasticity by holder type | Sarin-Summers-Zidar-Zwick 2022 | No (single $\eta$, single elastic class) |
| 6 | **Threshold/notch bunching** | Saez 2010; Kleven & Waseem 2013; Dowd-McClelland 2019 | **No** |
| 7 | **Intertemporal smearing across years near a threshold** | Le Maire & Schjerning 2013 | **No** |
| 8 | Frictions / inattention share | Kleven & Waseem 2013 | No (deterministic response) |
| 9 | Rational expectations over uncertain future rates | Hines-Schaffa 2023 | No (out of scope per §9) |

The user's $1M-threshold example sits squarely in rows 6 and 7. Rows 5
and 8 are adjacent and worth picking up at the same time because they
share the same per-record machinery.

---

## 3. The architectural problem

The realization-rate function is solved per cell, not per record, but
threshold behavior is fundamentally a per-record phenomenon: cell A's
median taxpayer might be $300k below threshold (no chunking incentive)
while cell A's 90th percentile is $400k above (significant chunking
incentive). Aggregating to a single cell-level $\tau$ erases this
heterogeneity.

There are two architectural responses:

**Option α — re-cell by income.** Replace `age_cohort` with
`age_cohort × income_bin`, then compute per-cell $\tau$ and $r^B$ on the
finer partition. The bracket and recurrence are unchanged in form. Costs:
sparser cells (more pooling fallback), more state to persist, and a
nontrivial decision about how to define income bins (especially under
reforms that change the threshold itself).

**Option β — keep age cells, modify the applier.** Augment
`kg_dyn_apply_to_records` (`src/sim/kg_dynamics.R:344`) with a per-record
*adjustment factor* that depends on the record's distance to the threshold
under baseline. The cell-level recurrence remains the workhorse for
average behavior; threshold logic is a sidecar applied at the per-record
distribution stage. The cell-level totals are preserved in expectation by
imposing a sum constraint on the adjustment factors within each cell.

I recommend **Option β** as the default and Option α as an opt-in for
deeper fidelity. The rationale: Option β is decisive about where the new
behavior lives (record-level applier), preserves the existing calibration,
and is composable with all six forms below. Option α introduces a
calibration choice (income binning) that we should defer until we have
infrastructure to study its sensitivity.

---

## 4. Six candidate functional forms

Ordered roughly from least to most intrusive. Each is specified with
(i) motivation, (ii) math, (iii) state variables, (iv) calibration source,
(v) hook into the current code.

### Form A — Permanent/Transitory Decomposition via Smoothed Tau

**Motivation.** Burman-Randolph (1994) is the canonical permanent-vs-
transitory split, and Hines-Schaffa (2023) argue that the bulk of the
"permanent elasticity" measured in the literature is actually an
*expectations* elasticity. Operationally: feed the bracket a smoothed
expected tau path rather than the spot tau.

**Math.** Define a perceived permanent rate

$$\tilde\tau_{a,t}^p = \frac{\sum_{j=0}^{H} \beta^j s_{a,j} \tau_{t+j}}{\sum_{j=0}^{H} \beta^j s_{a,j}}$$

— the realization-probability-weighted forward-looking average. Then
bypass the spot tau entirely and use $\tilde\tau^p$ in computing the
effective price level, while keeping a transitory deviation in the
realization rate:

$$r^S_{a,t} = r^B_{a,t} \cdot \exp\big(-\eta_p (P^p_S - P^p_B) - \eta_T (\tau_t - \tilde\tau^p_t)\big)$$

with $\eta_p$ calibrated to the permanent elasticity (~−0.5) and $\eta_T$
to the transitory elasticity (~−1 to −2).

**State.** None new — uses the existing tau path machinery.

**Calibration.** Two-target bisection: hit the permanent elasticity
target under a level shift and the transitory elasticity under a one-year
deviation. Specs in `src/sim/kg_dynamics.R:120` (`calibrate_eta`) need to
become a 2D solver.

**Hook.** Replace the single-eta scalar at `KG_DYN_DEFAULT_ETA` with a
$(\eta_p, \eta_T)$ tuple. Modify `kg_dyn_compute_bracket` to return the
$\tilde\tau^p$ alongside the bracket; modify `kg_dyn_step_recurrence` to
include the transitory term.

**Verdict.** Cheap. Resolves an inconsistency in the existing
calibration (we currently fit to a single elasticity blend) and gives us
the right behavior under announced paths. **Doesn't address bunching.**

---

### Form B — Threshold-Aware Per-Record Adjustment

**Motivation.** Saez (2010) bunching at a kink, applied per-record. The
$1M threshold is a kink in the marginal tax schedule (top rate
applies above), and the structural response is that taxpayers near the
threshold compress their realizations to stay below.

**Math.** Define each record $i$'s threshold proximity:

$$d_{i,t} = \frac{y^*_i - y_{-i,t}}{y^*_i}$$

where $y_{-i,t}$ is non-realization income and $y^*_i$ is the relevant
threshold ($1M, $\bar y$). For records with $d_{i,t} > 0$ (would-realize
crosses threshold), apply a shrinkage factor

$$\phi(d_{i,t}, \tau_{\text{above}} - \tau_{\text{below}}, e) = \begin{cases}
\exp\big(-e \cdot \Delta\ln(1-\tau)\big) & \text{if } d_{i,t} \in [0, \bar d] \\
1 & \text{otherwise}
\end{cases}$$

with $e$ the local bunching elasticity from Saez (~0.2–0.5 for taxpayers,
larger for self-employed). The shrinkage *moves* gains from "would-cross"
records to a delayed bucket (next-year carryover, see Form C). The
"compressed zone" $\bar d$ is the Saez "Δz*" — calibrated so that the
post-shrinkage density just to the right of threshold equals the
counterfactual pre-shrinkage density.

A friction parameter $a \in [0,1]$ (Kleven-Waseem inattention share)
governs what fraction of would-cross records actually adjust:

$$G^{\text{adj}}_{i,t} = (1-a) \cdot \phi \cdot G^{\text{plan}}_{i,t} + a \cdot G^{\text{plan}}_{i,t}$$

**State.** Per-record: $y_{-i,t}$ (non-KG income), available from the
tax-unit dataframe as
`agi - pmax(kg_lt, 0) - pmax(kg_st, 0)` or similar. No new persisted state.

**Calibration.** Three parameters: $e$, $\bar d$, $a$. Defaults:
$e=0.3$ (mid-range labor-supply bunching, Chetty 2012),
$a=0.7$ (Kleven & Waseem 2013 mid-range — most taxpayers don't bunch),
$\bar d$ chosen so $\bar d \cdot y^* = e \cdot \bar y \cdot \Delta\ln(1-\tau) / a$
(Saez frictionless formula adjusted for inattention). Treat as
calibration constants in a YAML; sensitivity-test ±0.1.

**Hook.** Add a function `kg_dyn_threshold_shrink` called inside
`kg_dyn_apply_to_records`, after `kg_lt_rate` is computed and before the
final assignment. The shrunk amount $G^{\text{plan}}_{i,t} - G^{\text{adj}}_{i,t}$
goes into a record-level accumulator that is added to next year's
realizations (this requires a small persistent state — see Form C).

**Verdict.** This is the minimum viable response to the prompt. It
captures the static bunching but punts on where the deferred gains go.

---

### Form C — Le Maire–Schjerning Two-Period Smearing

**Motivation.** Form B static-bunching is incomplete because the
realization that doesn't happen *this year* is not lost — it gets pushed
to next year. Le Maire & Schjerning (2013) is the canonical correction:
they show that ignoring intertemporal smearing inflates static elasticity
estimates by 2–3x because what looks like a static bunching response is
actually a multi-year smoothing problem.

**Math.** For each would-cross record, solve a 2-period (or H-period)
optimization:

$$\min_{\{G_t, \ldots, G_{t+H}\}} \sum_{j=0}^{H} \beta^j \, T(y_{-i,t+j} + G_{i,t+j}) \cdot G_{i,t+j}$$

subject to $\sum_j G_{i,t+j} = G^*_{i,t}$ (planned total realization),
$G_{i,t+j} \geq 0$. With a single threshold and isoelastic disutility of
"holding longer than planned," the closed form puts as much as fits below
the threshold each year and rolls the rest forward. With friction $a$,
only $(1-a)$ of would-cross records solve this; the rest realize
$G^*_{i,t}$ all at once.

**Implementation simplification.** A 2-period version is enough for the
threshold-chunking story. The optimal split when $\tau_{above} > \tau_{below}$:

$$G^*_{i,t} = \min(G^{\text{plan}}_{i,t}, \, y^*_i - y_{-i,t}) \quad \text{(stay below threshold)}$$
$$G^*_{i,t+1} = G^{\text{plan}}_{i,t} - G^*_{i,t}$$

unless next year's $y_{-i,t+1}$ is forecast to be even higher, in which
case the holder may prefer to absorb the threshold cross today.

**State.** Need a per-record persistent variable
`kg_lt_deferred[i,t+1]` rolled from year to year. This is the analog of
the bathtub state for individual records and requires a new persistence
hook in the kg_dynamics module (file per scenario per year, keyed by
record id).

**Calibration.** Same parameters as Form B plus a forecasting rule for
$y_{-i,t+1}$. Simplest: AR(1) calibrated to historical income volatility
(Guvenen et al. 2014 give $\rho \approx 0.95$ for log earnings, much
lower for capital income — keep separate). Or: assume $y_{-i,t+1} = y_{-i,t}$
and let the recurrence work it out.

**Hook.** Add `kg_dyn_record_state` parallel to `kg_dyn_state` —
persists per-record `kg_lt_deferred` between years. Called inside
`kg_dyn_apply_to_records` after Form B's shrinkage; the deferred amount
is added to next year's `kg_plan` for the same record. The cell-level
totals self-correct because the deferred amounts re-enter the pool next
year.

**Verdict.** This is the correct treatment of the user's example. Cost
is the per-record persisted state, which is mechanically similar to the
bathtub but at finer granularity.

---

### Form D — Constantinides Hazard with Threshold Penalty

**Motivation.** Generalize Form A to a logit-hazard form that natively
handles thresholds. Replace the smooth exponential with a richer
parametrization that admits non-smooth response.

**Math.** Replace the exponential rate function with

$$r^S_{a,t,k}(g, y_-) = r^B_{a,t,k} \cdot \Lambda\!\big(\beta_0 + \beta_g \ln g - \beta_\tau (P^S - P^B) - \beta_T \cdot \mathbb{1}[y_- + g > y^*]\cdot(g - (y^* - y_-))_+\big)$$

where $\Lambda$ is the logit and the last term is an explicit threshold
penalty: it raises the cost of realizing the portion of $g$ that would
push the holder above $y^*$. With $\beta_T \to \infty$ the holder
realizes exactly $y^* - y_-$ and no more — sharp bunching at the
threshold. With $\beta_T = 0$ the form collapses to a smooth Constantinides
hazard.

**State.** Per-record $g$ and $y_-$. Same as Form B.

**Calibration.** Four parameters: $\beta_0, \beta_g, \beta_\tau, \beta_T$.
Calibrate $\beta_\tau$ to the Dowd-McClelland-Muthitacharoen elasticity,
$\beta_T$ to Dowd & McClelland (2019) bunching at the short-term/long-term
holding-period notch (cleanest U.S. cap-gains analog), $\beta_g$ from
panel evidence on "cleaning out" old gains (less established;
sensitivity analysis), $\beta_0$ pinned by $r^B$ at baseline.

**Hook.** This is a structural rewrite of `kg_dyn_step_recurrence`. The
realization rate stops being a per-cell scalar and becomes a per-record
function evaluated inside the applier. Cell-level $r^S$ becomes a
weighted average of per-record realization rates. Significant code
change.

**Verdict.** Cleaner than Form B+C in the sense that it has one
unifying functional form, but more disruptive to the existing
calibration and machinery. Better fit if we anticipate multiple
threshold reforms (e.g., proposals at $400k AND $1M AND $5M) where
hard-coded shrinkage logic gets unwieldy.

---

### Form E — Sarin–Summers–Zidar–Zwick Group-Specific Elasticity

**Motivation.** Heterogeneity in elasticity by holder type. SSZ&Z (2022)
argue the average realization elasticity has fallen from −0.7 to −0.5
since the 1990s because the share of inelastic holders (passive funds,
retirement accounts, carried interest) has grown. Decomposing by holder
type gives us a more durable model.

**Math.** Tag each record as elastic ($E$) or inelastic ($I$) based on
asset composition:

$$\eta_i = \begin{cases}
\eta_E \approx -0.95 & \text{if } i \in E \\
\eta_I \approx -0.47 & \text{if } i \in I
\end{cases}$$

Apply per-record in the realization-rate calculation:

$$r^S_i = r^B_{a(i), k(i)} \cdot \exp(-\eta_i (P^S - P^B))$$

The cell-level $r^S$ becomes a weighted average over the two types
within the cell.

**State.** Per-record indicator. SOI captures asset composition; we'd
need to map (e.g.) Schedule D detail to "elastic share" and persist as a
record attribute.

**Calibration.** Already done by Budget Lab (per the in-house behavior
page): $\eta_E = -0.945$, $\eta_I = -0.473$, blended weights deliver
$\bar\eta = -0.62$ on current shares.

**Hook.** Pre-aggregation step in `kg_dyn_aggregate_cell_mtr`: split cell
totals by type, run the bracket twice. Or: per-record bracket inside the
applier (uniform across types in level $r^B$ but heterogeneous in $\eta$).

**Verdict.** Worth doing on its own merits regardless of threshold
behavior. Composes naturally with all other forms.

---

### Form F — Bounded-Foresight Bellman per Cell

**Motivation.** The intellectually most satisfying option. Solve a small
finite-horizon DP per cell at scenario startup; use the optimal policy
function to drive realizations.

**Math.** State: $(g, y_-)$ per record (or per fine cell). Bellman:

$$V_t(g, y_-) = \max_{G \in [0, g]} \Big\{ (1 - \tau(y_- + G)) \cdot G \, + \, \beta(1 - m_a) \, \mathbb{E}\big[V_{t+1}((g - G) \cdot R, y_-')\big] \, + \, \beta m_a \, c \cdot \tau(y_- + g) \cdot g\Big\}$$

The solution $G^*(g, y_-, t)$ is a policy function on a 2D grid. Compute
once per scenario, look up per-record realization at run-time.

**State.** $(g, y_-)$ grid + assumptions on $y_-$ process (AR(1) or
random walk).

**Calibration.** $\beta$, $R$, mortality table, income process, $c$
already in hand. $\tau(\cdot)$ comes from the policy YAML directly. No
new free parameters; the elasticity is *implied* by the structural
solve. We'd validate by computing the implied permanent elasticity at
the calibrated baseline and checking it matches Burman-Randolph 1994.

**Hook.** Replace the bracket-and-rate machinery in
`kg_dyn_run_bathtub_pass` with a value-function iteration. Substantial.
The cell aggregation and applier downstream remain unchanged; what
changes is what we put in the cell_table.

**Verdict.** Right conceptual home for "expectations and foresight."
Generates threshold bunching, lock-in, and pre-announcement spikes from
a single optimization. Cost is real: VFI on a (gain × income × age) grid,
plus an income-process specification, plus implementation. Makes sense as
a research benchmark, not a production default. **Recommended for a
"structural KG" mode that runs alongside the production form.**

---

## 5. What about uncertain future rates?

Rational expectations over *unannounced* future policy is in the §9 gap
list and the spec deliberately punts on it. I think this is the right
call — JCT and CBO don't model it either, and there's no consensus on the
"true" rate-uncertainty process.

But there's a cheap proxy that might be worth adding alongside Form A:
**a Bayesian update on the announced path.** If a holder gives weight
$\pi$ to the announced reform actually being permanent and $1-\pi$ to it
being reversed by the next administration, the bracket integrand becomes

$$\tau_{t+j}^{\text{perceived}} = \pi \tau_{t+j}^{\text{reform}} + (1-\pi) \tau_{t+j}^{\text{baseline}}$$

with $\pi$ a single calibration constant (or, more honestly, a sensitivity
range — say 0.4 to 0.8). This dampens the model's anticipation response
and is closer to the empirical evidence on how taxpayers actually respond
to announced changes (Auerbach-Burman-Siegel 2000 documented sub-rational
response to TRA86).

Mention this as an option for sensitivity work, not a default.

---

## 6. Recommended phased plan

**Phase 0 (now): clean up the existing forward-looking machinery.**
Document and expose the announced-path hook (`tau_ratio` in
`kg_dyn_compute_bracket`). Currently it's there but unused; we should
have a runscript-level option to declare expected rate paths and feed
them through. Cheap, mostly documentation, but unblocks Phase 1.

**Phase 1: Form E (group-specific elasticity).** Decouples the elastic
and inelastic shares. Keeps the existing single-η calibration as a
fallback. Composable with everything else. Roughly two days of work.

**Phase 2: Form A (permanent/transitory split).** Implement the smoothed
expected-tau via a forward-looking weighted average. Gives us proper
permanent and transitory elasticities. Requires the calibrator to fit
both at once. Maybe a week of work including new calibration test cases.

**Phase 3: Form B + Form C (threshold bunching with intertemporal
smearing).** This is the user's headline ask. Form B alone is misleading
(static bunching without smearing overstates the response); the two need
to ship together. Per-record state is the main implementation cost —
maybe two weeks including testing on a $1M-threshold reform.

**Phase 4 (research, parallel track): Form F (bounded-foresight
Bellman).** Build alongside production as a second mode (`behavior =
kg_dynamics_structural`). Use it to validate Form A+B+C calibration —
the structural solve gives us implied elasticities and bunching widths
that we can compare to the reduced-form values. If they diverge, that's
information.

**Phase 5 (later): Form D (logit hazard).** Only if we're hitting cases
where Form B+C's hard-coded threshold logic breaks down (multiple
thresholds, smoothly progressive top rates, etc.). Form D unifies but at
the cost of disrupting the calibration.

I'd skip Form D unless we have a specific reform that demands it.

---

## 7. Open questions

1. **Income binning.** Even with Form B+C (threshold logic at the
   applier), there's a question of whether to also disaggregate cells by
   income for finer per-cell elasticity. My current take: no — the
   per-record applier is enough, and finer cells just add noise.

2. **Realization composition.** The existing model collapses asset
   classes. Form C (smearing) interacts with this: equity gains are easy
   to time, real-estate gains aren't. Worth doing a per-asset-class $\bar d$
   (zone width). This is a small extension but worth flagging.

3. **The "next year forecast" problem.** Form C requires assumptions
   about $y_{-i,t+1}$. The simplest answer (assume next year = this year)
   is fine for most taxpayers but bad for one-time-spike cases (sale of a
   business, RSU vest). For these we'd want to look at the actual
   simulator's projected income for that record, which is available
   downstream — but the kg_dynamics pre-pass doesn't have access to it.
   This may require restructuring the pre-pass to run *after* the static
   counterfactual rather than before.

4. **Calibration target.** Currently we calibrate to elasticity = −0.62.
   With Form A this becomes $(\eta_p, \eta_T) = (-0.5, -1.5)$ (or
   wherever the bisection lands). With Form C the threshold elasticity
   $e$ is a third target. Need to decide whether to anchor to Burman-
   Randolph (1994), Dowd-McClelland-Muthitacharoen (2015), or
   Agersnap-Zidar (2021). My vote: Agersnap-Zidar for the permanent
   anchor (cleanest panel design, recent), Dowd-McClelland (2019) for the
   bunching anchor (only direct U.S. cap-gains bunching evidence).

5. **Frictions / inattention.** Kleven-Waseem $a$ matters a lot. We
   probably want to allow $a$ to vary by holder type (sophisticated
   high-wealth vs. small holders). Auerbach-Burman-Siegel (2000) has
   evidence that the high-end is much more elastic; we could set $a_{\text{top}}$
   low (high attention) and $a_{\text{rest}}$ high.

6. **Welfare interpretation.** Once we have Form C, the model produces
   not just revenue but also a measure of "tax-induced behavioral cost."
   Worth thinking about how to expose this in the output for distributional
   analysis.

---

## Citations

Core literature underlying the recommendations above:

- **Burman, L. & Randolph, W. (1994)**, "Measuring Permanent Responses to
  Capital Gains Tax Changes in Panel Data," *AER* 84(4):794–809. *The*
  canonical permanent-vs-transitory split.
- **Stiglitz, J. (1983)**, "Some Aspects of the Taxation of Capital
  Gains," *JPubE*. Lock-in / arbitrage argument.
- **Constantinides, G. (1983)**, "Capital Market Equilibrium with
  Personal Tax," *Econometrica*. Optimal-stopping closed form. The
  intellectual ancestor of the existing bracket mechanism.
- **Auerbach, A. (1991)**, "Retrospective Capital Gains Taxation,"
  *AER*. Accrual-equivalent retrospective tax. Useful welfare
  benchmark.
- **Auerbach, A., Burman, L. & Siegel, J. (2000)**, "Capital Gains
  Taxation and Tax Avoidance: New Evidence from Panel Data," in
  *Slemrod, Does Atlas Shrug?*. High-end concentration of avoidance
  behavior.
- **Dowd, T., McClelland, R. & Muthitacharoen, A. (2015)**, "New
  Evidence on the Tax Elasticity of Capital Gains," *NTJ*
  68(3):511–544. Source of the −0.72 anchor.
- **Dowd, T. & McClelland, R. (2019)**, "The Bunching of Capital Gains
  Realizations," *NTJ* 72(2):323–358. Only direct U.S. cap-gains
  bunching estimate; ST/LT notch design.
- **Agersnap, O. & Zidar, O. (2021)**, "The Tax Elasticity of Capital
  Gains and Revenue-Maximizing Rates," *AER:Insights* 3(4):399–416.
  Recent panel-state estimates with local-projection design; smaller
  permanent elasticity.
- **Sarin, N., Summers, L., Zidar, O. & Zwick, E. (2022)**, "Rethinking
  How We Score Capital Gains Tax Reform," *Tax Policy & Economy* 36.
  Group-specific elasticity decomposition.
- **Hines, J. & Schaffa, D. (2023)**, "Capital Gains Realizations,"
  *NBER WP 31059*. Argues the permanent elasticity is largely an
  expectations elasticity.
- **Saez, E. (2010)**, "Do Taxpayers Bunch at Kink Points?",
  *AEJ:EP* 2(3):180–212. Canonical bunching estimator.
- **Kleven, H. & Waseem, M. (2013)**, "Using Notches to Uncover
  Optimization Frictions," *QJE* 128(2):669–723. Frictions parameter.
- **Kleven, H. (2016)**, "Bunching," *Annual Review of Economics*
  8:435–464. Survey.
- **Le Maire, D. & Schjerning, B. (2013)**, "Tax Bunching, Income
  Shifting and Self-Employment," *JPubE* 107:1–18. Intertemporal
  smearing — the closest empirical analog to the user's example.
- **Jakobsen, K., Jakobsen, K., Kleven, H. & Zucman, G. (2020)**,
  "Wealth Taxation and Wealth Accumulation: Theory and Evidence from
  Denmark," *QJE* 135(1):329–388. Structural template for intertemporal
  avoidance modeling.
