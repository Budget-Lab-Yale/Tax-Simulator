# Two functional forms for the realization response

*Budget Lab at Yale — capital-gains microsimulation. Prepared for Danny Yagan.*

## The question

Our capital-gains model resolves how much of the realization response to a rate
change is timing versus a permanent change in the level of realizations. The
permanent margin is governed by an entropy cost of deviating from a taxpayer's
baseline realization rate, and that cost implies a **constant semi-elasticity**:
the log of realizations moves linearly with the tax wedge. Agersnap and Zidar's
preferred specification is instead a **constant net-of-tax elasticity** — the log
of realizations linear in the log of one minus the rate. Their online appendix
notes the two are empirically indistinguishable at observed rates; the choice is
an extrapolation prior, and it matters precisely for the revenue-maximizing-rate
question.

This memo adds the net-of-tax form as a selectable alternative alongside the
current one, calibrates **each independently to the same local moment** — a
full-model realization elasticity of −0.6 at current top rates — and compares the
two on three policy experiments and a capital-gains Laffer curve.

## What each form assumes away from the calibration point

Both forms pass through today's rate with the same slope, so they agree on any
small perturbation. They differ in how they extrapolate. The semi-elasticity
form treats each additional point of the rate as retiring a constant *percent of
the realizations that remain*; the net-of-tax form treats each additional point
of the *keep rate* (1 − τ) that way. Read as a distribution of the marginal,
just-deterred dollar of gains, the first is an exponential reservation
distribution — a thin tail, few dollars that survive very high rates — and the
second is a Pareto reservation distribution, with a fatter tail. The two priors
say nothing different near 23.8%; they say increasingly different things as the
rate approaches the neighborhood where revenue peaks.

## Calibration

Each form is pinned in two steps against the same targets. The long-run
parameter is set on the full simulator so the measured realization
semi-elasticity at the top rate equals −0.6/0.238 = −2.52; the short-run
timeable share is then set, given the long-run parameter, against the
announcement-year moment (1.2/0.238 = 5.04). The long-run moment is invariant to
the timeable share, so the two identify sequentially.

The levels form keeps its shipped calibration: η = 2.4825, timeable share 0.2542.
The net-of-tax form is pinned to **η̃ = 1.6625** (confirmed to reproduce the −2.52
moment within 0.03%) and the **same timeable share, 0.2542** — the announcement
moment lands at +5.26 there, inside tolerance, and the timing overlay is a
mechanical fraction rather than a behavioral response, so it is form-robust. That
η̃ is smaller than η is mechanical, not substantive: the two cost primitives map
their parameter into the measured moment on different slopes, so hitting the same
−2.52 requires a different number in each.

## Table 1 — three experiments, two forms

Ten-year conventional revenue, FY2027–2036, scenario minus baseline ($B). Static
shown alongside for reference; all three experiments take effect in 2027 on the
full behavioral stack.

| Experiment | Levels (static) | Levels (conv.) | Net-of-tax (static) | Net-of-tax (conv.) |
|---|---:|---:|---:|---:|
| +2pp on all preferred rates | 344.1 | 142.7 | 344.1 | 146.1 |
| Deemed realization at death | 290.9 | 678.2 | 290.9 | 650.3 |
| Both together | 663.0 | 915.0 | 663.0 | 898.2 |

The static columns are identical across forms by construction (behavior off) — a
check on the harness. The conventional columns differ by at most ~4%: near the
current rate, where these experiments sit, the two forms are close, exactly as a
shared local calibration implies.

## Figure 1 — capital-gains Laffer curves

Three panels, one per death-tax regime, plotting conventional revenue over the
third projection decade (FY2047–2056, as a share of GDP) against the top
preferred rate from 20% to 45%, with the two forms overlaid. Both curves cross at
20% by construction. Peaks (by dollar revenue over the grid):

| Regime | Levels peak | Net-of-tax peak | Behavior |
|---|---|---|---|
| Step-up | ≥45% (still rising) | **40%** | forms diverge above ~35%; net-of-tax turns down |
| Carryover | ≥45% | ≥45% | curves nearly coincide, both rising |
| Deemed | ≥45% | ≥45% | curves nearly coincide, both rising |

The step-up panel is where the form choice shows. At 25% the two are identical
($499B vs $500B over the decade); by 40% they have separated ($1,292B levels vs
$1,035B net-of-tax); and at 45% the levels curve is still climbing ($1,378B)
while the net-of-tax curve has already turned down ($920B). Under step-up the
pure living-realization response drives revenue, so the extrapolation prior is
fully exposed, and the net-of-tax form — with more of its mass deterrable — puts
the revenue-maximizing rate lower.

Under carryover and deemed the death-realization base dominates (revenue is three
to four times larger), and that base is far less sensitive to the living-response
form, so the two curves lie almost on top of each other and neither has peaked
inside the grid. The practical reading: the form matters for the step-up
revenue-maximizing rate and is close to immaterial once gains are taxed at death.

## Caveats

- The short-run timeable share is re-pinned per form; here it landed at the same
  value, but that is a result, not an assumption.
- The form choice propagates through the equivalent realization rate τ_eq into
  the entity-shifting response, so the net-of-tax runs shift entity-shifting
  slightly relative to the levels runs. This is correct — τ_eq should reflect the
  active response — and is disclosed rather than suppressed.
- The evaluation rate used as the divisor in the calibration (top combined
  federal rate, 23.8%) is a separate lever, and numerically a larger one than the
  choice of form; it is out of scope here and flagged for its own treatment.
- The Laffer grid tops out at 45%; under carryover and deemed the peak is beyond
  that, so those peaks are reported as lower bounds.
