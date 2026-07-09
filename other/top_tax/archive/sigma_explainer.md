# The σ conversion channel, in plain English

*2026-07-08. Companion to `DESIGN_LOCK.md` (authoritative rulings) and
`sigma_build_plan.md` (build steps). This memo explains what the thing IS.
Nothing here overrides those documents.*

## The question σ answers

When you raise top ordinary rates, owner-managers (people with both a top-
bracket return and an active business) can respond by **restructuring how
they pay themselves**: less current salary / active pass-through income, more
equity-type upside. The dollar doesn't disappear (that's the *evasion*
module) and it doesn't move to the C-corp tax base as profit (that's the
*entity shifting* module) — it becomes an **unrealized capital gain** held by
the household, taxed later (when sold) at capital-gains rates, or possibly
never (die under step-up basis).

**Be precise about the legal channel, because pass-throughs cannot retain
taxable earnings** (K-1 income is taxed currently whether or not cash is
distributed). The real-world margins σ stands in for are: (i) closely-held
C-corp owner compensation (salary vs. retained corporate value); (ii) "sweat
equity" — underpaying yourself while your labor builds enterprise value that
is realized as gain on eventual sale, which works inside a pass-through only
to the extent the business is appreciable; (iii) profits interests / carried
interest / founder stock structuring; (iv) restructuring into C-corp form
(which overlaps entity shifting — hence the pinned module order). The model's
destination (the household unrealized-gain stock) is agnostic about the
wrapper. The pool below (wages + 75% of active PT income) is a deliberately
broad reduced-form stand-in for these channels — DESIGN_LOCK ruling 4 accepts
the over-breadth, and since Δconv = σ·ΔW·pool, pool breadth and σ trade off
one-for-one: the validation check disciplines their PRODUCT, and any later
refinement (leg-specific haircuts, an appreciable-business gate on the PT
legs) re-anchors σ rather than changing the architecture.

## The three ingredients

**1. The price of the equity path: τ_eq(age, year).**
A salary dollar is taxed this year at the record's own marginal rate — we can
read that off the calculator (`mtr_wages1`, etc.). The equity dollar's tax
price is subtler: it depends on how fast people realize gains, what year they
die, and what death regime the law has (step-up forgives, carryover defers to
heirs, deemed realization taxes at death). All of that machinery already
exists in the kg bathtub. τ_eq is defined as: *inject $1 into the unrealized-
gain stock of an (age, year) cell, let the bathtub's own dynamics carry it
forward, add up the discounted tax it ever generates.* Computed exactly by a
fast backward recursion, verified against a brute-force forward simulation
(they agree to machine precision — `tests/test_tau_eq.R`, plus an in-run
check). Intuition checks: under step-up, τ_eq FALLS with age (death is
coming, and death forgives); under deemed realization it RISES with age;
carryover sits in between.

**2. The wedge.**
For each gated record and each compensation leg (own wages, spouse wages,
partnership, S-corp, sole prop):
`W = (record's own ordinary MTR on that leg) − τ_eq(record's age)`.
The FORCING is the wedge's change under the reform: ΔW = W(reform) −
W(baseline). A +5pp top-rate hike with an unchanged CG regime widens the
wedge ~5pp; a CG/death-regime reform moves τ_eq instead and can narrow it.

**3. The response.**
Gate: top-bracket taxable income AND any active business income (~43k
records, ~$1.5T of compensation in 2026). Pool: all wages plus 75% of active
pass-through income (the labor-content share). Response: convert
`σ × ΔW × pool` dollars — **σ = 0.08 central** (env knob `SIGMA_CONV`),
i.e., 0.08% of the pool per percentage point of wedge, about $5B/yr under a
+5pp top-rate hike. No phase-in; the response is recomputed fresh each year
from that year's wedge.

**Where 0.08 comes from (2026-07-08 calibration).** σ was originally
asserted at 0.6 from total-response literature — but entity shifting and
evasion are total-response-calibrated too, so stacking all three
double-counted: the full stack produced a top ETI of 0.431 vs the
Saez–Slemrod–Giertz bracket 0.12–0.40, while the stack *without* σ already
produced 0.223. σ = 0.08 is the residual that lands the total at 0.25 (the
SSG central), confirmed by rerun. It is therefore conditional on the rest
of the stack — if the entity-shifting or evasion parameters change, σ must
be re-derived (provenance + staleness conditions in
`src/sim/sigma_conversion.R`).

## Where the dollars go (conservation)

Each year, per record: wages and pass-through legs go DOWN by the conversion
(so ordinary income tax and payroll bases shrink immediately — that's the
revenue cost). The SAME dollars are injected into the kg bathtub's gain
stock, where they realize gradually at the holder's age-specific rate (taxed
as capital gains in later years) and meet the death regime like any other
gain. Nothing is added to any record's `kg_lt` directly. A hard assert checks
every year that dollars removed from records equal dollars injected into
cells (it holds exactly in the smoke).

## How it composes with the rest of the stack

Module order is pinned and enforced:
`kg_dynamics → conversion/sigma → entity_shifting → evasion` (charity floats).
kg must precede σ (σ lives on its state); entity/evasion run after so they
respond to the post-conversion compensation base — sequencing is what
prevents double-moving the same dollar. σ requires kg_dynamics and refuses
to run without it. Static runs never see σ (it's conventional-only, like all
behavior).

## The knobs, and what is deliberately NOT a knob

- `SIGMA_CONV` (env): 0.2 / 0.6 / 0.9.
- Everything else is derived: τ_eq from the bathtub's own primitives, the
  gate threshold from the reform's own top-bracket parameter, MTRs from the
  calculator.
- There is NO dial for "how much goes to the corporate base vs the gain
  state" — that split is an OUTPUT you read from the σ tracker + entity-
  shifting diagnostics, not an assumption.

## What the smoke run showed (2026–28, +5pp top ordinary rate, full stack)

- Pool ≈ $1.46–1.59T over ~43k gated records (~0.85M weighted).
- Mean pooled wedge change ≈ +4.5pp (5pp statutory, minus a ~0 τ_eq change
  since the CG side didn't move).
- Conversion ≈ $39–43B/yr (≈ 0.6 × 0.045 × pool), ~42% from wage legs / 58%
  from pass-through legs.
- Conservation exact; τ_eq in [0.056, 0.118] across ages (vs a ~0.24 top CG
  rate — deferral and step-up do a lot of work, as they should).

## What would falsify / worry us (the informal validation check)

Two full runs (a +5pp ordinary leg and a CG mirror leg) produce a 2×2 matrix
of own/cross elasticities of (top ordinary income, gains realizations). We
eyeball it against: own-ordinary ETI 0.12–0.40 (SSG), own-gains −0.8..−0.9
(Dowd/Mortenson), ord←CG cross below Mortenson's face value, gains←ord
positive and well under +2.77. If σ = 0.6 pushes these out of range —
especially given the known double-count risk with entity shifting also
running — we iterate on σ or the pool. It is an eyeball check, not a fail
gate (DESIGN_LOCK ruling 2).
