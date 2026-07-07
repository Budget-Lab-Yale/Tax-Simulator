# Top-tax project, part 2: conceptualizing the revenue-frontier exercise

*Working notes from code review + discussion, 2026-07-07. Covers: what the model
currently has, what's missing, what the exercise should be, and the pitch
vs. a naive ETI calculation.*

## 1. What we actually have (the interaction machinery, as the paper would state it)

The model currently closes four cross-base loops — three mechanical, one behavioral:

1. **Wealth ↔ capital income ↔ estate (the bathtub).** Any during-life tax
   increase (income + payroll + wealth) is financed partly out of wealth
   (s = 0.1 → 0.8 by wealth rank, calibrated 2026-07-07, persistent-flow
   anchor), which erodes `value.*`, which shrinks future
   dividends/interest/gains/pass-through flows, the estate base, *and* the
   wealth-tax base itself. One channel, three of the outline's interactions.
2. **Corporate → individual.** A corporate hike cuts
   dividends/interest/rent/pt flows, marks down equity (price margin on gains,
   basis fixed), scales payouts (quantity margin), and the individual-tax
   offset materializes endogenously in conventional receipts — "raise the
   corporate rate, fewer dividends to tax." Also feeds the bathtub via
   `corp_dY_exog`.
3. **Death regime → realization elasticity (the crown jewel).** In
   `kg_dynamics`, realization is the solution to a Bellman problem whose
   continuation value depends on the death regime (step-up / carryover /
   deemed — a per-asset-class law switch in `pref.yaml`). Ending step-up
   mechanically *lowers the realization elasticity*, so the revenue-maximizing
   CG rate rises endogenously. A naive ETI calc must assume that; we derive it.
4. **Entity shifting (behavioral, already built).**
   `entity_shifting/pearce_prisinzano.R` moves business income between the
   corporate and individual bases as the corp-vs-pass-through differential
   moves (semi-elasticity ≈ 0.63). This is the 1970s-sheltering margin from
   the historical section, live in the model: raising top ordinary rates with
   corp fixed triggers PT→C shifting.

Also on hand: charity elasticities (−0.5/−1.0 tax-price), deemed-death
avoidance haircut (0.25), NIIT `include_active` switch, `tax_at_ord` switch,
QBI/estate/wealth yaml levers all parameterized.

## 2. What's missing, ranked by threat to the headline

1. **No top-end ordinary-income ETI.** Biggest gap. A top-rate hike currently
   scores at mechanical + entity-shifting only, so any revenue-max search
   slams ordinary rates to the grid ceiling. Framing opportunity: the classic
   ETI is a *bundle* of margins we already model separately (realization
   deferral, entity shifting, charitable deductions). Build a **residual
   ETI**: take a defensible top-end total (SSG range, ~0.2–0.4), subtract the
   model-endogenous components to avoid double-counting, implement the
   remainder as a semi-elasticity on top-bracket income. The decomposition is
   itself a contribution — naive ETI applications at the top either
   double-count shifting or miss it.
2. **Wealth-tax avoidance uncalibrated and deliberately extreme** (−7/−17
   semi-elasticities ⇒ ≈19%/40% base erosion at a 3% rate; the module header
   itself says "must be re-justified before publishing"). The *design* is
   right — reported-base-only avoidance, cleanly separated from the real
   response (which is the bathtub) — but the parameters need anchoring to the
   Scandinavian/Swiss/Colombian estimates, which bracket a huge range ⇒ this
   becomes a frontier band, not a point.
3. **Estate tax has zero behavioral response.** Purely mechanical; given the
   report's "very leaky" framing, a mechanical estate score overstates. A
   Kopczuk–Slemrod-style reported-estate elasticity (mirroring the
   wealth-avoidance design: reported base only, `value.*` untouched) is a
   contained build. Related: `f_ded` is frozen measurement, but charitable
   bequests should respond to the estate rate — fold into the same elasticity.
4. **Mark-to-market doesn't exist.** We have deemed-at-death, not annual
   accrual. Either build a crude MTM regime in kg_dynamics (force the
   realization rate, kill the deferral option value — the machinery is
   well-suited) or demote MTM to the off-model sidebar with trusts /
   insurance / carried interest. Lean: sidebar for v1 — the death-regime lever
   already delivers the conceptual point about deferral — but it's a headline
   policy, so author's call.
5. **Corporate lever has an upstream dependency**: the channel maps a *given*
   OME corporate wedge onto records. Searching over corporate rates needs a
   τ_c → gross-receipts curve from Off-Model-Estimates (family of vintages or
   a parameterized curve). Plus six `CORP_*` constants are still Phase-0c
   placeholders (asset exposures, ω_div, ω_kg, κ, θ).
6. **Migration/expatriation** — nowhere, and it's the wealth-tax critics'
   first objection. Off-model haircut band, not a module.
7. **Housekeeping:** `capital_income/wealth_tax_interaction.R` (the old bridge
   to the standalone Wealth-Tax-Simulator; hardcoded scenario id + NFS path)
   is now a double-count hazard against the on-model machinery — retire it
   before this exercise. *(DONE 2026-07-07: module, its stale
   `private/wealth_tax/nickel_dime.csv` runscript, and the dead
   `Wealth-Tax-Simulator` interface entry all removed.)*

## 3. What the exercise should be

**Don't make the deliverable "the revenue-maximizing point."** An
unconstrained Laffer search returns "max out every lever except CG rate" —
fragile and boring. Three-layer structure:

- **Layer 1 (greenbook): standalone revenue curves.** Each of ~8 levers at
  3–4 intensities: top ordinary rate (+ millionaire bracket), CG/dividend rate
  (+ `tax_at_ord`), death regime, QBI repeal, NIIT expansion
  (`include_active = 1`), corporate rate, estate (exemption × rate), wealth
  tax (rate × threshold). ~30 runs. Reusable in the broader greenbook.
- **Layer 2 (the central exhibit): the interaction matrix.** All pairwise
  combinations at central intensities (~28 runs): stacked revenue vs. sum of
  standalones, in $ and %. "A corporate hike lowers the yield of a CG hike by
  X%; ending step-up raises it by Y%." Publishable, robust, and where the
  model has no competition.
- **Layer 3 (the search): surrogate, not grid.** Fit a quadratic response
  surface — main-effect curves from Layer 1, interaction terms from Layer 2 —
  optimize *that* analytically under constraints, validate the argmax with a
  handful of full runs, iterate once. Total ≈ 70–100 pipeline runs; at
  observed ~tens-of-minutes wall-clock per scenario on SLURM, that's days.
  Spot-check a few triples (death-regime × CG × wealth is the likeliest
  three-way) to confirm the pairwise surface suffices.

**Frontier axes.** Revenue-max needs a counterweight to be a frontier. Two we
get nearly free: (a) **leakage share** = 1 − conventional/static per package —
already an output; operationalizes "minimizing leakages." (b) Genuinely novel:
the bathtub state is on disk, so plot **revenue against cumulative top-1%
private wealth erosion** — how much of each marginal revenue dollar is
financed by decumulation vs. consumption. No ETI calculation can produce that
axis. As a constraint rather than an axis: hold the burden below P99 ≈ 0
using the distribution tables ("greenbook for the rich" discipline).

**Two conventions to fix upfront:**
1. The objective is *total package revenue vs. common baseline*, not stacked
   attribution — with the endogenous corporate offset, stacked rows aren't
   order-invariant (§8.13). Attribution order is a presentation convention.
2. Horizon matters: bathtub erosion compounds, so the year-10 revenue-max mix
   ≠ the year-30 mix. Report the 10-year window *and* the terminal-year
   annual rate; the divergence is itself a finding ("wealth taxes look better
   in the window than in the steady state").

**The frontier is a fan, not a line.** Every point is conditional on ψ, the
wealth-avoidance elasticities, the residual ETI, s, and the corp corners. Env-var
sweep machinery exists for several already; publish a band over a
low/central/high elasticity bundle.

## 4. The pitch vs. naive ETI (one paragraph)

An ETI calculation prices each instrument with a fixed, policy-invariant
elasticity and adds the results. That fails at the top for two reasons we fix:
the **bases overlap** — the same dollar of wealth is capital income during
life, an estate at death, and wealth-tax base in between, so hitting it
anywhere shrinks the other bases, tracked here on actual record-level balance
sheets rather than through an assumed elasticity — and the **elasticities are
policy-dependent** — the realization elasticity is an equilibrium object that
collapses when step-up ends, the shifting elasticity depends on the
corp-individual differential, and the financing response depends on the whole
tax vector. The Lucas critique bites hardest exactly where this report lives:
large reforms far outside the variation that identified the reduced-form
elasticities.

## 5. The ETI program (decided 2026-07-07, discussion with author)

Replaces the "residual ETI module" idea from §2.1. The top-end ETI is
decomposed and each piece gets its own treatment:

- **Real responses (labor supply etc.): EXCLUDED**, by conventional-scoring
  convention (macro aggregates fixed — same reason JCT excludes them). An
  assumed value (~0.05, banded) enters only the residualization arithmetic
  below, never the model. Note the error direction: assuming real too small
  inflates σ, which *understates* ordinary-rate revenue in open-exit
  configurations — the conservative direction.
- **Modeled legal avoidance**: realization (kg_dynamics), entity shifting
  (Pearce–Prisinzano), charity — already in the model. Measured via a
  top-rate perturbation run, with strict income-concept discipline: kg
  realization is EXCLUDED from the tally (the ETI literature's income concept
  excludes gains), the bathtub too (wrong horizon).
- **Evasion: its own module, a LEAK** (income leaves the system) —
  `config/scenarios/behavior/evasion/debacker.R` (`do_evasion()`), built
  2026-07-07. DeBacker–Heim–Yuskavage (NTA 2025, NRP 2006–2017) net-of-tax
  elasticities on reported income, gated by information-reporting visibility:
  Schedule C/F 0.046, partnership/S-corp 0.052, rent 0.04, wages/interest/
  dividends 0. Positive income legs only; SECA earner-split companions
  co-scale. Env knobs: `EVASION_E_SCHC/_PT/_RENT`, `EVASION_TOPEND_MULT`
  (NRP underdetects top-end evasion per Guyton et al. — detected-based values
  are a floor; sweep 1.5–2). Requires `mtr_vars = "sole_prop1 part_active
  rent"`. Smoke runscript: `config/runscripts/tests/evasion_smoke.csv`.
- **The remainder — cross-base shifting σ — is a CONSERVATION parameter, not
  a leak**: a wedge-driven semi-elasticity moving shiftable labor comp into
  the kg gain state (the founder-equity path), phased in as a flow, gated to
  owner-managers. Destination taxation is then endogenous to the death
  regime / CG rates, so the ordinary-rate response shrinks when a package
  closes the capital-side exits — operationalizing Kopczuk (2005): the
  elasticity is a function of base breadth. NOT YET BUILT.
- **σ calibration: triangulated, not just residualized.** Three anchors:
  (1) residualization — σ ≈ top ETI anchor (~0.25, itself banded 0.2–0.6)
  − real (~0.05) − evasion (DHY, ~0.05 top-weighted) − modeled (~0.05 from
  the perturbation run) ⇒ ~0.10 in ETI units, converted to wedge units by
  the same perturbation; (2) the Pearce–Prisinzano boundary-crossing prior
  (~0.63 semi-elasticity); (3) TRA86 shifting magnitudes (Gordon–Slemrod,
  Gordon–MacKie-Mason). Agreement across anchors is the validation story;
  disagreement is a finding to investigate. Key property: calibrated under
  current law (open exits), it REPRODUCES literature ETIs where they were
  estimated, and endogenously shrinks under closed-exit packages.
- **Disjointness constraint**: σ's flow (wages → gain state), entity
  shifting's flow (business income across the C/PT boundary), and evasion's
  leak (reported pass-through income) must not touch the same dollars —
  assert in code when σ is built.
- **Escape-route framing for the report**: the dominant labor-comp escape
  routes (founder equity, corporate retention, carry) move income INTO
  modeled bases rather than out of the system; the modern top W-2 base is
  the selection residue of past escape (RSUs are ordinary-at-vest). Proposed
  exhibit: the "conversion wedge" (top ordinary rate − accrual-equivalent
  effective rate on the equity path, from the kg Bellman) across packages.

## 6. Build order and open decisions

Build order: (1) residual-ETI module + estate elasticity + wealth-avoidance
calibration — the three parameter gaps that would otherwise dominate referee
reports; (2) retire the legacy capital-income bridge module; (3) sort the OME
corporate-rate curve dependency; (4) then the Layer 1–3 run campaign is mostly
orchestration we already know how to do.

Open decisions for the author:
- Constrained-frontier vs. pure-max framing (recommend: frontier, with the
  unconstrained max as one labeled point).
- MTM on-model (kg_dynamics forced-realization regime) vs. off-model sidebar.
- Whether the corporate lever makes v1 continuous or gets fixed at a few
  discrete OME-scored points.
- Which horizon headlines the report (10-yr window vs. terminal-year annual).
