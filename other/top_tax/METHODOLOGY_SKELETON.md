# Methodology skeleton-of-argument (v2, ground-up)

*Working title candidates:*
1. *"Taxing the top: model methodology"*
2. *"How much is really there? Methods behind the top-tax estimates"*
3. *"Behavioral and mechanical interactions in the top-tax model"*

*Purpose of this file: every section below is stated as a **claim** (the sentence
the section exists to establish), followed by what it argues, the decisions it
weaves in (ledger IDs), and what goes to the appendix. Mark up freely — strike
claims, reorder, demote/promote. Nothing is drafted yet; this is the contract
for the draft.*

*Audience contract: Danny (10 minutes) reads page 0 + the ledger + section
headline boxes. Leiserson/Patel (an hour) read Part I. Anyone challenging a
specific choice follows its ledger ID into Part II.*

---

## Page 0 — How to read this document

Three reading paths, stated explicitly on the first page:

- **10 minutes:** the thesis paragraph, the decision ledger (§0), and the
  boxed "choices made here" bullets that open each Part I section.
- **One hour:** Part I straight through (~18–22 pp target, math-light,
  mechanism-in-words).
- **Deep dive:** Part II appendix entries, keyed one-to-one to ledger IDs,
  carrying derivations, calibration procedures, and provenance.

**The thesis paragraph (draft claim):** *Progressive proposals reach the same
small group of taxpayers through several statutes at once. Scoring each alone
and summing overstates the combined capacity, because the bases overlap
mechanically and because behavior re-routes income between them. This model
closes both loops on record-level balance sheets: the channels interact not
through assumed cross-elasticities but through shared model state, and the
parameters are calibrated to the stack as a whole rather than channel by
channel. This document records every methodological choice that makes that
claim honest, and what each choice costs.*

---

## §0 — The decision ledger

One table, ~30 rows, grouped by family. Columns: **ID · Decision · Value ·
Anchor/evidence · Alternative considered · Status · Appendix ref.**
"Status" vocabulary is fixed: `pinned` / `residual` / `placeholder (disclosed)`
/ `accepted-with-disclosure` / `excluded-by-convention` / `re-pin pending`.

Draft rows (values to be re-verified against code constants at draft time —
the in-code provenance stamps, not this document, are the source of truth):

**Architecture**
| ID | Decision | Value / form | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| A1 | Static leg stays law-only; distribution tables static-sourced; interactions read from receipts | — | house convention | behavior-inclusive distribution | pinned |
| A2 | Pinned channel order: corp incidence → wealth haircut → realization → conversion → entity → evasion → avoidance → tax calc; asserted at runtime | — | no-dollar-moves-twice discipline | unordered/simultaneous | pinned |
| A3 | Reporting-vs-real firewall: reporting responses touch only cash tax paid; real responses flow into every base | — | reporting_vs_real audit | — | pinned |
| A4 | Interactions carried by shared state (τ_eq, F, h, hidden ledger, bathtub forcing), not hand-set cross-elasticities | — | — | assumed cross-elasticity matrix | pinned |
| A5 | The stack is the unit of calibration: σ residual, η full-sim inversion, staleness watch, batching rule | — | — | channel-by-channel calibration | pinned |
| A6 | Mechanical channels contribute reform deltas only (estate = CBO level + Δ; corporate = OME wedge mapped on-model) | — | CBO level disagreements | on-model levels | pinned |
| A7 | Surprise (unexpected) framing, effective 2027; FY death-year+1 booking; sim runs one year past window | — | Clausing convention | anticipated reform | pinned |
| A8 | Excluded by scoring convention: real labor supply, real saving elasticity, explicit migration, macro feedback, enforcement | — | — | — | excluded-by-convention |

**Realization & timing at death**
| ID | Decision | Value | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| R1 | Single-pool entropy Bellman; η *is* the long-run realization semi-elasticity; aggregate revmax ≈ 1/η | spec v3 | exponential reservation-benefit micro-foundation | v2 responsive/inert split | pinned |
| R2 | η pinned by full-simulator inversion | 2.4825 (target E = −0.6/0.238 = −2.52, slope 1.0155) | DMM / JCT–CBO tradition | miniature calibrator + dilution bridge (retired) | **re-pin pending** (+3.7% drift from estate term → ≈2.4901) |
| R3 | Short-run retiming as additive overlay, long-run invariant | timeable share 0.2542, ±1 yr, ref_wedge 0.05 | full-sim announcement moment | in-Bellman timing | pinned |
| R4 | Deemed-realization valuation haircut | 0.25 | JCT levels; estate ρ_pt consistency | none / per-asset | pinned (data calibration, not law) |
| R5 | Mechanical-vs-behavioral revenue allocation blend in the applier | 0.5 | bounded by R ($55.9B) and G ($40.4B) allocations | pure R or pure G | accepted-with-disclosure |
| R6 | Discounting in the Bellman | Fisher-deflated tsy_10y (real, year-varying) | inflation cancels in hold/realize | nominal / fixed β | pinned |
| R7 | Single asset bucket (5 wealth classes collapsed) | — | — | per-class Bellman | roadmap |
| R8 | Wealth-carry term h in MC and τ_eq | h = record-level E[τ_w·τ_cg], survivor branch only | deferral arithmetic (no free parameter) | product of means; two-state exposed split | accepted-with-disclosure (age-cell smear understates −8.5% to −33%, conservative) |
| R9 | Estate death-value offset in F | F = (1−c_φ,eff)·τ·(1−e), leg-paired e; c_φ,eff peels charitable bequests | §2053-style deductibility | F estate-blind (pre-build) | pinned |

**ETI / base-shifting**
| ID | Decision | Value | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| E1 | σ conversion calibrated as the ETI residual | σ = 0.16 → top-ordinary ETI 0.2508 | ETI target 0.25 (SSG central) | σ as free structural parameter | residual |
| E2 | σ pool & gate | wages + 0.75·active PT; top-bracket gate | pool×σ disciplined jointly | narrow founder-only pool | pinned |
| E3 | Entity-shifting semi-elasticity & repriced retention leg | e = 0.3788/0.6 (P&P); τ_dist = 0.45·mtr_kg + 0.55·τ_eq | Pearce–Prisinzano 2018 | legacy β = 0.25 stub (retired) | pinned |
| E4 | Evasion: visibility-gated net-of-tax response, positive legs | Sch C/F .046, PT .052, rent .040; wages/int/div zero; top mult 1.0 | DeBacker–Heim–Yuskavage | top-graded multiplier (deferred; sweep 1.5–2.0) | pinned |
| E5 | Charity: cash tax-price elasticity only | −0.5 central (−1 variant) | standard lit | appreciated-asset margin | partial (gap disclosed) |

**Wealth & estate reporting**
| ID | Decision | Value | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| W1 | Wealth-avoidance semi-elasticities | marketable −7, closely-held −17 | Wealth-Tax-Simulator seed; author-accepted | lit-band sweep | pinned (migration subsumed → wealth card is a ceiling) |
| W2 | Hidden ledger: concealment consistent across wealth/income/estate bases | χ_pub = 1.0, χ_priv = 0.5; evasion→wealth link | can't-lowball-an-exchange-price logic | base-by-base independent avoidance | pinned |
| W3 | Estate own-rate reported-estate response | ε = 0.16, net-of-tax power form, keyed to Δprice | Kopczuk–Slemrod 2001 (band .10–.22) | none (pre-build) | pinned (applied to reported gross, mild overstatement near low exemptions) |

**Mechanical channels**
| ID | Decision | Value | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| B1 | Bathtub saving-financing share s(age, wealth pctile) | 0.10 → 0.80 by percentile; effective dollar-weighted ≈0.6–0.8 at the top | persistent-flow anchor, ε≈0.7 (Straub); DFJ/DSZ/MSS/Fagereng tilts | flat s | pinned |
| B2 | Within-age mobility matrix M | identity | bounding sweep: M ≤ ±$2B/10y | uniform / estimated | pinned (second-order, shown) |
| B3 | Bathtub forcing & kernel | F = ΔT⁰ − ΔY_exog; G = (1+r_total) − s(τy + τ_w); conventional-only | — | behavioral saving elasticity | pinned (financing, not saving — by design) |
| B4 | Corporate incidence constants | σ_N .375, κ .40, ω_div .85, ω_kg .50, δ .057, ERP .05, exposure vector | OTA/TPC, Fed Z.1, NIPA, Rosenthal–Austin | measured Phase-1 values | **placeholder (disclosed)**; corporate row not stacking-order-invariant |
| B5 | Estate valuation bridge frozen; never reform-overridden | r = .951, ρ_pt = .612, γ = .0914; cluster cap 300 | SOI 2022 death-year | reform-adjustable measurement | pinned (law/measurement separation) |
| B6 | Heir allocation by cumulative-dollar rank match; inheritance gross of estate tax | — | ties to totals identity | proportional smear (kept for deemed, which has no threshold) | pinned |

**Measurement (income-ETR side + surrogate)**
| ID | Decision | Value | Anchor | Alternative | Status |
|---|---|---|---|---|---|
| M1 | Haig-Simons denominator: realized gains swapped for balance-sheet accruals; DC double-count removed; denominator & ranking frozen at baseline in both legs | — | — | behavior-moving denominator | pinned |
| M2 | Welfare vs collections = numerator-only swap: static liability ETR (envelope) vs conventional realized ETR | — | envelope theorem | re-sourcing microdata from conventional detail | pinned |
| M3 | Tier structure: nested federal tiers + standalone state/local/excise tier | — | DINA-style other-taxes rate | — | pinned |
| M4 | Corporate ETR shown as a three-convention band (equity-supernormal / capital-income / uniform-net-worth) | — | placeholder exposure status | single convention | accepted-with-disclosure |
| M5 | Interactive served by an f/g/I/T surrogate (solo curves + pairwise + triple interactions), exact at anchors | holdout: conventional ±2.6/±3.4/±4.4% by decade | blind quiz validation | serving raw runs only | pinned (worst corner wealth×estate, disclosed) |

---

# Part I — The argument

*(Every section opens with a boxed 2–5 bullet "Decisions made here: [IDs]"
recap. Target lengths in parentheses.)*

## §1 — The question and the shape of the answer (~2 pp)

**Claim:** *Because the proposals under study tax the same people and largely
the same dollars, the policy-relevant object is not any lever's own score but
the survival and destination of the combined ask — so the model's outputs are
conventional-as-a-share-of-static, a by-base destination ledger, and
ask-vs-collected effective rates, rather than a headline elasticity.*

- Sets up the two overstatement mechanisms (mechanical overlap, behavioral
  re-routing) in plain terms, with one worked miniature example (a dollar of
  founder equity under CG + estate + wealth levers).
- States what the reader will be able to trace by the end: every number in the
  atlas exhibits back to a ledger row.
- Decisions woven: A1 (what static and conventional each mean), A7 (framing
  and window), A8 (the exclusions, stated up front, not buried).

## §2 — One dollar at the top: the overlap map (~2–3 pp)

**Claim:** *On record-level balance sheets, base overlap is an accounting fact,
not an assumption: the model holds, for each tax unit, the asset portfolio,
the unrealized-gain stock, and the income flows those assets throw off, so a
dollar can be located in every base that claims it.*

- Follow one dollar: earned (ordinary base) → converted (unrealized-gain
  state) → held (wealth base, carrying a deferred CG liability) → realized
  (CG base) or died (estate base, heir basis treatment) → and upstream of all
  of it, corporate profit before it ever reaches the household.
- Introduces the data substrate exactly as deeply as the argument needs:
  per-record balance sheets (14 asset classes, accrual columns), the
  Forbes-anchored top tail, the age-80 topcode — one honest paragraph on
  donor-clone clustering and the cluster cap (B5), full detail to Appendix D.
- This section is the map the rest of the paper walks.

## §3 — Static, conventional, and the accounting layer (~2 pp)

**Claim:** *Every scenario runs twice — a law-only static pass that is the
welfare/first-order object, and a conventional pass with all channels on — and
because every reported quantity is a delta against one common baseline,
"where did the money end up" is an accounting identity across revenue heads,
not a modeling convention.*

- Static vs conventional, precisely; why distribution stays static-sourced
  (A1); why mechanical channels are delta-only (A6); FY booking and the
  run-one-year-past rule (A7).
- The destination ledger as identity: receipts by head (IIT/payroll, corporate,
  estate, wealth, deemed re-attributed to the death tier), summing exactly.
  Kept brief — this is the accounting frame, not an exhibit walkthrough.
- The two-channel-type distinction (behavior modules vs mechanical
  conventional-side appliers) and the pinned run order (A2), stated here as
  part of the specification, with the no-double-counting rationale.

## §4 — The shared-state architecture (~3 pp; the load-bearing section)

**Claim:** *The channels interact because they price off the same internal
objects — a change to one lever propagates into every other through model
state, which is what distinguishes this from a system of assumed
cross-elasticities.*

Four shared objects, each introduced with the interaction it carries:

1. **τ_eq(age, t)** — PV tax per dollar entering the unrealized-gain state;
   produced by the realization Bellman, consumed by σ conversion and the
   entity-shifting retention leg (E3). *CG-rate and death-regime levers
   therefore reprice the conversion and retention shelters endogenously.*
2. **F — the death-forgiveness value** = (1−c_φ,eff)·τ·(1−e): carries the
   death regime, charitable bequests, and estate exposure (R9). *The estate
   rate and step-up repeal move realizations through the same object.*
3. **h — the wealth-carry** = E[τ_w·τ_cg] (R8): a wealth tax makes deferral
   costlier every year the gain is held. *The wealth lever unlocks
   realizations and cheapens σ conversion.*
4. **The hidden ledger + firewall** (W2, A3): one concealment decision,
   consistent across the wealth, income, and estate bases; cash is reported,
   balance sheets are real.

Plus the **bathtub forcing** F = ΔT⁰ − ΔY_exog (B3) as the fifth coupling:
every during-life tax (and the corporate shock) drains future estate and
capital-income bases.

- Ends with the paper's honesty rule: where a coupling is *missing* (the
  Bellman prices exactly one tax rate plus these terms; the corporate layer is
  absent from τ_eq), the error shows up as missing interaction, not noise —
  forward-pointer to §8.

## §5 — The channels (~6–8 pp total; each channel ≤1 pp)

**Claim:** *Given the shared state, each channel is a short story: one margin,
one functional form, one calibration anchor, one disclosed limitation.*

Organized in the five families, but written as prose essays, not template
boxes (the template lives in Appendix A). Each ends with its ledger IDs.

- **5.1 Realization (the entropy Bellman).** Single pool, closed-form
  response r_D = r_B·exp(−η ΔMC), η is the long-run semi-elasticity, revmax
  ≈ 1/η; the v2→v3 collapse told as a decision narrative (why the inert floor
  was redundant and what it was doing to the revmax curve). Timing overlay as
  a separate, long-run-invariant margin. [R1–R7]
- **5.2 Wealth-carry and the estate death-value offset** — the two "the
  Bellman now sees the other levers" builds, told together as the Tier-1
  program; what each unlocked (63/63 cells responding to a pure estate
  reform; +$127B on a 3% wealth dial). [R8, R9]
- **5.3 Income conversion (σ).** The founder-equity path; wedge = own MTR −
  τ_eq; the residual logic previewed, full treatment in §6. [E1, E2]
- **5.4 Entity shifting.** P&P margin; the τ_eq repricing that retired the
  β stub (and thereby made the C-corp retention shelter collapse under
  deemed-at-death); SECA companion consistency. [E3]
- **5.5 Evasion.** Visibility gating as the identifying idea; what
  information reporting shuts down; the flat top-end multiplier as the
  disclosed weak joint. [E4]
- **5.6 Wealth avoidance, the hidden ledger, and the estate own-rate
  response.** Reported-base responses under the firewall; concealment vs
  valuation gaming; KS ε on the change in the estate price only (level
  avoidance already lives in the frozen valuation bridge — the law/measurement
  separation doing work). [W1–W3, B5]
- **5.7 The wealth bathtub.** Financing, not saving — a share s of the new
  tax bill is paid out of wealth and compounds into lost future estate/CG/
  wealth base; the calibrated s-profile in one paragraph + one small figure.
  [B1–B3]
- **5.8 Corporate incidence.** The wedge → flows/markdown/offset mapping in
  words; the placeholder status stated plainly, with what is and isn't
  sensitive to it; the stacking-order caveat. [B4]
- **5.9 First-order-labor margins and what is deliberately excluded.**
  Charity (and its missing appreciated-asset margin), employment,
  migration-as-ceiling. [E5, A8]

## §6 — Calibration as a system (~2 pp)

**Claim:** *Because the channels share state, the stack — not the channel — is
the unit of calibration: σ is a residual conditional on everything else, η is
pinned by inverting the full simulator's measured elasticity, and an automated
staleness watch keeps "conditional on the stack" true over time rather than
true once.*

- The σ residual chain: target ETI 0.25 → σ* formula → why σ doubled when
  entity shifting was repriced and evasion fixed (a feature of the residual
  design, presented as such). In-bundle vs out-of-bundle margins and the
  batching rule (re-derive σ once per batch). [E1, A5]
- The η full-sim pin: the E_full(η) line, the −0.6/0.238 target, why the
  measurement is now arithmetic to repeat; the pending +3.7% re-pin flagged
  honestly. [R2]
- Provenance guards and the calibration-reference watch as methodology, not
  plumbing: this is what makes the parameter table in Appendix B a living
  contract with the code.

## §7 — Measurement: effective tax rates, and a note on the interactive (~2 pp)

**Claim:** *The ask-vs-collected exhibit is one ETR measured twice: a
Haig-Simons income denominator frozen at baseline in both legs, with the
static-liability numerator giving the first-order welfare burden (envelope
theorem) and the conventional numerator giving realized collections — so the
gap between them is, to first order, the cost of the avoidance response.*

- The HS denominator construction (accruals swap, retirement double-count
  removal) and why the denominator must not move with behavior. [M1, M2]
- Tiers in one paragraph; the corporate three-convention band as the honest
  rendering of placeholder exposures. [M3, M4]
- **Box: how the interactive serves 200 runs as a continuous surface.** The
  f/g/I/T surrogate in ~150 words: solo anchor curves, pairwise and triple
  interaction terms, exact at fitted points, blind-holdout error bounds
  (±2.6–4.4% conventional by decade; worst corner wealth×estate, disclosed
  in-app). [M5]
- Evasion-in-the-welfare-ETR caveat (the wedge is pure DWL only for costly
  channels; the evasion slice is a transfer) — one honest paragraph.

## §8 — What the model does not do (~1.5 pp)

**Claim:** *The known gaps are enumerable, signed, and mostly conservative;
where a gap is in the ETI bundle it carries a re-calibration cost, and where
it is out-of-bundle it does not — so the register below is also a build
sequencer.*

The limitations register (table): each gap with location, direction, size
where measured, and status. Inherits the candor of the margins-gap
assessment; includes at minimum — corporate placeholders (B4); wealth-carry
smear (R8, conservative); single asset bucket (R7); appreciated-asset charity
(E5); corporate layer missing from τ_eq; flat evasion top-multiplier (E4);
migration-as-ceiling (A8/W1); η re-pin pending (R2); estate KS on reported
gross (W3).

## §9 — References

Full citations: Dowd–McClelland–Muthitacharoen; Saez–Slemrod–Giertz;
Kopczuk–Slemrod (2001); DeBacker–Heim–Yuskavage; Pearce–Prisinzano (2018);
Straub (2019); De Nardi–French–Jones; Mian–Straub–Sufi; Fagereng et al.;
Bastian (2023); Guyton et al.; JCT/CBO realization conventions; Rosenthal–
Austin; OTA/TPC supernormal-share estimates.

---

# Part II — Appendix (keyed to ledger IDs)

- **A. Channel deep dives** under the seven-header template (salvaged from the
  prior draft), one entry per channel, at referee depth. A.1 realization
  (derivation largely salvageable), A.2 τ_eq recursion + verification, A.3
  wealth-carry & estate-offset math, A.4 σ (pool/gate/wedge + residual
  algebra), A.5 entity shifting, A.6 evasion, A.7 wealth avoidance + hidden
  ledger mechanics (χ, R-rules, firewall), A.8 estate own-rate, A.9 bathtub
  (kernel, cells, applier, WEALTH_CAP_FLOWS), A.10 corporate incidence
  (wedge split, markdown recursion, offset, D/P rulings distilled), A.11
  charity/employment.
- **B. Parameter & provenance master table** — superset of the ledger, with
  reference moments, derivation dates, and code provenance stamps; doubles as
  the staleness-watch contract.
- **C. Mechanical plumbing detail** — estate valuation bridge + heir
  allocator; receipts/FY conventions; corporate ETR conventions.
- **D. Data substrate** — Tax-Data balance sheets and accruals, the Forbes
  splice, donor-clone clustering and the cluster cap, topcodes.
- **E. Calibration procedures** — the η inversion (grid, fit, target), σ
  re-derivation recipe, timeable-share root-find, staleness-watch spec.

---

## Open questions for you before drafting

1. **η re-pin (R2):** draft the doc at 2.4825 with the ≈2.4901 re-pin flagged
   as pending — or do the re-pin first so the doc ships internally consistent?
2. **Corporate posture (B4):** present corporate incidence as a full channel
   with placeholder constants (current plan), or fence it into a clearly
   provisional subsection so reviewers don't spend their bandwidth
   challenging Phase-0c numbers?
3. **Atlas numbers in the doc:** should Part I quote headline results
   (70→66→61% survival, 23.7/48.9/37.3 ETRs) to motivate sections, or stay
   results-free (pure methods) so it doesn't stale against re-runs?
4. **The σ story:** the 0.08→0.16 doubling is a great teaching example of the
   residual design but could read as instability to a cold reviewer. Tell it
   in full (§6, my preference), or summarize and put the history in App. E?
5. **Ledger granularity:** ~30 rows as sketched, or coarser (~15, one per
   channel) with sub-decisions only in the appendix?
