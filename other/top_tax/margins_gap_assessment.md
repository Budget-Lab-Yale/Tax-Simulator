# Missing behavioral / interaction margins — gap assessment for the top-tax atlas

*2026-07-11. Code audit on branch `wealth` (four parallel read-only sweeps over
kg_dynamics, evasion, entity_shifting, charity + sigma_conversion, cross-checked
against VISION.md v3, the 2026-07-07 frontier notes §2, the reporting-vs-real
audit, and the hidden-ledger design note). Companion to `VISION.md` §3/§6.*

## 0. Framing

The question is not "what margins exist in the literature" but "which missing
margins would change a number the atlas actually prints" — the four lever cards,
their featured conditioning panels (§2.2), and the parts-vs-whole exhibit
(§2.7). Two audit facts organize everything below:

1. **The kg Bellman sees exactly one tax rate.** The realization tradeoff, the
   death-forgiveness value F, and τ_eq are all priced off the cell-aggregate
   `mtr_kg_lt` alone (`kg_dynamics.R:850–940, 1393–1433`). No wealth-tax term,
   no estate-tax term, no corporate layer. NIIT rides along implicitly inside
   the MTR. So **three of the four levers (wealth, corporate) and two of the
   seven switches (estate, deemed-interacting-with-estate) cannot move
   realization behavior or the σ conversion price at all** — the exact
   cross-lever content the atlas exists to measure.
2. **σ is a residual** (0.08, conditional on the stack). Any new margin that
   lives inside the ordinary-rate ETI bundle (charity, evasion refinements)
   forces a σ re-residualization; margins *outside* the bundle (wealth/estate
   terms in the Bellman, estate own-rate, the entity-shifting β link) do not.
   This is a real tiebreaker for sequencing: the out-of-bundle margins are
   both higher-value for the atlas and cheaper to integrate.

Every new margin must also declare itself **REAL or REPORTING** under the
reporting-vs-real firewall (`reporting_vs_real_audit.md`): reporting margins
touch only tax-computation inputs / isolation points; real margins may touch
`value.*` and must be ordered before the reporting modules.

## 1. Tier 1 — do before (or with) the 128-run factorial campaign

These three directly misprice featured conditioning panels in §2.2.

### 1.1 Wealth tax in the kg Bellman holding cost and τ_eq

**The gap.** A wealth tax changes the realize-vs-hold margin through the
*deferred-liability channel*: holding $1 of unrealized gain defers τ dollars of
CG tax, and that unpaid tax remains in the wealth-tax base, costing τ_w·τ per
year of continued deferral. None of this enters the Bellman: the wealth card's
conditioning slice "wealth × top-CG" currently shows **zero** realization
response to the wealth tax.

**Magnitude — first order, not a refinement.** The per-year benefit of deferral
is ≈ r_real·τ (the real return on the deferred tax). With r_real ≈ 2% (the
Bellman's Fisher-deflated tsy_10y) and τ_w = 1%, the wealth-tax cost of deferral
is τ_w·τ, i.e. **the 1% wealth tax erodes roughly half the deferral advantage**
(0.01·τ vs 0.02·τ). The Bellman would deliver a material unlock effect — more
realizations, more CG revenue in every wealth-on cell — plus a mirrored
reduction in σ conversion (deferral is worth less, so the founder-equity path
prices worse). Both currently missing; both bias the wealth-conditioning panels
in the *same* direction (understating cross-lever revenue).

**Build.** Contained. Add a per-cell marginal wealth rate (aggregate
`mtr_net_worth` the way `mtr_kg_lt` is aggregated — the column already exists on
every record) and subtract s·τ_w·τ-style terms in the marginal-cost expression
(`kg_dynamics.R:906`) and the τ_eq recursion (`:1409–1414`). Gate to cells above
the wealth-tax threshold (most cells see τ_w = 0, so current-law behavior is
unchanged — a clean no-op check). REAL-side; no σ recalibration (kg realization
is excluded from the ETI tally).

**BUILT + RULED (2026-07-12).** Shipped as commits 47a31b1d2 + 9bb6550ca on
`wealth` per plan `enumerated-meandering-pinwheel` (record-level h product,
gain-weighted age-cell smear, end-of-year survivor convention, no
spec-version bump). T1–T5 all green (full-sample A/B: wealth dial +3.6%
realizations, τ_eq_S 0.070→0.095, +$127B conventional over 2027–2036).
**T6 smear gate breached and RULED ACCEPTED**: the age-cell smear understates
the record-level response (Jensen; conservative, never overstates) by −8.5%
(1%>$1B) to −33% (3%>$500M+CG30) because exposure is only 9–27% of gain
dollars; the exposed/unexposed two-state split closes it to ≤1.5% everywhere
(evidence: other/kg_model_tests/t6_smear_benchmark_result.csv) but was
**declined by the author** (2026-07-12) as not worth the extra state — the
smear's implicit zero-persistence wealth assumption accepted, disclosed.
OPEN follow-up: atlas disclosure line — wealth×CG conditioning panels
understate the interaction, most at aggressive corners (revmax).

Two halves, same family:

**(a) Estate-tax offset on the CG cost at death.** For an estate-taxable
decedent, CG/deemed tax paid at (or before) death shrinks the taxable estate
(and the deemed tax is income-tax-at-death deductible). The effective CG rate
near death for those cells is ≈ τ·(1−τ_e) — a **~40% discount at current top
estate rates, ~45% under the Clausing switch** — but the Bellman's F prices
death at full τ regardless of estate exposure (`kg_dynamics.R:887–888`). So the
model overstates lock-in for exactly the estate-taxable cells whenever switch
#6 is on, and misprices the crown-pair panel (CG × death regime) conditional on
estate. Build: an estate-exposure flag/marginal-rate per cell (the machinery
already reads `ESTATE_ASSET_COLS` to build the `p_char` regressor,
`:394–433`) entering F and MC. Contained, REAL-side, no σ impact.

**(b) Reported-estate elasticity + rate-responsive charitable bequests.** The
estate tax has zero own-rate behavioral response (`f_ded` is a frozen size-bin
fraction, `estate.R:176–193`; `p_char` in kg is a static logistic). Gate 2
de-gated this for MVP because estate is only a conditioning baseline — but the
wealth card's **featured** panel conditions on Clausing-2009 estate, and a
mechanical estate baseline overstates that backdrop's bite, which contaminates
the headline wealth-conditional delta. The contained build sketched in the
frontier notes stands: a Kopczuk–Slemrod-style REPORTING-side elasticity on
reported gross estate (mirroring `wealth/avoidance.R` — reported base only,
`value.*` and `estate_distributable` untouched, exactly the firewall pattern),
plus making `f_ded`'s charitable component and kg's `p_char` respond to the
relevant rates. No σ impact.

### 1.3 Entity-shifting β ← the model's own deferral value (kill the 0.25 stub)

**The gap.** The C-corp side of the shifting wedge is
`τ_c + (1−τ_c)·mtr_kg_lt·(α + (1−α)·β)` with **β = 0.25 a fixed
benefit-of-deferral stub** and dividends proxied at the gains rate
(`pearce_prisinzano.R:81–115`). The model *already computes* the true
deferral discount — τ_eq / τ_statutory from the kg machinery, per year and
death regime. Consuming it would make the C-corp retention shelter collapse
endogenously under deemed-at-death (switch #5) and respond correctly to the CG
rate: the "corporate × ordinary" panel on the corporate card and the
"deemed" conditioning on the ordinary card are both currently priced with a
death-regime-invariant shelter value. That contradicts the model's own thesis
(the 1970s-sheltering story is that closing exits changes the shifting
elasticity's *price*, not just its quantity).

**Build.** Small: replace `alpha + (1−alpha)*beta` with
`alpha + (1−alpha)·(τ_eq_t/τ_t)` (or pass τ_eq directly as the retained-leg
rate), sourced from the same series σ already consumes. Module header itself
flags β as the thing "a future version should more realistically model."
CAUTION: this changes the modeled-avoidance share of the ETI → **σ must be
re-residualized** (one perturbation run) — the one Tier-1 item with that cost.
Do it before the factorial, not after.

## 2. Tier 2 — contained fidelity upgrades, schedule after the campaign starts

### 2.1 Charity: the appreciated-asset margin (CG rate enters the giving price)

Charity responds on `char_cash` only, at the itemized-deduction price alone
(`charity/100.R:22–29`; no behavior module touches `char_noncash`). So the CG
card misses a well-documented leak: raising the top CG rate to 39.6% roughly
**doubles the marginal subsidy to donating appreciated stock instead of selling
it** (donation price = 1 − MTR_ord − τ_cg·gain-share), which both raises giving
and removes realizations. Two pieces: (a) a `char_noncash` response whose tax
price includes `mtr_kg_lt` × an assumed embedded-gain share (~1.0 for gifted
securities); (b) optionally net the induced non-cash giving out of the
realization pool so the dollars aren't taxed as if still realizable. REAL-side.
In the ETI bundle → σ re-residualization. Evidence anchor: the non-cash giving
elasticity literature (donations of appreciated property are the most
tax-sensitive giving component) — parameter honestly sweepable.

### 2.2 Evasion completeness (three deferred pieces, one knob upgrade)

The module is already rate-responsive (net-of-tax elasticities, DHY), so the
gap is narrower than the frontier notes implied. Remaining, all flagged in the
hidden-ledger note as deliberately deferred:

- **Evasion→estate link** — income evaders' estates under-report too; the
  wealth-side link exists (`avoidance.R:202–226`), the estate side is the one
  item left open (`hidden_ledger_design.md` #4). Pattern exists
  (`estate_concealed_frac`); small build.
- **Deemed × concealment blend** — hidden assets should partially escape deemed
  gains; documented gap. Matters only for cells with switch #5 on + wealth tax.
- **Overstated losses/deductions margin** — DHY itemizer elasticities
  (0.069–0.23) exceed the income-leg elasticities the module ships; currently
  positive legs only. Worth at least a sensitivity.
- **Income-graded top-end multiplier** — `EVASION_TOPEND_MULT` is a flat scalar
  on everyone; the Guyton-et-al story it proxies is specifically top-graded.
  Make it a function of income rank rather than a global.

All REPORTING-side; the first two don't disturb σ, the loss margin does.

### 2.3 Corporate layer in σ's conversion price

τ_eq prices the founder-equity path as **corporate-tax-free**: converted
dollars accrue in the gain state and the only tax ever priced is individual CG
(`sigma_conversion.R:335–338`; corp enters kg only as the separate gain-state
debit, not τ_eq). So the "ordinary +5pp conditional on corporate 28%" panel
misses that a higher corporate rate makes the conversion exit *less* attractive
(the retained earnings financing the equity growth bear the corporate layer).
A reduced-form fix — add θ·τ_c to the equity-path rate for the C-corp-routed
share of the pool — is honest and cheap; the clean fix (corp layer inside the
τ_eq recursion) is bigger. Interacts with 1.3 (both discipline the same
boundary); build them with a shared convention so the C-route price is one
object. σ re-residualization required.

## 3. Tier 3 — noted, recommend deferring (with reasons)

- **Dividend payout margin (Chetty–Saez).** α = 0.45 distributed-share is fixed
  in entity shifting and the corp channel's flow split is parameterized. An
  endogenous payout response to the dividend/CG differential matters mostly for
  reforms that split those rates — the v1 switch set doesn't (CG switch moves
  both `mtr_kg_lt` legs together). Revisit if a dividend-specific lever enters.
- **Inter-vivos gift timing.** Behavioral gift acceleration under estate-rate
  changes (the measurement-side γ add-back is not a response). Second-order
  while estate is only a conditioning switch; fold into 1.2(b)'s literature
  (Kopczuk) if estate is ever promoted to a card.
- **Portfolio clientele shifts (muni interest).** Ordinary-rate hikes push top
  portfolios toward tax-exempts. Real but small relative to the σ/entity
  channels at the top, and partially inside the ETI anchor already — adding it
  separately risks double-count against σ.
- **Deliberately excluded by scoring convention (keep excluded, keep saying
  so):** real labor supply, a real rate-of-return saving elasticity (the
  bathtub is *financing*, not a saving response), migration/expatriation
  (subsumed in the wealth-avoidance reduced form, disclosed as a ceiling).
- **Already on their own tracks (not re-litigated here):** annual-MTM /
  deferral-charge lever (`other/accrual_tax/DESIGN.md`, hybrid design, pre-build);
  CORP_* constant calibration (a parameter gap, not a margin gap; gates the
  corporate card's honesty label); anticipation framing (surprise is a D11
  ruling — an anticipatory-realization variant is a robustness run, not a new
  margin).

## 4. Recommended sequence and the σ discipline

1. **1.1 wealth-in-Bellman** and **1.2(a) estate-offset-at-death** first: both
   are out-of-ETI-bundle (no σ churn), both purely sharpen the conditioning
   panels the atlas features, and both have clean no-op checks under current
   law (τ_w = 0 / estate-nonexposed cells unchanged).
2. **1.3 β←τ_eq** + **2.3 corp-layer-in-σ** together (one C-route price
   convention), followed by **one** σ re-residualization run covering both.
3. **1.2(b) estate own-rate** in parallel (REPORTING-side, independent code
   path, no σ impact).
4. Tier 2 evasion/charity items opportunistically; each charity/evasion-loss
   change batches into the next σ re-residualization rather than triggering
   its own.

Rule of thumb going forward: **batch every in-bundle margin change and
re-derive σ once per batch** — σ churn, not build effort, is the real marginal
cost of most items on this list.
