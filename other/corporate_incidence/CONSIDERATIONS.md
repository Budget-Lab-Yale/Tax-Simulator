# On-model corporate tax incidence — considerations & design notes

_Drafted 2026-07-01 from a brainstorm on cross-base interactions (branch `wealth`). Status:
PRE-DESIGN. This documents the economics, the institutional conventions, the conceptual scope
decisions, and the architecture implications for flowing corporate tax changes through to the
individual model. No code yet; implementation ("thread 3") deliberately deferred._

---

## 0. The idea, and the bottom line

Corporate tax is the one major base that stays off-model, but its incidence lands on things we
now model well: dividends and capital gains flows, equity values on the balance sheet, the
wealth-tax base, gross estates, unrealized-gain state (kg_dynamics), and deferred retirement
distributions. Today we handle this with a **manual distribution smear** (see §1); the proposal
is to **replace the smear by modeling the dynamics that give rise to it** — shock record-level
incomes and balance sheets, let the calculator and the stock-tax machinery produce the
consequences.

_(Framing narrowed by the author rulings in §11, esp. D4: v1 is a REVENUE mechanism — gross
corporate receipts in, endogenous individual-side offset out. The distribution smear survives
untouched for distribution presentations; redesigning distribution is a separate, later
decision.)_

Bottom line of the analysis so far:

1. **The honest near-term conventional revenue offset is small** (dividend smoothing + only
   ~24% of C-corp equity in taxable accounts), while the **stock-side effects are immediate,
   large, and land on our newly built bases** (net worth → wealth tax, estate, deemed
   realization). Nobody scores "how much does a corporate rate hike reduce estate-tax and
   wealth-tax revenue over 30 years." We are one exogenous input away from being able to.
2. **Partly a departure, partly an un-hiding.** Per Nunns 2012 (verbatim in §3a, extracted
   2026-07-02), JCT and OTA already *embed* offsetting individual income tax effects inside
   the corporate revenue estimate — netted, undocumented, invisible in provision-level
   tables. Nobody adjusts individual income items in *distribution* tables. Modeling the
   flow-through on records makes the embedded offset explicit and micro-founded; label it
   and gate it so we can still produce convention-comparable output.
3. **The excise-tax 25% offset is NOT the analogy** (§4). That offset is a national-accounting
   identity; the corporate flow-through is an economic model with parameters. Viard (2014)
   formalizes why.
4. v1 collapses to **two boundable parameters**: a capitalization coefficient κ and a
   payout-split/timing parameter (§5, §6). Both get s×M-style bounding sweeps.
   _(SUPERSEDED by D9, 2026-07-02: there is no free κ. Capitalization is computed as the
   perfect-foresight PV of the scenario's legislated rate path; the free parameters are
   data-measurable exposure shares. See §11.)_

Strategic question, unresolved: is the deliverable (a) a better corporate line in conventional
tables, or (b) the 30-year corporate↔estate↔wealth interaction story no one else can produce?
These pull the design in different directions; (b) is the comparative advantage.

---

## 1. Current machinery (what exists today)

- **Off-Model-Estimates interface** supplies `corporate` receipts by year for baseline and
  reform (`revenues.csv`); `other_corp_delta = reform − baseline`.
- **Distribution smear** (`src/data/post_processing/distribution.R:340-345`): allocates
  `other_corp_delta` to records as
  `labor_share × (labor/Σlabor) + (1−labor_share) × (capital/Σcapital)`, with a
  `corp_incidence_phasein` schedule from the runscript. Cost-recovery deltas get a hardcoded
  50/50 (computed from Cost-Recovery-Simulator recovery ratios × `corp.rate`,
  `distribution.R:574-601`). The burden enters the `taxes_included` presentation variants as
  `liab_corp`.
- **Revenue side** (`src/data/post_processing/revenue.R:140`): corporate receipts booked
  off-model with a 0.75/0.25 CY→FY split.
- **Key defect of the smear**: the capital share is allocated by *taxable capital income
  flows*. Most household equity throws off no current taxable flows (retirement accounts,
  non-dividend growth stock, low-realizers) — so the smear systematically shifts corporate
  burden toward flow-rich records and away from wealth-rich/flow-poor ones. The retiree with
  $10M in an IRA and the founder with zero dividends both get ~zero corporate burden today.
  Allocating via actual `value.*` holdings materially changes who bears the tax — a
  publishable finding on its own.
- **Distribution tables are static-sourced** (`distribution.R:148, :200` read
  `static/detail` for both legs) — the wealth bathtub (conventional-only, plan D20) does NOT
  leak into them. Any corporate channel must preserve an equivalent wall (§8).

---

## 2. The flow-through channels (economics)

A corporate rate increase, all else equal:

1. **Shareholder flows (short run).** After-tax profits fall → dividends (`div_ord`/
   `div_pref`) and retained-earnings-driven appreciation (`kg_*`, eventually) fall. Standard
   convention: 100% on owners of capital in the short run.
2. **Capitalization of the existing stock (immediate, one-time).** A rate hike is a windfall
   loss on *old* capital — share values reprice at announcement. Hits `value.*` equity
   holdings, i.e. net worth, immediately. Distinct from the flow effect in both timing and
   mechanism.
3. **All-capital reallocation (medium run).** Harberger logic: capital shifts to the
   noncorporate sector until after-tax returns equalize — interest, rents, pass-through
   returns take a slice, not just C-corp equity.
4. **Labor (long run).** Smaller capital stock → lower wages. JCT/CBO phase toward ~75/25
   capital/labor by year 10. This is a general-equilibrium DYNAMIC effect (see §4 — Viard
   fn.15: absorbed gradually as slower nominal wage *growth*).
5. **Entity choice (behavioral, two-way).** Corporate rate relative to top individual + QBI
   drives C↔PT shifting — income migrates between the off-model base and the on-model base
   (the 1986 lesson). `entity_shifting` module exists. Explicitly sanctioned content for
   conventional estimates (JCT lists it). Deferred for v1.

**The ownership fact that changes the arithmetic** (Rosenthal–Austin 2016): the share of US
C-corp stock in *taxable* household accounts fell from 83.6% (1965) to **24.2%** (2015);
the rest is retirement accounts (~37%), foreigners (~26%), nonprofits/insurers. So:

- The conventional income-tax offset through dividends/gains is fed by ~¼ of the equity hit.
- The **stock-side** effects apply to the entire *household-held* position: retirement-account
  equity → smaller future `txbl_ira_dist`/`txbl_pens_dist` (ordinary income, decades later);
  all household equity → smaller net worth → smaller wealth-tax base and gross estates →
  less estate revenue and less deemed-realization revenue at death; lower accrued gains →
  smaller kg_dynamics unrealized-gain state → smaller step-up cost / deemed base.
- The foreign+nonprofit slice genuinely absorbs burden that never reaches our records (§8,
  reconciliation decision).

---

## 3. What the scorekeepers actually do (verified, primary sources §12)

### 3a. Revenue side (conventional estimates)

**Revised finding (2026-07-02): per Nunns 2012, JCT and OTA DO include these effects —
embedded inside the corporate revenue estimate itself, hence invisible in published
provision-level scores (verbatim quote below).** On the public-methodology surface, JCT
(*Revenue Estimating Process*, Feb 2021) holds GNP fixed but explicitly
allows **composition** responses: "changes among business sectors or the legal form of doing
business... shifts from bonds to stocks in response to dividend or capital gains changes."
So entity shifting, portfolio composition, payout and realization responses can cross the
corporate/individual boundary in a conventional score — but there is **no standardized
household-side offset** for corporate rate changes.

The contrast case: the **25% income-and-payroll offset for excise/indirect taxes**
(JCX-2-23; CBO pub 58549) books exactly the "fixed nominal GNP → indirect tax shrinks factor
incomes → individual receipts fall" logic. Corporate conspicuously lacks the analog — see §4
for why that's structural, not oversight.

**The smoking gun (Nunns 2012, pp. 11–12, "Burden Measure" — extracted verbatim from the PDF
2026-07-02):**

> "Increases or reductions in corporate income tax liabilities will correspondingly decrease
> or increase after-tax corporate income, which will change dividend payments, the value of
> corporate stock, or both. Changes in dividends received or in realizations of gains on
> stock will in turn alter individual income tax liabilities in an opposite, offsetting
> direction to the corporate income tax change. **JCT and OTA estimators include these
> offsetting individual income tax effects as part of the corporate income tax estimate.**
> Using JCT or OTA corporate income tax estimates results in TPC distributing these
> offsetting individual income tax effects in the same way it distributes the corporate
> income tax, instead of in proportion to changes in individual income taxes paid on income
> from dividends and gains on corporate stock."

Implications: (i) the offset exists in practice but is **netted into the corporate line** —
JCT/OTA publish provision-level estimates, never a receipts-type decomposition, so it is
invisible in any published table; (ii) its size, payout assumptions, and timing are
undocumented (Nunns, an OTA veteran, is insider testimony); (iii) supporting case — the 2022
buyback excise: Rosenthal (TaxVox, Aug 2022) infers JCT's revised $74B estimate counted the
increased dividend taxes (incl. foreign withholding) from the assumed buyback→dividend
shift, and PWBM (Mar 2023) explicitly modeled the shareholder-tax effects of that levy. ⚠ Before we book any on-model individual offset we must know
whether our Off-Model-Estimates corporate number already embeds one (one place, one answer).

### 3b. Distribution side (burden tables)

**All four shops IMPUTE a burden amount; none adjusts a single income line item.** The burden
is added to both pre-tax income and taxes (analogous to employer payroll tax treatment).

| | JCT (JCX-14-13, 2013) | CBO (2012→) | Treasury OTA (TP-5, 2012→) | TPC (Nunns 2012→) |
|---|---|---|---|---|
| Long-run split | 25 labor / 75 capital (5/95 for PT taxes) | 25/75 | 18/82 via 63% supernormal→shareholders + 1% cash-flow undistributed + 36% split 50/50 | 20 labor / 20 normal-all-capital / 60 supernormal→shareholders |
| Phase-in | Yr 1 = 100% capital → linear to 75/25 by yr 10 of window | None | None (long-run only) | Standard tables long-run; short-run changes = 100% shareholders (capitalization); cost-recovery provisions 50/50 on normal returns via level-annuity |
| Capital allocation base | 4 holding types: dividends (.602), taxable+exempt interest (.095), IRA reported FMV (.168), imputed DC balances (.135); DB→plan sponsor | Interest + dividends + rents + capital gains *scaled to long-run historical level* | Dividends/gains/closely-held capital share split 63/37 supernormal/normal; interest normal; 40% of retirement distributions | Qualified dividends + stock gains (60/40 supernormal/normal) |
| Sums to revenue? | **No** — 10.8% of capital's share dropped as foreign-borne | Yes — 100% of NIPA corporate receipts | ~Yes — foreign export "explicitly ignored" | Yes — despite documenting 18.8% rest-of-world ownership themselves |

Details that matter for us:
- JCT gives retirement accounts **30.3%** of the capital share (IRA FMV from information
  returns + imputed DC balances) — even the imputation conventions know 1040 flows aren't the
  base. JCT deliberately uses dividends, NOT dividends+gains ("substantial disconnect"
  between realized gains and holdings); CBO scales gains to trend for the same reason.
- TPC varies incidence **by provision type** (cost recovery hits only normal returns → 50/50;
  rate changes hit supernormal → 20/20/60). Precedent for shock-schedule-per-provision.
- JCT's foreign carve-out means **distributed burden ≠ scored revenue by construction** at
  JCT. Precedent for our tables not summing to the corporate revenue line.

### 3c. Load-bearing literature

- **Auerbach 2005/2006** (*Who Bears the Corporate Tax?*): old-capital/new-capital;
  capitalization at announcement; the TRA86 example — a revenue-**raising** corporate reform
  delivering 9–14% windfall **gains** to existing asset holders. Revenue and burden can have
  opposite signs for identifiable households; steady-state factor-share tables miss the
  timing element entirely.
- **Kennedy–Dobridge–Landefeld–Mortenson 2024** (JCT/Fed, TCJA tax records): calibration
  target if we model flow-through — ~49% of gains to firm owners, 11% to executives, 40% to
  workers *above the 90th percentile of within-firm pay*, ~0 to the bottom 90%. (Also: the
  "labor share" is not the bottom of the distribution.) _(Review item 8b: this measures
  rent-sharing out of a CUT; applying the allocation to a hike assumes symmetric wage
  adjustment, which Viard's own one-sided downward-rigidity logic undercuts. Flag whenever
  the labor leg gets modeled.)_
- **Wagner–Zeckhauser–Ziegler** (TCJA event studies): cross-sectional equity repricing lines
  up with firm-level *net* tax positions (deferred tax assets/liabilities, NOLs, foreign
  exposure) — empirical anchor for κ and its heterogeneity.
- **PWBM**: argues 100%-to-capital is the only assumption consistent with a *static* table;
  wage effects belong in dynamic analysis. Even PWBM's flow-through lives in the OLG layer,
  not on tax-unit records.
- **The gap** (verified by lit scan): nothing published (i) puts announcement-window equity
  capitalization into a household distribution table, or (ii) runs record-level flow-through
  inside a conventional score with the induced individual-receipts offset reported. Empty
  space — and a departure-from-convention flag.

---

## 4. Why the excise-offset analogy fails (Viard 2014)

Viard, "Tax Increases and the Price Level" (Tax Notes, Jan 6 2014): the price level is a
monetary phenomenon; the Fed accommodates only taxes that reduce the **short-run
market-clearing real wage** against downward nominal wage rigidity — i.e. taxes with no wage
deduction (employer payroll, VAT, RST). Business income taxes deduct wages, so: "business
income taxes reduce nominal after-tax profits, rather than increase prices, in the short
run"; "no rigidity prevents nominal after-tax profits from falling" (fn.22).

**The excise offset is an identity.** Indirect taxes sit *between* GNP at market prices and
factor incomes in the national accounts. Under the fixed-nominal-GNP convention, an excise
increase reduces nominal factor incomes **dollar-for-dollar, by construction** — immediately,
anonymously, economy-wide. Zero behavioral content; sign and magnitude certain; multiply by
an average MTR (~25%) and standardize as a coefficient.

**The corporate flow-through is a model.** The identity stops one step short: fixed GNP pins
*pre-tax* profits; the corporate tax comes out of the residual claim (after-tax profits),
which is nominally flexible and **largely invisible to the individual tax base until someone
acts**. Between "after-tax profits fall by $X" and "1040 base falls by $Y" sit three decision
layers with no excise analog:

1. **Firm financial policy** — split across dividends / buybacks / retentions. Lintner
   dividend smoothing ⇒ the near-term hit lands mostly on retentions and valuations, not
   current-year `div_ord`.
2. **Household realization timing** — the retention/valuation hit reaches the individual base
   only through realized gains: deferral, lock-in, step-up. Could be decades; could be never.
3. **Account location** — Rosenthal–Austin: ~¾ of the equity hit lands where the 1040 can't
   see it (retirement = taxable later as ordinary income; foreign/nonprofit = never).

Hence: the corporate "offset" is an economic model with parameters, not an identity with a
coefficient. That is *why* JCT standardized the excise offset and never built a corporate
analog — structural, not oversight.

Two direct implications:
- **Viard fn.15 seals the labor-share decision**: the wage effect of business income taxes is
  a long-run capital-accumulation effect, absorbed gradually as slower nominal wage *growth* —
  dynamic by his own taxonomy. Distribution-frame only; fenced out of conventional revenue.
- **No price-level machinery.** Our VAT infrastructure (`vat_price_offset` deflating incomes)
  implements a real economy-wide price wedge. The corporate hit changes *nominal incomes of
  specific identified people* (equity holders) with the price level irrelevant. Do NOT reuse
  the VAT price-offset pattern; the VAT precedent that DOES carry over is the presentation
  semantics (`liab_vat = income − income_reform`, `distribution.R:339` — off-model aggregate
  shock, applied to record-level incomes, burden backed out as an income delta and presented
  as a liability line).

---

## 5. Capitalization arithmetic (the κ parameter)

_(2026-07-02 review: κ-as-free-parameter is SUPERSEDED by ruling D9 — the markdown is the
perfect-foresight PV of the scenario's legislated rate path, and the wedges below survive as
the data content of the exposure parameter θ and as the priced-as-permanent sensitivity
corner. The arithmetic below remains the right benchmark and ceiling.)_

**Naive benchmark (a ceiling, and note the gross-up).** V ∝ (1−τ)π ⇒ a 1pp hike cuts equity
value by Δτ/(1−τ) = 1/79 ≈ **1.27%** at τ=21% — more than a point per point. TCJA sanity
check: 35→21 ⇒ naive +14/65 ≈ +21.5%; the market moved a few percent on tax news, not 21% —
the benchmark is an upper bound, not an estimate.

Five wedges between the benchmark and reality:

1. **US-taxable share ≪ economic profits** — foreign-source income, effective ≪ statutory,
   and deductions *scale with the rate*: interest shields and depreciation allowances are
   worth more at higher τ, so leveraged and NOL/DTA-rich firms are partially hedged (WZZ
   cross-section). The hit is Δτ × PV of *US taxable income*.
2. **Old vs new capital** — existing assets' quasi-rents eat the hike in full; zero-NPV future
   investment contributes no value loss (hurdle rates adjust). Counter-wrinkle: under
   expensing/bonus, the tax on new investment's normal return is ~zero (cash-flow-tax logic),
   so the remaining base is disproportionately rents + old capital ⇒ pushes *toward* fuller
   capitalization.
3. **Policy persistence** — markets capitalize the expected rate *path*, not the statute in
   perpetuity. Expected partial reversal cuts κ a lot. Unknowable ⇒ bounding parameter.
4. **Anticipation** — the fall happens at *news*, not the effective date. Convention: treat
   enactment as the event (defensible; event studies understate total capitalization).
5. **Shifting expectations** — expected migration of burden (wage growth, markups, Harberger)
   reduces what equity absorbs. The incidence debate, priced.

**Stock now, flows later.** The *value* falls immediately; household-received *flows* do not.
After-tax earnings drop on day one; dividends are smoothed (Lintner) — buybacks/retentions
absorb the near-term hit. In model terms: `net_worth`, wealth-tax base, estate base, and the
kg_dynamics unrealized-gain state reprice **now**; the flow-side income-tax effects phase in
behind a **payout-split parameter**. This timing asymmetry is the whole design — and why the
conventional individual offset is modest early even though capitalization is instant.
_(SUPERSEDED by D8/D9/D12: the central case is proportional IMMEDIATE payout — dividends
fall with current-year after-tax profits; no Lintner smoothing, no payout-split parameter.
The stock-reprices-now half stands; the flows-later half does not.)_

**v1 parameterization:** a single capitalization coefficient
κ ∈ [~0.3, 1.0] × Δτ/(1−τ), swept s×M-style, + a payout-split/timing parameter for the flow
leg. Anchors: TCJA event studies (κ), dividend-smoothing literature (payout split), Kennedy
et al. 2024 (within-firm allocation if the labor leg is ever modeled).
_(SUPERSEDED in full by D9/D12: no free κ, no payout-split parameter — perfect-foresight
PV markdown + data-measured exposures. This paragraph is kept for the arithmetic only.
NB: this κ is unrelated to D15's κ, the corporate share of the normal-capital stock.)_

---

## 6. Conceptual scope map (the three buckets)

The crisp statement of the tables relationship: **revenue tables answer "what does Treasury
collect, holding aggregates fixed"; distribution tables answer "who ultimately bears a dollar
of it."** Different questions; the shops already let them disagree (JCT drops foreign-borne
burden from tables but not the score; TPC distributes a burden the score never books against
individual receipts). The current smear architecture enforces that wall structurally. Going
on-model tears it down, so it must be rebuilt as **explicit per-channel frame tags**:

| Bucket | Channels | Revenue frame | Distribution frame |
|---|---|---|---|
| 1. Within-fixed-GNP mechanics + sanctioned behavior | after-tax-profit pool → payout/retention composition; capitalization of old capital (asset prices, not GDP); entity/portfolio/realization shifting | **In** (⚠ = departure from JCT comparability for the mechanical flow leg; gate it) | In |
| 2. GE factor-price import | labor share (wages via smaller capital stock) | **Out** (dynamic; Viard fn.15) | In (as conventions do, phased) |
| 3. Dynamic proper | capital path, productivity, growth | Out | Out |

⚠ Bucket 2 trap: if the labor share is modeled as record-level wage shocks, its induced
individual-tax changes MUST be fenced out of conventional revenue or we've done stealth
dynamic scoring. (The conventions avoid this by imputing a burden amount without touching
wages — the modeled version needs the fence built explicitly.)

_(v1 narrowing per D4/D5: the "Distribution frame" column above is the conceptual map;
v1 leaves distribution presentations on the smear (D4) and books the revenue frame on
conventional-side passes only (D5).)_

---

## 7. Provision-type scope (not all "corporate tax" is one shock)

The existing smear already encodes the right instinct: `other_corp_delta` gets the phased
labor/capital incidence rule, while cost-recovery deltas are computed separately from the
Cost-Recovery-Simulator and hardcoded 50/50 (`distribution.R:340-345`, `:574-601`). Do not
collapse that distinction in the on-model design. A corporate rate cut and more generous
depreciation policy have different economics, timing, and capitalization content.

Working taxonomy:

1. **Statutory rate changes / rent-heavy base changes.** Best v1 candidate for the
   balance-sheet channel. They change the after-tax residual claim on old capital, so
   announcement capitalization into C-corp equity values is conceptually cleanest. Flow effects
   then arrive through dividends, buybacks/realizations, and retained earnings.
2. **Cost recovery / depreciation / expensing.** Mostly a user-cost and timing shock on new
   investment and normal returns. It may affect asset values, but not as a simple old-equity
   haircut proportional to `Δτ/(1−τ)`. Keep this as a separate channel unless Off-Model-Estimates
   supplies an old-capital valuation shock; the current 50/50 special case is crude but correctly
   separate. _(Verdict upgraded by D13: the split is conceptually CORRECT — receipts = timing
   seesaw, distributed burden = annuitized τ·Δz·I time-value transfer — and the equity channel
   must never touch depreciation, because the old-capital revaluation is SIGN-FLIPPED
   relative to the rate case. See D13.)_
3. **Targeted base broadeners / credits / international provisions.** Interest limits, R&D
   amortization, sector credits, GILTI/FDII/BEAT, CAMT-style rules, etc. need custom incidence
   tags: affected firm set, rent vs normal-return share, debt/equity incidence, foreign share,
   permanence, and whether the provision changes marginal investment incentives.
4. **Entity-boundary provisions.** Corporate rate changes relative to individual/pass-through
   rates, QBI, integration, etc. primarily raise C↔PT shifting questions. These should route
   through or extend `entity_shifting`, not be silently absorbed into the equity-capitalization
   channel.
5. **One-time transition / repatriation / accounting-liability provisions.** Cash-flow and
   balance-sheet accounting events, often with limited marginal-investment content. Treat as
   separate provision types; capitalization depends on surprise, expected enforcement, and
   persistence.

Architecture implication: the input should not be a single corporate delta plus one incidence
phase-in. It should be a provision-type shock schedule. Minimal columns: `year`,
`provision_type`, `receipts_delta`, `capitalization_delta`, `payout_flow_delta`, `labor_share`,
`shareholder_share`, `normal_capital_share`, `foreign_nonprofit_share`, and an asset/exposure
tag. The status-quo smear remains the fallback for provision types without a defensible
on-model mapping.

---

## 8. Architecture implications (collected; implementation deferred)

1. **Input = provision-type exogenous shock schedule per scenario**, from the
   Off-Model-Estimates jawn
   (which keeps its job as the aggregate shock source: receipts for revenue tables + Δ
   after-tax domestic profits as the shock size). Wants the old/new-capital split: a
   capitalization factor (immediate) + flow factor path (phased) + labor-share path
   (distribution-only), keyed by `provision_type` (§7) — a small schedule, like the wealth
   financing profiles, absorbing the current `corp_incidence_phasein`.
2. **Static AND conventional placement** (it's exogenous incidence, not a behavioral
   response — the VAT precedent), with the frame tags of §6. _(SUPERSEDED by D5:
   conventional-side passes only; static stays the clean law-only counterfactual.)_
3. **Kill the smear when the shock is on-model.** One switch: shock scenarios skip the
   `liab_corp` allocation block; the burden enters the `taxes_included` presentations through
   the income-delta pattern instead (`liab_vat` template). _(MOOTED for v1 by D4: the smear
   survives untouched for distribution presentations; no kill-switch ships.)_
4. **Double-count check on the revenue side**: does the Off-Model-Estimates corporate number
   already embed an assumed individual offset? If yes it comes out. One place, one answer.
   _(Review item 8a, 2026-07-02: if the number is JCT-benchmarked, the embedded offset is
   undocumented (Nunns) and likely cannot be cleanly un-netted — Phase 0b may NOT resolve
   this. Error accounting if net is treated as gross: the offset is double-counted, so
   combined revenue is UNDERSTATED, bounded by the embedded offset's size — plausibly a few
   percent of the corporate estimate given the 24% taxable share and payout composition.
   State the sign and bound in outputs rather than assuming resolution.)_
5. **Basis does NOT scale with value.** The wealth-bathtub applier scales `kg_lt_basis`
   proportionally with `kg_lt` (sale-financed erosion preserves the gain ratio). A valuation
   drop is different: price falls, basis fixed, the *gain* absorbs the entire hit — every
   holder's gain falls more than proportionally to the value hit (amplification μ/(1−b),
   INCREASING in basis share b: high-basis lots are hit hardest in proportional terms;
   low-basis top-tail lots lose the most dollars because they hold the gains). Debit the
   kg_dynamics unrealized-gain state directly; do not reuse the proportional-scaling rule.
   _(Wording corrected in the formal-model session — FORMAL_MODEL P5; reduced form for
   non-kg runs ruled in D18.)_
6. **Heterogeneous asset vector.** Directly held stock + an equity share of mutual funds + an
   equity share of DC retirement get the direct capitalization hit — DB is NEVER debited on
   records (D10): its valuation shortfall lands on plan sponsors and joins the unallocated
   residual, and `value.db` is measured only to SIZE that residual slice. Pass-throughs get
   only the smaller Harberger all-capital slice (flows-only per D14/P14 — pt `value.*` columns
   untouched). Unlike the wealth channel's uniform `(1−f)`, the `value.*` scaling vector
   varies by column. (Recon 2026-07-02: `value.equities` is first-class; imputation needed
   for the dc/trusts/re_fund equity shares.)
7. **Retirement-distribution erosion** (`txbl_ira_dist`/`txbl_pens_dist` scaling with eroded
   balances) is a prerequisite-ish gap shared with the wealth channel — the corporate channel
   is its second customer.
8. **Bathtub composition (the sign trap).** If the shock lowers household incomes, individual
   liabilities fall ⇒ ΔT⁰ goes *negative* ⇒ the bathtub credits s×(tax relief) back to
   wealth. Economically correct **only if** the capitalization hit debits the wealth state
   directly as its own term: household loses $X of equity value (direct balance-sheet debit →
   estate/wealth bases) and gains $Y of individual tax relief (ΔT⁰ channel, partially
   offsetting). Route capitalization through ΔT⁰ by accident ⇒ wrong sign.
   **RESOLVED FULLY by D11 (2026-07-02):** even the "correct" version above still had the
   wrong sign on the flow leg — a tax-only forcing hands the bathtub the rebate without the
   loss. The forcing is generalized to F = ΔT − ΔY_exog (net after-tax cash flow), which
   debits wealth s·D(1−τ) instead of crediting s·R. See D11 for the derivation and the
   no-double-count theorem.
9. **Burden ≠ revenue reconciliation decision**: allocate the full corporate revenue to US
   households (CBO/OTA/TPC convention), or let foreign/nonprofit absorption reduce it (more
   accurate; JCT precedent for not summing; "foreigners bear $X of this hike" is itself a
   reportable line)? Must be decided and labeled either way.
10. **Measured offset as output.** JCT's individual-side effects are aggregate coefficients;
    we can *measure* the offset on microdata — the `mtr_cap_bundle` machinery already pushes
    composition-weighted capital flows through the calculator. The offset falls out as an
    honest micro-founded number, distributed correctly.
11. **Distribution stays static-sourced** (D20 invariant preserved): the corporate shock
    applies to both passes, so static detail carries it; the bathtub interaction remains
    conventional/receipts-only. _(SUPERSEDED by D5: the shock is conventional-only and
    static detail does NOT carry it; distribution stays on the smear per D4. The D20
    invariant is preserved trivially — and the heir-allocator pin to static totals is
    untouched, resolving §9.7.)_
12. **Entity shifting deferred** to v2 — real two-way revenue implications, but needs a stance
    on the corporate-side revenue statement, which is off-model.
13. **Stacked-estimate order dependence** (review item 7, 2026-07-02). Once the offset is
    endogenous (D1), the corporate provision's total depends on the individual-side law it
    is stacked on — stack it after a dividend/CG rate hike and its offset is mechanically
    larger. Standard stacking caveat, newly applicable to the corporate line specifically:
    the corporate row is NOT order-invariant. Note in stacked-report documentation when the
    channel ships.
14. **Fail-closed input contract** (added 2026-07-02, external review). D1 (gross-of-
    offset) and D13 (rate-type) are interface CONTRACTS, not comments — enforcement
    mirrors the house refusal-gate pattern (`kg_dyn_check_run_compat` /
    `wealth_dyn_check_run_compat`): the channel activates ONLY when the Off-Model-
    Estimates corporate input carries an explicit declaration (metadata:
    gross_of_offset + provision_type = rate); absent or contradicting metadata → hard
    stop, fall back to the smear (status quo), loudly. Plus a mechanical guard: reject
    receipts paths whose cumulative delta reverses sign or retraces beyond a threshold —
    the timing-seesaw signature of depreciation-type provisions, whose old-capital
    revaluation is SIGN-FLIPPED (D13) — even when declared rate-type.

---

## 9. Open questions

1. **Deliverable framing** (drives everything): better conventional corporate line vs the
   30-year stock-side interaction story? (Comparative advantage is clearly the latter.)
2. Does Off-Model-Estimates expose enough structure to derive the Δ after-tax domestic profit
   path, or receipts only?
3. What equity-exposure vector can be built from Tax-Data `value.*` (direct stock, fund
   shares, DC equity share — plus DB-residual SIZING only, per D10; DB is never debited)?
4. Convention toggle design: can one runscript column select {smear (status quo) |
   on-model incidence}, with frame tags handled internally?
5. Does our off-model corporate number embed an individual offset already? (§8.4)
6. Where do κ and the payout split get their central values — TCJA event-study calibration,
   or convention-anchored (e.g., κ = 1 short-run like JCT/TPC's 100%-capitalization year-1)?
   **RESOLVED (D9 + D12): no κ — perfect-foresight PV of the legislated path; exposure
   shares from data; payout composition data-embedded via record-level scaling (D12), no
   parameter needed.**
7. How does the corporate shock interact with the estate allocator identity (Σw·p·λ pinned to
   static totals) — mechanical check once the value.* debit exists on static side.
   **RESOLVED (formal-model session, 2026-07-02): under D5 the shock is conventional-only,
   so static estate totals never move and the pin is untouched (the question's
   static-side premise was itself stale).**
8. Which provision types are eligible for on-model capitalization in v1? Recommended: rate /
   rent-heavy shocks only; keep cost recovery and targeted provisions on the fallback smear until
   an old-capital valuation shock is supplied. **RESOLVED (D13): v1 asserts the corporate input
   is rate-type (labeled limitation); depreciation permanently separate on its own interface
   (sign-flip); eligibility criterion = receipts path is a valid Δπ proxy.**

---

## 10. Proposed path forward (added 2026-07-02)

Principle: **quantify before architecting** (the house style — bounding before building), and
don't stack a third major mechanical channel on an unmerged branch. Two cheap decision gates
this week, then a disciplined build.

### Phase 0 — decision inputs (no model changes; ~days)

- **0a. Stakes memo** (sbatch analysis under `other/corporate_incidence/`): take a canonical
  21→28 rate hike and, using existing baseline detail files + Tax-Data `value.*`, compute
  (i) the naive-κ capitalization hit to household equity by net-worth percentile
  (`value.equities` is a first-class column — recon 2026-07-02 — plus imputed equity shares
  of `value.dc`/`value.trusts`/`value.re_fund`; `value.db` is measured ONLY to size the
  D10 residual — DB is never debited on records); (ii) the taxable-flow offset
  ceiling (div + kg at effective rates; cf. the ~4–5¢/$ back-of-envelope); (iii) estate /
  wealth-tax / deemed-realization base deltas over 10–30 yrs using `estate_m` machinery;
  (iv) the holdings-based vs current-smear allocation comparison. **Gate: are the stock-side
  effects material?** (Prior: yes at the top tail — estates are equity-heavy.)
- **0b. Provenance recon**: inspect an Off-Model-Estimates vintage (structure: receipts only,
  or profit paths? who produces it, and is it JCT-benchmarked?) — the Nunns finding sharpens
  §8.4 into a fork: **if the off-model corporate line is JCT-benchmarked, the individual
  offset is already inside it** (embedded per Nunns), and booking an on-model offset
  double-counts unless the corporate line is grossed up. Also mine the sibling **`buyback-tax`
  repo** (recon: it exists in `~/Repositories`) — likely home of an in-house payout-shift
  model whose parameters seed the payout leg.
- **0c. Crowdsource harvest**: fold coworker/ex-JCT answers on embedded-offset mechanics into
  §3a; they pin the convention-comparable mode.

### Phase 1 — standalone prerequisite (independent change on `wealth`)

Retirement-distribution erosion (`txbl_ira_dist`/`txbl_pens_dist` scaling with eroded
balances, §8.7) — a real gap in the wealth channel today and a prerequisite here. Own commit,
own bounded-impact test.

### Phase 2 — design doc + adversarial plan review

Full design (shock-schedule schema per §7; frame tags per §6; placement; the §8 list as
requirements), then the multi-agent plan-review treatment that wealth dynamics got. Note the
expected simplification vs the bathtub: the corporate shock is an **input transformation at
the head of run_one_year** _(per D5: conventional-side passes only, incl. conv-no-wealth —
not "both passes" as an earlier draft said)_ — no forcing pre-pass, no new SLURM phase expected;
the cross-year complexity lives only in the kg_dynamics accrual-state debit (§8.5).

### Phase 3 — v1 build

Rate-type provisions only (§7.1). Capitalization leg (perfect-foresight PV markdown of the
legislated path, D9) + dividend/flow leg (statute-tracking, D8/D9) + receipts consistency
check + the D9 conservation identity as a hard invariant. Sensitivity sweeps: the exposure
parameters (θ, kg equity share, §8.6 vector) + the priced-as-permanent corner (legislated
sunset disbelieved). Validate against the smear on a test scenario; keep the smear as the
default for standard scores until the Phase 0b/0c convention questions are resolved.

### Phase 4 — deliverable

Recommended resolution of open question 9.1: **build once, two presentation modes; launch as
a research piece** ("who really bears a corporate rate increase — a stock-flow-consistent
microsimulation"), with two headline results: the holdings-based vs flow-smear distribution
comparison (§1) and the corporate→estate/wealth/deemed interaction revenue over 10–30 years.
Conventional scoring keeps the off-model line + smear until validated.

_(Review item 8d, 2026-07-02: the smear-defect finding — holdings-based vs flow-based
allocation, §1 — is publishable STANDALONE off the Phase 0a stakes memo with zero new
machinery (a static reallocation exercise on existing detail files + `value.*`). Elevate it
so it survives even if Phase 0 gates the big build poorly; it is also the natural first
deliverable while Phase 2 review runs.)_

Sequencing notes: merge/stabilize the `wealth` branch before Phase 2+ (corporate depends on
`value.*`, kg_dynamics state, and the bathtub interaction, and review scope stays sane);
heir-side recycling (from the broader interactions brainstorm) amplifies the 30-yr story but
is NOT blocking; financing-realizations is orthogonal.

---

## 11. Author decisions — conceptual rulings (grilled 2026-07-02)

Q&A session with the author on concept-level design questions (pre-implementation). D1–D8
are CONFIRMED rulings; P1–P3 were asked but the session timed out — the recommended defaults
are recorded and stand unless overridden.

### Confirmed rulings

- **D1 — Offset accounting: gross input, endogenous offset, nothing broken out.** The
  corporate input to the model is a **gross corporate revenue estimate**. The model then
  computes the individual tax offset **endogenously** — record-level income shocks flow
  through the calculator and the offset simply materializes inside the normal receipts
  deltas. No special offset line in input or output. Consequences: (i) the Off-Model-
  Estimates corporate number must be **gross-of-offset by construction** — if current numbers
  are JCT-benchmarked (offset embedded per Nunns, §3a), they must be re-derived or grossed
  up; §8.4's "one place, one answer" resolves as *the input is defined gross*. (ii) Our
  corporate line is deliberately NOT JCT-comparable line-by-line; the *total* is the
  internally consistent object.
- **D2 — Shock denomination: receipts in, mapping function does the math.** The input is
  denominated in **Δ receipts** (what corporate models emit). The central design object is
  the **mapping function** from receipts delta → household-side income/balance-sheet hit,
  internalizing scope (foreign/nonprofit ownership shares), possibly corporate-side
  avoidance, channel structure, and who owns corporate equity. The function's form is
  deliberately NOT pre-committed — it is the main Phase 2 design task, driven by which
  channels matter and the ownership data.
- **D3 — Foreign/nonprofit slice: honest absorption inside the mapping function.** The
  household hit is scoped to household-held exposure; no gross-up to force anything to sum
  to the revenue line. "Foreigners/nonprofits bear $X" is implicitly the unallocated
  residual of the mapping.
- **D4 — THE FRAME: this is a revenue mechanism, not a distribution redesign.** The channel
  changes what the model *collects* (endogenous offset in conventional receipts).
  Distribution is a **separate thing** — or at minimum must retain the possibility of being
  different. The smear survives untouched for distribution presentations. Supersedes the
  "replace the smear" framing of §0/§1 on the distribution side; what gets replaced is the
  *absence of the offset in revenue*. (Also moots §8.3's smear kill-switch and §9.4's
  convention toggle for v1.)
- **D5 — Pass placement: conventional only.** Mirrors the bathtub doctrine (D20): static
  stays the clean individual-law-only counterfactual; the offset surfaces in conventional
  receipts; static − conventional isolates the corporate interaction; static-sourced
  distribution tables are structurally untouched. (Design-doc item: the shock presumably
  applies on ALL conventional-side passes incl. conv-no-wealth, so the bathtub's ΔT⁰
  forcing inherits the corporate-induced tax relief — consistent with P2, but must be
  deliberate.)
- **D6 — v1 scope: flows AND stocks from day 1.** Income flows (dividends, realizations)
  AND balance-sheet repricing (`value.*` → net_worth → wealth/estate bases; kg_dynamics
  unrealized-gain debit; retirement balances). The endogenous offset therefore includes the
  estate / wealth-tax / deemed-realization legs — the comparative advantage. Pulls in the
  retirement-distribution-erosion prerequisite (§8.7).
- **D7 — Capitalization timing: enactment year, full surprise.** Balance sheets reprice in
  the enactment calendar year; flow effects follow the profit path. Standard
  scored-against-pre-announcement-baseline convention; matches JCT/TPC short-run
  100%-on-owners-of-existing-capital treatment.
- **D8 — Payout rule: proportional payout.** Dividends fall in proportion to after-tax
  profits immediately (fixed payout ratio) in the central case. Author chose simplicity and
  front-loading over Lintner smoothing; the smoothing/fixed-dollar variants remain available
  as sensitivity corners but are NOT the central rule. Consequence: the IIT offset arrives
  earlier than under smoothing; the valuation channel carries correspondingly less of the
  near-term hit.

### Rulings from the external economics review (item-by-item session, 2026-07-02)

- **D9 — Stock-flow consistency under perfect foresight; NO free κ, NO free flow knob.**
  Derived from finance-101 ground-up modeling with the author (V = PV of after-tax
  distributions, market-set r, MM on payout policy). One Δ-after-tax-profit path is read off
  the **scenario's legislated rate path** (law as written, perfect foresight — the scoring
  convention), scaled by a data-parameterized US-taxable exposure share θ. Everything derives
  from that one path:
  - **Dividends track the current-year statute** (firms pay out of actual current profits) —
    confirms and sharpens D8.
  - **Equity markdown at year t = PV of the *remaining* legislated flow hits**, recomputed
    yearly. Permanent shock ⇒ constant proportional markdown, dividend yield preserved
    (yield preservation is a theorem here, not an assumption). Temporary n-year shock ⇒
    markdown = annuity share at enactment, **shrinking to zero at expiry**: the price climbs
    back to baseline with above-normal recovery appreciation, holders during the window earn
    low dividends + recovery gains = r, and the entire burden is a one-time capital loss to
    whoever held at announcement (= PV of the window's dividend cuts). Auerbach old-capital,
    operationalized.
  - **Capital gains flow through the unrealized-gain state**: debit at enactment (basis NEVER
    scales, §8.5), credit back as the markdown shrinks. Non-kg runs use the reduced form
    (scale kg flows with the current-year markdown) _(SUPERSEDED by D18: basis-aware form
    μ·(kg + kg_lt_basis), plus the φ quantity term in BOTH run types)_. kg flows get a **corporate-equity
    exposure share** (~half of LTCG per SOI sale-of-capital-assets data — stock + fund shares
    vs pass-through sales/real estate/other) — the kg-line entry of the §8.6 exposure vector.
  - **Consequence for the headline**: temporary shocks depress estate/wealth/deemed bases
    only while the window is open (decedents dying after expiry are untouched) — the 30-year
    interaction story exists only insofar as the policy is permanent. Feature, not bug.
    _(AMENDED by D17: true of the markdown leg only — the D11 dissaving accumulated during
    the window compounds past expiry, so post-expiry decedents are NOT untouched.)_
  - **Conservation identity as the model invariant** (and, given D1 non-comparability,
    effectively v1's only external validation): per year and in PV, household hit +
    unallocated residual (foreign/nonprofit/DB) + Treasury take = Δπ. Check it
    allocator-style.
  - Double-counting the retention leg (value debit + independent kg flow shock) is
    structurally impossible: both legs are the same object measured twice (ΔV = Σ ΔCF/(1+r)^t).
  - Free parameters remaining: **data-measurable exposures only** (θ; kg equity share; the
    §8.6 balance-sheet vector). Sensitivity corner (NOT central): markets price the statute
    as permanent despite a legislated sunset (the TCJA-sunset-disbelief case). Supersedes
    §5's κ ∈ [0.3, 1] sweep and resolves §9.6.
- **D10 — DB pensions to the residual.** The DB equity share is NOT debited to household
  records: benefits are defined, the valuation shortfall lands on plan sponsors (JCT's own
  convention assigns DB to the sponsor). The DB slice joins foreigners/nonprofits in the
  honest unallocated residual (D3). Possible v2 refinement: recycle it as a second-round
  equity hit on sponsors; not v1.
- **D11 — The bathtub is the budget constraint: price/quantity taxonomy + generalized
  forcing.** (Resolves review item 2 AND the corporate↔wealth-bathtub interaction question;
  derived step-by-step with the author from the household budget identity
  C + Δsavings = Y − T. Author fully sold.)
  - **Taxonomy.** Every shock to a household is one of two things. A **revaluation** (price
    margin: p changes, shares held n don't — no cash moved) hits the balance sheet directly
    and NEVER routes through the bathtub; MPC out of revaluations = 0. This confirms P2's
    pure-debit doctrine with its real rationale: at the top tail, where the estate/wealth
    action lives, bequests are the residual claimant and a dollar of valuation loss passes
    ~one-for-one into the estate. A **cash flow** (quantity margin: dollars in/out this year,
    which can change n) enters the bathtub forcing and gets the s-split.
  - **Generalized forcing:** F = Δ(taxes) − Δ(exogenous pre-tax income), replacing the
    tax-only ΔT⁰. Numerically identical to the current design for every existing scenario
    (no income-shock channels yet) — a pure extension, nothing recalibrates.
  - **The sign flip it fixes.** Corporate flow leg, per household: dividend income falls D,
    tax falls R = τ·D (the relief is a REBATE ON A LOSS, always smaller than the loss). Net
    after-tax cash flow = −D(1−τ) < 0 always. Tax-only forcing hands the bathtub the rebate
    without the loss → wealth CREDITED s·R (household appears to profit from the hike;
    §8.8's sign trap one level down). Generalized forcing → wealth DEBITED s·D(1−τ):
    the household dissaves to partially maintain consumption, eroding wealth beyond the
    markdown. (D=$10, τ=20%, s=0.5: +$1/yr credit vs −$4/yr debit.) Fully resolves §8.8 and
    supersedes the D5 parenthetical about ΔT⁰ "inheriting the corporate-induced tax relief."
  - **No-double-count theorem.** The D9 markdown moves p (once); the dissaving moves n
    (yearly, chosen). Orthogonal margins. Proof by budget identity: each household's total
    PV burden — PV(consumption cuts) + PV(bequest reduction) — equals the announcement
    capital loss net of PV(tax rebates) under ANY closure (MPC 0, 1, or 1−s). **s does not
    change the burden; it allocates a fixed PV burden between lifetime consumption and the
    estate.** The bathtub adds nothing and cannot double-count by construction. Slots into
    the D9 conservation identity: household PV burden = capital loss − PV(rebates); rebates
    are Treasury's endogenous offset; the residual slice never reaches records.
  - **Endogenous behavioral responses stay OUT of F** (kg realizations, labor supply —
    chosen, not financing shocks). F takes exogenous shocks only.
  - **Consistency cleanup:** the previous implicit design carried three different MPCs
    (0 out of the stock loss, 1 out of lost dividends, 1−s out of tax changes). Now one
    parameter, one rule: a dollar of lost dividend income is treated exactly like a dollar
    of extra tax.
  - **Dropped:** the proposed MPC-out-of-wealth sensitivity corner — under the taxonomy it
    is a revaluation response, cleanly out of scope for v1.
- **D12 — Composition-neutral proportional scaling (the dividend/buyback/retention split is
  data-embedded, not a parameter).** (Resolves review item 4 — which largely DISSOLVED under
  D9.) The three distribution margins are pure tax character under MM (dividend dollar:
  taxed now, in full, all taxable holders; buyback dollar: sellers only, gain-over-basis
  only, seller mix skews retirement/foreign; retention dollar: deferred CG or step-up). The
  original worry — booking the whole payout cut on the dividend lines, overstating the
  near-term IIT offset ~2× since ~half of payout is buybacks — presumed an aggregate payout
  number mapped to a tax line. D9's machinery instead scales **record-level flow columns,
  each at its own tax character**: `div_*` fall by the flow factor (dividend share of
  profits already embedded in the microdata); `kg_*` fall by exposure × factor, which
  **automatically captures the buyback channel** (a buyback cut = fewer forced sales; the
  returned principal was never taxed, only the gain slice — proportional gain scaling is the
  correct reduced form); retentions live in the gain-state debit, where deferral/step-up
  come for free. Formal gloss on D8: every distribution margin scales with after-tax
  profits; the baseline composition is held fixed at whatever the microdata embodies.
  **On the single-factor blend (superseded):** any ONE kg factor blending buyback-driven
  realizations (which track the current-year statute, φ) with appreciation-driven ones
  (which track the markdown path, μ) carries a blend error ∝ (φ+μ) — exactly zero for
  permanent shocks but FIRST-order in the shock for short sunsets (φ+μ → −m as the window
  shrinks). No blended factor ships: D18's φ/μ split (quantity margin at φ, price margin at
  μ via `kg_lt_basis` / the gain-state debit) is the implementation.
  **Deferred to
  v2:** payout-SHIFT behavior (dividend↔buyback substitution, the 2022 buyback-excise
  question) — a behavior module, not mechanical incidence; the `buyback-tax` repo seeds it,
  and the Phase 0b recon on that repo now attaches to the v2 module, not v1.
- **D13 — Provision-type doctrine: v1 asserts rate-type; depreciation is permanently
  separate, with the sign-flip as the stated reason.** (Resolves review item 5 + the
  depreciation economics session, 2026-07-02.)
  - **The eligibility criterion (replaces "conservatism" as the §7.1 gate rationale):** a
    provision is eligible for the on-model equity channel iff its conventional receipts path
    is a valid year-by-year proxy for its after-tax domestic profit path. Rate/rent-heavy
    changes qualify almost by construction (what Treasury gains, shareholders lose, net of
    the avoidance already embedded in the conventional corporate score). Timing provisions
    (bonus, R&D amortization, CAMT), transition/repatriation taxes, and targeted
    base-broadeners fail it structurally. The gate is an **interface contract**: to promote
    a provision type off the smear, Off-Model-Estimates must supply an actual profit path
    (or, for transition-type provisions, a valuation shock) — not a receipts path.
  - **v1 operating rule:** ASSERT that the off-model corporate input is rate-type and run
    the D9 channel on it _(enforced fail-closed per §8.14: explicit input declaration +
    seesaw guard, not a comment)_. Labeled limitation: the further a scenario's corporate
    change is from the rate case, the worse the model. One exemption: **depreciation policy, which
    never enters through the corporate input at all** — it arrives on its own
    Cost-Recovery-Simulator interface (verified 2026-07-02): receipts delta read at
    `revenue.R:59-62,172-173` (confirmed = ccorp + PASS-THROUGH combined, checked against
    `totals/revenue.csv` form split on vintage 202506291812/01_bonus); distributed burden
    independently recomputed at `distribution.R:600` as investment × ΔPV(deductions) ×
    corp.rate, ccorp leg only, 50/50, no phase-in.
  - **Why depreciation stays out of the equity channel FOREVER (not caution — sign):**
    old/new capital economics. A rate change taxes existing capital's remaining cash flows —
    old capital absorbs it in VALUES (D9), new capital in QUANTITIES (dynamic). A
    depreciation change statutorily touches only NEW investment (old assets grandfathered);
    the burden reaches old capital as a REVALUATION in the shadow of the new-capital
    subsidy: nobody pays more for an installed machine than the net-of-subsidy price
    (1 − τz) of a new one, so more-generous depreciation DEVALUES existing equity by
    ≈ τ·Δz per dollar of installed reproducible capital (Tobin's q < 1; used-car logic;
    Auerbach–Kotlikoff/Summers). I.e., a receipts CUT is a shareholder LOSS — feed
    depreciation receipts into D9 and the sign is wrong, not just the size. Fully symmetric
    in reverse (stingier depreciation = windfall GAIN to old capital — Auerbach's TRA86
    9–14% windfall, §3c, is this theorem observed in the wild). Gale–Auerbach VAT
    equivalence: expensing = cash-flow tax = business half of a consumption tax; every step
    toward it is a one-time relative levy on old wealth, exempting new — same theorem as
    the VAT transition burden.
  - **Existing infra split BLESSED as conceptually correct** (upgrades §1's "crude but
    correctly separate"): receipts line takes the timing seesaw (right for Treasury cash —
    e.g. 01_bonus: −$61B 2026 reversing to +$35B by 2030); distribution takes the
    annuitized time-value transfer τ·Δz·I — which is the true economic gain (economic
    earnings barely move; the deferral's time value is the whole prize). The 50/50
    ALLOCATION of that burden stays as-is per D4. Also principled: the new-capital benefit
    has NO record-level owner (zero-NPV at the competitive margin including the subsidy;
    competed forward into prices/wages/expansion) — a smear is forced by the economics,
    not a shortcut. Caveat for someday: rent capture on inframarginal investment (Kennedy
    et al.) means partial shareholder capture; not v1, not v2.
  - **Old-capital revaluation: known, small, sign-flipped, DELIBERATELY UNMODELED.**
    Attenuated because only reproducible capital reprices (rents/intangibles — most of
    C-corp equity value — don't compete with subsidized new machines) and temporary bonus
    has small ΔPV. Recorded so nobody later "fixes" depreciation by capitalizing it with
    the rate-case sign.
  - **Pass-through leg = named v2 candidate.** The pt slice of a depreciation delta (e.g.
    −$13B of −$61B, 2026 bonus) is individual income tax in reality, but today it is
    (i) booked on the CORPORATE revenue line (the interface delta is ccorp+pt combined),
    (ii) distributed to NO ONE (the burden formula uses ccorp columns only), and (iii)
    never touches records. Unlike the C-corp leg it HAS record-level claimants (Schedule
    C/E/F, K-1 owners); running the deduction-timing path through the calculator prices it
    at true marginal rates + QBI + NIIT and fixes the labeling, the missing distribution,
    and the rate precision in one move. Interface already ships the pt path
    (`recovery_ratios_form.csv`).
- **D14 — Harberger migration ON in the central case, flows-only, vintaged by the house
  VAT replacement machinery, decaying the equity markdown to a rent-share floor.**
  (Resolves review item 6 — author's "important" flag — after a ground-up derivation
  session, 2026-07-02.)
  - **Mechanism:** capital exits the corporate sector until after-tax returns re-equalize
    at a lower economy-wide r′ (corporate PRE-tax returns rise via scarcity; noncorporate
    pre-tax returns fall via crowding; everyone's after-tax return falls). Pure
    reallocation of a FIXED capital stock = a COMPOSITION effect inside the fixed-GNP
    convention (JCT-sanctioned, §3a) — bucket 1. The labor leg (smaller TOTAL stock →
    wages) stays bucket-2 fenced (Viard fn.15). Entity shifting is the legal-form cousin
    of this same reallocation; still deferred (D12/v2 adjacency noted).
  - **The supply-elasticity theorem (what migrates as flows vs prices):** a shock splits
    between flow and price by the supply elasticity of the underlying asset. Elastic
    supply (reproducible capital — buildings, equipment): price PINNED at replacement cost
    by construction arbitrage; the burden arrives ENTIRELY as yield compression (flow
    falls with the discount rate, price cannot move). Inelastic supply or rigid contract:
    price absorbs (existing bonds APPRECIATE — fixed coupon, lower discount; land/fixed
    factors often GAIN — complementary capital inflow + lower discount). New bonds issue
    at par at r′ — no price event, pure yield for new lenders; economy-wide interest
    income declines only as old paper rolls over. Financial claims are pipes (layer
    principle): price a claim by the real assets under it + the rigidity of its
    contractual flow. Pass-through interests = residual claims on mostly-reproducible
    bundles ⇒ income falls, value doesn't.
  - **Design consequence — the migration leg touches FLOWS ONLY:** scale noncorporate
    capital-income lines (interest at rollover, rents, pass-through returns) by
    λ(t)·burden; touch NO noncorporate asset values — by theorem, not omission. The
    unmodeled residue (bond and land windfall GAINS) is offsetting and noted. The
    migrated slice reaches estates via D11 dissaving (sustained income loss → forcing →
    wealth erosion) — the COMPLETE channel, since no price effect exists to miss.
    Second-order discount-rate tailwind to all asset values (r′ < r): noted, unmodeled.
    _(AMENDED by D15: the noncorporate allocation is (1−κ)·λ(t)·w_norm, with the κ share
    retained permanently on corporate flow lines; the flows-only principle and the
    rent-share markdown floor stand.)_
  - **Equity markdown decays to a rent-share floor:** decompose corporate equity into
    reproducible capital (long-run value returns to replacement cost — the markdown on
    this slice = PV of transitional sub-r returns, decays over the adjustment horizon;
    same markdown-then-recovery shape as D9's temporary-shock result, because the
    quantity margin makes the flow hit temporary) + RENTS (inframarginal, no quantity
    margin, cannot shift: "taxes on margins get shifted; taxes on rents get
    capitalized") — permanent markdown. So the markdown path = full wedge PV at
    announcement, decaying to floor = rent share of the burden. **λ_LR = the
    NORMAL-RETURN share of the corporate base** (NOT the capital-share anchor from the
    earlier draft of this item — corrected): OTA 63% supernormal / TPC 60% ⇒ central
    λ_LR ≈ 0.35–0.40; house VAT convention (50% normal, "Auerbach via Toder") = upper
    sweep corner; λ_LR = 0 (equity-forever) = lower corner. Corollary: the majority of
    the long-horizon estate/wealth erosion is permanent BY THEOREM (rent share) plus
    accumulated D11 dissaving.
  - **Adjustment path: reuse the house VAT old/new vintaging** (`do_capital_adjustment`,
    `src/data/economy.R:193`, called `run.R:505`) instead of a hardcoded T_adj: capital
    lines ramp as λ(t) = λ_LR·(1 − (1−0.057)^t) (NIPA economic depreciation 5.7%;
    new-capital share 44% @10y, 69% @20y, 83% @30y; half-life ≈12y — the reallocation
    clock IS the replacement clock, the same quantity margin as the derivation); interest
    lines ramp on the empirical debt rollover schedule (`resources/debt_maturities.csv`,
    ~fully rolled by yr 10). Both curves, the 50%-normal split, and the 20% pass-through
    capital weight (= `WEALTH_CAP_FLOWS`) are the SAME old/new machinery the VAT channel
    already uses — one vintaging convention model-wide. Caveat (recorded): full-stock
    turnover is a conservative (slow) pace for reallocation — only the equilibrium ΔK_C
    must move and redirected gross investment could do it faster; house consistency
    trumps false precision, and the λ_LR corners bound it.
  - **Under D9 perfect foresight**, the day-one equity markdown = PV of the remaining
    (1−λ(t))-weighted path — i.e., §5's "shifting expectations" wedge, now derived: the
    initial markdown is smaller than the no-migration markdown because migration is
    anticipated.

**Review items still pending walkthrough** (recorded so the session can resume): ~~(2)
P2/consumption-response~~ RESOLVED by D11; ~~(4) buyback-vs-dividend tax character~~
RESOLVED by D12; ~~(5) receipts≈Δπ gate rationale~~ RESOLVED by D13 (incl. the depreciation
economics session); ~~(6) Harberger~~ RESOLVED by D14 (migration ON, flows-only, VAT
vintaging, rent-share floor); ~~(7) stacking order dependence~~ RESOLVED — recorded as §8.13; ~~(8) smaller
annotations~~ RESOLVED — notes placed at §8.4 (gross-input error sign/bound), §3c (Kennedy
cut-vs-hike symmetry), §10 Phase 4 (standalone smear-defect paper); the event-study κ
anticipation note was mooted by D9. **ALL REVIEW ITEMS CLOSED 2026-07-02** — outcomes D9–D14
plus annotations; the item-by-item economics review (organizing principle, consumption/
bathtub interaction, payout character, provision gate + depreciation, Harberger,
housekeeping) is complete.

### Rulings from the formal-model session (FORMAL_MODEL.md walkthrough, 2026-07-02)

Item-by-item session on the formal paper's §10 asks
(`other/corporate_incidence/FORMAL_MODEL.md`; derivations P1–P14 there). All five
resolved; editorial fixes applied in place throughout this document.

- **D15 — Harberger κ-split: the migrated burden is shared by ALL normal capital.**
  (Supersedes D14's noncorporate-only allocation, which is the κ→0 elastic-sink limit
  and is internally inconsistent: the only mechanism that compresses noncorporate yields
  is the common after-tax return falling to ρ′, and that single price necessarily keeps
  a pro-rata share of the compression on corporate normal capital — capital cannot
  escape to an equally-depressed sector. FORMAL_MODEL P13; D14's own "everyone's
  after-tax return falls" already conceded the mechanism. Consistent with the shops'
  conventions, which allocate the capital share across ALL capital income.) Decompose
  λ(t) = σ_N · η(t) (normal-return share × vintaging ramp). Long-run allocation:
  corporate dividend/kg flow factors retain κ·η(t)·w_norm PERMANENTLY; noncorporate
  lines get (1−κ)·η(t)·w_norm, where κ = C-corp share of the economy-wide normal-capital
  stock (~⅓–½ prior; measure from Fed Z.1 in Phase 0 — whether owner-occupied housing
  belongs in the substitutable stock is the main definitional fork and sets the sweep
  corners, κ ∈ {~0.25, ~0.4, ~0.5}). The equity MARKDOWN floor is UNCHANGED at the rent
  share (q-pinning: on the reproducible slice the retained flow compression and the
  lower discount rate cancel exactly — FORMAL_MODEL P14), so the value.*/estate-base
  machinery is identical under D14 and D15; only the flow-factor paths move. The Δρ
  revaluations (bond appreciation at fixed coupons, land gains, the pinning offset
  itself) become a NAMED line in the conservation residual so the identity closes by
  construction. Estate-composition consequence: corporate-equity-heavy portfolios bear
  permanent dividend compression; pt/interest-heavy portfolios bear (1−κ) of what D14
  assigned them.
- **D16 — ΔY_exog column contract (the internal/external criterion).** The generalized
  forcing F = ΔT − ΔY_exog partitions the shocked lines by whether the money crosses
  the household boundary. IN (external income; enters ΔY_exog): div_ord, div_pref;
  txbl_int, exempt_int (on the rollover ramp); rent and pass-through legs at the 0.2
  capital weight. OUT (internal conversions; tax leg only, automatically via ΔT):
  realized gains (kg_*), retirement distributions (txbl_ira_dist, txbl_pens_dist), sale
  proceeds of any marked-down asset — their resource loss is already booked by the
  balance-sheet / gain-state markdown, and counting the flow again double-counts it
  (FORMAL_MODEL P7/P9, the two-pocket lemma). Implementation riders: hard-code the
  partition as a WEALTH_CAP_FLOWS-style constant (single source of truth); compute
  ΔY_exog analytically from the applied scaling factors, never by differencing detail
  files. Accepted corollary, recorded so it is not later mistaken for a sign bug: a
  marked-down-IRA retiree shows F = a small tax-rebate CREDIT and a slight consumption
  rise — the P2 revaluation-MPC-0 doctrine applied consistently.
- **D17 — Temporary-shock persistence (amends the D9 headline gloss).** Only the
  markdown leg vanishes at a legislated sunset; the D11 dissaving accumulated during
  the window persists and COMPOUNDS to death (s·(1−τ)·Σ window cuts, grown forward).
  Decedents dying after expiry are NOT untouched. "The 30-year story requires
  permanence" is true of the markdown leg only; sunsetted corporate provisions retain a
  smaller but permanent estate footprint. Nothing to build — the bathtub state produces
  this automatically once D16's forcing exists; this ruling fixes the documentation and
  the headline framing.
- **D18 — Basis-aware realization scaling: one rule, two entry points.** (Supersedes
  D9's "scale kg flows with the current-year markdown" reduced form.) PRICE margin: the
  markdown hits the SALE VALUE, not the gain — basis is fixed, so the dollar hit to
  realized gains is μ·(kg + kg_lt_basis); scaling kg alone understates by 1/(1−b),
  worst for mid/high-basis lots (the low-basis top tail is nearly exact either way). In
  kg_dynamics runs this is automatic — the state is gain-denominated ($1 of markdown
  debit = $1 of gain), so the D9 state debit stands unchanged; in non-kg runs use the
  per-record form Δkg = ω_kg·[φ_t·kg − μ_t·(kg + kg_lt_basis)] (columns already
  exist). QUANTITY margin (buyback-forced sales tracking after-tax profits — the φ
  term): NOT automatic in either run type (the kg_dynamics realization rule knows MTRs
  and mortality, not payout policy), so the same buyback-weighted φ adjustment applies
  to the realization flow in BOTH kg and non-kg runs — same economics, one rule, two
  entry points. Contrast preserved: the wealth-bathtub applier's kg_lt_basis co-scaling
  is the QUANTITY-type erosion rule and remains correct for that channel; price-type
  shocks are the opposite case (basis fixed).

### Recommended defaults, pending author confirmation (session timed out)

- **P1 — Delta-only doctrine (recommend CONFIRM).** The channel contributes reform deltas
  only; the model never constructs baseline corporate incidence levels (baseline Tax-Data
  incomes/wealth already embody the existing corporate tax). A scenario with no corporate
  change is byte-identical to today. Symmetric for cuts (windfall gains, opposite sign).
  Mirrors the estate-receipts doctrine.
- **P2 — Pure-debit doctrine. CONFIRMED AS AMENDED by D11 (2026-07-02).** The capitalization
  is a pure balance-sheet event: a direct debit to `value.*`/net_worth/kg-state, never routed
  through the bathtub, no consumption response to the valuation loss (revaluation MPC = 0 —
  approximately true at the top tail, where bequests are the residual claimant). AMENDED:
  the final clause ("ΔT⁰ picks up only the individual tax relief") is superseded — the
  bathtub forcing is generalized to net after-tax cash flows F = ΔT − ΔY_exog, so the
  corporate flow leg debits wealth s·D(1−τ) rather than crediting s·R. See D11.
- **P3 — Distribution status quo (recommend CONFIRM; follows from D4).** v1 leaves the
  distribution tables exactly as they are (smear, static-sourced). Redesigning corporate
  burden in distribution — holdings-based allocation, capitalization windfalls, the labor-
  share stance — is a separate future decision with its own grilling.

### Effect on §9 open questions

9.1 (deliverable framing): narrowed by D4 — the v1 deliverable is the revenue mechanism; the
research piece rides on it. 9.2: still open (but D2 says receipts-denominated regardless).
9.3: partially answered by recon (`value.equities` first-class; imputation needed for
dc/db/trusts/re_fund equity shares). 9.4: mooted for v1 by D4. 9.5: resolved by D1 (input
defined gross). 9.6: resolved by D9 + D12 (no κ; perfect foresight; composition
data-embedded). 9.7: RESOLVED
in the formal-model session — under D5 the shock never touches the static pass, so static
estate totals are unchanged and the heir-allocator pin is untouched. 9.8: resolved by D13 (assert
rate-type, labeled limitation; depreciation permanently separate).

---

## 12. Primary sources

- JCT, *Modeling the Distribution of Taxes on Business Income*, JCX-14-13 (Oct 2013).
  https://www.jct.gov/publications/2013/jcx-14-13/
- JCT, *The Joint Committee on Taxation Revenue Estimating Process* (Feb 2021).
  https://www.jct.gov/getattachment/99eb52ef-7ad3-449d-921c-ebeb92abe5c4/Revenue-Estimating-Process-February-2021.pdf
- JCT, *Income and Payroll Tax Offset to Changes in Excise Tax Revenues*, JCX-2-23.
  https://www.jct.gov/publications/2023/jcx-2-23/ ; CBO companion: pub 58549 (Dec 2022).
- CBO, *The Distribution of Household Income, 2018* (Aug 2021), methodology pp. 42, 48–50.
  https://www.cbo.gov/publication/57404 ; convention adopted in pub 43373 (July 2012).
- Treasury OTA, Technical Paper 5 (Cronin, Lin, Power, Cooper, May 2012).
  https://home.treasury.gov/system/files/131/TP-5.pdf ; 2021 methodology summary:
  https://home.treasury.gov/system/files/131/Summary-of-OTA-Distribution-Methodology-05102021.pdf
- Nunns, *How TPC Distributes the Corporate Income Tax* (Sept 2012).
  https://www.urban.org/sites/default/files/publication/25796/412651-How-TPC-Distributes-the-Corporate-Income-Tax.PDF
- Rosenthal & Austin, "The Dwindling Taxable Share of U.S. Corporate Stock," *Tax Notes*
  (May 16, 2016). https://www.urban.org/sites/default/files/publication/80621/2000790-The-Dwindling-Taxable-Share-of-U.S.-Corporate-Stock.pdf
- Viard, "Tax Increases and the Price Level," *Tax Notes* (Jan 6, 2014).
  https://www.aei.org/wp-content/uploads/2014/01/-viard-tax-increases-and-the-price-level_091340576369.pdf
- Auerbach, "Who Bears the Corporate Tax? A Review of What We Know," NBER WP 11686 (2005).
  https://www.nber.org/papers/w11686
- Kennedy, Dobridge, Landefeld & Mortenson, "The Efficiency-Equity Tradeoff of the Corporate
  Income Tax" (2024). https://patrick-kennedy.github.io/files/TCJA_KDLM_2024.pdf
- Wagner, Zeckhauser & Ziegler, "Unequal Rewards to Firms: Stock Market Responses to the
  Trump Election and the 2017 Corporate Tax Reform," *AEA P&P* 108 (2018).
- Gravelle, *Corporate Tax Incidence: Review of General Equilibrium Estimates and Analysis*,
  CBO WP 2010-03. https://www.cbo.gov/publication/21486
- Gale & Thorpe, *Rethinking the Corporate Income Tax: The Role of Rent Sharing* (2022).
  https://www.brookings.edu/wp-content/uploads/2022/05/Rethinking-the-Corporate-Income-Tax-Formatted.pdf
- PWBM, *The House TCJA: Static Distributional Analysis* (2017) and *Dynamic Distributional
  Analysis of the Biden Platform* (2020). https://budgetmodel.wharton.upenn.edu/
