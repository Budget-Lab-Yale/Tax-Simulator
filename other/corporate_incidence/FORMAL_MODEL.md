# The corporate–individual channel: a formal model

_Technical documentation for the on-model corporate incidence design (CONSIDERATIONS.md,
rulings D1–D14). Drafted 2026-07-02. Status: DERIVATION / VERIFICATION. This paper states
the assumptions behind the design as numbered primitives, derives the claimed results as
propositions with proofs, and reports where the claims are theorems, where they need
qualification, and where the derivation surfaces a genuine inconsistency. Verdicts are
collected in §9; items needing a new author ruling are in §10._

_UPDATE (2026-07-02, later same day): all five §10 asks were ruled in an author
walkthrough session and are recorded as D15–D18 in CONSIDERATIONS §11 (item 4 was
editorial and is applied in place there). §10 below carries the outcomes inline._

_Everything is written in model-native objects: the record columns (`div_ord`, `kg_lt`,
`kg_lt_basis`, `value.*`, `txbl_ira_dist`, `net_worth`, `estate_m`), the wealth-bathtub
state and kernel (`src/sim/wealth_dynamics.R`), the VAT vintaging machinery
(`src/data/economy.R:193`), and the receipts/distribution plumbing
(`src/data/post_processing/{revenue,distribution}.R`)._

---

## 1. Frame and primitives

### 1.1 The conventional frame

**(F1) Fixed nominal GNP.** Aggregate pre-tax factor income is unchanged by the reform.
For a corporate rate change this pins the *pre-tax* profit path: the tax comes out of the
residual claim (after-tax profits), which is nominally flexible (Viard 2014, §4 of
CONSIDERATIONS). No price-level machinery: the shock changes nominal incomes of
identified equity holders, not the price level. Wages are fenced (bucket 2, Viard fn. 15).

**(F2) Perfect foresight of the legislated path (D9).** Agents and markets know the
scenario's statutory rate path $\{\tau_t\}$ as written, including sunsets. This is the
scoring convention, not an empirical claim; the "priced-as-permanent" corner is a
sensitivity, not the central case.

**(F3) Conventional-pass placement (D5).** The shock applies on conventional-side passes
only (including conv-no-wealth). Static stays the clean individual-law counterfactual;
distribution tables (static-sourced) and the smear are untouched (D4). The
corporate interaction is read off conventional − static.

**(F4) Delta-only doctrine (P1).** The channel contributes reform deltas only. Baseline
Tax-Data already embodies current-law corporate incidence; a scenario with no corporate
change is byte-identical to today.

### 1.2 Firm-side objects and the eligibility gate

Let $\Pi_t$ be aggregate US-taxable C-corp profit (pre-tax), $\tau_t$ the statutory rate
path, and

$$\pi_t = (1-\tau_t)\,\Pi_t$$

aggregate after-tax domestic profit. A reform changes the rate path by $\Delta\tau_t$;
under (F1) the pre-tax path is unchanged at impact, so the **wedge** is

$$w_t \;=\; \Delta\tau_t\,\Pi_t, \qquad \Delta\pi_t = -\,w_t .$$

**Definition (D13 gate).** A corporate provision is *eligible* for the on-model channel
iff its conventional receipts path is a valid year-by-year proxy for its after-tax profit
path:

$$\Delta \mathrm{Rev}^{corp}_t \;=\; -\,\Delta\pi_t \quad \forall t. \tag{G}$$

For a rate change on a fixed base this holds identically — and it holds *net of
corporate-side avoidance*, because whatever avoidance does to the base, receipts and
after-tax profits move dollar-for-dollar in opposite directions given (F1). §5 shows
depreciation fails (G) in both timing and sign; transition/one-time levies fail it
structurally (a stock levy is not a recurring profit change).

**Gross-input requirement (D1), formalized.** The channel books an endogenous individual
offset $O'_t$ on top of the corporate input $R_t$. If the supplied input is
JCT-benchmark-net, $R_t = R^{gross}_t - O_t$ (Nunns), then total booked revenue is
$R^{gross}_t - O_t + O'_t \approx R^{gross}_t$ **minus the double-counted offset**
$O_t$: combined revenue is understated, bounded by $|O_t|$ (a few percent of the
corporate estimate given the ~24% taxable share). The input must be gross by
construction, or the bound must be stated.

### 1.3 Household-side objects

Records $i$ with weights $w_i$. The exposure of each model column to C-corp equity is a
measured vector $\omega$ (Phase 0 recon content), not a free parameter:

| symbol | content | model column(s) |
|---|---|---|
| $\omega_{div}$ | C-corp share of dividends (direct + via funds; excludes REIT/bond-fund distributions) | `div_ord`, `div_pref` |
| $\omega_{kg}$ | C-corp equity share of realized LTCG (~0.5 per SOI sale-of-capital-assets) | `kg_st`, `kg_lt`, `kg_lt_basis` |
| $\omega_{a}$ | equity share of asset column $a$ | `value.equities` (≈1), imputed shares of `value.dc`, `value.trusts`, `value.re_fund` |
| — | DB pensions | to the residual (D10), never on records |

The **residual** (foreigners, nonprofits, DB sponsors) is the unallocated remainder of
the mapping (D3): no gross-up forces household hits to sum to the revenue line.

Two aggregate factors drive every record-level transformation:

- the **flow factor** $\phi_t$ — the proportional change in after-tax profits reaching
  equity payouts (defined in §2.1, migration-adjusted in §6);
- the **markdown** $\mu_t$ — the proportional equity price discount (defined in §2.2,
  floor derived in §6).

---

## 2. Asset valuation under perfect foresight (the D9 block)

### 2.1 Pricing assumptions

**(A1) PV pricing.** Equity value is the present value of distributions
(dividends + net repurchases): with a constant nominal discount rate $r$,
$$V_t = \sum_{s>t} \frac{X_s}{(1+r)^{s-t}}.$$

**(A2) Constant $r$.** The reform does not move the discount rate or risk premium.
(§6 relaxes this in exactly one place — the Harberger general-equilibrium return
$\rho' < \rho$ — and the interaction is load-bearing there; see P13.)

**(A3) MM payout irrelevance + fixed composition (D8/D12).** Total distributions track
after-tax profits with a fixed dividend/buyback/retention composition — the composition
embedded in the microdata. Hence every distribution margin scales with the same factor:

$$\phi_t \;=\; \frac{\Delta\pi_t}{\pi_t} \;=\; \frac{(1-\tau'_t) - (1-\tau_t)}{1-\tau_t}
\;=\; -\,\frac{\Delta\tau_t}{1-\tau_t}.$$

For a hike, $\phi_t<0$. Record level: `div_ord`, `div_pref` scale by
$(1+\omega_{div}\,\phi_t)$ — "dividends track the current-year statute."

### 2.2 Permanent shocks

**P1 (markdown = flow factor; the §5 ceiling).** For a permanent rate change
$\tau \to \tau'$, every future distribution scales by $(1+\phi)$, so by (A1)
$V \to (1+\phi)V$: the proportional markdown is

$$\mu \;=\; -\phi \;=\; \frac{\Delta\tau}{1-\tau},$$

scaled by exposure at the record level. This is §5's "naive benchmark" — exact under
(A1)–(A3) and full US-taxable exposure; the real-world wedges (§5's list) enter through
the measured $\omega$'s and through migration (§6), not through a free κ. ∎

**P2 (yield preservation).** Under P1, $D'/V' = (1+\phi)D / (1+\phi)V = D/V$. The
dividend yield is unchanged — a theorem given proportional scaling and constant $r$, as
D9 claims. ∎

### 2.3 Temporary shocks

Let the shock cut distributions by $d_s = m\,X_s$ for $s \in \{1,\dots,n\}$ only
(legislated sunset, known under (F2)).

**P3 (markdown path).** The markdown at year $t$ is the PV of the *remaining* cuts:

$$M_t \;=\; \sum_{s=t+1}^{n} \frac{d_s}{(1+r)^{s-t}}, \qquad
\mu_t = M_t / V_t,$$

which is largest at announcement and reaches zero at expiry $t=n$. With distributions
growing at $g$, the closed form at announcement is

$$\mu_0 \;=\; m\left[1 - \left(\tfrac{1+g}{1+r}\right)^{n}\right],$$

the "annuity share" of D9. ∎

**P4 (holders earn $r$; burden = announcement loss).** For any $t < n$, the one-period
return on the post-shock asset is exactly $r$:

$$\frac{V^{post}_{t+1} + X^{post}_{t+1} - V^{post}_t}{V^{post}_t} = r,$$

since $V^{post}_t = V_t - M_t$ and $M_t(1+r) = d_{t+1} + M_{t+1}$ (the telescoping
recursion of PV). Holders during the window receive low distributions plus above-normal
recovery appreciation summing to $r$; the entire pre-tax burden is the one-time capital
loss $M_0$ to whoever holds at announcement — Auerbach's old-capital result,
operationalized. ∎

**Caveat to D9's estate gloss.** D9 concludes "decedents dying after expiry are
untouched." That is true of the *markdown leg* (P3: $M_t = 0$ for $t \ge n$) but — after
D11 generalized the bathtub forcing — **false for the flow leg**: the during-window
dissaving $s\,(1-\tau)d_t$ accumulates in the bathtub state and compounds *past expiry*
(§3.6). Temporary shocks do have a (smaller) long-horizon estate effect. See P10 and
verdict V10.

### 2.4 The unrealized-gain state: basis invariance and realization reduced forms

Let a holding have value $V$, basis $B$, basis share $b = B/V$, unrealized gain
$U = V - B$.

**P5 (basis invariance and gain leverage).** A markdown $\mu$ moves price, not basis
(§8.5): $\Delta U = \Delta V = -\mu V$, so the *proportional* hit to the gain is

$$\frac{|\Delta U|}{U} = \frac{\mu}{1-b} \;\ge\; \mu,$$

with equality only at $b=0$. Every holder's gain falls more than proportionally to the
value hit; the amplification factor $1/(1-b)$ is *increasing in the basis share* —
high-basis holders lose gains most in proportional terms, while low-basis (top-tail)
holders lose the most *dollars* of gain because they hold the gains. (§8.5's phrasing
"low-basis holders lose accrued gains more than proportionally" conflates the two; the
mechanism it protects — debit the gain state, never scale basis — is exactly right.) ∎

**In kg_dynamics runs**: debit the unrealized-gain state by $\omega_{kg}\,M_t$ at
enactment and credit it back as $M_t$ shrinks (P3's recovery appreciation *is* the
credit-back). Deferral and step-up then come for free from the existing machinery.

**P6 (realization reduced forms; the D12 blend).** Realized gains have two drivers with
different scaling:

- *Buyback-driven (quantity margin).* Repurchase dollars scale with after-tax profits:
  $R' = (1+\phi)R$. Each dollar of sale carries gain share $1 - b/(1-\mu)$ (price down,
  basis fixed). Exactly:
  $$\frac{G'}{G} = (1+\phi)\,\frac{1 - b/(1-\mu)}{1-b}
  \;\approx\; 1 + \phi - \mu\,\frac{b}{1-b} \quad \text{(first order)}.$$
  The dominant term tracks the *current-year statute* $\phi$ (D12 ✓); the correction is
  the gain-slice compression.
- *Appreciation-driven (price margin).* Realizations proportional to the gain stock scale
  by P5: factor $1 - \mu/(1-b)$ — they track the *markdown path*.

A single kg factor blends the two; the blend error is
$\propto (\phi_t + \mu_t)$, which is **zero for permanent shocks** (there
$\mu_t = -\phi_t = m$ for all $t$) and second-order for temporary ones — confirming and
quantifying D12's "accepted approximation."

**Implementable exact form.** Because `kg_lt_basis` is a record column, the
appreciation-driven leg needs no approximation: per record,

$$\Delta kg \;=\; \omega_{kg}\left[\phi_t\,kg \;-\; \mu_t\,(kg + basis)\right]
\quad\text{(first order, both margins)},$$

using $-\mu(kg+basis) = -\mu V_{sold}$, the exact price-times-quantity hit on the sold
lot. Recommended over the flow-factor-only reduced form for non-kg runs. ∎

### 2.5 Retirement accounts: the two-pocket lemma

**P7 (internal-transfer invariance).** Let $B$ be a retirement balance inside household
wealth $A$, with distributions $\delta_t = \rho B_t$ (proportional draws). A markdown
$\mu$ on $B$ scales all future distributions by $(1-\mu)$; conversely, if distributions
did *not* scale, the balance markdown would be silently undone by relatively faster
drawdown. Consistency requirement (this is §8.7 / Phase 1, formalized): **every cash
flow sourced from a marked-down stock must scale with the markdown.**

Moreover a distribution is a transfer between the household's own pockets
($B \to$ cash): its only *net* cash-flow content at the total-wealth level is the tax on
it. The pre-distribution balance markdown $\mu B_t$ already contains the current year's
distribution cut $\mu\rho B_t$ plus the remaining markdown $\mu(1-\rho)B_t$ — counting
the distribution cut *again* as an income shock double-counts. Consequence for the
bathtub forcing in §3.3. ∎

Contrast with dividends: the model's `value.*` stocks are year-end (ex-distribution)
values, and a dividend is income *from the corporate sector into* $A$, not a draw on a
pocket of $A$. The year-$t$ dividend cut is therefore **not** inside the year-end
markdown (which prices cuts strictly after $t$) — dividends are a genuine external cash
flow. P9 confirms the two treatments jointly conserve the total burden.

---

## 3. The household budget identity and the bathtub (the D11 block)

### 3.1 Accounting and the taxonomy

Household wealth $A$ evolves by

$$A_{t+1} \;=\; (1+r)A_t \;+\; \kappa_t \;+\; Y_t \;-\; T_t \;-\; C_t,$$

where $\kappa_t$ = accrual revaluations (price margin: $p$ moves, holdings $n$ don't; no
cash moves), $Y_t$ = external cash income (dividends, interest, rents, pass-through
returns, wages…), $T_t$ = taxes (a function of *realized* flows), $C_t$ = consumption.
Internal conversions (asset sales, retirement distributions) do not appear: they move
value between pockets of $A$ and enter only through $T$.

**(B1) Revaluation MPC = 0 (P2-as-amended).** $\Delta\kappa$ shocks hit the balance
sheet directly; no consumption response. Defensible at the top tail, where bequests are
the residual claimant; weakest for distribution-dependent retirees (noted, accepted).

**(B2) Cash-flow MPC = $1-s$.** Consumption absorbs $(1-s)$ of any net after-tax
*exogenous* cash-flow shock; the rest is financed out of wealth. One parameter, one rule.

**(B3) Behavioral responses stay out.** Chosen responses (kg realization timing, labor
supply) are not financing shocks; they belong to behavior modules, not the forcing.

### 3.2 The generalized forcing and the sign theorem

Define the exogenous net cash-flow shock and the forcing

$$F_t \;=\; \Delta T_t \;-\; \Delta Y^{exog}_t.$$

Applying (B2) to the budget identity, the wealth deviation (deficit $P>0$ = wealth
below baseline) obeys

$$P_{t+1} \;=\; (1+r)\,P_t \;+\; s\,F_t,$$

which is exactly the model recurrence (`cohort_recurrence_step`,
`src/sim/cohort_bathtub.R:248`; inflow at face value end-of-year, D24) with the kernel
feedback discussed in P11.

**P8 (sign theorem — D11's fix, verified).** Corporate flow leg per household: dividends
fall by $D$, taxes fall by the rebate $R = \tau D$. Then

$$F = \Delta T - \Delta Y^{exog} = (-\tau D) - (-D) = D(1-\tau) \;>\; 0:$$

wealth is *debited* $s\,D(1-\tau)$ — the household dissaves to defend consumption.
The tax-only forcing $F = \Delta T = -\tau D < 0$ would *credit* wealth $s\tau D$
(household appears to profit from the hike). The generalization is necessary and
sufficient to fix the sign, and it is numerically identical to the current
implementation for every existing scenario ($\Delta Y^{exog} = 0$ to date). ∎

### 3.3 What enters $\Delta Y^{exog}$: the internal/external criterion

**P9 (line assignment).** By the budget identity and P7, the forcing must partition
model columns as follows — this is implementation-critical and CONSIDERATIONS does not
state it:

| enters $\Delta Y^{exog}$ (external income) | does NOT enter (internal conversion — tax leg only, via $\Delta T$) |
|---|---|
| `div_ord`, `div_pref` cuts | realized-gain cuts (`kg_*`) — the resource loss is the `value.*`/gain-state markdown |
| interest cuts at rollover (`txbl_int`, `exempt_int`) | retirement-distribution cuts (`txbl_ira_dist`, `txbl_pens_dist`) — the loss is the balance markdown (P7) |
| rent and pass-through cuts (Harberger leg, §6; capital weight 0.2 per `WEALTH_CAP_FLOWS`) | sale proceeds of any marked-down asset |

Putting a kg or retirement-distribution cut into $\Delta Y^{exog}$ double-counts the
markdown; leaving a dividend/interest/rent cut out reintroduces the P8 sign error on
that line. Since the corporate scaling factors are analytic, $\Delta Y^{exog}$ should
be computed exactly from the pre/post-scaling column deltas, not measured. ∎

Corollary (retiree case): with distributions excluded, a marked-down IRA produces
$F = -\tau\,|\Delta\delta| < 0$ — a small *credit* (the rebate) — and consumption
*rises* by $(1-s)\tau|\Delta\delta|$ while the full resource loss sits in the markdown.
That is the consumption-smoothing content of (B1), stated so nobody mistakes it for a
sign bug later.

### 3.4 The decomposition theorem: no double-count, and what $s$ does

**P10 (burden decomposition).** Household holds equity with baseline distributions
$D_t$; a shock cuts them by $d_t$ (any path — permanent or windowed), taxed at rate
$\tau$; the markdown obeys P3; the bathtub runs with (B1)–(B2). Let death occur at $T$.
Then, discounting at $r$:

- consumption absorbs $\;(1-s)(1-\tau)\sum_{t\le T} d_t/(1+r)^t$,
- the bathtub debit at death is $\;\Delta W_T = -\,s(1-\tau)\sum_{t\le T}(1+r)^{T-t}d_t$,
- the markdown at death is $\;M_T = \sum_{t>T} d_t/(1+r)^{t-T}$,

and the total household burden in PV is

$$\underbrace{(1-\tau)\sum_{t\le T}\frac{d_t}{(1+r)^t}}_{\text{during-life, split }(1-s)/s
\text{ between consumption and estate}}
\;+\; \underbrace{\sum_{t> T}\frac{d_t}{(1+r)^t}}_{\text{post-death cuts} \;=\; M_T
\text{ discounted}}
\;=\; M_0 \;-\; \tau\!\!\sum_{t\le T}\frac{d_t}{(1+r)^t},$$

i.e. **the announcement capital loss minus the PV of tax rebates, independent of $s$.**
The markdown leg and the dissaving leg partition the same total across time with no
overlap (the year-$t$ markdown prices only cuts after $t$; the year-$t$ cash shortfall
is the year-$t$ cut) — the no-double-count theorem, by budget identity, under any MPC.
$s$ allocates the during-life share between lifetime consumption and the estate; it does
not change the burden. ∎

Two consequences worth stating:

1. **Temporary shocks** ($d_t = 0$ for $t > n$): $M_T = 0$ for deaths after expiry, but
   the dissaving term $\Delta W_T$ persists and *compounds* for all $T > n$. The
   long-horizon estate erosion of a windowed shock is
   $s(1-\tau)\sum_{t\le n}(1+r)^{T-t}d_t$ — not zero. (Corrects the D9 gloss; see §2.3.)
2. Both legs hit `net_worth` → wealth-tax and estate bases; the theorem guarantees their
   sum is the right object, so implementing "markdown + haircut applier" is not
   double-charging the estate.

### 3.5 Kernel consistency and the second-round interaction

**P11 (the kernel is the forcing rule applied to its own feedback).** The implemented
kernel (`wealth_dynamics.R:881`)

$$G_t = (1+r_{total}(t)) - s\,(\tau\,y + \tau_w)$$

expands the recurrence to

$$P_{t+1} = (1+r_{total})P_t \;+\; s\big[F_t \;\underbrace{-\,(\tau y + \tau_w)P_t}_{
\Delta T^{endog}:\ \text{tax forgone on the missing wealth}}\big] :$$

the drag term is precisely the endogenous tax *relief* generated by the deficit itself
(missing capital income $y P$ taxed at $\tau$, missing wealth taxed at $\tau_w$), run
through the same $s$-split as any other cash flow. D11's "one parameter, one rule"
extends to the kernel — a consistency result, not an assumption. Note the boundary of
(B3): *mechanical* endogenous tax consequences of the state are in (measured via
`mtr_cap_bundle`); *behavioral* responses are out. ∎

**P12 (what breaks exact $s$-invariance, and why that's the point).** P10's invariance
assumed wealth compounds at the discount rate $r$. In the implementation
(i) $r_{total}$ is nominal GDP-per-capita growth (the house cohort-wealth convention),
not an asset return, and (ii) the drag makes the effective growth $G < 1+r_{total}$.
The gross burden remains $s$-invariant (P10); the *split* between household and Treasury
does not: a larger $s$ parks more of the burden in wealth, generating more forgone
future tax ($\tau y + \tau_w$ on $P$) — Treasury's long-horizon take falls with $s$.
That $s$-dependent second-round term **is the measured interaction the channel exists to
quantify**; it is a feature of the accounting, not a violation of the theorem. The
$r_{total}$-vs-$r$ gap is a genuine (small) approximation: the estate leg compounds at
GDP/capita growth rather than at portfolio returns; conservative when returns exceed
nominal growth. ∎

---

## 4. The conservation identity (the model invariant)

Per calendar year $t$, in CY liability space (before FY conversion), the wedge allocates
exactly:

$$w_t \;=\; \underbrace{\Delta \mathrm{Rev}^{corp}_t}_{\text{Treasury, gross input}}\,,
\qquad
\Delta\pi_t \;=\; -\,w_t \;=\;
\underbrace{\sum_i w_i\,\big[\Delta div_i + \Delta int_i + \Delta rent/pt_i\big]}_{
\text{household flow hits (}\Delta Y^{exog}\text{)}}
\;+\; \underbrace{\Delta \mathrm{Accr}^{hh}_t}_{\substack{\text{household accrual hits:}\\
\text{gain-state debit / markdown flow}}}
\;+\; \mathrm{Residual}_t,$$

with $\mathrm{Residual}_t \ge 0$ the foreign/nonprofit/DB slice (D3/D10) — reportable as
"borne outside the household sector." Because the flow and accrual legs are the same
object measured on different margins (P4/P10), the PV statement closes:

$$\mathrm{PV}(\text{household burden net of rebates}) + \mathrm{PV}(\mathrm{Residual})
= \mathrm{PV}(w) - \mathrm{PV}(\text{endogenous individual offset}),$$

where the offset is what the calculator produces on the conventional pass (D1: never a
special line — it materializes in ordinary receipts deltas, plus the estate / wealth-tax
/ deemed legs in death years). Check it allocator-style (the estate allocator's
$\Sigma w\cdot p\cdot\lambda$ precedent): per year, per leg, hard-error on violation
beyond tolerance. Under D14 the identity needs one explicit residual note (P13's
$\Delta\rho$ revaluations). FY booking wraps the CY identity afterward: corporate input
at 0.75/0.25 (`revenue.R:140`), estate and wealth at FY+1 (`revenue.R:147,160`) — run
one year past the window, as with the wealth channel.

---

## 5. Depreciation (the D13 block)

### 5.1 Setup

A dollar of new investment carries depreciation deductions with PV $z$ per dollar
(discounted at the firm's rate), so its effective acquisition cost is $1 - \tau z$.
Competitive entry drives new investment to the zero-NPV margin:

$$\mathrm{PV}\big[(1-\tau)\,q\text{-rents}\big] \;=\; 1 - \tau z. \tag{Z}$$

A reform moves $z \to z' = z + \Delta z$ (bonus, expensing, R&D amortization…).

### 5.2 The receipts seesaw vs the profit path

Receipts change by the *timing shift* of deductions on affected vintages:
$\Delta\mathrm{Rev}_t = -\tau\,\Delta(\text{deductions taken})_t \cdot I$, negative in
early years, reversing later (e.g. `01_bonus`: −$61B in 2026 → +$35B by 2030). Economic
after-tax profits barely move: the entire economic content is the *time value* of the
deferral,

$$\Delta\pi^{econ} \;\approx\; r\,\tau\,\Delta z \cdot I \quad\text{per vintage-year,}$$

a small, smooth flow against a large, sign-flipping receipts path. **Depreciation fails
gate (G) in timing** — feeding its receipts path into the D9 machinery would price a
seesaw as a profit path. ∎

### 5.3 The old-capital revaluation: the sign theorem

**P13′ (Tobin-q arbitrage).** Post-reform, a new unit of capacity costs $1 - \tau z'$
net. An installed (grandfathered) unit with the same capacity and no remaining
excess deductions must trade at replacement parity:

$$q^{old} \;=\; 1 - \tau z' \;=\; (1 - \tau z) - \tau\,\Delta z:$$

**more generous depreciation devalues existing capital by $\tau\Delta z$ per dollar of
installed reproducible capital.** A receipts *cut* is a shareholder *loss* — the sign is
flipped relative to the rate case, where a receipts cut is a shareholder gain. Feeding
depreciation receipts into the D9 channel is wrong in sign, not just size (D13 ✓).
Symmetric in reverse: stingier depreciation is a windfall *gain* to old capital
(Auerbach's TRA86 9–14% windfall). The Gale–Auerbach chain (expensing = cash-flow tax =
business half of a consumption tax) makes each step toward expensing a one-time relative
levy on old wealth. Attenuations, also structural: only *reproducible* capital reprices
(rents and intangibles — most of C-corp equity value — do not compete with subsidized
new machines), and temporary bonus has small $\Delta z$ in PV. Known, small,
sign-flipped, deliberately unmodeled — recorded so nobody "fixes" it later with the
rate-case sign. ∎

### 5.4 The existing infrastructure as a PV booking (blessed)

The current split is conceptually correct and should never be collapsed:

- **Receipts** take the timing seesaw — right for Treasury cash
  (`revenue.R:59-62,172-173`; ccorp + pass-through combined).
- **Distributed burden** is independently recomputed as the time-value transfer:
  `distribution.R:600` books $I \cdot \tau \cdot \Delta z$ (investment ×
  $\Delta$PV(deductions) × `corp.rate`) per year of investment — the full PV of each
  vintage's benefit at purchase. TPC's level-annuity spreads the same PV over the asset
  life; the two integrate identically and differ only in booking year. The 50/50
  labor/capital allocation of that burden is a smear **forced by the economics**: the
  new-capital benefit has no record-level owner (zero-NPV at the competitive margin,
  competed into prices/wages/expansion). Caveat for someday: rent capture on
  inframarginal investment (Kennedy et al.) implies partial shareholder capture.
- **Pass-through leg** (named v2): the pt slice of the interface delta is individual
  income tax booked on the corporate line, distributed to no one; it has record-level
  claimants (Schedule C/E/F, K-1) and an existing interface path
  (`recovery_ratios_form.csv`).

---

## 6. Harberger migration (the D14 block)

### 6.1 Two-sector equilibrium

Corporate sector produces with reproducible capital $K_c$ plus a fixed rent flow $R$
(inframarginal: IP, market power); noncorporate with $K_n$; total stock
$\bar K = K_c + K_n$ fixed (F1 horizon). After-tax arbitrage on the marginal unit:

$$(1-\tau)\,f_c'(K_c) \;=\; f_n'(K_n) \;\equiv\; \rho.$$

A hike $d\tau$: capital exits the corporate sector until returns re-equalize at
$\rho' < \rho$ — corporate pre-tax returns rise via scarcity, noncorporate pre-tax
returns fall via crowding, everyone's after-tax return falls. Pure reallocation of a
fixed stock = a composition effect inside fixed GNP (JCT-sanctioned; bucket 1). The
labor leg (smaller *total* stock) stays bucket-2 fenced.

**Burden decomposition.** Split the wedge $w_t = w^{rent}_t + w^{norm}_t$ with
$\sigma_N = w^{norm}/w$ the normal-return share ("taxes on margins get shifted; taxes on
rents get capitalized" — the rent slice has no supply margin and cannot migrate).
Anchors: OTA 63% / TPC 60% supernormal ⇒ $\sigma_N \approx 0.35$–$0.40$ central; house
VAT convention (50% normal, Auerbach via Toder) = upper corner; $\sigma_N = 0$ = lower.

**P13 (steady-state allocation — the κ factor).** In the migrated equilibrium the
normal-return burden is borne by *all* normal capital through the common compression
$\rho - \rho'$:

$$(\rho - \rho')\,\bar K^{norm} \;=\; w^{norm}, \qquad\text{so corporate equity retains}
\;\; \kappa\, w^{norm}, \;\; \kappa = \frac{K_c^{norm}}{\bar K^{norm}},$$

and the noncorporate sector receives $(1-\kappa)\,w^{norm}$ — **not** $w^{norm}$.
D14's rule "scale noncorporate capital-income lines by $\lambda(t)\cdot$burden" is the
$\kappa \to 0$ limit (noncorporate as an infinitely elastic sink), and that limit is not
internally consistent: the *only* mechanism that compresses noncorporate yields is
$\rho' < \rho$, and the same $\rho'$ necessarily keeps the $\kappa$-share of the
compression on corporate normal capital. Either both appear or neither does. With
$\kappa \approx$ the C-corp share of the private normal-capital stock (~⅓–½,
measurement item), the correction is first-order for the flow allocation:

- corporate flow lines (dividends, kg): long-run hit
  $\;w^{rent} + \kappa\,w^{norm}$, not $w^{rent}$;
- noncorporate lines: long-run hit $\;(1-\kappa)\,w^{norm}$, not $w^{norm}$. ∎

**P14 (the pinning theorem and the markdown floor — unchanged).** Asset-class pricing:

- *Reproducible capital* (elastic supply): price pinned at replacement cost by
  construction arbitrage; the burden arrives entirely as yield compression. At the new
  margin $q = 1$: $\mathrm{PV}_{\rho'}(\text{compressed flows}) = $ replacement cost —
  the flow compression and the lower discount rate cancel *exactly* for the normal
  slice. Hence the retained $\kappa\,w^{norm}$ of P13 shows up in **flows but not in the
  markdown**: the equity markdown still decays to the **rent-share floor**
  $w^{rent}$, exactly as D14 states. The reconciling item is the $\Delta\rho$
  revaluation — which is therefore *not* ignorable bookkeeping: it is first-order equal
  to the retained corporate flow compression, and its analogs on other assets (bond
  appreciation at fixed coupons, land gains from $\rho'$ + complementary capital) are
  the same object. Recommended treatment: values untouched (flows-only, by theorem),
  with the $\Delta\rho$ revaluations booked as a named line in the conservation
  residual so the §4 identity closes by construction rather than approximately.
- *Fixed-coupon claims*: price appreciates ($\rho'$ discount on fixed coupons); income
  unchanged until rollover — interest lines ramp on the empirical debt-maturity schedule
  (`resources/debt_maturities.csv`, ~fully rolled by year 10), the exact contract-
  rigidity content of the theorem.
- *Land/fixed factors*: often *gain* (lower discount + complementary inflow) — the
  offsetting residue, unmodeled, now booked in the residual note above.
- *Pass-through interests*: residual claims on mostly-reproducible bundles ⇒ income
  falls, value doesn't — flows-only at the `WEALTH_CAP_FLOWS` 0.2 capital weight;
  `value.*` pt columns untouched, which also keeps the frozen estate-valuation bridge
  ($\rho_{pt}$, $s_{pt}$) invariant. ∎

### 6.2 The adjustment path and the perfect-foresight markdown

Vintaging (house convention, reused from `do_capital_adjustment`,
`src/data/economy.R:193`): the reallocation clock is the replacement clock,

$$\eta(t) \;=\; 1 - (1-\delta)^t, \qquad \delta = 0.057 \;(\text{NIPA}),$$

(44% at 10y, 69% at 20y, 83% at 30y; half-life ≈ 12y; conservative — only $\Delta K_c$
must move — bounded by the $\sigma_N$ corners). D14's single curve
$\lambda(t) = \sigma_N\,\eta(t)$ conflates the share and the ramp; keeping them separate
makes the P13 correction explicit. The corporate equity *flow* hit path and markdown are

$$h^c_t \;=\; w^{rent}_t \;+\; w^{norm}_t\big[(1-\eta(t)) + \eta(t)\,\kappa\big],
\qquad
M_t \;=\; \mathrm{PV}_t\Big[w^{rent}_s + (1-\kappa)\,(1-\eta(s))\,w^{norm}_s\Big],$$

(the price-relevant hits exclude the $\Delta\rho$-offset $\kappa$-slice, P14), and the
noncorporate flow allocation is $(1-\kappa)\,\eta(t)\,w^{norm}_t$ across interest
(rollover ramp), rents, and pass-through (0.2 weight). Under perfect foresight the
announcement markdown is the PV of the *remaining* decaying path — smaller than the
no-migration markdown because migration is anticipated (§5's "shifting expectations"
wedge, derived); it decays to the rent-share floor either way, since
$(1-\kappa)(1-\eta) \to 0$. The migrated slice reaches estates *only* through D11
dissaving on the affected income lines (P9's external column) — the complete channel,
since no price event exists to miss (P14).

Estate-composition consequence of P13: relative to the D14 spec, corporate-equity-heavy
portfolios bear more of the long-run flow burden (persistent dividend compression) and
pass-through/interest-heavy portfolios bear $(1-\kappa)$ of what the spec assigns them.
Direction of the net estate-revenue effect depends on top-tail portfolio composition —
a Phase 0a column worth adding.

---

## 7. The shock pipeline (assembled)

Per scenario, per conventional-pass year (identical transformation on conv-no-wealth and
final conventional; head of `run_one_year`, before behavior modules, before the wealth
haircut applier so kg/wealth machinery run on the shocked frame):

1. **Inputs**: gross corporate receipts delta path (gate (G) asserted, D13);
   aggregates $\Pi_t, \pi_t$ (Macro-Projections/NIPA); measured exposures
   $\omega$, $\sigma_N$, $\kappa$; vintaging curves $\eta(t)$, debt rollover.
2. **Paths** (analytic, once per scenario): $w_t \to h^c_t, M_t/\mu_t$, noncorporate
   allocation, per §6.2.
3. **Flows**: `div_ord`, `div_pref` × $(1 + \omega_{div}\,\phi^c_t)$ where
   $\phi^c_t = -h^c_t/\pi_t$; interest lines on the rollover ramp; rent/pt lines at the
   0.2 capital weight — all recorded as $\Delta Y^{exog}$ (P9).
4. **Stocks**: `value.equities` (and $\omega_a$ shares of `value.dc`, `value.trusts`,
   `value.re_fund`) × $(1-\omega_a\,\mu_t)$; **column-specific, not the uniform
   $(1-f)$ wealth-haircut scaler** (which exists to keep $s_{pt}/\rho_{pt}$ invariant —
   a different design goal); `net_worth` recomputed from the marked-down balance sheet;
   **basis never scales** (P5). DB → residual (D10).
5. **Gains**: kg_dynamics runs debit/credit the unrealized-gain state by
   $\omega_{kg}M_t$; non-kg runs use P6's exact per-record form with `kg_lt_basis`.
   Realized-gain deltas stay out of $\Delta Y^{exog}$ (P9).
6. **Retirement**: `txbl_ira_dist`/`txbl_pens_dist` × $(1-\mu^{ret}_t)$ (P7; Phase 1
   prerequisite); distributions stay out of $\Delta Y^{exog}$ (P9).
7. **Bathtub**: forcing generalized to $F = \Delta T - \Delta Y^{exog}$ with
   $\Delta Y^{exog}$ from step 3 exactly; $\Delta T$ measured by the calculator on the
   conv-no-wealth pass as today (`wealth_dynamics.R:829-836`).
8. **Receipts**: corporate input at 0.75/0.25 CY→FY; endogenous offset rides the normal
   individual receipts deltas; estate/wealth/deemed legs at FY+1; run one year past the
   window. Conservation (§4) checked per CY year, allocator-style, with the
   $\Delta\rho$ residual line (P14).

Order-dependence caveat (§8.13) stands: the endogenous offset makes the corporate row
stacking-order-dependent by construction.

---

## 8. Model mapping

| math object | model object |
|---|---|
| $F_t$ forcing | `dT0` in `run_wealth_bathtub_pass` (`wealth_dynamics.R:829-836`), generalized per P8/P9 |
| $P$, $G$, recurrence | `cohort_recurrence_step` (`cohort_bathtub.R:248`), kernel `wealth_dynamics.R:881` |
| $s$, $M$ | financing profile folders (`config/wealth/profiles/`), `wealth_dyn_resolve_profile` |
| $\tau y + \tau_w$ drag | `mtr_cap_bundle` × cell yield + `mtr_net_worth` (P11) |
| flow columns & weights | `WEALTH_CAP_FLOWS_*` (`wealth_dynamics.R:79-110`) — single source of truth, incl. the 0.2 pt weight |
| $\mu_t$ on stocks | new column-weighted scaler over `ESTATE_ASSET_COLS` (NOT the uniform `wealth_dyn_apply_to_records` $(1-f)$) |
| gain state, basis | kg_dynamics unrealized-gain state; `kg_lt_basis` (P5/P6) |
| vintaging $\eta(t)$, rollover | `do_capital_adjustment` machinery (`economy.R:193-294`), `resources/debt_maturities.csv` |
| smear (fallback, distribution) | `distribution.R:341-345` (unchanged, D4); cost-recovery `distribution.R:600` (§5.4) |
| FY booking | `revenue.R:140` (corp 0.75/0.25), `:147-148` (estate FY+1), `:160` (wealth FY+1) |
| conservation check | new supplemental diagnostic, estate-allocator style (`estate_allocator_diag` precedent) |

---

## 9. Verdicts

| # | claim (source) | verdict |
|---|---|---|
| V1 | Naive markdown $\Delta\tau/(1-\tau)$ is the ceiling (§5) | **Theorem** (P1) under (A1)–(A3); wedges = measured $\omega$ + migration |
| V2 | Yield preservation under permanent shock (D9) | **Theorem** (P2) |
| V3 | Temporary markdown = annuity share, shrinking to zero; holders earn $r$; burden = announcement loss (D9) | **Theorem** (P3, P4) |
| V4 | Stock and flow legs cannot double-count (D9) | **Theorem** (P4 telescoping, P10 budget identity) |
| V5 | Generalized forcing fixes the sign; $s$ allocates a fixed PV burden (D11) | **Theorem** (P8, P10); in-model invariance is gross of the second-round drag, which is $s$-dependent *by design* (P12); kernel = the rule applied to its own feedback (P11, new consistency result) |
| V6 | Composition-neutral proportional scaling; blend error second-order (D12) | **Confirmed and quantified** (P6): error $\propto(\phi_t+\mu_t)$, exactly zero for permanent shocks; exact per-record form available via `kg_lt_basis` |
| V7 | Basis never scales; the gain absorbs the hit (§8.5) | **Theorem** (P5); wording fix: amplification $\mu/(1-b)$ *increases in basis share* — high-basis holders lose proportionally more, low-basis holders lose more dollars |
| V8 | Depreciation sign-flip; receipts/distribution split correct; permanently separate (D13) | **Theorem** (P13′, §5.4); gate (G) formalized; `distribution.R:600` = per-vintage PV booking of the time-value transfer |
| V9 | Flows-only migration; markdown decays to rent-share floor (D14) | **Floor confirmed** (P14, via the q-pinning/$\Delta\rho$ offset). **Allocation corrected** (P13): migrated normal burden splits $(1-\kappa)$ noncorporate / $\kappa$ retained on corporate flows; the noncorporate-only rule is the $\kappa\to 0$ limit and is internally inconsistent with its own mechanism. **RULED: adopted (D15)** |
| V10 | "Decedents dying after expiry are untouched" (D9) | **False post-D11**: the markdown leg vanishes at expiry but the accumulated dissaving compounds past it (P10.1). Temporary shocks have a smaller but nonzero 30-year story. **RULED: gloss amended (D17)** |
| V11 | Retirement/kg flows in the forcing | **Unspecified in CONSIDERATIONS; resolved here** (P7, P9): internal conversions enter via $\Delta T$ only; putting them in $\Delta Y^{exog}$ double-counts the markdown. **RULED: confirmed (D16)** |
| V12 | Gross-input requirement (D1) | Formalized error bound: net-treated-as-gross understates combined revenue by the embedded offset (§1.2) |
| V13 | Conservation identity as the invariant (D9) | Stated checkably (§4); requires CY space and the named $\Delta\rho$ residual line under D14 |
| V14 | Doc internal consistency | §8.2 ("static AND conventional"), §8.3 (smear kill-switch), §8.11 ("shock applies to both passes"), §10-Phase-2 ("both passes"), and the §6 bucket-table frames predate and contradict D4/D5 — mark superseded before implementation. Open question 9.7 resolves trivially under D5: static estate totals never move, so the heir-allocator pin is untouched |

---

## 10. Items needing an author ruling

1. **The κ factor (P13/V9).** Accept the two-sector allocation: noncorporate lines get
   $(1-\kappa)\eta(t)w^{norm}$, corporate dividend/kg flow factors retain
   $\kappa\,\eta(t)w^{norm}$; markdown floor unchanged at $w^{rent}$; the $\Delta\rho$
   revaluations (bond/land gains + the pinning offset) booked as a named conservation
   residual. Requires measuring $\kappa$ (C-corp share of the private normal-capital
   stock, ~⅓–½ prior). The alternative — keeping the D14 spec — should be recorded as
   the deliberate $\kappa\to 0$ approximation with its known bias (overstates pt/interest
   burden, understates persistent dividend compression).
   **RULED 2026-07-02: ADOPTED as D15** — κ measured from Fed Z.1 in Phase 0; the
   owner-occupied-housing definitional fork sets the sweep corners κ ∈ {~0.25, ~0.4, ~0.5}.
2. **The $\Delta Y^{exog}$ line list (P9/V11).** Confirm: dividends, interest, rents,
   pass-through IN; realized gains, retirement distributions, sale proceeds OUT (tax
   legs only). This is the difference between a correct forcing and a silent
   double-count, and it should be a hard-coded column contract like `WEALTH_CAP_FLOWS`.
   **RULED 2026-07-02: CONFIRMED as D16** (with the retiree-credit corollary explicitly
   accepted, recorded so it is not later mistaken for a sign bug).
3. **Temporary-shock persistence (V10).** Amend the D9 gloss: the estate erosion of a
   windowed shock does not end at expiry; the dissaving leg persists. Affects how the
   "the 30-year story requires permanence" headline is worded — it requires permanence
   only for the *markdown* leg.
   **RULED 2026-07-02: CONFIRMED as D17.**
4. **§8.5 wording (V7)** and the **stale sections (V14)** — editorial, but both have
   bitten implementations before.
   **RULED 2026-07-02: applied** — §8.5 rewritten; supersession annotations placed at
   §8.2, §8.3, §8.11, §6, §10-Phase-2; §9.7 marked resolved.
5. **Non-kg reduced form (P6).** Adopt the exact per-record form
   $\Delta kg = \omega_{kg}[\phi_t\,kg - \mu_t(kg + basis)]$ over flow-factor-only
   scaling; costless (uses existing columns) and removes the basis-share understatement
   (worst for mid/high-basis lots; the low-basis top tail is nearly exact either way).
   **RULED 2026-07-02: ADOPTED as D18, extended** — the $\phi$ quantity term applies to
   the realization flow in BOTH run types (the kg_dynamics realization rule knows MTRs
   and mortality, not payout policy); the price margin in kg runs stays the state debit,
   which is exact because the state is gain-denominated.

---

## Appendix: proof sketches not shown inline

**P4 (holder return).** $V^{post}_t = V_t - M_t$, $X^{post}_{t+1} = X_{t+1} - d_{t+1}$.
Return numerator $= (V_{t+1} - M_{t+1}) + (X_{t+1} - d_{t+1}) - (V_t - M_t)
= rV_t - (M_{t+1} + d_{t+1} - M_t)$. Since $M_t(1+r) = d_{t+1} + M_{t+1}$,
the bracket is $rM_t$, so the numerator is $r(V_t - M_t) = rV^{post}_t$. ∎

**P10 (decomposition).** Cash pocket deviation:
$\Delta W_{t+1} = (1+r)\Delta W_t - d_t + \tau d_t - \Delta C_t$ with
$\Delta C_t = -(1-s)(1-\tau)d_t$ gives
$\Delta W_{t+1} = (1+r)\Delta W_t - s(1-\tau)d_t$, hence
$\Delta W_T = -s(1-\tau)\sum_{t\le T}(1+r)^{T-t}d_t$. Total burden
$= -\sum \Delta C_t/(1+r)^t - (\Delta W_T + \Delta V_T)/(1+r)^T$ with
$\Delta V_T = -M_T$; substituting and using
$M_T/(1+r)^T = \sum_{t>T} d_t/(1+r)^t$ yields the stated sum, and adding/subtracting
$\tau\sum_{t\le T}d_t/(1+r)^t$ gives the $M_0$ form. No term depends on $s$ except the
split of the during-life share. ∎

**P13′ (q-arbitrage).** A buyer can always create capacity at $1-\tau z'$; no rational
buyer pays more for equivalent installed capacity, and entry at the margin enforces
equality given (Z) held pre-reform for old vintages at the old margin. The incumbent's
loss per unit is $(1-\tau z) - (1-\tau z') = \tau\Delta z$. Quasi-rents are pinned by
the *output* market in the short run; the capital-goods market reprices immediately. ∎

**P13 (κ).** Long-run arbitrage forces one after-tax return $\rho'$ on every unit of
normal capital. Fixed GNP ⇒ total after-tax capital income falls by exactly the revenue
$w$. Rents absorb $w^{rent}$ (no margin). The remainder $w^{norm}$ is
$(\rho-\rho')\bar K^{norm}$ by definition of the compression, distributed pro-rata over
$\bar K^{norm}$; corporate normal capital is $\kappa$ of that stock. For noncorporate
lines to absorb *all* of $w^{norm}$ would require $\rho$ unchanged on corporate normal
capital while changed on noncorporate — contradicting the single-price condition that
generates the migration in the first place. ∎
