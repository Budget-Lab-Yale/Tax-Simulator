# Reporting-vs-real audit
### Do reporting changes (evasion / avoidance) contaminate real-side state?
*2026-07-08. Code audit on branch `wealth`. Companion to VISION.md gate 1 and
the hidden-ledger design note (`hidden_ledger_design.md`).*

> **FOLLOW-UP: the hidden-ledger build shipped 2026-07-08 (commit
> `7b46a169c`) and obeyed this audit's iron rule** — concealment touches only
> reported income flows, the `net_worth` isolation point, and the new
> `estate_concealed_frac` input to `calc_estate`; `value.*`, `kg_lt_basis`, and
> `kg_deemed_full` are untouched. The "Implications for build 1" section below
> was the spec; all five points were implemented (χ=0 no-op exact,
> conservation ~1e-15).

## The principle being tested

**Cash is reported; balance sheets are real.** A reporting change (evasion,
wealth avoidance) may legitimately affect exactly one real-side object: cash
taxes actually paid (and hence the wealth bathtub's financing flow — if you
evade, you really don't write the check). It must never affect: the economic
balance sheet (`value.*`), the kg gain stock, bathtub cell assignment, heir
inheritances, or corporate exposure. Conversely, real changes (corp markdown,
wealth haircut, realization, conversion, entity shifting) SHOULD flow into
every base, including reported ones.

## Verdict: CLEAN. The firewall exists and was built deliberately.

The architecture already separates the two worlds. The pattern: every
real-side consumer reads **raw Tax-Data, static detail, or an explicitly
preserved raw column** — never the behavior-modified conventional frame.
Reporting modules write only to **tax-computation inputs** (income legs that
feed only `do_taxes`; the materialized `net_worth` column, which exists as a
documented isolation point).

## Operation order (final conventional pass, run.R:730–825)

1. Frame load; `net_worth` materialized from `value.*` sums (run.R:550)
2. **Corp applier** (REAL): marks down `value.*`, scales flows, recomputes
   `net_worth` — sees the pre-behavior frame (run.R:766)
3. **Wealth haircut** (REAL): erodes `value.*`/flows/basis, recomputes
   `net_worth`; bins on `rank_value = tax_units$net_worth` — the RAW
   pre-behavior vector (run.R:793–794)
4. **Behavior modules**, strictly in runscript column order; pinned
   kg → conversion/σ → entity_shifting → evasion for the top-tax stack.
   Reporting modules (evasion, wealth/avoidance) run at the end of the stack;
   no real-side machinery reads the frame after them — only `do_taxes` does,
   which is the point
5. `do_taxes`: `calc_estate` reads `value.*`; `calc_wealth` reads `net_worth`

## The matrix

| Real-side reader | What it reads | Source | Verdict |
|---|---|---|---|
| Bathtub cell ranking + cutoffs | `net_worth_raw`, explicitly NOT the detail's `net_worth` ("which a behavior module (e.g. wealth avoidance) may have overwritten") | wealth_dynamics.R:845–854 | **CLEAN** |
| Bathtub kernel denominator (Σgross) | `pmax(net_worth_raw, 0)` | wealth_dynamics.R:890–891 | **CLEAN** |
| Bathtub forcing ΔT⁰ | conv-no-wealth liabilities (post-behavior) − `corp_dY_exog` | wealth_dynamics.R:871–877 | **BY DESIGN** — cash taxes actually paid; evasion legitimately shrinks the drain |
| kg cell aggregates (G, R, m, per-asset gains) | raw Tax-Data CSVs, `fread` direct | kg_dynamics.R:1878 | **CLEAN** |
| kg MTR inputs | static detail (static pass runs no behavior) | kg_dynamics.R:1903ff | **CLEAN** |
| σ pool/gate legs | raw Tax-Data + static detail | sigma_conversion.R:108, 226, 559 | **CLEAN** |
| Estate base (`economic_gross`) | `value.*` (ESTATE_ASSET_COLS); never the `net_worth` column | estate.R:143–153 | **CLEAN** — avoidance provably cannot leak in; corp/haircut correctly DO (they scale `value.*`) |
| Heir inheritance (`estate_distributable`) | reported_gross − debts − f_ded; deliberately excludes the income-tax-ded term to stay scenario-invariant | estate.R:161–166 | **CLEAN** — and the precedent hook for build 1 |
| Wealth-tax base | materialized `net_worth` — the documented isolation point | wealth.R:13–19 | **BY DESIGN** — this IS the reported base |
| Corp applier exposure | pre-behavior frame (runs first) | run.R:752–766 | **CLEAN** |
| Distribution + distribution_etrs | static detail only (house convention D20) | distribution.R / distribution_etrs.R | **CLEAN** by convention |
| kg deemed-gain recompute | `value.*`/`basis.*` on the conv frame (post-corp, post-haircut = real changes only; reporting modules don't touch these columns) | run.R:830–839 | **CLEAN** |

Composition detail worth noting: the avoidance module recomputes reported
`net_worth` from the CURRENT frame's `value.*` component sums (avoidance.R:84–92),
i.e. post-corp-markdown, post-haircut — so real erosion correctly propagates
into the reported wealth-tax base before the reporting shrink applies. Right
order, right composition.

## Implications for build 1 (the hidden-ledger design note)

The firewall works because existing reporting modules touch only isolation
points. Concealment must follow the same pattern:

1. **Scale reported FLOW columns** (div_ord, txbl_int, etc.) — these feed only
   `do_taxes`, so no real-side reader is exposed. Do NOT scale `value.*`:
   every real-side channel (estate base, corp exposure, kg gains, haircut)
   reads it as the economic balance sheet.
2. **Estate concealment enters `calc_estate` as a reported-side term** that
   reduces `estate_base` but NOT `estate_distributable` — the exact pattern
   the income-tax-at-death deduction already uses (estate.R:161–169). Heirs
   keep inheriting hidden wealth; the estate tax just doesn't see it.
3. **Evasion→wealth consistency link**: evasion's response factors are
   currently temp columns, dropped before return (debacker.R:171–174). The
   link needs them exposed (persisted column or recompute) so the wealth
   module can shave the matching closely-held share off reported net worth.
4. **Module-order guard**: wealth/avoidance currently has no order assert.
   Build 1 should pin it after evasion (the consistency link reads evasion's
   outcome) with an evasion-style hard stop.
5. **kg_lt concealment** (hidden assets' unreported realizations) would
   interact with the kg applier's own overwrite of `kg_lt` — fiddly; the
   design note decides whether v1 includes it or documents its exclusion.

## Margins inventory (writers), for completeness

| Margin | Nature | Writes |
|---|---|---|
| Corp incidence applier | REAL | `value.*` markdown, flow scaling, `corp_dY_exog`, `net_worth` recompute |
| Wealth haircut | REAL | `value.*`/flows/basis erosion, `net_worth` recompute |
| kg_dynamics applier | REAL | `kg_lt` (realization behavior), deemed columns |
| conversion/σ | REAL | wages/PT legs down; kg cell-state injection |
| entity_shifting | REAL | `part_active` ↔ corp base, `kg_lt` offset |
| charity | REAL | `char_cash` etc. |
| evasion | **REPORTING** | sole_prop/part/scorp/rent legs + SECA companions (feed `do_taxes` only) |
| wealth/avoidance | **REPORTING** | materialized `net_worth` only (isolation point) |
