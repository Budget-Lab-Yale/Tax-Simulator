# Hidden-ledger design note
### Making concealment consistent across the wealth, income, and estate bases
*2026-07-08. Implements VISION.md gate 1, per author rulings this session.
Companion: `reporting_vs_real_audit.md` (the code audit this design obeys).*

> **STATUS — BUILT 2026-07-08.** All of §7 implemented on branch `wealth`:
> concealment fractions + flow/kg concealment + R3 evasion→wealth link + R4
> estate haircut + order guard + diagnostics in
> `config/scenarios/behavior/wealth/avoidance.R`; persisted `evasion_g_*` in
> `evasion/debacker.R`; optional `estate_concealed_frac` input in `calc_estate`
> (no `do_taxes` change needed — the `parse_calc_fn_input` contract carries the
> frame column through); `estate_concealed_frac` added to the conventional
> detail select-list. Guard/unit suite
> (`other/top_tax/tests/test_hidden_ledger_guards.R`) — 30/30 PASS. Full-sample
> smoke pair (`config/runscripts/tests/hidden_ledger_smoke.csv`, CHI-central vs
> CHI=0) + checker (`other/top_tax/tests/check_hidden_ledger.R`) — **all 5×4
> checks PASS**: CHI=0 no-op (marketable flows + estate == static exactly,
> estate_concealed_frac=0, net_worth identical across CHI legs), marketable flows
> shrink by exactly exp(mtr·public_e) where mtr>0, below-exemption central==CHI=0
> exactly, estate_distributable invariance, conservation identity (rel err
> ~1e-15), and direction — conventional estate revenue **−$7–8B/yr** and income
> tax **−$34–35B/yr** vs CHI=0 (concealed wealth ~$2.0T marketable / ~$2.4T
> closely-held). Open item #4 (estate extension of the evasion link) deliberately
> still parked.

---

## 1. What this builds, in one paragraph

Today, wealth-tax avoidance shrinks only the reported wealth-tax base: a
household that hides $100M still pays income tax on that wealth's dividends
and still leaves it in the taxable estate. This build splits avoidance into
**concealment** (the money leaves the tax authority's sight entirely — its
income flows and its estate value disappear from the reported bases too) and
**valuation gaming** (the assessed value is lowballed but the income is still
visibly received). It also adds the reverse link: income that the evasion
module hides should pull the matching business assets out of the reported
wealth-tax base. The organizing idea is a single per-record **hidden ledger**:
each reporting margin contributes hidden amounts to it, and every *reported*
base (wealth, income, estate) reads from the same one — while every *real*
base (the balance sheet, the bathtub, kg, heirs) never sees it.

## 2. Author rulings (2026-07-08)

| # | Ruling |
|---|---|
| R1 | **Marketable avoidance is 100% concealment.** You cannot lowball an exchange price, so avoiding marketable wealth means hiding it — flows (and estate value) go with it. |
| R2 | **Closely-held avoidance is 50% valuation / 50% concealment.** Valuation discounts are real and legal for private businesses; the other half is treated as hidden. |
| R3 | **The evasion elasticity applies to wealth too**: the evaded share of a record's business income pulls the matching share of its closely-held assets out of the reported wealth base. A consistency rule, not a new elasticity. |
| R4 | Concealed wealth also escapes the reported **estate** at death. CONFIRMED 2026-07-08. |
| R5 | **Homes keep the uniform 50/50 split** (no home-specific χ; the class-level ruling applies as stated, accepted as a reduced form). |
| R6 | **Capital gains are IN v1** as a reporting-quantity overlay: reported `kg_lt` scales by (1 − c_pub) AFTER the kg module sets realization behavior. The GAIN is scaled, not the price — a hidden sale never appears on the return and takes its entire gain with it; no basis adjustment (basis only prices reported sales), no `value.*` change (nothing real happened). |
| R7 | **χ wired as env knobs**: `WEALTH_CHI_PUB` (default 1.0), `WEALTH_CHI_PRIV` (default 0.5), read once, stamped into diagnostics. |

## 3. The iron rule (from the audit)

Concealment may touch ONLY the columns that feed tax computation:

- reported income-flow columns (they feed `do_taxes` and nothing else),
- the materialized `net_worth` column (the documented isolation point that
  only `calc_wealth` reads),
- a new reported-side input to `calc_estate`.

It must NEVER scale `value.*`: the estate's economic base, the corporate
channel's exposure, the kg gain machinery, and the bathtub all read `value.*`
as the real balance sheet, and scaling it would book hidden wealth as
destroyed wealth. The audit confirmed every real-side reader is insulated from
the reported side (raw Tax-Data / static detail / `net_worth_raw`), so
following this rule preserves the firewall exactly.

## 4. Mechanics

### 4.1 Concealment fractions (inside `wealth/avoidance.R`)

The module already computes, per record, the avoided fraction of each asset
class: `f_pub = 1 − exp(mtr_w · e_pub)` and `f_priv = 1 − exp(mtr_w · e_priv)`
(zero below the exemption, since `mtr_net_worth = 0` there). New:

```
c_pub  = CHI_PUB  · f_pub      # concealed fraction, marketable   (CHI_PUB  = 1.0)
c_priv = CHI_PRIV · f_priv     # concealed fraction, closely-held (CHI_PRIV = 0.5)
```

`CHI_*` are env-overridable constants (band sweeps), provenance-commented in
the module header. Reported `net_worth` keeps its current formula — the FULL
avoidance response (valuation + concealment) shrinks the wealth-tax base, as
today. `CHI_PUB = CHI_PRIV = 0` must reproduce current behavior exactly (test).

### 4.2 Flow scaling (income tax sees less)

Concealed assets stop producing reported income. Class → flow map:

- **Marketable (`c_pub`)**: `txbl_int`, `div_ord`, `div_pref`.
  *Deliberately excluded:* retirement distributions (`txbl_ira_dist`,
  `txbl_pens_dist`) — `value.dc`/`value.db` sit in the marketable class for
  the avoidance elasticity, but retirement accounts are third-party-reported
  and not realistically concealable; scaling their distributions would
  overstate. Documented as a scope choice.
- **Closely-held (`c_priv`)**: the pass-through legs (`part_active`,
  `part_passive`, `scorp_active`, `scorp_passive`, `sole_prop`) with SECA
  companions co-scaled (`part_se1/2`, `sole_prop1/2` — the debacker.R
  pattern), plus `rent`.
  *Note:* homes generate no taxable flow, so the home share of `c_priv` has
  no flow-side effect; it matters only through the estate haircut (§4.4).
- **Capital gains (`c_pub`), per R6**: reported `kg_lt` scales by
  (1 − c_pub), applied inside the wealth module — which runs AFTER the kg
  applier, so realization *behavior* is set first and the concealment
  overlay hides a share of the resulting reported sales. Quantity-of-
  reporting semantics: each hidden sale takes its whole gain; no basis
  adjustment, no price effect. Clean per the audit: the kg cell machinery
  reads raw Tax-Data, and the only downstream consumer of record `kg_lt`
  at that point is `do_taxes`.
  *Known gap (documented, not built):* in a package combining
  deemed-at-death with a wealth tax, hidden assets should escape deemed
  gains too; `kg_deemed_full` is a cross-class per-record aggregate, so a
  correct treatment needs a blended concealment rate — deferred until such
  a package is actually run.

These are multiplicative scalings of reported legs — the same operation
evasion already performs, priced off the wealth-tax margin instead of the
income-tax margin. No double-counting: two distinct forcings (a record with
both faces both, sequentially).

### 4.3 Evasion → wealth link (R3)

Evasion currently computes per-record response factors (`.g_schc`, `.g_pt`,
`.g_rent`) and drops them before returning (debacker.R:171–174). Change:
persist them as record columns (`evasion_g_schc`, `evasion_g_pt`,
`evasion_g_rent`). The wealth module — running AFTER evasion — reads them and
further scales the reported closely-held component of `net_worth` by the
record's evaded income share (weighted across its PT legs). Effect: an income
evader under a wealth tax does not report the assets whose income he hides.

- Consumed by both the wealth-tax and estate-tax reported bases. The estate
  combines income-evasion concealment with wealth-tax concealment as a
  multiplicative union, so overlap is not counted twice (extended 2026-07-11).
- Harmless when no wealth tax exists: the overwritten `net_worth` column is
  read only by `calc_wealth`.
- Why this isn't double-counting against `private_e = −17`: the avoidance
  elasticity prices the *wealth-tax* return; the evasion factors price the
  *income-tax* return. The link transmits each response to the other base
  rather than re-estimating either.

### 4.4 Estate haircut (R4)

Concealed dollars (both classes, summed per record, expressed as a fraction
of gross assets) ride the record as a new column and enter `calc_estate` as a
**reported-side reduction that shrinks the taxable base but not the
inheritance**. The exact precedent already exists in the calculator: the
income-tax-at-death deduction reduces `estate_base` while
`estate_distributable` — the heir allocator's bequest ladder — was
deliberately built to stay invariant (estate.R:161–169). The concealment term
enters in that same slot. Consequences, all intended:

- estate revenue falls under a wealth-tax scenario (the cross-base
  interaction we're currently missing);
- heirs inherit the hidden wealth unchanged — distribution untouched;
- the heir-allocator aggregate identity still ties to static totals (test).

### 4.5 Ordering and guards

- **Pin `wealth/avoidance` after `evasion/debacker`** when both are present
  (hard stop, evasion-style message) — the R3 link reads evasion's output.
  Currently the module has no order guard at all; this adds its first.
- Everything else is unchanged: the module keeps running late in the stack,
  after the real modules, which the audit showed is what keeps the firewall
  intact.

## 5. What this composes with (no action needed, verified in audit)

- **Bathtub**: concealment lowers conventional taxes paid → the forcing
  falls → less real wealth drain. Correct under the cash principle (money
  not paid in tax is not financed out of wealth). Cell ranking reads
  `net_worth_raw` — immune.
- **kg / corp / distribution / ETR file**: read raw Tax-Data, static detail,
  or pre-behavior frames — immune by construction.
- **Conventional-ETR variant** (VISION §D, if built): must use the static
  income denominator; a concealer's delivered ETR = conventional tax over
  actual income. Already the stated convention.

## 6. Diagnostics and tests

Supplemental per year (`conventional/supplemental/hidden_ledger_{t}.csv`):
concealed wealth by class, concealed flows by type, evasion-link wealth
reduction, estate concealment total — plus a reconciliation line:
reported base + hidden ledger = pre-avoidance base (hard assert in code).

Smoke tests (sbatch, wealth-tax test scenario + estate-visible records):

1. `CHI_PUB = CHI_PRIV = 0` and evasion absent ⇒ byte-identical to current
   behavior (no-op regression, smoke-diff harness pattern).
2. Flows shrink only for records with a positive wealth MTR; SECA companions
   move with parents (payroll frame consistency).
3. `estate_distributable` invariant record-by-record; heir-allocator
   Σw·p·λ identity still ties to static `totals/estate.csv`.
4. Conservation assert holds each year.
5. Order-guard tests: avoidance before evasion ⇒ hard stop.
6. Direction test: wealth-tax scenario shows estate revenue DOWN and
   income-tax revenue DOWN relative to a `CHI = 0` run, magnitudes ∝ χ·f.

## 7. Files touched

| File | Change |
|---|---|
| `config/scenarios/behavior/wealth/avoidance.R` | concealment fractions, flow scaling, evasion-link read, estate-concealment column, order guard, diagnostics |
| `config/scenarios/behavior/evasion/debacker.R` | persist per-record response factors as columns |
| `src/calc/functions/tax/estate.R` | optional concealment input → `estate_base` reduction (income-tax-ded pattern) |
| `src/calc/do_taxes.R` | thread the new optional estate input (mirroring `estate_income_tax_ded`) |
| `config/runscripts/tests/` | hidden-ledger smoke fixture |

No SLURM changes (all inside `run_one_year` / behavior modules — the safe
category per CLAUDE.md). No changes to wealth_dynamics, kg_dynamics,
corp_incidence, or the distribution/ETR builders.

## 8. Open items — RESOLVED 2026-07-08 (author prompts)

1. ~~R4 confirm~~ → **CONFIRMED** (estate escape in).
2. ~~Homes~~ → **uniform 50/50 as ruled** (reduced form accepted).
3. ~~kg_lt~~ → **IN v1** as the R6 reporting-quantity overlay (gain scaled,
   not price). Deemed-at-death × wealth-tax packages remain a documented
   gap (§4.2).
4. **Estate extension of the evasion link** (income evaders' estates):
   still flagged, not built — the one item deliberately left open.
5. ~~χ bands~~ → **env knobs** (`WEALTH_CHI_PUB`/`WEALTH_CHI_PRIV`,
   defaults 1.0/0.5); sweep `WEALTH_CHI_PRIV` 0.25/0.5/0.75 if a band is
   wanted on the interaction rows.
