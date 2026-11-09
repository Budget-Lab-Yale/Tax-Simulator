# Taxing the top: how much is really there?
## Project vision, v3 — 2026-07-09 (the per-lever atlas)

*v3 supersedes the v2 deliverable design (§4–§8 of the 2026-07-08 doc). The
research question (§1), the model inventory (§3), and the credibility gates (§6)
carry over intact — only the **shape of the deliverable** changed. Prior designs
(full-factorial frontier, multi-intensity greenbook schedules, standalone ETR
section, revmax exhibits) are retired; see §5. The v2 markdown and its HTML render
are now stale — regenerate `VISION.html` from this file when convenient.*

> **STATUS (2026-07-09) — a 7-switch binary factorial (128 combos) backing a
> 4-lever atlas.** The **report headline** is a **per-lever atlas** over the
> four rate levers with a single clear tunable direction: **top ordinary rate
> (+5pp), top capital-gains rate (→39.6%), corporate rate (→28%), wealth rate
> (1%>$50M).** Each card presents 10y-window revenue, conventional-as-a-share-of-
> static, the destination ledger, and a static+conventional ETR chart. The
> **run set** behind the atlas is the full **binary factorial over 7 switches**
> (the 4 rate levers + realization-at-death step-up/deemed + estate
> current/Clausing-2009 + QBI on/off) = **2⁷ = 128 scenarios** (§2.6). The
> conditioning-interaction panels and the interactive explorer are **slices of
> this one factorial** — with the Object-A / package-total guardrails of §2.5.
> Corporate ships with placeholder CORP_* constants disclosed (gate 4). **No
> open gate blocks the run campaign.**

---

## 1. The research question

**How much additional revenue is actually available at the top of the income
and wealth distribution?**

Progressive tax agendas propose raising revenue from the same small group of
people through several statutes at once: top ordinary rates, capital gains
rates, the treatment of gains at death, the estate tax, a wealth tax, the
corporate tax, QBI repeal, NIIT expansion. Scoring each proposal in isolation
and summing overstates the combined capacity, because the bases overlap
mechanically (the same dollar of wealth is capital income during life, estate
at death, wealth-tax base in between, and corporate profit upstream) and
behaviorally (avoidance re-routes income between bases; evasion removes it).

The model closes these loops on actual record-level balance sheets rather than
through assumed elasticities: realization behavior that responds to the death
regime (kg Bellman), a saving-financing channel that drains during-life taxes
out of future capital-income/estate/wealth bases (the bathtub), an on-model
corporate incidence channel with an endogenous individual-side offset, entity
shifting across the C/PT boundary, income conversion into the unrealized-gain
state (σ), evasion as a leak, and charitable responses.

## 2. The v1 deliverable: a per-lever atlas

The deliverable is organized by **policy lever**, not by exhibit type. One
**card** per lever. Each card has two parts.

### 2.1 The standalone score (was §A)

Each lever is scored at **one intensity**, set by the author and calibrated
judgmentally so that the levers raise a **comparable amount of revenue as one
another**. Revenue-matching is the point: it puts the behavioral-survival ratio
and the delivered ETRs on equal footing across levers, so differences read as
differences in *how the base responds*, not in how hard the lever was pushed.

Each card presents exactly four things, static and conventional:

1. **Revenue over the 10-year window (2027–2036)** — static (law-only) and
   conventional (all channels on), in dollars. (v1 is 10-year only; no 30y.)
2. **Conventional as a share of static** — the fraction of the mechanical score
   that survives behavior, over the window. The single most legible
   cross-lever comparison the atlas offers. (This scalar is the top line of the
   destination ledger, item 3.)
3. **Destination ledger** — a convention-free leakage waterfall answering
   *where the money ends up*. It is an accounting identity, not a behavioral
   attribution: take the static intent (≈ all in the lever's own head) and
   reconcile it to the conventional total via the change in revenue by head.

   ```
   e.g. CG rate up, 10y
     $300B   static intent          (all in the CG head)
    −$120B   own-base erosion        (own head falls 300 → 180)
    + $25B   resurfaces as estate
    + $10B   resurfaces as wealth
    −  $5B   corporate
    ─────
     $210B   conventional total      (= 70% of static)
       of the $120B that left the CG base:
          $30B relocated to another base   (conserved)
          $90B net gone in-window          (evasion + deferral not yet landed)
   ```

   Convention-free (every term is a Δ vs the common baseline), it sums exactly,
   and it directly answers "disappears vs. relocates to another base." A
   within-window timing story is built in: later in the 10-yr window more of the
   net-gone slice resurfaces as estate/wealth (deferred gains die). Comes
   straight from the existing revenue-by-head receipts — **no extra runs.**
   *This is Object A (see §2.5); it must never be drawn as a Sankey or fused
   with the mechanism attribution.*
4. **ETR chart(s)** — Haig-Simons effective tax rates on the top 1% and top
   0.1%, baseline vs. reform, showing **both**:
   - **static liability ETR** = first-order welfare burden (envelope theorem:
     the burden a taxpayer bears ≈ the tax owed absent the response); and
   - **conventional ETR** = realized-revenue rate (what the government actually
     collects) — the numerator-only swap (see build note, §2.4).

   The **gap between them is, to first order, the deadweight loss of the
   avoidance response** (real resources burned re-routing income) — content in
   its own right. Caveat retained: the wedge is pure DWL only for the *costly*
   channels (σ conversion, entity-shifting, portfolio distortion); the
   **evasion** slice is a transfer the evader keeps, so a strict welfare ETR
   would exclude it. The model separates conversion (conservation) from evasion
   (leak), so a clean-welfare variant exists if wanted — not shipped in v1.

### 2.2 The conditional re-baselines (was §B, collapsed to the matched intensity)

Within each card, a few **interactions**: the **same lever at the same
intensity**, re-scored against a **different baseline** — one where a
**conditioning switch *j*** is already on. The reported delta is the lever's
yield (and delivered ETR) *conditional on j*, presented with the identical
exhibits. This is the §B conditional schedule collapsed to the single matched
intensity: instead of a curve shift you get a small panel of "yield when the
base is already altered by j." **These panels are not new runs — they are
featured slices of the §2.6 factorial** (a conditioning panel = the
(lever + j) cell minus the (j-alone) cell). The table below is just which slices
the report narrative surfaces; the explorer exposes the rest.

Featured conditioning slices per card (author may edit — decision D2):

| Lever (card)          | Featured conditioning switch *j* (besides all-off)                        |
|-----------------------|---------------------------------------------------------------------------|
| Top ordinary rate     | deemed-at-death (closes the σ conversion exit) · QBI repealed              |
| Top capital-gains rate | deemed-at-death / step-up repealed (**crown pair** — endogenous realization) · corporate → 28% |
| Corporate rate        | top ordinary +5pp (entity-shifting boundary)                              |
| Wealth rate           | estate → Clausing-2009 · top-CG → 39.6% · deemed-at-death                 |

### 2.3 The lever set and intensities

**Four levers ship in MVP v1**, each a rate with one clear tunable direction (up).
Intensities are **author-set by hand** (decision D1), calibrated so the four
raise a comparable amount of revenue; placeholders below to be filled before the
run campaign.

| # | Lever                    | v1 intensity          | Direction | Honesty status                         |
|---|--------------------------|-----------------------|-----------|----------------------------------------|
| 1 | Top ordinary rate        | **+5pp** above baseline | rate up | full — endogenous rate-dependent ETI   |
| 2 | Top capital-gains rate   | **→ 39.6%** (gains at top ordinary) | rate up | full — endogenous realization (crown)  |
| 3 | Corporate rate           | **→ 28%** (author-supplied OME) | rate up | **placeholder** CORP_* constants (gate 4, disclosed) |
| 4 | Wealth rate              | **1% on net worth > $50M** | rate up | **ceiling** — migration in avoidance elast. |

Config details (resolved 2026-07-09 except where noted):
- **Effective year 2027, UNEXPECTED (surprise) framing** — no anticipation; set
  the kg-Bellman and corp perfect-foresight surprise flags accordingly (Clausing
  v2 precedent). Reform effective CY2027.
- **Window 2027–2036** (10-yr). Sim `years = 2026:2037` (FY lead-in at 2026 +
  one year past 2036 for estate/wealth FY death-year+1). **v1 is 10-year only —
  no 30y horizon** (D11).
- **Switches are INDEPENDENT** (clean factorial semantics): the CG switch fixes
  top CG at 39.6% *regardless* of the ordinary switch — it does NOT follow
  ordinary to 44.6%. So with both on, gains sit below ordinary; that is
  intentional, each switch is one fixed change.
- **Ordinary +5pp** is relative to the *baseline* top bracket, which is **37%**
  (OBBBA kept the TCJA schedule — there is no 39.6 reversion) → top rate **42%**.
  Only the 7th (top) rate moves; brackets unchanged.
- **CG → 39.6%** is the *statutory* top preferred rate (pref `rates` 3rd element
  0.20→0.396); **NIIT (3.8%) stacks → 43.4% all-in** (Biden-proposal convention).
- **Estate #6 → Clausing-Sarin** `clausing/07_estate` ($5M exemption, flat 45%),
  re-anchored to 2027 (campaign-specific copy).
- **Corporate → 28%** enters via the author's OME (`corporate_meta.yaml` + gross
  receipts); the on-model incidence channel then rides it.
- **Wealth 1% > $50M** — single flat marginal bracket, threshold not indexed unless specified.

**Not standalone cards in MVP v1** (they serve only as conditioning baselines,
§2.2): gains-at-death regime (deemed switch), estate tax, QBI repeal, NIIT
expansion. They can be promoted to cards in a later version.

### 2.4 Build notes (confirmed against the code)

- **Conventional ETR = numerator-only swap.** Read the reform leg's tax from
  `conventional/detail` while keeping the fixed balance-sheet economic-income
  denominator (`compose_etr_rows` already uses one baseline-sourced `core` for
  both legs). Do **not** re-source the whole microdata from conventional
  detail — that moves `expanded_inc` with behavior while `accruals_sum` stays
  frozen, corrupting the Haig-Simons denominator.
- **ETR source file:** `static/supplemental/distribution_etrs.csv` per scenario
  (ETR levels, baseline vs reform, HS denominator, tiered taxes, three corporate
  conventions). The three corp conventions enter as a robustness band on the
  ETR charts, not a footnote.
- **Run one year past the reporting window** — estate/wealth deltas book FY
  death-year+1.

### 2.5 Two leakage objects — keep them separate (do NOT nest)

The leakage story is two fundamentally different objects. Conflating them is
what made the v2 Sankey dishonest (it drew mechanism flows as if they were
identity flows) and it is the trap to avoid.

- **Object A — destination ledger ("where did the money end up?").** An
  accounting identity: the change in revenue by head, anchored to the static
  intent. Convention-free, sums exactly, no extra runs. **This is card item 3
  (§2.1) and it ships on every card.** It answers *disappears vs. relocates*
  fully — but it is agnostic about the behavioral path (multiple channels touch
  the same base).
- **Object B — mechanism attribution ("which behavior moved it?").** Attributes
  the static−conventional wedge to channels (realization / σ conversion /
  entity-shift / evasion / bathtub / corporate offset / charity). **NOT
  derivable from Object A** — mechanisms → bases is many-to-many. Requires a
  stated convention (leave-one-out or Shapley) and extra runs, and is
  order-dependent (esp. with the endogenous corporate offset).

**MVP v1 ruling (D7):** ship Object A only. Object B is **deferred** — decide
its depth *after* seeing the A waterfalls: if the net-gone slice is small, B
barely matters; if it is large, it earns the extra toggle runs. When built, B is
a separate, clearly-flagged panel and is never drawn as if it nests inside A.

### 2.6 The run set: a 7-switch binary factorial (128 combos) — decision D8

Rather than a curated handful of conditioning runs, v1 runs the **full binary
factorial over seven switches** (author ruling 2026-07-09 — the cluster makes
the compute tractable). This is a deliberate, bounded reversal of the
"no full-factorial" pruning: it is factorial over *these seven single-intensity
switches only* (2⁷ = 128), NOT the retired ~5,500-run multi-intensity frontier.

| # | Switch | Off (baseline state) | On |
|---|--------|----------------------|-----|
| 1 | Top ordinary rate | current law | **+5pp** (39.6→44.6) |
| 2 | Top capital-gains rate | current law | **→ 39.6%** (gains at top ordinary) |
| 3 | Corporate rate | current law | **→ 28%** (author OME) |
| 4 | Wealth tax | none | **1% on net worth > $50M** |
| 5 | Realization at death | **step-up** | **deemed** |
| 6 | Estate params | current law | **Clausing-2009** *(variant pinned — §2.3)* |
| 7 | QBI (§199A) | **on** (current law) | **off** (repeal) |

- **128 scenarios**, one of which (all-off) is the current-law baseline. Each is
  static + conventional, full-sample, run one year past the window (to 2037). Every
  conventional leg is heavy (the default wealth split-pass is on model-wide; the
  kg frozen pass fires whenever #5 = deemed; corp incidence whenever #3 = on).
- **The atlas cards and the §2.2 conditioning panels are projections of this
  factorial**, not separate runs: a card's standalone score is the single-switch-
  on cell vs the all-off baseline; a conditioning panel is the (lever + backdrop)
  cell vs the (backdrop-alone) cell.
- **The explorer serves the factorial** under the §2.5 guardrails: it shows
  package **totals vs the common all-off baseline** and the Object-A destination
  ledger (convention-free); it does NOT surface "switch-X's contribution" as a
  headline number without a flagged convention — any cell with corp (#3) on is
  hard-flagged as not order-invariant (endogenous offset).
- **Operational discipline (see §4):** run the **heaviest single combo (all-on)
  first**, measure real per-scenario wall-time + detail-file storage, then launch
  the remaining 127 with `delete_detail` / supplemental-retention set from that
  measurement — do not fire 128 full-sample heavy runs blind.

### 2.7 Parts-vs-whole: naive sum vs. actual (the thesis exhibit) — D10

For **any combo**, compare the naive sum of the standalone scores to the combo's
actual score. This is the direct visual answer to "how much is *really* there,"
and it is **convention-free** (every quantity is a total vs the common all-off
baseline — no per-lever attribution, so it clears the §2.5 guardrail) and needs
**no extra runs** (single-switch-on cells + the combo cell are all in the §2.6
factorial). Three bars:

```
$X   Σ standalone STATIC        ← the number a naive advocate quotes
$Y   Σ standalone CONVENTIONAL  ← each lever's own behavior, still summed
$Z   ACTUAL combo CONVENTIONAL  ← behavior AND overlapping bases = the truth
```

- **$X → $Z is the headline gap** — the overstatement from summing overlapping
  bases. Report it for the all-on / full-stack combo (and any featured combo);
  expose it for every combo in the explorer.
- **Honesty label:** the $X→$Y and $Y→$Z splits are *roughly* "behavior" and
  *roughly* "overlap," but they entangle (a switch's standalone behavior ≠ its
  in-combo behavior), so the split is illustrative, not a clean orthogonal
  decomposition. The convention-free content is the endpoints and the total gap.
- **10-year window (v1).** Within the window the gap tends to grow year over
  year (bathtub compounding, deferred gains landing in the estate base); the
  10y-vs-30y widening as a headline finding is out of v1 scope.
- This is the retired §C parts-vs-whole, resurrected in its convention-free
  total form.

## 3. What the model has (inventory and status)

| Channel / capability | Where | Status |
|---|---|---|
| Realization Bellman + death regimes (step-up / carryover / deemed) | `kg_dynamics` | Production; recalibrated under 0.5 applier rule; provenance-guarded |
| τ_eq(age, year) — PV tax per $ entering the gain state | kg machinery, `sigma_conversion.R` | Built, finite-difference-verified |
| Wealth bathtub (saving-financing → estate/cap-income/wealth bases) | `wealth_dynamics.R` | Production; default s-profile calibrated 2026-07-07 |
| Corporate incidence (flows, markdown, kg, endogenous offset) | `corp_incidence.R` | Code complete + verified; **CORP_* constants are Phase-0c placeholders** |
| Entity shifting (C ↔ PT) | `entity_shifting/pearce_prisinzano.R` | Hardened 2026-07-08 |
| Income conversion σ (salary → unrealized gain state) | `sigma_conversion.R` + `conversion/sigma.R` | Built; σ = 0.08 residual-calibrated to top ETI 0.25, conditional on the stack |
| Evasion (DHY visibility-gated leak) | `evasion/debacker.R` | Built; top-end multiplier sweepable |
| Charity (tax-price) | `charity/*` | Production |
| Wealth-tax base + avoidance + hidden ledger | `calc_wealth` + `wealth/avoidance.R` | Production; **hidden-ledger concealment BUILT 2026-07-08** (gate 1) |
| Estate tax (on-model, heir allocator) | `estate.R`, allocator | Production; **+ concealment haircut**; **no own-rate response** (gate 2) |
| ETR-levels incidence cube | `distribution_etrs.R` | Built + smoke-verified 2026-07-08; corp-alloc stock keys have placeholder exposures |
| Policy levers (QBI, NIIT include_active, estate/wealth yaml, tax_at_ord, death regime per asset class) | yaml | Production |

## 4. Sequencing

1. **Pin the two open config items** — the Clausing-2009 estate variant (§2.3)
   and the CG/ordinary coupling + NIIT-stacking rulings (§2.3). Gating input.
2. **Cost the heaviest combo first** — build + run the all-on scenario (all 7
   switches on), full-sample, `years=2026:2037`, static + conventional. Measure
   wall-time and detail-file storage per scenario. This sets `delete_detail` /
   supplemental-retention policy for the batch.
3. **Generate the 128-combo factorial runscript(s)** programmatically (7 binary
   switches → 128 rows, all vs the all-off baseline) and launch on SLURM.
4. **Prototype one card end-to-end** (ordinary or CG) as a slice of the
   factorial: 10y-window revenue, conventional/static ratio, destination ledger,
   static+conventional ETR chart. Confirms the card template.
5. **Build the §2.2 conditioning panels** as factorial slices (no new runs) and
   assemble the atlas + guard-railed explorer (§2.5/§2.6) over the 128 runs.

## 5. What v1 deliberately discards (the pruning, stated)

- **Multi-intensity greenbook schedules** → one author-set, revenue-matched
  intensity per lever.
- **Full curve-shift conditional schedules (§B)** → a few conditional
  re-baselines at the single matched intensity.
- **Revenue-maximizing-rate exhibits (§B.1, ordinary + CG revmax)** → cut.
- **Standalone ETR section (§D)** → cut as a section; ETR survives only as the
  per-card static+conventional chart.
- **Named coherent packages (§C), separate robustness section (§E),
  full-factorial explorer (§F)** → cut.
- **Multi-intensity full-factorial frontier (~5,500 runs)** → stays dropped. The
  128-combo factorial (§2.6) is single-intensity-per-switch and bounded; it is
  NOT a revival of the intensity frontier.
- **NIIT expansion** → dropped from v1 entirely (not a switch, not a card).
- **Estate / QBI / gains-at-death as standalone lever cards** → not headline
  cards (the four rate levers are), but they ARE binary switches in the §2.6
  factorial and surface via the conditioning panels + explorer.

## 6. Credibility gates

*Triaged with the author 2026-07-08; still current.*

1. **Hidden-wealth cross-base consistency — ✅ BUILT** (`7b46a169c`). Avoidance
   now shrinks reported income + estate bases (hidden ledger), with the reverse
   `evasion→wealth` link. Verified (estate −$7–8B/yr, IIT −$34–35B/yr,
   wealth own revenue unchanged, χ=0 no-op exact).
2. **Estate own-rate responsiveness — OPEN, but DE-GATED for MVP v1.** Estate is
   no longer a standalone lever card (D0) — it appears only as a conditioning
   baseline (§2.2). The zero own-rate response therefore affects only the
   fidelity of the conditioning base, not a headline number; disclosed where
   estate conditions the wealth card. Kopczuk–Slemrod reported-estate elasticity
   + charitable-bequest response remains the contained build if estate is
   promoted to a card later.
3. **0.431-vs-0.2505 reconciliation — ✅ RESOLVED** (stale σ=0.6 number).
4. **Corporate constants — placeholder, disclosed. Now gates a headline lever.**
   Corporate is an MVP rate lever (D0), so the CORP_* Phase-0c placeholders sit
   under a headline card — status disclosed on the card. Corp-alloc stock
   exposures in the ETR file share the placeholder status → shown as a
   convention band on the ETR charts.
5. **Wealth-avoidance elasticities — ✅ AUTHOR-ACCEPTED** (−7 / −17 centrals).
6. **Migration/expatriation — ✅ RESOLVED** (subsumed in the reduced-form
   wealth-avoidance elasticity). Wealth card flagged as a ceiling on this basis.

## 7. Consolidated author decisions (v3)

| # | Decision | Ruling (2026-07-09) |
|---|---|---|
| D0 | MVP lever set | **Four rate levers**: top ordinary, top capital-gains, corporate, wealth rate. Estate/QBI/NIIT/death-regime → conditioning baselines only |
| D1 | Lever intensities | **Author sets each by hand**, calibrated to a common revenue neighborhood; §2.3 table to fill |
| D2 | Conditional re-baselines per lever | Defaults in §2.2 table (conditioning policies drawn from the non-lever structural pieces + other levers); author may edit |
| D3 | Corporate in v1 | **Include** at placeholder CORP_* constants, disclosed |
| D4 | ETR object per card | **Static (welfare) + conventional (realized)**, HS, top 1% & 0.1%, gap = avoidance DWL; corp-convention band |
| D5 | Everything cut | revmax, standalone ETR section, packages, robustness section, factorial explorer, multi-intensity schedules, non-rate standalone cards |
| D6 | Interactive layer | modest at most (serve exact runs); default = link run-level CSVs |
| D7 | Leakage decomposition on the card | **Object A (destination ledger / by-head waterfall) only** — convention-free, no extra runs (card item 3). Object B (mechanism attribution) deferred; decide depth after seeing A |
| D8 | Run set | **7-switch binary factorial = 128 combos** (§2.6): 4 rate levers + step-up/deemed + estate current/Clausing-2009 + QBI on/off. Cards + conditioning panels + explorer are all slices of it. Cost the all-on combo before launching the batch |
| D9 | Clausing-2009 estate variant | **Clausing-Sarin** `clausing/07_estate` ($5M, flat 45%), re-anchored to 2027 |
| D11 | Effective year / framing / window | **2027, unexpected (surprise)**, window **2027–2036** (sim `years=2026:2037`); **v1 is 10-year only, no 30y**; switches independent (CG fixed 39.6, NIIT stacks → 43.4) |
| D10 | Parts-vs-whole exhibit | **Naive sum vs. actual**, 3 bars (Σ static-standalone → Σ conv-standalone → actual combo conv), 10-yr window; convention-free, no extra runs (§2.7). Headline for full-stack combo; per-combo in the explorer |
