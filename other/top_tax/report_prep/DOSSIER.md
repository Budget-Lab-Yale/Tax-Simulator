# Report-prep dossier: "Taxing the top"

*Prepared overnight 2026-07-16/17 for JR, ahead of drafting the public report. This is
input material — angles, numbers, critiques, factoids — not a draft. Everything here is
meant to be argued with.*

**Companion files in this folder** (`other/top_tax/report_prep/`):

| File | What it is |
|---|---|
| `metrics_results.md` | Full output of `report_metrics.py` — every number quoted below, with more precision |
| `report_metrics.py` | The script (rerunnable; reads dials v2 + kg_v3_revmax + atlas2_data.json) |
| `model_mechanics_review.md` | Plain-English mechanism inventory, interaction map, novelty claims, omissions register, 20 one-liners |
| `outline_critique.md` | Independent critique of the director's outline: angles, overlooked points, three alternative structures, traps, titles |
| `external_factoids.md` | Sourced external numbers: CBO Feb-2026 baseline, PSZ vs Auten-Splinter, CEA 8.2%, CBO/JCT/Greenbook option scores, wealth-tax literature, ITEP, OBBBA hooks |
| `output_data_map.md` | Where every number lives on disk; scenario-name decoder; vintage provenance and gotchas |
| `networth_by_group.py` / `.sbatch` | Net-worth-by-group aggregation from baseline detail (results appended at bottom of this file) |

---

## 1. The ten things I'd want you to see first

1. **Your placeholders are all computable today** — §2 below fills every $XX/Y%/Z% in the
   draft. Headlines: top 1% cash income 2027 **$5.2T** (accrual **$7.8T**), taxes
   **$1.4T**, ETR **26.2% cash / 17.6% accrual**, income floor **$911k**.

2. **The draft's "negative interaction tends to be small" claim needs a rewrite — the
   model's headline package interaction is *positive*.** The full 8-lever reference stack
   raises **$10.6T conventional over FY2027-36, 3.3% MORE than the sum of its standalone
   conventional scores**. Rate-on-rate interactions are indeed small and negative
   (corp28×cg30: −0.4%), but the structural switches (deemed realization, estate reform)
   raise *other* levers' yields by more than the rate hikes erode each other. The honest
   sentence is something like: "rate increases erode one another modestly; base reforms
   subsidize everything around them."

3. **The Figure 4 story confirms beautifully, with the sign flip intact.** CG→25% +
   deemed realization (current physics, surrogate-composed): Σstatic **$838B** < Σconv
   **$942B** < package conv **≈$1.11T**. Direct runs on the (stale-physics) revmax grid:
   $835B / $1,086B / **$1,263B**. Behavior *adds* revenue — leakage is negative — exactly
   the section-4 thesis.

4. **"Policy chooses its own elasticity" is now a measured output, not a slogan.** The
   model's implied realization semi-elasticity under a CG-rate hike is **−2.48** under
   step-up; under deemed realization the measured realization response *flips sign*
   (mean **+1.2** in `kg_dynamics_summary` — realizations rise, since deferral no longer
   escapes anything and the wealth-carry/timing margins unlock; note the +1.2 is a
   package diagnostic — it includes the regime's own realization effects, not a clean
   conditional elasticity). See metrics §11 and the Laffer table: the same +5pp CG hike
   yields **$338B conventional under step-up vs $515B marginal on top of deemed** (stale
   direct grid: 1,263−748); current-physics surrogate: **$235B solo vs ≈$400B
   conditional-on-deemed** at cg=25.

5. **The survival-ratio gradient is the single best "one chart" candidate nobody asked
   for**: conventional-as-share-of-static by lever (10y, current physics) —
   QBI repeal **0.99**, taxmax **0.99**, corporate **0.88**, ordinary **0.82–0.87**,
   wealth **0.55–0.69**, capital gains **0.43 → 0.29** (falling as the rate rises), and
   the death-regime/estate reforms **>1** (carryover 6.35, deemed 2.43, estate 1.17).
   One picture: which buckets leak, which flow uphill.

6. **The Laffer curve has a legal ceiling baked in, not just a behavioral one.** The
   calculator (correctly, per Sec. 1(h) lesser-of logic — `src/calc/functions/tax/tax.R:174`)
   never taxes gains above ordinary treatment, so once the statutory CG rate passes the
   37% ordinary top rate, even the *static* score flattens (static 10y: $1,891B at 40%,
   $1,925B at 45%, $1,942B at 50%). Figure 5 needs this footnote or readers will read the
   plateau as pure behavior. (It also means "raise the CG rate to 50%" is largely
   symbolic without an ordinary-rate increase — a nice analytical aside.)

7. **The destination ledgers quantify every "a dollar here erodes a dollar there" claim
   in the draft** (metrics §6, FY2028-36): corp28 → **−$98B income tax** per +$851B
   corporate (≈11.5¢ offset per gross dollar, endogenous, not assumed); wealth 2%>$50M →
   **−$450B income tax, −$87B estate, −$43B corp** against +$3.9T own take (~15%
   cross-erosion); taxmax → +$2.77T payroll with **−$398B income tax**; and the estate
   hike **adds +$49B of income tax** (unlocked realizations — a *positive* spillover no
   scorekeeper shows).

8. **Fiscal-context corrections from CBO's Feb-2026 baseline**: the $1.9T deficit is
   **FY2026**, not 2027 (relabel or reword); $3.1T in 2036 is right; cumulative
   FY2027-36 deficits ≈ **$24.4T**. Your doubling-the-ETR example computes cleanly:
   doubling the top 1%'s 26.2% ETR with a frozen base ≈ **+$16.1T over ten years ≈ 66%
   of the ten-year deficit**. Also: top ordinary rate is **37% permanent** (OBBBA), and
   the estate exemption **$15M is verified**.

9. **The full stack raises ~2.7% of GDP per decade and its survival decays 73% → 67% →
   62% across the three decades** — the 10-year window systematically understates
   base-drain for rate hikes and understates recapture for death-regime reforms. A
   30-year exhibit (even one line) would be genuinely novel for a public piece.

10. **One reconciliation must happen before publication**: the numbers in this dossier
    come from `top_tax_dials_30y_v2` (2026-07-12 physics: η=2.4825, σ=0.16,
    wealth-carry + estate-margins in — i.e., *matching the methodology memo*), but
    (a) the estate-avoidance activation bug means **estate-lever scenarios lack the
    Kopczuk–Slemrod own-rate response** (fixed in code 2026-07-16, not yet re-run);
    (b) the interactive's contribution-card JSON (`dist_card_data.json`) is still pinned
    to **v1** physics; (c) the revmax death-regime grid predates the η re-pin. A re-run
    of the dials batch + revmax grid on current code would make every number quotable.
    Until then, quote v2 numbers and disclose the estate-lever caveat.

---

## 2. Filling the draft's placeholders

All from `top_tax_dials_30y_v2` baseline (2027 cross-section unless noted); taxes =
federal iit + payroll + estate + deemed + wealth + corp (+vat), corp allocated by
capital income. Full tables in `metrics_results.md` §§1–4.

| Draft placeholder | Value | Note |
|---|---|---|
| Top 1% realized income, 2027 | **$5.2T** | "cash/expanded" concept |
| ...equal to income of bottom XX% | **bottom ~66%** | accrual basis: bottom ~71% |
| Top 1% accrual income, 2027 | **$7.8T** | +49% over cash; "considerably more uncertainty" is right |
| Taxes remitted by top 1% | **$1.4T** (2027) | not multiple trillions — "$XX trillion" needs the singular |
| Top 1% ETR realized / accrued | **26.2% / 17.6%** | top 0.1%: 29.2/21.5; top 0.01%: 30.0/23.7 |
| Top 1% income floor | **$911,000** | top 0.1%: $4.24M; top 0.01%: $21.4M |
| Deficit 2027 → 2036 | **$1.9T is FY2026**; FY2036 $3.1T ✓ | cumulative FY27-36 ≈ $24.4T (CBO Feb 2026) |
| Doubling-ETR stylized example | **+$16.1T/10y frozen-base ≈ 66% of the 10-year deficit** | uses 2027→2036 growth of the model's own baseline |
| Estate tax threshold | **$15M** ✓ (2026, permanent, indexed from 2027) | |
| Figure 3 package (all to 25%) | naive conv **$686B**, package **$685B** (interaction ≈ −$1B) | surrogate estimate; corp dial is linear by construction — see §3 |
| Figure 4 package (cg25 + deemed) | Σstatic $838B < Σconv $942B < package **≈$1.11T** | current physics; direct stale-grid runs: 835/1,086/1,263 |
| Top-group income shares | top 1%: **23.2% cash / 24.0% accrual** of total | top 0.01%: 5.3%/4.7% |

Two wording flags on this section of the draft: use "**accrue**," not "receive," for the
accrual measure (the whole deferral story depends on the distinction), and note the
accrual number sits inside the PSZ-vs-Auten-Splinter measurement fight (top-1% share
20.7% vs 15.5% in 2022 — see `external_factoids.md`), so "no disagreement" should be
scoped to the *realized level*, not the accrual magnitude or the trend.

## 3. Figure-by-figure notes

**Figure 1 (income by top group, cash vs accrual).** Data exists: `atlas2_data.json`
`income_levels` (2027) or any scenario's `distribution_etrs.csv` baseline columns (2027 +
2036, all three income definitions). Consider showing the *ratio* (accrual/cash: 1.49 at
top 1%, 1.27 at top 0.01%) as an annotation — it quantifies "unrealized gains play an
outsized role." Note the counterintuitive wrinkle: accrual/cash *falls* at the very top
in 2027 because realized gains are already a huge share of top-0.01% cash income.

**Figure 2 (taxes / the tool's distribution graph).** The tool's built section is titled
"Who holds the income — and what the plan takes." The data behind the report version
should be regenerated from v2 (the standalone `dist_card_data.json` is v1-pinned).
Baseline taxes by group are in metrics §2. For the second variant's net-worth bar chart,
see the appended net-worth-by-group table at the bottom of this file.

**Figure 3 (parts vs whole, rate package).** As literally specified (corp→25 + CG/div→25)
there is **no direct run** — corp runs exist only at 28% (single OME wedge). Two honest
options: (a) keep 25/25 and present the surrogate estimate (naive conv $686B, package
$685B — the corp dial is linear in Δτ by construction, so corp-25 = 4/7 × corp-28; the
interaction term is ≈ −$1B); or (b) switch the figure to the direct-run package
**corp→28 + CG→30** (`pc_corpr28_cgr30`): Σstatic $1,988B, Σconv $1,193B, package conv
$1,189B (interaction −0.4%). Option (b) is cleaner to defend; option (a) matches the
prose. Either way the interaction bar will be invisible at this scale — consider
printing the number on the chart rather than drawing it. Caveat to carry: corporate
constants are Phase-0c placeholders; the corporate share of this package is the least
settled number in it.

**Figure 4 (cg+5pp + deemed realization).** Confirmed, and it is the paper's best
exhibit (see §1.3). Recommend adding the *static-vs-conventional definition box between
Figures 3 and 4* — the sign flip is the subtlest idea in the piece
(`outline_critique.md` §C1 has the exact language problem). If you want direct-run bars
rather than surrogate composition, the revmax grid has all three cells — but it is
pre-η-re-pin; a 6-scenario re-run (cg_05pp_{stepup,deemed} + cg_00pp_deemed + baseline)
on current code would pin the figure.

**Figure 5 (Laffer curves by death regime).** Two sources: current-physics curves
(dials v2: step-up direct at 6 knots; deemed/carryover conditionals surrogate-composed —
metrics §8a) or the direct stale grid (metrics §8b). Shape notes: under step-up the 10y
curve is essentially flat from 40% up (and $564B at 50% vs $563B at 45% — the peak is a
plateau, not a spike); under deemed the conditional yield keeps climbing through 50%.
Two required footnotes: (i) the Sec. 1(h) ordinary-treatment ceiling (§1.6) — part of the
plateau is law, not behavior; (ii) NIIT stacks +3.8pp on everything shown. Also consider
plotting *30-year* revenue — the regime gap widens with the horizon (deferred gains land
at death), which is the analytically honest way to show carryover's in-between-ness: solo carryover
raises $252B/10y vs deemed's $706B, and at 30y $1,334B vs $3,183B — the ordering is
stable but the dollar gap roughly quadruples ($454B → $1.85T) as deferred gains land at
death.

**Variant-2 Figure 2 (aggregate net worth by group).** Computed from baseline detail —
table appended below (sbatch job). Ranking convention is simple tax-unit expanded-income
rank; label it as such.

## 4. Angles worth considering (beyond the outline's)

The two agent files have the full lists (14 angles in `outline_critique.md` §B; 20
one-liners in `model_mechanics_review.md` §5). The ones I would actually push, with the
numbers now attached:

1. **"Policy chooses its own elasticity"** — carried by Figure 5 plus the measured
   semi-elasticity flip (−2.48 → +1.2). This is the project's genuinely new quantified
   claim; frame as a capability ("because realization behavior is a model *output*, we
   can trace how the elasticity moves with the death regime"), soften "no one else has
   quantified this" to "to our knowledge."
2. **The leaky-bucket gradient** — the survival table (§1.5) as one chart. It reframes
   the whole piece from "how much money is there" to "which taxes keep what they ask
   for," and it makes base-broadening (QBI 0.99, taxmax 0.99) look as good as it actually
   is without editorializing.
3. **Deferred is not gone** — under the stack, **$3.3T of gains are deemed-realized at
   death within the first decade** (metrics §11); under current law those dollars exit
   through the step-up drain. Pairs with the destination ledgers: the money doesn't
   vanish, it relocates or waits.
4. **The estate hike that raises income tax** — the +$49B income-tax spillover from the
   estate lever is a tidy, counterintuitive proof that the model's cross-base wiring is
   real, and it runs *against* the report's general "levers erode each other" grain —
   which is exactly why it's credible.
5. **A wealth tax is an income surtax in stock units** — the memo's own arithmetic
   (1% at a 4% yield = 25% income surtax; ε=−7 ≈ ETI 0.28 in flow units). Best
   inoculation against "your avoidance elasticities are huge" *and* against
   "wealth taxes are free money."
6. **The 30-year lens** — survival 73→67→62; ~2.7% of GDP per decade; deferral
   recapture and bathtub drain both live mostly outside the 10-year window. No other
   public scorer shows this.
7. **The two ETRs (ask vs collected)** — under the full stack the top 0.01% accrual ETR
   goes **23.7% baseline → 48.9% asked → 40.9% collected** (v2 physics; the 37.3% in
   older notes was v1). The 8pp gap is the avoidance margin; label it "first-order
   welfare burden vs realized revenue," not "deadweight loss" (the evasion slice is a
   transfer).
8. **Payroll is not absent at the top, it's just capped** — the taxmax lever alone
   raises $2.5T/10y at 0.99 survival; that coexists awkwardly with the draft's "payroll
   taxes touch little to no income at the top" (also NIIT/Additional Medicare already
   reach top income). Precision here preempts a cheap rebuttal.

## 5. Analytical points the current draft overlooks

(Deduplicated against `outline_critique.md` §C, which has twelve — the ones below are
either mine or sharpened by the computed numbers.)

1. **The interaction-sign correction (§1.2).** As drafted, section 3 says cross-tax
   interactions are negative-and-small and section 4 says packages beat parts — but the
   stack-level fact is that the package interaction is *positive* once structural
   switches are in the mix. The two sections can be unified: "rate × rate: small,
   negative; rate × base-reform: large, positive." That is a better organizing insight
   than either sentence alone.
2. **Static ≠ "no interactions."** The draft's Figure-3 text calls the top bar "no
   interactions and no taxpayer response." In this model the static score *does* include
   mechanical law interactions (e.g., deemed tax deductible from the estate) — what it
   freezes is behavior. Precision here matters because Figure 4's whole point is a
   static-vs-conventional comparison.
3. **The Sec. 1(h) ceiling (§1.6)** — without it, Figure 5's plateau will be read as
   pure lock-in, and "the revenue-maximizing CG rate is ~45%" overstated: past ~41%
   statutory the dial mostly stops binding mechanically. (Related: the CG dial moves
   qualified dividends too — say so once, since "capital gains and dividend rates" is
   doing quiet work in the draft's package definitions.)
4. **What "conventional" excludes, stated as a floor.** No labor supply, no real saving
   response, no investment/growth effects; every behavioral number is reporting, timing,
   avoidance, or financing. So the behavioral haircuts here are a *floor* on true
   economic cost — a framing that both disciplines the "just raise rates" reading and
   preempts the "you ignored growth" attack. (The bathtub is an accounting channel, not
   a savings elasticity; the memo says this well — keep it in the report.)
5. **σ is a residual: the ETI decomposition is not separately identified.** The model
   matches ETI=0.25 by construction; what it adds is *where* the response goes (which
   bases, when), not an independent estimate of *how big* it is. The report should lean
   on destination claims, not magnitude claims, for the conversion channel.
6. **The wealth-tax cross-base drains are the least-disciplined numbers in the model**
   (elasticity bundle → concealment split is a structural ruling), while the wealth
   lever's own-base revenue is a disclosed ceiling (migration folded in). Quote the own
   number with a band, and the drains as direction + order of magnitude.
7. **The deemed-realization *level* is JCT-anchored** (25% valuation/compliance discount
   calibrated to land near JCT's ~$600B) — the novel content of Figures 4/5 is the
   *elasticity shift*, not the deemed revenue level. One sentence of disclosure buys a
   lot of credibility.
8. **Estate-lever numbers currently understate avoidance** (activation bug, §1.10):
   the estate solo's 1.17 survival would drop somewhat with the Kopczuk–Slemrod response
   active (expected give-back ≈ 13% of the estate delta per the margins build). Either
   re-run before publishing estate numbers or footnote the omission.
9. **First-decade FY plumbing**: FY2027 deltas are 0.75-weighted (surprise enactment
   CY2027, no 2026 lead-in in the dials vintage) and estate/wealth legs book FY+1 —
   single-year numbers near the window edges are conventions, not economics. Quote
   window totals.

## 6. Organization

`outline_critique.md` §D has three full structures. My recommendation is its **Alt 3**
(keep the director's five-beat spine, tightened): parts-vs-whole becomes the thesis
exhibit *inside* the rate-raising section; Figures 3 and 4 become an explicit
before/after pair with a static-vs-conventional definition box between them; the deficit
becomes a recurring yardstick rather than a one-time mention; realized income leads,
accrual follows with its uncertainty flagged. Two further notes:

- **The interactive section should use the tool's real section names** — the built atlas
  is titled "How much revenue is there at the top?" with sections "Build a package,"
  "Who holds the income — and what the plan takes," "Standalone scores vs. the combined
  package," "What each policy does to the other bases," "The frontier: how much is
  there, at what price?," and "The capital-gains policy in three dimensions." The
  outline's names (Distribution / Parts vs. Whole / Spillovers) don't match — either
  rename the tool sections or the prose. The outline also doesn't mention the frontier
  or the 3-D capital-gains surface at all; the frontier ("revenue vs. who pays")
  deserves a sentence, and the 3-D surface *is* Figure 5's interactive twin.
- **Caveats live next to the exhibit they qualify** (corporate placeholder on the
  corporate chart; conventional-only on the first revenue chart; wealth ceiling on the
  wealth dial), not in a terminal caveat paragraph.

## 7. Traps and honesty risks (top five of the ten in `outline_critique.md` §E)

1. "No disagreement that income is concentrated" — scope it to the realized level;
   name Auten–Splinter and PSZ (20.7% vs 15.5% top-1% share, 2022).
2. "Interactions are very small" — never unqualified (see §5.1).
3. False precision — surrogate ±2.5%, placeholder corp constants, residual σ, ceiling
   wealth elasticities: two significant figures everywhere, bands on corp and wealth.
4. The accrual ETR is not "the real rate they pay" — it's a single-year cross-section
   with a volatile denominator; the defensible claim is about deferral and forgiveness
   at death. (CEA's 8.2% is the same denominator move — cite it as a cousin, not a
   validation.)
5. The doubling example invites a strawman charge — show static and conventional side
   by side rather than build-then-demolish.

## 8. Vintage and staleness register (what's quotable tonight)

| Object | Vintage/physics | Quotable? |
|---|---|---|
| Income levels, ETRs, thresholds, taxes by group | dials v2 baseline (η, σ irrelevant to baseline) | **Yes** |
| Solo/pair/stack revenue scores, survival ratios, ledgers | dials v2 (2026-07-12 physics = memo physics) | **Yes**, with estate-lever caveat |
| Estate-lever scores (`s_estate_*`, estate-conditioned cells) | v2, missing KS own-rate response (activation bug) | Understates avoidance ≈13% of estate delta — footnote or re-run |
| "Ask vs collected" ETRs (stack) | v2 | **Yes** (note older 37.3% figure was v1) |
| Laffer grid by death regime (direct) | kg_v3_revmax, η=2.3992, pre-Tier-1 | Directionally yes; re-run before print |
| Contribution/dist card JSON | **v1** — one physics vintage behind | Regenerate before shipping the tool |
| Deficit/CBO numbers | External (CBO Feb 2026 via CRFB) | Yes; verify FY2027 exact figure against the CBO tables |

**Suggested runs before publication** (in value order): (1) re-run the dials batch on
post-2026-07-16 code (estate/avoidance activation fix; also picks up any σ/η follow-ups)
— this refreshes every headline; (2) re-run the 18-scenario revmax grid on current code
for Figure 5 (and consider extending to +30/35pp — memory notes the argmax hit the +25pp
boundary under the old spec); (3) if Figure 3 stays "all at 25%," author a corp-25 OME
(the channel is linear, but a real input beats a scaled one in a methods footnote);
(4) regenerate the contribution card from v2+.

## 9. Titles and pull-quotes

Ten titles and eight pull-quotes in `outline_critique.md` §F. The three I'd shortlist:
"**Taxing the Top: How Much Is Really There?**" (matches the tool's own headline),
"**The Same Dollar, Taxed Five Ways**," "**Deferred, Not Gone**." Best pull-quote of the
lot: *"How responsive the wealthy are to the capital-gains rate is not a fixed fact of
nature. It is, in part, a consequence of how we choose to tax gains held until death."*

---

## Appendix: net worth by group (baseline detail, dials v2)

*(From sbatch job 18580875, `networth_by_group.py` over `top_tax_dials_30y_v2` baseline
detail. Convention: non-dependent tax units, simple tax-unit ranking by expanded income
or net worth, no equivalence scale — so the income cutoffs here differ slightly from the
production distribution tables ($830k vs $911k top-1% floor); use the distribution_etrs
cutoffs in the report text and this table for the wealth aggregates only.)*

**2027** (187.4M non-dependent tax units; totals: income $21.4T, net worth $203.0T):

| Group | By income: floor / net worth held | By net worth: floor / net worth held |
|---|---|---|
| Top 20% | $146k / **$161.7T** | $814k / **$181.4T** |
| Top 10% | $226k / $136.1T | $1.82M / $158.1T |
| Top 5% | $326k / $110.1T | $3.66M / $134.4T |
| Top 1% | $830k / **$61.0T** | $15.0M / **$80.0T** |
| Top 0.1% | $3.87M / $25.9T | $75.0M / $34.7T |
| Top 0.01% | $18.8M / $11.1T | $238M / $16.1T |

**2036**: totals income $29.7T, net worth $290.3T; top 1% by income holds $86.4T; top 1%
by wealth holds $113.5T (floor $20.1M).

Factoids that fall out of this table:

- **The income-rich and the wealth-rich are overlapping but distinct groups**: the top 1%
  by income holds $61T of net worth; the top 1% by wealth holds $80T. Ranked by wealth,
  the top 1% *income* is only $3.6T (vs $5.2T income-ranked) — retirees and low-realizing
  holders dominate the wealth top. Useful for the draft's "income and wealth are highly
  correlated" note — correlated, yes; interchangeable, no.
- **The 2027 top-1% wealth floor (~$15.0M) sits almost exactly at the estate exemption
  ($15M)** — the estate tax is, almost by construction, a top-1%-of-wealth tax.
- Wealth concentration exceeds income concentration: top 1% share of net worth ≈ 30%
  (income-ranked) to 39% (wealth-ranked) vs a 23% income share.
- Aggregate net worth ≈ **9.5× aggregate cash income** (203/21.4) — the stock-vs-flow
  ratio that makes small wealth-tax rates equivalent to large income surtaxes.
