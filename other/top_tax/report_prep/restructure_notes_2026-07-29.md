# Restructure notes, 2026-07-29 conversation

You're not floundering, actually — I think you just resolved it and haven't
noticed. Let me play back what your three answers plus the Leiserson comment
add up to, because they're mutually consistent in a way your draft isn't yet.

Leiserson gave you the taxonomy and you should just take it: the estimate has
three pieces — the fully static piece, base interactions, and behavioral
responses. Your confusion ("is this stuff static or mechanical?") comes from
treating those two words as a dichotomy when they're answers to different
questions. Static asks: does the base move? Mechanical asks: does anyone
optimize? The corporate offset and decumulation are mechanical AND base-moving
— nobody chooses anything, but the base isn't fixed either. So they're not
static, and they're not behavior. They're their own category, which is exactly
why Leiserson named it separately instead of cramming it into either side of
the static/conventional line.

And here's the part that should relieve the anxiety about the
static/conventional split: nothing conceptual rides on it. It's a production
convention — it tells you which pass of the model a number came from, and it
matters for comparability with CBO and JCT. But the report doesn't have to
organize its argument around where the model's passes happen to draw the line.
The model draws it at "does the base move," which puts mechanical interactions
and avoidance in the same bucket; your argument wants to draw it at "does
anyone optimize," which separates them. Fine — the headline scores stay
conventional in the standard sense, and the decomposition is a presentation
choice layered on top. You can write around the word "static" exactly as
Leiserson suggests, by naming each piece directly.

## The ladder

What you actually have is a ladder, and it's the same ladder as your "you have
to do economics" steelman — each rung is one more thing bean-counting misses:

1. **The pool is finite.** Pure arithmetic on today's base — Table 1, the
   100-percent-rate ceiling. No economics yet.

2. **The pool is shared.** Across instruments (a corporate dollar comes partly
   out of the shareholder-income base) and across time (a dollar financed from
   wealth isn't there to generate investment income or estate value later).
   This is accounting and incidence — still zero optimization. Decumulation
   goes here; it's literally mechanical for a given saving rate. The honest
   footnote is just that the saving rate itself is a fixed behavioral
   parameter, not a response we model.

3. **The pool responds.** Deferral, recharacterization, entity shifting,
   evasion. This is the only rung where "avoidance," "inefficiency," and
   welfare-cost language belong — and calling avoidance inefficient is a
   positive claim, not a normative one. The word that was causing trouble was
   never "inefficiency," it was "leakage" smeared across rungs 2 and 3 as if
   they were the same substance.

4. (**Real activity responds** — labor, saving, entrepreneurship. Out of
   scope, the dynamic-scoring parenthetical.)

The current section already has this structure — the three reasons are the
three rungs. The draft's problem is entirely downstream: the summary metrics
("collects 63 cents per statically-assessed dollar," the frontier's vertical
axis, "efficiency-enhancing") collapse rungs 2 and 3 back into one number and
then reach for a welfare word to describe the blend. That's the precise
location of the wrongness. Not the section, not the split, not the modeling
emphasis.

## The modeling worry

The memo's thesis and the report's thesis are the same thesis — revenue from a
package is jointly produced by overlapping bases and regime-dependent
behavior, so packages must be scored in one integrated model. The report
shouldn't argue the modeling point, it should use it: the model is what lets
you put a number on each rung separately, which no bean-count and no
bolted-together set of standalone scores can do. That's the distinctive
contribution, quietly demonstrated rather than announced.

## Busy-ness

The decomposition needs no new visual machinery. Figure 3 keeps its two bars;
the text walking through the corporate policy already naturally separates
"shifts to pass-through form" (rung 3) from "reduces after-tax profits and
thus shareholder income" (rung 2) — it just needs one sentence totting them
up: of the 37 cents not collected, roughly this much is the double-count
correction and this much is avoidance.

The frontier keeps its axis but gets a bloodless label — collected minus
assessed, per dollar — and one line of text noting that the composition of
that gap differs by package: corporate-heavy packages sit low mostly for
accounting reasons, capital-gains-heavy ones mostly for behavioral ones. That
is a genuinely interesting result on its own — two packages with the same
collection rate can differ completely in how much of their gap is real social
waste — and it's only sayable because of the integrated model.

## Two open questions

1. Leiserson's aside about distribution tables — that a properly built table
   should carry some base interactions (the corporate incidence attribution
   certainly; arguably the decumulation incidence too), so the distribution
   isn't "static" in his sense either. The tables do smear corporate tax to
   households already. Should the report say which rungs the distribution
   tables include, or is that a methods-memo matter?

2. Naming: the tool already speaks in "ask" versus "collected." Should the
   report adopt that pair as its running spine — assessed, after interactions,
   collected — or is "ask" too colloquial for the register? Picking the
   ladder's vocabulary once and using it in section headers, figures, and the
   tool turns the piece from a collection of facts into one walk down a single
   staircase.

---

# Addendum: decomposition numbers, the overlap taxonomy, and the two big questions

## Do we need to re-run to split the wedge? (question 1)

No. The two mechanical pieces are recoverable from existing v5 output:

- Corporate overlap: `conventional/supplemental/corp_conservation_diag_{t}.csv`
  per scenario-year (analytic-vs-realized by line; source of the 8-cents
  figure).
- Decumulation: conventional minus the conventional-no-wealth pass
  (`conventional_no_wealth/detail/`, behavior on / haircut off; detail only,
  so it needs aggregation, which is now done -- see below).
- Behavior: the residual.

Caveats to disclose: the corporate mechanical piece is inferred (income flows
x effective rates), not scored -- the exact number would need one cheap re-run
(`behavior: default` + an economy alternative pinning `corp.rate_eti: 0`,
since the corporate avoidance ETI lives in the economy leg and is applied per
pass type, not gated by the behavior leg). And the decomposition is
order-dependent (behavior operates on the marked-down, haircut frame) -- the
same disclosure already made for stacking order.

## The numbers (30-year CY totals, top_tax_dials_30y_v5)

| Scenario          | Static  | Conv    | Wedge  | Decumulation | Decum share |
|-------------------|---------|---------|--------|--------------|-------------|
| Corp 21->28       | $7,399B | $5,377B | $2,022B| $81B         | 4.0%        |
| CG 20->30         | $4,876B | $1,825B | $3,051B| $211B        | 6.9%        |
| Corp 35 + CG 30   | $19,675B| $11,308B| $8,367B| $374B        | 4.5%        |

Corporate external-income cut (dY_exog): -$905B corp-alone, -$1,729B package
over 30y. At plausible top effective rates on those flows this reproduces the
~8 cents per corporate dollar in the draft, i.e. roughly a quarter to a third
of the corporate wedge is mechanical. Decumulation compounds as the draft
claims: $0.8B/yr (2028) -> $32B/yr (2057) for the package.

The sentence this buys: for the corporate increase, roughly a third of the
uncollected 27 cents is the shared-pool correction rather than avoidance; for
capital gains, the 63-cent gap is essentially all behavior, with decumulation
contributing a few cents. Punchy because heterogeneous -- the two flagship
dials have opposite gap compositions, which is the payoff of separating rung 2
from rung 3 and the one line of text the frontier's relabeled axis needs.

Raw legs: other/top_tax/report_prep/decomp/decomp_legs.csv (script + SLURM
log alongside; job 20318842).

Verdict: the wedge is behavior-dominated everywhere; mechanical is
quantifiable from existing output to the precision a sentence needs. A re-run
is justified only if the middle bar (assessed -> after interactions ->
collected) should be drawn in Figure 3 rather than stated in text.

## The overlap taxonomy (is it just corporate + decumulation?)

Sort every mechanical overlap by two questions: same person or different?
same year or across years?

1. Same person, same year -- statutory stacking: deemed-realization tax
   shrinking the estate base via the income-tax-at-death deduction, NIIT on
   ordinary rates, wealth tax repricing estate values. Inside the law, so the
   static number already nets them; they never appear in the wedge. Worth one
   sentence in the report: the assessed number is already
   interaction-inclusive within the law, which is why summing standalone
   static scores is wrong even before anyone behaves.

2. Different person, same year -- corporate incidence: the entity's tax
   landing in shareholders' and bondholders' income and estates. The only
   cross-person mechanical channel in the model; must live in the wedge
   because static holds everyone's income fixed.

3. Same person, across years -- decumulation: the overlap story for taxes
   assessed on one person, projected over time. The dollar taken this year
   isn't in the estate, and isn't generating dividends, twenty years from now.

4. Different person, across years -- the death interface itself. Step-up,
   carryover, and deemed realization are the rules for how the pool passes
   between persons across time. The star reform is not outside the taxonomy;
   it is the fourth cell -- which is another way of seeing why it does double
   duty (it changes both the reach of the base and the elasticity of the
   response).

So: in the conventional wedge, the mechanical layer is literally corporate
overlap plus decumulation -- the only two base-moving mechanical channels on
that side of the model. But base overlap as a concept has four kinds; two hide
inside the assessed number, and one is secretly the subject of Section 5.

## Structural implication (question 2)

The ladder should be the report's spine: economic income at the top -> income
the tax system reaches -> tax assessed -> revenue collected. Sections map to
it:

1. How much income is at the top (cash vs accrual; Fig 1) -- defines the
   topmost quantity.
2. How much does the tax system reach (the "progressive but narrow" section
   reframed from progressivity verdict to statement of reach; Fig 2 as
   evidence about reach).
3. The arithmetic ceiling (Table 1 stands alone) -- bounds "assessed."
4. From assessed to collected -- the wedge, explicitly two-part: shared pool
   (overlap + decumulation, accounting corrections) and responding pool
   (avoidance, the only part carrying efficiency language). Fig 3 with the
   one-sentence split.
5. Design changes the ladder -- the death regime operates on two rungs at
   once: it widens the reach (unrealized gains at death enter the assessed
   base) and shrinks the response (realization elasticity 0.6 -> 0.3).
   Carryover only does the second, weakly. No other reform in the piece has
   that dual character.
6. The tool -- frontier axis relabeled to collected-minus-assessed per
   dollar; one line noting gap composition varies by package (corporate-heavy
   sits low mostly for accounting reasons, CG-heavy mostly behavioral).

Surgery required: split the ceiling from the wedge; split the wedge in two;
reframe progressivity as reach; adopt one vocabulary (assessed / interactions
/ collected, or ask / collected) across headers, figures, and the tool.

---

# Open: which ranking convention for the accrual series (UNDECIDED 2026-07-29)

The cash-versus-accrual effective rates in Figure 2 and the accrual prose can be
computed two ways, and both are in the v5 output. Nothing here is stale; the
sources just disagree on convention.

| Deliverable | Ranking | Top 1% accrual ETR |
|---|---|---|
| Interactive (ETR view, distribution card) | fixed, by cash income | 17.6% |
| `metrics_results.md` | fixed, by cash income | 17.6% |
| Figure 1 / Figure 2 | self, by own measure | 14.9% |
| Report prose | self, by own measure | 15% |

Under fixed ranking the group is held still by cash income and only the
denominator changes. Under self-ranking each group is re-selected on the measure
being used, so the accrual groups are different households: the top 1% floor
moves from $0.91M of cash income to $1.64M of accrual income, and both the
numerator and the denominator move. At the top 0.01% the tax falls from $358B to
$260B while accrual income rises from $1,512B to $1,714B.

The two conventions disagree about the shape, not only the level. The cash-ETR
minus accrual-ETR gap narrows toward the tail under fixed ranking, 8.5 points at
the top 10% to 6.3 points at the top 0.01%, and widens under self-ranking, 9.7
points to 14.8 points.

Comparison figure: `fig2_ranking_compare.html`, published at
https://claude.ai/code/artifact/4d6271cf-f88c-40ff-b73c-c32f1173737b

Arguments on each side. Fixed ranking isolates the reach of the tax code by
holding the households still, matches the interactive and the metrics doc, keeps
the tool's cash/accrual denominator toggle meaningful, and states the more
conservative gap. Self-ranking measures concentration under each definition on
its own terms, is the more striking picture, and is the convention behind the
top-400 breakout. Against it: part of the widening toward the tail is the
selection rather than the tax code.

Switching the figures to fixed ranking costs two arrays in
`fig1_2_income_rates_v5.html` (`hsLvl` to [17.9, 14.0, 7.8, 3.4, 1.5], `hsETR` to
[15.4, 15.8, 17.6, 21.5, 23.7]), the accrual shares off the $32.3T accrual total
(top 1% share 27% to 24.0%), two numbers in the prose ($8.7T to $7.8T, 26-to-15
to 26-to-18), and dropping "self-ranked" from the figure eyebrow. Switching the
tool the other way would repoint `ranking` in `extract_atlas_data.py` and
`build_dist_card_data.py` and rebuild. No model run either way.

Also unused by any deliverable: `hs_ex_home`, which under fixed ranking gives
[16.3, 16.5, 18.1, 21.7, 23.8]. The exclusion of primary-residence accruals moves
the top 1% accrual rate by half a point.
