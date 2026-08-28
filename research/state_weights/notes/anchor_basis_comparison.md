---
title: "Which source sets the level: the anchor basis comparison"
role: notes
workstream: state_weights
status: open
updated: 2026-08-28
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Which source sets the level: the anchor basis comparison

**JI, 2026-08-27.** Measured by
`research/state_weights/nonfiler_residual/12_anchor_basis_comparison.R`;
per-state output in `nonfiler_residual/results/anchor_basis_comparison_{year}.csv`.
**Status**: the basis change is **approved and implemented** (S15, 2026-08-27);
the out-of-state question that gated it is resolved. What remains open is the
tolerance constant, which does not decompose, and the TY2022 adult-dependent
reading, which needs a table fetch.

## The question

The rule for this pair of sources should be that **Pub 1304 owns national levels
and HT2 owns state shares**, because that is what each is authoritative on and
it never differences them. Two facts about HT2 force the division: it carries no
age dimension and **no married-filing-separately series in any year** (only
`MARS1`/`MARS2`/`MARS4`), so any national quantity keyed on age or on MFS has to
come from the national by-size tables.

On the filer side that rule is already the design — `read_ht2()` drops the `US`
row and stub 0, and every target is a PUF national total distributed by HT2
state shares. It also *must* be: under the split-weight invariant
Σ<sub>st</sub> W[i,st] = w<sub>i</sub>, reweighting cannot move a national
total, so a national target on the filer partition is redundant if it agrees
with the PUF and infeasible if it does not.

**The non-filer anchors are the exception, and it is not deliberate.** The
national anchor takes filing adults from Pub 1304 Table 1.6; the state anchors
take them from the HT2 identities. Nothing reconciles the two, so the 51 state
anchors do not sum to the national anchor:

| non-filing adults 18+ | TY2017 | TY2022 |
|---|---:|---:|
| national anchor (T1.6 basis) | 47.342M | 46.538M |
| Σ state anchors (HT2 identity basis) | 46.697M | 47.534M |
| wedge | **−1.36%** | **+2.14%** |

The fit targets the state file. The plan quotes the national one. **The sign
flips between the two anchor years.**

## Part A — the 0.3–0.5% gap does not decompose

The tolerance script carries `E_FILING_ADULTS <- 0.005` on the strength of "the
two independent SOI routes disagree by −0.31% / +0.47%". Two of the differences
are identifiable, and they pull in opposite directions:

| filing adults | TY2017 | TY2022 |
|---|---:|---:|
| HT2 identity, 51 states | 206.132M | 213.080M |
| **+** out-of-state buckets (`OA`, and `PR` from 2018) | +1.044M | +1.209M |
| **−** T1.6 under-18 filers (HT2 has no age, so it cannot exclude them) | −2.191M | −2.147M |
| = comparable to T1.6 | 204.985M | 212.142M |
| Pub 1304 T1.6, filing adults 18+ | 205.487M | 214.076M |
| **unexplained remainder** | **−0.244%** | **−0.903%** |
| raw gap before decomposition | −0.313% | +0.467% |

**Naming the components makes 2022 worse, and flips its sign** (+0.467% →
−0.903%). So the quoted 0.3–0.5% is not one wedge; it is the net of two larger
offsetting universe differences plus a year-specific remainder up to 0.9%.
Two consequences:

- `E_FILING_ADULTS = 0.005` is **not conservative**. On the decomposed view the
  2022 disagreement is 0.9%, so the tolerances in
  `residual_tolerance_{year}.csv` are understated by roughly a factor of two on
  that component.
- Reconciling the two constructions is not a tractable route. Picking one
  authoritative level is.

**Both bases carry a universe error, in opposite directions.** The HT2 basis
subtracts under-18 filers from an 18+ population, understating the residual by
~2.1–2.2M. The national anchor subtracts returns filed from out-of-state, who
are not in PEP's resident denominator, understating it by ~1.0–1.2M.

## Part B — three bases, scored against each state's own tolerance

- **A** — current: `pep_st − ht2_filing_adults_st`
- **B1** — T1.6 national level × HT2 state share
- **B2** — (T1.6 level − out-of-state) × HT2 state share

A and B1 differ by a uniform scale factor on the subtrahend, so the per-state
effect is proportional to filing/residual — largest exactly where the non-filer
share is smallest, which is the same amplification structure the tolerance has.
The prediction is that the two offset. They do:

| | TY2017 | TY2022 |
|---|---|---|
| Σ A | 46.697M | 47.534M |
| Σ B1 | 47.342M | 46.538M |
| Σ B2 | 48.386M | 47.747M |
| B1 vs A, mean (range) | +1.56% (+0.80% DC to +2.81% SD) | −2.32% (−3.94% SD to −1.22% MS) |
| **B1 states outside own tolerance** | **0 of 51** | **0 of 51** |
| B2 vs A, mean | +4.09% | +0.50% |
| **B2 states outside own tolerance** | **43 of 51** | 0 of 51 |

**B1 is a consistency fix, not a change of answer.** It makes the state anchors
sum to the national anchor by construction, it inherits T1.6's adults-only
universe (which basis A cannot, having no age), and it moves **no state beyond
the tolerance the anchors already claim**, in either year. Recommended.

> **IMPLEMENTED 2026-08-27** in `02_build_residual_anchors.R`, as B2 once the
> out-of-state question resolved. `level_51 = T1.6 − out_of_state − qss`, and
> `residual_st = pep_st − level_51 × ht2_share_st`. The identity is asserted at
> build time. TY2017 48.470M, TY2022 47.803M; both bases retained as columns
> (`filing_adults_ht2`, `residual_nonfiling_adults_ht2basis`). Script 12 now
> asserts that its own arithmetic reproduces what the builder writes, so the two
> cannot drift apart silently.
>
> **The pro-rata age allocation is an assumption, and it is the one to suspect
> first** if the constructed pool's age validation disagrees at the young end.
> HT2's out-of-state bucket carries no age; the footnote covers overseas forces
> (18–34-skewed) and the much larger "citizens abroad" (not), and the bucket is
> a mailing-address artifact rather than a residency determination, so nothing
> in the data supports concentrating it. The choice moves the 18_25 band's
> residual by **+1.3% (pro rata) to +11.7% (all of it in 18_25)** and leaves the
> total unchanged either way.

**B2 is a substantive level change — and it is now the recommended one.**
Removing the out-of-state filers is right in principle, because PEP counts
residents of the 51 states, and the open question was whether `OA` is *citizens
abroad and the territories* (not PEP residents, so remove) or *domestic returns
whose state could not be classified* (PEP residents, so reallocate).

**Resolved 2026-08-27 (JI).** SOI's footnote on the Other Areas line reads:
*"Includes, for example, returns filed from Army Post Office and Fleet Post
Office addresses by members of the armed forces stationed overseas; and returns
filed by other U.S. citizens abroad."* Both populations are **outside** the
Census resident population, which excludes Armed Forces overseas and citizens
abroad; bona fide Puerto Rico residents are likewise outside the 51-state PEP.
So the level must have them removed, and **the basis is B2, not B1**.

Two things to carry with that. B2 moves 43 of 51 states outside their own
tolerance in TY2017 — that is now evidence the correction *matters*, not an
argument against it, and the tolerances are themselves understated (Part A).
And the change is consistent with the filer side rather than a new omission:
the weights already carry **53** jurisdictions (51 + PR + OA), so those returns
have a destination in the fit; what was wrong was subtracting them from a
resident population that never contained them.

*Provenance note:* the footnote wording is not present in any of the eleven
`state_docguide_*.doc` files in our store (all searched). It is Pub 1304's
state-table footnote, supplied by JI; cite it there rather than to the HT2
documentation guide.

### What the basis change did not do, measured after implementing it

Re-running the placement check — fitted non-filer adult **shares** against
anchor shares — the metric is essentially unchanged:

| TY2022 | before | after |
|---|---:|---:|
| MARD, raw anchor | 10.54% | **10.44%** |
| states within 5% | 15 of 51 | **15 of 51** |

**This was predictable from Part B and should have been predicted.** A and B
differ by a *uniform* scale factor on the subtrahend — 0.9987 in TY2022, 0.9914
in TY2017 — so if the change sits inside every state's tolerance, as Part B
measured, it cannot materially move a share-placement metric either. The two
statements are the same fact. The basis change earns its place on **consistency
and universe correctness**, exactly as the dorm netting does, and not on fit;
anyone expecting the placement metric to improve has mistaken what it fixes.
What closes the placement gap is F1b plus phase C's calibration.

One genuinely new reading did come out of the re-run, and it refines F1c.
Dorm netting is **not** uniformly worse than the raw anchor — it is worse on the
*mean* and better on everything the netting was for:

| TY2022, netted vs raw | raw | net of dorm |
|---|---:|---:|
| MARD (mean) | 10.44% | 10.69% |
| median error | 9.10% | **8.11%** |
| states within 5% | 15 | **18** |
| the eight college states | 15.75% | **12.39%** |

So netting helps the states it exists for and pays for it in the tail. "It does
not improve the fit" was too blunt; the mean is the only metric on which that is
true.

## Part C — MFS is published, QSS is derivable and tiny

HT2's status residual `N1 − MARS1 − MARS2 − MARS4` is MFS **plus** qualifying
surviving spouse. **Verified against the sheet 2026-08-27: T1.6 has exactly
four status blocks and the joint one is titled "Returns of married persons
filing jointly *and returns of surviving spouses*".** Table 1.2 folds QSS the
same way. So QSS is separately published nowhere, and netting is the only route:

| returns | TY2017 | TY2022 |
|---|---:|---:|
| HT2 status residual (MFS+QSS), 51 states | 3.157M | 3.854M |
| + out-of-state buckets | +0.140M | +0.195M |
| Pub 1304 T1.6 MFS, published | 3.213M | 3.993M |
| **= implied QSS** | **0.084M** | **0.056M** |

**Two findings.**

1. **The HT2 status residual is 98%+ MFS.** QSS is 1.4–2.2% of it. Decision #5's
   concern — that the residual absorbs surviving spouses and may be too dirty to
   target by state — is real but immaterial at that magnitude. The residual can
   carry the state distribution with a named 2% contamination, and the national
   MFS level comes from T1.2/T1.6 (3.993M in TY2022, which is the figure the
   plan already quotes).
2. **T1.6 counts QSS returns as two adults when a surviving spouse files
   alone**, overstating `filing_adults` by the QSS count — 0.041% (2017) /
   0.026% (2022), closing 17% / 3% of Part A's remainder. Small, but it is a
   *bias*, not noise, and it is now named.

### Part C, continued — can the PUF correct the surviving-spouse count? (JI, 2026-08-27)

**Not by identification.** Tax-Data carries `filing_status` straight from the
PUF's `MARS`, and it takes **four values only** — 1 single, 2 joint, 3 MFS,
4 head of household — with QSS folded into joint exactly as SOI folds it.
Verified on the production vintage for both anchor years. There is no
surviving-spouse flag to impute *from*, and no clean demographic signature
either: a QSS return is one adult on the joint schedule with a dependent child,
which in the PUF is indistinguishable from a single-earner couple with a child.

**But the PUF gives an independent second estimate of the MFS level**, which is
what the netting actually needs:

| | TY2017 | TY2022 |
|---|---:|---:|
| PUF/Tax-Data MFS returns (`filing_status == 3`, filers) | 3.186M | 3.768M |
| Pub 1304 T1.6 MFS, published | 3.213M | 3.993M |
| ratio | 0.992 | 0.944 |
| **implied QSS via the PUF route** | **0.111M** | **0.281M** |
| implied QSS via the published route | 0.084M | 0.056M |

So **QSS brackets at 0.06–0.28M, or 1.4–7.0% of the HT2 status residual** —
wider than the single netting suggested, and the width is just the PUF-versus-
published MFS gap. The conclusion is unchanged: the residual is 93%+ MFS and
can carry the state distribution with a named contamination. Report the range,
not the point.

**Direction matters and the two corrections compound.** Correcting the QSS
double-count *raises* the residual, as does removing the out-of-state filers.
Together the two universe corrections raise the anchor by **+2.4% (2017) to
+3.2% (2022)** — 1.13–1.16M and 1.27–1.49M adults respectively.

**One methodological point worth keeping.** On the 51-state universe the implied
QSS came out **negative** (−0.056M / −0.139M), because T1.6's published MFS
exceeds the whole 51-state residual. That impossibility is what identified the
missing out-of-state buckets. It is the same tell recorded in F10: when a
derived quantity takes a value its definition forbids, the two inputs are on
different universes.

## The axes are compatible, which is what makes any of this available

Verified 2026-08-27: **Table 1.2's AGI size classes are a strict refinement of
HT2's ten stubs.** Every HT2 boundary — 1, 10k, 25k, 50k, 75k, 100k, 200k,
500k, 1M — is also a T1.2 boundary, and "No adjusted gross income (includes
deficits)" maps onto stub 1. A national level collapses onto HT2 stubs exactly,
with no interpolation.

## The adult-dependent sweep (JI, 2026-08-27): the characteristic IS published

Asked whether return-claimed adult dependents exist in an OTA, JCT or SOI
source. **They do, in SOI, for TY2017 — one of the two anchor years.**

**SOI Pub 1304 Table 2.3, "All Returns: Exemptions by Type and Number of
Exemptions, by Size of Adjusted Gross Income."** Dependent exemptions are split
by *relationship*: children at home, children away from home, parents, other
dependents — each with returns and exemptions, by AGI size. Published 1996
through **2017 and no further**, because TCJA repealed personal exemptions. The
five pre-TCJA years that overlap HT2 availability are now in the store as
`national/by_size/exemptions_{2012,2014,2015,2016,2017}.xls` with manifest rows;
column positions verified identical across all five.

**TY2017**, from the `All returns, total` row (identity closes to one exemption):

| Dependent exemptions | count | adult? |
|---|---:|---|
| children at home | 83.161M | mostly minors, **unknown adult share** |
| children away from home | 0.422M | predominantly adult (students) |
| parents | 3.370M | **adult by definition** |
| other dependents | 7.755M | mixed |
| total | 94.709M | |

**This reconciles the three irreconcilable figures rather than adjudicating
between them.** Parents plus children-away-from-home is **3.792M, all adult**.
Add the mixed "other dependents" and the ceiling on those three categories is
**11.547M**, before any adult children living at home. So:

- the HT2 identity's **5.58M** sits inside the range — the all-adult floor plus
  roughly a quarter of other dependents — and is corroborated as a *floor*;
- `DEPSTAT`'s **13.80M** sits just above 11.547M and becomes plausible the
  moment adult children at home are counted;
- Mok's finding that the CPS *understates* 1040 dependents by ~11M is
  consistent with the same gap.

They were never three estimates of one quantity. They differ by categories
Table 2.3 names. What remains genuinely unmeasured is the adult share of the
83.161M children-at-home exemptions, and no published table carries dependent
age.

**A third construction of filing adults falls out, and it is the tightest yet.**
Exemptions for taxpayers (197.953M) plus returns filed by dependents (9.608M,
who claim no personal exemption) less T1.6's under-18 filers (2.191M) gives
**205.370M**, against T1.6's 205.487M — **−0.057%**, where the HT2 identity is
−0.370%. So Part A's unexplained remainder is an **HT2-versus-Pub-1304 family
difference, not Table 1.6 being odd.** Two Pub 1304 tables built from different
sheets agree six times more closely with each other than either does with HT2.

### The rest of the sweep

| Source | Result |
|---|---|
| **SOI Table 2.3** | **The answer, for TY2017.** Above. |
| **SOI, TY2022** | No exemptions table exists after 2017. The analogue is the **credit for other dependents** — \$500, defined as dependents ineligible for the child credit: 17–18-year-olds at home, 19–24-year-old full-time students, and other qualifying relatives, which is close to return-claimed adult dependents by construction. It lives in Pub 1304's credit tables, **not in our store**, and covers 2018 onward only. A named next step, not a dead end. |
| **Treasury OTA** | Read for the *definition*, not a number. The Jan-2025 non-filer study defines a non-filer as someone who appeared on an information return and was **not a primary filer, secondary filer, or listed in the first four dependent positions** — so it nets dependents out administratively, which makes its 50.343M a different estimand from our residual, exactly as F10's rule requires. TP-12's family unit includes dependents whether or not any member files. |
| **JCT** | **Dead end, recorded so it is not swept again.** JCT publishes revenue estimates and explanatory pamphlets, not tabulations of dependents by age or relationship. |
| **CBO** | Already in hand (Mok 2017, Table 14). |

## What this does not reach

**Return-claimed adult dependents, for TY2022.** The current bound is
`HT2 dependents − PEP under-18` = 5.58M. Two problems: the amplification is
~14× (78.9M − 73.4M = 5.5M), far worse than the residual's 4.5×; and the series
is not stable. HT2 dependents is `N2 − (N1 + MARS2)`, and `N2` changes concept
at TCJA (exemptions through TY2017, individuals from TY2018) — the state sum
goes **84.171M (2017) → 78.883M (2022)**, a 6.3% level break in exactly the
series the bound rests on. Separately, `NUMDEP` is present in HT2 2017 and
**absent from HT2 2022 with no replacement** (header diff, 2026-08-27).

No published table gives dependents by **age** in any year, and the exemptions
route that solves TY2017 does not exist for TY2022. HT2 2022 fuses the credit
for other dependents with the child credit as `N07225`/`A07225`, so the state
tables cannot separate it; Pub 1304's credit table can, and is **not in the
store**.

**So the recommendation changes shape rather than standing as it was.** The two
anchor years are no longer symmetric:

- **TY2017** — decompose. Table 2.3 gives an all-adult floor of 3.792M and a
  ceiling of 11.547M on the non-home categories, which brackets the three
  competing figures and explains their differences.
- **TY2022** — fetch Pub 1304's credit table for the other-dependent count, the
  nearest administrative analogue. Until then the HT2 identity's bound is what
  there is, with the instability above stated.

Decision #2's discipline is unchanged — report the figures, average none — but
it is now a *decomposition* for 2017 and a *bound* for 2022, rather than three
irreconcilable numbers in both.

## Code changes this pass made

Both are one-definition-per-computation moves, not behaviour changes:

- `read_pub1304_t16()` / `read_pub1304_t17_total()` promoted from
  `02_build_residual_anchors.R` into `src/data/state_weights.R`, so script 12
  reads the same national level the anchors are built on.
- `ht2_filing_persons()` gained a `states` argument (default: the 51 modeled
  jurisdictions) so the out-of-state buckets can be measured rather than only
  dropped, and a `mfs_qss_returns` column so the status residual is a named
  quantity. `compare_individuals_acs_irs()` now selects its three columns
  explicitly, leaving its output unchanged.
