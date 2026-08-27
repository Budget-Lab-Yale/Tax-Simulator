---
title: "Which source sets the level: the anchor basis comparison"
role: notes
workstream: state_weights
status: open
updated: 2026-08-27
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Which source sets the level: the anchor basis comparison

**JI, 2026-08-27.** Measured by
`research/state_weights/nonfiler_residual/12_anchor_basis_comparison.R`;
per-state output in `nonfiler_residual/results/anchor_basis_comparison_{year}.csv`.
**Open**: it recommends a change to the state anchors and a resolution of plan
decision #5, neither signed off.

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

**B2 is a substantive level change and needs its own decision.** Removing the
out-of-state filers is right in principle — PEP counts residents of the 51
states — but it moves 43 of 51 states outside tolerance in TY2017. It also
turns on a question this note does not settle: whether HT2's `OA` bucket is
*citizens abroad and territories* (not PEP residents, so remove) or *domestic
returns whose state could not be classified* (PEP residents, so reallocate).
`raw_data/IRS-Ind/state/HT2/state_docguide_{year}.doc` is the place to check,
and it has not been read for this purpose.

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

## What this does not reach

**Return-claimed adult dependents.** The current bound is
`HT2 dependents − PEP under-18` = 5.58M. Two problems: the amplification is
~14× (78.9M − 73.4M = 5.5M), far worse than the residual's 4.5×; and the series
is not stable. HT2 dependents is `N2 − (N1 + MARS2)`, and `N2` changes concept
at TCJA (exemptions through TY2017, individuals from TY2018) — the state sum
goes **84.171M (2017) → 78.883M (2022)**, a 6.3% level break in exactly the
series the bound rests on. Separately, `NUMDEP` is present in HT2 2017 and
**absent from HT2 2022 with no replacement** (header diff, 2026-08-27).

No published table gives dependents by age. The closest administrative proxy is
the credit for other dependents (\$500, non-child dependents, 2018+), blocked
twice: HT2 2022 fuses it with the child credit as `N07225`/`A07225`, and Pub
1304's credit table is **not in the store**, so it would need a fetch and would
cover 2018+ only — excluding the TY2017 anchor year. Plan decision #2's "report
all three, average none" therefore stands; an ODC count would be a fourth,
tighter reading rather than a replacement.

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
