---
title: "Cross-state student linkage — the IPEDS option, and the question it does not answer"
role: notes
workstream: state_weights
status: open
updated: 2026-08-24
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Cross-state student linkage

**Recorded 2026-08-23 as an option, not a decision.** It came out of task B1's
step 4: reclassifying dorm students from own-state heads to dependents removes
them from their institution state's non-filer margin but does not put them
anywhere, because the ACS gives no cross-household parent link. This note holds
the candidate data sources and — more importantly — the definitional question
that has to be settled before any of them is useful.

## 0. What B1 actually did, and the size of the hole it leaves

**Updated 2026-08-24, when B1 shipped.** This section exists because the note was
written a day before the measurement, and the measurement changes how urgent §4
is.

B1 reclassified **2.52M dorm students** (of 2.81M identified; the other 0.29M
clear the single filing threshold on their own income and stay filer units) out
of their institution state's non-filer margin, and — as designed — placed them
nowhere. Measured against the residual anchors on the TY2022 extract, the
treatment improves the national mean absolute relative difference from **10.28%
to 8.79%**, and it confirms that blanket exclusion is *worse* than no treatment
at all (**11.12%**).

**But the college states move the wrong way, and DC is the exhibit.** DC's
non-filer margin was already **0.779×** its anchor — the lowest of the 51 — and
the reclassification takes it to **0.637×**. The other large movers are the same
kind of place: VT −17.7%, RI −14.1%, MA −12.6%, ND −11.9%, CT −11.7%. The
students are gone from the denominator state and absent from every other one.

Two things follow, and they pull in opposite directions:

- **This is not an argument against the reclassification.** A dorm student with
  no income is genuinely not a DC filing unit, and HT2 already counts them in
  N2 on a parent's return. Leaving them as DC heads was the error; removing them
  is the fix.
- **It is an argument that §4 cannot stay open indefinitely.** The residual
  method's discipline comes from the anchors, and in the college states the
  margin is now further from its anchor than before. That is tolerable while the
  margins only supply *within-cell geography* to a fit whose levels come from the
  PUF (which is the case today), and it stops being tolerable the moment anything
  reads the non-filer margin as a level.

Note the direction of the bias is now known and one-sided: **college states are
short, and no state is long as a result.** Whatever §4 decides, it can be
sanity-checked against that.

## 1. Why the obvious approach fails

**`MIGPLAC1` (ACS "state of residence 1 year ago") is not usable, and the way it
fails is quiet.** It is a pulled column in our extract (positions 255–257,
alongside `MIGRATE1`), and for a first-year student who moved in the past year it
does give the home state. But it identifies **movers**. A student in their second
year or later has been at college more than a year, so `MIGPLAC1` reports the
*institution* state, and they are indistinguishable from a local. Using it would
silently turn a whole-student-population measure into a **freshman-only** one,
biased toward whatever is distinctive about first-years, with nothing in the
output to signal the restriction. Rejected (JI, 2026-08-23).

## 2. The IPEDS option

**NCES IPEDS, Fall Enrollment (EF) component — "Residence and migration of
first-time degree/certificate-seeking undergraduates."** For each institution it
reports first-time freshmen by **state of residence**, so aggregated to the
institution's state it gives a **state-of-origin × state-of-institution flow
matrix**.

**The suggested refinement (JI): link several years.** The component covers
first-time freshmen only, so a single year describes one cohort, not the enrolled
stock. Two ways to get from one to the other, and the cheap one is probably
enough for a rough cross-state linkage:

- **Cumulate cohorts.** Age several freshman cohorts forward to approximate the
  enrolled stock by origin state. Needs retention and persistence assumptions
  per state, which is where the error would come from.
- **Apply the freshman origin mix as a ratio to total enrollment.** IPEDS gives
  total enrollment by institution state separately, so the freshman flow matrix
  becomes a *share* applied to the larger stock. Far simpler, and its one
  assumption — that origin mix does not vary much by class year — is stateable
  and testable against the cumulated version.

**What it is not.** Recorded so the limitations are not rediscovered:

- **First-time degree-seeking undergraduates only.** Graduate students are
  outside the component entirely.
- **Collected biennially** (even years), so between-year values are
  interpolated.
- **"State of residence" is permanent residence at application**, which is close
  to but not the same as the state of the return that claims the student. A
  financially independent student is not a dependent at all.
- **It is not in the shared store.** No `NCES-IPEDS` family exists; this would be
  a new acquisition, unlike `MIGPLAC1`.

## 3. The population is narrower than "out-of-state students"

B1 reclassifies `GQ == 4` dorm residents. An out-of-state student renting an
apartment is a **household** record: they form their own tax unit, sit in the
institution state, and are misassigned in exactly the same way — but B1 does not
touch them and neither would an IPEDS reallocation keyed to dorm residents. Any
serious treatment has to decide whether it is fixing *dorm* students or
*students*, and the second is a much larger population reached through a
different mechanism.

## 4. ⚠ The question no data source answers

This is the part to settle first, because it determines whether a flow matrix is
useful at all.

**The anchor and the margins use different placement conventions, and they
disagree for exactly this population.**

- The **anchor** is PEP resident adults by state with no GQ subtraction
  (design memo §3.0). PEP places a dorm student in the **institution** state —
  that is where they sleep.
- The **HT2 dependents identity** counts a dependent slot in the state of the
  **return that claims them** — the **parent's** state.

So if we reallocate a reclassified dorm student's dependent slot to the parent
state, the ACS margin and the PEP anchor now disagree about where that person
lives, and §3.0's invariant — "the same GQ population must sit inside (or
outside) all three objects" — is broken in a new place.

**Therefore: decide what the non-filer partition is counting before sourcing any
flow data.** Residence (PEP convention) or claiming-return (HT2 convention)?
Better cross-state data cannot resolve this; it is a definitional choice about
the object being estimated, and IPEDS would only make a wrong choice more
precise.

## 5. Candidate uses, once §4 is settled

| use | what it needs | comment |
|---|---|---|
| **(a) Reallocation matrix** — move reclassified students' dependent slots from institution state to origin state | §4 resolved in favour of the claiming-return convention | The full version. Also needs the unassigned-dependent pool of §6 to cap it |
| **(b) Validation target only** — do not reallocate; check each state's unassigned-dependent pool is consistent with implied student flows | nothing beyond IPEDS | Cheapest, and useful under **either** convention, because it tests coherence rather than moving mass |
| **(c) Soft margin in the fit** — add out-of-state student counts so the fit places student-heavy states correctly | IPEDS plus a decision on target status | Fits the existing margin machinery; avoids a hard reallocation |

**(b) is the recommended first step** whatever §4 concludes: it is the only one
that cannot be invalidated by the convention decision.

## 6. The pool this connects to already exists in the design

Design memo §3.0 already specifies the object: *"the residual includes adult
dependents claimed on filed returns, who are neither filing adults nor
non-filer-unit heads… The non-filer-partition target is therefore PEP adults −
filing adults − **adult dependents claimed on returns**, the last estimated from
the HT2 dependents identity net of the under-18 population"* — carried as T2/T5
material, or in the tolerance if too noisy.

That is the pool of available-but-unassigned dependent slots, and it needs no new
source: supply per state from the HT2 identity (`dependents = N2 − (N1 +
MARS2)`, already implemented as `ht2_filing_persons()`), demand per state from
ACS-assigned dependents, residual = the pool. **Building it is worth doing
regardless of any of the above**, because §3.0 requires it for the anchor, and it
is what would cap a reallocation if one is ever done.

## 7. What is NOT available, checked so it is not re-searched

No IRS or Treasury source gives **dependents by state × age or education**:

| source | geography | dependents | age |
|---|---|---|---|
| HT2 (and SOI county / ZIP) | state and finer | yes, via the identity | **none** — `ELDERLY` is a *filer* count |
| Pub 1304 Table 1.6 | national | — | filers' age only |
| Pub 1304 Table 1.7 | national, by AGI size | dependent *returns* (filed **by** dependents) | none |

Dependent age is not tabulated by SOI in any geography. The single partial
exception is that CTC versus ODC amounts imply under-17 against 17-and-over, a
coarse two-way age proxy available at state level through HT2's credit columns.

## Revision history

- **2026-08-24** — §0 added when B1 shipped: 2.52M students reclassified, the
  measured MARD improvement, and DC at 0.637× its anchor. The cross-state
  question is no longer hypothetical, and the residual bias is now known to be
  one-sided (college states short, nothing long).
- **2026-08-23** — written. `MIGPLAC1` rejected; IPEDS multi-year linkage
  recorded as the candidate approach with its limitations; the
  residence-versus-claiming-return convention identified as the prior question;
  the IRS dependents-by-age search recorded as closed with a negative result.
