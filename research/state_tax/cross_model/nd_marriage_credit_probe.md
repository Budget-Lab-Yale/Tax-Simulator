---
title: "ND probe — the marriage penalty credit is the whole residual, and it is representable"
role: evidence
workstream: state_tax
status: current
updated: 2026-08-22
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# ND probe, 2026-08-22

The 2026-08-22 class sweep left North Dakota without a swept crosswalk-exposure
row: its federal-itemizer split (0.793 exposed against 0.934 unexposed) sat
inside the range the sweep's control reaches *without* the mechanism, so the
split could not carry a row and ND was queued for the CO/UT treatment. This is
that probe. It found something better than a known-difference row.

**Result in one line: all three failing ND cells have a single cause, TAXSIM is
right and we are wrong, and the fix takes every cell in the window to ~0.999.**

Scripts: `output/probe/nd_decomp.R`, `nd_credit_dig.R`, `nd_v40_dig.R`,
`nd_marriage_probe.R`, `nd_marriage_probe2.R`, `nd_marriage_feasibility.R`,
`nd_marriage_fullwindow.R`. Parameters:
`output/class_sweep/nd_marriage_credit_params.csv`, with the nine source
booklets alongside them.

## 1. The exposure class has no route into ND — the control was right

Two independent facts close it:

- **TAXSIM never populates `v35_state_itemized_deduction` for ND.** Share
  positive is 0.0000 and the maximum is 0 in all four years. The state-itemizing
  half of the class predicate therefore cannot fire at all, and the
  Idaho-style route (fed-taxable start, federal itemized deduction entering the
  base through `v35`) does not exist here.
- **Federal itemizers are 1.3–1.5% of the ND clean subset.** They do match
  worse (0.78–0.81 against 0.93–0.94), but excluding every one of them moves the
  2019 cell only 0.929 → 0.937. The residual is overwhelmingly in
  NON-itemizers, which match at 0.932–0.937.

So the sweep's refusal to add a row on the strength of the split was correct,
and for the right reason.

## 2. Where the residual actually is

The pipeline decomposition puts it in credits, not in the base:

| | 2017 | 2018 | 2019 | 2020 |
|---|---|---|---|---|
| our ND taxable income agrees with TAXSIM `v36` on the misses | 0.864 | 0.906 | 0.860 | 0.884 |
| our credits agree with `v40` on the misses | 0.000 | 0.015 | 0.016 | 0.013 |
| median credit gap on the misses (theirs − ours) | 117.5 | 136.2 | 150.6 | 149.2 |

The base machinery is fine. TAXSIM grants a credit we do not, and it is the
whole gap: on the 2019 misses `v40` equals the liability difference almost
record for record, and our own credits are zero.

**The affected population is unambiguous.** 98.4–98.7% of ND misses are
two-earner joint returns. Filing statuses 1, 3 and 4 match at 0.997–1.000;
two-earner joint returns match at **0.556–0.583** in 2018–2020. The credit gap
has a hard ceiling that rises by year — **188 / 192 / 195 / 198** — and the
residual's two dominant modes in 2019 are +122 (211 records) and +195 (111).

## 3. What the credit is, and that TAXSIM computes it correctly

The **marriage penalty credit**, N.D.C.C. 57-38-01.28, Form ND-1 line 22. A
controlled probe reproduces the statutory shape exactly: at 2019 ND law on a
joint return with total wages of 400,000, the credit is **0.00** for a
one-earner couple, 26.77 at a 12.5% lesser share, and **195.00** from a 25%
share upward; against total income at an even split it runs 0.00 at 80,000,
122.20 at 120,000, 159.92 at 200,000 and 195.00 from 300,000 — the +122 and
+195 modes in the residual, recovered.

The worksheet, transcribed from the instructions:

```
 1  ND taxable income (Form ND-1 line 18/19)
 2  gate: line 1 must exceed T1
 3  a/b  each spouse's QUALIFIED INCOME
 4  the smaller of 3a and 3b
 5  gate: line 4 must exceed T2; constant = the single filer's allowance
 6  line 4 - line 5 constant
 7  tax on line 6 at the SINGLE schedule
 8  line 1 - line 6
 9  tax on line 8 at the SINGLE schedule
10  tax on line 1 at the MARRIED FILING JOINTLY schedule
11  line 7 + line 9
12  line 10 - line 11   (zero or less: no credit)
13  maximum credit
14  the smaller of line 12 and line 13
```

Published parameters, all nine years from the booklets:

| year | T1 (taxable income) | T2 (lesser qualified income) | line 5 allowance | max credit |
|---|---|---|---|---|
| 2017 | 63,505 | 35,955 | 10,400 | **188.00** |
| 2018 | 64,755 | 38,055 | 12,000 | **192.00** |
| 2019 | 66,006 | 38,756 | 12,200 | **195.00** |
| 2020 | 67,312 | 39,430 | 12,400 | **198.00** |
| 2021 | 67,812 | 39,830 | 12,550 | 201.00 |
| 2022 | 69,812 | 40,979 | 12,950 | 208.00 |
| 2023 | 74,862 | 43,980 | 13,850 | 287.00 |
| 2024 | 78,836 | 46,275 | 14,600 | 303.00 |
| 2025 | 81,036 | 47,550 | 15,750 | 312.00 |

**All four TAXSIM-observed ceilings match the published maxima exactly.** TAXSIM
is not approximating this credit; it implements the worksheet. Nothing here is
an external-model issue.

Two readings worth recording. The line 5 constant is the **single filer's
zero-tax allowance**, not simply the standard deduction: from 2018 it is the
federal single standard deduction, and in 2017 it is that deduction PLUS the
personal exemption (6,350 + 4,050 = 10,400). And the jump from 208 (2022) to
287 (2023) is the HB 1158 three-tier restructure, which vindicates the
`~$303 in TY2024` figure carried as a comment in `nd/credits.yaml`.

## 4. The credit IS representable — the "not PUF-representable" call is wrong

`research/source_packets/nd.md` and `nd/credits.yaml` both record this credit as
"not PUF-representable". That is incorrect, and the reason is structural rather
than empirical: **TAXSIM receives per-spouse WAGES and nothing else per spouse**
(the crosswalk hands `pwages`/`swages`, with retirement income at unit level),
so whatever it computes for lines 3a/3b is reachable from inputs we already
have. Statutory qualified income also counts taxable Social Security and
IRA/pension/annuity distributions, which we hold only at unit level — the same
limitation, on both sides.

Implementing the worksheet with `pmin(ei1, ei2)` as the lesser qualified income,
our own encoded single and MFJ schedules, and the published per-year parameters:

| year | reproduces `v40` within \$1 (where either is positive) | we fail to grant it | we over-grant it |
|---|---|---|---|
| 2017 | 0.864 | 0.0001 | 0.0022 |
| 2018 | 0.983 | 0.0001 | 0.0003 |
| 2019 | 0.983 | 0.0000 | 0.0007 |
| 2020 | 0.983 | 0.0000 | 0.0006 |

And the effect on the cells:

| year | match@\$100 before | after | match@\$15 before | after | misses |
|---|---|---|---|---|---|
| 2017 | 0.9810 | **0.9986** | 0.9703 | 0.9951 | 132 → 10 |
| 2018 | 0.9253 | **0.9988** | 0.9103 | 0.9948 | 667 → 11 |
| 2019 | 0.9290 | **0.9989** | 0.9133 | 0.9938 | 629 → 10 |
| 2020 | 0.9272 | **0.9988** | 0.9138 | 0.9939 | 621 → 10 |

2017 reproduces less cleanly (0.864) than the post-TCJA years, consistent with
the two-part 2017 allowance being an approximation on some records; it is
already the year that clears, so it is not what gates the state.

## 5. What encoding requires

This is our omission and the fix is ours, so a known-difference row would paper
over a gap rather than document a divergence. The precedent is ND's own HB 1515
resident tax relief credit, where the same documented-not-modeled call was
reversed once it was clear the credit sits inside the liability concept.

Needed, and none of it is blocked:

1. A **generic minimum-of-two-schedules credit component** in `st_credits.R`.
   The mechanism — split taxable income at a per-spouse quantity, tax each part
   at the unmarried schedule, credit the excess of the joint tax over the sum,
   capped — is not ND-specific; it is the natural home for other states'
   marriage-penalty and two-earner relief, so it should be parameterized by
   schedule and threshold rather than named for North Dakota.
2. Four indexed subparameter series plus the two gates, 2017–2025, in
   `nd/credits.yaml` with a `reference:` per subparameter. Values and citations
   are in `output/class_sweep/nd_marriage_credit_params.csv`.
3. A worksheet test at a two-earner couple above both gates, one at the cap, and
   a one-earner couple that correctly gets nothing.
4. A known-difference row for the part that genuinely is not representable: the
   taxable Social Security and IRA/pension/annuity components of qualified
   income, which we hold only at unit level. It bites where a retiree couple's
   qualified income is not mostly earnings.

## 6. Encoded and closed, same day

**It needed no new calculator code.** ND's worksheet is arithmetically the same
mechanism as Minnesota's Schedule M1MA credit, which already exists as the
generic `mc_*` family, so this became six subparameters × nine years in
`nd/credits.yaml`, each citing its worksheet line:

| ND-1 worksheet | generic parameter |
|---|---|
| line 2 gate (ND taxable income) | `mc_min_joint_txbl` |
| line 5 gate (lesser qualified income) | `mc_min_lesser_income` |
| line 5 printed constant | `mc_share_offset` |
| lines 7 and 9, single-schedule tax | `mc_single_brackets` |
| line 10, MFJ-schedule tax | the unit's own mapped brackets |
| line 13 maximum | `mc_max` |

The shared component taxes both schedules from one rate vector, so it is only
valid where a state's rates do not vary by filing status. **Verified rather than
assumed: ND's ordinary rates are identical for single and joint in all nine
years 2017–2025** — only the brackets differ. Had that failed, the generic would
have produced wrong numbers silently.

Tests ND-9/9b/9c/9d/9e: below the cap (59.69), at the cap (195), a one-earner
couple that correctly declines, each gate in isolation, and a single filer.
Full `test_state_calc()` suite green.

**PolicyEngine was probed before the rerun, not after.** The credit also runs
2021–2024, where ND's PE cells were ALREADY clear — so had PE omitted the
credit, encoding it would have broken four passing cells to fix three failing
ones. PE models it (286.66 against the statutory 287.00 in 2023; the 34-cent gap
is far below tolerance and not worth a row), so both windows move the same way.

### Confirmed result

| | before | after | match@\$15 before → after |
|---|---|---|---|
| TAXSIM 2017 | 0.9810 | **0.9986** | 0.9703 → 0.9951 |
| TAXSIM 2018 | 0.9253 | **0.9988** | 0.9103 → 0.9948 |
| TAXSIM 2019 | 0.9290 | **0.9989** | 0.9133 → 0.9938 |
| TAXSIM 2020 | 0.9272 | **0.9988** | 0.9138 → 0.9939 |
| PE 2021 | 0.9945 | **1.0000** | 0.9808 → 0.9863 |
| PE 2022 | 0.9762 | **0.9857** | 0.9715 → 0.9786 |
| PE 2023 | 0.9763 | **0.9871** | 0.9677 → 0.9828 |
| PE 2024 | 0.9694 | **0.9891** | 0.9650 → 0.9847 |

The TAXSIM figures match §4's prediction to four decimal places. **All eight
cells clear; worst 0.9857. No cell outside ND moved.** ND is `done`.

### What is left, honestly

41 misses out of 33,249 federally aligned records (0.12%), and they are two
different things:

- **2017, ten records**, all joint with the credit positive and ours low by a
  median \$126. That is the two-part 2017 allowance: it reproduced TAXSIM at
  0.864 against 0.983 in the post-TCJA years, so the approximation is real but
  confined to the one year that already cleared before the fix.
- **2018–2020, 28 non-joint records**, median \$294 with ours high. TAXSIM grants
  no credit on them and our credits are zero, and the base agrees on 75% with a
  median gap of only −\$35. Diffuse, and it points nowhere yet. ND's documented
  non-representable items — the US-obligation interest share, military
  pay/retirement, and the family member care credit — are the standing
  candidates.

A `data-proxy` **annotate** row records the part of statutory qualified income
we cannot see per spouse (taxable Social Security and IRA/pension/annuity
distributions). Annotate rather than exclude, because TAXSIM shares the
limitation exactly, so the proxy does not itself create a cross-model
divergence — it is a divergence from true law.

## Revision history

- **2026-08-22** — written. Probe executed; the crosswalk-exposure hypothesis
  refuted for ND on two independent grounds; the residual attributed in full to
  the marriage penalty credit; TAXSIM verified against nine years of published
  booklets; the "not PUF-representable" call refuted with a measured
  reproduction. **Encoded the same day** on the generic `mc_*` family with no new
  calculator code; all eight ND cells now clear (worst 0.9857) and the state is
  `done`.
