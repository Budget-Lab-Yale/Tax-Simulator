---
title: "CA follow-up plan: miscellaneous itemized deductions + CA AMT"
role: notes
workstream: state_tax
status: open
updated: 2026-08-19
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# CA follow-up plan: miscellaneous itemized deductions + CA AMT

Drafted 2026-08-16 at the close of the CA cross-model triage (see
`research/source_packets/ca.md` and `cross_model/results/reports/ca.md`). These are
the two remaining implementable CA features large enough to plan rather
than just do. They interact — see Sequencing — and neither is
cross-model verifiable, so both land with known-difference rows rather
than harness targets.

## 1. Miscellaneous itemized deductions under California law

**Law.** California conforms to the IRC as of 2015-01-01 (through TY2024),
so the TCJA repeal of 2%-floor miscellaneous itemized deductions never
happened for CA: unreimbursed employee expenses, tax preparation fees, and
investment/other expenses remain deductible on Schedule CA (540) Part II,
subject to the 2%-of-federal-AGI floor. Our CA itemized base currently
uses `misc_item_ded_potential`, the FEDERAL as-if-itemizing amount, which
is $0 from 2018 — so CA misc is silently zero in exactly the years CA
still allows it. 2017 is already correct (federal law still had misc).

**Implementation.**
1. `st_ded` gains a recompute switch and floor:
   `item_misc_own_style` (0 = federal potential, 1 = recompute) and
   `item_misc_floor_agi` (CA: 0.02 of federal AGI). When style 1:
   `st_misc_item = pmax(0, job_exp + tax_prep_exp + other_misc_exp -
   item_misc_floor_agi * pmax(0, agi))`, replacing
   `misc_item_ded_potential` in `st_item_components`.
2. Raw inputs `job_exp`, `tax_prep_exp`, `other_misc_exp` join the
   `st_ded` req vars and the `st_test_unit` defaults (all three exist in
   the microdata: nonzero on roughly 9% / 23% / 36% of records).
3. Schema entries + `ca/ded.yaml` (`item_misc_own_style: 1` for all years
   — in 2017 the recompute equals the federal potential, so no year key
   is needed; verify equality in the test).
4. The CA 6%/80% limitation already treats misc as non-protected, so the
   limitation flows through unchanged.
5. Harness: fold the recomputed misc into the `xw_unhanded_item` /
   `xw_pe_unhanded_item` exposure covariates so the existing itemized
   KD exclusions keep capturing it (neither TAXSIM nor PE deducts
   post-TCJA misc — the feature is cross-model unverifiable by
   construction).
6. Tests: one CA hand case (2019 state-only itemizer with job expenses
   above the 2% floor) and one 2017 equivalence check (recompute ==
   federal potential).

**Long-term note.** CA's conformity group is the 2015 reference law. Once
the P1-B reference-law bridge lands, the federal calculator re-run under
2015 law produces a nonzero `misc_item_ded_potential` natively, and
`item_misc_own_style` can be retired. The switch is the interim.

**Effort:** ~half a day. **Direction:** we currently OVERSTATE CA tax for
itemizers with employee/investment expenses.

## 2. California AMT (Schedule P 540)

**Law.** 7% tentative minimum tax on AMTI: taxable income plus CA
preference/adjustment addbacks, less an exemption (2023: $109,288 MFJ /
$81,966 single, phased out at 25 cents per dollar of AMTI above ~$409k /
~$306k). AMT = max(0, TMT - regular tax before most credits). The
OBSERVABLE addbacks are exactly the deductions the regular-tax side
allows: property taxes (we uncap them) and miscellaneous itemized
deductions (item 1 above). ISO exercises, private-activity-bond interest,
and depreciation preferences are unobserved — the same gaps the federal
AMT calculation lives with — so the model captures the deduction-driven
AMT only.

**Implementation.**
1. New generic module `st_amt.R` (MN's 6.75% AMT can reuse it later):
   - params: `amt_rate`, `amt_exempt` (filing-status mapped),
     `amt_exempt_po_thresh` (mapped), `amt_exempt_po_rate` (CA 0.25),
     `amt_addback_prop_tax` / `amt_addback_misc` / `amt_addback_std`
     selectors (CA adds back the standard deduction for std-takers too);
   - AMTI = st_txbl_inc + selected addbacks; TMT = rate x max(0, AMTI -
     phased exemption); `liab_st_amt = pmax(0, TMT - st_tax_pre_credit)`;
   - wire into `st_liab` and the detail outputs; year-keyed exemption
     series 2017-2025 from the Schedule P instructions (indexed annually).
2. Credit-limitation nuance (Schedule P Part III restricts which credits
   offset TMT) — v1 skips it and documents; the exemption/rate mechanics
   dominate.
3. Tests: two Schedule P hand cases (one std-taker, one itemizer with
   property tax + misc addbacks) at the exemption phase-out boundary.
4. Cross-model: TAXSIM's CA AMT coverage is unverified and PE does not
   model it — KD annotate (or exclude on a high-AMTI predicate if the
   rerun shows material movement).
5. Aggregate sanity: FTB Schedule P statistics (~30-40k returns,
   ~$300-400M/yr) once weights land.

**Effort:** 2-3 days including worksheet tests.

## Sequencing

Misc itemized FIRST, CA AMT immediately after, ideally in one arc:
implementing misc without AMT overstates deductions for exactly the
high-income itemizers most likely to be in AMT (both misc and property
tax are AMT addbacks). Neither is urgent for the cross-model bar — CA
clears TAXSIM everywhere and the PE window clears after the 2026-08-16
CDCTC fix — so schedule this when top-of-distribution CA output matters
(P1 production use or CA distributional tables).
