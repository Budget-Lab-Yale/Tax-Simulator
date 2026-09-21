---
title: "State Tax Batch Analysis"
role: notes
workstream: state_tax
status: historical
updated: 2026-08-19
true_as_of: 2026-07-13
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# State Tax Batch Analysis

Last updated: `2026-07-13`

## Purpose

Use the tax-base construction and reusable calculator components to choose
research and implementation batches. A shared nominal rate is not enough: two
states belong together only when the federal starting point, deduction flow,
credit shape, and required data are sufficiently similar.

## Batch A: rolling federal AGI, flat-rate validation

States: `IL / IN / MI`

- Illinois starts from federal AGI, has no state deduction, and applies
  exemptions plus refundable EITC-style credits.
- Indiana and Michigan supply the closest completed comparison cases: rolling
  federal-AGI base, flat rate, exemptions, and refundable federal-EITC matches.
- This is a validation and source-packet batch, not a generic-code batch. The
  necessary primitives already exist.
- Do not add Pennsylvania: its class-income base and local income taxes are a
  different model family. Arizona, Georgia, and North Carolina are useful
  regression comparisons but have distinct deduction or retirement mechanics.

## Batch B: federal taxable income construction

States: `CO / ND / SC`

- These states begin tax construction from federal taxable income, so federal
  standard and itemized deductions pass through before state modifications.
- Colorado anchors the generic addition/subtraction and credit machinery;
  North Dakota and South Carolina should be researched in that order.
- Keep the ordinary rate schedule and state-specific credits parameterized.
  Colorado's tiered refundable credits should not become a state-name branch.
- Do not add Idaho or Oregon. Their current resident returns begin with federal
  AGI and rebuild state deductions. Oregon also has a federal-tax subtraction,
  which belongs in the federal-state feedback backlog.

## Batch C: graduated federal AGI with high-income calculation layers

States: `NY / CT`

- Both begin with federal AGI, apply state additions and subtractions, and use
  graduated rates with a separate high-income calculation layer.
- New York supplies the existing reusable recapture and independently elected
  itemization components. Connecticut should use generic rate/recapture inputs
  rather than New York-specific code.
- Local programs remain explicitly out of scope: NYC/Yonkers/MCTMT for New
  York and municipal property-tax concepts for Connecticut.
- Virginia is intentionally deferred to a later graduated-federal-AGI batch.
  Its standard/itemized deduction, exemptions, age deduction, and exclusive
  low-income/EITC choices are a more useful separate generic-component test.

## Common completion gates

1. Complete the anchor source packet and feature-gap inventory.
2. Collect current and historical forms for the unencoded members.
3. Add a generic primitive only when more than one state needs it.
4. Add form-worksheet tests before enabling a new jurisdiction.
5. Run cross-model checks before treating the state as record-validated.
6. Defer aggregate conclusions until calibrated state weights are available.
