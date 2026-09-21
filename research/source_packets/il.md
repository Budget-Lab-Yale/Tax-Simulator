# Illinois State Source Packet

State: `IL`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; source packet normalized; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published parameters are transcribed through
  2026 where available; later exemption amounts use the documented CPI proxy.
- Resident IL-1040 baseline. Nonresident allocation, credits for tax paid to
  other states, and business-credit schedules are outside this baseline.
- Major features: rolling federal-AGI base, flat tax, no state standard or
  itemized deduction, retirement and Social Security subtractions, exemptions,
  refundable EITC/child credit, and nonrefundable property-tax credit.

## Primary sources

- [2025 IL-1040 instructions](https://tax.illinois.gov/forms/incometax/currentyear/individual/il-1040-instr.html)
  for the return structure, filing rule, rate, exemptions, and credits.
- [2025 Schedule M instructions](https://tax.illinois.gov/forms/incometax/currentyear/individual/il-1040-schedule-m-instr.html)
  for federal-AGI additions and subtractions.
- [2025 Schedule ICR instructions](https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-schedule-icr-instr.pdf)
  for property-tax and K-12 credit eligibility.
- [2025 Schedule IL-E/EITC instructions](https://tax.illinois.gov/content/dam/soi/en/web/tax/forms/incometax/documents/currentyear/individual/il-1040-schedule-il-e-eic-instr.pdf)
  for the refundable EITC and child-credit implementation.
- Historical annual IL-1040 instructions and the cited statutes in the YAML
  files are the source of the 2017-2024 time series. Preserve year-specific
  forms when revising a historical value.

## Parameter inventory

- `agi.yaml`: federal AGI start; municipal-interest addback proxy; U.S.
  obligation, Social Security, retirement-income, and state-refund
  subtractions.
- `ded.yaml`: deliberately zero standard and itemized deductions.
- `exempt.yaml`: personal/dependent, age, and blind exemptions plus the
  statutory high-income cliff.
- `ord.yaml`: 2017 blended rate, then the 4.95 percent flat rate.
- `credits.yaml`: refundable federal-EITC match, Illinois child credit, and
  property-tax-credit parameters.
- `filing.yaml`: federal-filing or Illinois-base-over-exemption requirement.

## Worksheet tests

- Basic single filer and the 2017 midyear blended rate.
- Retirement/Social Security subtraction, exemptions, property credit, and
  refundable EITC.
- $250,000/$500,000 exemption and property-credit cliffs.

## Known differences

- `exempt_int` does not identify Illinois-obligation interest, so the all-muni
  addback is an explicit proxy.
- Property-tax credit eligibility requires Illinois principal-residence and
  prior-year payment information that the PUF does not fully identify.
- K-12 education, other-state tax, business, and household-specific credits
  are not calculated; their YAML metadata must not be read as active
  calculator support.
- The expanded EITC's ITIN and age eligibility cannot be fully replicated from
  available data.

## Triage 2026-08-22 — the PolicyEngine dependent-age cliff

Illinois matches TAXSIM at **1.0000** in all four years on about 10,000
federally aligned records each, while the PolicyEngine window sat at
0.9316 / 0.9252 / 0.9370. A state whose law is that thoroughly confirmed
against one external model is not the likely source of a disagreement with
the other.

The residual is a single point mass. The miss mode is exactly **-120** in 2022 and 2023 and **-137** in 2024, on
27 of 29, 30 of 35 and 19 of 29 misses. Illinois is a flat tax, so those are
one personal exemption to the cent: $2,425 x 4.95% = $120.04 and
$2,775 x 4.95% = $137.36. The misses are single, head-of-household and
married-filing-separately returns, not joint -- a second exemption on a
one-adult return is a dependent.

**PolicyEngine gates the benefit on the dependent being under 18; Illinois
does not.** Probed against policyengine-us 1.775.7 on single Illinois filers
differing only in one dependent's age, 2023: the exemption is granted at ages 5, 10, 16 and 17 (state tax 2,234.93
against 2,354.96 with no dependent) and denied from 18 on. The cliff sits at 18,
not at federal child-tax-credit eligibility -- a 17-year-old is too old for
the federal CTC but still earns the benefit in PolicyEngine, so a predicate
keyed on `n_dep - n_dep_ctc` would over-exclude.

35 ILCS 5/204 allows an exemption for each person claimed as a dependent on
the taxpayer's federal return, with no age condition; IL-1040 simply carries
the federal dependent count. On the real records the split is decisive: the
miss rate is 0.5-0.7% for returns with no dependent aged 18+ against 47-59%
for returns with one.

Excluded as a PolicyEngine-side difference, with the predicate also requiring
a live exemption since the difference cannot arise when it is zero. Cells
move to 0.9949 / 0.9928 / 0.9951, and all eight now clear. This is the class recorded as P9 in
[`cross_model/external_model_issues.md`](../state_tax/cross_model/external_model_issues.md);
a sweep of all 48 enabled jurisdictions finds an age effect in 23, though most
of the others are unchecked against their own statutes and some genuinely do
restrict by age.

## Batch role and validation

- Anchor for the `IL / IN / MI` rolling-federal-AGI, flat-rate validation
  cohort. Do not merge Pennsylvania into this group: its class-income base and
  local taxes are structurally different.
- Cross-model: compare 2017, 2024, and 2025 IL-1040 resident cases against a
  form calculation or TAXSIM where coverage matches.
- Aggregate: once weights land, compare returns, net liability, EITC, and
  property-tax-credit totals to Illinois DOR statistics and SOI HT2.
