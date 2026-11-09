# North Carolina State Source Packet

State: `NC`  
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

> **Status note (as of 2026-07-13), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Form D-400 history is transcribed through 2025 and the enacted 2026 3.99 percent rate is included. The same rate carries forward unless a later revenue-trigger certification changes it.
- Baseline only. Resident individual income tax; no nonresident apportionment.
- Major features: federal-AGI base, AGI-tiered child deduction, independent state itemization with direct component selection, standard deduction, and a flat-rate schedule.

## Primary sources

- [North Carolina individual forms and instructions](https://www.ncdor.gov/taxes-forms/individual-income-tax/individual-income-tax-forms-instructions), including D-401 files for 2017-2025.
- [North Carolina child deduction guidance](https://www.ncdor.gov/taxes-forms/individual-income-tax/filing-topics/north-carolina-child-deduction).
- [North Carolina standard deduction and itemized deduction guidance](https://www.ncdor.gov/taxes-forms/individual-income-tax/filing-topics/north-carolina-standard-deduction-or-north-carolina-itemized-deductions).
- [North Carolina tax-rate schedules](https://www.ncdor.gov/taxes-forms/individual-income-tax/tax-rate-schedules).
- [G.S. 105-153.5](https://www.ncleg.gov/EnactedLegislation/Statutes/HTML/BySection/Chapter_105/GS_105-153.5.html) and [G.S. 105-153.7](https://www.ncleg.gov/EnactedLegislation/Statutes/HTML/BySection/Chapter_105/GS_105-153.7.html).

## Parameter inventory

- `agi.yaml`: federal AGI start and municipal/U.S. obligation interest flags.
- `child_ded.yaml`: reusable generic AGI-table child deduction; 2018 introduction and 2022 amount/table expansion.
- `ded.yaml`: standard deduction history and direct selection of the permitted medical, mortgage-interest, charitable, and capped real-property-tax components.
- `exempt.yaml`: no personal/dependent exemption; child benefit remains separate.
- `ord.yaml`: 5.499 percent (2017-18), 5.25 percent (2019-21), enacted rate reductions through 3.99 percent in 2026.
- `credits.yaml`: neutral explicit placeholder; no broad credit is ready for the resident baseline.
- `filing.yaml`: status-specific standard-deduction thresholds used as an AGI proxy for gross-income filing tests.

## Worksheet tests

- 2025 single filer: child-deduction table at the $30,000 boundary.
- 2025 independent state itemizer: direct permitted components and $10,000 property-tax cap.
- 2026 single filer: enacted 3.99 percent rate.

## Known differences

- Taxable and tax-exempt interest do not reveal U.S. obligation or North Carolina-bond sources; existing generic flags preserve the legal rule but cannot fully allocate the base.
- Schedule S additions/subtractions, including Bailey settlement treatment, military retirement, claim-of-right, and numerous federal-conformity items, need further component-level data and are omitted. EXCEPTION (2026-07-23 elderly-provisions survey): the taxable Social Security / Railroad Retirement deduction (G.S. 105-153.5(b)(6)) was found missing despite being fully modelable — now encoded (`ss_sub_share = 1`, test NC-4). Bailey and military-retirement pension deductions remain omitted (pension source unobservable; both bias NC liability UP for affected retirees).
- The generic itemizer uses federal deductible component inputs; some North Carolina-specific qualification/limitation details still need cross-model review.
- Filing thresholds are based on gross income and differ for dependents and older taxpayers. `st_filer` is a documented AGI proxy, not an official returns estimate.
- The 2027+ rate-reduction triggers depend on annual revenue certifications and are deliberately not assumed in the baseline.

## Triage 2026-08-22 — the 2017 child credit was being refunded

NC 2017 sat at 0.9090 on the clean subset while 2018-2020 were all above 0.96.
The 2017-only shape pointed at the credit for children (G.S. 105-153.10), which
`ctc_style` correctly switches off from TY2018 when the child deduction
replaces it.

Of 635 clean misses, 330 sat on exact multiples of the credit: **-125** (196
records), **-250** (84), **-375** (29), plus the $100 upper-AGI tier. The
cross-tab against `n_dep` is exact -- diff = -(125 x children) -- and `st_ctc`
is 125 on every record in the -125 group. Negative means our liability is lower,
i.e. we were paying the credit out.

**Cause: refundability.** `params_schema.yaml` defaults
`st_credits.ctc_refundable` to 1 and `nc/credits.yaml` never overrode it, so a
zero-tax filer received the credit as a refund. Median state AGI in the -125
group is about $8,100 -- below NC's standard deduction, so no tax to offset.

G.S. 105-153.10(a) allows "a credit against the tax imposed by this Part" and
nowhere declares it refundable; the repealed NC EITC (G.S. 105-151.31) said
"This credit is refundable" in terms, and its absence here is dispositive.
TAXSIM agrees: probed on a single filer with $5,000 of wages and one child, it
reports the credit in `v40` and holds `siitax` at 0.00.

Fixed by `ctc_refundable: 0` in `nc/credits.yaml`; worksheet case NC-5c locks
it. Only CO, NC and NY relied on the schema default, and Colorado's and New
York's child credits are genuinely refundable, so NC was the only state
affected.

**Result.** 2017 0.9090 -> 0.9702. The other three years also improved (0.9637
-> 0.9697, 0.9689 -> 0.9738, 0.9618 -> 0.9711) from the separate state-refund
crosswalk fix described in the RI packet -- NC does not subtract its own refund
either. All four TAXSIM cells now clear.

**Still open.** PolicyEngine 2024 sits at 0.9457, two records above the 23
misses that would clear on n_clean = 460. The misses are high-income scatter
(median state AGI $930k) with no point mass; not chased, because moving two
records out of a 460-record cell is fitting the metric rather than finding a
cause.

## Cross-model and aggregate validation

- Cross-model: `todo`; compare 2018, 2022, 2025, and 2026 cases, especially child-deduction thresholds and independent itemization.
- Aggregate: blocked until state weights land; inspect NC HT2 returns/AGI by bracket and reconcile against DOR annual collections.
