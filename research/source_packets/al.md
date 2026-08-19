# State Source Packet: Alabama

State: `AL`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025 (nothing is indexed, so TY2025 law carries forward;
  no TY2026 change is confirmed enacted)
- Baseline only
- Major structural features: OWN income base, not federal AGI; 2/4/5% rates
  unchanged for nine years; FULL and UNCAPPED federal income tax deduction
  (constitutionally entrenched); AGI-sliding standard deduction; AGI-tiered
  dependent exemption with hard cliffs; own Schedule A that deducts employee FICA

## Primary sources

### Current forms and instructions

- Form/booklet: Form 40 booklet (`17f40bk.pdf` ... `25f40bk.pdf`), plus the blank
  TY2025 Form 40 and Schedule A
- DOR landing page: revenue.alabama.gov

### Historical forms and instructions

- All nine booklets TY2017-TY2025 downloaded and text-extracted; every parameter
  below verified across all nine rather than carried forward from one
- Missing years / gaps: Schedule RS (the age-65 retirement exclusion) is not
  bound into the Form 40 booklet and was not retrieved -- see known differences

### Statutes and technical guidance

- Rate schedule: Ala. Code 40-18-5
- Federal income tax deduction: Ala. Const. Amendment 225 / section 211.03 and
  Ala. Code 40-18-15(a)(2) (citations not confirmed from the booklets, which
  simply provide the deduction; the substance is fully verified from the
  worksheet)
- Income concept: Ala. Code 40-18-14
- Standard deduction and dependent exemption: Acts 2022-292 and 2022-297
- Age-65 retirement exclusion: Act 2022-294
- TY2021 federal-deduction recomputation: Act 2022-37 (HB 231)

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 0 with the `ob_*` class shares; `ob_class_floor` 0
  (Alabama has no Pennsylvania-style class loss ring-fence, and is in fact more
  generous than federal law on capital losses); `ob_ss_share` and `ob_ui_share`
  zero; new `ob_ira_share` separating IRA distributions from pension/annuity
  income; `pension_excl_65plus` $6,000 from TY2023 with `pension_excl_min_age` 65
- Known approximations: the defined-benefit exclusion (see below); the
  U.S.-obligation interest share; out-of-state municipal interest missed entirely

### `ded.yaml`

- Encoded: the sliding standard deduction via the new
  `std_po_amount_per_step` / `std_po_floor` pair with filing-status-mapped
  maximum, threshold, step, decrement and floor for three vintages; own Schedule A
  via `item_component_style` 1 with `item_add_payroll`; `item_coupling` 0
  (independent of the federal election); `fed_tax_ded` uncapped with
  `fed_tax_ded_add_niit`, `_less_eitc`, `_less_ctc_ref`, `_less_ed_ref`
- Known approximations: the 4% medical floor cannot be recovered from the
  federal post-7.5%-floor amount; the TY2021 Act 2022-37 recomputation is not
  modeled

### `exempt.yaml`

- Encoded: `personal_amount` $1,500 per taxpayer, $3,000 for head of family;
  the new `dep_tier_bounds` / `dep_tier_amounts` family on Alabama AGI
- Known approximations: Alabama's narrower dependent definition

### `ord.yaml`

- Encoded: `rates` [0.02, 0.04, 0.05] with `brackets_single` [0, 500, 3000] and
  `brackets_married` [0, 1000, 6000]; head of family maps to the single ladder
- Known approximations: the published midpoint tax table differs from the
  statutory schedule by a few dollars below $100,000

### `credits.yaml`

- Encoded: `eitc_match` 0, stated explicitly to record the finding
- Alabama has no state earned income, child, or dependent care credit

### `filing.yaml`

- Encoded: `req_type` 2 with filing-status-mapped gross-income thresholds for
  two vintages

## New generic machinery introduced for Alabama

1. `st_ded.std_po_amount_per_step` and `st_ded.std_po_floor` -- a stepped
   standard-deduction phase-down in fixed DOLLARS with a floor, as against
   Rhode Island's share-per-step. Verified against every printed row of every
   chart: 1,508 of 1,512 (year, status, AGI) probe points exact.
2. `st_exempt.dep_tier_bounds` / `dep_tier_amounts` / `dep_tier_income_base` --
   an income-tiered DEPENDENT exemption, as against the existing tier family
   which sets one amount for taxpayer, spouse and dependents alike.
3. `st_agi.ob_ira_share` -- IRA distributions split out from pension and annuity
   income in the own-base build, because an IRA can never be a defined benefit
   plan. Pennsylvania is unaffected (both shares zero there).
4. `st_ded.fed_tax_ded_add_niit` and the split `_less_eitc` / `_less_ctc_ref` /
   `_less_ed_ref` / `_less_ptc` flags -- the Missouri and Alabama worksheets name
   genuinely different refundable-credit lists, so each is its own flag.

## Worksheet tests added

- AL-1 single with the full federal tax deduction
- AL-2 the TY2019 standard deduction vintage
- AL-3 the standard deduction slide in its stepped region (ninth step)
- AL-4 the flat maximum below the slide
- AL-5a / AL-5b the dependent exemption cliff at $100,000
- AL-6a / AL-6b the federal earned income credit RAISING Alabama tax
- AL-7 Schedule A including the FICA deduction, itemizing while taking the
  federal standard deduction
- AL-8a / AL-8b Act 2022-294's age-65 retirement exclusion arriving in TY2023
- AL-9 Social Security and unemployment compensation fully exempt
- AL-10 the net investment income tax added back to the deductible federal tax

## Known differences

- **Defined-benefit pension exclusion NOT MODELED** (largest Alabama gap).
  Alabama exempts every distribution from an IRC 414(j) defined benefit plan,
  plus each named public system. Federal 1040 line 5b pools defined-benefit with
  401(k)-type distributions and 1099-R box 7 codes do not separate them, so the
  whole pool is taxed. Overstates Alabama tax for defined-benefit retirees, which
  in Alabama is most retirees with pension income. Clears with the Tier 1
  pension-source imputation, alongside the Missouri and New York equivalents.
- **The overtime pay exemption is a genuine hole for TY2024-TY2025.** Alabama
  exempted certain overtime pay (commonly attributed to Act 2023-421) but it
  appears NOWHERE in the Form 40 booklets -- zero hits for "overtime" across all
  nine text extractions -- because it runs through employer withholding and the
  Schedule W-2 state wage figure. Effective and sunset dates, scope, and any cap
  are all unverified. Must be resolved from the Schedule W-2 instructions or the
  act text before Alabama TY2024+ wage results are relied on.
- **The age-65 $6,000 retirement exclusion is encoded per person on an
  unverified reading.** The amount, the age and the TY2023 effective year are
  confirmed from the TY2023 booklet's What's New. Whether the $6,000 is per
  person or per return, which distribution types qualify, and whether any income
  test applies live in the Schedule RS instructions, which were not retrieved.
- **Separate Alabama returns are not modeled.** Alabama's rate ladder is
  perfectly split-neutral (joint brackets are exactly twice single ones), so the
  schedule gives no separate-filing advantage. But the standard deduction slide
  and the dependent exemption cliffs both key on each spouse's OWN Alabama AGI,
  so an unevenly-split couple can do better filing separately -- a $60,000 /
  nothing couple gains $1,750 of standard deduction, and splitting either side of
  the $50,000 dependent bound is worth $500 per dependent. Federally-joint
  couples are treated as filing jointly in Alabama. This needs the generic
  minimum-liability election pass already planned for the Wisconsin Act 15
  retirement election (roadmap item R2.6); Alabama joins that queue. Overstates
  Alabama tax for uneven-income married couples.
- Gross capital gains and losses: Alabama allows the entire loss in the year it
  occurs with no cap and no carryforward, while the model carries the federally
  capped net.
- Alabama public employees' retirement contributions are deferred federally but
  not in Alabama; the difference is invisible on a federal record.
- The 4% medical floor, the severance exclusion, Alabama's own above-the-line
  adjustments, the pass-through entity credit and roughly thirty certificated
  credits are all documented in the yaml files.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the unmodeled defined-benefit exclusion will
  dominate retiree cells; the overtime exemption will show up in TY2024-2025 wage
  cells if TAXSIM models it and we do not; the midpoint tax table gives a few
  dollars of dispersion below $100,000; separate-return elections are not
  modeled. Worth checking whether TAXSIM treats 1040 line 5b as fully taxable in
  Alabama, in which case that cell stays clean.

## Aggregate validation notes

- HT2 targets once weights land; Alabama DOR publishes annual statistical
  abstracts for a revenue-agency benchmark.
