# State Source Packet: New Jersey

State: `NJ`
Status: `done`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2025
- Baseline only
- Major structural features: own base built from enumerated gross-income
  CATEGORIES with no cross-category loss offsets and no carryovers; no
  standard deduction; a pension exclusion that changed from a cliff to a
  tiered step-down in TY2021; a capped property tax deduction with a flat
  credit alternative

## Primary sources

- All nine NJ-1040 instruction booklets TY2017-TY2025, with PyMuPDF text
  extractions, plus the standalone form PDFs. NOTE `2017_1040.pdf` is
  field-only with no text layer, so TY2017 line references come from the
  booklet
- N.J.S.A. Title 54A for the rules that generate values
- The research pass also machine-compared all 10,015 rows of the printed Tax
  Table against the rate schedule, which is how the $0.50 schedule defect
  below was found

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 0 with the `ob_*` class shares and `ob_class_floor` 1;
  `ob_ss_share` and `ob_ui_share` 0; the income-banded pension exclusion
  (`pension_excl_tier_*`) with per-element filing-status mapping
- Known approximations: elective deferrals; the qualifying-spouse pension
  split; the "other retirement income exclusion"

### `ord.yaml`

- Encoded: Table A and Table B rate schedules for all nine years, padded to
  eight elements

### `ded.yaml`

- Encoded: `prop_tax_ded_cap` ($10,000 then $15,000)
- Known approximations: the medical deduction's 2% floor on New Jersey gross
  income; the 18%-of-rent rule; the $50 credit alternative

### `exempt.yaml`

- Encoded: personal $1,000 per taxpayer, dependent $1,500, age-65 $1,000,
  blind/disabled $1,000
- Known approximations: the veteran and college-dependent exemptions

### `credits.yaml`

- Encoded: the earned income credit (35% to 40%) and the Child Tax Credit
  (tiered on state taxable income, refundable)
- Documented: the child and dependent care credit (the top follow-up)

### `filing.yaml`

- Encoded: $10,000 single/separate, $20,000 joint/head of household/surviving
  spouse, unchanged and unindexed

## New generic machinery introduced for New Jersey

1. `st_agi.pension_excl_tier_{bounds,caps,shares}` -- an income-banded pension
   exclusion computed as min(cap, share x eligible pension income) per band,
   tested on TOTAL income before the exclusion. Covers both the pre-2021 flat
   maximum behind a cliff and the post-2021 tiered step-down. The tier income
   is computed from the starting point ahead of the mutate, because testing
   anything downstream would be circular.
2. `st_ded.prop_tax_ded_cap` -- a capped property tax deduction.
3. `st_credits.ctc_tier_income_base` plus a ninth `st_income_base` enum
   (state taxable income) -- the tiered child credit now selects its tier on
   an enum base rather than hard-coded federal AGI.

## Worksheet tests added

NJ-1 the personal exemption alone; NJ-2 the property tax deduction; NJ-3a and
NJ-3b the cap at $15,000 and its $10,000 predecessor; NJ-4 the pension
exclusion inside the band; NJ-5a and NJ-5b the TY2020 cliff against the TY2021
37.5% tier on the same unit; NJ-6 a business loss failing to offset wages;
NJ-7 the Child Tax Credit tier; NJ-8 the earned income credit at 40%.

## Research findings worth flagging

- **The pension exclusion became tiered in TY2021, not TY2020.** The TY2020
  booklet still prints the flat "$100,000 or less" test at the fully phased-in
  maxima. Getting this wrong shifts a whole year of retiree liability.
- **The age test is 62, not 65**, and it is disjunctive with blindness or
  disability.
- **The Child Tax Credit was introduced in TY2022, not TY2023**, at $500 per
  child, and doubled to $1,000 for TY2023.
- **New Jersey excludes 401(k) deferrals from wages but TAXES 403(b), 457,
  federal Thrift Savings and SEP contributions.** The booklets state both
  sides explicitly. This is why New Jersey W-2 box 16 routinely exceeds box 1.
- **The printed rate schedule carries a $0.50 defect** relative to the Tax
  Table at band boundaries, found by comparing all 10,015 table rows.
- Only the top bracket ever moved (TY2018 and TY2020); nothing is indexed.

## Known differences

- **The child and dependent care credit is not modeled** -- a percentage of
  the FEDERAL credit tiered on New Jersey taxable income, nonrefundable and
  dollar-capped through TY2020 and refundable with a $150,000 ceiling from
  TY2021. It needs a banded percentage-of-federal-credit mechanism the
  calculator does not have. The top New Jersey follow-up.
- **Elective deferrals are not modeled**, so New Jersey wages are federal
  wages. This understates the base for 403(b), 457 and Thrift Savings
  participants -- teachers, public employees and non-profit staff. A Tier 2
  imputation target.
- **The medical expense deduction is not modeled**: New Jersey uses a 2% floor
  on its own gross income against the federal 7.5%, and the extra expenses
  cannot be recovered from the federal post-floor amount.
- **The "other retirement income exclusion" (line 28b) is left out entirely**
  rather than half-encoded. Its unclaimed-pension component turns on an
  ambiguity the booklets do not resolve -- Worksheet D takes a percentage of
  line 27 while line 28a takes a percentage of line 20a -- which determines
  whether that component is dead above $100,000 of total income. A follow-up
  agent was tasked with resolving it against N.J.S.A. 54A:6-15 and bulletin
  GIT-1 & 2.
- The 18%-of-rent property tax rule and the $50 property tax credit
  alternative are not modeled (rent is a Tier 1 target; the credit only beats
  the deduction for very low liabilities).
- The veteran ($3,000, then $6,000 from TY2019) and college-dependent
  exemptions, and the NJEITC's childless flat minimums and sub-federal age
  floor, are all unobserved or unencoded.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the care credit will show in family cells; the
  deferral treatment in public-employee cells; the pension exclusion tiers are
  worth checking carefully either side of TY2021 since an external model that
  dated the change to TY2020 would diverge sharply for that one year.

## Aggregate validation notes

- HT2 targets once weights land; the New Jersey Division of Taxation publishes
  statistics of income for a revenue-agency benchmark.
