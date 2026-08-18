# State Source Packet: Missouri

State: `MO`
Status: `done`
Last updated: `2026-08-18`

## Scope

- Tax years covered: 2017-2026 (2026 rates and brackets from the DOR withholding
  formula; the TY2026 MO-1040 is unpublished)
- Baseline only
- Major structural features: federal-AGI start; equal-width indexed "micro-bracket"
  schedule with PUBLISHED base amounts; capped and AGI-tiered federal income tax
  deduction; married filing combined with a per-spouse rate computation on
  income-share-split taxable income; retirement exemptions taken as deductions
  AFTER state AGI; business income deduction

## Primary sources

### Current forms and instructions

- Form/booklet: MO-1040 Instructions (contains MO-1040, MO-A, MO-CR, MO-NRI,
  MO-PTS, MO-WFTC and the tax chart)
- Instruction PDF: `https://dor.mo.gov/forms/MO-1040%20Instructions_<year>.pdf`
- DOR landing page: dor.mo.gov/forms/
- 2026 Missouri Withholding Tax Formula:
  `https://dor.mo.gov/forms/Withholding%20Formula_2026.pdf`

### Historical forms and instructions

- All nine booklets TY2017-TY2025 downloaded and text-extracted; every rate chart
  transcribed verbatim rather than inferred
- Missing years / gaps: none in the window. TY2026 forms not yet published

### Statutes and technical guidance

- Starting point / conformity: RSMo 143.121 (Missouri AGI), RSMo 143.091 (rolling
  conformity -- federal terms carry their federal meaning, no fixed date)
- Rate schedule: RSMo 143.011 (rates, bracket indexation, trigger reductions)
- Federal income tax deduction: RSMo 143.171
- Retirement: RSMo 143.124 (public and private pension), 143.125 (Social Security)
- Deductions: RSMo 143.131 (standard = federal), 143.141 (itemized)
- Business income deduction: RSMo 143.022
- Working Family Tax Credit: RSMo 143.177
- Filing: RSMo 143.481; combined return required at RSMo 143.031

## Secondary cross-checks

- Used only to sanity-check the TY2026 top rate; every encoded value comes from a
  booklet or the DOR withholding formula

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 1; `add_exempt_int` + `own_state_exempt`; `sub_us_int`;
  `sub_state_ref`; `cap_gains_excl_share` (0 -> 1.0 in TY2025, HB 594);
  `bus_excl_share` (new generic: 0 / .05 / .10 / .15 / .20 by year)
- Known approximations: the business income deduction base floors the whole
  Schedule C + E-part-2 + F pool at zero, where the form floors the Schedule C
  line separately first

### `ded.yaml`

- Encoded: `std_equals_federal` (new generic); itemized coupling both ways;
  `salt_addback`; `item_add_payroll` (new generic); TY2017 Pease;
  `fed_tax_ded` with `fed_tax_ded_less_eitc` / `_less_ed_ref` / `_less_ptc`,
  filing-status-mapped `fed_tax_ded_cap` ($5,000 / $10,000), and the
  `fed_tax_ded_band_upper` / `_band_share` family on state AGI;
  `retire_exempt_ss` (+ min age 62, income limits removed TY2024) and
  `retire_exempt_priv_cap` $6,000 with its own unchanged limits
- Known approximations: per-spouse retirement worksheet applied at the return
  level; the model-wide uncapped-property-tax itemized convention

### `exempt.yaml`

- Encoded: `personal_amount` $2,100 per taxpayer / $3,500 head of family in
  TY2017, zero from TY2018, $1,400 head-of-family-only from TY2022;
  `dep_amount` $1,200 in TY2017
- Known approximations: qualifying widow(er)s cannot receive the $1,400 because
  the model folds that status into married filing jointly

### `ord.yaml`

- Encoded: `brackets`, `rates` and `base_amounts` for all nine published charts
  plus projected TY2026; `combined_split` and `combined_split_round` (both new
  generics)
- Known approximations: TY2026 base amounts are integrated-and-rounded, not
  transcribed

### `credits.yaml`

- Encoded: `eitc_match` (0.10 TY2023, 0.20 TY2024+) with `eitc_refundable` 0
- Known approximations: credit ordering relative to the unmodeled property tax
  credit

### `filing.yaml`

- Encoded: `req_type` 2, `req_if_fed_filer`, `req_income_thresh` $1,200

## New generic machinery introduced for Missouri

All default-neutral; the full suite stays green across the other 35 states.

1. `st_ded.fed_tax_ded` and its component flags -- the federal income tax
   deduction, base = 1040 line 22 (`liab_bc - nonref`), with per-state flags for
   which refundable credits reduce it and whether the net investment income tax
   is added back, a filing-status-mapped cap, and an income-banded share family.
   Shared with Alabama and Oregon.
2. `st_ord.combined_split` / `combined_split_round` -- pooled deductions, taxable
   income split by each spouse's share of state AGI (rounded half UP to whole
   percent), schedule run on each share. Distinct from KY's `combined_sep`,
   which splits deductions per spouse and takes the better of joint and separate.
3. `st_ded.std_equals_federal` -- adopt the federal standard deduction outright
   rather than transcribing a copy that can drift.
4. `st_ded.item_add_payroll` -- employee payroll and self-employment taxes added
   to the state itemized base. Shared with Alabama.
5. `st_ded.retire_exempt_*` -- a retirement exemption taken as a DEDUCTION after
   state AGI, each piece falling dollar-for-dollar with income over its own limit.
6. `st_agi.bus_excl_share` -- a flat share of zero-floored business income
   subtracted, as against Ohio's cap-and-carve-out.
7. `sched_tax_at()` in st_tax.R is now base-amount aware, so the published
   schedule (not the smooth one) drives the combined-return split.

## Worksheet tests added

- MO-1 basic single with the federal deduction at 25%
- MO-2 the band CLIFF at $50,000 of state AGI
- MO-3 the $10,000 combined cap binding in TY2018 (pre-percentage)
- MO-4 combined return splitting taxable income 60/40
- MO-5 whole-percent income shares
- MO-6 the pre-2023 whole-income 1.5% band
- MO-7 TY2017 personal and dependent exemptions
- MO-8 Social Security exempt while the pension exemption is phased out
- MO-9 private pension exemption partially reduced
- MO-10a / MO-10b SB 190 removing the Social Security income limit at TY2024
- MO-11 Working Family Tax Credit, and the earned income credit netting the
  deductible federal base
- MO-12 itemized deductions with the payroll add-on
- MO-13 the business income deduction at 20%
- MO-14 the TY2025 full capital gains subtraction

## Known differences

- **Public pension exemption NOT MODELED** (largest Missouri gap). MO-A Part 3
  Section A exempts public pension income up to the year's maximum Social
  Security benefit ($37,089 in TY2017 to $47,633 in TY2025) per spouse. The
  public/private split of `txbl_pens_dist` is unobserved in the PUF -- the same
  limit that leaves the New York government-pension exclusion unmodeled.
  Missouri filers receive only the $6,000 private exemption on the whole pension
  pool. Overstates Missouri tax for government retirees; Missouri has an
  unusually large public-retiree population. Clears with the Tier 1
  pension-source imputation.
- Military retirement, active-duty pay, MOST 529 and ABLE contributions,
  long-term care premiums, health care sharing ministry payments, depreciation
  timing differences and the farmland/first-time-homebuyer/foster-parent
  deductions are all unobserved.
- Property tax credit (circuit breaker) blocked on the Tier 1 rent/property-tax
  imputation, and on its own "net household income" add-back schedule.
- Kansas City and St. Louis earnings taxes, which stay deductible inside the
  MO-A Part 2 state-tax subtraction, are not modeled.
- Aged/blind standard deduction add-ons ride the federal amount, so they are
  exact.
- TY2026 is projected: rates and brackets from the DOR withholding formula, base
  amounts integrated and rounded.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the unmodeled public pension exemption will dominate
  any retiree-heavy cell; the whole-percent income-share rounding produces small
  discontinuities; TAXSIM's handling of the federal-tax-deduction percentage
  bands should be checked against the single-schedule-for-all-statuses rule,
  which secondary implementations commonly get wrong by doubling the bands for
  joint filers.

## Aggregate validation notes

- HT2 targets once weights land; Missouri DOR publishes annual individual income
  tax statistical reports for a revenue-agency benchmark.
