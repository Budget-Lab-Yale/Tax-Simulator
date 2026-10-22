# State Source Packet: Missouri

State: `MO`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-22` (itemized deduction per MO-A Part 2; cross-model close-out); previous: `2026-10-07`

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

## State EITC cross-check, 2026-10-07

Form MO-WFTC excludes married filing separately ("Do you have a filing status of married filing separately
... Yes - STOP", 2024 MO-1040 instructions). It binds because the federal EITC reaches separated spouses with
a qualifying child from TY2021. Now encoded (`st_credits.eitc_mfs_barred` = 1); test MO-1b.

## Data-gap batch (2026-10-13)

- Public pension exemption (MO-A Part 3 Section A; RSMo 143.124.1-.2)
  encoded as a deduction on the `pension_public` overlay
  (`retire_exempt_pub*`): per spouse the lesser of public pension income
  and the year's maximum Social Security benefit -- $37,089 (2017), $37,720,
  $38,437, $39,014, $39,365, $41,373, $44,683, $46,381, $47,633 (2025),
  transcribed from each year's form -- net of the Section C exemption and,
  through TY2023, of income over $85,000 / $100,000 (removed by SB 190).
  Inert until published; the private exemption then applies to the
  remainder. PolicyEngine's 2025 figure (48,216) contradicts the form.
  Tests MO-15a..d.
- Property tax credit (MO-PTS/PTC): waits on the generic circuit breaker
  (S46(a)); PolicyEngine's credit on `salt_prop` and any TAXSIM v37 are
  excluded meanwhile.
- Spec: `research/state_tax/notes/datagap_spec_mi_mo_or.md` §1, §3.

## Itemized deduction and cross-model close-out, 2026-10-22

Read from the MO-1040 booklets (2019, 2023, 2024: line 14 and MO-A Part 2 instructions):

- **Base (line 1, line 9).** The federal Schedule A total, SALT cap included, less only the income-tax share of
  the capped SALT deduction -- Schedule A line 5a over line 5d, times line 5e, the worksheet the booklet supplies
  when the cap binds. New generic `item_component_style` 5. The model had rebuilt the base with uncapped property
  taxes (the v1 convention the old `itemized_salt_convention` entry documented), over-deducting for
  high-property-tax itemizers.
- **Payroll add-on (lines 2-7).** Line 7 is "Schedule 2, line 4 ... minus Schedule 1, line 15" -- self-employment
  tax less the half already deducted above the line; new `item_payroll_seca_er` = 0 (the shared code had added both
  halves, which Alabama keeps). Lines 6-7 include the Additional Medicare Tax (Form 8959): `item_payroll_add_med` = 1.
- **Election (line 14).** "If you itemized on your federal return, you may want to itemize on your Missouri return
  or take the standard deduction, whichever results in a higher deduction"; only a filer *required* to itemize
  federally must itemize. `item_fed_gate` replaces the hard coupling. The federal standard deduction is read as
  `std_ded_potential`, since the federal calculator zeroes `std_ded` for itemizers.
- Worksheet tests MO-16 (prorated SALT share), MO-17 (federal itemizer takes the standard deduction), MO-18 (SE
  half plus Additional Medicare Tax).

The business income deduction stays a subtraction in Missouri AGI (MO-A Part 1 line 17: "a subtraction from federal
adjusted gross income", 2019 and 2023 booklets), as encoded. RSMo 143.124.1 counts traditional IRAs (not Roth) as
private retirement income for Section B, as encoded.

Attributed to the external models (issues doc T75, T76, P59, P60):

- T75 TAXSIM has no business income deduction (TY2018-2020).
- T76 TAXSIM gives no private pension exemption under 65; from 65 it applies the public-pension exemption to every
  pension. The second half is a proxy difference: public versus private is unobserved and we read every pension as
  private until `pension_public` is published (S43).
- TAXSIM exempts Social Security at every age; Section C exempts benefits from 62 and disability benefits at any
  age, and disability is unobserved (proxy difference).
- P59 PolicyEngine computes the business income deduction but nothing applies it.
- P60 PolicyEngine's Section B leaves out IRA distributions.
- Joint returns with one spouse under 62: who receives the benefits is unobserved (we prorate by spouses 62+, the
  PolicyEngine driver gives them all to the head).
- Harness: PolicyEngine's Section B reads `taxable_private_pension_income`, an input the driver never filled, so it
  paid no private exemption at all; for Missouri the driver now copies pensions into it
  (`PE_PRIVATE_PENSION_STATES`).

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
