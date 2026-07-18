# Virginia — Source Packet

**Status:** encoded (baseline TY2017–2025, projected through the statutory
TY2030 standard-deduction reversion) | worksheet tests VA-1..VA-13 pass |
cross-model todo | aggregate blocked on weights.

**Primary sources:** Virginia Form 760 instruction booklets, TY2017–2025
(all nine downloaded from tax.virginia.gov and transcribed per year);
Virginia Schedule A instructions 2019–2025; Va. Code §§ 58.1-301, -320,
-321, -322.01/.02/.03, -324, -339.8; Tax Bulletins 23-1 etc. Full
booklet-verified research records with per-row citations:
`raw/va_research_core.md` (rates, standard deduction, itemization/Pease,
exemptions, thresholds, conformity, filing statuses) and
`raw/va_research_mods_credits.md` (age deduction, SS/refund/UI/US-obligation
items, disability/military/529, CDCTC deduction, CLI/EITC, Spouse Tax
Adjustment worksheet).

## Structure

FAGI (Line 1) + additions − age deduction − taxable SS − state refund −
other subtractions = **VAGI** (Line 9; filing threshold and no-tax cliff
apply here) − standard/itemized deduction − Schedule ADJ deductions (child
care) − exemptions = VA taxable income → single rate schedule → Spouse Tax
Adjustment → CLI/EITC credit.

Filing statuses: single/MFJ/MFS only. **No HoH status** — federal HoH files
as Single (informational oval only); all mappers set '4' to single values.
Exception: VA's Pease-style limitation uses *federal* filing status, so
pease_thresh maps HoH separately.

## Encoded parameters (highlights)

- **Rates:** 2/3/5/5.75% at $0/3k/5k/17k, one schedule for all statuses,
  constant 2017–2025 (unchanged since 1990).
- **Spouse Tax Adjustment** (`st_ord.sta_max` = 259, new generic param):
  MFJ separate-schedule recomputation, 50/50 cap on the notional split.
- **Standard deduction** (mapper, single=MFS=HoH): 3,000/6,000 → 4,500/9,000
  (2019) → 8,000/16,000 (2022) → 8,500/17,000 (2024) → 8,750/17,500 (2025)
  → 9,200/18,400 (2027) → 9,300/18,600 (2028) → 3,000/6,000 (2030, statutory
  reversion per 2026 GA). Dependent filers: min(earned income, std), no floor.
- **Itemization:** coupled to federal (item_coupling=1); component style
  with income/sales taxes excluded (SALT addback); property taxes uncapped
  (VA deconformed from the federal SALT cap TY2019+); VA's own Pease
  limitation TY2019+ at VA-published thresholds (2017 federal Pease; 2018
  none), thresholds transcribed for every year 2019–2025.
- **Exemptions:** $930/person+dependent, $800 aged/blind add-ons
  (taxpayer/spouse only, stackable), no phase-out. `personal_amount` is per
  taxpayer (x2 MFJ) — matches the machinery convention.
- **Age deduction** (new generic `st_agi.age_ded_*` family): $12,000/person
  65+; born ≤1/1/1939 exempt from the income test (year-keyed end-of-year
  age = TY−1938); otherwise the combined pool (12,000 × claimants) reduced
  $1-for-$1 by AFAGI (FAGI − taxable SS; po_base=2) over $50k single/$75k
  married-combined.
- **Subtractions:** 100% taxable SS; state refunds; 100% unemployment
  compensation (new generic `st_agi.sub_ui_share`); US-obligation interest
  (flag; no-op known-difference); non-VA muni addback with own-state carve-out.
- **Child/dependent care DEDUCTION** (new generic `st_ded.care_exp_ded_*`):
  federal credit expense base, capped $3,000/one, $6,000/two+ in ALL years
  including TY2021 (booklet controls over ARPA's federal caps).
- **CLI/EITC** (Schedule ADJ): household claims ONE of CLI ($300/exemption,
  family VAGI ≤ poverty guideline — table transcribed for all nine years;
  new generic `cli_*` params), 20% federal EITC nonrefundable, or (2022+)
  refundable match — 15% for 2022–2024, **20% for 2025+** (nonref option
  dropped from the 2025 form as dominated). Unit takes the max benefit.
- **Age package ↔ CLI/EITC exclusivity** (new generic machinery): neither
  CLI nor EITC if anyone claims the age deduction or 65+/blind exemption,
  household-wide; the unit takes the larger side, approximated at the top
  schedule rate (decision in calc_st_agi, consumed by st_exempt/st_credits).
- **Filing threshold / no-tax cliff:** VAGI ≥ $11,950 ($23,900 MFJ) must
  file; BELOW it tax is $0 outright (new generic
  `st_filing.no_tax_below_thresh`). Encoded as threshold−1 (form tests "or
  more", calculator tests strict greater-than).
- **Conformity:** group 3 (fixed-date annual) 2017–2022; group 0 (rolling)
  2023–2025; group 4 (fixed Dec 31, 2025 + extenders; new group) 2026+.
  enabled:false in jurisdictions.yaml (no reference-law bridge), like SC/CA.

## Known differences (documented, not modeled)

1. **Military subtractions** — basic pay ($15k, phase-out), National Guard,
   combat pay, and the Military Benefits Subtraction ramp
   ($10k/$20k/$30k/$40k, 2022→2025, per spouse). Material for VA's large
   military-retiree population; grows the pension-side gap from TY2022.
2. **Disability income subtraction** ($20k/person, exclusive with the age
   deduction per person) — disability wages unobserved.
3. **529 ($4k/account), LTC premiums, WOTC wage subtraction, Tier 2 RR,
   narrow codes** — unobserved.
4. **TY2018 SALT-cap flow-through:** the one-year federal cap + addback
   proration on VA itemized is not reproduced (property taxes uncapped here
   in 2018). TY2019+ treatment is exact.
5. **Sales-tax electors:** capped-but-not-added-back Line 5a sales-tax
   deduction not modeled (income-tax addback assumed).
6. **Medical floor:** VA retains a 10%-of-FAGI floor; federal deductible
   amount used (small gap 2017–2020 when the federal floor was 7.5%).
7. **MFS combined-spouse AFAGI** for the age-deduction phase-out and the
   one-spouse CLI rule / MFS EITC proration — own-record income used.
8. **STA:** joint non-wage income split 50/50 (ownership unobserved);
   published $259 tax-table-rounding cap vs continuous $257.50 max.
9. **Exclusivity approximation:** the age-package-vs-EITC choice compares
   package value at the top schedule rate against the best EITC match,
   uncapped by liability; exact only in liability terms.
10. **Schedule CR / OSC credits** (land preservation, PTET, other-state
    credit, etc.) — not modeled.
11. **Poverty guidelines and Pease thresholds** carried forward flat after
    2025 (published annually; refresh on later encoding passes).
12. **CLI family VAGI** excludes dependents' own income (unobserved).

## Machinery added for VA (all generic, neutral-defaulted)

`st_ord.sta_max`; `st_agi.age_ded_amount/_min_age/_no_test_min_age/
_po_thresh/_po_base`, `st_agi.age_excl_eitc`, `st_agi.sub_ui_share`;
`st_exempt.aged_blind_addl_excl_eitc`; `st_ded.care_exp_ded*`;
`st_credits.eitc_match_alt`, `eitc_refundable_alt`, `cli_amount`,
`cli_poverty_bounds1..8`, `cli_poverty_addl`,
`eitc_cli_excl_age_package`; `st_filing.no_tax_below_thresh`; plus new
calc_st_agi outputs `st_age_package_taken`/`st_age_package_forgone`.
Bug fix ridden along: `sched_tax_at()` in st_tax.R treated a trailing-NA
bracket column as the top bracket's upper bound (dropped top-bracket tax
in mixed-bracket-count law slices).
