---
title: "Data-gap implementation spec: Michigan, Missouri, Oregon"
role: notes
workstream: state_tax
status: open
updated: 2026-10-13
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Data-gap implementation spec: Michigan, Missouri, Oregon

Decision S43 (`research/decisions_log.md:59`) says a provision blocked on data
is encoded against a named optional overlay variable with a neutral default,
inert today and live when a producer publishes the variable. Decision S46
(`research/decisions_log.md:62`) sets three scope calls this note respects:
(a) the Michigan homestead property tax credit (MI-1040CR) and the Missouri
property tax credit (MO-PTS/MO-PTC) go on ONE generic refundable
circuit-breaker component that reads only the `rent_paid`,
`property_tax_paid` and `hh_resources_inc` overlay variables, never
`salt_prop`, and JI reviews the provisions before the component ships (plan
`research/state_tax/plan.md:629`); (b) the Michigan home heating credit
(MI-1040CR-7) stays OUT of liability; (c) any provision that reads a
prior-year state liability -- the Oregon surplus kicker -- is deferred, not
encoded, and excluded in the harness where an external model pays it.

This is the executable spec for the three states: the circuit breaker (§1),
Michigan's Tier-1 unlimited public pension subtraction and blind special
exemption (§2), Missouri's public pension exemption (§3), Oregon's
retirement income credit and the missing other-states bond-interest addition
(§4), the known-difference rows the harness needs (§1.7, §2.4, §3.4, §4.5),
the our-side fixes the packets left pending (§5) and the execution checklist
(§6). It follows the recipe in
`research/state_tax/supplemental_inputs_interface.md:149-161` (§7) and the
shape of the sibling note `research/state_tax/notes/datagap_spec_hi_ky_pa.md`.

Conventions used throughout:

- "the resolver" = `ensure_st_data()`, `src/calc/state/do_state_taxes.R:159-207`:
  overlay column present -> value with default for NA and `has_<v>` TRUE
  where non-NA; native column present -> untouched, `has_<v>` TRUE; declared
  `tax_data_equivalent` present -> that column; otherwise default, `has_<v>`
  FALSE.
- "the schema" = `config/interfaces/supplemental_inputs_schema.yaml`
  (`rent_paid` at `:72-79`, `property_tax_paid` at `:80-87`, `pension_public`
  at `:90-97`, `hh_resources_inc` at `:226-234`, `disabled1/2` at `:262-275`,
  `prior_year_liab` at `:351-358`); "the crosswalk" =
  `research/state_tax/tax_data_gaps.csv`; "the law schema" =
  `config/scenarios/tax_law_state/params_schema.yaml`.
- Worksheet cases are written for `run_case()` in
  `src/tests/state/test_state_calc.R:56-82`; an overlay-live case sets the
  bare variable name on the unit (the resolver's branch 2 treats it as a
  native column, `has_<v>` TRUE), as the invariance test's `moved` check does
  with `sup.interest_us_share`
  (`src/tests/state/test_supplemental_inputs_reader.R:258-262`).
- Harness record columns available to known-difference predicates are the
  headers of `research/state_tax/cross_model/results/raw/policyengine_2024.csv`
  and `taxsim_2019.csv`: `id ... filing_status, agi, age1, age2, gross_ss,
  n_dep, ui, txbl_int, ei1, ei2, txbl_pens_dist, txbl_ira_dist, other_inc,
  alimony, itemizing, n_dep_ge18, n_dep_ge65, care_exp, kg_st, kg_lt, ...,
  salt_prop, exempt_int`, the TAXSIM `v30_state_household_income`,
  `v31_state_rent_expense`, `v37_state_property_tax_credit` intermediates,
  and the `pe_*` exports (built at
  `src/tests/state/test_state_cross_model.R:967-1010`). There is no
  `wages1/2` and no `blind1/2` on the record today (§2.4 adds `blind_n`).
- What the external models receive: TAXSIM gets `proptax = salt_prop` and
  `rentpaid = 0` (`src/tests/test_taxsim.R:127,260`); PolicyEngine gets
  `real_estate_taxes = salt_prop` (`test_state_cross_model.R:396`,
  `src/tests/state/cross_model/pe_state_tax.py:80`) and no rent, no
  blindness, no disability, no public-pension split, no prior-year liability
  (`pe_state_tax.py:55-87` is the whole input map). So both external models
  can pay a property-tax circuit breaker on federal itemizers' `salt_prop`
  and nothing else, which is what the known-difference rows key on.
- The parent landed the HI/KY/PA batch while this note was written:
  `pension_public` is already in `calc_st_agi` req_vars (`st_agi.R:49`) and
  the KY grandfather term `st_sub_pens_grand` sits at `st_agi.R:574-579,643`.
  The Michigan change in §2.1 composes with it (different parameter, different
  stage of the cap); line numbers below are the working tree of 2026-10-13.

## 0. Summary of what the parent executes

| # | State | Provision | Overlay variables (schema) | New law params (law schema) | Stage change | Gate |
|---|---|---|---|---|---|---|
| 1 | MI, MO | Refundable circuit breaker: MI-1040CR homestead credit (MCL 206.520, 206.522); MO-PTS/PTC property tax credit (RSMo 135.010-135.030) | `rent_paid`, `property_tax_paid`, `hh_resources_inc` (all exist) | `st_credits.cb_*` family, 29 scalars (§1.3) + `credit_tables.csv` ids `cb_senior_share`, `cb_exempt_rate` (MI) | NEW block in `src/calc/state/st_credits_household.R` after the LICTR block (`:347-370`), new return `st_cb_credit`; `st_credits.R:579-589` refundable sum, `:550` nonrefundable sum | none: countable property tax is 0 until a producer publishes rent or property tax, so every style is inert |
| 2 | MI | Tier-1 (born before 1946) unlimited public pension subtraction (MCL 206.30(9)(a); Form 4884 Section A) | `pension_public` (exists) | `st_agi.pension_excl_public_uncapped_min_age` (dbl, `.inf`) | `st_agi.R:292-315` (band cap block) | none (default `.inf`; `pension_public` default 0 gives `max(cap, 0) = cap`) |
| 3 | MI | Special exemption for blind filers (MI-1040 line 9b; MCL 206.30(3)(a)) | none -- `blind1/2` are Tax-Data columns already in `calc_st_exempt` req_vars (`st_exempt.R:44-45`) | none -- `mi/exempt.yaml` `blind_addl` 0 -> the line 9b amounts | none (law fix) | n/a |
| 4 | MO | Public pension exemption, MO-A Part 3 Section A (RSMo 143.124.1-.2) | `pension_public` | `st_ded.retire_exempt_pub` (int, 0), `st_ded.retire_exempt_pub_cap` (dbl per_person, 0), `st_ded.retire_exempt_pub_limit` (dbl per_return, `.inf`), `st_ded.retire_exempt_pub_less_ss` (int, 0) | `src/calc/state/st_ded.R:633-657` | none |
| 5 | OR | Retirement income credit (ORS 316.157; OR-17 2025 p.106, credit code 811) -- law, not data | `hh_resources_inc` (household-income add-back; exists) | `st_credits.retire_inc_credit_*`, 7 scalars (§4.2) | `src/calc/state/st_credits_senior.R:84-100` (folded into `st_retire_credit`) | n/a (law) |
| 6 | OR | Other states' bond interest addition (ORS 316.680(2)(a); OR-17 2025 p.63, addition code 158) -- law, not data | `interest_own_state_share` (exists, default 0.75) | none -- `or/agi.yaml` sets `add_exempt_int: 1`, `own_state_exempt: 1` | none (existing `st_add_muni`, `st_agi.R:481-482`) | n/a (law fix) |
| -- | OR | Surplus kicker (ORS 291.349) | `prior_year_liab` (exists) | **none -- DEFERRED by S46(c)** | none | harness rows only (§4.5) |
| -- | MI | Home heating credit (MI-1040CR-7) | -- | **none -- OUT by S46(b)** | none | harness rows only (§1.7) |

Our-side fixes that need no data (§5): the MI blind exemption (row 3 above);
the OR other-states bond addition (row 6); the OR retirement income credit
(row 5); the stale `or/agi.yaml:24-26,33-34` text that says no U.S.-interest
subtraction is taken when `sub_us_int: 1` subtracts 15% of `txbl_int`; and
the stale packet and reference strings listed per state.

## 1. The circuit breaker (MI-1040CR homestead credit; MO-PTS/PTC)

### 1.1 Michigan law

MCL 206.520 and 206.522 (text fetched from legislature.mi.gov 2026-10-13,
reachable from the cluster) and the 2024 MI-1040CR instructions (NBER mirror
`historical_state_tax_forms/MI/2024/MI-1040CR Instructions.pdf`, pp.32-34;
2019 instructions pp.33 for the pre-2021 bands):

- **Countable property tax** (MI-1040CR lines 10-13). Owners: property taxes
  levied on the homestead. Renters: 23% of gross rent for TY2018+, 20% for
  TY2017 (MCL 206.520(2); line 12 "Multiply line 11 by 23%"). The sum is
  line 13. Owners with a taxable value above $135,000 (indexed from 2021:
  $143,000 2022, $154,400 2023, $160,700 2024, $165,400 2025) are ineligible
  (206.520(1)) -- unobservable, documented.
- **Total household resources** (lines 14-33): every household member's
  income, taxable or not -- wages, interest, dividends, business/farm/rental
  net income (losses do not reduce it), pensions (including the Michigan
  subtractions), gross Social Security and SSI, railroad retirement, child
  support, unemployment, gifts over $300, veterans' and workers'
  compensation benefits, FIP/MDHHS cash -- less health insurance premiums
  (line 31) and federal Schedule 1 adjustments (line 30). MCL 206.508(4).
- **The exemption** (line 34): 3.2% of total household resources (3.5%
  before TY2018) for general claimants, MCL 206.522(1)(a); for senior
  citizens and the deaf/blind/disabled (Table 2 "other claimants"), a ladder
  by resources: 0% to $3,000; 1% $3,001-$4,000; 2% $4,001-$5,000; 3%
  $5,001-$6,000; 3.2% above $6,000 (3.5% before 2018), MCL 206.522(1)(c).
  Line 35 = line 13 - line 34, floored at zero.
- **The share** (lines 36-41): general claimants 60% of line 35 (Section C,
  206.522(1)(a)); senior citizens the Table A percentage of line 35 (Section
  A, 206.522(1)(b)): 100% at resources of $21,000 or less, then 96 / 92 /
  88 / 84 / 80 / 76 / 72 / 68 / 64% in $1,000 bands to $30,000, 60% above
  $30,000 (frozen in statute 2017-2025); deaf/blind/disabled 100% (Section B,
  206.522(1)(c)). "Senior citizen" is the MI-1040CR box 5a: "Age 65 or
  older; or an unremarried spouse of a person who was 65 or older at the
  time of death" (2024 form line 5a).
- **Cap** (line 38/39/41 "maximum $1,800"): $1,200 TY2017; $1,500 TY2018-2021;
  $1,600 2022; $1,700 2023; $1,800 2024; $1,900 2025 (MCL 206.520(15), CPI
  indexed from 2021 rounded to $100; the printed form maxima).
- **Phase-out** (lines 43-44, Table B): the credit is reduced 10% for each
  $1,000 or part of $1,000 of resources over the threshold -- $41,000 TY2017
  (206.520(8) "for tax years before the 2018 tax year"); $51,000 2018-2020;
  $51,600 2021; $54,000 2022; $58,300 2023; $60,700 2024; $62,500 2025
  (indexed from 2021, rounded to $100). Zero at the threshold plus $9,001
  (the printed resources ceiling: $50,000 2017, $60,000 2018-2020, $60,600
  2021, $63,000 2022, $67,300 2023, $69,700 2024, $71,500 2025).
- **Alternate credit for senior renters** (MCL 206.520(9)-(10); Worksheet 5):
  a 65+ renter takes the larger of the line 44 credit and rent paid less 40%
  of total household resources, capped at the same maximum.
- **FIP/MDHHS proration** (206.520(7), Worksheet 4): the credit is scaled by
  (resources - FIP/MDHHS cash) / resources. `hh_resources_inc` pools TANF with
  SSI and the rest, so the proration is not separable; documented.
- Refundable (206.520(3)); claimed on the MI-1040 line 27b for filers.

### 1.2 Missouri law

RSMo 135.010, 135.025, 135.030 (revisor.mo.gov, fetched 2026-10-13, text
effective 2025-08-28 carrying HB 594 & 508 of 2025) and Form MO-PTS 2024 /
the 2025 MO-1040 instructions pp.17-18 and chart pp.50-52 (NBER mirror):

- **Claimant** (135.010(1)): the claimant or spouse is 65 or older at year
  end and a full-year resident; or a 100% service-connected disabled
  veteran; or "disabled" (135.010(2), the SSDI standard); or 60 or older and
  received surviving-spouse Social Security benefits. Married couples who
  may file jointly must claim on a combined return with combined income.
- **Income** (135.010(5)): Missouri AGI (MO-1040 line 6, i.e. BEFORE the
  Part 3 retirement exemption, which is line 8) less the spouse exemption
  ($2,000; $4,000 if the homestead was owned and occupied all year; $2,800 /
  $5,800 from TY2026), "increased, where necessary, to reflect" (a) Social
  Security, railroad retirement and veterans' benefits; (b) all other public
  and private pensions and annuities; (c) public relief, public assistance
  and cash unemployment benefits; (d) no deduction for losses not incurred in
  a trade or business; (e) interest on U.S., state and local obligations. The
  form (MO-PTS lines 1-10) walks exactly that list; line 9 enters $0 for
  single or married living separate, $2,000 for married combined renters,
  $4,000 for married combined owners.
- **Countable property tax** (135.025; MO-PTS lines 11-13): actual property
  tax paid up to $1,100 for owners; "rent constituting property taxes" = 20%
  of gross rent (Form MO-CRP line 9) up to $750 for renters; the sum capped
  at $1,100. From TY2026: $1,550 / $1,055 (HB 594 & 508), CPI-indexed from
  2027.
- **Credit** (135.030.2(1)): where net household income is at or below the
  maximum upper limit, the credit is the countable property tax less a
  percentage of income: 0% up to the minimum base ($14,300 from 2008);
  "1/16 percent accumulative per $300 from 0 percent to 4 percent" above
  it. Maximum upper limit (135.030.1(1)): $27,500 for renters and $30,000
  for owner-occupiers through TY2025 (the form's "$27,200" renter cliff is
  where the $750 renter credit reaches zero inside that limit); from TY2026
  $38,200 single renter / $42,200 single owner / $41,000 married-combined
  renter / $48,000 married-combined owner, CPI-indexed from 2027.
- The published chart (2025 MO-1040 instructions pp.50-52; 2021 "MO-PTC
  Claim Chart") evaluates the statutory formula at the midpoint of each $25
  property-tax column and each $300 income row (verified on the 2021 chart:
  row $19,701-$20,000, column $976-$1,000 prints $752 = $988 - 19 x 0.0625% x
  $19,850), so a record-level exact computation differs from the chart by up
  to about $15. Encode the statute, not the chart.
- Refundable (MO-1040 line 43 is in the refundable block; MO-WFTC line 9 then
  limits the nonrefundable Working Family Tax Credit to tax remaining after
  it -- `mo/credits.yaml:22,26`).

### 1.3 The component: `st_credits.cb_*`

One block in `src/calc/state/st_credits_household.R`, placed after the
LICTR block (after `:370`, before the `list(` at `:372`), returned as
`st_cb_credit`. It reads `rent_paid`, `property_tax_paid`, `hh_resources_inc`
and the observed unit (ages, filing status, federal or state AGI, nontaxable
Social Security, exempt interest, loss components) -- never `salt_prop`
(S46(a)). Tenure is inferred from which of the two money overlays is
positive, so the `tenure` overlay (`schema:64-71`) is not read either.

Law schema, a new block after `percap_after_eitc` (`params_schema.yaml:852`),
all at inert defaults:

```yaml
  # Refundable property-tax circuit breaker on the rent_paid / property_tax_paid
  # / hh_resources_inc optional data columns (S43, S46(a)): MI-1040CR homestead
  # credit (MCL 206.520/206.522) and MO-PTS/MO-PTC property tax credit (RSMo
  # 135.010-135.030). Inert while the money overlays are at their zero
  # defaults: countable property tax is zero, so every style pays zero
  st_credits.cb_style: 0                 # 0 off; 1 = share of (countable tax - exempt share of resources), MI; 2 = countable tax less a stepped % of income, MO
  st_credits.cb_refundable: 1
  st_credits.cb_income_base: 1           # st_income_base enum (MI 1 federal AGI; MO 2 state AGI = MO-1040 line 6)
  st_credits.cb_add_nontax_ss: 0         # 1 = add gross_ss - txbl_ss (both)
  st_credits.cb_add_exempt_int: 0        # 1 = add exempt_int (both)
  st_credits.cb_add_hh_resources: 0      # 1 = add hh_resources_inc (both)
  st_credits.cb_unnet_losses: 0          # 1 = add back the positive part of each loss (the lictr list; MI floors business losses, MO adds back nonbusiness losses)
  st_credits.cb_income_offset_joint_owner: {default: 0, units: per_return}   # MO $4,000 (married combined, owned all year)
  st_credits.cb_income_offset_joint_renter: {default: 0, units: per_return}  # MO $2,000
  st_credits.cb_rent_share: 0            # rent constituting property taxes (MI 0.20 TY2017, 0.23 after; MO 0.20)
  st_credits.cb_rent_cap: .inf           # MO $750 ($1,055 from 2026)
  st_credits.cb_prop_cap: .inf           # MO $1,100 ($1,550 from 2026)
  st_credits.cb_countable_cap: .inf      # MO $1,100 on the sum ($1,550 from 2026)
  st_credits.cb_exempt_share: 0          # MI 0.035 / 0.032: share of resources the general claimant absorbs before the credit
  st_credits.cb_share: 0                 # MI 0.60 of the excess for general claimants
  st_credits.cb_senior_min_age: .inf     # MI 65 on the older spouse: Table A share ladder (credit_tables cb_senior_share) and the senior/disabled exemption ladder (cb_exempt_rate)
  st_credits.cb_senior_rent_alt: 0       # MI 1: senior renters take the larger of the credit and rent less cb_senior_rent_alt_share of resources (206.520(9))
  st_credits.cb_senior_rent_alt_share: 0 # MI 0.40
  st_credits.cb_credit_max: .inf         # MI $1,200 .. $1,900 (206.520(15))
  st_credits.cb_po_thresh: .inf          # style 1: MI Table B start ($41,000 .. $62,500); style 2: MO minimum base ($14,300)
  st_credits.cb_po_step: 1               # MI $1,000; MO $300
  st_credits.cb_po_rate: 0               # style 1: share of the CREDIT lost per step (MI 0.10); style 2: share of INCOME subtracted per step (MO 1/16% = 0.000625)
  st_credits.cb_po_pct_max: .inf         # style 2 ceiling on the accumulated percentage (MO 0.04)
  st_credits.cb_elig_age: 0              # either spouse at least this age (MO 65; MI 0 = no age test)
  st_credits.cb_income_limit_owner: .inf # style 2 maximum upper limit, owner-occupiers (MO $30,000; mapped by status from 2026)
  st_credits.cb_income_limit_renter: .inf # ... renters (MO $27,500)
  st_credits.cb_elig_disabled: 0         # REVIEW ITEM: 1 = the disabled1/2 optional flag also qualifies (MO "disabled"; MI 100% share) -- off until JI rules on S46(a)'s "only"
  st_credits.cb_disabled_share: 0        # MI 1.00 where cb_elig_disabled is on
  st_credits.cb_offsets_nonref: 0        # MO 1: nonrefundable credits are limited to tax remaining AFTER the circuit breaker (Form MO-WFTC line 9)
```

That is 29 scalars. `cb_income_limit_owner/renter` are filing-status mapped in
`mo/credits.yaml` (single vs married combined, which only differ from
TY2026); through 2025 every status shares one value.

Dense tables (a new `credit_tables.csv` beside
`config/scenarios/tax_law_state/baseline/mi/credits.yaml`; schema `credit_id,state,year,filing_status,key_concept,income_lower,income_upper,value`,
`src/data/state_tax_law.R:79-130`):

```
credit_id,state,year,filing_status,key_concept,income_lower,income_upper,value
cb_senior_share,MI,2017,0,0,0,21000,1.00
cb_senior_share,MI,2017,0,0,21001,22000,0.96
cb_senior_share,MI,2017,0,0,22001,23000,0.92
cb_senior_share,MI,2017,0,0,23001,24000,0.88
cb_senior_share,MI,2017,0,0,24001,25000,0.84
cb_senior_share,MI,2017,0,0,25001,26000,0.80
cb_senior_share,MI,2017,0,0,26001,27000,0.76
cb_senior_share,MI,2017,0,0,27001,28000,0.72
cb_senior_share,MI,2017,0,0,28001,29000,0.68
cb_senior_share,MI,2017,0,0,29001,30000,0.64
cb_senior_share,MI,2017,0,0,30001,999999999999,0.60
cb_exempt_rate,MI,2017,0,0,0,3000,0
cb_exempt_rate,MI,2017,0,0,3001,4000,0.01
cb_exempt_rate,MI,2017,0,0,4001,5000,0.02
cb_exempt_rate,MI,2017,0,0,5001,6000,0.03
cb_exempt_rate,MI,2017,0,0,6001,999999999999,0.035
cb_exempt_rate,MI,2018,0,0,0,3000,0
cb_exempt_rate,MI,2018,0,0,3001,4000,0.01
cb_exempt_rate,MI,2018,0,0,4001,5000,0.02
cb_exempt_rate,MI,2018,0,0,5001,6000,0.03
cb_exempt_rate,MI,2018,0,0,6001,999999999999,0.032
```

Sources: MCL 206.522(1)(b) for Table A (identical in the 2019 and 2024
instructions, pp.33/32), MCL 206.522(1)(c) for the exemption ladder.
`state_credit_tables_for_year()` (`state_tax_law.R:177-200`) carries the
2017 Table A and the 2018 ladder forward, which is the law (both frozen).
The Missouri step rule is parametric (`cb_po_*`), because RSMo 135.030.2(1)
is a formula and the printed chart is its midpoint approximation (§1.2); if
JI prefers the chart reproduced cell-for-cell it is a `cb_amount` table of
14 columns x ~50 rows per vintage, keyed by the countable-tax column, which
this note does not recommend.

Stage code (new block in `st_credits_household.R`; `n_aged`, `n_taxpayers`
and `lictr_opt_col()` are already in scope at `:127,105,344`):

```r
  #--------------------------------------------------------------------
  # Refundable property-tax circuit breaker (MI-1040CR homestead credit,
  # MCL 206.520/206.522; MO-PTS/MO-PTC property tax credit, RSMo
  # 135.010-135.030). Reads ONLY the rent_paid / property_tax_paid /
  # hh_resources_inc optional data columns for the money inputs (S46(a));
  # tenure is inferred: owner = property tax paid and no rent. Countable
  # tax is zero at the schema defaults, so the block is inert until a
  # producer publishes rent or property tax
  #--------------------------------------------------------------------
  cb_owner     = tax_unit$property_tax_paid > 0 & tax_unit$rent_paid <= 0
  cb_countable = pmin(
    pmin(pmax(0, tax_unit$property_tax_paid), tax_unit$st_credits.cb_prop_cap) +
    pmin(tax_unit$st_credits.cb_rent_share * pmax(0, tax_unit$rent_paid),
         tax_unit$st_credits.cb_rent_cap),
    tax_unit$st_credits.cb_countable_cap)
  cb_income = st_income_base(tax_unit, tax_unit$st_credits.cb_income_base) +
    tax_unit$st_credits.cb_add_nontax_ss *
      pmax(0, tax_unit$gross_ss - tax_unit$txbl_ss) +
    tax_unit$st_credits.cb_add_exempt_int * tax_unit$exempt_int +
    tax_unit$st_credits.cb_add_hh_resources * tax_unit$hh_resources_inc +
    tax_unit$st_credits.cb_unnet_losses * (
      pmax(0, -tax_unit$sole_prop) + pmax(0, -tax_unit$farm) +
      pmin(3000, pmax(0, -(tax_unit$kg_lt + tax_unit$kg_st))) +
      pmax(0, tax_unit$rent_loss) + pmax(0, tax_unit$part_active_loss) +
      pmax(0, tax_unit$part_passive_loss) +
      pmax(0, lictr_opt_col('scorp_active_loss')) +
      pmax(0, lictr_opt_col('scorp_passive_loss'))) -
    (tax_unit$filing_status == 2) *
      if_else(cb_owner, tax_unit$st_credits.cb_income_offset_joint_owner,
                        tax_unit$st_credits.cb_income_offset_joint_renter)
  cb_income = pmax(0, cb_income)
  cb_older_age = pmax(tax_unit$age1, coalesce(tax_unit$age2, 0) *
                                     (tax_unit$filing_status == 2))
  cb_senior   = cb_older_age >= tax_unit$st_credits.cb_senior_min_age
  cb_disabled = tax_unit$st_credits.cb_elig_disabled == 1 &
                (coalesce(tax_unit$disabled1, FALSE) |
                 (tax_unit$filing_status == 2 & coalesce(tax_unit$disabled2, FALSE)))
  cb_elig = (tax_unit$dep_status != 1) & cb_countable > 0 &
            (cb_older_age >= tax_unit$st_credits.cb_elig_age | cb_disabled)
  cb_income_r = floor(cb_income + 0.5)

  # Style 1 (MI): exempt share of resources (ladder for seniors/disabled,
  # flat otherwise), share of the excess (Table A ladder for seniors, 100%
  # disabled, flat otherwise), capped, then Table B steps; senior renters
  # take the larger of that and rent less 40% of resources, capped
  cb_exempt_rate = if_else(
    cb_senior | cb_disabled,
    lookup_state_credit_table(cb_income_r, rep(0L, n), credit_tables, 'cb_exempt_rate'),
    tax_unit$st_credits.cb_exempt_share)
  cb_share = case_when(
    cb_disabled ~ tax_unit$st_credits.cb_disabled_share,
    cb_senior   ~ lookup_state_credit_table(cb_income_r, rep(0L, n), credit_tables,
                                            'cb_senior_share'),
    TRUE        ~ tax_unit$st_credits.cb_share)
  cb_excess = pmax(0, cb_countable - cb_exempt_rate * cb_income)
  cb_po_factor = pmax(0, 1 - st_step_reduction(
    cb_income, tax_unit$st_credits.cb_po_thresh,
    tax_unit$st_credits.cb_po_step, tax_unit$st_credits.cb_po_rate))
  cb_credit_1 = pmin(cb_excess * cb_share, tax_unit$st_credits.cb_credit_max) *
                cb_po_factor
  cb_alt = cb_senior * tax_unit$st_credits.cb_senior_rent_alt *
    pmin(pmax(0, tax_unit$rent_paid -
                 tax_unit$st_credits.cb_senior_rent_alt_share * cb_income),
         tax_unit$st_credits.cb_credit_max)
  cb_credit_1 = pmax(cb_credit_1, cb_alt)

  # Style 2 (MO): countable tax less (steps x 1/16%) of income, zero above
  # the maximum upper limit for the tenure
  cb_pct = pmin(tax_unit$st_credits.cb_po_pct_max,
                st_step_reduction(cb_income, tax_unit$st_credits.cb_po_thresh,
                                  tax_unit$st_credits.cb_po_step,
                                  tax_unit$st_credits.cb_po_rate))
  cb_limit = if_else(cb_owner, tax_unit$st_credits.cb_income_limit_owner,
                               tax_unit$st_credits.cb_income_limit_renter)
  cb_credit_2 = pmax(0, cb_countable - cb_pct * cb_income) * (cb_income <= cb_limit)

  st_cb_credit = cb_elig * case_when(
    tax_unit$st_credits.cb_style == 1 ~ cb_credit_1,
    tax_unit$st_credits.cb_style == 2 ~ cb_credit_2,
    TRUE ~ 0)
```

`st_step_reduction()` (`src/calc/state/st_utils.R:228-244`) rounds a partial
step UP, which is both forms' rule ("or part of $1,000"; "per $300" with the
chart's row bounds). Add `st_cb_credit = st_cb_credit` to the return list
(`:372-383`) and the 29 names to `st_credits_household_req_vars` (`:8-64`).

Wiring in `src/calc/state/st_credits.R`:

- req_vars (`:138-140` block): `'rent_paid'`, `'property_tax_paid'`
  (`# (dbl) optional data; schema default 0`), `'rent_loss'`,
  `'part_active_loss'`, `'part_passive_loss'`, `'sole_prop'`, `'farm'` if not
  already present (the LICTR block reads them, so they are).
- `return_vars$calc_st_credits` (`:6-18`): add `'st_cb_credit'` -- the harness
  raw CSV then carries it as a column, like `st_lictr`.
- Refundable sum (`:579-589`): `+ hh$st_cb_credit * tax_unit$st_credits.cb_refundable`;
  nonrefundable sum (`:550`): `+ hh$st_cb_credit * (1 - tax_unit$st_credits.cb_refundable)`.
- Ordering (`cb_offsets_nonref`, MO): Form MO-WFTC line 9 limits the Working
  Family Tax Credit to "tax remaining after the property tax credit and
  miscellaneous credits" (`mo/credits.yaml:22,26`). With the circuit breaker
  refundable and the WFTC nonrefundable, the calculator's order (nonrefundable
  first) overstates the WFTC by up to the circuit breaker for a unit whose
  tax is smaller than the two credits combined. Implement as
  `st_eitc_nonref_cap = if_else(cb_offsets_nonref == 1, pmax(0, st_tax_pre_credit - hh$st_cb_credit), Inf)`
  applied to the `st_eitc * (1 - earn$st_eitc_ref_share)` term at `:564`,
  the same shape as `st_percap_nonref` at `:570-577`. MO sets 1; MI's EITC is
  refundable so it does not arise. **Review item for JI** with the rest.

Michigan `mi/credits.yaml` additions (reference strings abbreviated here;
the YAML carries the full cites of §1.1):

```yaml
cb_style: {value: 1, reference: 'MI-1040CR homestead property tax credit, MCL 206.520/206.522; Section A/B/C lines 34-44 (2024 form); reads rent_paid / property_tax_paid / hh_resources_inc (S43, S46(a)); inert at the schema defaults'}
cb_refundable: {value: 1, reference: 'MCL 206.520(3); MI-1040 line 27b'}
cb_income_base: {value: 1, reference: 'Total household resources (MCL 206.508(4); MI-1040CR lines 14-33): all income taxable or not, less Schedule 1 adjustments (line 30) -- federal AGI is the observable core'}
cb_add_nontax_ss: {value: 1, reference: 'MI-1040CR line 20: Social Security, SSI, railroad retirement, gross'}
cb_add_exempt_int: {value: 1, reference: 'line 16: interest and dividends "including nontaxable"'}
cb_add_hh_resources: {value: 1, reference: 'lines 22-27: child support, FIP/MDHHS, veterans, workers compensation, gifts; the hh_resources_inc optional data column (S43). Health insurance premiums (line 31) are unobserved [overstates resources; the health_premiums overlay is outside S46(a)]'}
cb_unnet_losses: {value: 1, reference: 'lines 18-19 and instructions: net business, farm, rental losses may not reduce total household resources; capital losses limited as federally'}
cb_rent_share:
  value: {'2017': 0.20, '2018': 0.23}
  reference: 'MCL 206.520(2): 20% of gross rent before TY2018, 23% after TY2017; MI-1040CR line 12'
cb_exempt_share:
  value: {'2017': 0.035, '2018': 0.032}
  reference: 'MCL 206.522(1)(a); MI-1040CR line 34 "Multiply line 33 by 3.2%" (2018+; 3.5% on the 2017 form)'
cb_share: {value: 0.60, reference: 'MCL 206.522(1)(a); Section C line 41 "Multiply line 40 by 60%"'}
cb_senior_min_age: {value: 65, reference: 'MI-1040CR box 5a "Age 65 or older; or an unremarried spouse of a person who was 65 or older at the time of death" (surviving-spouse prong unobserved); older spouse; Table A (credit_tables cb_senior_share, MCL 206.522(1)(b)) and the senior/disabled exemption ladder (cb_exempt_rate, 206.522(1)(c))'}
cb_senior_rent_alt: {value: 1, reference: 'MCL 206.520(9)-(10); MI-1040CR Worksheet 5 "Alternate property tax credit for renters age 65 and older": larger of line 44 and rent less 40% of resources, maximum the line 44 cap'}
cb_senior_rent_alt_share: {value: 0.40, reference: 'MCL 206.520(9) "more than 40% of the total household resources"; Worksheet 5 line C'}
cb_credit_max:
  value: {'2017': 1200, '2018': 1500, '2022': 1600, '2023': 1700, '2024': 1800, '2025': 1900}
  reference: 'MCL 206.520(15): $1,200 before TY2018, $1,500 from TY2018, CPI-indexed from TY2021 rounded to $100; printed maxima on MI-1040CR lines 38/39/41 each year (2021 stays $1,500). 2026+ carries 2025 [S45]'
cb_po_thresh:
  value: {'2017': 41000, '2018': 51000, '2021': 51600, '2022': 54000, '2023': 58300, '2024': 60700, '2025': 62500}
  reference: 'MCL 206.520(8); MI-1040CR Table B first band each year (2019 instructions p.33 "$51,000 or less"; 2024 p.33 "$60,700 or less"); indexed from 2021 rounded to $100. 2026+ carries 2025 [S45]'
cb_po_step: {value: 1000, reference: 'MCL 206.520(8) "each increment of $1,000.00"'}
cb_po_rate: {value: 0.10, reference: 'MCL 206.520(8): reduced 10% at the threshold and 10% per further $1,000 or part; Table B'}
cb_elig_age: {value: 0, reference: 'No age test for the base credit (MCL 206.520(1)); seniors differ only in Table A and the exemption ladder'}
documented_not_modeled:
  homestead_taxable_value_limit: {reference: 'MCL 206.520(1): owners with taxable value over $135,000 ($165,400 in 2025, indexed) are ineligible; taxable value unobserved [overstates the credit for high-value homes]'}
  fip_mdhhs_proration: {reference: 'MCL 206.520(7), Worksheet 4: credit prorated by non-FIP resources; FIP is pooled inside hh_resources_inc'}
  special_housing_and_service_fee: {reference: '206.520(2) 10% of rent for service-fee housing; Part 5 alternate housing facilities; unobserved'}
  disabled_and_veteran_claimants: {reference: '206.522(1)(c)-(e): 100% share for deaf/blind/disabled, the veteran/serviceperson taxable-value formula, the blind 100%/$3,500 formula; cb_elig_disabled is off pending the S46(a) review'}
```

Missouri `mo/credits.yaml` additions (replacing `documented_not_modeled.property_tax_credit` at `:29-30`):

```yaml
cb_style: {value: 2, reference: 'MO-PTS / MO-PTC property tax credit, RSMo 135.010-135.030; MO-PTS lines 1-14 (2024 form); reads rent_paid / property_tax_paid / hh_resources_inc (S43, S46(a)); inert at the schema defaults'}
cb_refundable: {value: 1, reference: 'MO-1040 line 43 (refundable block); RSMo 135.020'}
cb_income_base: {value: 2, reference: 'RSMo 135.010(5): Missouri adjusted gross income (MO-1040 line 6), BEFORE the Part 3 pension/SS exemption on line 8; MO-PTS line 1'}
cb_add_nontax_ss: {value: 1, reference: 'RSMo 135.010(5)(a); MO-PTS line 2 nontaxable Social Security and Tier I railroad'}
cb_add_exempt_int: {value: 1, reference: '135.010(5)(e); MO-PTS line 3 tax-exempt interest (the model-wide 15% U.S.-interest subtraction at MO-A Part 1 line 8 is not added back [understates income slightly])'}
cb_add_hh_resources: {value: 1, reference: '135.010(5)(a),(c); MO-PTS lines 5-6: veterans benefits, public assistance, SSI, child support, TANF; the hh_resources_inc optional data column (S43)'}
cb_unnet_losses: {value: 1, reference: '135.010(5)(d); MO-PTS line 7 nonbusiness losses added back as a positive amount (capital loss)'}
cb_income_offset_joint_owner: {value: {'2017': 4000, '2026': 5800}, reference: '135.010(5): $4,000 spouse exemption where the homestead was owned and occupied all year ($5,800 from TY2026, HB 594 & 508 of 2025); MO-PTS line 9'}
cb_income_offset_joint_renter: {value: {'2017': 2000, '2026': 2800}, reference: '135.010(5): $2,000 ($2,800 from TY2026); MO-PTS line 9 "rented or did not own your home for the entire year"'}
cb_rent_share: {value: 0.20, reference: 'Form MO-CRP line 9 "rent constituting property taxes" = 20% of gross rent; 2025 MO-1040 instructions p.18 example ($4,000 x 20 percent)'}
cb_rent_cap: {value: {'2017': 750, '2026': 1055}, reference: 'RSMo 135.025; MO-PTS line 12 "or $750, whichever is less"; $1,055 from TY2026'}
cb_prop_cap: {value: {'2017': 1100, '2026': 1550}, reference: 'RSMo 135.025; MO-PTS line 11 "or $1,100, whichever is less"; $1,550 from TY2026'}
cb_countable_cap: {value: {'2017': 1100, '2026': 1550}, reference: 'MO-PTS line 13 "total of Lines 11 and 12, or $1,100, whichever is less"'}
cb_po_thresh: {value: 14300, reference: 'RSMo 135.030.1(2) minimum base $14,300 (2008+); 2021 chart first row "1 - 14,300: refund is the actual total"'}
cb_po_step: {value: 300, reference: 'RSMo 135.030.2(1) "per $300"'}
cb_po_rate: {value: 0.000625, reference: 'RSMo 135.030.2(1) "1/16 percent accumulative per $300": 1/16 of one percent of income per step'}
cb_po_pct_max: {value: 0.04, reference: 'RSMo 135.030.2(1) "from 0 percent to 4 percent"'}
cb_elig_age: {value: 65, reference: 'RSMo 135.010(1): claimant or spouse 65 on or before December 31; MO-PTS qualification A. The 100% disabled veteran, disabled and 60+ surviving-spouse prongs (B-D) are unobserved [understates the credit for those claimants]; cb_elig_disabled reserved for the disabled1/2 flag'}
# filing_status_mapper: cb_income_limit_owner -> cb_limit_owner_single (1,3,4) / cb_limit_owner_combined (2); same for renter
cb_limit_owner_single: {value: {'2017': 30000, '2026': 42200}, reference: 'RSMo 135.030.1(1) maximum upper limit, homestead owned and occupied all year: $30,000 through TY2025 for every status; TY2026 $42,200 for a single filing status (HB 594 & 508 of 2025), CPI-indexed from 2027'}
cb_limit_owner_combined: {value: {'2017': 30000, '2026': 48000}, reference: 'RSMo 135.030.1(1)(d): $48,000 married filing combined, owned and occupied, from TY2026'}
cb_limit_renter_single: {value: {'2017': 27500, '2026': 38200}, reference: 'RSMo 135.030.1(1): $27,500 through TY2025 (the form prints $27,200 because the $750 renter credit reaches zero there); TY2026 $38,200 single'}
cb_limit_renter_combined: {value: {'2017': 27500, '2026': 41000}, reference: 'RSMo 135.030.1(1)(c): $41,000 married filing combined from TY2026'}
cb_offsets_nonref: {value: 1, reference: 'Form MO-WFTC line 9: the Working Family Tax Credit is limited to tax remaining after the property tax credit (MO-1040 line 36 less lines 42-43)'}
```

The TY2026 values are enacted law in the RSMo text fetched 2026-10-13 (HB
594 & 508, effective 2025-08-28) but the MO packet does not yet cite them, so
under S45 the parent decides whether to encode the 2026 keys or carry 2025
forward and record them; the note recommends encoding with the statute cite
and updating `state_parameter_rollout.csv:27` `ty2026_law`.

Inertness: `rent_paid` and `property_tax_paid` default 0 give
`cb_countable = 0`, so `cb_elig` is FALSE and `st_cb_credit = 0` for every
style; `hh_resources_inc` default 0 leaves the income concept at today's
observable proxy. The invariance test
(`test_supplemental_inputs_reader.R:200-268`) loops the schema, so it covers
both variables once MI or MO is in its `states` vector (`:212`, currently
`IL MN MT`); add MI there.

Split: `rent_paid` and `property_tax_paid` are `split: halve` in the schema
(`:76,84`), so a married-separate half-unit sees half of each; neither MI nor
MO uses `split_election`, and Missouri's combined-return rule
(135.010(1)) makes a separate claim rare. The 135.010(5) offset is applied
only to `filing_status == 2`.

### 1.4 Worksheet cases -- Michigan (TY2024; exemption $5,600; rate 4.25%)

- **MI-13a (inert):** MI-1 (`test_state_calc.R:634`) unchanged -- no
  overlay, `st_cb_credit 0`.
- **MI-13b (general claimant, owner):** single 45, `wages1 40000`,
  `agi 40000`, `property_tax_paid 3000`. Resources 40,000; exemption 3.2% x
  40,000 = 1,280; excess 1,720; x 60% = 1,032 (cap 1,800 not binding); Table
  B 100% (40,000 <= 60,700). Tax (40,000 - 5,600) x 4.25% = 1,462.00 ->
  **`st_cb_credit 1032`, `st_credits_ref 1032`, `liab_st_iit 430.00`**.
- **MI-13c (senior owner, Table A):** single 70, `wages1 26000`, `agi
  26000`, `property_tax_paid 2500`. Resources 26,000 -> Table A 80%; senior
  exemption ladder 3.2% x 26,000 = 832; excess 1,668 x 0.80 = 1,334.40; Table
  B 100% -> credit 1,334.40. Liability: age 70 in 2024 is Tier 3 at 67+
  (`mi/agi.yaml:195-221`): Michigan Standard Deduction 20,000 netted by
  taxable SS (0) and the exemption 5,600 = 14,400 (no-net age 72 not
  reached); st_agi 11,600; taxable 6,000; tax 255.00 ->
  **`st_cb_credit 1334.40`, `liab_st_iit -1079.40`**.
- **MI-13d (senior renter, alternate credit):** single 68, `agi 0`,
  `gross_ss 12000`, `txbl_ss 0`, `rent_paid 9000`. Resources = 0 + 12,000
  nontaxable SS = 12,000; countable 23% x 9,000 = 2,070; ladder 3.2% x
  12,000 = 384; excess 1,686; Table A 100% -> 1,686. Worksheet 5: rent 9,000
  - 40% x 12,000 = 4,200, capped 1,800 > 1,686 -> **`st_cb_credit 1800`,
  `liab_st_iit -1800.00`** (tax 0).
- **MI-13e (Table B phase-out):** single 50, `agi 63000`, `wages1 63000`,
  `property_tax_paid 4000`. Exemption 2,016; excess 1,984 x 60% = 1,190.40;
  steps ceil((63,000 - 60,700) / 1,000) = 3 -> 70% -> 833.28. Tax (63,000 -
  5,600) x 4.25% = 2,439.50 -> **`liab_st_iit 1606.22`**.
- **MI-13f (over the ceiling):** as 13e with `agi 70000` -> 10 steps -> 0%
  -> **`st_cb_credit 0`**.

### 1.5 Worksheet cases -- Missouri (TY2024; federal standard deduction via `std_ded`)

- **MO-16a (inert):** MO-1 unchanged, `st_cb_credit 0`.
- **MO-16b (single owner 70):** `agi 15000`, `wages1 15000`, `ei1 15000`,
  `age1 70`, `std_ded 16550`, `liab_bc 0`, `property_tax_paid 1000`. Income
  15,000 (no offset: single); countable min(1,000, 1,100) = 1,000; steps
  ceil((15,000 - 14,300) / 300) = 3 -> 3/16% = 0.1875% x 15,000 = 28.125 ->
  credit 971.875; income <= 30,000. Tax 0 (std > AGI) ->
  **`st_cb_credit 971.88`, `liab_st_iit -971.88`** (tol 0.01). Chart check:
  2021 chart row $14,901-$15,200, column $976-$1,000 prints $959 (midpoints
  $988 and $15,050 give 959.8); the exact rule at the record's own values is
  971.875.
- **MO-16c (married combined renter, over the limit):** `filing_status 2`,
  ages 66/64, `agi 24000`, `gross_ss 6000`, `txbl_ss 0`, `rent_paid 6000`,
  `std_ded 30750`. Income 24,000 + 6,000 - 2,000 = 28,000 > 27,500 ->
  **`st_cb_credit 0`**.
- **MO-16d (married combined renter, inside):** as 16c with `agi 20000`.
  Income 20,000 + 6,000 - 2,000 = 24,000; countable min(20% x 6,000, 750) =
  750; steps ceil(9,700 / 300) = 33 -> 2.0625% x 24,000 = 495 -> credit
  255.00; tax 0 -> **`st_cb_credit 255`, `liab_st_iit -255.00`** (chart
  cell $23,901-$24,200 / $751-$775 prints $266 at the midpoints).
- **MO-16e (under 65):** as 16b with `age1 60`, `std_ded 14600` ->
  **`st_cb_credit 0`** (no disabled flag; `cb_elig_disabled 0`).

### 1.6 What the external models do

- **PolicyEngine Michigan** (`policyengine_us/variables/gov/states/mi/tax/income/credits/homestead_property_tax/`):
  `mi_homestead_property_tax_credit` = max(pre-alternate senior amount,
  alternate senior amount) where the allowable credit uses
  `real_estate_taxes` + 0.23 x `rent`, the senior Table A ladder
  (`rate/senior/base.yaml`), the 0.6 general rate, the senior/disabled
  exemption ladder, the Table B phase-out and the cap -- the same mechanics
  as §1.3. Eligibility requires `assessed_property_value <= limit` (0 <=
  limit is TRUE) and `mi_household_resources <= limit`. **In the harness the
  driver passes `real_estate_taxes = salt_prop` and no rent, so PE pays the
  homestead credit on every MI federal itemizer with resources under the
  ceiling, and it is netted into `state_income_tax` through
  `credits/refundable.yaml` (`mi_eitc, mi_home_heating_credit,
  mi_homestead_property_tax_credit`).** We pay zero (the component is inert).
  PE's household resources concept is the full MI list
  (`household_resources.yaml`) less `health_insurance_premiums` and
  `above_the_line_deductions`.
- **PolicyEngine Michigan home heating credit**
  (`credits/home_heating/standard/`): the standard credit = base amount by
  exemption count ($581 for one in 2024) less 3.5% of resources, x the
  credit percentage (1.0 / 0.9 / 0.56 / 0.52 for 2021-2024), for every unit
  with resources <= 110% of the poverty guideline, no heating-cost input
  needed. **PE pays it to every low-resource MI record in the harness** and
  nets it into liability. S46(b) keeps it out of ours. This is the
  PolicyEngine twin of the TAXSIM row `known_differences.csv:34`.
- **PolicyEngine Missouri** (`variables/gov/states/mo/tax/income/credits/property_tax/mo_property_tax_credit.py`):
  min(0.2 x rent, 750) + min(real_estate_taxes, 1,100), capped 1,100, less
  0.000625 x ceil((net income - 14,300) / 300) x net income -- the statute.
  Eligible where head or spouse is 65+, disabled, military-disabled or an
  aged survivor (`mo_ptc_taxunit_eligible.py`; only the ages reach the
  harness). Net income = MO AGI + exempt pension/interest + nontaxable SS +
  veterans + public assistance + capital loss, less the joint offset. In
  `credits/refundable.yaml`. **PE pays it on `salt_prop` for 65+ MO
  itemizers with net income under the limits.** PE has no 2026 values
  beyond the caps (`property_tax_limit.yaml` 1,550 / `rent_property_tax_limit.yaml`
  1,055 from 2026-01-01) -- its upper limits stay at 27,500/30,000 and its
  offsets at 2,000/4,000, which is wrong for TY2026 (file as a P-series
  item).
- **TAXSIM** receives `proptax = salt_prop` and `rentpaid = 0`. The MI
  triage already found TAXSIM's MI homestead credit in `v37` (row 34's
  predicate subtracts `v37_state_property_tax_credit` from total credits),
  and TAXSIM nets property-tax credits into `siitax` in DC and RI
  (`known_differences.csv:68,92`). Whether TAXSIM models the MO-PTC is not
  established; the row in §1.7 is self-gating on `v37 > 0`.

### 1.7 Known-difference rows (circuit breaker and home heating)

Columns: `state,model,year_min,year_max,category,description,expected_direction,expected_magnitude,action,predicate,source`.
Direction words follow the file's PE rows (`over` = the external model pays
a credit we do not, so its liability is lower) and TAXSIM rows (`under` =
TAXSIM's liability is lower); each description says it in words.

1. **MI, policyengine, 2021-2025, transfer-netting, exclude.** "PE nets the
   MI-1040CR homestead property tax credit (mi_homestead_property_tax_credit,
   in mi_refundable_credits) into state_income_tax, computed on
   real_estate_taxes = salt_prop with no rent and a zero assessed value that
   passes the taxable-value gate; our generic circuit breaker (S46(a)) reads
   the rent_paid / property_tax_paid overlays, which no producer publishes in
   harness runs, so we pay zero. One-sided and limited to federal itemizers
   with household resources under the Table B ceiling." Direction `over`
   (PE lower). Magnitude "up to the MI-1040CR cap ($1,500 2021, $1,900
   2025)". Predicate `pe_mi_homestead > 0`. Source "this note §1.6;
   mirrors the WI row (known_differences.csv:50)". Requires the export
   `"pe_mi_homestead": ["mi_homestead_property_tax_credit"]` in
   `pe_state_tax.py` OUTPUT_VARS (`:101-166`) and the column in the record
   select at `test_state_cross_model.R:1002-1010`.
2. **MI, policyengine, 2021-2025, transfer-netting, exclude.** "PE nets the
   MI-1040CR-7 home heating credit (mi_home_heating_credit; standard credit =
   exemption-count base less 3.5% of household resources, x 0.52-1.0, for
   resources under 110% of the poverty guideline, no heating-cost input)
   into state_income_tax. The home heating credit is an energy-assistance
   transfer claimed outside the MI-1040 and deliberately outside our IIT
   concept (S46(b)); PE twin of the TAXSIM row at line 34." Direction `over`.
   Magnitude "up to $497 (2021) / $302 (2024, one exemption) per unit".
   Predicate `pe_mi_home_heating > 0` (export
   `"pe_mi_home_heating": ["mi_home_heating_credit"]`). Source "this note
   §1.6; decision S46(b)". Why defensible: the credit is outside our concept
   by decision, and PE pays it on an input-free rule, so excluding it is
   the only way the MI PE cells (0.62-0.78 clean,
   `research/state_tax/cross_model/results/summary.csv`) can measure the
   income tax.
3. **MI, taxsim, 2017-2020, transfer-netting, exclude.** "TAXSIM nets its
   MI-1040CR homestead credit (v37_state_property_tax_credit) into siitax,
   computed on proptax = salt_prop and rentpaid = 0 (src/tests/test_taxsim.R:127,260);
   our circuit breaker is inert in the harness (S46(a))." Direction `under`
   (TAXSIM lower). Magnitude "up to $1,200 (2017) / $1,500 (2018-2020)".
   Predicate `v37_state_property_tax_credit > 0`. Source "this note §1.6;
   MI raw diagnosis 2026-08-11 (row 34 already nets v37 out of total
   credits)". Row 34 stays: its predicate isolates the home heating
   point mass by subtracting v37, so the two rows partition the TAXSIM
   credit column.
4. **MO, taxsim, 2017-2020, transfer-netting, exclude (pre-registered).**
   "TAXSIM may pay the MO-PTS property tax credit (v37) to 65+ records on
   proptax = salt_prop; ours is inert in the harness. Self-gating: fires
   only where TAXSIM reports a property tax credit." Direction `under`.
   Magnitude "up to $1,100". Predicate `v37_state_property_tax_credit > 0`.
   Source "this note §1.6; verify on the next MO rerun that v37 is nonzero
   before counting it as a finding".
5. **MO, policyengine, 2021-2025, transfer-netting, exclude.** "PE nets
   mo_property_tax_credit (RSMo 135.030 formula on real_estate_taxes =
   salt_prop, age-65+ head or spouse, net income from MO AGI plus
   nontaxable SS) into state_income_tax; ours is inert in the harness
   (S46(a))." Direction `over`. Magnitude "up to $1,100". Predicate
   `pe_mo_property_tax_credit > 0` (export
   `"pe_mo_property_tax_credit": ["mo_property_tax_credit"]`). Source "this
   note §1.6; mirrors the RI PE row (known_differences.csv:135)".

Why these are `exclude` rather than `annotate`: the input the credit
depends on (rent, and property tax for non-itemizers) does not exist in our
data, the same class as DC/RI/WI (rows 68, 92, 135, 50), and the external
model's own credit column keys the record exactly. When a producer is
pinned in a harness run the rows must be revisited (interface note §7 step 5
assumes no producer).

## 2. Michigan: Tier-1 public pension and the blind exemption

### 2.1 Tier-1 (born before 1946) unlimited public pension subtraction

**Law.** MCL 206.30(9)(a): "For a person born before 1946, this subsection
provides no additional restrictions or limitations under subsection (1)(f)"
-- so Michigan and federal public retirement benefits ((1)(f)(i)) are
subtracted in full and only private benefits ((1)(f)(iv)) take the indexed
cap, which is first reduced by military/railroad amounts and then by the
public benefits subtracted (Form 4884 Section A, 2025 lines 9-16: "If the
public retirement benefits are greater than the maximum amount, the
recipient ... [is] not entitled to claim an additional subtraction for
private retirement benefits"; `research/raw/mi_retirement_research.md:123-129`).
Tier 2 (born 1946-1952) sums public and private under the $20,000/$40,000
limit (206.30(9)(b)) and the PA 4 of 2023 phase-in caps public and private
combined at the Tier-1 maximum (206.30(10)(d)), so the unlimited-public rule
is Tier 1 only. Today `mi/agi.yaml:53-58` treats every pension as private
and capped; crosswalk row `tax_data_gaps.csv:290` (materiality high,
direction overtax).

**Overlay.** `pension_public` (`schema:90-97`, exists; split halve).

**Law parameter** (law schema, after `st_agi.pension_excl_band_per_return`
at `params_schema.yaml:78`):

```yaml
  # Age (older spouse where band_per_return = 1, else own age) at or above
  # which government pension income is subtracted without limit and only the
  # private remainder takes the cap (MI Tier 1, MCL 206.30(9)(a); Form 4884
  # Section A). Reads pension_public (S43); .inf = off
  st_agi.pension_excl_public_uncapped_min_age: .inf
```

**Stage change** in `st_agi.R`, after the band block closes (after `:315`,
before the own-base starting point comment):

```r
  # Unlimited public pension at or above the Tier-1 age (MI MCL 206.30(9)(a)):
  # public benefits come out in full and the private remainder takes the cap,
  # i.e. the effective cap is max(cap, public). Older-spouse age where the
  # bands are per return; own age otherwise. Reads pension_public (S43)
  public_pool = pmin(pmax(0, tax_unit$pension_public), pmax(0, tax_unit$txbl_pens_dist))
  tier1_age   = if_else(tax_unit$st_agi.pension_excl_band_per_return == 1,
                        pmax(tax_unit$age1,
                             if_else(tax_unit$filing_status == 2 & !is.na(tax_unit$age2),
                                     tax_unit$age2, tax_unit$age1)),
                        tax_unit$age1)
  cap1_v = if_else(tier1_age >= tax_unit$st_agi.pension_excl_public_uncapped_min_age,
                   pmax(cap1_v, public_pool), cap1_v)
```

Algebra: with `pens_cap = cap1 + cap2` (`st_agi.R:582-590`, MI takes the
`pension_cap_incl_ss == 0` branch) and `st_sub_pens_raw = min(pens_inc,
pens_cap)` (`:595`), the subtraction is `min(pens_inc, max(cap, public))`
= `public + min(private, max(0, cap - public))`, which is Section A lines
12-16. The KY grandfather term (`st_sub_pens_grand`, `:574-579`) is zero in
Michigan (`pension_excl_public_grandfather` 0) and the MI change is zero in
Kentucky (`.inf` age), so the two do not interact. req_vars:
`'st_agi.pension_excl_public_uncapped_min_age'` after
`'st_agi.pension_excl_band_per_return'` (`:150`); `pension_public` is
already listed (`:49`).

`mi/agi.yaml` addition (year-keyed, the same "born before 1946" age series
as `senior_inv_min_age` at `mi/agi.yaml:128-149`):

```yaml
pension_excl_public_uncapped_min_age:
  value: {'2017': 72, '2018': 73, '2019': 74, '2020': 75, '2021': 76, '2022': 77, '2023': 78, '2024': 79, '2025': 80, '2026': 81, '2027': 82, '2028': 83, '2029': 84, '2030': 85, '2031': 86, '2032': 87, '2033': 88, '2034': 89, '2035': 90}
  reference: 'MCL 206.30(9)(a) (born before 1946: no limitation on the subsection (1)(f)(i) public benefits); Form 4884 Section A lines 9-16 (2025): private benefits limited to the cap less public benefits claimed. Older-spouse age = year - 1945. Reads pension_public (S43); inert at its default 0. Military/railroad reductions to the cap (line 10) stay unobserved; other states'' public pensions are private (206.30(1)(f)(ii)) and would need a producer to publish only MI/federal amounts in pension_public'
```

and rewrite the `mi/agi.yaml:53-58` comment so "public pensions (unlimited
for Tier 1) treated as private/capped -- source unobservable" becomes the
S43 statement.

**Worksheet cases** (base MI-2, `test_state_calc.R:636-646`: TY2019 single
75, AGI 65,000, pension 60,000, `txbl_int` 5,000; cap 52,808; st_agi 65,000 -
52,808 - 750 = 11,442; liab (11,442 - 4,400) x 4.25% = 299.29):

- **MI-2 (inert, unchanged):** 299.29.
- **MI-11a (overlay live, all public):** `pension_public 60000` -> cap
  max(52,808, 60,000) = 60,000 -> `st_sub_pens 60000`, `st_agi 4250`
  (65,000 - 60,000 - 750), senior investment cap 11,771 - 60,000 < 0 -> 0;
  taxable 4,250 - 4,400 < 0 -> **`liab_st_iit 0`**.
- **MI-11b (overlay live, mixed):** `pension_public 30000` -> public 30,000
  + private min(30,000, 22,808) = 52,808 -> identical to MI-2 (**299.29**).
  Pins that the cap is a ceiling on public plus private, not on private alone.
- **MI-11c (overlay live, Tier 2 unaffected):** MI-3 (`:648-654`, 2019 single
  68, pension 30,000, cap 20,000) with `pension_public 30000` -> age 68 < 74
  -> **`st_agi 20000`** unchanged. Pins that Tier 2 gets no unlimited public.

**External models.** PolicyEngine's tier-one amount
(`mi_retirement_benefits_deduction_tier_one_amount.py`) is exactly Section
A: public in full plus private up to the cap less public, reading
`taxable_public_pension_income` and `taxable_private_pension_income`; the
driver passes only `taxable_pension_income` (`pe_state_tax.py:67`), so in
the harness PE's public amount is 0 and it caps the whole pool -- our inert
encoding. TAXSIM takes one pooled `pensions` input and its Tier-1 handling
is the external-model-bug row `known_differences.csv:31` (TAXSIM applies the
Tier-2 $20,000 to all 67+). **No new row**; crosswalk row 290 moves to
`encoded_overlay`.

### 2.2 Blind special exemption (MI-1040 line 9b) -- law fix, data present

**Law.** MCL 206.30(3)(a) and MI-1040 line 9b: one special exemption "per
person as it applies to you, your spouse and your dependents" who are
"deaf, blind, hemiplegic, paraplegic, quadriplegic, or totally and
permanently disabled" (2024 instructions p.10). Amounts, read from each
year's MI-1040 line 9b on the NBER mirror: $2,600 (2017), $2,700 (2018,
2019), $2,800 (2020, 2021), $2,900 (2022), $3,100 (2023), $3,300 (2024),
$3,400 (2025). Michigan has no aged special exemption in this window (the
age-65 exemption ended with 2011 PA 38), so `aged_addl` stays 0.

**Today** `mi/exempt.yaml:27-29` sets `blind_addl: 0` with the reference
"require ... status inputs not in the baseline PUF" -- but `blind1/2` are
Tax-Data columns, already in `calc_st_exempt` req_vars (`st_exempt.R:44-45`)
and counted at `st_exempt.R:130` (`n_blind`) into `st_exempt_gross` at
`:155-160`. Crosswalk row `tax_data_gaps.csv:287` (present_unused).

**Fix.** `mi/exempt.yaml`:

```yaml
blind_addl:
  value: {'2017': 2600, '2018': 2700, '2020': 2800, '2022': 2900, '2023': 3100, '2024': 3300, '2025': 3400}
  reference: 'MI-1040 line 9b special exemption amount, read from each year''s form (NBER mirror MI/2017-2025): $2,600 / $2,700 / $2,700 / $2,800 / $2,800 / $2,900 / $3,100 / $3,300 / $3,400; MCL 206.30(3)(a) ($1,800 in statute, indexed by Treasury). Blind filer and spouse only: blind1/blind2 are Tax-Data columns; deaf/disabled filers and blind dependents are unobserved [understates the exemption for them; the disabled1/2 overlay could add a st_exempt.disabled_addl later]. 2026+ carries 2025 [S45]'
aged_addl:
  value: 0
  reference: 'Michigan has no age-based special exemption 2017-2025 (repealed by 2011 PA 38); line 9b covers deaf/blind/disabled only'
```

**Worksheet case** (base MI-1, `:629-634`: TY2025 single AGI 50,000, EITC
1,000; exemption 5,800; liab (50,000 - 5,800) x 4.25% - 300 = 1,578.50):

- **MI-12 (blind filer):** add `blind1 TRUE` -> `st_exempt 9200` (5,800 +
  3,400) -> liab (50,000 - 9,200) x 4.25% - 300 = **1,434.00**.

### 2.3 Deaf/disabled filers and the disabled veteran exemption (not this batch)

Crosswalk rows 288 (`disabled1` overlay) and 283 (`veteran1`): a
`st_exempt.disabled_addl` / `st_exempt.veteran_addl` pair reading the
schema flags (`:262-290`) is the natural follow-up; PolicyEngine's
`mi_exemptions.py` does exactly that from `is_blind | is_deaf |
is_permanently_and_totally_disabled` (under 66) and
`is_fully_disabled_service_connected_veteran` ($400; $500 from 2024). Left
out of this batch so the exemption stage change stays a YAML edit.

### 2.4 Known-difference rows (MI pensions and exemptions)

1. **MI, both, 2017-2025, input-coverage, exclude.** "Blind special
   exemption (MI-1040 line 9b, $2,600-$3,400 per blind filer or spouse):
   encoded from the Tax-Data blind1/blind2 columns; TAXSIM has no blindness
   input and the PolicyEngine driver passes none (pe_state_tax.py input map),
   so both external models tax the amount." Direction: ours lower (`over`
   for PE, `under`-reversed for TAXSIM -- say "external liability higher").
   Magnitude "blind_addl x 4.25% = $110.50 (2017) to $144.50 (2025) per blind
   person, above the $100 tolerance". Predicate `blind_n > 0`, where
   `blind_n = coalesce(blind1, 0) + coalesce(blind2, 0)` is added to the
   record at `test_state_cross_model.R:974-983` (beside `n_dep_ge24`) and
   the select at `:1002-1010`. Source "this note §2.2". Why defensible: the
   input exists only on our side; a one-column predicate names the
   population exactly.
2. **Tier-1 public pension: no row** (§2.1). Row 31 (TAXSIM tier bug) stays.

## 3. Missouri: public pension exemption (MO-A Part 3 Section A)

### 3.1 Law

RSMo 143.124.1-.2 and MO-A Part 3 Section A, transcribed from each year's
form on the NBER mirror (`MO-A_2017.pdf` ... `MO-A_2024.pdf`, 2025 from the
2025 MO-1040 instructions p.2148 of the text):

- Per spouse, the exempt public pension is the lesser of taxable public
  pension income and the year's "maximum social security benefit": **$37,089
  (2017), $37,720 (2018), $38,437 (2019), $39,014 (2020), $39,365 (2021),
  $41,373 (2022), $44,683 (2023), $46,381 (2024), $47,633 (2025)** (Section A
  line 7 through 2023, line 2 from 2024). PolicyEngine's
  `mo_max_social_security_benefit.yaml` carries 48,216 for 2025, which the
  form contradicts -- file as a P-series item; it never binds in the
  harness (§3.3).
- Less the Social Security exemption taken for that spouse (line 8: "enter
  the amount(s) from Line(s) 6Y and 6S" of Section C, the taxable benefits
  eligible for exemption; "See instructions if Line 3 of Section C is more
  than $0" -- the reduced amount applies when the SS income limit binds).
- Through TY2023, the total is reduced dollar-for-dollar by Missouri AGI
  less taxable Social Security above $85,000 ($100,000 married combined)
  (lines 1-5, 11). SB 190 (2023) removed the limit: the 2024 and 2025
  Section A has no income lines (lines 1-5 of the 2024 form are the per-spouse
  computation only).
- Private pensions, annuities, IRAs and 401(k)s go to Section B ($6,000 per
  spouse, own limits) -- `mo/ded.yaml:150-161` today -- so a public pension
  never also takes the $6,000.

Today `mo/agi.yaml:60-62` and `mo/ded.yaml:163-165` document it as not
modeled; crosswalk row `tax_data_gaps.csv:335` (materiality high, overtax,
"largest MO gap"); packet `mo.md:151-159`.

### 3.2 Overlay, parameters, formula

**Overlay.** `pension_public` (exists). Per-spouse attribution is unobserved;
the existing private cap already applies two caps against the pooled
pension income (`mo/ded.yaml:167`), and the same convention carries here.

**Law parameters** (law schema, after `st_ded.retire_exempt_less_ss` at
`params_schema.yaml:354`):

```yaml
  # Public pension exemption taken as a DEDUCTION after state AGI (MO-A Part 3
  # Section A; RSMo 143.124.1-.2): per spouse the lesser of public pension
  # income and retire_exempt_pub_cap, less the Social Security exemption
  # taken, less income over retire_exempt_pub_limit. Reads pension_public
  # (S43); inert at retire_exempt_pub 0 and pension_public 0
  st_ded.retire_exempt_pub: 0
  st_ded.retire_exempt_pub_cap: {default: 0, units: per_person}
  st_ded.retire_exempt_pub_limit: {default: .inf, units: per_return}
  st_ded.retire_exempt_pub_less_ss: 0
```

**Stage change** `st_ded.R:633-657` (the retirement block; `n_taxpayers_ded`
and `retire_limit_income` are in scope):

```r
      # Public pension exemption (MO-A Part 3 Section A): the government
      # share of the pension pool, per-spouse capped at the year's maximum
      # Social Security benefit, net of the SS exemption taken and of income
      # over its own limit (removed by SB 190 from TY2024). Reads
      # pension_public (S43); the private exemption below then applies to
      # the remainder of the pool
      retire_pub_pool = pmin(pmax(0, pension_public), pmax(0, txbl_pens_dist)),
      st_retire_pub_exempt = st_ded.retire_exempt_pub * pmax(
        0,
        pmin(retire_pub_pool, n_taxpayers_ded * st_ded.retire_exempt_pub_cap) -
          st_ded.retire_exempt_pub_less_ss * st_retire_ss_exempt -
          pmax(0, retire_limit_income - st_ded.retire_exempt_pub_limit)
      ),
      st_retire_priv_exempt = pmax(
        0,
        pmin(pmax(0, txbl_pens_dist - retire_pub_pool + txbl_ira_dist),
             n_taxpayers_ded * st_ded.retire_exempt_priv_cap) -
          pmax(0, retire_limit_income - st_ded.retire_exempt_priv_limit)
      ),
      st_retire_exempt = st_retire_ss_exempt + st_retire_pub_exempt + st_retire_priv_exempt,
```

(`st_retire_ss_exempt` must be computed before this term, as it is at
`:645-647`.) Add `'pension_public'` (`# (dbl) optional data; schema default
0`) to the `calc_st_ded` req_vars beside `'txbl_pens_dist'`, and the four
law names after `'st_ded.retire_exempt_less_ss'` (`:196`).

`mo/ded.yaml` addition (replacing `documented_not_modeled.public_pension_exemption_placement`
at `:163-165`, and `mo/agi.yaml:60-62`):

```yaml
retire_exempt_pub:
  value: 1
  reference: 'MO-A Part 3 Section A; RSMo 143.124.1-.2. Public pension (federal, state, local) exempt per spouse up to the maximum Social Security benefit, net of the Section C exemption and (through TY2023) of income over the limit. Reads pension_public (S43); inert at its default 0, in which case the whole pool stays private (Section B). Per-spouse attribution unobserved: two caps against the pooled public amount, the Section B convention'
retire_exempt_pub_cap:
  value: {'2017': 37089, '2018': 37720, '2019': 38437, '2020': 39014, '2021': 39365, '2022': 41373, '2023': 44683, '2024': 46381, '2025': 47633}
  reference: 'MO-A Part 3 Section A line 7 (2017-2023) / line 2 (2024-2025) "maximum social security benefit", transcribed from each year''s form (NBER mirror MO-A_2017 .. MO-A_2024; 2025 MO-1040 instructions). 2026+ carries 2025 [S45]'
retire_limit_pub_single:
  value: {'2017': 85000, '2024': 999999999999.0}
  reference: 'Section A line 4, 2017-2023 ($85,000 single/HoH/MFS/QW); removed from TY2024 by SB 190 (2023) -- the 2024 Section A has no income lines'
retire_limit_pub_combined:
  value: {'2017': 100000, '2024': 999999999999.0}
  reference: 'Section A line 4, married filing combined $100,000, 2017-2023; removed TY2024'
retire_exempt_pub_less_ss:
  value: 1
  reference: 'Section A line 8 (line 3 from 2024): subtract the Section C Social Security exemption amount for the spouse; the model nets the unit''s computed SS exemption (post-limit where the SS limit binds, per the Section A instruction)'
```

with a `retire_exempt_pub_limit` entry in the `filing_status_mapper`
(`mo/ded.yaml:15-30`) mirroring `retire_exempt_ss_limit`.

### 3.3 Worksheet cases

- **MO-9 (inert, unchanged):** `:5801`, 257.56.
- **MO-15a (overlay live, TY2023, under the limit):** MO-8 (`:5788`: single
  70, wages 60,000, `txbl_ss 10000`, pension 20,000, AGI 90,000, `std_ded
  13850`, `liab_bc 10000`; limit income 80,000) with `pension_public 20000`.
  Section A: min(20,000, 44,683) = 20,000 - SS exemption 10,000 = 10,000;
  80,000 < 85,000 -> 10,000. Section B: private pool 0. `st_retire_exempt
  20000` (10,000 SS + 10,000 public). TI = 90,000 - 13,850 - 1,500 - 20,000
  = 54,650; tax 234 + 4.95% x 46,201 = **2,520.95**.
- **MO-15b (overlay live, TY2024, no limit):** MO-10b (`:5819`: single 70,
  AGI 120,000, `txbl_ss 15000`, `std_ded 16550`, `liab_bc 20000`) with
  `txbl_pens_dist 25000`, `pension_public 25000`. Section A: 25,000 - 15,000
  = 10,000, no limit -> `st_retire_exempt 25000`; TI = 120,000 - 16,550 -
  1,000 - 25,000 = 77,450; tax 248 + 4.8% x 68,539 = **3,537.87** (MO-10b
  4,017.87 less 10,000 x 4.8%).
- **MO-15c (overlay live, TY2023, limit binds):** MO-10a (`:5813`: AGI
  120,000, `txbl_ss 15000`, limit income 105,000, SS exemption 0) with
  `txbl_pens_dist 30000`, `pension_public 30000`. Section A: 30,000 - 0 -
  (105,000 - 85,000) = 10,000 -> `st_retire_exempt 10000`; TI = 120,000 -
  15,700 - 1,000 - 10,000 = 93,300; tax 234 + 4.95% x 84,851 = **4,434.12**.
- **MO-15d (overlay live, private remainder):** MO-9 with `pension_public
  12000`: Section A 12,000 (limit income 28,000 < 85,000); Section B private
  8,000 -> min(8,000, 6,000) - 3,000 = 3,000; `st_retire_exempt 15000`; TI
  = 28,000 - 15,700 - 375 - 15,000 < 0 -> **`liab_st_iit 0`**.

### 3.4 External models and known-difference rows

PolicyEngine's Section A (`mo_pension_and_ss_or_ssd_deduction_section_a.py`)
is the form, but it reads `taxable_public_pension_income`, which the driver
never passes, so PE's public exemption is 0 in the harness -- the inert
encoding; MO is not in `PE_IRA_SEPARATE_STATES`
(`test_state_cross_model.R:340`), so IRAs ride in the private pool there as
in Section B. TAXSIM takes one pooled pension input. **No new row**; the
TAXSIM crosswalk row 116 and the MO cells (0.89-0.93 clean TAXSIM,
0.87-0.94 PE) are unaffected. Crosswalk row 335 -> `encoded_overlay`; row
332 (per-spouse) unchanged.

## 4. Oregon: retirement income credit, other-states bond interest, kicker

### 4.1 Retirement income credit -- law

ORS 316.157 (oregon.public.law, as amended by HB 4034 of 2024, fetched
2026-10-13; oregonlegislature.gov reachable; law.justia.com Cloudflare-blocked)
and OR-17 2025 pp.106-107 (NBER mirror `OR/2025/publication-or-17_101-431_2025.pdf`,
credit code 811):

- Credit = the lesser of tax liability and **9% of net pension income**
  (316.157(1)); nonrefundable, no carryforward (OR-17: "Any credit not used
  this year is lost").
- "Eligible individual": receiving pension income and **62 or older** at
  year end (316.157(2)(a)).
- "Net pension income" (2)(d): joint return -- the lesser of the eligible
  individuals' pension income or the excess of **$15,000** over (i) Social
  Security benefits received by the individual or spouse and (ii) the excess
  of household income over **$30,000**; any other status -- the same with
  **$7,500** and **$15,000**.
- "Pension income" (2)(e): distributions included in Oregon taxable income
  from ERISA/401 employer plans, state/local and other states' public
  systems, federal civil service and military retirement, 457 plans, IRAs
  and SEPs (408), 403(a)/(b) annuities -- i.e. `txbl_pens_dist +
  txbl_ira_dist`, less any federal pension income subtracted (OR-17
  worksheet line 2; our federal pension subtraction is not modeled,
  crosswalk row 489).
- "Household income" (2)(b)-(c): the couple's aggregate federal AGI, **not
  including Social Security benefits**, plus otherwise-exempt items (exempt
  pensions, child support, inheritances, gifts over $500, insurance
  proceeds, nontaxable sick pay and strike benefits, workers' compensation,
  military pay, veterans' benefits, welfare, nontaxable dividends and
  interest, foreign income) and with net losses limited to $1,000 per source
  and depreciation over $5,000 added back; less the Oregon refund.
- "Social Security benefits" (2)(f): IRC 86 benefits, Title II and Tier 1
  railroad -- `gross_ss`.

The worksheet (OR-17 p.106): 1 retirement income of the eligible
individual(s); 2 federal pension subtraction; 3 = 1 - 2; 4 base $7,500 /
$15,000; 5 Social Security and Tier 1 RRB of both spouses; 6 = 4 - 5; 7
household income; 8 $15,000 / $30,000; 9 = 7 - 8; 10 = 6 - 9; 11 = lesser of
3 and 10; 12 = 9% of 11. Today `or/credits.yaml:141-142` documents it as not
modeled ("base and household-income measure not transcribed"); packet
`or.md:167-170`; tracker row 39 "next OR task".

### 4.2 Parameters, formula, worksheet cases

**Law parameters** (law schema, after `st_credits.retire_credit_income_base`
at `params_schema.yaml:736`):

```yaml
  # Rate-style retirement income credit (OR ORS 316.157): rate x the lesser
  # of the eligible spouses' pension + IRA income and a filing-status base
  # reduced by GROSS Social Security received and by household income over
  # a threshold; nonrefundable, folded into st_retire_credit
  st_credits.retire_inc_credit_rate: 0
  st_credits.retire_inc_credit_min_age: .inf
  st_credits.retire_inc_credit_base: {default: 0, units: per_return}
  st_credits.retire_inc_credit_hh_thresh: {default: .inf, units: per_return}
  st_credits.retire_inc_credit_income_base: 5   # st_income_base enum (OR: federal AGI less taxable SS)
  st_credits.retire_inc_credit_add_exempt_int: 0
  st_credits.retire_inc_credit_add_hh_resources: 0   # hh_resources_inc (S43)
```

**Stage change** in `src/calc/state/st_credits_senior.R`, after the banded
credit block (`:84-100`), adding to `st_retire_credit`:

```r
  # Rate-style retirement income credit (OR ORS 316.157): per-spouse
  # attribution of the pension pool is unobserved, so the eligible share is
  # the fraction of spouses at or above the age (the MO SS convention,
  # st_ded.R:642-647); household income excludes Social Security by statute
  # and the gross benefits reduce the base instead
  ric_n_elig = (tax_unit$age1 >= tax_unit$st_credits.retire_inc_credit_min_age) +
               (tax_unit$filing_status == 2 & !is.na(tax_unit$age2) &
                tax_unit$age2 >= tax_unit$st_credits.retire_inc_credit_min_age)
  ric_n_tp   = 1 + (tax_unit$filing_status == 2)
  ric_pension = pmax(0, tax_unit$txbl_pens_dist + tax_unit$txbl_ira_dist) *
                ric_n_elig / ric_n_tp
  ric_hh_income = st_income_base(tax_unit, tax_unit$st_credits.retire_inc_credit_income_base) +
    tax_unit$st_credits.retire_inc_credit_add_exempt_int * tax_unit$exempt_int +
    tax_unit$st_credits.retire_inc_credit_add_hh_resources * tax_unit$hh_resources_inc
  ric_base = pmax(0, pmax(0, tax_unit$st_credits.retire_inc_credit_base -
                             pmax(0, tax_unit$gross_ss)) -
                     pmax(0, ric_hh_income - tax_unit$st_credits.retire_inc_credit_hh_thresh))
  st_retire_credit = st_retire_credit +
    tax_unit$st_credits.retire_inc_credit_rate * pmin(ric_pension, ric_base)
```

`st_retire_credit` is already in the nonrefundable sum (`st_credits.R:553`
region, `senior$st_retire_credit`) and is a return variable, so the harness
column carries it. Add the seven names to `st_credits_senior_req_vars`
(`:11-29`); `gross_ss`, `exempt_int`, `hh_resources_inc`, `txbl_ira_dist` are
already in `calc_st_credits` req_vars (`st_credits.R:97,124,127,140`).
Inert: rate 0.

`or/credits.yaml` (replacing `documented_not_modeled.retirement_income_credit`
at `:141-142`; mapper entries `retire_inc_credit_base` and
`retire_inc_credit_hh_thresh` keyed 1/3/4 -> single, 2 -> joint):

```yaml
retire_inc_credit_rate: {value: 0.09, reference: 'ORS 316.157(1) "nine percent of net pension income"; OR-17 2025 p.106 worksheet line 12; credit code 811. Nonrefundable, no carryforward'}
retire_inc_credit_min_age: {value: 62, reference: 'ORS 316.157(2)(a) "attained 62 years of age before the close of the taxable year" (the 1999+ step of the 58-62 phase-in)'}
retire_inc_base_single: {value: 7500, reference: 'ORS 316.157(2)(d)(B) $7,500 for a return other than joint (head of household included); worksheet line 4'}
retire_inc_base_joint: {value: 15000, reference: 'ORS 316.157(2)(d)(A) $15,000 joint'}
retire_inc_hh_thresh_single: {value: 15000, reference: 'ORS 316.157(2)(d)(B)(ii) household income over $15,000; worksheet line 8'}
retire_inc_hh_thresh_joint: {value: 30000, reference: 'ORS 316.157(2)(d)(A)(ii) $30,000'}
retire_inc_credit_income_base: {value: 5, reference: 'ORS 316.157(2)(b): household income is federal AGI of both spouses "except that household income does not include Social Security benefits"; enum 5 = federal AGI less taxable SS (the gross benefits reduce the base at line 5 instead)'}
retire_inc_credit_add_exempt_int: {value: 1, reference: 'ORS 316.157(2)(c)(A)(xvii) nontaxable interest; (xvi) nontaxable dividends'}
retire_inc_credit_add_hh_resources: {value: 1, reference: 'ORS 316.157(2)(c)(A)(ii),(xiii),(xv): child support, veterans benefits, welfare -- the hh_resources_inc optional data column (S43). Gifts, inheritances, insurance proceeds, the $1,000 loss limits and the $5,000 depreciation add-back are unobserved [overstates the credit for recipients]. The federal pension subtraction (worksheet line 2) is not modeled, so the pension base is the whole pool'}
```

**Worksheet cases** (TY2024: single standard deduction 2,745 + aged $1,200
at 65+, joint 5,495 (`or/ded.yaml:98,111,128-134`); brackets 4.75% to 4,300,
6.75% to 10,750, 8.75% above, doubled for joint (`or/ord.yaml:45,58`);
exemption credit 249 per exemption (`or/credits.yaml:36`)):

- **OR-11a (eligible, SS reduces the base):** single 70, `txbl_pens_dist
  12000`, `agi 12000`, `gross_ss 6000`, `txbl_ss 0`, `liab_bc 0`. Household
  income 12,000; base 7,500 - 6,000 = 1,500; excess income 0; net pension
  min(12,000, 1,500) = 1,500 -> credit 135. TI = 12,000 - 3,945 = 8,055; tax
  204.25 + 6.75% x 3,755 = 457.71; less 249 exemption credit, less 135 ->
  **`st_retire_credit 135`, `liab_st_iit 73.71`**.
- **OR-11b (household income kills it):** single 65, `wages1 10000`, `ei1
  10000`, `txbl_pens_dist 20000`, `agi 30000`, `liab_bc 1000`. Household
  income 30,000 -> excess 15,000 >= base 7,500 -> **`st_retire_credit 0`**;
  TI = 30,000 - 1,000 - 3,945 = 25,055; tax 639.63 + 8.75% x 14,305 =
  1,891.31 - 249 = **1,642.31**.
- **OR-11c (joint, one eligible spouse, nonrefundable):** `filing_status 2`,
  ages 64/58, `txbl_pens_dist 16000`, `agi 16000`, `gross_ss 2000`, `txbl_ss
  0`, `liab_bc 0`. Eligible share 1/2 -> pension 8,000; base 15,000 - 2,000 =
  13,000; household income 16,000 < 30,000; credit 9% x 8,000 = 720. TI =
  16,000 - 5,495 = 10,505; tax 4.75% x 8,600 + 6.75% x 1,905 = 537.09; less
  498 exemption credits = 39.09; the 720 is nonrefundable ->
  **`st_retire_credit 720`, `liab_st_iit 0`**.

**External models.** PolicyEngine's `or_retirement_credit.py` is the statute
with `taxable_pension_income` (the driver's pension plus the folded IRA),
gross `social_security`, household income = AGI + exempt interest - taxable
SS, per-person age gate at 62, pension split evenly between spouses in the
driver (`pe_state_tax.py:87`) -- which equals our proration exactly. So
after encoding, PE and we should agree on OR retirees where today PE is
lower; expect the OR PE cells (0.78-0.93 clean) to move up on 62+ pension
records. TAXSIM: unknown whether it models the credit; the row in §4.5 is
`annotate` until measured.

### 4.3 Other states' bond interest addition -- law fix

ORS 316.680(2)(a) (fetched 2026-10-13): "There shall be added to federal
taxable income: (a) Interest or dividends, exempt from federal income tax, on
obligations or securities of any foreign state or of a political subdivision
or authority of any foreign state" ("foreign state" is Oregon's term for
another U.S. state). OR-17 2025 p.63, addition code 158: "Oregon taxes
interest and dividends on bonds and notes of another state or political
subdivision of another state that you didn't include on your federal
return." Oregon's own bonds stay exempt (the code 317 subtraction on p.85
covers Oregon obligations that reached federal income). Today
`or/agi.yaml:41-42` says the addition "is not encoded here (not even the 75%
convention)"; crosswalk row `tax_data_gaps.csv:493` (undertax); packet
`or.md:52-54`.

**Fix** in `or/agi.yaml` (the existing `st_add_muni` at `st_agi.R:481-482`
does the work; `interest_own_state_share` default 0.75, `schema:54-61`):

```yaml
add_exempt_int:
  value: 1
  reference: 'ORS 316.680(2)(a); OR-17 2025 p.63 "Interest and dividends on government bonds of other states" [addition code 158], Schedule OR-ASC Section A; 2017-2025'
own_state_exempt:
  value: 1
  reference: 'Oregon and Oregon-subdivision obligations are not added (ORS 316.680(2)(a) reaches other states only); the Oregon share of exempt_int is the interest_own_state_share optional data column, default 0.75 (S43)'
```

and delete `documented_not_modeled.out_of_state_muni_interest` (`:41-42`).

**Worksheet case:** **OR-12** = OR-1 (`:5384`) with `exempt_int 4000` ->
`st_additions 1000`, `st_agi 61000`; TI = 61,000 - 6,000 - 2,745 = 52,255;
tax 639.63 + 8.75% x 41,505 = 4,271.31 - 249 = **4,022.31**.

### 4.4 Kicker -- deferred (S46(c))

ORS 291.349; OR-40 2025 line 32 at 9.863% of TY2024 liability before
credits; 17.171% on the TY2019 return; 17.341% TY2021; 44.28% TY2023
(`research/state_tax/notes/or_kicker_design_note.md:28-36`). Not encoded in
this batch: it reads a prior-year state liability, which is the orchestration
design that note owns. The `prior_year_liab` overlay (`schema:351-358`) and
crosswalk row 496 stay as they are.

### 4.5 Known-difference rows (Oregon)

1. **OR, taxsim, 2019, state-law, exclude -- row 88 stays** (the kicker:
   `st_tax_pre_credit > 0`, TY2019 only, 0.999 after exclusion).
2. **OR, policyengine, 2021-2025, state-law, exclude (pre-registered
   guard).** "The Oregon kicker (ORS 291.349) is in or_refundable_credits
   for every year (or_kicker = percent x or_tax_before_credits_in_prior_year;
   17.341% 2021, 44.28% 2023, 9.863% 2025). PE's prior-year base is an input
   with no formula and the driver never passes it, so PE pays zero in the
   harness today; the provision is deferred on our side (S46(c)). This row
   fires only if a future driver passes the base." Direction `over` (PE
   lower). Magnitude "the certified percentage of prior-year tax before
   credits". Predicate `pe_or_kicker > 0` (export `"pe_or_kicker":
   ["or_kicker"]`). Source "or_kicker_design_note.md; this note §4.4". Why:
   S46(c) says "excluded in the harness where an external model pays it";
   the guard documents that PE does not, and protects the odd-year PE cells
   if that changes.
3. **OR, taxsim, 2017-2020, external-model-concept, annotate (pre-registered).**
   "Retirement income credit (ORS 316.157): encoded 2026-10-13 at 9% of net
   pension income with the $7,500/$15,000 base less gross Social Security
   and household income over $15,000/$30,000, 62+; whether TAXSIM models it
   is not established (its state code is a WASM binary)." Direction
   `either`. Magnitude "up to 9% x $7,500 = $675 single / $1,350 joint".
   Predicate `(age1 >= 62 | (filing_status == 2 & !is.na(age2) & age2 >= 62)) & (txbl_pens_dist + txbl_ira_dist) > 0 & agi < 45000`.
   Source "this note §4.2; measure on the next OR TAXSIM rerun -- if the
   62+ low-income pension cells move toward 1 the row is deleted, if away it
   becomes an exclude with the same predicate". No PE row: PE models it on
   the same inputs (§4.2).
4. **OR, both, 2017-2025, input-coverage, exclude.** "Other states' bond
   interest addition (ORS 316.680(2)(a)): encoded 2026-10-13 as 25% of
   exempt_int (the 1 - interest_own_state_share convention); TAXSIM has no
   tax-exempt interest input and PolicyEngine's or_additions has no formula
   (variables/gov/states/or/tax/income/or_additions.py), so neither external
   model adds it. Materiality bound: 25% x exempt_int x the top rate must
   clear the $100 tolerance." Direction: ours higher. Magnitude "0.25 x
   exempt_int x 8.75-9.9%". Predicate `exempt_int * 0.25 * 0.099 > 100`
   (exempt_int above about $4,040). Source "this note §4.3; the UT
   materiality-bound precedent (known_differences.csv:93)". Why defensible:
   the input never reaches either model, and the bound keeps the excluded
   share small.
5. Rows 87 (OR itemizer crosswalk exposure) stays.

## 5. Our-side fixes that need no data

### 5.1 Michigan

- `mi/exempt.yaml:24-29`: `blind_addl` to the line 9b amounts (§2.2);
  `aged_addl` reference rewritten (no aged exemption, not "status inputs not
  in the PUF").
- `mi/agi.yaml:53-58` comment ("public pensions ... source unobservable")
  and `mi.md:36` ("public pensions (unlimited for Tier 1) treated as
  private/capped -- source unobservable"): S43 overlay statement (§2.1).
- `mi.md:13` ("Homestead property-tax credit is not part of this initial
  liability"), `mi.md:37` ("needs household resources, rent, property tax,
  and disability inputs and is omitted"), `mi.md:38` ("Disabled-veteran,
  stillbirth, disabled, and other special exemptions are omitted pending
  eligibility inputs" -- blind is now encoded): rewrite per §1 and §2.
- `mi.md:5` "Last updated 2026-07-13" and `mi.md:43` "Cross-model: todo"
  are stale: the tracker (`state_parameter_rollout.csv:25`) records the
  2026-08-11 triage, rerun and the home heating resolution; bring the
  packet's cross-model section up to the tracker and date it.
- `mi/credits.yaml` is three parameters (`:1-9`); the circuit-breaker block
  (§1.3) and a `documented_not_modeled` block for the home heating credit
  (S46(b)) go there.

### 5.2 Missouri

- `mo/agi.yaml:60-62` `documented_not_modeled.public_pension_exemption` and
  `mo/ded.yaml:123-128` (header: "Only the pieces that survive the PUF's
  inability to tell a public pension from a private one are encoded"),
  `mo/ded.yaml:163-165`: replace with the §3.2 encoding; `mo/ded.yaml:167`
  (per-spouse worksheet) extends to the public cap.
- `mo/credits.yaml:6-9` (header: the circuit breaker "waits on the Tier 1
  tenure/rent imputation") and `:24-30` (`wftc_credit_ordering`,
  `property_tax_credit`): replace with §1.3; the WFTC ordering becomes
  `cb_offsets_nonref`.
- `mo.md:94-95`, `:151-159`, `:164-165`, `:176-177`: rewrite; add the
  per-year maximum Social Security benefit table (§3.1) to the packet.
- `state_parameter_rollout.csv:27` notes ("BIGGEST KD: public pension
  exemption ... NOT MODELED") and `ty2026_law` (if the HB 594 & 508 circuit
  breaker values are encoded).

### 5.3 Oregon

- **`or/agi.yaml:24-26` and `:33-34` are wrong today**: `sub_us_int: 1` is
  set and the calculator subtracts `interest_us_share` (0.15) x `txbl_int`
  (`st_agi.R` `st_sub_us_int`; crosswalk row 486 says so), but the reference
  says "the flag is carried and no subtraction is taken" and the
  documented_not_modeled entry says "no amount is actually subtracted". Fix
  the strings to the S43 statement (the share is the `interest_us_share`
  overlay, default 0.15).
- `or/agi.yaml:41-42` (`out_of_state_muni_interest`): delete, encode (§4.3).
- `or/credits.yaml:7-11` header and `:141-142`: the retirement income credit
  is encoded (§4.2); the header's "the other two because their bases are not
  reconstructible" now covers the WFHDC only.
- `or.md:52-54`, `:86-88`, `:167-174`, `:180-182` and tracker row 39
  ("retirement income credit (ORS 316.157, 9% rate known but base/limits not
  transcribed -- next OR task)"): close.
- `or.md:154-162` and `or/credits.yaml:137-138` (kicker): add "deferred by
  S46(c); design in or_kicker_design_note.md; PE guard row" -- the text
  "deliberately not attempted" stays true.

## 6. Crosswalk, schema and documentation edits (checklist)

- `research/state_tax/tax_data_gaps.csv`: row 279 (MI homestead)
  `overlay_variable` -> `property_tax_paid;rent_paid;hh_resources_inc`,
  `current_handling` -> `encoded_overlay`, `yaml_file` -> `mi/credits.yaml`;
  row 333 (MO circuit breaker) and 334 (MO net household income) likewise;
  row 290 (MI Tier 1 public) -> `encoded_overlay`; row 287 (MI blind) ->
  `current_handling` `encoded` (native column); row 335 (MO public pension)
  -> `encoded_overlay`; row 493 (OR bond addition) -> `constant` with
  `overlay_variable interest_own_state_share` (now the same class as row
  282); row 278 (MI home heating) note "out of scope by S46(b); PE/TAXSIM
  exclusion rows"; row 496 (kicker) note "deferred by S46(c)".
- `config/interfaces/supplemental_inputs_schema.yaml`: no new variables.
- `config/scenarios/tax_law_state/params_schema.yaml`: the 29 `cb_*`
  scalars (§1.3), `st_agi.pension_excl_public_uncapped_min_age` (§2.1), the
  four `st_ded.retire_exempt_pub*` (§3.2), the seven
  `st_credits.retire_inc_credit_*` (§4.2); `st_credits.cb_income_limit_owner/renter`
  need no family entry (scalars, mapped in YAML).
- A new `credit_tables.csv` beside
  `config/scenarios/tax_law_state/baseline/mi/credits.yaml`: the
  `cb_senior_share` and `cb_exempt_rate` rows (§1.3).
- `src/calc/state/st_credits_household.R`, `st_credits.R`, `st_agi.R`,
  `st_ded.R`, `st_credits_senior.R`: per §1.3, §2.1, §3.2, §4.2.
  `return_vars$calc_st_credits` gains `st_cb_credit`; no SLURM file changes
  (all inside `run_one_year()`).
- `src/tests/state/test_supplemental_inputs_reader.R:212`: add `'MI'` (and
  `'MO'`) to the invariance `states` vector so the circuit breaker and the
  pension overlays are exercised.
- `src/tests/state/cross_model/pe_state_tax.py` OUTPUT_VARS (`:101-166`):
  `pe_mi_homestead`, `pe_mi_home_heating`, `pe_mo_property_tax_credit`,
  `pe_or_kicker`; `test_state_cross_model.R:974-983` record: `blind_n`;
  `:1002-1010` select: the new columns.
- `src/tests/state/cross_model/known_differences.csv`: §1.7 rows 1-5, §2.4
  row 1, §4.5 rows 2-4.
- `research/state_tax/notes/state_data_imputation_plan.md:90-118` (item 1):
  add "MO-PTS/PTC" to "Unlocks" and note the component exists; `:141-155`
  (item 3): add the MI/MO circuit-breaker income concepts and the OR
  retirement-credit household income to "Unlocks".
- `research/state_tax/plan.md:629`: the review item now points here.
- Packets `mi.md`, `mo.md`, `or.md` and tracker rows 25, 27, 39 in the same
  change (state-tax protocol step 2).
- Worksheet tests: MI-11a/b/c, MI-12, MI-13a-f, MO-15a-d, MO-16a-e,
  OR-11a/b/c, OR-12 as computed; re-run MI-1, MI-2, MI-3, MO-8, MO-9,
  MO-10a/b, OR-1 unchanged as the inert pins.
- JI review before the circuit breaker ships (S46(a); plan §7): the MI Table
  A and exemption ladders as tables vs the MO step rule as parameters; the
  two household-income concepts (§1.3 `cb_add_*`); `cb_elig_disabled` and
  the disabled1/2 flag against "reads only" in S46(a); `cb_offsets_nonref`;
  the MO TY2026 statute values against S45; which other circuit breakers
  (WI Schedule H, MT 2EC, ME 5219-KK, RI-1040H, VT, NJ ANCHOR) the component
  carries next.

## Revision history

- **2026-10-13** -- First version. Sources read: MCL 206.520, 206.522
  (legislature.mi.gov), MI-1040CR instructions 2019 and 2024, MI-1040CR form
  2024, MI-1040 forms/booklets 2017-2025 for line 9b (all NBER mirror);
  RSMo 135.010, 135.025, 135.030 (revisor.mo.gov, text of 2025-08-28),
  MO-A forms 2017-2024 and the 2025 MO-1040 instructions for the Section A
  maximum Social Security benefit, MO-PTS 2024, the 2021 MO-PTC claim chart
  (NBER mirror); ORS 316.157 and 316.680 (oregon.public.law;
  law.justia.com is Cloudflare-blocked from the cluster), OR-17 2025
  pp.63, 85, 106-107 (NBER mirror); policyengine-us source in the pinned
  venv (`mi/`, `mo/`, `or/` parameters and variables). No model was run.
