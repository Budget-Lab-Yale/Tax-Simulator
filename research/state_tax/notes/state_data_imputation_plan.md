---
title: "State-tax data extensions and imputations — scoped plan"
role: notes
workstream: state_tax
status: open
updated: 2026-10-13
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# State-tax data extensions and imputations — scoped plan

Drafted 2026-07-24, from the known-differences accumulated across the 30
encoded jurisdictions (source packets + `src/tests/state/cross_model/known_differences.csv`). This is
the inventory of everything the PUF cannot see that state law needs,
organized as buildable workstreams. Companion to the Phase 7 list in
`research/state_tax/plan.md`; items here are DATA work, distinct
from the locality-weights and coupled-iteration machinery items.

Guiding rule: each imputation should be (a) driven by data we already
hold or can mirror (the IPUMS ACS extracts and IRS-Ind are wired in),
(b) calibrated to a published administrative aggregate, and (c) switchable
off (a flag) so the record-level cross-model harness can keep comparing
law-only calculations.

**CPS ASEC note (added 2026-08-18).** Several items below name CPS ASEC data
(the pension source split; the nontaxable-income/household-resources vector).
The non-filer residual workstream is registering an ASEC family in the shared
extract store — checking `raw_data` first, and otherwise adding one through the
same common IPUMS download machinery that maintains `ACS/acs_common`. **Consume
that family rather than pulling a project-specific extract**, and add any
variables these items need to the shared request instead of forking it. Same
rule as IRS-GEO/IRS-Ind: one store, one manifest, several consumers. Status and
naming: `research/state_weights/nonfiler_residual_design.md` §4.1.

**Crosswalk (added 2026-10-13, decision S43).** The machine-readable registry
is now `research/state_tax/tax_data_gaps.csv`: one row per (state, provision)
that the pinned Tax-Data vintage cannot compute as written, with the required
variable, the nearest Tax-Data column and its status (`absent`,
`present_unused`, `present_zero`, `present_capped`, `present_pooled`,
`present_partial`), the current handling, the rollup item number below, and
the **overlay variable** name the provision will be gated on once the
supplemental-inputs interface (W2 of the 2026-10-13 plan) carries it.
`research/state_tax/scripts/check_tax_data_gaps.R` validates the file. This
document keeps the prose rollup and the method notes; the per-state detail is
the CSV, and a packet's "Data limitations" bullet points at its rows rather
than restating them.

**Maintenance convention (added 2026-08-11):** this file is the single
registry of encoding-blocked-on-data cases. When a packet documents a
feature as unencodable because the PUF lacks an input, add or extend the
matching item here (and its rollup row below) in the same commit — the
per-state packets hold the law detail; this file holds the cross-state
picture.

## Rollup — one line per imputation, states and stakes

Counts as of 2026-10-13, derived from `tax_data_gaps.csv` (613 rows, 45
jurisdictions with an income or investment-income tax; AK FL NV SD TX WY have
no rows). "States (rows)" counts jurisdictions with at least one row on the
item; "High / medium" counts rows at those materiality grades. Items 12–14
were added on 2026-10-13 because the crosswalk showed recurring gaps the
original eleven did not name. The 2026-08-11 table (30 states) is in git
history.

| # | Imputation | Tier | States (rows) | High / medium | Jurisdictions | Main overlay variables |
|---|---|---|---|---|---|---|
| 1 | Tenure / rent / property tax (homestead family) | 1 | 23 (31) | 12 / 13 | AR AZ CA CT DC HI IL IN MA ME MI MN MO MT NE NJ NM NY OK RI VT WI WV | property_tax_paid, rent_paid, tenure |
| 2 | Pension source split (public / military / railroad; HI employer-funded share; KY pre-1998 service share) | 1 | 36 (70) | 14 / 25 | AL AR AZ CO CT DC DE HI IA ID IN KS KY LA MA MD ME MI MN MO MT NC ND NE NM NY OH OK OR RI SC UT VA VT WI WV | pension_military, pension_public, pension_railroad |
| 3 | Household-resources income (SSI/TANF/VA/child support/SNAP) -- and its opposite, nontaxable SUPPORT income (gifts, inheritances, educational assistance; PA Schedule SP excludes SS/UC/public assistance/child support) | 1 | 12 (13) | 2 / 6 | GA HI ID ME MI MO MT NM OK PA TN VA | hh_resources_inc; nontax_support_inc (PA) |
| 4 | Elective deferrals (401(k)/403(b)/457) wage-base add-back | 2 | 2 (2) | 1 / 1 | NJ PA | elective_deferral1/2 — **Tax-Data column exists, all zero** |
| 5 | Dependent detail (>3 slots, student, disabled, first-year) | 2 | 17 (24) | 0 / 3 | AL AR ID IN MA MD ME MN MO MT NJ NM OR SC UT VA WV | dep_disabled_n, dep_age4plus, dep_student_n |
| 6 | Filer disability status | 2 | 19 (22) | 0 / 1 | AR CO DE GA HI IA IN KS LA MD MI MN MO NE NH NJ OH VA WV | disabled1, disabled2 |
| 7 | Interest composition (US-obligation / own-state muni shares) | 2 | 41 (78) | 0 / 1 | every broad-IIT state plus NH TN | interest_us_share, interest_own_state_share — now applied as constants 0.15 / 0.75 in `st_agi.R`, not "flagged, not taken" |
| 8 | Business-side detail (asset class, situs, NOLs, depreciation) | 3 | 28 (47) | 2 / 4 | AL CA CT DC DE IA ID KS KY LA MD ME MN MO NC NE NM NY OH OK PA RI SC VA VT WA WI WV | other:kg_asset_class_share and kin |
| 9 | Expense niches (529/ABLE, LTC, K-12, adoption, premiums, tuition) | 3 | 38 (87) | 0 / 6 | 38 states | contrib_529, tuition_paid, health_premiums, ltc_premiums |
| 10 | Election features (min-liability passes) | 3 | 4 (4) | 0 / 0 | HI NE UT VA | other:salt_sales_elector (WI Act 15 is encoding, not data) |
| 11 | Credit take-up | 3 | 2 (2) | 0 / 1 | NE WA | care_provider_approved |
| 12 | Per-person attribution of unit-level income (pension, SS, unearned, MFS spouse) | 2 | 29 (42) | 1 / 9 | AL AR AZ CA CO DE GA HI IA IN KY LA MD ME MN MO MS MT ND NJ NY OH OK PA RI SC VA WA WI | pension_person1/2, ss_person1/2, living_apart, spouse_agi_mfs (HI N-311, KY family size, PA SP; encoded 2026-10-13) |
| 13 | Prior-year state liability (OR kicker; carryforwards) | 2 | 5 (6) | 1 / 1 | HI IA LA MT OR | prior_year_liab — a lag in sequential runs, not a survey imputation |
| 14 | Source state / nonresident and other-state credit | 3 | 42 (42) | 0 / 2 | every broad-IIT state | source_state — model-wide (`docs/model_documentation.md` §10.5) |
| new | Unassigned niches | 3 | 41 (101) | 0 / 12 | — | see `overlay_variable` per row |
| none | Locality (Phase 7) and law-only rows kept for completeness | — | 26 (42) | 5 / 13 | — | other:locality |

---

## Tier 1 — big, broad-based, buildable from assets we already hold

### 1. Tenure / rent / property-tax imputation (the homestead family)

> **Owned by `research/state_weights/plan.md` groups I–J as of 2026-08-23.**
> This entry stays as the state-side statement of what the imputation unlocks;
> the *method*, the donor-survey choice, the pre-TCJA itemizer strategy, the
> calibration-target tension with the federal side, and the per-state
> application now live in that plan's second phase. Do not scope the work from
> here.


- **Unlocks:** WI homestead credit; MI homestead credit; **MN renter
  credit (2024+, ON the M1 — a structural liability gap, not a side
  program)**; the renter side of WI's school property tax credit;
  property taxes for NON-itemizers feeding the IL/CT property-tax
  credits and MD/MN/NJ deduction bases; future NJ ANCHOR/NY/DC circuit
  breakers.
- **Data:** our IPUMS ACS extracts carry TENURE, gross rent, and
  property-tax amounts by state, income, age, household size. PUF-side
  anchor: `salt_prop` for itemizers (calibration overlap cell).
- **Method:** model-based or hot-deck imputation of (tenure, rent,
  property tax) within state x income x age x household-size cells;
  preserve the itemizer `salt_prop` where observed; calibrate state
  totals to ACS aggregates and, for credits, to each state's published
  program statistics (WI DOR Schedule H claims by income range; MN DOR
  renter-credit totals; MI homestead reports).
- **Notes:** PolicyEngine models these credits but only meaningfully on
  survey data — in our harness it sees no rent, so cross-model stays
  exclusion-based until this lands. Sequence after Phase 1 weights (the
  same ACS cells are already wired).

### 2. Pension source split (public / military / occupation)

- **Unlocks (largest recurring elderly gap):** MI Tier-1 unlimited
  public pensions; MD military (code u) and Hometown Heroes (code v);
  NC Bailey settlement + military; ID 63-3022A (CSRS/military/police);
  KY source distinctions; CT teacher/military; AZ government pensions;
  OH uniformed services; MN QPEN; WI pre-1964/military exemptions. All
  currently omitted one-sidedly (we overtax affected retirees).
- **Data:** CPS ASEC pension income with industry/class-of-worker of
  longest job; DoD Office of the Actuary military-retiree counts and
  dollars BY STATE (published annually); Census of Governments
  retirement-system payments by state; SOI aggregates.
- **Method:** impute a (public share, military flag) onto pension-
  receiving units by state x age x pension size, calibrated so each
  state's military-retiree dollars match the DoD actuary tables and
  public-pension dollars match Census retirement-system payouts.
- **Notes:** the military flag alone unlocks most of the list (military
  pay is the most common full exemption). Direction of current bias is
  uniformly UP on liability for affected units, so this also matters
  for aggregate validation reads.

### 3. Broad household-resources income (nontaxable additions)

- **Unlocks:** homestead-family income concepts (with #1); PA Tax
  Forgiveness eligibility income (currently overstates forgiveness);
  MD/VA poverty-level credits (poverty-guideline gates); ID grocery
  SNAP-month proration and PBF public-assistance exemption.
- **Data:** nontaxable SS is already observable (`gross_ss − txbl_ss`).
  SSI, TANF/public assistance, veterans' benefits, child support, and
  workers' comp need CPS ASEC/ACS imputation by state x income x
  demographic cell.
- **Method:** joint imputation of a small vector of nontaxable income
  items (SSI, PA, VA benefits, child support) with program-participation
  flags (SNAP for ID); calibrate to SSA/state program administrative
  totals.

## Tier 2 — targeted, moderate effort

### 4. Elective deferrals (401(k)/403(b)/457) wage-base add-back

- **Unlocks:** PA compensation base (the single largest PA wage-base
  known-difference; all external models share it); NJ later (same rule).
- **Data:** SOI W-2 statistics (deferral amounts by wage size); DCP
  participation rates by earnings from BLS NCS.
- **Method:** impute deferral amounts onto wage earners by wage level
  and age; add to the PA/NJ compensation class only.

### 5. Dependent detail extensions

- **Unlocks:** MN CTC (no child limit — we cap at three tracked ages);
  EITC-type "older child" categories (MN M1CWFC; proxied 18-23 now);
  disabled-dependent categories (MD CTC, care credits); IN first-year
  dependent exemption; MD 65+ dependents' extra exemption.
- **Data/method:** extend the tax-unit data model beyond three dependent
  age slots where the underlying microdata supports it; impute student
  status at 19-23 (ACS enrollment rates) and dependent disability (ACS
  disability rates by age). Small revenue but recurring machinery
  friction.

### 6. Disability status of filers

- **Unlocks:** MD pension exclusion under-65 disabled; homestead
  disabled-eligibility doors; M1R; several senior credits' disabled
  prongs.
- **Data:** ACS disability items by age/state; SSDI receipt (imputable
  jointly with #3's SSI).

### 7. Interest-composition shares

- **Unlocks:** US-obligation subtraction (flag carried in EVERY state,
  no subtraction taken anywhere); own-state municipal share (75%
  convention everywhere).
- **Data:** ICI fund-holdings data, Fed flow of funds, SOI interest
  detail; state-level muni-fund shares for the largest states.
- **Method:** replace the 0/75% conventions with imputed shares by
  portfolio size; low ceiling on accuracy — treat as a bounded
  refinement, calibrate against nothing stronger than national shares.

## Tier 3 — document-permanently unless a state's triage forces it

- **Business-side detail:** bonus-depreciation/179 addback stacks
  (MN/ID/WI/MD 500DM), state NOL regimes, OH BID rental inclusion, SSTB
  shares. Not representable at PUF granularity; permanent
  known-differences.
- **Expense-based niches:** K-12 education expenses (MN/IL/WI), 529/
  HSA/MSA/ABLE contributions (PA/ID/MN/WI/MD), long-term-care premiums,
  adoption expenses, student-loan interest at the state level. Small
  aggregates; document, revisit only if a state's cross-model or
  aggregate validation shows a material residual.
- **Election/optimization features:** WI 2025 retirement election with
  credit forfeiture (needs a min-liability election pass); sales-tax
  deduction election imputation (already on the Phase 7 list; TAXSIM
  imputes it, we use as-reported).
- **Take-up:** state credits currently assume full take-up. EITC state
  participation rates exist as an untargeted validation covariate (see
  the weights work); modeling take-up belongs with behavioral modules,
  not data imputation.

## Items 12–14 (added 2026-10-13)

### 12. Per-person attribution of unit-level income

Tax-Data carries pensions, Social Security, interest, dividends and capital
gains at the tax-unit level. Twenty-nine states apply a per-spouse cap,
per-spouse age test or per-spouse eligibility to one of these, and the
combined-return and separate-return machinery (`st_split.R`) halves non-wage
income 50/50 because ownership is unobserved. Item 2 is the *source* of a
pension; this item is *whose* it is. Overlay: `pension_person1/2`,
`ss_person1/2`, `living_apart`. Candidate source: CPS ASEC person-level
retirement and SS receipt, conditioned on the unit's age pair.

### 13. Prior-year state liability

Oregon's kicker (and a handful of carryforward provisions in HI, IA, LA, MT)
need last year's state liability. In a sequential production run the model
has it; in a single-year or year-parallel run it does not. This is a lag
convention to settle in `or_kicker_design_note.md`, not a survey imputation.
Overlay: `prior_year_liab`.

### 14. Source state and nonresident income

Every record is wholly resident in one state and all its income is sourced
there, so no state's credit for taxes paid to other states is computed and
no nonresident return is modeled. Universal, low per-record, concentrated in
commuter corridors (DE, NJ, DC, NY, MD). Candidate source: LODES
residence-workplace flows. Overlay: `source_state`.

---

## Columns Tax-Data already carries (candidate first-use — not adopted)

The crosswalk found 38 rows where the YAML or packet calls an input
"unobserved" but a Tax-Data column exists, and 28 more where the column is
present but capped or zero. **JI decided on 2026-10-13 not to adopt any of
these as proxies in the current push**; they are recorded so that the next
Tax-Data conversation can start from what is already in the file. Status
`present_unused` rows by column (states):

| Column | What it is | Rows (states) | Provision family |
|---|---|---|---|
| `trad_contr_er1/2` | employer-plan pretax contributions — **all zero in every vintage checked** (2026070814, 2026092720_filer_refit_v2) | 5 (MA ME NJ PA) + 2 partial | item 4 deferral add-back; MA/ME public-retirement pick-ups |
| `early_penalty` | 10% early-distribution penalty, i.e. a flag for the distribution | 4 (ME MS NE PA) | pre-55 carve-outs, NE other tax, PA cost-recovery |
| `med_exp` | pre-floor medical expenses (itemizers) | 3 (AL AR HI) + 6 partial | state medical floors of 4% / 10% applied to `med_item_ded_potential`, which is post-federal-floor |
| `sl_int_ded` | student-loan interest deduction | 3 (ME MN) | student-loan credits |
| `care_exp` | child-care expenses **capped at the federal $3k/$6k** | 2 unused (CO SC) + 10 capped | every state care credit with its own cap or base |
| `blind1/2` | blind status | 2 (MI NM) | blind exemptions the state YAML zeroed as "not in the PUF" |
| `se_health`, `old_cred`, `alimony_exp`, `divorce_year`, `other_gains`, `hsa_contr`, `kg_collect`, `aoc_exp`, `nols`, `ot1`, `auto_int_exp`, `first_mort_bal`, `res_energy_cred`, `sole_prop`, `exempt_int`, `dep_age1` | one row each | MO NJ NE VT MS PA MA KY NE AL HI DC UT SC | see the CSV |
| `c_housing_utilities` | CEX owned-plus-rented housing and utilities, collapsed in Tax-Data `src/cex.R` | 1 (MI home-heating) | the only rent-like field; not rent paid |
| `value.primary_home`, `value.primary_mortgage` | SCF wealth imputation (59% / 34% non-zero) | not cited by any row yet | the natural tenure proxy for item 1 |

Two more Tax-Data facts the crosswalk relies on: `n_dep_eitc` is zero for
every separate filer (STATUS.md, "Tax-Data input question"), and the file has
no state or county column — every `key = state` row depends on the
State-Weights split, not a record field.

---

## Asks of Tax-Data (ordered by materiality in the crosswalk)

Each ask names the overlay variable it would land in, so that Tax-Data
absorbing it is zero-code on the simulator side (the resolver in
`ensure_st_data()` prefers a native column over the overlay).

1. **Fill `trad_contr_er1/2`** (item 4). The column exists and is all zero;
   SOI W-2 tabulations or NCS give the distribution. Unlocks PA (largest
   single PA base gap) and NJ; `elective_deferral1/2`.
2. **Pension source: public / military / railroad shares** (item 2). 36
   states, 14 high-materiality rows; the military flag alone unlocks most.
   `pension_public`, `pension_military`, `pension_railroad`, `military_pay`.
3. **Tenure, rent paid, property tax paid for non-itemizers** (item 1). 23
   states, 12 high rows: MN renter credit (on-form 2024+), MI/WI/MO/MT
   homestead, NJ/MA rent deductions, NE PTC. `tenure`, `rent_paid`,
   `property_tax_paid`. Tax-Data's plan already carries this as groups I/J;
   Affordability-Index holds reported ACS values per donor id (see the
   supplemental-inputs interface note).
4. **Per-person SS and pension amounts** (item 12). `ss_person1/2`,
   `pension_person1/2`.
5. **Household-resources income: SSI, TANF, VA, child support, SNAP** (item
   3). PA forgiveness, NM LICTR, ME/VA/ID credits. `hh_resources_inc`.
6. **Dependent detail: student 19–23, disabled, slots beyond three, first
   year** (item 5). `dep_student_n`, `dep_disabled_n`, `dep_age4plus`.
7. **Filer disability and veteran status** (item 6). `disabled1/2`,
   `veteran1/2`.
8. **Uncapped care expenses and provider type** (items 9/11).
   `care_exp_uncapped`, `care_provider_approved`.
9. **`n_dep_eitc` for separate filers** — an input question, not an
   imputation.

Everything in items 8–9 (529/ABLE, LTC, K-12, adoption, asset-class shares)
stays documented-permanently unless a state's triage forces it.

---

## Cross-cutting notes

- **County/locality of residence** is deliberately NOT here — it is the
  Phase 7 locality-weights workstream (§2.6: SOI county data; NYC and MD
  counties first). The MD research already banked the county rate
  matrices and PE's `gov/local/md` tree as sources.
- **Harness discipline:** every imputation ships with an off switch and
  its own validation note, because the record-level TAXSIM/PE harness
  compares LAW, not imputations — imputed inputs should be off (or
  matched into the external models' inputs) in cross-model runs.
- **Sequencing (amended 2026-10-13, S43):** *encoding* no longer waits on
  any of this. A data-blocked provision is encoded against its overlay
  variable with a neutral default, so it is inert until a producer publishes
  the variable through the supplemental-inputs interface. The *imputation*
  work keeps its old order: Tier 1 items follow the Phase 1 weights
  production swap-in (shared cells, shared validation datasets); Tier 2 items
  are independent and can ride along with the states that need them.
- **Two producers, one contract:** Tax-Data (native column; preferred) or
  Affordability-Index (ACS values collapsed to the Tax-Data `donor_id`,
  published under the supplemental-inputs contract). The simulator does not
  care which; the coverage file written per run says which variables were
  live.

---

## Revision history

- **2026-10-13** — Rollup rebuilt from `tax_data_gaps.csv` at 51 states (was
  30, 2026-08-11); items 12–14 added; "Columns Tax-Data already carries" and
  "Asks of Tax-Data" sections added; sequencing note amended for S43.
- **2026-08-23** — Last amendment before the crosswalk (CPS ASEC note,
  maintenance convention).
