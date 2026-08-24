---
title: "State-tax data extensions and imputations — scoped plan"
role: notes
workstream: state_tax
status: open
updated: 2026-08-23
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

**Maintenance convention (added 2026-08-11):** this file is the single
registry of encoding-blocked-on-data cases. When a packet documents a
feature as unencodable because the PUF lacks an input, add or extend the
matching item here (and its rollup row below) in the same commit — the
per-state packets hold the law detail; this file holds the cross-state
picture.

## Rollup — one line per imputation, states and stakes

Counts as of 2026-08-11 (30 encoded jurisdictions; "R6 joiners" are
un-started states whose preliminary classification in
`research/state_tax/STATE_ENCODING_REVIEW_2026_08_11.md` §2.1 adds them to the item).

| # | Imputation | Tier | Encoded states affected | R6 joiners | Materiality and direction |
|---|---|---|---|---|---|
| 1 | Tenure / rent / property tax | 1 | 6 — WI MI MN (homestead/renter credits) + IL CT (credit inputs) + MD (ded base) | NJ ME NE (+NY DC circuit breakers later) | **HIGH.** Includes the module's one structural on-form gap (MN renter credit 2024+); one-sided against low-income renters/owners |
| 2 | Pension source split (public/military/RR) | 1 | 12 — MI MD NC ID KY CT AZ OH MN WI + NY (govt full sub) SC (military) — VA's military-benefits ramp and UT's military credit ride the same flag | HI (employer-funded share) MO (public) | **HIGH.** Uniformly overtaxes affected retirees; the military flag alone unlocks most of the list |
| 3 | Household-resources income (SSI/TANF/VA/child support) | 1 | 4 — PA (forgiveness income; currently UNDERTAXES via overstated forgiveness) MD VA (poverty credits) ID (SNAP proration) | ME (PTFC income concept) | MEDIUM-HIGH; both directions (PA down, MD/VA credits up) |
| 4 | Elective deferrals (401k/403b/457) | 2 | 1 — PA (largest single PA base gap; shared by TAXSIM/PE so invisible to cross-model) | NJ (taxes 403(b)/457 but NOT 401(k) — different sign per plan type) | MEDIUM; concentrated, one-sided (understates the PA base) |
| 5 | Dependent detail (>3 slots, student 19-23, disabled, tenure) | 2 | 3 — MN (CTC no child limit; M1CWFC older children) MD (disabled/65+ deps) IN (first-year dep) | — | LOW-MEDIUM; recurring machinery friction |
| 6 | Filer disability status | 2 | 2+ — MD (under-65 disabled pension excl) MN (M1R) + homestead disabled doors with #1 | — | LOW |
| 7 | Interest composition (US-obligation / own-state muni shares) | 2 | ALL broad-IIT states (sub_us_int flagged everywhere, subtracted nowhere; 75% own-state muni convention) | all | LOW per record, universal; current treatment overtaxes (no US-int sub) |
| 8 | Business-side detail (bonus dep/179 stacks, NOLs, SSTB) | 3 | MN ID WI MD OH | MT IA (conformity-era addbacks) | Document-permanently unless triage forces it |
| 9 | Expense niches (K-12, 529/HSA/MSA/ABLE, LTC, student loans) | 3 | 9+ — MN IL WI PA ID MD UT CT VA ND IN | many | Small aggregates; document |
| 10 | Election features (WI Act 15 retirement; sales-tax electors) | 3 | WI VA | — | Needs a min-liability election pass, not data |
| 11 | Credit take-up (all state credits assume 100%) | 3 | all credit states | all | Belongs with behavioral modules, not data imputation |

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

## Cross-cutting notes

- **County/locality of residence** is deliberately NOT here — it is the
  Phase 7 locality-weights workstream (§2.6: SOI county data; NYC and MD
  counties first). The MD research already banked the county rate
  matrices and PE's `gov/local/md` tree as sources.
- **Harness discipline:** every imputation ships with an off switch and
  its own validation note, because the record-level TAXSIM/PE harness
  compares LAW, not imputations — imputed inputs should be off (or
  matched into the external models' inputs) in cross-model runs.
- **Sequencing:** Tier 1 items depend on nothing but the ACS assets and
  should follow the Phase 1 weights production swap-in (shared cells,
  shared validation datasets). Tier 2 items are independent and can ride
  along with the states that need them (deferral add-back with NJ;
  dependent extensions with any MN triage push).
