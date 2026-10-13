---
title: "Data-gap implementation spec: Hawaii, Kentucky, Pennsylvania"
role: notes
workstream: state_tax
status: open
updated: 2026-10-13
sot: research/state_tax/plan.md
supersedes: []
superseded_by: null
---

# Data-gap implementation spec: Hawaii, Kentucky, Pennsylvania

Decision S43 (`research/decisions_log.md:59`) says a provision blocked on data
is encoded against a named optional overlay variable with a neutral default,
inert today and live when a producer publishes the variable. This note is the
executable spec for the three states' Tier 1 data-gap provisions -- the Hawaii
employer-funded pension exclusion, the Kentucky pre-1998 government pension
exemption and family-size credit, and the Pennsylvania Tax Forgiveness
eligibility income and elective-deferral compensation base -- plus the
known-difference rows the harness needs and the our-side law fixes the packets
left pending. It follows the recipe in
`research/state_tax/supplemental_inputs_interface.md:149-161` (§7).

Out of scope by S46(c) (`research/decisions_log.md:62`): the Hawaii
nonrefundable EITC carryforward stocks (crosswalk row
`research/state_tax/tax_data_gaps.csv:132`, overlay `prior_year_liab`) read a
prior-year state quantity and stay deferred to
`research/state_tax/notes/or_kicker_design_note.md`. Nothing below touches
them.

Conventions used throughout:

- "the resolver" = `ensure_st_data()`,
  `src/calc/state/do_state_taxes.R:159-207`: overlay column present ->
  value with default for NA and `has_<v>` TRUE where non-NA; native column
  present -> untouched, `has_<v>` TRUE; declared `tax_data_equivalent`
  present -> that column, `has_<v>` TRUE; otherwise default, `has_<v>` FALSE.
- "the schema" = `config/interfaces/supplemental_inputs_schema.yaml`; "the
  crosswalk" = `research/state_tax/tax_data_gaps.csv`; "the law schema" =
  `config/scenarios/tax_law_state/params_schema.yaml`.
- Worksheet cases are written for `run_case()` in
  `src/tests/state/test_state_calc.R:56-82`; an overlay-live case sets the
  bare variable name on the unit (the resolver's branch 2 treats it as a
  native column, `has_<v>` TRUE), which is what the invariance test's
  `moved` check does with `sup.interest_us_share`
  (`src/tests/state/test_supplemental_inputs_reader.R:258-262`).
- Harness record columns available to predicates are the headers of
  `research/state_tax/cross_model/results/raw/policyengine_2024.csv` and
  `taxsim_2019.csv` (`id ... filing_status, agi, age1, age2, gross_ss, n_dep,
  ui, txbl_int, ei1, ei2, txbl_pens_dist, txbl_ira_dist, other_inc, alimony,
  itemizing, n_dep_ge18, n_dep_ge65, care_exp, kg_st, kg_lt, ...`; built at
  `src/tests/state/test_state_cross_model.R:967-996`). There is no
  `wages1/2`; earned income is `ei1`/`ei2`, and wages = `ei1 - se1` where
  `se1` is present.

## 0. Summary of what the parent executes

| # | State | Provision | Overlay variables (schema) | New law params (law schema) | Stage change | Gate |
|---|---|---|---|---|---|---|
| 1 | HI | Employer-funded pension exclusion (N-11 line 13) | `pension_public` (exists, `schema:90-97`); NEW `pension_er_funded_share` (dbl, default 1) | `st_agi.pension_sub_er_funded_only` (int, 0) | `src/calc/state/st_agi.R:644-648` | none (defaults 0 and 1 reproduce the full subtraction) |
| 2 | KY | Pre-1998 government-service pension exemption (Schedule P Part I) | `pension_public` (exists); NEW `pension_public_pre1998_share` (dbl, default 0) | `st_agi.pension_excl_public_grandfather` (int, 0) | `st_agi.R:556-575, 623, 853-858` | none |
| 3 | KY | Family-size credit: qualifying-children count (KRS 141.066(1)(b),(e)) | none (dep ages already on the unit) | `st_credits.family_credit_child_max_age` (dbl, .inf) | `src/calc/state/st_credits_household.R:91` | n/a (law fix) |
| 4 | KY | Family-size credit: married-separate living together -> combined MGI, spouse in family size (KRS 141.066(1)(c),(4)) | NEW `spouse_agi_mfs` (dbl, default 0); `living_apart` (exists, `schema:164-170`) | `st_credits.family_credit_mfs_combined` (int, 0) | `st_credits_household.R:91-97` | **`has_spouse_agi_mfs`** -- zero is a real value and the spouse counts in family size even with no income |
| 5 | HI | Food/excise credit: married-separate combined FAGI (N-311 lines 4-6) | `spouse_agi_mfs` | `st_credits.percap_add_spouse_mfs` (int, 0) | `st_credits_household.R:257-263` | none (adding 0 is inert) |
| 6 | PA | Compensation includes elective deferrals (72 P.S. 7301(d); W-2 box 16 vs box 1) | `elective_deferral1/2` (exist, `schema:174-191`, `tax_data_equivalent: trad_contr_er1/2`) | `st_agi.ob_comp_add_deferrals` (int, 0) | `st_agi.R:345` | none (the Tax-Data columns are all zero) |
| 7 | PA | Tax Forgiveness eligibility income: nontaxable support (Schedule SP Part III lines 4, 5, 7, 9, 10) | NEW `nontax_support_inc` (dbl, default 0) -- NOT `hh_resources_inc` | `st_credits.forgive_add_nontax_support` (dbl, 0) | `src/calc/state/st_credits_earned.R:471-474` | none |
| 8 | PA | Tax Forgiveness: married-separate joint eligibility income (72 P.S. 7304(d)(1); SP instructions) | `spouse_agi_mfs` | `st_credits.forgive_add_spouse_mfs` (int, 0) | `st_credits_earned.R:471-474` | none (adding 0 is inert; Table 2 base already applies to status 3 via `pa/credits.yaml:27-32`) |

Our-side fixes that need no data (section 3): the KY family-size table
vintages (one row is a transcription typo, the "2017" vintage is the TY2018
table, seven years are missing); the KY qualifying-child count (row 3 above);
Hawaii in `PE_IRA_SEPARATE_STATES`; and the stale reference strings listed
per state.

## 1. Hawaii

### 1.1 Law

HRS 235-7(a)(2)-(3) exclude from Hawaii gross income the benefits of the
state Employees' Retirement System and comparable public retirement systems,
and "compensation received in the form of a pension for past services". The
operative instruction is Form N-11 line 13 ("Pensions taxed federally but not
taxed by Hawaii"). From the 2022 N-11 instructions, p.13 (NBER mirror
`historical_state_tax_forms/HI/2022/n11ins.pdf`, the same section in every
booklet 2017-2025 per `config/scenarios/tax_law_state/baseline/hi/agi.yaml:58`):

- Excluded: "(1) Pension or annuity distributions from a public (i.e.,
  government) retirement system (e.g., federal civil service annuity, military
  pension, state or county retirement system), unless voluntary contributions
  were made by an employee under an elective right"; "(2) Distributions from
  a private employer pension plan received upon retirement ... if the employee
  did not contribute to the pension plan"; "(3) Distributions from a pension
  plan at age 70-1/2 that are made to comply with the federal mandatory payout
  rule".
- Partly taxed: "Distributions from a private employer pension plan received
  upon retirement are partially taxed by Hawaii if the employee contributed to
  the pension plan" (Schedule J apportions).
- Taxable: deferred compensation plans "in which the employee has a choice of
  whether to contribute money into the plan or take that amount in cash"
  (401(k), SARSEP, Thrift Savings Plan, section 457); early distributions
  subject to the federal 10% penalty; and annuity plans and "Individual
  Retirement Accounts (IRAs) that are not funded through a SEP plan" are
  "considered to be your own investments" and "may be fully or partly
  taxable" (rollover IRAs keep the source plan's character).

So the excluded amount is: all public-system pension income (net of any
elective employee share) plus the employer-funded share of private-plan
distributions. Today `hi/agi.yaml:55-58` sets `pension_sub_share: 1` and the
whole `txbl_pens_dist` pool is subtracted through the CT share path at
`st_agi.R:644-648`; `hi/agi.yaml:47-54` records this as the Tier 1
approximation, and the crosswalk carries it as row
`tax_data_gaps.csv:134` (materiality high, direction undertax).

### 1.2 Overlay variables

`pension_public` already exists (`schema:90-97`: unit, dbl, default 0,
split halve, "federal, state and local government pension distributions
within txbl_pens_dist"). Add one variable:

```yaml
# --- Pension source and military (item 2), continued
pension_er_funded_share:
  key: unit
  type: dbl
  default: 1
  split: whole
  shadow: forbid
  units: share of (txbl_pens_dist - pension_public)
  description: employer-funded (non-contributory, non-elective) share of private-plan pension distributions; 1 - share is the elective-deferral/employee-contribution share (HI N-11 line 13, Schedule J)
```

Why a share with default 1 rather than a dollar amount with default 0: the
provision's inert state is "subtract everything", so a dollar variable would
need a `has_` gate (zero is a real value: a pure 401(k) retiree has an
employer-funded amount of exactly 0). A share whose default is 1 reproduces
today's output with no gate, and a producer (SOI 1099-R plan-type shares, CPS
ASEC class of worker, the `value.db/value.dc` anchor the crosswalk names)
naturally publishes a share by age/industry cell. Record both designs in the
crosswalk row; the share is the one to ship.

Place it in the schema immediately after `pension_railroad`
(`schema:106-113`), inside the "Pension source and military (item 2)" block.
Add the manifest-contract line for it in
`research/state_tax/supplemental_inputs_interface.md` §2/§8 per §7 step 2.

### 1.3 Law parameter and formula

Law schema, next to `st_agi.pension_sub_share` (`params_schema.yaml:138`):

```yaml
  st_agi.pension_sub_share: 0
  st_agi.ira_sub_share: 0
  # 1 = the share applies to pension_public in full plus pension_er_funded_share
  #     of the private remainder, not to the whole txbl_pens_dist pool (HI N-11
  #     line 13: public systems and non-contributory employer plans excluded,
  #     elective deferrals and employee shares taxed). Reads the pension_public
  #     and pension_er_funded_share optional data columns (S43)
  st_agi.pension_sub_er_funded_only: 0
```

Stage change at `st_agi.R:644-648`, replacing the `st_sub_retire_share`
expression:

```r
      pens_share_pool = if_else(
        st_agi.pension_sub_er_funded_only == 1,
        pmin(pmax(0, pension_public), pmax(0, txbl_pens_dist)) +
          pension_er_funded_share *
          pmax(0, txbl_pens_dist - pmax(0, pension_public)),
        pmax(0, txbl_pens_dist)
      ),
      st_sub_retire_share = retire_factor *
        (st_agi.pension_sub_share * pens_share_pool +
         st_agi.ira_sub_share     * pmax(0, txbl_ira_dist)),
```

Add to `calc_st_agi` req_vars beside the interest shares
(`st_agi.R:47-48`):

```r
    'pension_public',          # (dbl) optional data; schema default 0 (government pension within txbl_pens_dist)
    'pension_er_funded_share', # (dbl) optional data; schema default 1 (employer-funded share of the private remainder)
```

and `'st_agi.pension_sub_er_funded_only'` to the law req_vars after
`'st_agi.pension_sub_share'` (`st_agi.R:138`). Update the function header
comment at `st_agi.R:25-26` ("government vs private pension split
unobserved") to say it is now read from the overlay.

`hi/agi.yaml`: add

```yaml
pension_sub_er_funded_only:
  value: 1
  reference: 'N-11 line 13 instructions (2022 booklet p.13; identical 2017-2025): public retirement system distributions excluded in full "unless voluntary contributions were made by an employee under an elective right"; private employer plan distributions excluded "if the employee did not contribute"; 401(k)/SARSEP/TSP/457 deferred compensation and IRAs taxable; HRS 235-7(a)(2)-(3). Reads pension_public and pension_er_funded_share (S43): inert at the defaults, which reproduce the pre-2026-10-13 full subtraction'
```

and rewrite the `pension_sub_share` comment block (`hi/agi.yaml:47-54`) so it
no longer claims "TAXSIM and PolicyEngine face the same input and make the
same call" (see §1.5 and §3.1).

Inertness: `pension_public` default 0 and `pension_er_funded_share` default 1
give `pens_share_pool = txbl_pens_dist`, bit-identical to today. The
invariance test (`test_supplemental_inputs_reader.R:200-268`) covers it once
HI is added to its state list or the parent trusts the algebra; add HI to the
`states` vector at `test_supplemental_inputs_reader.R:212` so the test
exercises a `pension_sub_share` state.

Split: `pension_public` is `split: halve` (already in the schema);
`pension_er_funded_share` is a share, `split: whole`. Hawaii has no
married-separate election so neither is exercised there.

### 1.4 Worksheet cases

Base: HI-2 (`test_state_calc.R:3767-3779`): TY2017 MFJ ages 66/64, wages
40,000, `txbl_pens_dist` 30,000, taxable SS 10,000, FAGI 80,000; standard
deduction 4,400, exemptions 3 x 1,144 = 3,432; 2017 joint brackets
`hi/ord.yaml:56` with rates `hi/ord.yaml:42`.

- **HI-2 (inert, unchanged):** no overlay -> subtract 30,000 + 10,000 -> HI
  AGI 40,000 -> TI 32,168 -> 1,363.20 + 6.8% x 3,368 = **1,592.22**.
- **HI-9a (overlay live, mixed source):** add `pension_public = 10000,
  pension_er_funded_share = 0.5`. Excluded pension = 10,000 + 0.5 x (30,000 -
  10,000) = 20,000; HI AGI = 80,000 - 20,000 - 10,000 = 50,000; TI = 50,000 -
  4,400 - 3,432 = 42,168. Tax: base at 38,400 = 1,363.20 + 6.8% x 9,600 =
  2,016.00; plus 7.2% x 3,768 = 271.30 -> **liab_st_iit 2,287.30**,
  `st_agi 50000`, `st_subtractions 30000`.
- **HI-9b (overlay live, all public):** `pension_public = 30000,
  pension_er_funded_share = 0` -> excluded 30,000 + 0 x 0 = 30,000 -> identical
  to HI-2 (1,592.22). This pins that a public pension is excluded regardless
  of the share.
- **HI-9c (overlay live, pure 401(k)):** `pension_public = 0,
  pension_er_funded_share = 0` -> excluded 0 -> HI AGI 70,000, TI 62,168 ->
  base at 48,000 = 2,016.00 + 7.2% x 9,600 = 2,707.20; plus 7.6% x 14,168 =
  1,076.77 -> **3,783.97**.

### 1.5 What the external models do

- **PolicyEngine** subtracts the whole of `taxable_pension_income` on the
  Hawaii return
  (`policyengine_us/parameters/gov/states/hi/tax/income/subtractions/subtractions.yaml`,
  `taxable_pension_income # Line 13` in both the 2021 and 2025 lists;
  `variables/gov/states/hi/tax/income/hi_subtractions.py` sums that list). It
  has no source split. So PE equals our inert encoding on pensions.
- **The harness hands PE Hawaii's IRA distributions as pension income.**
  `src/tests/state/test_state_cross_model.R:337` sets
  `PE_IRA_SEPARATE_STATES = c('MD', 'RI')`, and lines 376-378 fold
  `txbl_ira_dist` into `pension_income` for every other state. For Hawaii that
  gives PE an exclusion the form denies (IRAs are "your own investments",
  taxable; `hi/agi.yaml:55` "IRA distributions stay taxable" is our encoding).
  This is an our-side harness fix, not a known difference: see §3.1.
- **TAXSIM** applies a partial pension exclusion of its own. The 2026-08-21
  residual decomposition (`research/source_packets/hi.md:187-205`, class C,
  31.9% of TY2019 mismatches) found TAXSIM subtracting a median $10,447 where
  we subtract $30,574, "not gross Social Security nor 85% of it", on records
  84% of which have SS. TAXSIM's state code is a WASM binary
  (`src/tests/state/cross_model/README.md:44`), so the rule is not readable;
  it has to be probed synthetically (single age 70, pension 30,000, no other
  income, TY2019; then pension 10,000/50,000 to see whether the subtraction is
  a share or a cap). Pattern: `output/probe/vt_taxsim_probe.R`.

### 1.6 Known-difference rows (HI)

Columns: `state,model,year_min,year_max,category,description,expected_direction,expected_magnitude,action,predicate,source`.

1. **HI, taxsim, 2017-2020, data-proxy, annotate** (promote to `exclude`
   once the probe in §1.5 identifies TAXSIM's rule). Description: "Pension
   source split. We subtract all of txbl_pens_dist (pension_sub_share = 1,
   the Tier 1 approximation; HI N-11 line 13 taxes elective-deferral and
   employee-contribution portions); TAXSIM applies a partial exclusion of its
   own on the same pooled input (2026-08-21 class C: median TAXSIM
   subtraction $10,447 vs ours $30,574; 31.9% of TY2019 mismatches). Neither
   model observes the split; the overlay encoding (S43) is inert in the
   harness because no producer is pinned." Direction: `under` (ours lower).
   Magnitude: "state rate x the elective/employee share of pensions; median
   -$1,044 among class C records". Predicate: `txbl_pens_dist > 0`. Source:
   "hi.md residual decomposition 2026-08-21; this note §1.5". Why
   defensible: the predicate names exactly the population whose input both
   models lack; it is an `annotate` until the TAXSIM rule is known, because
   excluding every pension record would also hide the class D residual the
   packet says needs a synthetic probe (`hi.md:214-222`).
2. **HI, policyengine, 2021-2025 -- NO row.** PE's full subtraction equals
   ours; a row would be wrong. The IRA fold is fixed in the harness (§3.1).
   If the parent wants an interim row until that one-line fix lands: HI,
   policyengine, 2021-2025, structural (harness), "PE receives IRA
   distributions as pension income and excludes them; Hawaii taxes IRAs",
   direction `under` (PE lower), magnitude "HI marginal rate x
   txbl_ira_dist", action `exclude`, predicate `txbl_ira_dist > 0`. Delete
   the row when `PE_IRA_SEPARATE_STATES` includes HI.
3. The existing rows 89 (`known_differences.csv:89`, TAXSIM itemized-base
   crosswalk exposure), 128 (PE deducts Hawaii's own income tax) and 129 (PE
   nets the Act 115 rebate into TY2021) stay as they are.

### 1.7 HI food/excise credit, married-separate combined AGI (row 5 of §0)

N-311 lines 4-6 combine both spouses' federal AGI for the gate and band
lookup when filing separately (`hi/credits.yaml:71-72` documents it; crosswalk
`tax_data_gaps.csv:122`, de minimis). The per-person credit's table income is
`percap_table_income` at `st_credits_household.R:257-259`. Add the law
parameter `st_credits.percap_add_spouse_mfs: 0` (law schema, beside
`percap_add_hh_resources` at `params_schema.yaml:812`) and change line 259 to
`st_income_base(...) + (tax_unit$filing_status == 3) *
tax_unit$st_credits.percap_add_spouse_mfs * pmax(0, spouse_agi_mfs)`; HI sets
it to 1 with reference "N-311 lines 4-6 (2017-2025): married filing
separately enters the spouse's federal AGI; reads spouse_agi_mfs (S43)".
Inert at default 0. The variable `spouse_agi_mfs` is specified in §2.3.
Also list `'spouse_agi_mfs'` in the `calc_st_credits` req_vars next to
`'hh_resources_inc'` (`src/calc/state/st_credits.R:140`).

## 2. Kentucky

### 2.1 Pension exclusion: pre-1998 government service (row 2 of §0)

**Law.** KRS 141.019(1)(g) (the Schedule P statutory cite,
`policyengine_us/parameters/gov/states/ky/tax/income/exclusions/pension_income/threshold.yaml`
references it) and Schedule P (2025, `revenue.ky.gov/Forms/Schedule P
(2025).pdf`):

- Part I, line 1: "the amount of federal, Kentucky state and Kentucky local
  government pension income attributable to service credit earned before
  January 1, 1998, and supplemental (Tier 2) U.S. Railroad Retirement Board
  benefits" is excluded in full. Line 1(a) if retired before 1998-01-01
  (100%); line 1(b) otherwise: "Multiply the taxable pension by the exempt
  percentage" (months of service before 1998 / total months, computed once
  in the year of retirement). "Do Not Include Income From Deferred
  Compensation Plans" in Part I.
- Part II, line 2: all other taxable retirement income -- "pensions,
  annuities, IRA accounts, 401(k) and similar deferred compensation plans"
  -- plus the non-exempt remainder of a government pension.
- Part III: line 3 = lesser of line 2 or $31,110 ($41,110 through TY2017,
  `ky/agi.yaml:27-36`); line 4 = line 1(c) + line 3. "This exclusion is for
  each taxpayer. A taxpayer and spouse must compute and claim their own
  exclusion, regardless of filing status."

Today `ky/agi.yaml:27-39` applies only the per-person cap to everything
(`pension_excl_under65/65plus`, `pension_excl_incl_ira` default 1 at
`params_schema.yaml:52`, so IRAs are in the pool as Schedule P says).
Crosswalk row `tax_data_gaps.csv:207` (materiality high, direction overtax).

**Overlay variables.** `pension_public` (exists) plus one new:

```yaml
pension_public_pre1998_share:
  key: unit
  type: dbl
  default: 0
  split: whole
  shadow: forbid
  units: share of pension_public
  description: share of government pension income attributable to service credit earned before 1998-01-01 (KY Schedule P exempt percentage; 1 if retired before 1998)
```

Default 0 is inert: with no pre-1998 share the government pension simply
stays in the capped pool, which is today's treatment. No `has_` gate.
Producer: Census of Governments / CPS ASEC class of worker for
`pension_public`; the share is a retirement-cohort function (anyone retiring
from government service after roughly 2030 has share 0, so it decays over
the horizon -- a producer should publish it by birth cohort, and the note in
the crosswalk should say so).

Per-person detail: KY computes the exclusion per taxpayer. The calculator
already sums per-spouse caps (`st_agi.R:272-280`, `cap1 + cap2` at 567-571)
against a unit-level pool; the `pension_person1/2` overlays
(`schema:132-147`) would tighten this but are not needed for inertness.
Document as the existing per-spouse approximation.

**Law parameter.** Law schema, after `st_agi.pension_excl_incl_ira`
(`params_schema.yaml:52`):

```yaml
  # 1 = pension_public x pension_public_pre1998_share is excluded in full
  #     outside the per-person cap and the remainder of the government
  #     pension joins the capped pool (KY Schedule P Parts I-III; KRS
  #     141.019(1)(g)). Reads the two optional data columns (S43)
  st_agi.pension_excl_public_grandfather: 0
```

**Stage change** in `st_agi.R`:

- line 556-557 (`pens_inc`): 
  ```r
      st_sub_pens_grand = st_agi.pension_excl_public_grandfather *
                          pmin(pmax(0, pension_public), pmax(0, txbl_pens_dist)) *
                          pmin(1, pmax(0, pension_public_pre1998_share)),
      pens_inc  = txbl_pens_dist +
                  st_agi.pension_excl_incl_ira * txbl_ira_dist -
                  st_sub_pens_grand,
  ```
- line 623: `st_sub_pens = pmax(st_sub_pens_raw, st_senior_std) + st_sub_pens_grand,`
  (the grandfathered amount is outside the cap and outside the ME/MT
  phase-outs at 587-593, which run on `st_sub_pens_raw` only).
- `st_subtractions` at 853-858 needs no change (it already adds
  `st_sub_pens`); `st_sub_pens` is a return variable (`st_agi.R:9`), so the
  harness column `st_sub_pens` carries the total.
- req_vars: `'pension_public'`, `'pension_public_pre1998_share'` beside the
  interest shares (`st_agi.R:47-48`); `'st_agi.pension_excl_public_grandfather'`
  after `'st_agi.pension_excl_incl_ira'` (`st_agi.R:157`).

`ky/agi.yaml` addition:

```yaml
pension_excl_public_grandfather:
  value: 1
  reference: 'KRS 141.019(1)(g); Schedule P (2025) Part I lines 1(a)-(b): federal, Kentucky state and Kentucky local government pension income attributable to service credit earned before January 1, 1998 (and Tier 2 railroad benefits) is excluded in full; the remainder joins Part II and takes the lesser of itself or $31,110 (Part III). Reads pension_public and pension_public_pre1998_share (S43); inert at the defaults'
```

and change the `pension_excl_min_age` reference at `ky/agi.yaml:39` from
"subject to source-specific exceptions" to name the grandfather parameter.

Tier 2 railroad benefits (`pension_railroad`, `schema:106-113`) are also
fully exempt in Part I; the same parameter can add
`pmin(pension_railroad, txbl_pens_dist)` to `st_sub_pens_grand` if the parent
wants it in the same change -- `pension_railroad` is default 0, so it is
inert. Record as optional.

**Worksheet cases** (base KY-8, `test_state_calc.R:536-541`: TY2018 single,
AGI 60,000 of which pension 40,000; standard deduction 2,530; rate 5%):

- **KY-8 (inert, unchanged):** exclusion min(40,000, 31,110) = 31,110 ->
  `st_agi 28890`, liab (28,890 - 2,530) x 5% = **1,318.00**.
- **KY-12a (overlay live):** `pension_public = 40000,
  pension_public_pre1998_share = 0.25` -> Part I exempt 10,000; Part II
  30,000 -> min(30,000, 31,110) = 30,000; total 40,000 -> `st_agi 20000`,
  `st_sub_pens 40000`, liab (20,000 - 2,530) x 5% = **873.50**.
- **KY-12b (overlay live, post-1998 retiree):** `pension_public = 40000,
  pension_public_pre1998_share = 0` -> exempt 0; Part II 40,000 -> 31,110;
  identical to KY-8 (1,318.00). Pins that a government pension with no
  pre-1998 service gets only the cap.
- **KY-12c (overlay live, with IRA):** add `txbl_ira_dist = 5000` (AGI
  65,000) to KY-12a -> Part II 35,000 -> 31,110; total 41,110 -> `st_agi
  23890`, liab **1,068.00**.

**External models.** PolicyEngine implements exactly Schedule P
(`variables/gov/states/ky/tax/income/exclusions/pension_income/ky_pension_income_exclusion.py`:
`exempt_amount = exemption_eligible * exemption_percentage * pension_income`;
`base = min_(other_income, p.threshold)`), but `exemption_eligible` adds the
inputs `retired_from_federal_government`, `retired_from_ky_government`,
`railroad_benefits`, none of which the harness driver passes
(`src/tests/state/cross_model/pe_state_tax.py:61-83` has no such column), so
in the harness PE's exemption is always 0 and it computes the cap only --
the same as our inert encoding. KY is not in `PE_IRA_SEPARATE_STATES`, so
IRAs are folded into `pension_income` and capped, matching Schedule P Part
II (PE's `other_retirement_income_sources.yaml` lists
`taxable_ira_distributions` and `taxable_401k_distributions`). TAXSIM: the
2026-08-11 WASM probes (`known_differences.csv:25,27`) touched the standard
deduction, not pensions; KY-8's cap is the only pension test. **No new KD
row**: both external models and our inert encoding agree on the cap-only
treatment, and the crosswalk row 207 already records the source gap. Keep
the existing packet sentence (`ky.md:51`) and point it at this spec.

### 2.2 Family-size credit: the count rule (row 3 of §0) -- law fix

**Law.** KRS 141.066 (text from the Legislative Research Commission PDF at
`apps.legislature.ky.gov/law/statutes/statute.aspx?id=49188`, effective
2019-06-27):

- (1)(b) "Qualifying dependent" means a qualifying child as defined in IRC
  152(c), including a child living in the household who cannot be claimed
  because of 152(e)(2)/(4).
- (1)(c) "Qualifying individual" = single, or married filing separately "if
  during the taxable year the individual's spouse is not a member of the
  household".
- (1)(e) Threshold amount: family unit size 1 = individual with no qualifying
  children; 2 = individual with one, or couple with none; 3 = individual
  with two, or couple with one; 4 = individual with three or more, or couple
  with two or more.
- The 2025 Form 740 instructions (p.23, NBER mirror) repeat it: "Family
  Size -- Consists of yourself, your spouse if married and living in the same
  household and qualifying children", with the 152(c) four tests
  (relationship, residence, "under the age of 19 ... or under the age of 24
  if a full-time student ... or be permanently and totally disabled",
  support).

**Today** `st_credits_household.R:91` counts every dependent:
`family_size = pmin(4L, 1L + (filing_status == 2) + n_dep)`. A dependent
parent or other qualifying relative inflates family size and so the
threshold. (PolicyEngine makes a different error: `tax_unit_size`, every
member of the unit --
`variables/gov/states/ky/tax/income/credits/family_size_credit/ky_family_size_tax_credit_rate.py`.)

**Fix.** Law parameter `st_credits.family_credit_child_max_age: .inf` (law
schema, after `family_credit_income_base` at `params_schema.yaml:679`;
`.inf` keeps today's `n_dep` behaviour for any other state). Move the
`n_dep_under()` helper (`st_credits_household.R:268-272`, defined inside the
per-person credit block) above line 91 and write

```r
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2) +
                       n_dep_under(tax_unit$st_credits.family_credit_child_max_age))
```

KY sets `family_credit_child_max_age: 24` -- the 152(c) age test is "under
19, or under 24 if a full-time student"; 24 is the student proxy already used
for the NM child credit (`config/scenarios/tax_law_state/baseline/nm/credits.yaml:43-45`
uses 23 as `ctc_max_child_age`, i.e. under 24; note the helper counts
`dep_ages < max_age`, so the value here is 24). Permanently disabled
children of any age are missed; `dep_disabled_n` (`schema:212-219`) could
add them, but a disabled child under 24 would double count, so leave it
documented. Reference string: "KRS 141.066(1)(b),(e); 2025 Form 740
instructions p.23: qualifying children under IRC 152(c) only (under 19, or
under 24 if a student); dependents outside the age slots are not qualifying
children; 24 proxies the student test (NM precedent)".

**Worksheet cases** (base KY-1, `test_state_calc.R:477-482`: TY2025 single,
AGI 16,000, standard deduction 3,270, rate 4%, tax 509.20):

- **KY-1 (unchanged):** no dependents -> size 1 -> 16,000 in the 15,651-16,276
  band -> 90% -> credit 458.28, liab **50.92**.
- **KY-13a (non-child dependent):** `n_dep = 1, dep_age1 = 72`. Today: size 2
  -> 16,000 <= 21,150 -> 100% -> liab 0. After: size 1 -> **50.92**.
- **KY-13b (qualifying child):** `n_dep = 1, dep_age1 = 10` -> size 2 ->
  100% -> liab **0** (unchanged by the fix).
- **KY-13c (student-age child):** `dep_age1 = 22` -> counted (< 24) -> size 2
  -> liab 0.

**Known-difference row** (both models count all dependents / all members):
**KY, both, 2017-2025, external-model-concept, annotate** -- "Family size for
the KRS 141.066 credit counts qualifying children (IRC 152(c)) only; PE uses
tax_unit_size (every member) and TAXSIM its dependent count, so a unit with a
dependent who is not a qualifying child (a parent, a sibling over 23) gets a
larger family size and a higher threshold in the external model"; direction
`under` (external liability lower); magnitude "up to the whole credit, only
where MGI is below 133% of the larger threshold"; predicate
`n_dep_ge65 > 0` for an `exclude` variant (a dependent 65+ is never a
qualifying child), `n_dep_ge18 > 0` for the `annotate` variant (ages 18-23
may be students). Better: add `n_dep_ge24` beside `n_dep_ge18` at
`test_state_cross_model.R:967-971` and use `n_dep_ge24 > 0` as the exclude
predicate; source "this note §2.2; KRS 141.066(1)(b)".

### 2.3 Family-size credit: married-separate living together (row 4 of §0)

**Law.** KRS 141.066(4): a qualifying married couple "living together,
whether filing separate returns or filing separately on a combined return"
computes the credit on "their combined modified gross income", and (1)(e)
counts the spouse in the family unit. The 2025 MGI worksheet lines (b) and
(g) (740 instructions p.23): "If married filing separate returns and living
in the same household, enter your spouse's federal adjusted gross income
... If zero or less, enter zero." Crosswalk row `tax_data_gaps.csv:203`
(overlay `other:spouse_income_mfs`, low); packet `ky.md:49`.

**Overlay variable** (new; also serves §1.7 HI and §4.4 PA, and replaces
the crosswalk placeholder `other:spouse_income_mfs` in rows 122, 203, 501):

```yaml
# --- Per-person attribution (item 12), continued
spouse_agi_mfs:
  key: unit
  type: dbl
  default: 0
  split: whole
  shadow: forbid
  units: dollars
  description: for a married-filing-separately unit, the spouse's own federal AGI (the spouse files a separate return that is not this record); NA/absent for every other status
```

Federal AGI is the one concept a producer can publish uniformly (CPS ASEC
spouse income for MFS units, or a Tax-Data fill). KY's worksheet asks for
exactly that. HI's N-311 asks for the spouse's federal AGI. PA's Schedule SP
asks for the spouse's eligibility income, for which federal AGI is a proxy
(over-includes spouse SS/UI/retirement that SP excludes, under-includes
nontaxable support) -- document that in `pa/credits.yaml`.

**Gate.** `has_spouse_agi_mfs`: the spouse counts in family size even if the
spouse's AGI is 0, so a default of 0 cannot mean "no spouse". The rule
switches on only when the producer published a value for the record.
`living_apart` (`schema:164-170`, lgl, default FALSE) turns it off for a
couple that did not share a household (KRS 141.066(1)(c)).

**Law parameter** `st_credits.family_credit_mfs_combined: 0` (law schema,
after `family_credit_child_max_age`). **Stage change** at
`st_credits_household.R:91-97`:

```r
  mfs_combined = tax_unit$st_credits.family_credit_mfs_combined == 1 &
                 tax_unit$filing_status == 3 & tax_unit$has_spouse_agi_mfs &
                 !coalesce(tax_unit$living_apart, FALSE)
  family_size = pmin(4L, 1L + (tax_unit$filing_status == 2 | mfs_combined) +
                       n_dep_under(tax_unit$st_credits.family_credit_child_max_age))
  family_income = pmax(0, st_income_base(tax_unit, tax_unit$st_credits.family_credit_income_base)) +
                  mfs_combined * pmax(0, tax_unit$spouse_agi_mfs)
```

(The `pmax(0, ...)` on the own base is worksheet line (a) "If zero or less,
enter zero"; it is a de minimis law fix bundled here -- it does not touch
the `st_income_base` enum 3 that UT also reads, `params_schema.yaml:679`.)
Add `'spouse_agi_mfs'`, `'has_spouse_agi_mfs'` and `'living_apart'` to the
`calc_st_credits` req_vars (`st_credits.R:140` block). KY sets
`family_credit_mfs_combined: 1`, reference "KRS 141.066(1)(c),(4); 2025 Form
740 MGI worksheet lines (b),(g): married filing separately and living in the
same household adds the spouse's federal AGI and counts the spouse in family
size; reads spouse_agi_mfs gated on has_spouse_agi_mfs and living_apart
(S43)".

**Interaction with the married-separate election.** `st_split_spouse_unit()`
(`src/calc/state/st_split.R:79-...`) builds filing-status-3 half-units from
joint records and copies every column, `has_` flags included. KY and PA do
not use `split_election`, but a state that does would see
`has_spouse_agi_mfs` TRUE on a half-unit whose spouse is the other half.
Have the builder reset `spouse_agi_mfs` to the schema default and
`has_spouse_agi_mfs` to FALSE on half-units (the split already knows both
columns and nets them itself). One line in the split-class loop, from the
schema.

**Worksheet cases** (TY2025, `filing_status = 3`, AGI 12,000; KY MFS
standard deduction 3,270, rate 4% -> tax 349.20; `exempt_credit_personal` is
0 from 2018, `ky/credits.yaml:25-29`):

- **KY-14a (inert):** no overlay -> size 1, MGI 12,000 <= 15,650 -> 100% ->
  liab **0**.
- **KY-14b (overlay live):** `spouse_agi_mfs = 12000` (`has_` TRUE),
  `living_apart = FALSE` -> size 2, MGI 24,000 in 23,689-24,534 -> 60% ->
  credit 209.52, liab **139.68**.
- **KY-14c (overlay live, apart):** `spouse_agi_mfs = 12000, living_apart =
  TRUE` -> identical to KY-14a (liab 0).
- **KY-14d (overlay live, spouse with no income):** `spouse_agi_mfs = 0`
  (`has_` TRUE) -> size 2, MGI 12,000 -> 100% -> liab 0 -- but a different
  band path from 14a (size 2 threshold 21,150), which is what the gate buys.

### 2.4 Modified gross income (existing enum, no change)

`family_credit_income_base: 3` (`params_schema.yaml:679`, "federal AGI plus
state additions") matches worksheet lines (a)+(c): the non-Kentucky
municipal-bond interest addition is `st_add_muni` (25% of `exempt_int` under
`interest_own_state_share`, `st_agi.R:495` region). Line (d), Form 4972
lump sums, is unobservable; PE adds `form_4972_lumpsum_distributions`
(`ky_modified_agi.py`) which the harness never sets. The existing annotate
row `known_differences.csv:28` covers this; narrow its description to
"lump-sum distributions and the spouse's income for MFS units living
together" once rows 3 and 4 land, and leave its predicate empty.

## 3. Our-side fixes that need no data

### 3.1 Hawaii in `PE_IRA_SEPARATE_STATES` (harness)

`src/tests/state/test_state_cross_model.R:337`:
`PE_IRA_SEPARATE_STATES = c('MD', 'RI')` -> `c('HI', 'MD', 'RI')`. Lines
367-375 explain the list as states whose law excludes IRAs from the pension
exclusion; Hawaii is one (N-11 line 13 instructions, 2022 p.13: IRAs not
funded through a SEP "are considered to be your own investments ...
distributions from these plans may be fully or partly taxable";
`hi/agi.yaml:55`). With the fold, PE excludes `txbl_ira_dist` through
`taxable_pension_income` (PE's `subtractions.yaml` reads only that
variable), running LOW on every HI IRA record; with IRAs passed as
`ira_dist` -> `taxable_ira_distributions` (`pe_state_tax.py:73`), PE taxes
them as Hawaii does. Expect the HI PE cells (0.786/0.767/0.878 after row 128,
`state_parameter_rollout.csv:15`) to move up on IRA records; record the
before/after in the tracker row. Also update the comment at lines 367-375 to
list HI, and extend row 129's "HI's PE verdict rests on 2022-2024" note if the
2022-2024 cells change.

### 3.2 Kentucky family-size credit table vintages

`config/scenarios/tax_law_state/baseline/ky/credit_tables.csv` carries two
vintages labelled 2017 and 2025 (`ky/credits.yaml:6-8`: "from the Schedule
ITC Table C for 2018 and 2025"). Three problems:

1. **Line 80 is a transcription typo.** `family_size_tax_credit,KY,2025,0,4,40832,41695,0.2`
   should read `...,40832,41795,0.2`: the printed 2025 Schedule ITC Table C
   (NBER mirror `historical_state_tax_forms/KY/2025/Schedule ITC (2025).pdf`,
   p.2) shows "40,831 / 41,795" for the four-or-more 20% band, and
   32,150 x 1.30 = 41,795. Every other 2025 cell matches the form.
2. **The "2017" vintage is the TY2018 table.** Its 100% column (12,140 /
   16,460 / 20,780 / 25,100) is the 2018 HHS guideline; the 2017 guideline is
   12,060 / 16,240 / 20,420 / 24,600. `ky.md:53` records this as pending.
3. **TY2019-2024 and TY2026 are missing.** `state_credit_tables_for_year()`
   (`src/data/state_tax_law.R:177-200`) selects each table's most recent
   vintage on or before the simulation year, so TY2019-2024 run on the 2018
   bands (understating the credit as the guideline rises) and TY2026+ carries
   2025 forward (S45).

**Generator, not transcription.** KRS 141.066(1)(a) defines the federal
poverty level as the HHS guideline "available on June 30 of the taxable
year" -- the guideline published in January of that tax year -- and
141.066(3)(c) fixes the bands at 100 / 104 / 108 / 112 / 116 / 120 / 124 /
127 / 130 / 133 percent of the threshold (90% down to 10%), zero above 133%.
Band upper bounds are the percentage times the threshold, rounded half-up
to the dollar; lower bounds are the previous upper plus one (the CSV's
convention, which matches the printed "is over / is not over" columns with
whole-dollar MGI). Checked: this rule reproduces every cell of the printed
2018 and 2025 tables (the only diff being item 1). HHS guidelines, 48
contiguous states, first person and each additional person
(`aspe.hhs.gov/.../prior-hhs-poverty-guidelines-federal-register-references`):

| TY | 1 person | +each | size 2 | size 3 | size 4 | 133% size 1 .. size 4 |
|---|---|---|---|---|---|---|
| 2017 | 12,060 | 4,180 | 16,240 | 20,420 | 24,600 | 16,040 .. 32,718 |
| 2018 | 12,140 | 4,320 | 16,460 | 20,780 | 25,100 | 16,146 .. 33,383 |
| 2019 | 12,490 | 4,420 | 16,910 | 21,330 | 25,750 | 16,612 .. 34,248 |
| 2020 | 12,760 | 4,480 | 17,240 | 21,720 | 26,200 | 16,971 .. 34,846 |
| 2021 | 12,880 | 4,540 | 17,420 | 21,960 | 26,500 | 17,130 .. 35,245 |
| 2022 | 13,590 | 4,720 | 18,310 | 23,030 | 27,750 | 18,075 .. 36,908 |
| 2023 | 14,580 | 5,140 | 19,720 | 24,860 | 30,000 | 19,391 .. 39,900 |
| 2024 | 15,060 | 5,380 | 20,440 | 25,820 | 31,200 | 20,030 .. 41,496 |
| 2025 | 15,650 | 5,500 | 21,150 | 26,650 | 32,150 | 20,815 .. 42,760 |
| 2026 | 15,960 | 5,680 | 21,640 | 27,320 | 33,000 | 21,227 .. 43,890 |

Write all ten vintages (400 rows) into `ky/credit_tables.csv`, rename the
"2017" rows to 2018 and add true 2017 rows, and cite in `ky/credits.yaml`:
"KRS 141.066(1)(a),(3)(c); HHS poverty guidelines 2017-2026 (Federal
Register, January of each year); bands generated, verified cell-for-cell
against the printed 2018 and 2025 Schedule ITC Table C". The 2025 ITC and
the 2025 Form 740 instructions p.23 Chart A ("$15,650 / $21,150 / $26,650 /
$32,150") are the primary anchors; the 2018 ITC is already cited. For TY2027+
the table carries 2026 forward until the next guideline is transcribed (S45);
a cleaner follow-up is a `family_credit_fpl_*` parameter pair so the bands
index from the law file, but that is a new mechanism and not required here.
Update `ky.md:53` and tracker `ty2026_law` text for KY
(`state_parameter_rollout.csv:20`) to say TY2026 Table C is encoded.

Note the income-gap credit of KRS 141.066(3)(d) ($3-$69, TY2019-2020 only)
is not encoded anywhere; record it in `ky/credits.yaml`
`documented_not_modeled` (de minimis).

### 3.3 Stale reference strings

Hawaii:
- `hi/agi.yaml:52-53` and `hi.md:94-95`: "TAXSIM and PolicyEngine face the
  same input and make the same call, so cross-model comparisons are clean"
  -- contradicted by the packet's own class C finding (`hi.md:196-204`:
  TAXSIM applies a partial exclusion). Rewrite to: PE subtracts the pool in
  full; TAXSIM applies its own partial rule (probe pending, §1.5).
- `hi.md:158-159` ("the pension full-subtraction is SHARED (clean)"): same.
- `hi/agi.yaml:47-54` comment block: replace "DOCUMENTED APPROXIMATION" with
  the S43 overlay statement once §1.3 lands.

Kentucky:
- `ky/credits.yaml:6-8` ("two vintages 2017 and 2025 ... for 2018 and 2025"):
  rewrite per §3.2.
- `ky/agi.yaml:39` ("subject to source-specific exceptions"): name the
  grandfather parameter.
- `ky.md:51` and `ky.md:53`: close after §2.1 and §3.2.

Pennsylvania:
- `pa/agi.yaml:9-11` and `pa/agi.yaml:34` say elective deferrals are
  "unobserved in the PUF"; Tax-Data carries `trad_contr_er1/2` (all zero;
  `schema:172-173`, crosswalk `tax_data_gaps.csv:505`). Rewrite per §4.2.
- `pa/credits.yaml:87` (`cdctc_match` reference) still says "model applies
  the match to the limited federal credit [known-difference]" although
  `pa/credits.yaml:93` encodes `cdctc_fed_base: 1` (2026-10-07) and the
  header at lines 18-21 says the difference is resolved. Same stale sentence
  in `pa.md:100-102`.
- `pa/credits.yaml:11-14` (header) and `:44` (`forgive_base_married`: "MFS
  combined-spouse income unobserved; own income used"): rewrite per §4.3-4.4.
- `pa/ded.yaml:5-7` and `:28` ("all unobserved or unmapped in the PUF"):
  Tax-Data carries `hsa_contr` and `sl_int_ded` (crosswalk `tax_data_gaps.csv:499`),
  and the own-base adjustment `st_agi.ob_adj_hsa` already exists
  (`params_schema.yaml:189`). Not part of this batch (it is a deduction, not
  a data gap), but the string should say "observable, not yet encoded".

## 4. Pennsylvania

### 4.1 Law

- **Compensation.** 72 P.S. 7301(d): compensation "shall include any part of
  a distribution under a plan described in section 409A(d)(1) ... attributable
  to an elective deferral of income"; and employee contributions to
  401(k)/403(b)/457/thrift plans are PA-taxable compensation when made
  (2024 PA-40 instructions p.11, "Contributions"; p.10: "The amount in Box 16
  of your Form W-2 may be different from the amount in Box 1. PA law requires
  a PA employer to follow PA income tax rules to determine Box 16").
  Tax-Data wages are federal Box 1, which excludes the deferral
  (`pa.md:79-81`). Crosswalk `tax_data_gaps.csv:505` (materiality high,
  undertax).
- **Tax Forgiveness.** 72 P.S. 7304(d): $6,500 single / $13,000 joint
  "poverty income" limit plus $9,500 per dependent, forgiveness dropping 10
  points per $250 (or fraction) of excess, zero above $2,250 (confirmed on the
  FindLaw text). "Poverty income" (7301(o.2)) is "all moneys or property ...
  received of whatever nature and from whatever source derived" less a list
  of exclusions. Schedule SP Part III (2024 PA-40 instructions pp.38-39)
  operationalizes it: line 1 PA taxable income; 2 nontaxable interest,
  dividends and gains; 3 alimony; 4 insurance proceeds and inheritances (incl.
  1099-R code 4 and survivor annuities); 5 gifts, awards and prizes; 6 non-PA
  income (part-year); 7 nontaxable military income; 8 excluded residence
  gain; 9 nontaxable educational assistance; 10 cash received for personal
  use from outside the home (spousal support from outside the household,
  cash from parents or grown children, nontaxable cafeteria-plan payments,
  foster-care payments, government education grants). **Excluded** ("Do not
  include ... in Line 10", p.39, and the PIT Guide Tax Forgiveness chapter
  per `research/raw/pa_research_core.md:172-182`): Social Security and
  Railroad Retirement, retirement benefits after retiring, child support,
  workers' compensation, sick pay and disability benefits, damage awards,
  unemployment compensation, public assistance.
- **Married.** "If married, whether filing jointly or separately, you must
  determine your total (your Eligibility Income plus your spouse's
  Eligibility Income) joint Eligibility Income" and use Table 2 (p.37).
  "Separated" (living apart the last six months) uses Table 1.
- **Dependents.** "A dependent is a minor or adult child claimed as a
  dependent on a 2024 federal income tax return" -- child, grandchild or
  foster child of any age; "You cannot claim any other adult as a dependent"
  (p.38). 72 P.S. 7301(e.1): "a child who is the dependent of a claimant for
  purposes of section 151".

### 4.2 Elective deferrals in compensation (row 6 of §0)

**Overlay.** `elective_deferral1/2` already in the schema (`schema:174-191`,
person-keyed, `shadow: allow`, `tax_data_equivalent: trad_contr_er1/2`). The
resolver's branch 3 (`do_state_taxes.R:166-172` comment, `:198-202` code) fills
`elective_deferral1 = trad_contr_er1` with `has_` TRUE when the native column
is on the unit, so the provision reads the Tax-Data column directly -- all
zero today, live the day Tax-Data fills it, with no runscript opt-in. **This
also means §7 step 5 of the interface note ("in the harness the provision
stays off") does not hold for this variable**: the harness reads Tax-Data
natively, so when the fill lands the harness turns the provision on and both
external models (which take Box 1 wages) fall below us by 3.07% x deferrals.
Pre-register that known-difference row now (§4.6 item 3) with
`action: annotate` and a predicate that fires on nothing until the fill.

**Law parameter** (law schema, after `st_agi.ob_comp_share` at
`params_schema.yaml:161`):

```yaml
  # 1 = own-base compensation adds pre-tax elective deferrals (PA 72 P.S.
  #     7301(d): 401(k)/403(b)/457 deferrals are compensation when made; W-2
  #     box 16 vs box 1). Reads elective_deferral1/2 (S43; Tax-Data
  #     trad_contr_er1/2 through tax_data_equivalent)
  st_agi.ob_comp_add_deferrals: 0
```

**Stage change** `st_agi.R:345`:

```r
    ob_floor(st_agi.ob_comp_share *
             (wages1 + wages2 +
              st_agi.ob_comp_add_deferrals *
                (pmax(0, elective_deferral1) + pmax(0, elective_deferral2)))) +
```

req_vars: `'elective_deferral1'`, `'elective_deferral2'` after `'wages2'`
(`st_agi.R:55`), comment "(dbl) optional data; schema default 0; Tax-Data
trad_contr_er1/2 via tax_data_equivalent"; `'st_agi.ob_comp_add_deferrals'`
after `'st_agi.ob_comp_share'` (`st_agi.R:98`). `pa/agi.yaml` adds
`ob_comp_add_deferrals: 1` with reference "72 P.S. 7301(d) (elective
deferrals are compensation); 2024 PA-40 instructions p.10-11 (Box 16 follows
PA rules; contributions to 401(k)/403(b)/457 plans taxable when made); reads
elective_deferral1/2 (S43), which resolve to Tax-Data trad_contr_er1/2 --
all zero in every vintage checked, so inert until Tax-Data fills them
(imputation plan item 4)". Rewrite the header lines `pa/agi.yaml:9-11` and
the `ob_comp_share` reference at `:34`.

Because PA taxable income feeds eligibility income (`forgive_income_base:
2` = state AGI, `pa/credits.yaml:58-60`), the deferral add-back flows into
Schedule SP line 1 automatically, as the form has it.

NJ shares crosswalk item 4 (`state_data_imputation_plan.md` item 4 lists
"NJ PA") but NJ excludes 401(k) deferrals and taxes only 403(b)/457/SEP
deferrals; the parameter is a plain add-all switch, so NJ stays at 0 until
its own research pass.

**Worksheet cases** (base PA-4, `test_state_calc.R:1713-1719`: TY2024 single,
wages 7,000, tax 214.90, forgiveness 80%):

- **PA-4 (inert, unchanged):** `elective_deferral1` absent/0 -> liab
  214.90 x 0.2 = **42.98**.
- **PA-8a (overlay live):** `wages1 = 7000, elective_deferral1 = 1000` ->
  compensation 8,000 -> tax 245.60; eligibility income 8,000; excess over
  6,500 = 1,500 -> 6 steps -> 40% (Table 1: $8,000 column = .40) -> credit
  98.24, liab **147.36**; `st_agi 8000`.
- **PA-8b (joint):** `filing_status = 2, wages1 = 20000, wages2 = 10000,
  elective_deferral1 = 2000, elective_deferral2 = 500` -> 32,500 x 3.07% =
  **997.75**, no forgiveness (0 dependents, 32,500 > 15,250).

### 4.3 Tax Forgiveness eligibility income: the right variable (row 7 of §0)

The crosswalk (`tax_data_gaps.csv:500`) names `hh_resources_inc` as the
overlay for the SP nontaxable additions. **That is the wrong concept.**
`hh_resources_inc` is defined (`schema:194-201`) as "nontaxable household
income (SSI, TANF, VA benefits, child support, SNAP)" -- the OK/NM gross
household income measure (`ok/credits.yaml:112-114`,
`nm/credits.yaml:113-115`). Schedule SP explicitly excludes public
assistance, child support, unemployment compensation and Social Security
(§4.1). Adding `hh_resources_inc` would move exactly the units the form
holds harmless.

**New overlay variable** (schema, a new block "Nontaxable support income
(item 3, PA Schedule SP)"):

```yaml
nontax_support_inc:
  key: unit
  type: dbl
  default: 0
  split: halve
  shadow: forbid
  units: dollars
  description: nontaxable cash support received in the year that a poverty test counts -- gifts, inheritances and insurance proceeds, scholarships and educational assistance, nontaxable military pay, support from persons outside the household -- EXCLUDING Social Security, unemployment, public assistance, child support, workers' compensation and retirement distributions (PA Schedule SP Part III lines 4, 5, 7, 9, 10)
```

Producer: CPS ASEC (gifts/regular contributions from outside the household,
educational assistance, veterans' pay as the military-pay proxy) or ACS;
record in the crosswalk row 500 (`overlay_variable` -> `nontax_support_inc`)
and in the imputation plan item 3 text, which currently lists PA under the
household-resources item.

**Law parameter** (law schema, after `forgive_add_alimony` at
`params_schema.yaml:764`): `st_credits.forgive_add_nontax_support: 0   #
share of nontax_support_inc added`. **Stage change**
`st_credits_earned.R:471-474`:

```r
  forgive_income = st_income_base(
    tax_unit, tax_unit$st_credits.forgive_income_base
  ) + tax_unit$st_credits.forgive_add_exempt_int * tax_unit$exempt_int +
      tax_unit$st_credits.forgive_add_alimony * tax_unit$alimony +
      tax_unit$st_credits.forgive_add_nontax_support * pmax(0, tax_unit$nontax_support_inc) +
      (tax_unit$filing_status == 3) * tax_unit$st_credits.forgive_add_spouse_mfs *
        pmax(0, tax_unit$spouse_agi_mfs)
```

(the last term is §4.4). Add `'nontax_support_inc'` and `'spouse_agi_mfs'`
to the `calc_st_credits` req_vars (`st_credits.R:140`) and the two law
names to `st_credits_earned_req_vars` after `'st_credits.forgive_add_alimony'`
(`st_credits_earned.R:51`). `pa/credits.yaml` adds
`forgive_add_nontax_support: 1`, reference "PA-40 Schedule SP Part III lines
4 (insurance proceeds and inheritances), 5 (gifts, awards, prizes), 7
(nontaxable military income), 9 (nontaxable educational assistance), 10
(cash received for personal use from outside your home) -- 2024 instructions
pp.38-39, identical 2017-2025; 72 P.S. 7301(o.2). Reads nontax_support_inc
(S43). NOT hh_resources_inc: SP excludes Social Security, unemployment,
public assistance and child support". Rewrite the header
`pa/credits.yaml:11-14`. The existing `forgive_add_exempt_int: 0.75` plus the
25% already in the base equals the whole of `exempt_int`, which is what PE
does (below); no change.

Inert at default 0. Dependent-claimant linkage (crosswalk row 502,
`st_credits_earned.R:483` `dep_status != 1`) is unchanged.

**Worksheet cases** (base PA-5, `test_state_calc.R:1721-1729`: TY2024 MFJ,
2 dependents, wages 30,000, `exempt_int` 2,000; base 30,500, tax 936.35;
eligibility income 32,000 = Table 2 limit for 2 dependents):

- **PA-5 (inert, unchanged):** liab **0**.
- **PA-9a (overlay live):** add `nontax_support_inc = 1000` -> eligibility
  33,000; excess 1,000 -> 4 steps -> 60% (Table 2, 2 dependents, $33,000 =
  .60) -> credit 561.81, liab **374.54**.
- **PA-9b (overlay live, out of range):** `nontax_support_inc = 3000` ->
  eligibility 35,000; excess 3,000 > 2,250 -> 0% -> liab **936.35**.

### 4.4 Tax Forgiveness: married-separate joint eligibility income (row 8)

Crosswalk `tax_data_gaps.csv:501`; `pa/credits.yaml:44`. The Table 2 base
already applies to status 3 through the mapper (`pa/credits.yaml:27-32`);
only the income is missing. Law parameter `st_credits.forgive_add_spouse_mfs:
0` (int; law schema after `forgive_add_nontax_support`); stage change is the
last term in §4.3. PA sets 1, reference "2024 PA-40 instructions p.37: 'If
married, whether filing jointly or separately, you must determine your total
... joint Eligibility Income'; 72 P.S. 7304(d)(1) 'joint poverty income of
married claimants'. Reads spouse_agi_mfs (S43), the spouse's federal AGI as
a proxy for the spouse's eligibility income [over-includes spouse SS/UI/
retirement that SP excludes; under-includes spouse nontaxable support]".
Inert at default 0 (no gate: a spouse with zero AGI adds zero, and Table 2
is already in use). `living_apart` could switch status-3 units to Table 1
("Separated"); not required for inertness, note as optional.

**Worksheet case** (TY2024, `filing_status = 3`, `wages1 = 10000` -> tax
307.00; Table 2, 0 dependents: 100% at 13,000, 10% column 15,250):

- **PA-10a (inert):** eligibility 10,000 <= 13,000 -> 100% -> liab **0**.
- **PA-10b (overlay live):** `spouse_agi_mfs = 10000` -> eligibility 20,000
  > 15,250 -> 0% -> liab **307.00**.
- **PA-10c:** `spouse_agi_mfs = 4000` -> 14,000; excess 1,000 -> 4 steps ->
  60% -> credit 184.20, liab **122.80**.

### 4.5 What the external models do (PA)

- **PolicyEngine** eligibility income
  (`parameters/gov/states/pa/tax/income/forgiveness/eligibility_income_sources.yaml`)
  = `pa_total_taxable_income + alimony_income + tax_exempt_interest_income`;
  the other SP lines are commented out as unmodeled. That is our inert
  encoding exactly (`forgive_add_exempt_int 0.75` + 25% in the base =
  `exempt_int`; `forgive_add_alimony 1`). Forgiveness rate
  (`pa_tax_forgiveness_rate.py`): base doubled for JOINT or SEPARATE, so an
  MFS record is scored on own income with the married base -- also our inert
  treatment. Dependents: PE counts `is_qualifying_child_dependent` (age-
  limited) where the law counts any child dependent and we count `n_dep`
  (any dependent); three different counts, all within a few dollars of each
  other except for units with adult dependents.
- **PolicyEngine taxes pensions and IRAs for PA filers under 65.**
  `pa_nontaxable_pension_income.py` excludes `taxable_pension_income` only
  `where(retired, ...)`, and `is_retired` is "age >= 65"
  (`variables/.../is_retired.py`); `pa_nontaxable_retirement_distributions.py`
  excludes IRA/401(k) sources only at age >= 59.5 -- but the harness folds
  `txbl_ira_dist` into `pension_income` for PA (not in
  `PE_IRA_SEPARATE_STATES`), so IRAs take the age-65 path too. We exempt all
  distributions (`pa/agi.yaml:59-61`, `ob_retirement_share: 0`), the packet's
  documented approximation (`pa.md:82-84`). PE's convention is not law (the
  form exempts "payments ... after qualifying for retirement" at any plan age
  and early distributions only above basis), but it is a coded, verifiable
  rule and a likely slice of the diffuse 0.81-0.88 PE residual
  (`state_parameter_rollout.csv:40`). See row 4 in §4.6.
- **PolicyEngine compensation** is Box 1 `employment_income` with no deferral
  add-back (`pa_total_taxable_income.py` starts from `irs_gross_income`), so
  it shares our Box 1 gap.
- **TAXSIM** models Tax Forgiveness (`known_differences.csv:17`: "verified:
  86% of forgiveness records match raw; back-adding the credit drops match to
  14%"); its eligibility-income construction is unknown and row 56
  (`known_differences.csv:56`) already annotates the $20-70 residual.

### 4.6 Known-difference rows (PA)

1. **Rows 17 and 56 stay.** Nothing in this batch changes what the harness
   computes for PA (every new term is inert), so the attribution recorded in
   row 56's source field ("100% of PA clean mismatches") still holds.
2. **PA, policyengine, 2021-2025, external-model-concept, annotate** --
   "Schedule SP dependents: law counts any child claimed under IRC 151
   (minor or adult); we count every dependent (n_dep); PE counts qualifying-
   child dependents only. Differs only for units with adult dependents";
   direction `either`; magnitude "$9,500 of threshold per miscounted
   dependent, 10 points of forgiveness per $250 -- up to the whole credit";
   predicate `n_dep_ge18 > 0`; source "this note §4.5". Keep `annotate`:
   the record cannot say which adults are children.
3. **PA, both, 2017-2025, input-coverage, annotate (pre-registered)** --
   "Elective deferrals: PA compensation includes 401(k)/403(b)/457 deferrals
   (72 P.S. 7301(d)); we add elective_deferral1/2 = Tax-Data trad_contr_er1/2
   once Tax-Data fills them (all zero at vintage 2026100117_review_fixes);
   TAXSIM and PE take Box 1 wages and never see the deferral"; direction
   `over` (ours higher); magnitude "3.07% x deferrals, ~$300-$700 per
   deferring worker"; predicate: none until the harness exports the column
   -- add `elective_deferral = trad_contr_er1 + trad_contr_er2` to the
   harness record at `test_state_cross_model.R:967-996` and set the predicate
   to `elective_deferral > 0`, which fires on no record today. Source:
   "crosswalk row 505; this note §4.2".
4. **PA, policyengine, 2021-2025, external-model-approximation, exclude
   (after a probe)** -- "policyengine-us 1.775.7 exempts PA pension income
   only where is_retired (age >= 65) and, with the harness folding IRAs into
   pension income, applies the same age-65 test to IRA distributions
   (pa_nontaxable_pension_income.py; is_retired.py); the PA-40 exempts
   distributions received after qualifying for retirement at any plan age,
   and we exempt all (ob_retirement_share 0). PE runs HIGH by 3.07% of the
   under-65 spouse's distributions"; direction `over` (PE higher); magnitude
   "3.07% x (txbl_pens_dist + txbl_ira_dist) attributable to under-65
   spouses (pension split evenly across spouses in the driver,
   `pe_state_tax.py:87`)"; predicate
   `(txbl_pens_dist + txbl_ira_dist) > 0 & (age1 < 65 | (filing_status == 2 & !is.na(age2) & age2 < 65))`;
   source "this note §4.5; probe TBD (single age 60, pension 20,000, no other
   income, TY2023: expect PE pa_income_tax 614 against our 0)". Do not add
   without the probe -- the rule is read from code, not measured on
   records -- but the probe is three lines in the pattern of
   `output/probe/hi_pe_synthetic.R`.

## 5. Crosswalk, schema and documentation edits (checklist)

- `research/state_tax/tax_data_gaps.csv`: row 134 (HI pension)
  `overlay_variable` -> `pension_public;pension_er_funded_share`,
  `current_handling` -> `encoded_overlay`; row 207 (KY pension) ->
  `pension_public;pension_public_pre1998_share`, `encoded_overlay`; rows
  122, 203, 501 `other:spouse_income_mfs` -> `spouse_agi_mfs`,
  `encoded_overlay`; row 500 `hh_resources_inc` -> `nontax_support_inc`,
  `encoded_overlay`; row 505 `current_handling` -> `encoded_overlay`.
- `config/interfaces/supplemental_inputs_schema.yaml`: add
  `pension_er_funded_share`, `pension_public_pre1998_share`,
  `spouse_agi_mfs`, `nontax_support_inc` (§1.2, §2.1, §2.3, §4.3). Each is
  then exercised by the invariance test automatically (it loops the schema).
- `config/scenarios/tax_law_state/params_schema.yaml`: the seven parameters
  in §0, all at their inert defaults.
- `research/state_tax/supplemental_inputs_interface.md` §2 manifest table:
  the four new variables; §7 step 5: note the `tax_data_equivalent`
  exception (§4.2).
- `research/state_tax/notes/state_data_imputation_plan.md` item 3: move PA
  from "household-resources income" to a new "nontaxable support income"
  line, or rename the item to cover both concepts with the SP exclusion
  spelled out; item 12: `other:spouse_income_mfs` -> `spouse_agi_mfs`.
- `src/tests/state/test_state_cross_model.R:337`: add `'HI'` (§3.1);
  `:967-971`: add `n_dep_ge24` and `elective_deferral` to the record (§2.2,
  §4.6).
- `src/tests/state/cross_model/known_differences.csv`: §1.6 row 1; §2.2 row;
  §4.6 rows 2-4 (row 4 after its probe).
- Packets: `hi.md`, `ky.md`, `pa.md` "Known differences" sections and the
  tracker notes (`state_parameter_rollout.csv:15,20,40`) in the same change,
  per the state-tax protocol step 2.
- Worksheet tests: HI-9a/b/c, KY-12a/b/c, KY-13a/b/c, KY-14a/b/c/d, PA-8a/b,
  PA-9a/b, PA-10a/b/c as computed above; re-run KY-1 (unchanged), KY-8
  (unchanged), PA-4/PA-5 (unchanged), HI-2 (unchanged) as the inert pins.

## Revision history

- **2026-10-13** -- First version. Sources read for this note: HRS 235-7
  via the 2022 N-11 instructions p.13 (NBER mirror; capitol.hawaii.gov and
  law.justia.com are Cloudflare-blocked from the cluster); KRS 141.066 (LRC
  PDF); KRS 141.019(1)(g) via Schedule P (2025) and the 2025 Form 740
  instructions pp.22-23 and Schedule ITC (2025) p.2 (NBER mirror); 72 P.S.
  7301 and 7304 (FindLaw) and the 2024 PA-40 instructions pp.10-11, 37-39
  (NBER mirror); policyengine-us 1.775.7 source in the pinned venv; HHS
  poverty guidelines 2017-2026 (ASPE). No model was run.
