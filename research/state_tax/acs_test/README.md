---
title: "ACS fill test: which state-tax data gaps the ACS can close"
role: evidence
workstream: state_tax
status: current
updated: 2026-11-10
sot: research/state_tax/notes/state_data_imputation_plan.md
supersedes: []
superseded_by: null
---

# ACS fill test: which state-tax data gaps the ACS can close

Step 3a of the ACS state-input test. The question: of the 614 rows in
`research/state_tax/tax_data_gaps.csv` (one row per state provision that the
pinned Tax-Data vintage cannot compute as written; decision S43), which could be
filled from the American Community Survey (ACS) person and household records
underneath the Affordability-Index matched ACS–PUF file (tax year 2023,
implicate `B_m1`)? The answer is a per-row classification plus weighted coverage
numbers for the two families picked first: the homestead family (tenure, rent,
property tax; imputation item 1) and dependent detail (more than three
dependents, students; item 5).

Nothing here changes the calculator. The overlay names are the ones declared in
`config/interfaces/supplemental_inputs_schema.yaml`.

## How to run

From the Tax-Simulator repo root. Peak memory is 5.2 GB, above the ~5 GB login-node cap, so batch it rather
than running on the login node:

```bash
module load R/4.4.2-gfbf-2024a
AFFORDABILITY_ROOT=<Affordability-Index clone> R_LIBS_USER= \
  Rscript research/state_tax/acs_test/01_fillable_inventory.R
```

`AFFORDABILITY_ROOT` is required, and the script stops if it is unset. `R_LIBS_USER=` and the
`.libPaths()` line at the top keep the personal Arrow build, which lacks zstd,
from shadowing the module's Arrow 17. Inputs read (column subsets only):
`data/interim/income/2023/tax_units_2023.parquet` (person rows),
`files/B_m1_2023_federal.parquet` (one row per ACS return) and
`claim_links_2023_edges.parquet` (claimed-dependent edges). Runtime is about 3.5
minutes on a `day` node with 24 GB.

| Output | Content |
|---|---|
| `fillable_inventory.csv` | every registry column, plus `acs_fill`, `acs_variables`, `acs_source`, `acs_notes`, `acs_rule` (which rule table matched) and `co_input_gap` (a second input the row needs that ACS cannot fully supply) |
| `fillable_summary.csv` | row counts by `acs_fill`, by imputation item and by materiality |
| `acs_coverage.csv` | weighted coverage statistics, TY2023 |
| `acs_coverage_by_state.csv` | renter share, attributable-rent share, positive-property-tax share, by state |

### `acs_fill` values

- **filled**: an ACS variable in the matched file measures the overlay variable as the schema defines it.
- **partial_proxy**: in the matched file, but a weaker concept. Examples: no full-time flag on enrollment; residence-only property tax where the statute counts all property; a Census Supplemental Poverty Measure (SPM) modelled amount rather than a survey answer.
- **not_in_extract**: the ACS asks it, but the variable is not in the matched file. `acs_source` gives the fix. `acs_common_v2_join` means it is in the shared IPUMS extract (`raw_data/ACS/acs_common_v2`) and joins on SAMPLE+SERIAL+PERNUM: PWSTATE2, MIGPLAC1, CLASSWKRD, MOVEDIN. `ipums_repull` means it is in neither and needs a new IPUMS pull: VETSTAT, the DIFF* disability items, WIDINYR, BIRTHQTR.
- **not_in_acs**: the ACS does not ask it. This covers pension source or type, interest composition, 529 contributions, the pass-through entity tax (PTET) election, business detail and prior-year liability.

The classification is two rule tables in the script. The first keys on
`overlay_variable`. The second is a set of overrides keyed on a regex of
`required_variable`, for rows whose overlay name is `none` or too coarse. Any
row neither table names defaults to `not_in_acs`, and the script prints those
overlay names so a reader can check that nothing ACS-relevant was skipped.

## Results

### By imputation item (614 rows)

| Item | Rows | filled | partial_proxy | not_in_extract | not_in_acs |
|---|---|---|---|---|---|
| 1 homestead (tenure / rent / property tax) | 31 | 28 | 3 | 0 | 0 |
| 2 pension source | 70 | 0 | 8 (military pay) | 0 | 62 |
| 3 household resources | 13 | 1 | 11 | 0 | 1 |
| 5 dependent detail | 24 | 4 | 10 | 9 (disability) | 1 |
| 6 filer disability | 22 | 0 | 0 | 21 | 1 |
| 7 interest composition | 78 | 0 | 0 | 0 | 78 |
| 8 business detail | 47 | 0 | 0 | 0 | 47 |
| 9 expense niches | 87 | 0 | 10 | 0 | 77 |
| 12 per-person attribution | 42 | 25 | 17 | 0 | 0 |
| 14 source state | 42 | 0 | 0 | 42 (PWSTATE2 join) | 0 |
| new | 102 | 4 | 16 | 20 | 62 |
| none (locality and law-only rows) | 42 | 1 | 21 | 3 | 17 |
| items 4, 10, 11, 13 | 14 | 0 | 0 | 0 | 14 |
| **All** | **614** | **63** | **96** | **95** | **360** |

By materiality: of the 38 **high** rows, 12 are filled and 7 are partial; of
the 97 **medium** rows, 24 are filled and 22 are partial.

### Build list (high- and medium-materiality rows that ACS fills or proxies)

1. **rent_paid**: 12 rows filled. Covers the renter's credits and deductions in CA, DC, HI, IN, MA, MN, NJ, VT and WI, plus the circuit breakers keyed to rent in MT, NY and RI.
2. **property_tax_paid**: 10 rows filled and 2 partial. Covers the non-itemizer property tax bases in IL, IN, MA, ME, MI, MN, NJ and WV; NE's Form PTC and CT's credit are partial.
3. **tenure**: NJ ANCHOR. Note that CA's renter's credit needs only tenure plus AGI.
4. **Per-spouse shares** (item 12): pension_person1/2 from INCRETIR and ss_person1 from INCSS (9 + 1 rows).
5. **hh_resources_inc**: 7 rows, partial. Supplemental Security Income (SSI) and public assistance are observed; VA benefits and child support are only lumped inside INCOTHER.
6. **other:locality**: 11 rows, partial (PUMA / COUNTYFIP).

Many homestead rows also need inputs that ACS lacks, recorded in
`co_input_gap`. Disability is not in the extract, and it gates the MI, MO, OK, RI
and WI circuit breakers. Household income including nontaxable items is only
partial.

## Proposed ACS → schema mapping

### tenure (state key, 0/1/2)

`OWNERSHP`: 1 → 1 (owner-occupier), 2 → 2 (renter). OWNERSHPD separates
"owned free and clear" (12) from "with a mortgage" (13), and renters with cash
rent (22) from those with none (21). Group quarters are already excluded from
the Affordability frame, so no tax unit has tenure 0. Tenure is a household
attribute, so every tax unit in the household inherits it.

### rent_paid (state key, $/year): **RENT × 12, not RENTGRS**

`RENT` is monthly contract rent, and it includes utilities only if the lease
bundles them. `RENTGRS` adds separately billed utilities and fuel. The
registry's statutory bases all count rent paid **for occupancy**:

- **WI** homestead and school property tax credits: rent-constituting property taxes are 25% of rent, or 20% when heat is included. This needs contract rent and a heat-included flag. The script builds the flag from FUELHEAT plus the code 99997 ("included in rent") on COSTGAS, COSTELEC or COSTFUEL. It is set for 9.9% of renter returns.
- **NJ**: 18% of rent is treated as property tax.
- **MA**: deduction of 50% of rent, capped.
- **DC Schedule H, MI, MN M1RENT, RI 1040H, MO PTC, MT 2EC, NY IT-214, ME**: a fixed share of rent is treated as property tax. These forms define rent as paid for occupancy only. My reading is that RI, MO and NY expect included utilities, food or services backed out; check this against each form before encoding. That makes RENT a slight overstatement where the lease bundles utilities, while RENTGRS would overstate for everyone who pays utilities separately.
- **HI**: a threshold test only (rent above $1,000).
- **CA renter's credit, NJ ANCHOR (renter side)**: need tenure only, not an amount.
- **AZ 140PTC, IN renter's deduction**: rent paid on the principal residence, read as contract rent.

RENTGRS is the right variable only for a gross-housing-cost concept, such as
the SNAP excess-shelter deduction (SNAP is the federal food assistance program).
No current registry row has that concept.

**Missing values.** RENT is NA on every non-renter, as expected. It is also NA
on all 23,432 no-cash-rent renter households (OWNERSHPD 21). Those are coded
$0, not dropped. Every cash-rent renter has a value, and the script stops
otherwise. RENT is allocated (Census hot-deck, QRENT) for 8.5% of renter
returns. RENT is in "deflated" dollars, but the income-stage deflation is the
identity for TY2023, so the values are nominal 2023 dollars.

### property_tax_paid (state key, $/year): PROPTX99 recoded

PROPTX99 is **bracketed**, not continuous: 160 codes. The script maps each
code to its bracket midpoint (`proptx99_midpoint()`, edges read off the
acs_common_v2 2023 DDI, the IPUMS codebook file):

| code | meaning |
|---|---|
| 0 | N/A (not owner-occupied); none among owner households |
| 1 | none ($0) |
| 2 | $1–49 |
| 3–21 | $50 steps from $50 |
| 22–56 | $100 steps from $1,000 |
| 58–62 | $100 steps from $4,500 (57 is 1990-only) |
| 63–68 | $5,000–5,499, $5,500–5,999, then $1,000 steps to $9,999 |
| 69–158 | $1,000 steps from $10,000 to $99,999 |
| 159 | $100,000+ (coded at $100,000; no owner return lands here) |

One codebook anomaly: code 89 is labelled "$30000-39999" but sits between
$29,000–29,999 (88) and $31,000–31,999 (90), so it is read as $30,000–30,999.
The script stops on any unexpected code. PROPTX99 covers the principal
residence only. It does not include other real property (NE), motor-vehicle tax
(CT) or rental property. It covers owners whether or not they itemize, which is
the point: `salt_prop` on the PUF sees only 2015-base itemizers. It is allocated
for 16.7% of owner returns.

### Household → tax unit attribution (decided 2026-10-26, JI)

Rent and property tax are household amounts. **Decision: split them equally
across the household's non-dependent tax units with positive total income;
units with no income receive none** (they still inherit tenure). If no unit in
the household has income, the amount is unassigned. The equal split (rather
than in proportion to income) is our reading of the decision; revisit if the
program-total comparison argues for income shares. Implemented in the 3b
unit builder, not in `01_fillable_inventory.R`.

The coverage numbers below were computed under the earlier householder-only
rule, in which only the unit holding RELATE = 1 gets the amount. That rule
left roommates and adult children with nothing: 23% of filed returns are not
the householder's unit, and among renter returns 26% are not, while MA, NJ and
WI all let co-tenants claim their share. The "renter returns in the
householder unit" share (73.7%) therefore understates how many renter returns
will carry a positive `rent_paid` under the split.

### dep_age4plus (unit key, count)

This is `max(n_dep − 3, 0)` on the return row. `n_dep` reconciles exactly with
the claimed-dependent edges (0 mismatches over all units). Ages come from the
edges joined to the dependent's person row.

**Surprise worth knowing.** The person file's `person_role == "dependent"` is
the membership *before* Affordability's household-coordinated claim
reassignment. It disagrees with the returns' `n_dep` on 82,091 tax units, 5.2%
weighted; the totals match within each household. Dependent detail must
therefore come from `claim_links_2023_edges.parquet` (`claimant_tu_id`,
`dep_pernum`, `dep_age`, `dep_student`), not from `person_role`. For couples
split into two returns (parts 1 and 2; 30,561 units), Affordability assigns
dependents to parts on the fly (`04_harmonize.R`) and does not store that
assignment. Dependent detail is therefore exact per tax unit but not per part.

### dep_student_n (unit key, count): partial proxy

This counts claimed dependents aged 19–23 with `SCHOOL == 2` (enrolled at any
time in the past 3 months; the edges' `dep_student` equals this on every row).
The ACS has **no full-time indicator**, so the statutory "full-time student"
test (IN, MN, NM) is proxied by any enrollment. GRADEATT 6–7 (undergraduate,
graduate) narrows the count to college, for NJ's "under 22 in full-time
college". IN's private-school deduction can use SCHLTYPE = 3 (private) with
GRADEATT 2–5 (kindergarten through grade 12); homeschooling is not identified.
No dependent has SCHOOL or AGE missing.

## Weighted coverage, TY2023 (B_m1, weight `hhwt`, filed non-dependent returns)

| Statistic | Value |
|---|---|
| Filed non-dependent returns | 156.0 M (1.56 M records) |
| Owner / renter household | 65.8% / 34.2% (none unknown) |
| Returns that are the householder's own unit | 76.9% |
| Renter returns: cash rent > 0 / no cash rent | 96.1% / 3.9% |
| Renter returns in the householder unit (rent attributable) | 73.7% |
| Renter returns with heat included in rent | 9.9% |
| Renter householder returns: mean annual contract / gross rent | $16,800 / $19,200 |
| All renter households: aggregate contract rent | $720 bn |
| Owner returns: property tax > 0 / none (code 1) | 95.5% / 4.5% |
| Owner returns in the householder unit | 78.5% |
| Owner householder returns: mean property tax (midpoints) | $4,340 |
| All owner households: aggregate property tax | $358 bn |
| Tax units with more than 3 dependents | 2.44 M (1.6%); filed returns with `n_dep` > 3: 2.34 M |
| Dependents beyond the third age slot | 3.35 M |
| Claimed dependents aged 19–23 | 6.21 M, of which 4.59 M enrolled and 4.27 M in college |
| Tax units with at least one enrolled dependent aged 19–23 | 2.75% |
| Claimed dependents aged 24 and over | 6.26 M |
| Private-school K–12 dependents | 6.38 M |
| Dependents aged 0 | 3.03 M |

Renter shares by state are in `acs_coverage_by_state.csv`. Examples: CA 44%,
NY 44%, DC 59%, MA 36%, NJ 35%, WI 31%, MN 27%, MI 25%.

The aggregates ($720 bn rent, $358 bn residential property tax) are worth
checking against published ACS tables before calibration. They have not been
benchmarked here.

## Not in the extract: what a re-pull would add

- **VETSTAT / VETDISAB**: veteran1/2 (5 rows), plus the disabled-veteran prong of the MO and ME homestead credits.
- **DIFFREM, DIFFPHYS, DIFFMOB, DIFFCARE, DIFFSENS, DIFFEYE, DIFFHEAR**: disabled1/2 (20 rows) and dep_disabled_n (10 rows), plus the disability prongs of the MI, MO, OK, RI and WI circuit breakers. Affordability's `fetch_acs_1yr.R` already lists the DIFF* items as pending. Neither the matched file nor acs_common_v2 carries them.
- **WIDINYR** (widowed in the past year): qualifying-widower and spouse-death-year rows. **BIRTHQTR** (quarter of birth): the age-59½ and full-retirement-age gates.
- **PWSTATE2** (place-of-work state), already in acs_common_v2: the 42 source-state rows, for wages only.
