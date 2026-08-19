# SSA-EEDATA-SC: Notes on the data

What is in this store, what each table means, and the traps that would
otherwise be rediscovered painfully. Compiled 2026-08-19 from the publication
itself and **verified directly against the files here** — every number quoted
below was computed from the workbooks in this directory, not copied from a
summary.

Drafted and maintained in Tax-Simulator
(`other/state_tax_research/nonfiler_residual/resources/ssa_notes/`); this copy
is placed with the data for anyone who reaches it without the repo.
`01_fetch_residual_inputs.R` never touches this file.

---

## 1. Publication identity

- **Title:** *Earnings and Employment Data for Workers Covered Under Social
  Security and Medicare, by State and County*
- **Publisher:** Social Security Administration, Office of Retirement and
  Disability Policy, Office of Research, Evaluation, and Statistics
- **Frequency:** annual; each edition is an **annual flow** for one data year
- **Landing page:** https://www.ssa.gov/policy/docs/statcomps/eedata_sc/
- **Source of the counts:** **Continuous Work History Sample, 1 percent
  sample** — see §5, this is the single most important caveat in this file.
- **No flat series exists** for this publication: per-year workbooks only.

**Retrieval requires a real browser engine**, and none exists on this cluster.
Identical situation to SSA-OASDI-SC — see that family's notes §1 for the detail
and for the check that was run. Files here are placed by hand.

## 2. What is in this store

`eedata_sc17.xlsx` … `eedata_sc23.xlsx` — per-year workbooks, data years
**2017–2023**, filename carrying the **two-digit data year**. 106 sheets each.
`manifest.csv` carries path, source URL, data year, bytes, md5, retrieval date.

**The series ends at data year 2023.** Any anchor year after 2023 has **no
covered-worker margin at all** from this source. That bounds how far the method
extends without a substitute, and it is a hard constraint, not a lag that will
resolve on its own schedule — plan for it.

## 3. Table → measure map

The workbook publishes **two parallel universes**, and choosing between them is
the main decision a consumer of this family makes:

| Sheet | Universe | Geography | Detail |
|---|---|---|---|
| **Table 1** | **OASDI**-covered | state | number of persons, taxable earnings, OASDI contributions |
| **Table 2** | **OASDI**-covered | state | number of persons **× age group** |
| Table 3 | OASDI-covered | one sheet per state | county detail |
| **Table 4** | **HI (Medicare)**-covered | state | number of persons, taxable earnings, HI contributions |
| **Table 5** | **HI (Medicare)**-covered | state | number of persons **× age group** |
| Table 6 | HI-covered | one sheet per state | county detail |

Tables 1 and 4 (and 2 and 5) are **structurally identical** — same layout, same
columns, different coverage concept.

**Tables 1 and 4 columns.** Header occupies rows 1–3 (merged); data begins at
row 4 with `All areas`. Three column groups of three:

- `Number`: **`Total`** / **`Wage and salary`** / `Self-employed`
- `Taxable earnings` (**$ thousands**): same three splits
- `OASDI contributions` (Table 1) or `HI contributions` (Table 4), $ thousands

**The covered-worker margin is `Number` → `Wage and salary`** — *not* `Total`,
which also counts the self-employed.

**Tables 2 and 5 age groups** (identical across all editions checked):
`Total, all ages` · `Under 20` · `20–29` · `30–39` · `40–49` · `50–59` ·
`60–61` · `62–64` · `65–69` · `70 or older`. The bands are cut around Social
Security eligibility, not around round decades. This is **an age dimension the
design memo did not expect to have**, and it is the natural input to the
working-age layer of D6 — note it does not nest inside `age_band()`'s
boundaries without a decision about `60–61`/`62–64`.

**Row structure.** Each geography occupies **three rows**: the area total, then
`Men`, then `Women`. In Tables 1 and 4, `All areas` is row 4 and states run
rows 7, 10, 13, … 157.

## 4. Geography rows

`All areas` · **51 jurisdictions** (50 states + DC) · `Outlying areas` (label
only) · `Puerto Rico` · `Other` — the last defined by footnote *d* as persons
employed in American Samoa, Guam, the Northern Mariana Islands and the U.S.
Virgin Islands. **Fewer geography rows than SSA-OASDI-SC**, which breaks the
territories out individually and adds `Foreign countries` and `Unknown`.

The partition reconciles to `All areas` **to within a handful of persons** in
both anchor years (sampling/rounding residual, e.g. 2017 Table 1 total:
173,009,996 summed vs 173,010,000 published). As with OASDI, **use the
51-jurisdiction sum for the anchors**, not `All areas`.

**`NOTES: State designation is based on employee residence.`** — character
identical in the 2017 and 2022 editions. Residence, **not** place of work: it
does not need a commuter adjustment, and it should not be given one.

## 5. Universe and concept caveats

**These are 1% sample estimates, not a count.** The Continuous Work History
Sample is a 1-in-100 sample of Social Security numbers; every figure in this
publication is a weighted-up estimate. Consequences that matter:

- National totals show the rounding: 2017 wage-and-salary persons publish as
  exactly `161,986,000`.
- **State-level counts carry sampling error, and it bites hardest in the
  smallest states** — a state with 300,000 covered workers rests on ~3,000
  sampled records. Do not treat a small-state margin as exact, and do not build
  a hard constraint on one.
- SSA-OASDI-SC, by contrast, is **100 percent data**. The two families in this
  store are *not* of equal precision, and a residual that binds them together
  inherits the weaker one.

**OASDI vs HI coverage is a real difference, not a labelling one.** HI
(Medicare) coverage is broader — it picks up state and local government
employment that is outside OASDI coverage:

| Wage-and-salary persons | 2017 | 2022 |
|---|---|---|
| OASDI-covered (Table 1) | 161,986,000 | 168,525,999 |
| HI-covered (Table 4) | 166,205,000 | 172,587,000 |
| difference | 4.2M (2.6%) | 4.1M (2.4%) |

**OASDI taxable earnings are capped; HI taxable earnings are not.** Table 1's
earnings are limited by the annual taxable maximum (footnote *a*: $127,200 in
2017, $147,000 in 2022). Table 4's are uncapped. Checked against
`BLS-QCEW/`, on the 51-jurisdiction sum:

| Wage-and-salary earnings, 51 jurisdictions | 2017 | 2022 |
|---|---|---|
| QCEW total annual wages | $7.968 T | $10.500 T |
| **HI taxable (Table 4)** | **$8.022 T** (**1.007×**) | **$10.635 T** (**1.013×**) |
| OASDI taxable (Table 1) | $6.590 T (0.827×) | $8.673 T (0.826×) |

**For any dollar cross-check against QCEW, use Table 4, not Table 1.** Table 1
sits ~17% low in both years purely because of the taxable maximum, and reading
that gap as a data problem would be a wasted investigation. The HI figure
agrees with QCEW to about 1% — the residual is genuine coverage difference
(QCEW excludes some agricultural, domestic and self-employed-adjacent
employment that HI picks up), not error.

**Other caveats:**

- **`Total` ≠ `Wage and salary` + `Self-employed`.** Footnote *c*: a worker
  with both kinds of earnings is counted in **both** component columns but once
  in the total. 2017: 161,986,000 + 19,615,000 = 181,601,000 against a
  published total of 173,010,000.
- **Non-covered employment is out of scope by construction** — some state and
  local government (for OASDI), certain railroad employment. Those workers will
  never appear in the margin, however the data are handled.
- **EEDATA counts *persons with covered earnings*; IRS HT2 counts *returns with
  wages*.** These are different objects, and the design memo's ~75% ±9pp ratio
  is the **wedge between them**, not an error to be driven to zero.
- Earnings are **annual flows**; SSA-OASDI-SC beneficiary counts are a December
  **stock**. Do not combine the two without a stated reconciliation.

## 6. Continuity across editions

**The two anchor years are on the same basis.** Sheet inventory identical (106
sheets, same names and order); Tables 1/2/4/5 header blocks and column layouts
identical; the residence-based geography note character-identical; age bands
identical; geography row list identical. The only differences found between the
2017 and 2022 editions are the **taxable maximum in footnote *a*** (as it should
be) and the **contact line** dropping its phone number.

## 7. Vintage pairing

This publication carries no population denominator of its own, so there is no
vintage to pair — unlike SSA-OASDI-SC Table 1 (see that family's notes §7).
Any denominator comes from `Census-PEP/`, and the same rule applies: **counts
from SSA, denominators from PEP.**

## 8. Consumers

- **Tax-Simulator** `other/state_tax_research/nonfiler_residual/` — Stage D
  residual anchors; wage-and-salary persons feed the
  `nonfiler_wage_margin_{year}.csv` columns and the returns-per-person ratio.
- **Affordability-Index** — expected later.

Per design memo §7.3, a shared margin must carry an explicit **universe tag**
(`resident` vs `household`). This family is **neither**: it is *covered
workers*, a person-level administrative universe cutting across both.

> **DECIDED (JI, 2026-08-19): the coverage concept is HI (Medicare).**
> Read **Table 4** for persons and dollars and **Table 5** for the state x age
> detail; Tables 1/2 (OASDI) are the cross-check, not the margin. Reasons: HI is
> the closer analogue to the W-2 universe, since it includes the state and local
> government employment that sits outside OASDI coverage (~4.1M persons); and on
> dollars it is not a close call — HI is **uncapped** and matches QCEW to ~1%,
> where OASDI's taxable-maximum-capped earnings run ~17% low.
>
> Tag every margin drawn from this family **`covered_worker_hi`**. The tag is not
> bureaucratic: the two universes differ by ~2.5% in persons and ~20% in dollars,
> both tables have identical layouts, and a margin that loses its provenance is
> indistinguishable from the wrong one.
