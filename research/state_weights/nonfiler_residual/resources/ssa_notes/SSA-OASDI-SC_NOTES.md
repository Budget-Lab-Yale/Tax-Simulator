---
title: "SSA-OASDI-SC: Notes on the data"
role: notes
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/nonfiler_residual_design.md
supersedes: []
superseded_by: null
---

# SSA-OASDI-SC: Notes on the data

What is in this store, what each table means, and the traps that would
otherwise be rediscovered painfully. Compiled 2026-08-19 from the publication
itself and **verified directly against the files here** — every number quoted
below was computed from the workbooks and flat series in this directory, not
copied from a summary.

Drafted and maintained in Tax-Simulator
(`research/state_weights/nonfiler_residual/resources/ssa_notes/`); this copy
is placed with the data for anyone who reaches it without the repo.
`01_fetch_residual_inputs.R` never touches this file.

---

## 1. Publication identity

- **Title:** *OASDI Beneficiaries by State and County*
- **Publisher:** Social Security Administration, Office of Retirement and
  Disability Policy, Office of Research, Evaluation, and Statistics
- **Frequency:** annual; each edition is a **December point-in-time stock**
- **Landing page:** https://www.ssa.gov/policy/docs/statcomps/oasdi_sc/
- **Flat series:** https://www.ssa.gov/policy/docs/statcomps/oasdi_sc/flat-series.html
- **data.gov catalogue:**
  https://catalog.data.gov/dataset/oasdi-beneficiaries-by-state-and-county-series
- **Source of the counts:** Master Beneficiary Record, **100 percent data**,
  geocoded with U.S. Postal Service geographic data. (Contrast SSA-EEDATA-SC,
  which is a 1% sample — see that family's notes.)

**Retrieval requires a real browser engine.** ssa.gov 403s automated retrieval
on **TLS fingerprint, not user agent**: curl with full browser headers, .NET
`Invoke-WebRequest` and hosted fetch services are all refused, for static
`.xlsx`/`.json` assets as well as HTML. Headless Chrome retrieves everything.
**No browser engine exists on this cluster** — `google-chrome`, `chromium`,
`chromium-browser` are all absent from `PATH` and no Lmod module provides one
(checked 2026-08-19). Files here are therefore placed by hand from a
workstation; do not re-litigate this without first installing a browser engine.

## 2. What is in this store

| File(s) | Contents |
|---|---|
| `oasdi_sc17.xlsx` … `oasdi_sc25.xlsx` | Per-year workbooks, data years **2017–2025**. Filename carries the **two-digit data year**. |
| `oasdi_sc_flatseries_table2_beneficiaries.json` | **Source of record for the age margin.** Table 2 flattened, **1999-12 … 2025-12** |
| `oasdi_sc_flatseries_table1_population_shares.json` | Table 1 flattened, same span |

`manifest.csv` carries path, source URL, data year, bytes, md5 and retrieval
date. The flat-series files span many years and so carry no single `year`.

**Source of record: the flat series, with the workbooks as the cross-check.**
That cross-check has been performed and is exact — all **59 areas × 11 measures
× 2 anchor years (2017, 2022), zero mismatches**. Prefer the flat series
because it delivers every year at once (design memo §8 needs 2014 and 2016–2019
once back-year weights are fit) and carries a documented schema with labelled
`dimensions` and `measures`, rather than a workbook with two-row merged headers.
A reader should still read one and assert against the other, so the agreement
stays enforced rather than assumed.

**Flat-series gaps and quirks, all of which a reader must handle:**

- **2010 is absent** from the series (1999–2009, then 2011–2025 = 26 months).
- The U.S. Virgin Islands is labelled **`Virgin Islands` for 1999–2006** and
  **`U.S. Virgin Islands` from 2007**. Match on both.
- Early years carry 58 areas, later years 59.
- The JSON has **no `Outlying areas` row**; the workbook has one, but it is a
  **group label with no data**, so neither source double-counts.

## 3. Table → measure map

### Table 2 — the one the anchors consume

*"Number of beneficiaries in current-payment status, by state or other area,
type of benefit, and sex of beneficiaries aged 65 or older, December {year}"*

Header occupies rows 1–3 (merged); data begins at row 4 with `All areas`.
Eleven measures, in workbook column order:

| # | Workbook column | Flat-series measure |
|---|---|---|
| 1 | Total | `persons_oasdi` |
| 2 | Retirement: Retired workers | `persons_ret_workers` |
| 3 | Retirement: Spouses | `persons_ret_spouses` |
| 4 | Retirement: Children | `persons_ret_children` |
| 5 | Survivors: Widow(er)s and parents | `persons_surv_widows_parents` |
| 6 | Survivors: Children | `persons_surv_children` |
| 7 | Disability: Disabled workers | `persons_di_workers` |
| 8 | Disability: Spouses | `persons_di_spouses` |
| 9 | Disability: Children | `persons_di_children` |
| 10 | **Aged 65 or older: Men** | `persons_oasdi_65_older_men` |
| 11 | **Aged 65 or older: Women** | `persons_oasdi_65_older_women` |

**The D6 age margin is measures 10 + 11 summed.** Columns 1–9 partition
beneficiaries by benefit type; columns 10–11 are a **separate cut of the same
total by age and sex**, not additional categories. Do not add them to 1–9.

### Table 1 — population shares (context, and one trap; see §5)

Total resident population and population aged 65+ by state, each with the
percentage receiving benefits. Population is the Census Bureau's **July 1
resident population estimate** for the same year — a different vintage from the
PEP extracts in `Census-PEP/` (see §7).

### Tables 3–5

Table 3 is county-level; Tables 4 and 5 are one sheet per state (107 sheets per
workbook). Not consumed by the residual anchors. County detail is available if
a sub-state allocation is ever wanted.

## 4. Geography rows — 59 areas, not 51

Every state table carries, in order:

`All areas` · **51 jurisdictions** (50 states + District of Columbia) ·
`Outlying areas` (label only, no data) · American Samoa · Guam · Northern
Mariana Islands · Puerto Rico · U.S. Virgin Islands · `Foreign countries` ·
`Unknown`

The partition is **exact in both anchor years**: 51 jurisdictions + 5
territories + foreign + unknown = `All areas`, to the person.

**Use the 51-jurisdiction sum for the residual anchors, not `All areas`.**

| Aged 65+ | 2017 | 2022 |
|---|---|---|
| `All areas` | 45,808,776 | 52,052,807 |
| **51 jurisdictions (the anchor)** | **44,635,968** | **50,766,317** |
| 5 territories | 581,823 | 641,189 |
| Foreign countries | 589,823 | 644,832 |
| Unknown | 1,162 | 469 |

`All areas` overstates the US-resident 65+ beneficiary count by **1.17M (2.6%)
in 2017 and 1.29M (2.5%) in 2022**, almost entirely beneficiaries residing
abroad and in the territories — neither of which is in the residual's universe.
`06_verify_ssa_inputs.R` checks the `All areas` row deliberately, because that
is a **file-identity** check; it is not the anchor value.

## 5. Universe and concept caveats

- **December current-payment status** is a **point-in-time stock**; tax
  concepts are **annual flows**. A person who died in June or started benefits
  in that December is treated differently by the two.
- The 65+ split is published **by sex only**. There is **no finer age detail
  within 65+** from this source — relevant if `age_band()`'s `65_74` / `75p`
  split is retained (design memo pre-flight, task P2). It cannot be sourced
  here.
- Geography is **beneficiary residence**, geocoded from Postal Service data,
  not the state of the paying office or of prior employment.
- Beneficiaries, not recipients of all SSA programs: **SSI is not in this
  publication**.
- **Table 1's national percentage changed basis with the 2019 edition.** The
  state rows are consistent throughout, but the `United States` row is not:

  | Edition | Published 65+ pct | 51-juris/pop | All-areas/pop |
  |---|---|---|---|
  | 2017 | 90.1 | 0.8776 | **0.9007** |
  | 2018 | 89.9 | 0.8766 | **0.8994** |
  | 2019 | 87.7 | **0.8773** | 0.9000 |
  | 2022 | 87.8 | **0.8784** | 0.9006 |
  | 2025 | 86.6 | **0.8658** | 0.8869 |

  Through 2018 the numerator is `All areas` over a **US-resident** denominator —
  a mismatched ratio. From 2019 it is the consistent 51-jurisdiction ratio.
  **Do not read the published national percentage as a time series across 2018/2019.**
  The underlying Table 2 state data are unaffected and *are* comparable: the
  consistent ratio sits at 0.877 ± 0.01 across 2017–2024.

  This also disposes of a sanity check in the (now archived) SSA inputs plan, which
  read the ~0.90 all-areas ratio as agreeing with the publication's own coverage
  percentage. The agreement was real for 2017 and coincidental: both sides were
  making the same mismatched comparison. The right ratio is ~0.878.

## 6. Geography-assignment continuity across editions

**Answered, not deferred — the two anchor years are on the same basis.**
Evidence, all checked directly against the files:

1. The source note is **character-identical** in 2017 and 2022: *"Social
   Security Administration, Master Beneficiary Record, 100 percent data; and
   U.S. Postal Service geographic data."*
2. Sheet inventory is identical (107 sheets, same names, same order), and
   Table 2's header block and 11-column layout are identical.
3. The geography row list is identical, and the 51 + territories + foreign +
   unknown partition reconciles exactly to `All areas` in both years.
4. The 51-jurisdiction 65+ share of the 65+ population is stable across
   2017–2024 (0.8727–0.8811), with no step at any edition boundary.

Two things are *not* continuous and are flagged above: the **Table 1 national
percentage basis (2018→2019)**, and the **Virgin Islands label (2006→2007)** in
the flat series.

The 2025 edition's 65+ population jumps to 64.6M from 61.2M in 2024 (+5.6%),
which is a **Census population-estimate revision, not an SSA change** — it moves
the denominator only. Treat 2025 population shares with care until the vintage
is pinned down.

## 7. Vintage pairing

- Table 1's population is the Census **July 1 resident population estimate of
  the same year, as published in that edition** — 50,858,679 (65+, 2017) and
  57,794,852 (65+, 2022).
- These are **not** the figures in `Census-PEP/`: the design memo's PEP-based
  65+ population for 2022 is 57,505,037, about **0.5% below** the SSA edition's
  57,794,852, because SSA used the vintage current at publication and the PEP
  extracts here are later vintages (`sc-est2020int-alldata6.csv` intercensal for
  2017, `sc-est2024-alldata6.csv` for 2022).
- **Rule: take beneficiary counts from SSA and population denominators from
  `Census-PEP/`.** Never mix an SSA-published share with a PEP denominator —
  that is exactly the error §5 documents. This closes the design memo §8 open
  question on vintage pairing.

## 8. Consumers

- **Tax-Simulator** `research/state_weights/nonfiler_residual/` — Stage D
  residual anchors; the 65+ margin drives the state × age allocation (**D6**).
- **Affordability-Index** — expected later.

Per design memo §7.3, a margin shared between the two systems must carry an
explicit **universe tag** (`resident` vs `household`). SSA's universe is
**neither**: it is *beneficiaries* (a person-level administrative universe,
including group-quarters residents and, in `All areas`, people abroad). Tag it
`beneficiary` and convert deliberately rather than assuming either tag fits.
