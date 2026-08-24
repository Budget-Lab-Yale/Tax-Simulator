---
title: "Source documents and hand-transcribed resources"
role: index
workstream: state_weights
status: current
updated: 2026-08-24
sot: research/state_weights/nonfiler_residual_design.md
supersedes: []
superseded_by: null
---

# Source documents and hand-transcribed resources

This folder holds four kinds of thing: **hand-transcribed tables** (the CSVs
below), **generated inputs** to the Tax-Data rework (see the next section), the
**bibliography** for the filing-model literature (`filing_model_refs.bib`), and
**source PDFs** we could not rely on retrieving again (see "Source PDFs" at the
end).

## Generated inputs (not transcriptions)

### `nonfiler_age_shape_{year}.csv` — D1's age draw

**Written by `02_build_residual_anchors.R`, so do not hand-edit it**; it lives
here rather than in `results/` because it is a committed *input* to the Tax-Data
rework, not a regenerable diagnostic.

| column | meaning |
|---|---|
| `band` | the 7-band CELL space: `18_25 … 55_64`, then `65_74` / `75p` |
| `age_group` | **Tax-Data's own 1-6 coding** (`project_puf.R:324-330`), so D1 can join directly. `65_74` and `75p` both carry 6 |
| `residual_nonfiling_adults` | PEP adults in band − T1.6 filing adults in band |
| `share` | share of the national residual; sums to 1 |
| `share_within_age_group` | for splitting Tax-Data's `age_group == 6` into `65_74` / `75p` |
| `ira_share_65_74` | the SOI IRA Table 4 share used for the 65+ split, carried for provenance |

**Why the 65+ split needs a second source.** Pub 1304 Table 1.6 stops at "65 and
over" and SSA OASDI-SC publishes 65+ by sex only, so nothing in the anchor's own
inputs divides it. SOI's IRA study **Table 4, column (1)** publishes Form 1040
filers by five-year band, and `65 under 70`+`70 under 75` /
`75 under 80`+`80 and over` aggregate exactly onto our two. Read by
`read_soi_ira_age_split()`.

**It supplies the SHARE, never the level.** Its own 65+ total runs ~4.5% below
T1.6's, consistently, because it assigns each taxpayer *their own* age where
T1.6 assigns a joint return the *primary's*. Applying its share to T1.6's level
keeps T1.6's convention and borrows only the shape.

**⚠ Convention wedge, carried not resolved.** PEP counts each person at their own
age; T1.6 assigns a joint return's two filing adults to the primary's band. So
the residual by band mixes conventions, and the Tax-Data draw it feeds sets the
*primary's* age — exact for the single/HoH majority, approximate for the ~17% of
non-filer units that are joint. Do not read `share` as a distribution of
non-filing adults over their own ages.

**Files:** `ira_t04_{year}.xlsx` in `raw_data/IRS-Ind/national/ira/`, TY2000–2023
with **2003 not published**. Note the extension: a `.xls` glob silently misses
this whole family.

## Hand-transcribed tables (Stage D)

Transcribed 2026-08-16 from IRS Publication 5785, "The Individual Income Tax
and Self-Employment Tax Nonfiling Tax Gaps for Tax Years 2014-2016" (Hertz,
Langetieg, Payne and Plumley), https://www.irs.gov/pub/irs-pdf/p5785.pdf
(also cached at the Stage-D scratch dir). Verified against the PDF text
extraction; money amounts in $ billions, counts in millions.

- `pub5785_table1_potential_nonfilers.csv` — Table 1 (PDF p.17/"Page 9"),
  PERSON level: counts of potential nonfilers (people not appearing on a
  filed return, valid TIN, alive, under 110) with each income type, and
  aggregate amounts, TY2014-2016. Column transcribed: **Administrative
  Population (n), 3rd-party information returns** (column A — the full
  administrative universe, not the linked-sample reweight). This is the
  same conceptual object as the Stage-D residual anchor: use its receipt
  rates (count / total_population) to discipline the non-filer
  investment-income repair (design memo §5.1b) and its level as a
  triangulation of the residual (T1).
- `pub5785_table3_notfiler_units.csv` — Table 3 (PDF p.21/"Page 13"),
  TAX-UNIT level: not-filers WITH a filing obligation (the above-threshold
  hazard's level anchor), by filing status and income item, TY2014-2016 +
  period average. Key structural facts (PDF p.22): 49.6M potential
  nonfilers (2014) reduce to 10.6M above-threshold units — ~2M via spouse
  combining, ~37M lacking a filing obligation; under 20% of not-filer units
  are married (vs ~38% of filers); ~45% have net business/farm income (the
  self-employment signature motivating a SE dimension in the hazard, memo
  D3).

## Transcribed coefficients (Stage D, todo A4/A5)

Both tables were transcribed **from rendered page images**, 2026-08-19, and both
are checked by `research/state_weights/nonfiler_residual/11_verify_coef_transcriptions.py`, which recomputes each PDF
page's multiset of numeric tokens and confirms every value the CSV claims from
that page is present. That catches typos and dropped digits; it cannot catch a
swap of two cells on the same page, which is why the images were read rather
than the text dump. Run it after any edit:

```
module load poppler/25.07.0-GCC-13.3.0
python3 research/state_weights/nonfiler_residual/11_verify_coef_transcriptions.py
```

### ⚠ The two tables have OPPOSITE dependent variables

**Mok's `mok_coefs.csv` predicts P(FILES). Cilke's `cilke_coefs.csv` predicts
P(DOES NOT FILE).** A positive Mok coefficient means *more likely to file*; a
positive Cilke coefficient means *more likely to be a non-filer*. Scoring one
set through the other's convention inverts the entire model while producing
perfectly plausible probabilities. Convert deliberately, and never pool them.

The shared anchor that makes this checkable: *no earned income* raises
non-filing. It is **positive in all nine Cilke columns** (0.5069 … 0.9598).
Mok has no such term, but her wage-presence coefficients are **positive**
(0.552 … 0.9), i.e. having wages raises *filing* — the same fact, opposite sign.

### `mok_coefs.csv` — the model of record

Mok (2017) Table 14, pp. 48–50 of `mok2017_cbo_wp2017-06.pdf`; **14 groups × 17
terms = 238 rows**, coefficients, standard errors, significance stars, group
sample sizes and weighted filing rates. Estimated on the 2007 CPS ASEC linked to
the IRS Individual Master File, TY2006. This **replaces** Cilke as the model of
record (`../05_filing_model_literature.md`, design memo §3.2.2).

Three irregularities in the published table, all encoded rather than smoothed:

- **Panel E's columns run "Age 65 or Older" FIRST, then "Under Age 65"** — the
  reverse of the intuitive order, and text extraction returns the headers
  swapped, which would silently exchange the 0.23 and 0.10 filing rates. Sample
  sizes (909 vs 62,438) disambiguate.
- **Panel E has `.` rather than a coefficient for self-employment income in the
  65+ column.** Stored blank with a `note`, never as zero.
- **Panel E has no "Retirement income" row at all** — it is the only panel
  missing one, so Panel E's equations carry 15 covariates where the others carry
  16. Stored blank with a `note` for both Panel E groups.

Reading the equations (from the table's own footnote): the dependent variable is
1 if the primary taxpayer has a 1040; **gross income is total income NET OF
taxable Social Security**; the income-source variables are **presence
indicators**, not amounts; `n_medicaid` is a **count**, not an indicator;
means-tested transfers means TANF, SNAP, LIHEAP and housing assistance; stars
are *** p<0.01, ** p<0.05, * p<0.1. The omitted category is described as
"non-Hispanic white with more than a college education" — which sits oddly with
a two-dummy education scheme (`educ_less_than_hs`, `educ_college`) whose natural
omitted group is high-school-to-some-college. **Resolve this against Mok before
scoring**; it decides which population the intercept describes.

Mok's frame **excludes the institutionalized and military-barracks populations**,
so these coefficients do not cover the group-quarters records the PUF universe
includes (design memo C7).

### `cilke_coefs.csv` — the comparison fit only

Cilke (1998) OTA WP-78 Table 3, pp. 26–29 of `cilke1998_ota_wp78.pdf`; **9 groups
× 24 terms = 216 rows**, probit estimates, standard errors and group sample
sizes (8,469 / 3,544 / 4,379 / 462 / 2,590 / 940 / 233 / 2,413 / 692; total
23,722). Estimated on the March 1991 CPS, TY1990.

- **14 cells are published as `0.0000 / 0.0000`** — not estimated for that group
  (gender in the four married groups, household head in six groups, AFDC in
  four). Stored blank with a `note`. A zero coefficient and an unestimated one
  are different things and the verifier asserts the count of them.
- Each group's equation is printed across **two pages** — variables through
  `no_public_housing` on the first, the remaining six plus the sample size on
  the second. The CSV carries `pdf_page` per row so provenance survives.
- **Two groups are too thin to support 24 parameters** (n=462 and n=233); treat
  their coefficients as indicative at best.
- The paper's own appendix codes `INON: 0 = NON-FILER, 1 = FILER`, contradicting
  the table. **The table is right** — see the sign check above.

Use it only to ask whether Mok's fit is doing something Cilke's did not. If only
one model is fit, the memo says fit Mok.

## Source PDFs

- **`mok2017_cbo_wp2017-06.pdf`** — Shannon Mok, "An Evaluation of Using Linked
  Survey and Administrative Data to Impute Nonfilers to the Population of Tax
  Return Filers," CBO Working Paper 2017-06, September 2017 (60pp). Original
  filename `53125-nonfilers.pdf`; md5 `eb649ef0c5918ad0571b2005dc1d2437`.
  **Committed because cbo.gov returns 403 to automated retrieval**, which cost a
  round of unverified citations — the coefficients this project depends on should
  not sit behind a bot block. A US government work, so public domain. Verified
  line by line on 2026-08-18; see `../05_filing_model_literature.md` §7 for what
  was checked and the one claim that verification corrected.

  Note the house convention is that PDFs consolidate in the `references` repo.
  This copy is here deliberately, as provenance for the transcription that has
  not happened yet; move or de-duplicate it if `references` takes it on.

- **`cilke1998_ota_wp78.pdf`** — James Cilke, "A Profile of Non-Filers,"
  U.S. Treasury Office of Tax Analysis Working Paper 78, July 1998 (38pp).
  Retrieved 2026-08-19 from https://home.treasury.gov/system/files/131/WP-78.pdf
  (which does *not* block automated retrieval, unlike cbo.gov); md5
  `647d7a6a2af6a04f6bad1343feb613ee`. A US government work, so public domain.
  Committed as provenance for `cilke_coefs.csv`, which the verifier checks
  against it page by page — the transcription and its source should not be
  separable.

## SSA store documentation (`ssa_notes/`)

Drafts of the `NOTES.md` files that document the two SSA statcomps families,
kept here so they are reviewable in git; the placed copies live in the store at
`raw_data/SSA-OASDI-SC/NOTES.md` and `raw_data/SSA-EEDATA-SC/NOTES.md`
(IRS-Ind convention). Written 2026-08-19, with every figure computed from the
files rather than copied from a summary. Edit here and re-copy; the downloader
never touches them.

The three findings in them that change what a consumer must do: use the
**51-jurisdiction sum, not `All areas`** (2.5-2.6% overstatement); use EEDATA
**Table 4 (HI, uncapped)** for any QCEW dollar cross-check, not Table 1 (OASDI,
capped ~17% low); and remember **EEDATA is a 1% sample** while OASDI-SC is 100%
data.

## Staged data — placed and removed

`ssa_flatseries/` held the two OASDI-SC flattened time-series JSONs
(1999-12 – 2025-12) as a transfer mechanism, because ssa.gov cannot be reached
from the cluster. **Placed in `raw_data/SSA-OASDI-SC/` and deleted from git on
2026-08-19** (md5-verified before removal, then registered by
`01_fetch_residual_inputs.R`). They agree with the per-year workbooks exactly —
59 areas × 11 measures × 2 anchor years, zero mismatches — and are the **source
of record** for the 65+ margin, with the workbooks as the standing cross-check.
