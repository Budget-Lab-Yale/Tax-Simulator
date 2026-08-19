# Source documents and hand-transcribed resources

This folder holds three kinds of thing: **hand-transcribed tables** (the CSVs
below), the **bibliography** for the filing-model literature
(`filing_model_refs.bib`), and **source PDFs** we could not rely on retrieving
again (see "Source PDFs" at the end).

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

## Coefficients still to transcribe

**`mok_coefs.csv` — the primary below-threshold model, not yet transcribed.**
Mok (2017) Table 14, pp. 48–50 of `mok2017_cbo_wp2017-06.pdf` (below): 14 group
probits with coefficients and standard errors. This **replaces** Cilke as the
model of record — see `../05_filing_model_literature.md` and design memo §3.2.2.
Two warnings before transcribing:

- **Panel E's columns run "Age 65 or Older" FIRST, then "Under Age 65"** — the
  reverse of the intuitive order, and automated text extraction returns the two
  headers in the wrong sequence, which would silently swap the 0.23 and 0.10
  filing rates. Sample sizes (909 vs 62,438) disambiguate. **Transcribe from a
  rendered image, not a text dump.** Panel E also has `.` rather than a
  coefficient for self-employment income in the 65+ column.
- Mok's CPS frame **excludes the institutionalized and military-barracks
  populations**, so these coefficients do not cover the group-quarters records
  the PUF universe includes.

**`cilke_coefs.csv` — retained as the comparison fit only, not yet transcribed.**
Cilke (1998), https://home.treasury.gov/system/files/131/WP-78.pdf, Table 3
(pp. 26–29), 9 group probits. Table 3 predicts P(non-filer) despite a
contradictory appendix coding. **Extract with PyMuPDF word positions, not
`pdftotext -layout`**, which scrambles the row-label alignment and silently
mis-assigns coefficients.

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

## Staged data awaiting placement in the shared store

**`ssa_flatseries/oasdi_sc_flatseries_table2_beneficiaries.json`** (1,520 rows)
and **`..._table1_population_shares.json`** (1,404 rows) — the OASDI-SC
*flattened time series*, 1999-12 through 2025-12, retrieved 2026-08-19. Table 2
carries `persons_oasdi_65_older_men` / `_women`, which summed **are the D6 age
margin**; Table 1 carries state population with the share receiving benefits.

These are raw data and **belong in the shared store, not in git** — they are
staged here only because ssa.gov cannot be reached from the cluster (its block is
on TLS fingerprint, so curl, .NET and hosted fetchers all get 403; only a real
browser engine works). Committing them is the transfer mechanism. **Move them to
`raw_data/SSA-OASDI-SC/` and re-run `01_fetch_residual_inputs.R` to register
them, then delete this directory** — see `../07_ssa_inputs_plan.md` task 2.

Verified against the per-year workbooks: both sources give 65+ beneficiaries of
45,808,776 (2017) and 52,052,807 (2022).
