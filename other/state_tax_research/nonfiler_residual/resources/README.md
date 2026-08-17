# Hand-transcribed resources (Stage D)

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

Cilke (1998) probit coefficients (`cilke_coefs.csv`) are NOT yet
transcribed — needed for v1b implementation, not for Stage D diagnostics.
Source: https://home.treasury.gov/system/files/131/WP-78.pdf.
