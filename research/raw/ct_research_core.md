# Connecticut Personal Income Tax — Core Structure, Tax Years 2017–2025

Research for Budget Lab Tax-Simulator `state-tax` module. Transcribed from Connecticut DRS Form CT-1040
instruction booklets (Tax Calculation Schedule and Tables A–E) for every tax year 2017 through 2025.
All nine year-booklets were downloaded and machine-checked directly (see "Verification method" and
"Sources used" at the end). Compiled 2026-07-17.

**Headline findings**

- CT parameters are fixed statutory nominal amounts — **nothing is inflation-indexed**. Verified: the
  numeric content of Table A (exemptions) and Table E (personal credit) is *bit-identical* in all nine
  booklets 2017–2025; Tables C and D changed exactly once (TY2024, PA 23-204); the rate schedule
  changed exactly once (TY2024).
- Tax is computed on **CT taxable income = CT AGI − personal exemption**, but every phase-out/add-back
  table (A, C, D, E) is keyed to **CT AGI** (Tax Calculation Schedule Line 1), not taxable income.
- Order of operations (Tax Calculation Schedule, identical structure all years; 2025 booklet p. 19):
  1. CT AGI (Form CT-1040, Line 5)
  2. − exemption (Table A) → CT taxable income
  3. Initial tax on taxable income (Table B rate schedule)
  4. + lowest-rate phase-out add-back (Table C, keyed to CT AGI)
  5. + tax recapture (Table D, keyed to CT AGI; contains ALL recapture tiers in one cumulative table)
  6. × (1 − Table E personal credit decimal, keyed to CT AGI) → CT income tax before credits
- **There are no dependent exemptions and no standard deduction.** The only exemption is the per-return
  personal exemption of Table A, which varies solely by filing status and CT AGI (CGS § 12-702; OLR
  2024-R-0130 describes the exemption system with no dependent component).
- MFS uses the single rate schedule but has its own (half-MFJ) exemption, Table C, and Table E parameters.
- "Qualifying widow(er)" (through ~2021 booklets) = "qualifying surviving spouse" (2022+) and always uses
  the MFJ column.
- The DRS tax tables (pre-computed tax with all exemptions/credits built in) cover CT AGI ≤ $102,000;
  above that filers use the Tax Calculation Schedule. A simulator should implement the schedule directly.

---

## 1. Rate schedule (Table B — "Initial Tax Calculation")

Applies to **CT taxable income** (Line 3 of the Tax Calculation Schedule).

### TY 2015–2023 rates (verified identical in the 2017, 2018, 2019, 2020, 2021, 2022, and 2023 booklets)

Rates: **3.0%, 5.0%, 5.5%, 6.0%, 6.5%, 6.9%, 6.99%** (seven brackets; in place since TY2015 per OLR
2025-R-0080, Table 9).

| Bracket lower bound → | Single & MFS | Head of Household | MFJ / QSS | Rate |
|---|---|---|---|---|
| 1 | $0 | $0 | $0 | 3.0% |
| 2 | $10,000 | $16,000 | $20,000 | 5.0% |
| 3 | $50,000 | $80,000 | $100,000 | 5.5% |
| 4 | $100,000 | $160,000 | $200,000 | 6.0% |
| 5 | $200,000 | $320,000 | $400,000 | 6.5% |
| 6 | $250,000 | $400,000 | $500,000 | 6.9% |
| 7 | $500,000 | $800,000 | $1,000,000 | 6.99% |

Published cumulative-tax form (2017 booklet, p. 49; identical text 2018–2023):

- Single/MFS: ≤$10,000 → 3.00%; $300 + 5.0% over $10,000; $2,300 + 5.5% over $50,000; $5,050 + 6.0%
  over $100,000; $11,050 + 6.5% over $200,000; $14,300 + 6.9% over $250,000; $31,550 + 6.99% over $500,000.
- MFJ/QSS: ≤$20,000 → 3.00%; $600 + 5.0% over $20,000; $4,600 + 5.5% over $100,000; $10,100 + 6.0%
  over $200,000; $22,100 + 6.5% over $400,000; $28,600 + 6.9% over $500,000; $63,100 + 6.99% over $1,000,000.
- HoH: ≤$16,000 → 3.00%; $480 + 5.0% over $16,000; $3,680 + 5.5% over $80,000; $8,080 + 6.0% over
  $160,000; $17,680 + 6.5% over $320,000; $22,880 + 6.9% over $400,000; $50,480 + 6.99% over $800,000.

### TY 2024–2025 rates (verified in the 2024 and 2025 booklets; identical in both)

**PA 23-204, § 376 (June Sp. Sess. 2023, effective TY2024)** cut the bottom two rates:
**3% → 2%** and **5% → 4.5%**. All bracket boundaries unchanged. Rates now:
**2.0%, 4.5%, 5.5%, 6.0%, 6.5%, 6.9%, 6.99%** (OLR 2024-R-0130 Table 3; OLR 2025-R-0080 Table 10:
"2024 to present").

Published cumulative-tax form (2025 booklet, p. 20; 2024 booklet identical):

- Single/MFS: ≤$10,000 → 2.00%; $200 + 4.5% over $10,000; $2,000 + 5.5% over $50,000; $4,750 + 6.0%
  over $100,000; $10,750 + 6.5% over $200,000; $14,000 + 6.9% over $250,000; $31,250 + 6.99% over $500,000.
- MFJ/QSS: ≤$20,000 → 2.00%; $400 + 4.5% over $20,000; $4,000 + 5.5% over $100,000; $9,500 + 6.0%
  over $200,000; $21,500 + 6.5% over $400,000; $28,000 + 6.9% over $500,000; $62,500 + 6.99% over $1,000,000.
- HoH: ≤$16,000 → 2.00%; $320 + 4.5% over $16,000; $3,200 + 5.5% over $80,000; $7,600 + 6.0% over
  $160,000; $17,200 + 6.5% over $320,000; $22,400 + 6.9% over $400,000; $50,000 + 6.99% over $800,000.

### 2026 and later

No enacted rate/bracket/exemption/credit-structure changes for TY2026 were found (checked 2026-07-17).
OLR 2025-R-0080 (June 16, 2025) lists the 2%–6.99% schedule as "2024 to present," and the CT FY26–27
budget acts signed in 2025 (and the 2026 adjustments per Tax Foundation's Jan 1 2026 survey and DRS
"2026 developments" page) contain no personal income tax rate or bracket changes. See Confidence & gaps.

---

## 2. Table A — Personal exemption

One per-return exemption; **no dependent exemptions**. Identical in every booklet 2017–2025
(hash-verified; 2017 booklet p. 48 and 2025 booklet p. 19 transcribed in full and equal).
Statutory source: CGS § 12-702.

| Filing status | Max exemption | Full exemption if CT AGI ≤ | Phase-out band (CT AGI) | Exemption = $0 when CT AGI > |
|---|---|---|---|---|
| Single | $15,000 | $30,000 | $30,000 – $44,000 | $44,000 |
| MFJ / QSS | $24,000 | $48,000 | $48,000 – $71,000 | $71,000 |
| MFS | $12,000 | $24,000 | $24,000 – $35,000 | $35,000 |
| Head of Household | $19,000 | $38,000 | $38,000 – $56,000 | $56,000 |

**Phase-out mechanics:** the exemption is reduced by **$1,000 for each $1,000 (or fraction thereof) of
CT AGI above the threshold**. The published table implements this as bands: "More Than X / Less Than or
Equal To X+$1,000 → exemption falls by $1,000 per row." Example rows (single, identical 2017 and 2025):
AGI ($30,000, $31,000] → $14,000; ($31,000, $32,000] → $13,000; …; ($43,000, $44,000] → $1,000;
over $44,000 → $0. Because the bands are "more than / less than or equal to," any fraction of a $1,000
step costs the full $1,000 of exemption — i.e., "or fraction thereof." (Exact band-table wording is the
primary source; the "$1,000 per $1,000 or fraction thereof" phrasing is the CGS § 12-702 formulation as
summarized by OLR 2024-R-0130, Table 2, which matches the DRS bands exactly.)

Full single-column table structure (all statuses follow the same $1,000-band pattern from their threshold
down to $0): 15 rows for single ($0–$30,000 → $15,000, then 14 declining rows), 24 rows of decline for
MFJ, 12 for MFS, 18 for HoH.

*Note for pre-2017 modeling:* the single-filer maximum was already $15,000 by TY2017 (verified). Earlier
phase-in steps (2016 and prior) were not researched here.

---

## 3. Table C — Lowest-rate phase-out add-back (3% bracket 2017–2023; 2% bracket 2024–2025)

DRS name: "Table C – 3% Tax Rate Phase-Out Add-Back" (2017–2023 booklets); renamed
"Table C – 2% (Tax Rate) Phase-Out Add-Back" (2024–2025). Keyed to **CT AGI** (for NR/PY filers,
CT-source income if greater). Entered on Tax Calculation Schedule Line 5.

Statutory mechanic (CGS § 12-700(a), as summarized in OLR reports): for CT AGI above the threshold, the
amount of taxable income taxed at the lowest rate is reduced — by $1,000 per $5,000 increment (or
fraction) for singles, $2,000 per $5,000 for MFJ, $1,000 per $2,500 for MFS, and $1,600 per $4,000 for
HoH — shifting that income into the second bracket. The published dollar add-back per increment equals
the shifted amount × (second rate − lowest rate): × 2.0pp through 2023, × 2.5pp from 2024.

### Parameters (published bands verified: 2017 booklet p. 50; hash-identical 2018–2023; 2025 booklet p. 21; hash-identical 2024)

| | Single | MFJ / QSS | MFS | HoH |
|---|---|---|---|---|
| Phase-out starts (CT AGI >) | $56,500 | $100,500 | $50,250 | $78,500 |
| Increment size | $5,000 | $5,000 | $2,500 | $4,000 |
| Add-back per increment, **2017–2023** | $20 | $40 | $20 | $32 |
| Max add-back, **2017–2023** (10 increments) | $200 | $400 | $200 | $320 |
| Add-back per increment, **2024–2025** | $25 | $50 | $25 | $40 |
| Max add-back, **2024–2025** | $250 | $500 | $250 | $400 |
| Fully phased out (max applies) at CT AGI > | $101,500 | $145,500 | $72,750 | $114,500 |

(OLR 2024-R-0130 Table 4 states the 2024 rule as "$25 per $5,000 of CT AGI over starting point" single,
"$50 per $5,000" MFJ, "$25 per $2,500" MFS, "$40 per $4,000" HoH, max $250/$500/$250/$400 — matching the
DRS bands.)

### Full published band table (2025 booklet p. 21; 2017–2023 versions identical except add-back column scaled by 20/25, 40/50, 20/25, 32/40)

Single (More Than / ≤ / add-back 2024–25 [2017–23]):
$0–$56,500: $0; $56,500–$61,500: $25 [$20]; $61,500–$66,500: $50 [$40]; $66,500–$71,500: $75 [$60];
$71,500–$76,500: $100 [$80]; $76,500–$81,500: $125 [$100]; $81,500–$86,500: $150 [$120];
$86,500–$91,500: $175 [$140]; $91,500–$96,500: $200 [$160]; $96,500–$101,500: $225 [$180];
over $101,500: $250 [$200].

MFJ/QSS: $0–$100,500: $0; then $5,000 bands $100,500→$145,500 at $50 [$40] per band; over $145,500:
$500 [$400].

MFS: $0–$50,250: $0; then $2,500 bands $50,250→$72,750 at $25 [$20] per band; over $72,750: $250 [$200].

HoH: $0–$78,500: $0; then $4,000 bands $78,500→$114,500 at $40 [$32] per band; over $114,500:
$400 [$320].

---

## 4–5. Table D — Tax recapture (ALL tiers in one cumulative table)

**Important structural fact:** DRS publishes a single "Table D – Tax Recapture" whose dollar amounts are
**cumulative across tiers** — there is no separate published "Table C recapture / Table D recapture"
split. 2017–2023 the table embeds two tiers; 2024–2025 it embeds three. Keyed to **CT AGI**; entered on
Tax Calculation Schedule Line 6. MFS uses the Single column. Statutory source: CGS § 12-700(a)(10)
(as amended by PA 23-204 § 376 for 2024+); cross-check OLR 2024-R-0130 Tables 5–6.

### TY 2017–2023 (verified: full transcription of 2017 booklet p. 51; hash-identical 2018–2023)

| Tier | Single & MFS | MFJ / QSS | HoH |
|---|---|---|---|
| **Tier 1** start (CT AGI >) | $200,000 | $400,000 | $320,000 |
| Tier 1 amount per increment | $90 per $5,000 | $180 per $10,000 | $140 per $8,000 |
| Tier 1 max (30 increments) | $2,700 | $5,400 | $4,200 |
| Tier 1 max reached at CT AGI > | $345,000 (flat to $500,000) | $690,000 (flat to $1,000,000) | $552,000 (flat to $800,000) |
| **Tier 2** start (CT AGI >) | $500,000 | $1,000,000 | $800,000 |
| Tier 2 amount per increment | $50 per $5,000 | $100 per $10,000 | $80 per $8,000 |
| Tier 2 max (9 increments) | $450 | $900 | $720 |
| **Combined maximum** | **$3,150** | **$6,300** | **$4,920** |
| Combined max reached at CT AGI > | $540,000 | $1,080,000 | $864,000 |

Published first/last rows (2017, single): ($200,000, $205,000] → $90; ($205,000, $210,000] → $180; …
($340,000, $345,000] → $2,610; ($345,000, $500,000] → $2,700; ($500,000, $505,000] → $2,750; …
($535,000, $540,000] → $3,100; over $540,000 → $3,150. (Tier 2 rows show $2,700 + k×$50.) MFJ and HoH
follow the same pattern with their increments. Note: the "single max $2,250 / $90 per $5,000" folk
description is **wrong** — the published tier-1 cap is $2,700 (it runs 200k→345k, 29 stepped rows plus
the plateau row, i.e., 30 increments).

Purpose (OLR): tier 1+2 phase out the benefit of all rates below 6.99% so that at the top the entire
CT AGI is effectively taxed at 6.99%.

### TY 2024–2025 (verified: full transcription of 2025 booklet p. 22; hash-identical 2024)

PA 23-204 added a **new lowest tier ("benefit recapture" of the 2024 rate cuts)** and left tiers 1–2
unchanged; all published amounts from $200,000 up (single) shift by the new tier's cap.

| Tier | Single & MFS | MFJ / QSS | HoH |
|---|---|---|---|
| **Tier 0 (new 2024)** start (CT AGI >) | $105,000 | $210,000 | $168,000 |
| Tier 0 amount per increment | $25 per $5,000 | $50 per $10,000 | $40 per $8,000 |
| Tier 0 max (10 increments) | $250 | $500 | $400 |
| Tier 0 max reached at CT AGI > | $150,000 (flat to $200,000) | $300,000 (flat to $400,000) | $240,000 (flat to $320,000) |
| **Tier 1** | as 2017–2023: $90/$5,000 over $200,000, +$2,700 | $180/$10,000 over $400,000, +$5,400 | $140/$8,000 over $320,000, +$4,200 |
| **Tier 2** | $50/$5,000 over $500,000, +$450 | $100/$10,000 over $1,000,000, +$900 | $80/$8,000 over $800,000, +$720 |
| **Combined maximum** | **$3,400** | **$6,800** | **$5,320** |
| Combined max reached at CT AGI > | $540,000 | $1,080,000 | $864,000 |

Published anchor rows (2025, single): ($105,000, $110,000] → $25; … ($145,000, $150,000] → $225;
($150,000, $200,000] → $250; ($200,000, $205,000] → $340 (= $250 + $90); … ($345,000, $500,000] →
$2,950; ($500,000, $505,000] → $3,000; … over $540,000 → $3,400.
MFJ: ($210,000, $220,000] → $50; … ($300,000, $400,000] → $500; ($400,000, $410,000] → $680; …
($690,000, $1,000,000] → $5,900; … over $1,080,000 → $6,800.
HoH: ($168,000, $176,000] → $40; … ($240,000, $320,000] → $400; ($320,000, $328,000] → $540; …
($552,000, $800,000] → $4,600; … over $864,000 → $5,320.

OLR 2024-R-0130 (Tables 5–6) confirms: tier 0 "eliminates the benefit of the tax rate reductions enacted
in 2023" for CT AGI above $105,000 / $210,000 / $168,000, maxing at $250 / $500 / $400 when income
exceeds $150,000 / $300,000 / $240,000; tiers 1–2 are stated exactly as the 2017–2023 parameters above.

*(Historical note: tier 2 dates from TY2015, when the 6.99% bracket was added — outside this window; both
tiers were already present in the 2017 booklet.)*

---

## 6. Table E — Personal tax credit (percentage of tax)

A credit equal to a **decimal fraction of the tax after add-back and recapture** (Tax Calculation
Schedule: Line 9 = Line 7 × Line 8; tax = Line 7 − Line 9). Keyed to **CT AGI** (Line 1), not taxable
income. Statutory source: CGS § 12-703.

**Identical in every booklet 2017–2025** (hash-verified; independently transcribed in full from the 2017
booklet p. 52 and the 2025 booklet p. 23 — every row equal). So one table serves all nine years.

Eligibility: the credit exists only for CT AGI **above** the filing-status minimum (below that, the
exemption ≥ AGI so tax is already zero) and **at or below** the top bound; filers with CT AGI above
$64,500 (single) / $100,500 (MFJ/QSS) / $52,500 (MFS) / $78,500 (HoH) get **no credit** (decimal .00).
Percentage steps: 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6,
5, 4, 3, 2, 1, 0.

Full table — CT AGI ranges are (More Than, Less Than or Equal To]:

| Credit | Single | MFJ / QSS | MFS | HoH |
|---|---|---|---|---|
| .75 | $15,000–$18,800 | $24,000–$30,000 | $12,000–$15,000 | $19,000–$24,000 |
| .70 | $18,800–$19,300 | $30,000–$30,500 | $15,000–$15,500 | $24,000–$24,500 |
| .65 | $19,300–$19,800 | $30,500–$31,000 | $15,500–$16,000 | $24,500–$25,000 |
| .60 | $19,800–$20,300 | $31,000–$31,500 | $16,000–$16,500 | $25,000–$25,500 |
| .55 | $20,300–$20,800 | $31,500–$32,000 | $16,500–$17,000 | $25,500–$26,000 |
| .50 | $20,800–$21,300 | $32,000–$32,500 | $17,000–$17,500 | $26,000–$26,500 |
| .45 | $21,300–$21,800 | $32,500–$33,000 | $17,500–$18,000 | $26,500–$27,000 |
| .40 | $21,800–$22,300 | $33,000–$33,500 | $18,000–$18,500 | $27,000–$27,500 |
| .35 | $22,300–$25,000 | $33,500–$40,000 | $18,500–$20,000 | $27,500–$34,000 |
| .30 | $25,000–$25,500 | $40,000–$40,500 | $20,000–$20,500 | $34,000–$34,500 |
| .25 | $25,500–$26,000 | $40,500–$41,000 | $20,500–$21,000 | $34,500–$35,000 |
| .20 | $26,000–$26,500 | $41,000–$41,500 | $21,000–$21,500 | $35,000–$35,500 |
| .15 | $26,500–$31,300 | $41,500–$50,000 | $21,500–$25,000 | $35,500–$44,000 |
| .14 | $31,300–$31,800 | $50,000–$50,500 | $25,000–$25,500 | $44,000–$44,500 |
| .13 | $31,800–$32,300 | $50,500–$51,000 | $25,500–$26,000 | $44,500–$45,000 |
| .12 | $32,300–$32,800 | $51,000–$51,500 | $26,000–$26,500 | $45,000–$45,500 |
| .11 | $32,800–$33,300 | $51,500–$52,000 | $26,500–$27,000 | $45,500–$46,000 |
| .10 | $33,300–$60,000 | $52,000–$96,000 | $27,000–$48,000 | $46,000–$74,000 |
| .09 | $60,000–$60,500 | $96,000–$96,500 | $48,000–$48,500 | $74,000–$74,500 |
| .08 | $60,500–$61,000 | $96,500–$97,000 | $48,500–$49,000 | $74,500–$75,000 |
| .07 | $61,000–$61,500 | $97,000–$97,500 | $49,000–$49,500 | $75,000–$75,500 |
| .06 | $61,500–$62,000 | $97,500–$98,000 | $49,500–$50,000 | $75,500–$76,000 |
| .05 | $62,000–$62,500 | $98,000–$98,500 | $50,000–$50,500 | $76,000–$76,500 |
| .04 | $62,500–$63,000 | $98,500–$99,000 | $50,500–$51,000 | $76,500–$77,000 |
| .03 | $63,000–$63,500 | $99,000–$99,500 | $51,000–$51,500 | $77,000–$77,500 |
| .02 | $63,500–$64,000 | $99,500–$100,000 | $51,500–$52,000 | $77,500–$78,000 |
| .01 | $64,000–$64,500 | $100,000–$100,500 | $52,000–$52,500 | $78,000–$78,500 |
| .00 | over $64,500 | over $100,500 | over $52,500 | over $78,500 |

---

## 7. Filing requirement

A resident must file Form CT-1040 if **any** of the following applies (2017 booklet p. 9; wording stable
through 2021 booklet; PE Tax Credit trigger added in 2018; current wording on DRS "Tax Information"
web page for TY2025):

1. CT income tax was withheld;
2. estimated CT tax payments (or a CT-1040 EXT payment) were made;
3. (2018+) a PE Tax Credit is claimed / had a PE tax credit;
4. the **Gross Income Test** is met;
5. the taxpayer had a federal alternative minimum tax liability; or
6. the taxpayer claims the CT EITC.

**Gross Income Test — unchanged for all years 2017–2025** ("You must file a Connecticut income tax
return if your gross income for the taxable year exceeds"):

| Filing status | Gross income threshold |
|---|---|
| Single | $15,000 |
| MFJ / QSS | $24,000 |
| MFS | $12,000 |
| Head of Household | $19,000 |

Directly verified in the 2017 (p. 9), 2018 (p. 9), 2019 (p. 10), 2020 (p. 8), and 2021 (p. 8) booklets
and on the DRS Tax Information page for TY2025. The 2022–2025 "instructions" PDFs no longer print the
Who-Must-File section (DRS moved it to the web page), but their Line 6 instruction confirms the same
amounts: "if the amount on Line 5 [CT AGI] is: $12,000 or less MFS; $15,000 or less single; $19,000 or
less HoH; or $24,000 or less MFJ/QSS, enter '0' on Line 6" (2022–2025 booklets, Line 6). The thresholds
equal the maximum personal exemptions, so no one below them owes tax.

**No federal-filer rule:** merely having filed a federal return does *not* trigger a CT filing
requirement — the six triggers above are exhaustive in the DRS instructions. "Gross income" is a broad
federal-style concept (all income not exempt from federal income tax, plus CT addition modifications;
includes income from all sources inside and outside CT).

---

## 8. Legislative change log within the window

| Effective TY | Change | Authority |
|---|---|---|
| 2017–2023 | No changes to rates, brackets, Tables A–E, or filing thresholds (all verified identical) | — |
| 2024 | Bottom rates 3%→2% and 5%→4.5%; Table C add-backs re-scaled ($20/$40/$20/$32 → $25/$50/$25/$40); new Table D tier 0 (rate-cut benefit recapture, starts $105k/$210k/$168k, max $250/$500/$400) | PA 23-204, § 376 (June Sp. Sess. 2023), amending CGS § 12-700; OLR 2024-R-0130 |
| 2025 | No changes (2025 booklet identical to 2024 in all core parameters) | 2025 CT-1040 instructions |
| 2026 | No enacted core-structure changes found as of 2026-07-17 | OLR 2025-R-0080; Tax Foundation Jan-2026 survey; DRS 2026 developments page |

(PA 23-204 also expanded retirement-income subtractions and the CT EITC to 40% of federal — outside core
structure, noted for completeness from OLR 2024-R-0130.)

---

## Verification method (which booklets were actually checked)

All nine instruction booklets, TY2017–TY2025, were downloaded as PDFs and machine-read (PyMuPDF text
extraction):

- **Fully transcribed by hand-inspection:** 2017 (pp. 47–52: TCS, Tables A–E, tax-table note) and 2025
  (pp. 19–23: TCS, Tables A–E). 2024 Table B was also read line-by-line.
- **Rate schedules (Table B):** the full "Less than or equal to … plus x% of the excess over …" text was
  extracted from every booklet 2017–2025 and compared; 2017–2023 identical, 2024–2025 identical.
- **Tables A, C, D, E:** every dollar/decimal token on each table's page was extracted from every booklet
  and hashed. Results: Table A — one hash for all nine years; Table E — one hash for all nine years;
  Tables C and D — one hash 2017–2023 and a second hash 2024–2025. Table titles per year also checked
  (Table C renamed "2% Phase-Out Add-Back" in 2024).
- **Filing thresholds:** Gross Income Test text extracted from 2017, 2018, 2019, 2020, 2021 booklets;
  Line 6 zero-tax text from 2022–2025 booklets; DRS web page for TY2025.

So no year in any table above is inferred from statute alone; year-ranges shown (e.g., "2018–2023: same")
reflect direct checks of each year's booklet.

---

## Sources used

Primary (DRS Form CT-1040 instruction booklets — the TCS and Tables A–E pages cited above):

- TY2017: https://portal.ct.gov/-/media/drs/forms/1-2017/income/booklets/ct1040onlineinstructionbooklet1217pdf.pdf (downloaded via NBER mirror: https://taxsim.nber.org/historical_state_tax_forms/CT/2017/ct-1040_online_instruction_booklet_1217.pdf)
- TY2018: https://portal.ct.gov/-/media/DRS/Forms/1-2018/Income/CT-1040-Online-Booklet_1218.pdf
- TY2019: https://portal.ct.gov/-/media/DRS/Forms/2019/Income/CT-1040-Online-Booklet_1219.pdf
- TY2020: https://portal.ct.gov/-/media/DRS/Forms/2020/Income/CT-1040-Online-Booklet_0221.pdf
- TY2021: https://portal.ct.gov/-/media/DRS/Forms/2021/Income/CT-1040-Online-Booklet_1221.pdf
- TY2022: https://portal.ct.gov/-/media/DRS/Forms/2022/Income/2022-CT-1040-Instructions_1222.pdf
- TY2023: https://portal.ct.gov/-/media/DRS/Forms/2023/Income/2023-CT-1040-Instructions_1223.pdf
- TY2024: https://portal.ct.gov/-/media/drs/forms/2024/income/2024-ct-1040-instructions_1224.pdf
- TY2025: https://portal.ct.gov/-/media/drs/forms/2025/income/2025-ct-1040-instructions_1225.pdf
- TY2024 standalone Tax Calculation Schedule (Form CT-1040 TCS; used for the CT-AGI asterisk footnote:
  "Form CT-1040NR/PY filers must use income from Connecticut sources if it exceeds Connecticut adjusted
  gross income."): https://portal.ct.gov/-/media/drs/forms/2024/income/ct-1040-tcs_1224.pdf
- DRS "Tax Information" (resident income tax; TY2025 Who Must File / Gross Income Test):
  https://portal.ct.gov/drs/individuals/resident-income-tax/tax-information

Quasi-primary cross-checks (CT OLR, cga.ct.gov):

- OLR 2024-R-0130, "OLR Backgrounder: A Guide to Connecticut's Personal Income Tax" (Nov. 7, 2024) —
  exemptions, brackets, phase-out, both benefit-recapture provisions, credit bounds, statutory cites:
  https://cga.ct.gov/2024/rpt/pdf/2024-R-0130.pdf
- OLR 2025-R-0080, "Connecticut Income Tax Rates and Brackets Since 1991" (June 16, 2025) — rate history,
  confirms 2015–2023 and 2024-to-present schedules: https://cga.ct.gov/2025/rpt/pdf/2025-R-0080.pdf

Secondary (used only to confirm the absence of 2026 changes; no values transcribed from them):

- Tax Foundation, "2026 State Tax Changes Taking Effect January 1st":
  https://taxfoundation.org/research/all/state/2026-state-tax-changes/
- DRS "2026 developments" page (located via search):
  https://portal.ct.gov/drs/miscellaneous-taxes/other-tax-page/state-tax-developments/2026-developments

---

## Confidence and gaps

**High confidence (multiple primary sources, machine-verified):** rate schedules for all years; Tables
A, C, D, E band values for all years; the 2024 PA 23-204 changes; filing thresholds 2017–2021 and 2025;
constancy (non-indexation) of all parameters.

**Flags / lower-confidence items:**

1. **Statutory phrasing vs. published bands.** The "reduced by $1,000 for each $1,000 (or fraction
   thereof)" (exemption) and "reduced by $1,000 per $5,000 (or fraction thereof)" (3%/2% bracket)
   formulations come from CGS §§ 12-702 / 12-700 as summarized by OLR; I did not read the raw statute
   text. The DRS "More Than / Less Than or Equal To" band tables — which are what should be implemented —
   were transcribed directly and are mathematically equivalent to the or-fraction-thereof rule.
2. **Recapture keyed to CT AGI, not taxable income.** OLR 2024-R-0130's prose says recapture applies to
   "taxable incomes exceeding" the thresholds, but the DRS tables are explicitly keyed to CT AGI (TCS
   Line 1). Follow the DRS tables (CT AGI).
3. **Filing thresholds for 2022–2024** are not printed in those years' booklets (section moved to the DRS
   website, whose archived per-year versions were not retrieved). They are corroborated by the identical
   Line 6 zero-tax amounts in the 2022–2024 booklets, the unchanged statute, and identical 2021/2025
   endpoints. Risk of an unnoticed change: negligible.
4. **TY2026:** "no changes" is based on OLR's June 2025 report and secondary surveys as of 2026-07-17,
   not on a TY2026 booklet (not yet published). Re-verify when the 2026 forms appear (late 2026).
5. **NR/PY filers:** this document covers residents. For CT-1040NR/PY, Tables C/D/E use the greater of
   CT AGI or CT-source income (per the TCS footnote), and tax is apportioned; not further researched.
6. **Pre-2017 phase-in of the single exemption/credit schedule** (relevant only if the simulator is
   extended before TY2017) was not researched.
