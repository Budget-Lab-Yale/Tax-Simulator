# Minnesota Individual Income Tax — Core Mechanics Research Packet (TY2017–TY2025)

**Prepared:** 2026-07-23, for Tax-Simulator state-tax module encoding.
**Method:** Year-specific Form M1 instruction booklets and schedules (M1M, M1MA, M1SA, M1NC, M1WFC/M1CWFC, M1CD, M1MT) for TY2017–TY2025 were downloaded as PDFs (from revenue.state.mn.us where live; NBER historical archive `taxsim.nber.org/historical_state_tax_forms/MN/{year}/` for files DOR no longer posts) and text-extracted with `pdftotext`. Every parameter below was transcribed from the year's own form/instructions unless flagged. Two exceptional primary sources: the MN DOR Tax Research Division **"Inflation-Adjusted Amounts in Minnesota Statutes"** memos (TY2024, TY2025) which map every indexed dollar amount to its statute section, and the DOR **"Income Tax Calculations" algorithm sheets** (TY2018, TY2021) which give exact bracket math. Statute cites (Minn. Stat. ch. 290) follow the DOR memos' section mapping. Cross-checks: MN House Research WFC/EITC publications; PolicyEngine-US parameter tree (coverage check only).

**Source inventory (all fetched and read):**

| TY | M1 booklet | Key schedules read |
|---|---|---|
| 2017 | NBER `MN/2017/m1_inst_17.pdf` | M1 form, M1M, M1MA (+DOR "Marriage Credit Computation Steps TY2017"), M1WFC (+"WFC Computation Steps TY2017"), M1CD, M1MT |
| 2018 | DOR `files/2023-12/m1_inst_18.pdf` | M1 form, **M1NC** (`files/2023-01/m1nc_18.pdf`, Rev. 1/23), M1M, M1SA, M1MA, M1WFC (+steps), M1CD, "Income Tax Calculations TY2018" |
| 2019 | DOR `files/2024-01/m1_inst_19.pdf` | M1M (`m1m_19_0`), M1SA, M1MA, M1WFC (+steps), M1CD |
| 2020 | DOR `files/2023-12/m1_inst_20.pdf` | M1M (`files/2024-01/m1m_20.pdf`), M1SA, M1MA, M1WFC, M1CD |
| 2021 | DOR `files/2023-12/m1_inst_21.pdf` | M1M, M1SA, M1MA, M1WFC (+steps), M1CD, "Income Tax Calculations TY2021" |
| 2022 | DOR `files/2024-02/m1-inst-22.pdf` | M1M (`files/2024-01/m1m-22.pdf`), M1NC-22, M1SA, M1MA (+steps), M1WFC, M1CD |
| 2023 | DOR `files/2025-06/m1-inst-23.pdf` | M1 form, M1M (m1m-23), M1CWFC (m1cwfc-23), M1MA, M1MT, M1CD |
| 2024 | DOR `files/2026-01/m1-inst-24.pdf` | M1 form, M1CWFC, M1MA, M1CD, DOR TY2024 inflation memo |
| 2025 | NBER `MN/2025/m1-inst-25_0.pdf` | M1M (m1m-25), M1CWFC, M1SA, M1MA, M1MT, M1R, M1CD, DOR TY2025 inflation memo |

---

## 1. Starting point and return structure by year

Verified from the Form M1 itself each year (line-1 text transcribed):

| TY | M1 line 1 | Deduction stack | Citation |
|---|---|---|---|
| 2017 | **Federal TAXABLE income** ("from line 43 of federal Form 1040...") | Federal std/itemized deduction and personal exemptions flow through federal taxable income; MN addbacks on M1 lines 2–3 (state income/sales tax; M1M disallowed-itemized and exemption-phase-out additions, see §5–6) | 2017 Form M1 line 1 |
| 2018 | **Federal ADJUSTED GROSS income** ("from line 7 of federal Form 1040") | MN's OWN standard deduction (pre-TCJA amounts) or MN itemized (new Schedule M1SA) on line 4; MN personal+dependent exemptions ($4,150) on line 5; M1NC nonconformity adjustment folds into line 2 additions / line 7 subtractions via M1M | 2018 Form M1 lines 1–5 |
| 2019–2025 | **Federal ADJUSTED GROSS income** (1040 line 11 in recent years) | MN standard deduction or Schedule M1SA itemized (line 4); MN **dependent-only** exemptions (line 5); state income tax refund subtraction (line 6); M1M/M1MB other additions/subtractions (lines 2, 7) | Form M1 each year, lines 1–9 |

**Correction to prior assumption:** the switch from federal-taxable-income start to federal-AGI start happened for **TY2018** (not 2019). The 2018 restructuring was forced by TCJA nonconformity: the federal 1040 no longer produced a pre-TCJA taxable income, so MN rebuilt its own deduction/exemption stack on the M1 starting from (TCJA-definition) FAGI. The 2019 conformity act (Laws 2019, 1st Spec. Sess. ch. 6) then kept the FAGI start permanently (Minn. Stat. 290.01 subd. 19 defines net income starting from FAGI) and replaced the pre-TCJA exemption structure with the dependent exemption + MN standard deduction described below.

- Minnesota taxable income (M1 line 9, 2018+; line 8 in 2017) = line 1 + additions − subtractions.
- Tax from table if MTI < $90,000 ($86,800 in the 2025 booklet text), else rate schedule. Tax table uses $100-bracket midpoints; first $20 of taxable income has zero tax (DOR "Income Tax Calculations" sheets TY2018/TY2021, identical mechanics both years). Encoding: use the continuous rate schedule; the midpoint rounding is a ≤ few-dollar artifact.
- Filing statuses = federal filing statuses (1040-coupled; MFS uses half-MFJ brackets — verified in every rate schedule below). Qualifying widow(er)/surviving spouse = MFJ amounts everywhere.

## 2. TY2018 nonconformity year and Schedule M1NC

**Legal posture:** For TY2018 Minnesota law referenced the IRC **as amended through December 16, 2016** (pre-TCJA), so MN did not conform to TCJA for 2018. The 2019 omnibus (enacted May 31, 2019) **retroactively** updated the reference to the IRC as amended through **December 31, 2018** — i.e., MN retroactively adopted most TCJA *AGI-level* provisions for TY2018, which is why the archived 2018 Schedule M1NC (Rev. 1/23) has most lines marked "This line intentionally left blank … These lines were required prior to Minnesota tax law changes enacted May 31, 2019" (2018 M1NC instructions). The surviving 2018 M1NC (post-retroactivity revision) covers only post-Dec-31-2018 federal acts affecting 2018 (TCDTR and CARES): tuition & fees (line 8), excess business loss/NOL (line 15a, CARES), TCDTR business items (line 26), plus a handful of items MN still did not adopt for 2018 (bicycle commuting reimbursements, moving expenses via M1MOVE, student-loan discharge on death/disability, 529→K-12 earnings, deferred qualified equity grants, opportunity-zone deferral, 965/GILTI/FDII treatment, entertainment-expense deduction restoration, etc.). Line 32 recomputes AGI-cliff items (taxable SS via a MN provisional-income worksheet, IRA deduction, student loan interest, rental loss allowance) using MN-adjusted AGI. M1NC net adjustment flows to Schedule M1M line 13 (addition) or line 40 (subtraction). Source: 2018 Schedule M1NC + instructions, `revenue.state.mn.us/sites/default/files/2023-01/m1nc_18.pdf`.

**What the 2018 M1 did with the TCJA-suspended items** — MN kept its own pre-TCJA-style structure via its OWN lines rather than via M1NC:
- **MN standard deduction 2018** (M1 line 4, Standard Deduction Table): Single $6,500 (+$1,600 per aged/blind box, i.e. 1 box $8,100, 2 boxes $9,700); MFJ/QW $13,000 (+$1,300 per box up to $18,200); MFS $6,500 (+$1,300); HoH $9,550 (+$1,600). These are the pre-TCJA-law projected 2018 federal amounts. (2018 booklet, Standard Deduction table for Line 4, p. 11.)
- **MN itemized 2018** (new Schedule M1SA): pre-TCJA federal Schedule A replica — medical over **10%** of AGI; **state/local income or general sales taxes deductible on line 5 with NO $10k cap** but then **added back in full on line 27**; real estate + personal property taxes **uncapped**; mortgage interest; investment interest; charitable; casualty/theft (M1CAT); unreimbursed employee business expenses + misc over 2% AGI (M1UE); other misc. **Own Pease limitation** on line 28 if MN AGI > $190,050 ($95,025 MFS). (2018 Schedule M1SA, lines 1–30.)
- **MN personal + dependent exemptions 2018** (M1 line 5): $4,150 × (filer + spouse + federal dependents), phased out 2% per $2,500 of AGI ($1,250 MFS) over thresholds MFJ/QW $285,050, HoH $237,550, Single $190,050, MFS $142,525; fully gone once excess > $122,500. (2018 booklet, Worksheet for Line 5.)
- 2018 filing statuses/AGI itself = TCJA-law FAGI from the actual 2018 1040.

**Recommended encoding for 2018:** our federal calculator produces TCJA-law FAGI for 2018, which is exactly the 2018 M1 line 1 input. Encode 2018 as: FAGI start + MN 2018 standard deduction (with aged add-ons) or MN itemized (federal pre-TCJA-style Schedule A base is not recoverable from a TCJA-law calculator — approximate MN itemized with our federal itemized components: property taxes UNCAPPED, income/sales tax excluded (addback), mortgage/charitable/medical-over-10%, plus misc-2% items if available in the PUF (they are: e19200-era fields exist for 2015-law variables; for our projected data treat misc as zero) + 2018 Pease at $190,050) + $4,150 exemptions with the phase-out above. **Skip M1NC entirely** (its surviving items are niche business/foreign provisions; the big-ticket pre-TCJA restorations are handled by MN's own lines). This is exact for the mass of filers except the small M1NC population — document as a known difference.

## 3. Rates and brackets, TY2017–TY2025

Four rates all years. **Second-tier rate cut 7.05% → 6.80% effective TY2019** (2019 booklet rate schedules print 6.80%; 2018 algorithm sheet prints 7.05% — the change year is 2019, verified). Rates: **5.35% / 7.05% (6.80% from 2019) / 7.85% / 9.85%**. Bracket thresholds (lower bound of each bracket above the first), transcribed from each year's booklet rate-schedule page (2018 from the DOR TY2018 algorithm sheet; 2021 cross-checked against the TY2021 algorithm sheet; 2024–2025 cross-checked against the DOR inflation memos, Minn. Stat. 290.06 subd. 2c):

**Single**

| TY | 2nd bracket starts | 3rd | 4th |
|---|---|---|---|
| 2017 | $25,390 | $83,400 | $156,900 |
| 2018 | $25,890 | $85,060 | $160,020 |
| 2019 | $26,520 | $87,110 | $161,720 |
| 2020 | $26,960 | $88,550 | $164,400 |
| 2021 | $27,230 | $89,440 | $166,040 |
| 2022 | $28,080 | $92,230 | $171,220 |
| 2023 | $30,070 | $98,760 | $183,340 |
| 2024 | $31,690 | $104,090 | $193,240 |
| 2025 | $32,570 | $106,990 | $198,630 |

**Married filing jointly / qualifying widow(er)-surviving spouse**

| TY | 2nd | 3rd | 4th |
|---|---|---|---|
| 2017 | $37,110 | $147,450 | $261,510 |
| 2018 | $37,850 | $150,380 | $266,700 |
| 2019 | $38,770 | $154,020 | $269,010 |
| 2020 | $39,410 | $156,570 | $273,470 |
| 2021 | $39,810 | $158,140 | $276,200 |
| 2022 | $41,050 | $163,060 | $284,810 |
| 2023 | $43,950 | $174,610 | $304,970 |
| 2024 | $46,330 | $184,040 | $321,450 |
| 2025 | $47,620 | $189,180 | $330,410 |

**Married filing separately** (= exactly half the MFJ thresholds in every year, verified):

| TY | 2nd | 3rd | 4th |
|---|---|---|---|
| 2017 | $18,560 | $73,730 | $130,760 |
| 2018 | $18,930 | $75,190 | $133,350 |
| 2019 | $19,385 | $77,010 | $134,505 |
| 2020 | $19,705 | $78,285 | $136,735 |
| 2021 | $19,905 | $79,070 | $138,100 |
| 2022 | $20,525 | $81,530 | $142,405 |
| 2023 | $21,975 | $87,305 | $152,485 |
| 2024 | $23,165 | $92,020 | $160,725 |
| 2025 | $23,810 | $94,590 | $165,205 |

**Head of household**

| TY | 2nd | 3rd | 4th |
|---|---|---|---|
| 2017 | $31,260 | $125,600 | $209,200 |
| 2018 | $31,880 | $128,090 | $213,360 |
| 2019 | $32,650 | $131,190 | $214,980 |
| 2020 | $33,190 | $133,360 | $218,540 |
| 2021 | $33,520 | $134,700 | $220,730 |
| 2022 | $34,570 | $138,890 | $227,600 |
| 2023 | $37,010 | $148,730 | $243,720 |
| 2024 | $39,010 | $156,760 | $256,880 |
| 2025 | $40,100 | $161,130 | $264,050 |

Indexation: brackets re-based by the 2019 act (statutory year 2019 per the DOR inflation memos) and indexed annually using chained CPI (Minn. Stat. 270C.22); rounded to nearest $10.

## 4. Minnesota standard deduction (TY2018+) and its limitation

Statute: Minn. Stat. 290.0123. Amounts from each booklet's Standard Deduction Table for Line 4 (2018 covered in §2):

| TY | Single / MFS | MFJ/QSS | HoH | Aged/blind add-on: Single/HoH (per box) | MFJ/MFS/QSS (per box) | Citation |
|---|---|---|---|---|---|---|
| 2018 | $6,500 | $13,000 | $9,550 | $1,600 | $1,300 | 2018 booklet p. 11 |
| 2019 | $12,200 | $24,400 | $18,350 | $1,650 | $1,300 | 2019 booklet Line 4 table |
| 2020 | $12,400 | $24,800 | $18,650 | $1,650 | $1,300 | 2020 booklet |
| 2021 | $12,525 | $25,050 | $18,800 | $1,650 | $1,300 | 2021 booklet |
| 2022 | $12,900 | $25,800 | $19,400 | $1,700 | $1,350 (UNVERIFIED — add-on inferred; table shows single 1 box = $14,600? not transcribed) | 2022 booklet |
| 2023 | $13,825 | $27,650 | $20,800 | $1,850 | $1,450 | 2023 booklet Line 4 dependent worksheet step 4 |
| 2024 | $14,575 | $29,150 | $21,900 | $1,950 | $1,550 | 2024 booklet; DOR TY2024 inflation memo (290.0123 subd. 2) |
| 2025 | $14,950 | $29,900 | $22,500 | $2,000 | $1,550 | 2025 booklet; DOR TY2025 inflation memo |

Note the 2019–2023 amounts track the *federal* TCJA standard deduction levels but are separately MN-indexed from a 2019 statutory base; from 2024 they visibly exceed federal ($14,575 vs $14,600 federal single — close but distinct series; 2025 $14,950 vs federal $15,000). The DOR TY2024 memo notes the 2023 tax bill contained a **drafting error using the wrong inflation base year** for the standard deduction/aged-blind/dependent amounts; the memo's amounts "assume a legislative solution in 2024" (which was enacted) — the booklet amounts above are the operative ones.

**Dependent-filer standard deduction** (Worksheet for Line 4 — Dependent Standard Deduction): greater of the minimum or earned income + increment, capped at the regular standard deduction (+aged/blind add-ons):

| TY | Minimum | Earned-income increment |
|---|---|---|
| 2019–2021 | $1,100 | $350 |
| 2022 | $1,150 | $350 |
| 2023 | $1,200 | $350 |
| 2024 | $1,300 | $400 |
| 2025 | $1,250 | $350 |

(The non-monotonic 2024→2025 step is the drafting-error base-year switch: 2024 amounts were computed off the 2019 statutory base, 2025 off the re-based 2023 statutory amounts — both as printed in the booklets/DOR memos.)

**High-income limitation — applies to BOTH the standard deduction (290.0123 subd. 5) and itemized deductions (290.0122 subd. 2), identical thresholds:**

*TY2019–2022 mechanics* (Worksheet for Line 4 — Standard Deduction Limitation; M1SA line 26 worksheet): reduction = **lesser of 3% of (AGI − threshold) or 80% of the deduction** (for itemized: 80% of the deduction excluding medical, investment interest, and casualty/theft — standard Pease carve-outs; UNVERIFIED for MN whether the carve-out list matches federal Pease exactly — the M1SA worksheet was not fully transcribed).

| TY | Threshold (all statuses except MFS) | MFS |
|---|---|---|
| 2019 | $194,650 | $97,325 |
| 2020 | $197,850 | $98,925 |
| 2021 | $199,850 | $99,925 |
| 2022 | $206,050 | $103,025 |

*TY2023+ mechanics* (2023 act; Worksheets A and B for Line 4; same for M1SA): two-tier — reduction = 3% of (min(AGI, T2) − T1) + **10%** of (AGI − T2), capped at 80% of the deduction; if AGI > the Worksheet-B trigger, flat 80% reduction regardless:

| TY | T1 (MFS = half) | T2 (MFS = half) | Worksheet-B trigger (all statuses) |
|---|---|---|---|
| 2023 | $220,650 ($110,325) | $304,970 ($152,485) | $1,000,000 |
| 2024 | $232,500 ($116,250) | $321,350 ($160,675) | $1,053,750 |
| 2025 | $238,950 ($119,475) | $330,300 ($165,150) | $1,083,150 |

(2023 booklet Worksheet A/B for Line 4; DOR inflation memos 290.0122 subd. 2 / 290.0123 subd. 5. Note the T2 amounts equal the top bracket threshold for MFJ.)

**The famous 2019–2021 DOR limitation error:** DOR announced (fall 2021) that the standard-deduction-limitation worksheet in the originally-printed 2019 and 2020 M1 instructions was wrong, requiring adjustment letters/bills to roughly 45k–100k returns. The archived booklet PDFs used here are **corrected revisions** (both the 2019 and 2020 worksheets as archived compute reduction = min(3% excess, 80% of deduction), consistent with statute). Encode the statute (80% cap); no form-vs-statute conflict remains in the corrected forms. UNVERIFIED: exact text of the original erroneous worksheet (immaterial for encoding).

**MFS itemizer coupling:** if one MFS spouse itemizes (files M1SA), the other must also (may not claim standard deduction) — every booklet 2018–2025. **MN itemizer choice is fully decoupled from the federal choice from 2019 (and effectively 2018):** "You may itemize deductions on your Minnesota income tax return even if you claimed the standard deduction on your federal income tax return" (2019 booklet What's New; same in later years). → `item_coupling = 0` from 2018 on. 2017: federal itemization status flows through federal taxable income (coupled by construction).

## 5. Minnesota itemized deductions (Schedule M1SA, 2018+) and 2017 addback treatment

**2019–2025 Schedule M1SA components** (2019 schedule transcribed in full; 2025 spot-checked identical structure):
1. Medical/dental over **10% of AGI** (line 1–4; still 10% in 2025 — MN did not adopt the federal 7.5% floor).
2. Taxes: real estate (line 5) + personal property (line 6), **combined capped at $10,000 ($5,000 MFS)** (line 8), plus "other taxes" (line 9, uncapped — foreign income taxes etc.). **State/local income taxes and general sales taxes are NOT deductible at all** (no line for them from 2019 on; the 2018-only structure deducted-then-added-back, §2).
3. Interest: home mortgage interest + points + investment interest (lines 11–14) — federal definitions.
4. Charitable: cash, non-cash, carryover (lines 15–17; AGI-percentage limits follow the M1SA instructions/IRC).
5. Casualty/theft losses via Schedule M1CAT (line 19) — MN allows **non-federally-declared-disaster** personal casualty losses (10%-of-AGI floor per M1SA instructions).
6. Unreimbursed employee business expenses (Schedule M1UE) + other misc, over **2% of AGI** (lines 20–23) — TCJA suspension NOT adopted.
7. Other miscellaneous deductions (line 24, no floor — gambling losses etc.).
8. Total (line 25) minus limitation (line 26; same thresholds/mechanics as the standard-deduction limitation, §4) = line 27 → M1 line 4.

**2017 treatment (no M1SA):** federal itemized deductions flowed through federal taxable income; the M1 added back (a) state income tax or sales tax deduction (M1 line 2 worksheet) and (b) **Schedule M1M line 1 "Itemized deduction limitation"** — because MN imposed its own tighter Pease: if federal AGI > **$186,350 ($93,175 MFS, same threshold all other statuses)**, addback = [lesser of 3% of AGI excess or 80% of affected itemized deductions] minus the federal Pease disallowance already reflected — i.e., MN's Pease started at $186,350 while federal started at $261,500/$313,800; the worksheet computes the incremental disallowance (2017 M1M Worksheet for Line 1). Also M1M line 2: exemption phase-out addback (§6).

## 6. Exemptions

**2017 (flow-through + addback):** federal personal/dependent exemptions ($4,050) came through federal taxable income. Schedule M1M line 2 added back the exemption amount phased out under MN's **lower** thresholds: complete worksheet if AGI > MFJ $279,500 / HoH $232,900 / Single $186,350 / MFS $139,750 (2017 M1M instructions, Line 2) — i.e., MN disallowed 2% per $2,500 over these thresholds net of the federal phase-out (which began at $313,800/$287,650/$261,500/$156,900).

**2018 (own exemptions):** $4,150 × (filer + spouse + dependents), phase-out per §2.

**2019+ (dependent exemptions only, Minn. Stat. 290.0121):** Worksheet for Line 5 = (number of federal-definition dependents) × amount, reduced by 2% for each $2,500 ($1,250 MFS) or fraction thereof by which FAGI exceeds the threshold; zero once excess > $122,500 ($61,250 MFS):

| TY | Per-dependent | MFJ/QSS threshold | HoH | Single | MFS | Citation |
|---|---|---|---|---|---|---|
| 2019 | $4,250 | $291,950 | $243,300 | $194,650 | $145,975 | 2019 booklet Worksheet for Line 5 |
| 2020 | $4,300 | $296,750 | $247,300 | $197,850 | $148,375 | 2020 booklet |
| 2021 | $4,350 | $299,750 | $249,800 | $199,850 | $149,875 | 2021 booklet |
| 2022 | $4,450 | $309,050 | $257,550 | $206,050 | $154,525 | 2022 booklet |
| 2023 | $4,800 | $330,950 | $275,800 | $220,650 | $165,475 | 2023 booklet |
| 2024 | $5,050 | $348,850 | $290,700 | $232,550 | $174,425 | 2024 booklet; DOR memo (290.0121 subd. 1–2) |
| 2025 | $5,200 | $358,550 | $298,800 | $239,050 | $179,275 | 2025 booklet; DOR memo |

(Note the Single threshold equals the deduction-limitation T1 in 2019–2023 but diverges in 2024–2025 — the exemption thresholds stayed on the 2019 statutory base.)

## 7. Social Security subtraction (Minn. Stat. 290.0132 subd. 26)

**Regime 1, TY2017–TY2022 ("alternate"/sliding subtraction, enacted 2017):** subtraction = min(taxable SS + taxable Tier-1 RRB portion cap logic, max amount − 20% × (provisional income − threshold)), where "provisional income" = MN-modified provisional income (AGI excluding taxable SS + 50% of gross SS + tax-exempt interest, minus certain Schedule 1 adjustments — per the year's M1M Worksheet). Parameters per year (M1M worksheet for the SS line; maxima and thresholds both statutory-indexed):

| TY | Max: MFJ/QW | Single/HoH | MFS | Phase-out threshold: MFJ/QW | Single/HoH | MFS | Source |
|---|---|---|---|---|---|---|---|
| 2017 | $4,500 | $3,500 | $2,250 | $77,000 | $60,200 | $38,500 | 2017 M1M line 40 worksheet |
| 2018 | $4,500 | $3,500 | $2,250 | $78,530 | $61,400 | $39,270 | 2018 M1M worksheet |
| 2019 | $5,150 | $4,020 | $2,575 | $78,180 | $61,080 | $39,090 | 2019 M1M line 12 worksheet |
| 2020 | $5,240 | $4,090 | $2,620 | $79,480 | $62,090 | $39,740 | 2020 M1M worksheet |
| 2021 | $5,290 | $4,130 | $2,645 | $80,270 | $62,710 | $40,135 | 2021 M1M worksheet |
| 2022 | $5,450 | $4,260 | $2,725 | $82,770 | $64,670 | $41,385 | 2022 M1M worksheet |

(2019 threshold dip vs 2018 is as printed — the 2019 act re-based indexation. Phase-out rate 20% in all years. Subtraction also capped at taxable SS benefits net of the Tier-1 RRB amount separately subtracted.)

**Regime 2, TY2023+ (HF1938 / 2023 omnibus): "Simplified" full subtraction + greater-of election.** Subtraction = **greater of**:
- *Simplified method:* 100% of taxable SS (1040 line 6b), reduced by **10% for each $4,000 ($2,000 MFS), or fraction thereof**, of AGI above the threshold (fully gone once AGI ≥ threshold + $40,000/$20,000):

| TY | MFJ/QSS | Single/HoH | MFS |
|---|---|---|---|
| 2023 | $100,000 | $78,000 | $50,000 |
| 2024 | $105,380 | $82,190 | $52,690 |
| 2025 | $108,320 | $84,490 | $54,160 |

- *Alternative method:* the old regime-1 sliding subtraction with parameters **frozen (not indexed)** at: max $5,840 / $4,560 / $2,920; provisional-income thresholds $88,630 / $69,250 / $44,315 (MFJ / Single-HoH / MFS), 20% phase-out. (2023/2025 M1M line 12 worksheets; DOR inflation memos "Alternate Subtraction … Not Indexed".)

If AGI ≤ the simplified threshold, the full taxable SS amount is entered directly (no worksheet). Sources: 2023 M1M instructions Line 12; 2025 M1M Worksheet for line 12 (steps 1–29); DOR TY2024/TY2025 memos (290.0132 subd. 26).

## 8. Other Schedule M1M additions and subtractions

**Additions (2019+ M1M lines 1–10; 2017 lines 1–17):**
- **Non-Minnesota municipal bond interest** and non-MN exempt-interest dividends (M1M lines 1–2, all years). MN-source munis exempt (no addback).
- Expenses attributable to non-MN-taxed income; capital-gain portion of lump-sum distributions (Form 4972); M1HOME recapture.
- 2017–2018 only: itemized-limitation and exemption-phase-out addbacks (§5–6); domestic production activities deduction (2017 line 8); pass-through state income taxes (2017 line 7; later years this addback lives on M1SA/M1M instructions for pass-throughs).
- **Bonus depreciation and section 179 addbacks:** 80% of federal bonus depreciation (and, pre-2020, 80% of excess §179 expensing) added back in the year claimed, then subtracted **one-fifth per year over the following five years** (2017 M1M lines 5–6 additions, 21–22 subtractions; business items moved to **Schedule M1MB** from TY2023). MN conformed to full federal §179 expensing for property placed in service in TY2020+ (2020 act) — bonus-depreciation addback continues through 2025. **Known-difference item for us: document, model the 80%/5-year mechanics only if depreciation detail exists in the data (it does not, cleanly) — recommend documenting as structural difference.**
- TY2024+: Net operating loss / business items on M1MB; M1 line 2 = M1M line 10 + M1MB line 9.

**Subtractions (2019+ M1M lines 11+; 2017 lines 18+):**
- **State income tax refund** (M1 line 6 itself, not M1M — full subtraction of federally-taxed refunds).
- **Charitable contributions for non-itemizers:** 50% of (total allowable contributions − $500) (2017 M1M line 20; 2023 M1M line 11 worksheet; all years 2017–2025).
- **K-12 education expense subtraction:** up to $1,625/child grades K-6, $2,500/child grades 7-12 (2019 M1M line 13 instructions; unchanged amounts all years — not indexed). No income limit. Data-limited (no education-expense variable) — note-only.
- **Net interest/dividends from U.S. government obligations** (M1M line 14 in 2023) — full subtraction. Model with taxable-interest share assumption or skip; flag.
- Subtraction for persons **age 65+/disabled (Schedule M1R)**: income-tested subtraction (max $9,600 MFJ both 65+/$12,000 base minus nontaxable SS and excess income; thresholds unindexed and very low — de minimis today). Note-only (UNVERIFIED details; M1R-25 downloaded but not transcribed).
- Railroad Retirement Board benefits: fully subtracted (all years).
- **Military pensions/retirement pay** (M1M line 25 in 2023): full subtraction of Title-10 military retirement pay/SBP payments, all years 2017–2025; claiming it forecloses the (small, nonrefundable, $750) past-military-service credit on M1C. PUF cannot distinguish military pensions — known difference, likely skip (small MN incidence).
- **Qualified public pension subtraction (NEW TY2023, Minn. Stat. 290.0132 subd. 34; M1M line 29 → Schedule M1QPEN from 2024):** for public-safety-type pensions where the service **did not also earn Social Security credit** (police/fire/CSRS-type). Max and AGI phase-out (10% per $2,000, or fraction, over threshold):

| TY | Max MFJ/QSS | Max other | Threshold MFJ | Single/HoH | MFS |
|---|---|---|---|---|---|
| 2023 | $25,000 | $12,500 | $100,000 | $78,000 | $50,000 |
| 2024 | $26,340 | $13,170 | $105,380 | $82,190 | $52,690 |
| 2025 | $27,080 | $13,540 | $108,320 | $84,490 | $54,160 |

  (2023 M1M Worksheet for Line 29; DOR memos.) PUF has no "non-SS-covered public pension" flag — recommend skip + document (PE models it from pension income with eligibility imputation; check their approach before triage).
- Others (note-only): reservation income of enrolled members; active-duty military pay; National Guard; organ donor; volunteer mileage; AmeriCorps awards; 529 contributions subtraction (up to $1,500/$3,000 MFJ, alternative to the 529 credit); first-time-homebuyer account earnings; education-loan discharge (income-driven repayment); TY2025 new: coerced-debt, consumer-restitution, foreign-service pension, SEIU stipend subtractions (2025 What's New).

## 9. Credits

### 9a. Working Family Credit, TY2017–TY2022 (Minn. Stat. 290.0671; Schedule M1WFC)
Independent EITC-analog: credit = phase-in rate × earned income up to a cap; phased out at its own rate on the **greater of earned income or FAGI** over a threshold (MFJ threshold = other-filer threshold + fixed marriage offset). Requires federal-EIC-style eligibility (2017–2018: must be eligible for federal EIC; **2019+ expanded**: childless age window 21–64 instead of 25–64, and MN income limits allowed beyond federal). Investment-income cap follows the federal EIC cap. All parameters from DOR "WFC Computation Steps" sheets (2017/2018/2019/2021), M1WFC schedules (2020/2022 phase-out thresholds), booklet WFC tables (2020 maxima), and MN House Research (2022 maxima):

**TY2017 (3 tiers):** phase-in 2.10% of first $6,360 (max $134) / 9.35% of first $11,440 (max $1,070) / 11% of first $18,760 (max $2,064) for 0/1/2+ children; phase-out rates 2.01% / 6.02% / 10.82%; phase-out starts $8,360 / $21,800 / $25,850 (+$5,590 if MFJ: $13,950 / $27,390 / $31,440).

**TY2018 (3 tiers):** 2.10% × $6,480 (max $136) / 9.35% × $11,670 (max $1,091) / 11% × $19,130 (max $2,104); phase-out 2.01%/6.02%/10.82% from $8,530 / $22,230 / $26,360 (MFJ $14,230 / $27,930 / $32,060).

**TY2019 (4 tiers, restructured):** 3.90% × $7,150 (max $279) / 9.35% × $11,950 (max $1,117) / 11% × $19,600 (max $2,156) / 12.5% × $20,000 (max $2,500) for 0/1/2/3+ children; phase-out 2.0% / 6.0% / 10.5% / 10.5% from $8,730 / $22,770 / $27,000 / $27,300 (MFJ +$5,840: $14,570 / $28,610 / $32,840 / $33,140).

**TY2020:** maxima $284 / $1,136 / $2,191 / $2,541 (booklet WFC table plateaus; phase-in caps implied ≈ $7,280 / $12,150 / $19,920 / $20,330 — **derived, UNVERIFIED as printed caps**); phase-out (same rates) from $8,870 / $23,150 / $27,450 / $27,750 (MFJ $14,810 / $29,080 / $33,380 / $33,690) (2020 M1WFC line 4).

**TY2021:** 3.90% × $7,340 (max $286) / 9.35% × $12,270 (max $1,147) / 11% × $20,120 (max $2,213) / 12.5% × $20,530 (max $2,566); phase-out from $8,960 / $23,380 / $27,720 / $28,030 (MFJ +$6,000: $14,960 / $29,380 / $33,720 / $34,030).

**TY2022:** maxima $295 / $1,183 / $2,283 / $2,646 (House Research, sseitcwfc.pdf Aug 2022 — flagged secondary source; phase-in caps implied ≈ $7,570 / $12,650 / $20,755 / $21,170, UNVERIFIED); phase-out from $9,240 / $24,110 / $28,590 / $28,900 (MFJ $15,430 / $30,290 / $34,770 / $35,090) (2022 M1WFC line 4). Refundable (via Schedule M1REF) all years.

### 9b. Child Tax Credit + WFC, TY2023+ (HF1938; Minn. Stat. 290.0661/290.0671; Schedule M1CWFC)
Combined refundable credit = **WFC component + CTC component − joint phase-out**:
- **WFC component:** 4% of first X of earned income (max $350-ish) **plus** a fixed "qualifying older child" amount (older child = qualifying child not under 18):

| TY | Earned income cap X (max 4% credit) | +1 older child | +2 | +3+ |
|---|---|---|---|---|
| 2023 | $8,750 (max $350) | $925 | $2,100 | $2,500 |
| 2024 | $9,220 (max $369*) | $970 | $2,210 | $2,630 |
| 2025 | $9,480 (max $379*) | $1,000 | $2,270 | $2,710 |

  (*2024/2025 booklet "maximum working family credit" figures state the max per older-child count; the 4%×cap arithmetic is the encoding rule — M1CWFC line 3–4.)
- **CTC component:** **$1,750 per qualifying child under 18** (no child limit; not indexed 2023–2025 — DOR memo lists statutory year 2025 for the $1,750, i.e. indexation begins after 2025).
- **Joint phase-out:** total credit reduced by **12%** of the **greater of earned income or AGI** exceeding the threshold (**9%** instead if the filer has an older-child amount but zero under-18 qualifying children):

| TY | MFJ | All other |
|---|---|---|
| 2023 | $35,000 | $29,500 |
| 2024 | $36,880 | $31,090 |
| 2025 | $37,910 | $31,950 |

- Full refundability; ITIN filers eligible; MFS generally ineligible (separated-spouse exception); federal-EIC investment income cap applies ($11,000 in 2023).
- **Advance payment / minimum credit:** from TY2024 filers may elect advance payment of the next year's CTC (three installments); electing filers get a **"minimum credit"** floor in the advance year (credit not less than the minimum-credit base established at election, a safe harbor against phase-out clawback) and must file to reconcile (2024/2025 M1CWFC + 2025 What's New). Recommend: model the credit formula; ignore advance-payment/minimum-credit timing mechanics (annual-liability model).

Sources: 2023/2024/2025 Schedule M1CWFC + instructions; DOR TY2024/TY2025 inflation memos (290.0661 subd. 3–4, 290.0671 subd. 1).

### 9c. Marriage credit (Schedule M1MA; Minn. Stat. 290.0892) — nonrefundable
Two-earner credit = tax on joint taxable income (MFJ schedule) − [tax-as-single on each spouse's imputed share], where the lesser-earning spouse's share = their earned income (+taxable pension/SS elements per M1MA lines 1–5) minus half the MFJ standard deduction (2017–2018: minus one personal exemption ($4,050/$4,150) and half the MFJ standard deduction), remainder to the other spouse; both taxed on the single rate schedule (DOR "Computation of the Marriage Credit" TY2017/TY2022). Eligibility floors and maxima (from M1MA line 6/7 and line 19 caps):

| TY | Min lesser-earner income | Min joint taxable income | Look-up table cutoff | Maximum credit |
|---|---|---|---|---|
| 2017 | $23,000 | $38,000 | $101,000 | $1,433 |
| 2018 | $23,000 | $38,000 | $101,000 | $1,462 |
| 2019 | $25,000 | $39,000 | $103,000 | $1,508 |
| 2020 | $25,000 | $40,000 | $103,000 | $1,533 |
| 2021 | $26,000 | $40,000 | $104,000 | $1,548 |
| 2022 | $26,000 | $42,000 | $106,000 | $1,596 |
| 2023 | $28,000 | $44,000 | $114,000 | $1,710 |
| 2024 | $30,000 | $47,000 | $120,000 | $1,801 |
| 2025 | $31,000 | $48,000 | $123,000 | $1,851 |

Encoding: compute directly from the formula (we have both spouses' earned income); the printed look-up table is just midpoint-discretized values of the same formula.

### 9d. Child & dependent care credit (Minn. Stat. 290.067; Schedule M1CD) — REFUNDABLE (via M1REF)
- **Mechanics all years 2017–2025:** MN credit = the federal-formula credit (expenses ≤ $3,000/$6,000 × federal applicable percentage 35%→20%; computed on the M1CD itself from 2020-ish, copied from Form 2441 earlier), **capped, when AGI exceeds the threshold, at $600 (one qualifying person) / $1,200 (2+) minus 5% of AGI over the threshold** (i.e., fully gone $12,000/$24,000 above the threshold). Below the threshold the full federal-formula amount (up to $1,050/$2,100) is allowed. MN did **not** adopt the 2021 ARPA expansion (2021 M1CD computes its own pre-ARPA credit).
- Thresholds by year (M1CD "Income limit" each year): 2017 $50,000; 2018 $50,990; 2019 $52,230; 2020 $53,100; 2021 $53,630; 2022 $55,300; 2023 $59,210; 2024 $62,410; 2025 $64,150 (DOR memos: 290.067 subd. 1, statutory year 2019).
- Extras: deemed $3,000 expenses for a child born during the year (conditions); licensed day-care operators' own children; MFS ineligible (exceptions).

### 9e. Other credits (note-only)
- **Credit for income tax paid to another state** (M1C/M1CR; M1RCR for Wisconsin) — standard resident credit; out of scope for single-state simulation.
- **K–12 education credit (Schedule M1ED; 290.0674):** refundable; pre-2023: 75% of qualifying expenses, max $1,000/child, phased out on household income over $33,500 (unindexed; $1 per $4 hmm — phase-out 25%/50% two-child schedule, note-only); **2023+: max $1,500/child, AGI-based phase-out threshold $70,000** (2024 $73,760; 2025 $75,820), reduced $1 per $12 of excess income (UNVERIFIED rate). Data-limited (education expenses unobserved) — skip + document.
- **Student loan credit (M1SLC/M1C; 290.0682):** nonrefundable, up to $500 per taxpayer with payments on own qualified loans — needs loan-payment data; skip + document (PE does not have a MN student-loan-credit parameter dir; TAXSIM no).
- **529 plan credit (290.0684):** 50% of contributions up to $500 credit, phased out (2025: first phase-out threshold $96,220, second $173,200 — DOR memo); alternative to the subtraction; skip (no 529 data).
- Past military service credit ($750, nonrefundable), long-term care insurance credit (max $100/person), historic rehab, beginning farmer, stillborn child ($2,000-ish refundable), parents-of-stillborn, attaining master's degree, employer transit — all note-only, no data.
- **Renter's credit (NEW TY2024, 290.0693; Schedule M1RENT → M1REF):** the former renters' property-tax refund (M1PR circuit breaker) moved ONTO the income tax return beginning TY2024 — refundable credit computed from rent-constituting-property-taxes (17% of rent) against a household-income schedule (max $2,640 in 2024, $2,720 in 2025; fully phased out at household income $75,390 (2024) / $77,570 (2025) — DOR memos print the full schedule). **We have no rent data — exclude and document** (this makes MN "income tax" totals structurally differ from ours for 2024+ by the renter-credit amount; PE includes it).

### 9f. Refundability structure
Schedule M1REF (2018+) collects refundable credits: dependent care (M1CD), WFC/M1CWFC, K-12 education credit, parents-of-stillborn, historic credit, renter's credit (2024+ via M1RENT), plus withholding-type items. Marriage credit, credit for other states' taxes, student loan credit etc. are nonrefundable (Schedule M1C).

## 10. Minnesota AMT (Minn. Stat. 290.091; Schedule M1MT)

Structure (verified 2017/2023/2025 M1MT): alternative minimum taxable income = MN taxable income base rebuilt with AMT preferences (add back: M1SA taxes-paid deduction, misc/2% deductions, depletion, tax-exempt private-activity-type interest addbacks, etc.; subtract U.S.-bond interest, medical over the floor, charitable, casualty, investment interest to extent of income) minus exemption; **rate 6.75%** flat; pay excess over ordinary tax. Exemption phased down by **25%** of AMTI over the phase-out threshold (thresholds $150,000/$112,500/$75,000 MFJ/Single-HoH/MFS, unindexed — 2023 M1MT lines 22–24). Exemptions:

| TY | MFJ/QSS | Single/HoH | MFS |
|---|---|---|---|
| 2017 | $74,280 | $55,710 | $37,140 |
| 2023 | $87,960 | $65,970 | $43,990 |
| 2024 | $92,710 | $69,530 | $46,360 |
| 2025 | $95,300 | $71,470 | $47,660 |

(2018–2022 exemptions not pulled — UNVERIFIED; indexed series between the endpoints above, statutory year 2019 per DOR memo 290.091 subd. 3.) There is also an AMT credit (M1MTC) for prior-year AMT.
**Recommendation: document-only, do not model now** (consistent with our no-state-AMT policy; MN AMT bites mainly large-SALT/misc-deduction itemizers, a modest population; PE does model it — expect small cross-model residuals on high-itemizer records).

## 11. Net Investment Income Tax (NEW TY2024; Schedule NIIT)

Beginning TY2024: **1% tax on Minnesota net investment income over $1,000,000** (interest, dividends, capital gains, rental/royalty, non-qualified annuities; excludes gains on class 2a agricultural land) — 2024 booklet What's New; Schedule NIIT; M1 line 14. Not an AMT — an add-on tax. Affects a tiny, high-weight population; **recommend encoding** (simple: 1% × max(0, investment income − $1M)); threshold indexation UNVERIFIED (check 290.033: appears unindexed through 2025).

## 12. Filing requirement

MN residents must file if required to file federally, OR if MN gross income ≥ the year's threshold (chart in each booklet; equals the year's MN standard deduction for the status from 2019 on; higher for aged). Part-year/nonresidents: MN-source gross income ≥ the single/under-65 threshold:

| TY | Single <65 | MFJ both <65 | HoH <65 | MFS | Citation |
|---|---|---|---|---|---|
| 2017 | $10,400 | $20,800 (UNVERIFIED — resident chart not fully transcribed; $10,400 confirmed from part-year/nonresident rule) | — | — | 2017 booklet p. 6-7 |
| 2018 | $10,650 | $21,300 | $13,700 | $4,150 | 2018 booklet chart |
| 2019 | $12,200 | $24,400 | $18,350 | $12,200* | 2019 booklet chart |
| 2020 | $12,400 | $24,800 | $18,650 | * | 2020 booklet |
| 2021 | $12,525 | $25,050 | $18,800 | * | 2021 booklet |
| 2022 | $12,900 | $25,800 | $19,400 | * | 2022 booklet |
| 2023 | $13,825 | $27,650 | $20,800 | * | 2023 booklet |
| 2024 | $14,575 | $29,150 | $21,900 | * | 2024 booklet |
| 2025 | $14,950 | $29,900 | $22,500 | * | 2025 booklet |

(*MFS rows not separately transcribed 2019+ — the chart follows the standard deduction; UNVERIFIED detail, immaterial for tax-unit simulation since we compute liability regardless.) Encoding note: since MN taxable income already nets out the standard deduction, the filing threshold is approximately redundant; encode threshold = standard deduction (or skip).

## 13. IRC conformity timeline (relevant to our federal-calculator inputs)

| Period | MN IRC reference date | Consequence |
|---|---|---|
| TY2017 | Dec. 16, 2016 | Pre-TCJA year federally too — no issue |
| TY2018 | Dec. 16, 2016 as filed → **retroactively Dec. 31, 2018** (2019 act) | See §2; encode from TCJA FAGI + MN 2018 stack |
| TY2019–TY2022 | Dec. 31, 2018 (2019 act) | CARES/TCDTR/ARPA items nonconformed year-by-year via Schedule M1NC (e.g., 2020 UI $10,200 exclusion: MN **conformed** by the July 2021 act retroactively; charitable above-the-line $300: not in FAGI 2021 — no MN impact; 2020's $300 was below AGI... UNVERIFIED item-level list; M1NC-22 exists for 2022) — recommend: ignore M1NC for these years (our federal calculator applies actual federal law; MN differences are mostly business-side) |
| TY2023–TY2024 | **May 1, 2023** (2023 act, HF1938) | Effectively fully conformed for these years (SECURE 2.0 predates May 2023); no M1NC published for 2023/2024 |
| TY2025 | Still May 1, 2023 as of the 2025 booklet | **H.R. 1 of 2025 (OBBBA) NOT conformed** — 2025 Schedule M1NC required (2025 booklet What's New: "Since that date Congress has enacted H.R. 1 of 2025"). Key for us: OBBBA's individual marquee items (tips/overtime/senior/car-loan deductions) are **below-AGI** deductions, so they do not change FAGI → no M1NC adjustment needed for them and — because MN has its own deduction stack — they do NOT flow into MN taxable income. AGI-level OBBBA items (e.g., business provisions, above-the-line charitable if AGI-level) would need M1NC adjustments — document as low-priority known difference. M1NC-25 itself (files/2026-01/m1nc-25_2.pdf) returned 404 at research time — line list UNVERIFIED |

## 14. Known differences / PUF-encoding recommendations

1. **2018:** encode exactly per §2 (TCJA FAGI start + MN std ded/M1SA-approx + $4,150 exemptions with phase-out). M1NC residual items ignored — small.
2. **M1SA misc-2% deductions (2018–2025):** PUF post-TCJA data has no unreimbursed-employee-expense amounts; set to zero and document (understates MN itemized slightly).
3. **Bonus depreciation 80% addback / 5-year recovery:** structural known difference; not modelable from PUF; document.
4. **US-obligation interest, non-MN muni addback:** need interest-split assumptions; standard treatment as in other states (share parameters), else document.
5. **Military pension, public pension (QPEN), K-12 subtraction/credit, 529, student loan, M1R elderly subtraction:** data-limited; skip + document (QPEN and SS subtraction are the two big retiree items — SS is fully modeled §7; QPEN is the residual).
6. **Renter's credit 2024–2025:** on-form refundable income tax credit we cannot compute (no rent); document as structural totals difference; PE includes it.
7. **MN AMT:** document-only (§10). **NIIT 1%:** encode (§11).
8. **Dependent counts:** MN dependent exemption/CTC use federal-definition dependents and under-18 qualifying children (vs federal CTC's under-17) — our data has ages; use under-18 for MN CTC, all dependents for exemption, "qualifying older children" = EITC-type qualifying children aged 18+ (cap 3).
9. **WFC 2017–2018 requires federal EIC eligibility** (including the 25–64 childless age window); 2019+ uses 21–64 for childless — mirror our federal EITC eligibility flags with the age-window override.

## 15. Cross-model coverage notes

**TAXSIM (our 2017–2020 external comparator):** MN calculator historically covers rates/brackets, standard/itemized, exemptions, WFC, dependent care, marriage credit, SS subtraction — but **item-level fidelity is UNVERIFIED** (TAXSIM docs are thin; typical gaps: the 2018 nonconformity stack, the std-deduction limitation, the M1M addback set). Expect triage findings around: 2018 (all of it), std-deduction limitation for AGI > ~$195k, SS subtraction worksheet edge cases, marriage credit table midpoints.

**PolicyEngine (2021+ comparator):** parameter tree `gov/states/mn/tax/income/` confirms modeling of: `rates`, `deductions/standard` (base + extra + **reduction** — the limitation), `deductions/itemized` (+ reduction, incl. the alternate flat-80% rule), `exemptions`, `additions`, `subtractions/{social_security (both alternative & simplified methods), pension_income, elderly_disabled, charity, k12_education, education_savings}`, `credits/{cwfc (ctc + wfc + phase_out), cdcc, marriage (formula-based with standard_deduction_fraction), k12_education, renters}`, `amt`, `niit`. I.e., PE models MN AMT, the renter's credit, and QPEN-type pension subtraction — three things we plan to skip/document — plus everything we plan to encode. For cross-model triage: PE MN AMT and renters credit will create expected one-sided divergences; PE's marriage-credit formula matches our recommended direct computation.

## 16. UNVERIFIED summary

1. 2022 aged/blind standard-deduction add-on amounts ($1,700/$1,350 inferred from indexation, not transcribed from the 2022 table).
2. WFC 2020 and 2022 phase-in earned-income caps (maxima verified from booklet-table plateaus / House Research; caps back-derived from rate × cap = max).
3. 2022 WFC maxima are from MN House Research (secondary), not the DOR steps sheet (2022 steps sheet not archived).
4. M1SA limitation carve-out list for 2019–2022 (whether medical/investment-interest/casualty are excluded from the 3%/80% base exactly as federal Pease) — worksheet not fully transcribed.
5. MN AMT exemptions for 2018–2022 (endpoints + statutory indexation known; per-year values not pulled) — moot if document-only.
6. 2017 resident filing-requirement chart amounts other than the $10,400 single figure.
7. 2019–2022 M1NC item-by-item list (CARES/TCDTR/ARPA nonconformity for those years) — recommended to ignore for encoding, but the UI-exclusion conformity (2021 act, retroactive to 2020) should be spot-checked if 2020 cross-model deltas appear on UI-heavy records.
8. 2025 Schedule M1NC (OBBBA) line list — PDF 404 at research time; only the booklet What's-New description obtained.
9. K-12 education credit phase-out rate detail (pre-2023 and 2023+); note-only item.
10. NIIT $1M threshold indexation status for 2025 (encoded as unindexed).
11. Original (erroneous) text of the 2019/2020 standard-deduction-limitation worksheets (archived PDFs are corrected revisions; statute mechanics used).
12. M1R (age 65+/disabled) subtraction parameters — note-only.

## 17. Sources (URLs)

**Form M1 instruction booklets:** 2017 https://taxsim.nber.org/historical_state_tax_forms/MN/2017/m1_inst_17.pdf ; 2018 https://www.revenue.state.mn.us/sites/default/files/2023-12/m1_inst_18.pdf ; 2019 https://www.revenue.state.mn.us/sites/default/files/2024-01/m1_inst_19.pdf ; 2020 https://www.revenue.state.mn.us/sites/default/files/2023-12/m1_inst_20.pdf ; 2021 https://www.revenue.state.mn.us/sites/default/files/2023-12/m1_inst_21.pdf ; 2022 https://www.revenue.state.mn.us/sites/default/files/2024-02/m1-inst-22.pdf ; 2023 https://www.revenue.state.mn.us/sites/default/files/2025-06/m1-inst-23.pdf ; 2024 https://www.revenue.state.mn.us/sites/default/files/2026-01/m1-inst-24.pdf ; 2025 https://taxsim.nber.org/historical_state_tax_forms/MN/2025/m1-inst-25_0.pdf

**Forms M1:** 2017/2018/2023/2024 from NBER archive (`m1_17.pdf`, `m1_18.pdf`) and DOR (`files/2024-12/m1-23.pdf`, NBER `MN/2024/m1-24.pdf`).

**Schedule M1NC:** 2018 https://www.revenue.state.mn.us/sites/default/files/2023-01/m1nc_18.pdf ; 2022 https://www.revenue.state.mn.us/sites/default/files/2024-02/m1nc_22_0.pdf

**Schedule M1M:** 2017/2019/2021/2023/2025 via NBER archive (`m1m_17.pdf`, `m1m_19_0.pdf`, `m1m_21.pdf`, `m1m-23.pdf`, `m1m-25.pdf`); 2020 https://www.revenue.state.mn.us/sites/default/files/2024-01/m1m_20.pdf ; 2022 https://www.revenue.state.mn.us/sites/default/files/2024-01/m1m-22.pdf ; 2018 NBER `m1m_18_0.pdf`.

**Schedules M1SA / M1MA / M1WFC / M1CWFC / M1CD / M1MT / M1R:** NBER archive per-year directories `https://taxsim.nber.org/historical_state_tax_forms/MN/{2017..2025}/`.

**DOR Tax Research Division parameter memos:** "Tax Year 2024 Inflation-Adjusted Amounts in Minnesota Statutes" (Dec. 8, 2023) — NBER `MN/2024/inflation-adjusted-amounts-ty-2024.pdf`; "Tax Year 2025 Inflation-Adjusted Amounts" (Dec. 11, 2024) — NBER `MN/2025/inflation-adjusted-amounts-2025 (1).pdf`.

**DOR algorithm/computation sheets (NBER archive):** "Minnesota Income Tax Calculations for Tax Year 2018" (`it_algorithm_18.pdf`), same for 2021; "WFC Computation Steps" TY2017/2018/2019/2021; "Computation of the Marriage Credit" TY2017/TY2022.

**MN House Research:** "The Working Family Credit and Federal Earned Income Credit" (Aug. 2022) https://www.house.mn.gov/hrd/pubs/ss/sseitcwfc.pdf ; "Minnesota's Child Credit and Working Family Credit" https://www.house.mn.gov/hrd/pubs/ss/sschldwfc.pdf

**Statutes (cross-reference via DOR memos):** Minn. Stat. 290.06 subd. 2c (rates), 290.0121 (dependent exemption), 290.0122 (itemized), 290.0123 (standard deduction), 290.0132 subd. 26 (SS) & 34 (public pension), 290.067 (dependent care), 290.0661 (child credit), 290.0671 (WFC), 290.0674 (K-12), 290.0684 (529), 290.0693 (renter's credit), 290.091 (AMT) — https://www.revisor.mn.gov/statutes/cite/290

**PolicyEngine coverage:** https://github.com/PolicyEngine/policyengine-us tree `policyengine_us/parameters/gov/states/mn/tax/income/` (listed via GitHub API 2026-07-23).
