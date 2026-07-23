# Idaho Individual Income Tax — Research Notes (Core)

- **State:** Idaho (ID)
- **Date:** 2026-07-23
- **Status:** Core parameters verified from primary sources (Form 40 / Form 39R instruction packets for TY2017–TY2025 extracted directly from tax.idaho.gov PDFs, plus the Idaho STC published rate-schedule page and Idaho Code Title 63 ch. 30). Items not verifiable from primary sources are flagged UNVERIFIED.
- **Scope:** Tax years 2017–2025 plus enacted changes known as of 2026-07-23. Resident Form 40 / Form 39R only (nonresident Form 43/39NR out of scope for the model's resident-unit assumption).

---

## 0. Primary sources

Instruction packets (Form 40 + Form 43 + 39R/39NR instructions, incl. tax tables and worksheets), all fetched and text-extracted 2026-07-23:

| TY | URL |
|---|---|
| 2017 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_10-30-2017.pdf |
| 2018 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_02-25-2019.pdf |
| 2019 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_01-07-2020.pdf |
| 2020 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_12-21-2020.pdf |
| 2021 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_11-15-2021.pdf |
| 2022 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_03-01-2023.pdf |
| 2023 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_11-06-2023.pdf |
| 2024 | https://tax.idaho.gov/wp-content/uploads/forms/EIN00046/EIN00046_10-23-2024.pdf |
| 2025 | https://tax.idaho.gov/document-mngr/forms_ein00046/ (current packet, EIN00046 rev. 03-02-2026) |

Other primary:
- Idaho STC, "Individual Income Tax Rate Schedule" (official per-year rate schedules, 2012–2025): https://tax.idaho.gov/taxes/income-tax/individual-income/individual-income-tax-rate-schedule/
- Prior-year forms archive: https://tax.idaho.gov/taxes/income-tax/individual-income/forms/archive/
- Idaho Code Title 63 ch. 30: 63-3004 (IRC conformity), 63-3011B/63-3022 (Idaho taxable income = federal taxable income + adjustments), 63-3022A (retirement benefits), 63-3022D (dependent care), 63-3022E (home for aged/DD), 63-3022H (capital gains deduction), 63-3022K (medical savings account), 63-3022O (bonus depreciation adjustment), 63-3024 (rates), 63-3024A (food/grocery credit), 63-3024B (rebate fund), 63-3029L (child tax credit), 63-3082 (Permanent Building Fund $10 excise). Base URL: https://legislature.idaho.gov/statutesrules/idstat/Title63/T63CH30/
- Session laws referenced: 2018 HB463 & HB675; 2021 HB380; 2022 HB436, HB509 (2022 ch. 268); 2022 1st Extraordinary Session HB1 (2022 1st E.S. ch. 1); 2024 HB521 (2024 ch. 237); 2025 HB40 (2025 ch. 13), HB231 (2025 ch. 56).

Secondary (cross-checks only, flagged): Tax Foundation state bracket tables; PolicyEngine research note on 2025 Idaho changes; CNBC/TaxAct/TaxSlayer state-conformity pages on the 2020 UI exclusion; Idaho Capital Sun on 2026 session CTC outcome.

---

## 1. Starting point and IRC conformity

**Statutory definition vs. form mechanics.** Idaho Code 63-3011B/63-3022 defines Idaho taxable income as **federal taxable income** with Idaho adjustments. However, the **Form 40 operationalizes this from federal AGI** and rebuilds the deduction stack (this is the operational truth the model should follow):

- **Form 40 line 7 = federal adjusted gross income** — verified 2017 ("LINE 7 FEDERAL ADJUSTED GROSS INCOME: Enter your federal adjusted gross income from Form 1040, line 37...", 2017 instructions p. 6) and identically in all later years.
- Line 8: additions (Form 39R Part A total); line 10: subtractions (Form 39R Part B total); line 11: Idaho adjusted income.
- Lines 13–16: **federal itemized deductions (minus the SALT addback, §3 below) or the federal standard deduction, whichever is larger** — federal amounts, incl. aged/blind add-ons (e.g., 2024 worksheet: $14,600 single/MFS, $29,200 MFJ/QSS, $21,900 HoH; aged/blind add-on $1,550/$1,950 — identical to federal; 2022 add-ons $1,400/$1,750; 2023 $1,500/$1,850; dependent-filer limitation earned income + $450, min $1,300 — all mirror federal).
- **2017 only, line 18: personal exemptions** — $4,050 × federal exemptions, with the federal AGI phaseout ($156,900 MFS / $313,800 MFJ / $261,500 single / $287,650 HoH), "Your Idaho exemption amount should be the same as your federal exemption amount" (2017 instructions, Line 18). Gone 2018+ (TCJA conformity), no Idaho replacement other than the $205 CTC (§5.2).
- **QBI (IRC 199A): Idaho ALLOWS it.** Separate Form 40 line: 2018 "LINE 11 QUALIFIED BUSINESS INCOME DEDUCTION — Enter the amount from federal Form 1040, line 9"; 2023–2025 "Line 18 Qualified Business Income Deduction — Enter the amount from federal Form 1040 or 1040-SR, line 13." So 199A flows through in full (no addback). Verified 2018, 2023, 2024, 2025 packets.
- Line 19 (modern numbering): Idaho taxable income; line 20: tax.

**Net effect:** Idaho taxable income ≈ federal taxable income with (a) state income/sales tax component of itemized deductions removed, (b) 39R additions/subtractions, and — 2017 only — federal personal exemptions intact. Equivalent to the statutory "start from federal taxable income" description.

**IRC conformity (Idaho Code 63-3004): annual fixed-date conformity.** Each year's packet states the date (all verified from "What's New" sections):

| TY | Conformity date printed in packet |
|---|---|
| 2017 | IRC as of Jan 1, 2017 |
| 2018 | IRC as of Jan 1, 2018 |
| 2019 | IRC as of Jan 1, 2019 |
| 2020 | IRC as of Jan 1, 2020 (packet published 12/2020; the 2021 session retroactively advanced TY2020 conformity to Jan 1, 2021 — captures CAA but NOT ARPA. Bill number for the 2021 conformity act UNVERIFIED here) |
| 2021 | IRC as of Jan 1, 2021 |
| 2022 | IRC as of Jan 1, 2022 |
| 2023 | IRC as of Jan 1, 2023 |
| 2024 | IRC as of Jan 1, 2024 ("Notable exceptions include bonus depreciation and Idaho net operating losses") |
| 2025 | IRC as of **Jan 1, 2026** ("Notable exceptions include bonus depreciation, Idaho net operating losses, and Idaho Research Credit") — i.e., the 2026 session picked a date that captures 2025 federal legislation (OBBBA) for TY2025 |

**Known decouplings relevant to AGI/taxable income:**
- **Bonus depreciation (IRC 168(k)): Idaho DECOUPLES** for property acquired after 2009 (and before 2008); conformed only for 2008–2009 property. Every packet 2017–2025 states "Idaho doesn't conform to bonus depreciation for assets acquired after 2009." Mechanics: recompute depreciation without 168(k); if federal > Idaho, addback on 39R Part A line 5; if Idaho > federal, subtraction on 39R Part B (2022: Part B line 21). Statute: 63-3022O.
- **2020 ARPA $10,200 UI exclusion: Idaho did NOT conform** (TY2020 conformity date 1/1/2021 predates ARPA). Excluded UI had to be added back on the Idaho return (tax software implements as an addback; exact 39R line UNVERIFIED — flagged secondary: CNBC 3/30/2021 list of non-conforming states; TaxAct Idaho support page "addback the federal unemployment exclusion amount"). Otherwise **UI benefits are fully taxable in Idaho** (no subtraction) in all years.
- **QBI/199A: conformed (no addback)** — see above.
- Idaho NOLs are computed separately (add federal NOL deduction, 39R Part A line 1; subtract Idaho NOL, Part B line 1) — a permanent structural difference, minor for PUF modeling.

---

## 2. Rates and brackets, 2017–2025

Filing-status mapping (verified from tax-table headers and worksheets in every packet):
- **Single and MFS** use the single schedule.
- **MFJ, qualifying widow(er)/QSS, and HoH** use the married schedule (tax-table column "Married Filing Jointly* or Head of Household"; 2023+ worksheet gives HoH the joint subtraction amount).
- **Married thresholds = exactly 2x single** (rounding aside, e.g., 2019 married $23,108 = 2 × $11,554).
- Brackets are **CPI-indexed annually** (63-3024(2)); the STC publishes an "index formula" multiplier each year (2017: 1.472; 2018: 1.504; 2019: 1.541; 2020: 1.568; 2021: 1.588; 2022: 1.662).
- Form mechanics: tax tables for taxable income < $100,000; rate schedules above.

All schedules below are from the STC official rate-schedule page (URL in §0), cross-checked against the packet tax-table endpoints (e.g., 2017 table at $100,000: single $7,150 = $563.21 + 7.4% × ($100,000 − $11,043) rounded; married $6,899 — matches).

### TY2017 (pre-HB463 law; 7 brackets)

Single/MFS (taxable income):

| Over | Not over | Rate | Cumulative tax at floor |
|---|---|---|---|
| $0 | $1,472 | 1.6% | $0 |
| $1,472 | $2,945 | 3.6% | $23.56 |
| $2,945 | $4,417 | 4.1% | $76.57 |
| $4,417 | $5,890 | 5.1% | $136.94 |
| $5,890 | $7,362 | 6.1% | $212.03 |
| $7,362 | $11,043 | 7.1% | $301.85 |
| $11,043 | — | 7.4% | $563.21 |

MFJ/HoH/QW: thresholds $2,944 / $5,890 / $8,834 / $11,780 / $14,724 / $22,086 (same rates; cumulative $47.12 / $153.14 / $273.88 / $424.06 / $603.70 / $1,126.42).

### TY2018–TY2020 (2018 HB463: all rates cut ~0.475pp; 7 brackets 1.125/3.125/3.625/4.625/5.625/6.625/6.925%)

Single/MFS thresholds (bracket floors; married = 2x):

| Bracket rate | 2018 | 2019 | 2020 |
|---|---|---|---|
| 1.125% | $0 | $0 | $0 |
| 3.125% | $1,504 | $1,541 | $1,568 |
| 3.625% | $3,008 | $3,081 | $3,136 |
| 4.625% | $4,511 | $4,622 | $4,704 |
| 5.625% | $6,015 | $6,162 | $6,272 |
| 6.625% | $7,519 | $7,703 | $7,840 |
| 6.925% (top) | $11,279 | $11,554 | $11,760 |

Married (MFJ/HoH) floors: 2018: $3,008/$6,016/$9,022/$12,030/$15,038/$22,558 top; 2019: $3,082/$6,162/$9,244/$12,324/$15,406/$23,108 top; 2020: $3,136/$6,272/$9,408/$12,544/$15,680/$23,520 top.

### TY2021 (2021 HB380, retroactive to 1/1/2021: 5 brackets 1.0/3.1/4.5/5.5/6.5%)

Single/MFS: 1.0% $0–1,588; 3.1% $1,588–4,763; 4.5% $4,763–6,351; 5.5% $6,351–7,939; 6.5% over $7,939.
MFJ/HoH: 1.0% $0–3,176; 3.1% $3,176–9,526; 4.5% $9,526–12,702; 5.5% $12,702–15,878; 6.5% over $15,878.

### TY2022 (2022 HB436, retroactive to 1/1/2022: 4 brackets 1.0/3.0/4.5/6.0%)

Single/MFS: 1.0% $0–1,662; 3.0% $1,662–4,987; 4.5% $4,987–8,311; 6.0% over $8,311.
MFJ/HoH: 1.0% $0–3,324; 3.0% $3,324–9,974; 4.5% $9,974–16,622; 6.0% over $16,622.

### TY2023+ (2022 1st E.S. HB1: flat tax)

**Mechanics (Form 40 line 20 worksheet, verified 2023/2024/2025):** tax = rate × max(0, Idaho taxable income − subtraction amount). The subtraction amount is a de facto **zero bracket**, statutorily written as $2,500 single / $5,000 joint (63-3024(1)) **but CPI-indexed via the same bracket-adjustment mechanism (63-3024(2))**, so the operative amounts exceed the statutory base:

| TY | Rate | Subtraction: single/MFS | Subtraction: MFJ/HoH/QSS | Source |
|---|---|---|---|---|
| 2023 | 5.8% | $4,489 | $8,978 | 2023 Form 40 line 20 worksheet; 2022 1st E.S. HB1 (ch. 1, §5) |
| 2024 | 5.695% | $4,673 | $9,346 | 2024 Form 40 line 20 worksheet; 2024 HB521 (ch. 237, §2) |
| 2025 | 5.3% | $4,811 | $9,622 | 2025 Form 40 line 20 worksheet; 2025 HB40 (ch. 13, §3) |

(The STC rate-schedule page presents these as "0% on $1–$4,489 / 5.8% above" etc. — identical math.)

**Do NOT hard-code $2,500/$5,000 — the threshold is indexed annually.**

**2026:** no enacted rate change found as of 2026-07-23 (rate remains 5.3%; secondary sources only — UNVERIFIED). 2026 threshold amounts not yet published.

---

## 3. SALT / deduction mechanics (Form 40 line 14 addback)

Idaho itemizers use federal Schedule A totals **minus state/local income or general sales taxes** (both elections are backed out — sales tax gets no better treatment). Statute: 63-3022(j)(2) ("itemized deductions except state or local taxes measured by net income"; the form extends this to the sales-tax election). No addback of the federal standard deduction (Idaho simply grants the federal standard deduction). Taxpayer may itemize federally and take the standard deduction for Idaho (instructions explicitly note this may be beneficial "because of this addback").

- **2017 (pre-cap):** addback = all state/local income or general sales taxes on Schedule A, prorated if federal itemized deductions were Pease-limited (worksheet: addback × (allowed itemized / pre-limit itemized)). Source: 2017 instructions, "Lines 13–16" + limitation worksheet.
- **2018–2025 (SALT cap era), verified 2022/2024 text:** Form 40 line 14 =
  - if Schedule A line 5d (total SALT before cap) ≤ $10,000 ($5,000 MFS): **addback = line 5a** (income or sales taxes);
  - if line 5d > cap: **addback = line 5e (capped total, i.e. $10,000/$5,000) − line 5b (real-estate taxes) − line 5c (personal property taxes), floor $0.**
  - i.e., property taxes fill the cap first; only the residual capped amount attributable to income/sales taxes is added back. Fully modelable from PUF-style Schedule A components.
- Foreign tax credit claimed federally may be added to Idaho itemized deductions (minor; unobservable in PUF — ignore).

---

## 4. Form 39R Part A — additions (modelable subset)

Verified from 2022 packet (structure stable 2019–2025; 2017–2018 same content, different line labels):

| Line | Item | Model note |
|---|---|---|
| A-1 | Federal NOL deduction addback | Structural; pairs with Part B-1 Idaho NOL subtraction |
| A-2 | Non-Idaho capital loss carryover adjustment | Unobservable |
| A-3 | **Non-Idaho state/local bond interest and dividends** (municipal bonds of other states/their subdivisions, foreign obligations), less related expenses | Idaho munis exempt; other-state munis taxed. PUF has total tax-exempt interest only — needs an Idaho-share assumption |
| A-4 | Nonqualified withdrawal from Idaho 529 | Unobservable |
| A-5 | **Bonus depreciation addback** (property acquired pre-2008/post-2009) | Decoupling confirmed; unobservable at micro level (net effect could go either direction; Part B has the mirror subtraction) |
| A-6 | Other additions: Form 4972 lump-sum distributions, Idaho MSA nonqualified withdrawals, FTHB account nonqualified withdrawals, non-Idaho passive losses, (2020–2022) emergency rental assistance exclusion addback | Unobservable |

**No 39R addback for state income taxes deducted federally** — that lives on Form 40 line 14 (§3). **No addback of the federal standard deduction.** For TY2020 only, the ARPA UI exclusion addback (§1) — exact line UNVERIFIED.

---

## 5. Form 39R Part B — subtractions

| Line (2022) | Item | Values / rules | Model status |
|---|---|---|---|
| B-1 | Idaho NOL carryover/carryback | — | skip |
| B-2 | State income tax refund included in federal income (Schedule 1 line 1) | full | modelable (pairs with itemizer status) |
| B-3 | **Interest on U.S. government obligations** (Treasuries, savings bonds, etc.; FNMA/GNMA taxable) | full amount included in fed AGI | PUF: taxable-interest split unobservable — known difference |
| B-6 | **Child/dependent care DEDUCTION** (63-3022D) | Idaho converts the federal CDCTC *expense base* to a deduction: lesser of expenses paid, **$3,000 (one qualifying person) / $6,000 (2+)** minus employer-excluded benefits, and each spouse's earned income. Worksheet amounts $3,000/$6,000 verified for 2018, 2021 (NO adoption of ARPA's $8,000/$16,000 in 2021), and 2022. The "$12,000" figure sometimes attributed to 63-3022D does NOT appear on any form worksheet 2017–2025 — treat $3,000/$6,000 as operational truth. | modelable from CDCTC inputs |
| B-7 | **Social Security & Railroad Retirement benefits** | subtract the **taxable** amount (Form 1040 line 6b, and RRB amounts) — i.e., SS is fully exempt in Idaho, all years 2017–2025 | fully modelable |
| B-8 | **Retirement benefits deduction** (63-3022A) | See caps below. Eligibility: age 65+ (or 62+ and disabled); MFS excluded. Qualified benefits ONLY: CSRS (pre-1984 eligibility; FERS excluded), military retirement, Idaho Firemen's Retirement Fund, certain Idaho city police funds. Cap reduced dollar-for-dollar by SS/RRB benefits received (line 8c/8b). Capped at qualified benefits included in federal income. | **data-limited**: PUF pension income is not split by source; military/CSRS share unobservable |
| B-10 | **Idaho capital gains deduction** (63-3022H) | 60% of capital gain net income from qualifying **Idaho** property (real property held 12+ mo; tangible personal property in revenue-producing enterprise; cattle/horses 24 mo; timber 24 mo; certain partnership interests). Stocks/intangibles do NOT qualify. Form CG. | **unobservable known-difference** (Idaho-situs property share of gains unknowable in PUF) |
| B-11 | Active-duty military pay earned outside Idaho (120+ days) | full | unobservable, small |
| B-12 | Adoption expenses (max $10,000; was $3,000 through 2017, raised 2018) | — | skip |
| B-13 | **Idaho MSA** contributions + interest | cap $10,000 single / $20,000 MFJ (2022 text) | unobserved — skip/flag |
| B-14 | **Idaho 529 (IDeal)** contributions | **$6,000 single / $12,000 MFJ** per year (verified 2017 and 2022 — same cap across window) | unobserved — skip/flag |
| B-15 | Home for aged (65+) / developmentally disabled family member: $1,000/person deduction | alternative to the $100/person credit (§6.4) | skip |
| B-16+ | Misc: energy efficiency upgrades (B-4), alternative energy device (B-5, 40%/$5,000), tech equipment donation (B-9), FTHB account, worker's comp, etc. | — | skip |

Retirement benefits deduction maximums (Form 39R line 8a; all verified from packets):

| TY | MFJ max | Single max |
|---|---|---|
| 2017 | $48,366 | $32,244 |
| 2018 | $50,184 | $33,456 |
| 2019 | $51,498 | $34,332 |
| 2020 | $54,198 | $36,132 |
| 2021 | $56,664 | $37,776 |
| 2022 | $60,210 | $40,140 |
| 2023 | $65,286 | $43,524 |
| 2024 | $68,796 | $45,864 |
| 2025 | UNVERIFIED (in 2025 packet 39R section; not extracted — expect ~3% over 2024) |

---

## 6. Credits

### 6.1 Grocery credit / Food tax credit (63-3024A) — REFUNDABLE

- **Refundable** ("If taxes due are less than the total credit allowed, the taxpayer shall be paid a refund," 63-3024A). Claimable by residents with no tax due; seniors 65+ not required to file can claim via Form 24. Not available to anyone claimable as a dependent on another return (the dependent's credit goes to whoever claims them).
- Per-person (filer, spouse, each dependent — all must be Idaho residents):

| TY | Base per person | 65+ add-on (filer/spouse only) | Source |
|---|---|---|---|
| 2017 | $100 | +$20 | 2017 instr., Line 42 ("$100 per exemption for all income levels") |
| 2018 | $100 | +$20 | 2018 instr., Line 43 |
| 2019 | $100 | +$20 | 2019 instr. (worksheet $8.33/mo; $10/mo if 65+) |
| 2020 | $100 | +$20 | 2020 instr. |
| 2021 | $100 | +$20 | 2021 instr. |
| 2022 | $100 | +$20 | 2022 instr. |
| 2023 | **$120** | +$20 (= $140) | 2023 instr., Line 43 ("$120 each... additional $20 if age 65 or older"); increase enacted by 2022 HB509 (ch. 268) effective TY2023 |
| 2024 | $120 | +$20 (= $140) | 2024 instr. |
| 2025 | **$155 flat** — **65+ add-on eliminated**; NEW option: actual Idaho sales tax paid on qualifying food, up to **$250/person**, with receipts | — | 2025 instr., Line 43 "Food Tax Credit" worksheet ($12.92/mo); 2025 HB231 (ch. 56) |

- The 65+ add-on never applied to dependents (only filer/spouse), and the base amount is the same for dependents as for filers.
- **Proration:** no credit for any month (or part-month) in which the person received federal food stamps/SNAP, was incarcerated, or was in the U.S. illegally — monthly worksheet ($8.33/$10/$12.92 per month by era). **SNAP-month proration unobservable in PUF** — known difference.
- **Donate option:** entire credit may be donated to the Cooperative Welfare Fund (checkbox, enter 0). Ignore in model (small, voluntary).

### 6.2 Idaho Child Tax Credit (63-3029L) — NONREFUNDABLE

- **$205 per qualifying child, TY2018–TY2025, constant.** Enacted 2018 HB463 (initially $130) and raised to $205 by 2018 HB675 before taking effect (statute history shows two 2018 amendments; the 2018 packet's What's New already states "$205 per qualifying child"). **No Idaho CTC in 2017** (zero mentions in the 2017 packet — verified).
- Qualifying child: IRC 24(c) definition; form phrasing "age 16 or under as of December 31" (i.e., under 17), must be your qualifying child. Verified every packet 2018–2025.
- Nonrefundable; per form note, limited to tax liability after the credit for taxes paid other states and Form 39R/44 credits.
- **Sunset:** statute applies to taxable years beginning **before January 1, 2026**. The 2025 tax package did not extend it and a 2026-session attempt (SB1450) failed — so **no Idaho CTC for TY2026+** (sunset verified from statute; 2026 non-extension per Idaho Capital Sun — secondary, near-certain).

### 6.3 No state EITC

Confirmed: no earned income credit exists on Form 40 or in the packets (string "earned income credit" absent from 2018/2022/2024 packets; no statute). Idaho has never had one in this window.

### 6.4 Credit for maintaining a home for aged/developmentally disabled (63-3022E pairing)

$100 per qualifying person, **max $300** (max 3 people), for maintaining a household for an immediate family member 65+ (not self/spouse) or developmentally disabled (may include self/spouse); alternative to the $1,000/person deduction (39R Part B line 15). Claimable even below the filing threshold. Note only — not modelable from PUF.

### 6.5 Other credits (note only)

Credit for income tax paid other states (Form 39R Part C); charitable-entity/youth-rehab/live-organ credits (Part D); business credits (Form 44). One-time rebates: 2022 HB436 rebate and 2022 1st E.S. HB1 rebate (paid via 63-3024B rebate fund) — **out of scope** (not annual tax law; administered as separate payments, not lines on Form 40 tax law).

---

## 7. Permanent Building Fund (PBF) tax — $10 per return (63-3082)

- **$10 flat excise per return** for everyone **required to file** an Idaho return; MFJ = one $10. On Form 40: line 31 (2017), line 32 (2018–2024), **line 31 (2025)**.
- **NOT repealed** — verified present on the 2023, 2024, AND 2025 forms/instructions, and Idaho Code 63-3082 remains in force (last amended 2014 ch. 36). Any recollection of a TY2023 repeal is wrong. (2022 1st E.S. HB1 changed the income-tax *distribution* to funds, not the taxpayer-side $10.)
- **Exemptions** (draw a line through the $10): (1) gross income below the filing requirement ("NRF") — e.g., filing only for a withholding refund or grocery-credit refund; (2) receiving Idaho public assistance payments at year-end (food stamps/WIC do NOT count as public assistance for this purpose); (3) taxpayer or spouse legally blind at year-end. Verified 2021 and 2025 instructions (list stable).
- Model note: apply $10 to units required to file (see §8), excluding the blind (observable?) and public-assistance cases (unobservable, small).

---

## 8. Filing requirement

Verified 2024/2025 instructions "Who Must File" (stable across window):
- **Every Idaho resident who must file a federal income tax return** — i.e., the federal gross-income thresholds pass through directly (no separate Idaho dollar table for residents).
- Part-year residents: >$2,500 total gross income (all sources while resident + Idaho-source while nonresident). Nonresidents: >$2,500 Idaho-source gross income. (Out of scope for resident modeling.)
- Idaho filing status must match federal.

Model rule: resident unit files (and owes PBF pre-exemption) iff it meets the federal filing requirement; grocery-credit-only refund claims are allowed regardless.

---

## 9. Known differences / limitations for a PUF-based model

1. **Retirement benefits deduction (63-3022A):** requires pension income split by source (CSRS pre-1984 / military / ID police & fire only) and age/disability. PUF has total taxable pensions + age. Modeling the cap-minus-SS structure on an assumed qualifying share will overstate coverage; omitting understates. Data-limited — decide via calibration; the SS offset makes the net deduction small for most SS recipients (caps are reduced dollar-for-dollar by SS received).
2. **Idaho capital gains deduction (63-3022H):** 60% of gains on Idaho-situs qualifying property (no stocks) — unobservable known-difference; expect our liability to be biased UP for gain-heavy returns relative to actuals.
3. **US-obligation interest subtraction:** share of taxable interest from Treasuries unobservable.
4. **Non-Idaho muni interest addback:** share of tax-exempt interest from non-Idaho munis unobservable (all-or-nothing assumptions bracket the truth).
5. **529 (IDeal, $6k/$12k) and MSA ($10k/$20k) deductions:** contributions unobserved — omit; small aggregate bias down on subtractions (liability up).
6. **Grocery credit SNAP/incarceration month proration:** unobservable; modeling full annual credit for all residents overstates the credit for SNAP households (could proxy with SNAP imputation if available).
7. **PBF edge cases:** public-assistance and blind exemptions largely unobservable; NRF (below-filing-threshold) is modelable.
8. **Bonus-depreciation decoupling:** direction and size unobservable at micro level; net addback historically positive in high-168(k) years. Omit with a flag.
9. **2020 UI addback (ARPA non-conformity):** if the federal calculator applies the $10,200 exclusion for 2020, Idaho requires adding it back — implement as state-level addback of the federally excluded UI amount.
10. **2017 exemption phaseout** must be applied (Idaho = federal exemption amount incl. PEP).
11. **Dependent-filer standard deduction rules** mirror federal — reuse federal logic.
12. **Idaho PTE workaround (ABE, 63-3026B, 2021+)** shifts some owner-level liability to entities — out of scope; can distort comparisons to SOI totals for high-income units.

---

## 10. Cross-model notes (harness: TAXSIM covers 2017–2020, PolicyEngine 2021+)

- **TAXSIM (2017–2020 window):** models ID rates/brackets, federal-linked standard/itemized rebuild and SALT addback. Whether TAXSIM includes the grocery credit, the $205 CTC (2018+), and the $10 PBF is **UNVERIFIED** — check empirically in the harness: grocery credit appears as a refundable −$100 × (exemptions) step (+$20 for aged); PBF as a flat +$10 for filers; CTC as −$205 × kids under 17. Flag divergences accordingly before triage.
- **PolicyEngine (2021+ window):** PE models the ID rate schedules/flat tax, grocery credit, and Idaho CTC (their 2025 research note explicitly covers the 5.3% rate, the $155 food credit, and the CTC expiration — https://www.policyengine.org/us/research/idaho-2025-tax-change — secondary). Whether PE models the PBF $10, the retirement-benefits deduction, and the capital-gains deduction is **UNVERIFIED** — inspect policyengine-us `parameters/gov/states/id/` in the harness.
- Both external models are cross-checks only; the Form 40/39R values in this document are authoritative.
- Divergence triage hints: 2023+ any mismatch of the zero-bracket amount usually means the other model hard-coded $2,500/$5,000 (statutory base) instead of the indexed $4,489/$4,673/$4,811. For HoH, verify the other model gives HoH the MARRIED schedule (Idaho is unusual here).

---

## 11. Open items / UNVERIFIED summary

1. 2025 retirement-benefits deduction caps (39R line 8a) — extract from the 2025 packet 39R section. (Everything else for 2025 is verified.)
2. Exact 39R line used for the TY2020 ARPA UI addback (non-conformity itself is solid).
3. Bill number of the 2021 conformity act that moved TY2020 conformity to 1/1/2021.
4. Session-law chapter numbers for 2018 HB463/HB675, 2021 HB380, 2022 HB436 (bills themselves and their parameter effects verified via forms; chapters for 2022 1st E.S. HB1 = ch. 1, 2024 HB521 = ch. 237, 2025 HB40 = ch. 13, HB231 = ch. 56, HB509 = 2022 ch. 268 are verified from statute amendment histories).
5. TY2026: 5.3% carryforward and CTC expiration are expected law but 2026 forms don't exist yet; no enacted 2026 rate change found.
6. TAXSIM/PE feature coverage of grocery credit / CTC / PBF — verify in harness (see §10).
7. 2017 "Who Must File" gross-income dollar table (packet page 2) not extracted verbatim; resident rule = federal filing requirement confirmed for 2019–2025 and believed identical for 2017–2018 (UNVERIFIED verbatim).
