# Virginia Individual Income Tax — Core Mechanics Research Packet (TY2017–TY2025)

**Prepared:** 2026-07-18, for Tax-Simulator state-tax module encoding.
**Method:** Year-specific Virginia Form 760 Resident Individual Income Tax instruction booklets (Virginia Department of Taxation, tax.virginia.gov) were downloaded as PDFs and text-extracted (`pdftotext`); every parameter below was transcribed from the year's own booklet unless noted. Statutory cross-checks against Va. Code Title 58.1 (law.lis.virginia.gov, text as of 2026-07-18) and Virginia Tax bulletins. Booklet URLs are in the Sources list; per-row citations reference booklet year + section/line.

**Booklet inventory (all fetched and read):**

| TY | File | Revision stamp seen |
|---|---|---|
| 2017 | `2017-form-760-instructions.pdf` | incl. conformity insert "Rev. 2/18" |
| 2018 | `2018-form-760-instructions.pdf` | — |
| 2019 | `2019-760-instructions.pdf` | Sch A instr Rev. 02/20 |
| 2020 | `2020-form-760-instructions.pdf` | incl. ARPA cover insert |
| 2021 | `2021-760-instructions.pdf` | — |
| 2022 | `2022-760-instructions.pdf` | — |
| 2023 | `2023-760-instructions.pdf` | Rev. 11/23 |
| 2024 | `2024-760-instructions.pdf` | Rev. 09/24 |
| 2025 | `2025-760-instructions.pdf` | — |

---

## 1. Rate schedule

Transcribed verbatim from the **"TAX RATE SCHEDULE"** page (back of booklet, ahead of the Tax Table) in **every** booklet 2017–2025. The schedule text is character-identical in all nine years:

> IF YOUR VIRGINIA TAXABLE INCOME IS:
> Not over $3,000, your tax is 2% of your Virginia taxable income.
>
> | over-- | but not over-- | your tax is-- | of excess over-- |
> |---|---|---|---|
> | $3,000 | $5,000 | $60 + 3% | $3,000 |
> | $5,000 | $17,000 | $120 + 5% | $5,000 |
> | $17,000 | — | $720 + 5.75% | $17,000 |

| TY | Rates / brackets | Same for all filing statuses? | Citation |
|---|---|---|---|
| 2017–2025 (each year verified) | 2% ≤ $3,000; 3% $3,000–5,000; 5% $5,000–17,000; 5.75% > $17,000 | Yes — one schedule; no MFS or any status-specific schedule exists | Tax Rate Schedule page of each year's Form 760 instructions (2017 booklet TOC p. 39; 2022 booklet Line 16 instruction refers to "the tax rate schedule on Page 35") |

- **No filing-status differentiation, confirmed two ways:** (a) the booklet schedule has no status column and the Spouse Tax Adjustment (STA) instructions in every year explain that the *same* progressive schedule applied to combined MFJ income is what the STA compensates for ("HOW IT WORKS: Virginia tax rates increase with income: 2% up to $3,000; 3% from $3,001 to $5,000; 5% from $5,001 to $17,000; and 5.75% for income over $17,000" — 2017 booklet Line 18 STA instruction, repeated in later years); (b) **Va. Code § 58.1-320** imposes the tax "on the Virginia taxable income for each taxable year of every individual" with a single schedule — no filing-status language; current brackets apply to "taxable years beginning on and after January 1, 1990" (last substantive amendment 1987, c. 9).
- Encoding note: MFS uses the identical schedule (the marriage penalty this creates for MFJ is mitigated by the STA credit, Form 760 Line 17/18 — out of scope here).

## 2. Standard deduction

Amounts are entered on Form 760 Line 12 (TY2017–2018) / **Line 11** (TY2019–2025). Filing Status 1 = Single, 2 = MFJ, 3 = MFS. Transcribed from the Line 12/11 instruction of each booklet:

| TY | Single (FS1) | MFJ (FS2) | MFS (FS3) | Citation (booklet, line instruction) |
|---|---|---|---|---|
| 2017 | $3,000 | $6,000 | $3,000 | 2017 instr., Line 12 ("Filing Status 1 Enter $3,000 / Filing Status 2 Enter $6,000 / Filing Status 3 Enter $3,000"), p. 11 |
| 2018 | $3,000 | $6,000 | $3,000 | 2018 instr., Line 12 |
| 2019 | $4,500 | $9,000 | $4,500 | 2019 instr., Line 11; What's New: "Increased Standard Deduction" |
| 2020 | $4,500 | $9,000 | $4,500 | 2020 instr., Line 11 |
| 2021 | $4,500 | $9,000 | $4,500 | 2021 instr., Line 11 |
| 2022 | $8,000 | $16,000 | $8,000 | 2022 instr., Line 11; What's New: "Increase in Standard Deduction" |
| 2023 | $8,000 | $16,000 | $8,000 | 2023 instr., Line 11; What's New |
| 2024 | $8,500 | $17,000 | $8,500 | 2024 instr., Line 11; What's New |
| 2025 | **$8,750** | **$17,500** | **$8,750** | 2025 instr., Line 11 ("Filing Status 1 Enter $8,750 / Filing Status 2 Enter $17,500 / Filing Status 3 Enter $8,750"); What's New |

**Transitions and enactment history (booklet What's New transcriptions):**
- **TY2019 increase:** "For taxable years beginning on and after January 1, 2019, but before January 1, 2026, the Virginia standard deduction increases from $3,000 to $4,500 for individuals and married taxpayers filing separately, and from $6,000 to $9,000 for married taxpayers filing joint returns." (2019 booklet, What's New.) Sunset (Jan 1, 2026) was baked in from enactment.
- **TY2022 increase:** "New legislation enacted during the 2022 General Assembly session increases the standard deduction from $4,500 to $8,000 for single filers and from $9,000 to $16,000 for married filers filing jointly. The increase for Taxable Year 2022 is contingent on annual revenue growth of at least five percent for the six-month period of July 2022 through December 2022... If the five percent growth rate is not met for either taxable year, the standard deduction for that taxable year will be $7,500 for single individuals and $15,000 for married persons." (2022 booklet, What's New.) **The revenue trigger was met for both 2022 and 2023** — the Line 11 instructions in both booklets print $8,000/$16,000, and the statute codifies $8,000/$16,000 for TY2022–2023. "Under this Act, the increase in the standard deduction is scheduled to sunset after Taxable Year 2025 and revert to... $3,000 for single filers and $6,000 for married couples filing jointly."
- **TY2024 increase:** "Legislation enacted during the 2023 General Assembly session increased the standard deduction for Taxable Year 2024 from $8,000 to $8,500 for single filers and from $16,000 to $17,000 for married filers filing jointly. The increase... is scheduled to sunset after Taxable Year 2025 and revert to... $3,000... and $6,000..." (2024 booklet, What's New.)
- **TY2025 increase (differs from prior expectation of $8,500/$17,000):** "Legislation enacted during the 2025 General Assembly session increased the standard deduction for Taxable Years 2025 and 2026 from $8,500 to $8,750 for single filers and from $17,000 to $17,500 for married filers filing jointly. The increase... is scheduled to sunset after Taxable Year 2026 and revert to... $3,000... and $6,000..." (2025 booklet, What's New.)

**Sunset under current law (Va. Code § 58.1-322.03 subdiv. 1, text as of 2026-07-18)** — the statute now carries the *entire* schedule, extended by 2026 legislation beyond what the 2025 booklet says:

| Statutory window | Single | Married (MFS = half) |
|---|---|---|
| before TY2019 and **on/after TY2030** | $3,000 | $6,000 |
| TY2019–2021 | $4,500 | $9,000 |
| TY2022–2023 | $8,000 | $16,000 |
| TY2024 | $8,500 | $17,000 |
| TY2025–2026 | $8,750 | $17,500 |
| TY2027 | $9,200 | $18,400 |
| TY2028–2029 | $9,300 | $18,600 |

Amendment line includes "2025, cc. 615, 658, 725; 2026, c. 7; 2026, Sp. Sess. I, c. 1" — i.e., the 2026 General Assembly extended/raised the enhanced amounts through TY2029; under current law the reversion to $3,000/$6,000 now occurs in **TY2030** (the 2025 booklet's "sunset after Taxable Year 2026" statement is superseded for out-years). For 2017–2025 encoding this is moot; for projections use the statutory schedule.

**MFS / HoH treatment:** MFS standard deduction = the single amount in every booklet (statute: "one-half of" the married amount, which equals the single amount in all years). There is **no Head of Household standard deduction** — HoH filers file as Single (Filing Status 1, see §7) and get the single amount.

**Dependent filers:** identical text in every booklet 2017–2025: "**Dependent on Another's Return** — If you can be claimed as a dependent on the federal return of another taxpayer, your standard deduction is limited to the amount of your earned income. Enter the smaller of the amount of earned income or the standard deduction amount on Line 11 [Line 12 in 2017–18]." Statute: "Any person who may be claimed as a dependent on another taxpayer's return for the taxable year may compute the deduction only with respect to earned income" (§ 58.1-322.03 subdiv. 1). No dollar floor (unlike federal).

## 3. Itemization coupling (item_coupling = 1) and the SALT addback

**Coupling rule — verified in every booklet 2017–2025 (identical in substance):**
- 2017 booklet, "ITEMIZED OR STANDARD DEDUCTIONS" (p. 10): "You must claim the same type of deductions (standard or itemized) on your Virginia return as you claimed on your federal return."
- 2017 Line 10: "You must claim itemized deductions on your Virginia return if you claimed itemized deductions on your federal return." / Line 12: "If you claimed the standard deduction on your federal return, you must also claim the standard deduction on your Virginia return." (Same sentences appear in the 2018 Line 10/12, and 2019–2025 Line 10/11 instructions.)
- 2019+ Virginia Schedule A instructions, General Information: "You must claim the same type of deductions (standard or itemized) on your Virginia return as you claimed on your federal return. As a result, you may not claim itemized deductions on your Virginia return if you claimed the standard deduction on your federal return."

This confirms **item_coupling = 1 for all years 2017–2025**: VA itemizer status is forced to equal federal itemizer status, both directions.

**Statutory basis (Va. Code § 58.1-322.03 subdiv. 1(a)):** the deduction is "the amount allowable for itemized deductions for federal income tax purposes where the taxpayer has elected for the taxable year to itemize deductions on his federal return, but reduced by the amount of income taxes imposed by the Commonwealth or any other taxing jurisdiction and deducted on such federal return" (plus a minor mileage-rate add-on to 18¢/mile).

**Mechanics by year:**

| TY | Mechanism | SALT-cap flow-through | Pease | Citation |
|---|---|---|---|---|
| 2017 | Form 760 Line 10 = federal itemized (federal Schedule A totals); Line 11 = state/local income tax addback; Line 12 = net. FDC Worksheet recomputes charitable/casualty limits for conformity | n/a (pre-TCJA) | Federal Pease flows through; booklet has an itemized-limitation worksheet keyed to federal thresholds ($313,800 MFJ/QW, $287,650 HoH, $261,500 single, $156,900 MFS) with an 80%-of-addback adjustment when limited | 2017 instr., Lines 10–12 (p. 11) + Itemized-deduction limitation worksheet (p. 12 area) |
| 2018 | New **Virginia Schedule A** introduced ("New Virginia Schedule A: For Taxable Year 2018 and after..." — 2018 What's New); line items "defined in federal law". 760 Line 10 = itemized; Line 11 = addback; Line 12 = net | **Yes** — federal $10,000/$5,000 cap applies (VA conformed to TCJA for 2018). Addback proration when cap binds: "If Schedule A, Line 5d is equal to or less than $10,000 ($5,000 if married filing separately), then enter the amount of state and local income taxes reported on Schedule A, Line 5a on Line 11... If Schedule A, Line 5d is greater than $10,000 ($5,000...), add Schedule A, Lines 5b and 5c" [and compute the prorated income-tax share of the capped $10k] | None (TCJA suspension conformed for TY2018) | 2018 instr., Line 11 "State and Local Income Taxes claimed on Schedule A" |
| 2019–2025 | Va. Schedule A computes everything; 760 **Line 10 = Va. Sch. A Line 19** (net of addback). Sch. A Line 17 = total itemized (after VA limitation), **Line 18 = "Reduction for State and Local Income Taxes"** (generally = Line 5a), Line 19 = 17 − 18 | **No (deconformed).** 2019 Sch. A instructions, "Taxes You Paid": "For Virginia purposes, your deduction is generally not subject to the $10,000 federal limitation for taxable year 2019 and thereafter." Exception: the **Line 5a entry itself** (state/local income taxes, or sales taxes if the sales-tax oval is filled) "may not exceed $10,000 or $5,000 if... married filing separately" — inconsequential for income taxes since Line 5a is added back anyway, but binding for the **sales-tax election** (sales taxes stay capped and are *not* added back: "if you filled in the oval on Line 5a because you are claiming a deduction for general sales taxes instead of income taxes, enter zero on Line 18"). Property taxes (5b/5c) are **uncapped** on the VA return | **Yes — Virginia's own Pease.** "For taxable years beginning on or after January 1, 2019, Virginia deconforms from the suspension of the overall limitation on itemized deductions, commonly known as the Pease limitation" (2019 booklet What's New; repeated every year through 2025). Applied via the Sch. A "Limited Itemized Deduction Worksheet" using **federal filing status** AGI thresholds — 2019: $326,050 MFJ/QW, $298,850 HoH, $271,700 single, $163,025 MFS; 2022: $343,950 / $315,300 / $286,600 / $171,975; 2024: $388,400 / $356,000 / $323,650 / $194,200. When limited, Line 18 comes from the worksheet (Part B, Line 15) instead of Line 5a | Va. Schedule A instructions 2019 (Rev. 02/20), 2022 (Rev. 09/22), 2024 (Rev. 08/24); 760 booklets' Line 10 instruction each year |

- Foreign income taxes deducted federally (Sch. A Line 6) are **also added back** on Line 18 ("income taxes imposed by... any other taxing jurisdiction"): "If you claimed a deduction for foreign income taxes on Line 6, enter on Line 18 the amount of such deduction, plus any amounts from Line 5a." (2019 Sch. A instr.; same in 2022/2024.)
- "If state and local income tax is the only federal itemized deduction you are claiming on the Virginia return, enter zero on Form 760, Line 10." (2022 Sch. A instr.)
- **TY2025/OBBBA:** "Regarding itemized deductions, Virginia generally deconforms from the federal state and local tax (SALT) cap and the new federal overall limitation on itemized deductions. However, Virginia retains the Virginia overall limitation on itemized deductions [Pease] and applies the federal SALT cap amount for the taxable year when calculating the Virginia overall limitation on itemized deductions." (2025 booklet, conformity section; see revised 2025 Va. Schedule A instructions.) I.e., for 2025 the Line 5a entry cap becomes the OBBBA-increased federal SALT cap amount ($40,000-family) rather than $10,000, but the structure (addback of income taxes; VA Pease) is unchanged.
- MFS allocation: "If a joint federal return was filed and you are filing separate returns in Virginia (Filing Status 3), itemized deductions that cannot be accounted for separately must be allocated proportionately between spouses based on each spouse's share of the combined federal adjusted gross income." (Line 10 instruction, every year.)

**Simulator mapping:** item_coupling=1 all years. VA itemized = federal itemized − state/local (and foreign) income taxes deducted federally; for 2018 apply the federal cap + proration; for 2019+ un-cap the taxes-paid section (income-tax portion is washed out by the addback; property taxes fully deductible) and apply VA's Pease-style limitation at the year's thresholds.

## 4. Personal and dependent exemptions

Form 760 Exemption Section A: (You + Spouse [FS2] + Dependents) **× $930**. Section B: (You 65-or-over + Spouse 65-or-over + You Blind + Spouse Blind) **× $800**. Sum of A + B → Form 760 Line 13 (2017–18) / Line 12 (2019–25) "Exemptions."

| TY | Personal/dependent | Age 65+ (each) | Blind (each) | Citation |
|---|---|---|---|---|
| 2017 | $930 | $800 | $800 | 2017 instr., Exemptions (p. 9): "Multiply the sum of exemptions claimed in the 'You,' 'Spouse' and 'Dependents' boxes by $930"; STA worksheet: "multiply the total by $800. Add $930..." |
| 2018 | $930 | $800 | $800 | 2018 instr., Exemptions section (same sentence) |
| 2019 | $930 | $800 | $800 | 2019 instr., Line 12 + STA worksheet |
| 2020 | $930 | $800 | $800 | 2020 instr., Line 12 |
| 2021 | $930 | $800 | $800 | 2021 instr., Line 12 |
| 2022 | $930 | $800 | $800 | 2022 instr., Line 12 |
| 2023 | $930 | $800 | $800 | 2023 instr., Line 12 |
| 2024 | $930 | $800 | $800 | 2024 instr., Line 12; 2024 Form 760 face: "× $930 =" (Section A), "× $800 =" (Section B) |
| 2025 | $930 | $800 | $800 | 2025 instr., Line 12 |

- **No change in any year 2017–2025; no phase-out** in booklet or statute. Statute: "a deduction in the amount of $930 for each personal exemption allowable to the taxpayer for federal income tax purposes" and, for "blind or aged" taxpayers, "an additional personal exemption in the amount of $800" (Va. Code § 58.1-322.03 subdiv. 2). Note the $930 count keys off federal-law exemption *eligibility* (unaffected by the TCJA's zeroing of the federal exemption *amount*).
- **Stackable:** the 2024 Form 760 face has four separate Section B boxes (You 65+, Spouse 65+, You Blind, Spouse Blind) that are summed and multiplied by $800 — a taxpayer who is both 65+ and blind gets $1,600; an MFJ couple both 65+ and both blind gets $3,200. The STA worksheet ("Enter a 1 in the boxes that apply and multiply the total by $800. Add $930...") confirms per-instance stacking.
- The $800 add-ons apply **only to taxpayer and spouse**, not dependents (form has no dependent 65+/blind boxes).
- Age test: "you must have been age 65 or older on or before January 1, [TY+1]" (each booklet). Blind test: "considered blind for federal income tax purposes."
- MFS: "Each spouse must determine exemptions as if separate federal returns had been filed... If dependent exemptions cannot be accounted for separately, they must be proportionately allocated between each spouse based on each spouse's income. One spouse may never claim less than a whole personal exemption." (Every booklet, Filing Status 3 note.)

## 5. Filing threshold

Based on **VAGI** (Form 760 Line 9 = FAGI + additions − age deduction/SS benefits/other subtractions). Booklet text (identical all years): "If the amount on Line 9 is less than the amount shown below for your filing status, your Virginia income tax is $0.00 and you are entitled to a refund of any withholding or estimated tax paid. You must file a return to receive a refund... You are required to file a return if you are:"

| TY | Single | MFJ (combined) | MFS | HoH | Citation |
|---|---|---|---|---|---|
| 2017–2025 (each year verified) | VAGI ≥ $11,950 | VAGI ≥ $23,900 | VAGI ≥ $11,950 | no separate threshold (files as Single → $11,950) | "Single and your VAGI is $11,950 or more / Married filing jointly and combined VAGI is $23,900 or more / Married filing separately and your VAGI is $11,950 or more" — Line 9 instruction + "Who Must File"/"Filing Options" pages of each booklet |

- Statute: **Va. Code § 58.1-321** — no tax on single individuals with VAGI (plus the specified modification) "less than $11,950" and married couples "less than $23,900" combined (half for separate returns), for TY2012 and after (prior: $11,650/$23,300 for 2010–2011; amendment history through 2020, c. 606 — unchanged since 2012 and unchanged through TY2025).
- Note the threshold is a **cliff, not an exemption**: at VAGI below the threshold, tax = $0; at or above, the full computation applies.

## 6. Start point and IRC conformity (for conformity-group note)

**Start point, all years:** Form 760 **Line 1 = "Federal Adjusted Gross Income"** — "Enter the federal adjusted gross income from your federal return" (every booklet; MFS: "enter only the amount of income attributable to you"). VA is an FAGI-start state; the VA standard/itemized deduction and exemptions are subtracted after VAGI, so federal *below-the-line* items never enter.

**Conformity date by taxable year** (the operative date for TY N is generally set by the General Assembly session in year N+1 via emergency "conformity bills"; booklets for a TY are sometimes printed before that session — flagged where so):

| TY | Operative IRC conformity date | Primary-source evidence |
|---|---|---|
| 2017 | **February 9, 2018** (fixed date; advanced from Dec 31, 2016 by 2018 GA emergency legislation) | Insert bound into the 2017 booklet (Rev. 2/18): "Under emergency legislation enacted by the 2018 General Assembly, Virginia's date of conformity... will advance from December 31, 2016 to February 9, 2018. This allows Virginia to conform to... most provisions of the Tax Cuts and Jobs Act (TCJA) and the Bipartisan Budget Act of 2018 that are effective for Taxable Year 2017... exception for the TCJA provision related to the medical expenses deduction [7.5% floor not conformed; addition required]." (The main 2017 What's New text, printed earlier, says Dec 31, 2016.) |
| 2018 | **December 31, 2018** | 2018 booklet What's New: "advanced from February 9, 2018 to December 31, 2018, subject to certain exceptions... Tax Bulletin 19-1" |
| 2019 | **December 31, 2019** | 2019 booklet What's New: "advanced from December 31, 2018 to December 31, 2019... Tax Bulletin 20-1" |
| 2020 | **December 31, 2020** | 2020 booklet cover insert: "Because Virginia's date of conformity to federal tax law is fixed at December 31, 2020, Virginia does not currently conform to federal tax legislation enacted during 2021, including the American Rescue Plan Act of 2021 [ARPA unemployment exclusion → addback on 2020 VA return]." 2021 booklet What's New confirms: "advanced from December 31, 2019 to December 31, 2020... also generally conformed Virginia to... CARES... and the Consolidated Appropriations Act, 2021," with deconformity from CARES NOL/excess-business-loss/§163(j) changes (Tax Bulletin 21-4) |
| 2021 | **December 31, 2021** | 2022 booklet What's New: "advanced from December 31, 2020 to December 31, 2021... allows Virginia to generally conform to the American Rescue Plan Act of 2021 (ARPA)... See Tax Bulletin 22-1." (The TY2021 booklet itself, printed pre-session, states Dec 31, 2020 — superseded.) |
| 2022 | **December 31, 2022** | Tax Bulletin 23-1 (Feb 27, 2023; SB 882, 2023 Acts c. 1): date advanced from Dec 31, 2021 to Dec 31, 2022, conforming to the Inflation Reduction Act and SECURE 2.0/CAA 2023. (The TY2022 booklet, printed pre-session, states Dec 31, 2021 — superseded.) |
| 2023 | **Rolling conformity** | 2023 booklet What's New: "Effective for taxable years beginning January 1, 2023, Virginia will conform to the Internal Revenue Code (IRC) on a rolling basis, including any federal extenders, subject to certain exceptions." Guardrail: no automatic conformity to federal changes with revenue impact ≥ $15M in the year of enactment or any of the 4 succeeding fiscal years (TB 23-1 / 2023 legislation) |
| 2024 | **Rolling conformity** | 2024 booklet What's New: "Virginia continues to conform to the Internal Revenue Code on a rolling basis, subject to certain exceptions" |
| 2025 | **Rolling conformity, with specific OBBBA exceptions** | 2025 booklet What's New: "Except where specifically noted, Virginia conforms to the provisions of 2025 H.R.1 [P.L. 119-21] to the extent they affect the computation of federal adjusted gross income or federal itemized deductions for individuals... for Taxable Year 2025." See Tax Bulletin 26-1. NOTE for out-years: Va. Code § 58.1-301 as amended by the 2026 GA (2026 c. 7; 2026 Sp. Sess. I c. 1) now reads as a **fixed date of December 31, 2025** plus automatic conformity to later extenders — i.e., Virginia stepped back from pure rolling conformity after TY2025 |

**Standing decouplings (recited in every booklet's What's New, 2017–2025; Va. Code § 58.1-301(B)):**
- **Bonus depreciation** (IRC §§ 168(k), (l), (m), (n), 1400L, 1400N) — never conformed; FDC addition/subtraction.
- Five-year carryback of 2008–2009 NOLs (§ 172(b)(1)(H)); applicable high-yield discount obligations (§ 163(e)(5)(F)); deferral of COD income (§ 108(i)).
- **Medical expense deduction floor** — Virginia deconformed from the TCJA/later reductions of the floor below 10% of FAGI (2017 insert; 2019+ Schedule A: "you are allowed a medical expense deduction only for qualified expenses that exceed 10 percent of federal adjusted gross income").
- **Pease limitation** — from TY2019, VA deconforms from the federal *suspension* (and from OBBBA's replacement limitation), i.e., VA applies its own overall limitation (see §3).
- **CARES Act (affecting TY2018–2020):** deconformity from suspension of NOL limitations, excess-business-loss suspension, and the §163(j) 50%-ATI increase (TB 21-4).
- **§163(j):** VA conforms to the federal limitation but allows an individual **subtraction of a share of disallowed business interest**: 20% (TY2018–2021), 30% (TY2022–2023), **50%** ("Effective for taxable years beginning on or after January 1, 2024, the Business Interest Deduction has increased from 30% to 50%" — 2024 booklet); the 2025 booklet states the deduction "is reduced to 20% for Taxable Year 2025."
- **PPP (TY2020):** deconformity from full expense deductibility for forgiven PPP loans; a limited deduction (up to $100,000) was allowed — per TB 21-4 and the 2021 booklet's CAA/CARES discussion (see verification notes).
- **OBBBA (TY2025):** deconformity from immediate expensing of qualified production property, immediate expensing of domestic R&E (§ 70302, incl. retroactive/catch-up), increased § 179-type expensing limits (§ 70306); SALT cap & new overall limitation handled per §3 (TB 26-1).

## 7. Filing statuses on Form 760

Verified in every booklet 2017–2025 (Filing Status instruction, ~p. 7–8) and on the 2024 Form 760 face:

- **Only three statuses exist:** "1 = Single, 2 = Joint, 3 = Married filing separately" ("Filing Status Enter in box (1 = Single, 2 = Joint, and 3 = Married Filing Separately)" — 2024 Form 760).
- **Head of Household is NOT a Virginia filing status.** Federal HoH filers file as **Filing Status 1 (Single)** and mark an informational oval: "Fill in the Head of Household oval if your filing status is Single and you checked the Head of Household box on your federal return." (Identical sentence in 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, and 2025 booklets; 2024 form face: "Federal head of household? Filing Status 1 only YES".) The oval has **no rate/deduction consequence** — HoH gets single standard deduction, single threshold, same rate schedule. (Exception: VA's Pease-style Schedule A limitation uses *federal* filing status, so HoH thresholds exist there — see §3.)
- **MFS on a joint federal return:** permitted; "If using Filing Status 3, enter the spouse's Social Security Number at the top of the form and the spouse's name" (all booklets). Line 1: "If married filing separately (Filing Status 3), enter only the amount of income attributable to you" of joint FAGI. Itemized deductions and dependent exemptions not separately accountable are allocated proportionately to each spouse's share of combined FAGI; "the spouse claiming an exemption for a dependent must be reporting at least half of the total federal adjusted gross income" (2017 booklet, Filing Status section; carried forward). Mixed-residency couples: resident spouse files Form 760 FS3 unless electing joint resident treatment (2017 booklet, same section).
- Same-sex marriages recognized per Tax Bulletin 14-7 (2017 booklet note).

## Verification notes (gaps / secondary-source items / flags)

1. **TY2025 standard deduction is $8,750/$17,500 — not the $8,500/$17,000 hypothesized.** Booklet-verified (2025 Line 11) and statute-verified. Encode TY2024 = $8,500/$17,000, TY2025 = $8,750/$17,500.
2. **Sunset drift:** each booklet's stated sunset kept moving (2019–2024 booklets: revert after TY2025; 2025 booklet: revert after TY2026; statute as of July 2026: enhanced amounts through TY2029, revert TY2030). Treat sunset statements as vintage-specific.
3. **SALT cap does NOT flow through to VA itemized deductions for TY2019–2025** (deconformed; VA Sch. A "Taxes You Paid" uncapped except the Line 5a entry). It DID flow through for **TY2018** (with addback proration). This contradicts the "federal $10k SALT cap flows through post-2018" premise in some secondary summaries.
4. **Virginia applies its own Pease-style limitation for TY2019+** (thresholds transcribed for 2019, 2022, 2024 from Schedule A instructions; 2020, 2021, 2023, 2025 threshold values were not individually fetched — same mechanism confirmed via each year's 760 booklet, but pull the year's Va. Schedule A instructions if exact thresholds are needed). 2017: federal Pease flows through; 2018: none.
5. **Conformity-date sourcing:** TY2021 and TY2022 operative dates come from the *following* year's booklet and TB 23-1 respectively, because those booklets went to print before the conformity bill passed (booklets printed Dec 31, 2020 / Dec 31, 2021 respectively). All other years are same-booklet-verified.
6. **PPP $100,000 deduction limit (TY2020)** is cited here from Tax Bulletin 21-4's treatment as referenced in the 2021/2022 booklets; the exact $100,000 figure was **not** transcribed from a 760 booklet in this pass (bulletin-level primary source; booklet grep found no "Paycheck Protection" string).
7. **§ 58.1-321 nuance:** the statutory threshold is VAGI "plus the modification specified" (a cross-reference not transcribed); the booklets operationalize the test purely on Line 9 VAGI. Encode per the form (VAGI).
8. **Statute text as of 2026-07-18** was used for § 58.1-301, -320, -321, -322.03; historical versions were not pulled, but year-specific booklets pin the operative values for each TY. The 2026 amendments (§ 58.1-301 re-fix at Dec 31, 2025; std-ded extension to 2029) affect TY2026+ only.
9. **2023/2024 Schedule A instruction revisions** and the **revised 2025 Schedule A instructions** (OBBBA handling) were not separately downloaded (2025 file not found at the standard URL pattern at fetch time); 2025 treatment is transcribed from the 2025 760 booklet's conformity section.
10. Web fetches of statute pages used an LLM-assisted reader; all booklet quotations were extracted directly from PDF text (`pdftotext`), not paraphrased.

## Sources

**Form 760 instruction booklets (Virginia Dept. of Taxation) — all downloaded 2026-07-17/18:**
- TY2017: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2017-form-760-instructions.pdf
- TY2018: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2018-form-760-instructions.pdf
- TY2019: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2019-760-instructions.pdf
- TY2020: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2020-form-760-instructions.pdf
- TY2021: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2021-760-instructions.pdf
- TY2022: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2022-760-instructions.pdf
- TY2023: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2023-760-instructions.pdf
- TY2024: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf
- TY2025: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf

**Virginia Schedule A instructions:**
- TY2019: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2019-schedule-a-instructions.pdf (Rev. 02/20)
- TY2022: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2022-schedule-a-instructions.pdf (Rev. 09/22)
- TY2024: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-schedule-a-instructions.pdf (Rev. 08/24)

**Forms:**
- 2024 Form 760: https://www.tax.virginia.gov/sites/default/files/taxforms/individual-income-tax/2024/760-2024.pdf

**Va. Code (law.lis.virginia.gov, current text fetched 2026-07-18):**
- § 58.1-301 (IRC conformity): https://law.lis.virginia.gov/vacode/title58.1/section58.1-301/
- § 58.1-320 (rates): https://law.lis.virginia.gov/vacode/title58.1/section58.1-320/
- § 58.1-321 (filing thresholds): https://law.lis.virginia.gov/vacode/title58.1/section58.1-321/
- § 58.1-322.03 (deductions & exemptions): https://law.lis.virginia.gov/vacode/title58.1/section58.1-322.03/

## Pease thresholds — completion (appended 2026-07-18)

Completes verification-note items 4 and 9 (both now **superseded** by this section). All seven years of Virginia Schedule A instructions were located and text-extracted; the "Limited Itemized Deduction Worksheet" (VA Pease) trigger thresholds — compared against **Form 760/760PY/763 Line 1 (FAGI)**, using **federal filing status** — are transcribed below. Wording in each year: "if the amount on Line 1 of Forms 760, 760PY, or 763 exceeds [amounts], use the Limited Itemized Deduction Worksheet to compute your itemized deduction limitation and state and local income tax modification."

| TY | MFJ / QW-QSS | Head of Household | Single | MFS | Citation |
|---|---|---|---|---|---|
| 2019 | $326,050 | $298,850 | $271,700 | $163,025 | 2019 Va. Schedule A instructions (Rev. 02/20), "Total Itemized Deductions" |
| 2020 | $326,050 | $298,850 | $271,700 | $163,025 | 2020 Va. Schedule A form+instructions (`schedule-and-instructions-2020.pdf`), same section |
| 2021 | $334,150 | $306,300 | $278,450 | $167,075 | 2021 Va. Schedule A instructions, same section |
| 2022 | $343,950 | $315,300 | $286,600 | $171,975 | 2022 Va. Schedule A instructions (Rev. 09/22), same section |
| 2023 | $368,900 | $338,150 | $307,400 | $184,450 | 2023 Va. Schedule A instructions, same section |
| 2024 | $388,400 | $356,000 | $323,650 | $194,200 | 2024 Va. Schedule A instructions (Rev. 08/24), same section |
| 2025 | $399,200 | $365,950 | $332,700 | $199,600 | 2025 Schedule A instructions (revised edition reflecting 2026 GA legislation), same section |

Notes:
- **TY2019 and TY2020 print identical thresholds** — transcribed as published (the department did not index between those two years).
- The worksheet is the pre-TCJA federal §68 computation: reduction = lesser of 3% of the Line 1 excess over the threshold or 80% of affected itemized deductions; Part B of the worksheet also produces the proportional state/local income-tax modification used on Sch. A Line 18 in place of Line 5a when the limitation binds (see §3).
- **TY2025 revision (supersedes part of §3's TY2025 row and verification note 9):** the revised 2025 Schedule A instructions state: "UPDATE: See changes to the Virginia SALT cap amounts resulting from legislation passed by the 2026 General Assembly... The 2026 Virginia General Assembly passed legislation that conforms the Virginia SALT cap amounts to the federal SALT cap amounts. The applicable cap amount is determined under federal law for the taxable year and is generally **$20,000 for married filing separately and $40,000 for all other taxpayers for Taxable Year 2025** returns. See Tax Bulletin 26-1." Mechanics per the revised instructions: Line 5a income taxes are capped at the Virginia SALT cap amount **only when** the return is subject to the limited itemized deduction computation ("Otherwise, enter the state and local income taxes paid during the taxable year"); sales-tax electors are capped at the VA SALT cap amount; and "Lines 5b and 5c. Real and Personal Property Taxes... The SALT cap does not apply to this Virginia deduction." So for TY2025, property taxes remain uncapped on the VA return, the income-tax entry cap is $40,000/$20,000 (and washed by the Line 18 addback), and the VA Pease limitation applies at the thresholds above. This replaces the earlier-printed 2025 *booklet* framing ("generally deconforms from the federal SALT cap... applies the federal SALT cap amount... when calculating the Virginia overall limitation").

Additional sources for this section:
- 2020 Va. Schedule A form + instructions: https://www.tax.virginia.gov/sites/default/files/taxforms/individual-income-tax/2020/schedule-and-instructions-2020.pdf
- 2021 Va. Schedule A instructions: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2021-schedule-a-instructions.pdf
- 2023 Va. Schedule A instructions: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2023-schedule-a-instructions.pdf
- 2025 Schedule A instructions (revised): https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-sch-a-instructions.pdf

**Bulletins / other:**
- Tax Bulletin 23-1 (TY2022 conformity date; rolling conformity from TY2023): https://www.tax.virginia.gov/sites/default/files/inline-files/tb-23-1-date-of-irc-conformity-advanced.pdf
- Referenced but not fetched: Tax Bulletins 19-1, 20-1, 21-4, 22-1, 26-1 (cited within booklets).
