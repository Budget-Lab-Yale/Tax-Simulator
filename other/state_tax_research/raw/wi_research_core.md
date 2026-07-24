# Wisconsin Individual Income Tax — Core Mechanics Research Packet (TY2017–TY2025)

**Prepared:** 2026-07-24, for Tax-Simulator state-tax module encoding.
**Method:** Year-specific Form 1 instruction booklets (I-111), the Form 1 itself, and Schedules WD, I, and SB for TY2017–TY2025 were downloaded as PDFs from `revenue.wi.gov` (all nine years are still posted: `TaxForms2017through2019/` for 2017–19, `TaxForms{year}/` for 2020+; the NBER archive was not needed and is currently 403-blocked anyway) and text-extracted with pypdf. Bracket schedules for every year were taken from the DOR's own "Individual Income Tax Rates" FAQ page (live page for TY2025; Internet Archive captures of the same DOR page for TY2017–TY2024) and cross-checked against each booklet's Tax Computation Worksheet. Sliding-standard-deduction formula parameters come from the Legislative Fiscal Bureau *Informational Paper 2, Individual Income Tax* (Jan 2017/2019/2021/2023/2025 editions, which print the exact formula table for even tax years) and, for odd years, from least-squares recovery off the DOR Standard Deduction Table in each booklet (fits are exact to ≤$3 across 170–250 table rows with the statutory slopes; see §3). Statute cites are Wis. Stat. ch. 71. Cross-checks: PolicyEngine-US parameter tree (coverage only).

**Source inventory (all fetched and read):**

| TY | Form 1 booklet | Other primary sources read |
|---|---|---|
| 2017 | `TaxForms2017through2019/2017-Form1-Inst.pdf` | 2017 Form 1 (`2017-Form1f.pdf`), Sch WD inst, Sch I inst (Rev. 2-18), DOR rate page (archive.org 2017-12-28) |
| 2018 | `2018-Form1-Inst.pdf` | Sch WD inst, Sch I inst (R. 12-18), LFB IP-2 Jan-2019 (TY2018 SSSD table), DOR rate page (2018-12-13 capture) |
| 2019 | `2019-Form1-Inst.pdf` | Sch WD inst, Sch I inst (R. 01-20), DOR rate page (2019-12-09 capture) |
| 2020 | `TaxForms2020/2020-Form1-Inst.pdf` | Sch WD, Sch I, Sch SB inst; LFB IP-2 Jan-2021 (TY2020); rate page (2020-12-15 capture) |
| 2021 | `2021-Form1-Inst.pdf` | Sch WD, Sch I, Sch SB inst; rate page (2021-12-30 capture) |
| 2022 | `2022-Form1-Inst.pdf` | Sch WD, Sch I (incl. Oct-2023 update note), Sch SB inst; LFB IP-2 Jan-2023 (TY2022); rate page (2022-12-11 capture) |
| 2023 | `2023-Form1-Inst.pdf` | Sch WD, Sch I (R. 12-23), Sch SB inst; rate page (2023-11-30 capture) |
| 2024 | `2024-Form1-Inst.pdf` | Sch WD, Sch I, Sch SB inst; Schedule WI-2441 inst (N. 10/24); LFB IP-2 Jan-2025 (TY2024); rate page (2024-12-14 capture) |
| 2025 | `TaxForms2025/2025-Form1-Inst.pdf` | 2025 Form 1 (`2025-Form1f.pdf`), Sch WD, Sch I (R. 12-25), Sch SB inst; live DOR rate page |

---

## 1. Starting point, return structure, and federal conformity (Schedule I)

### 1.1 Starting point

Form 1 line 1 = **federal adjusted gross income**, every year 2017–2025 (2017 Form 1 line 1: "Federal adjusted gross income"; 2025 Form 1 line 1 same). Wisconsin income (line 13 in 2017–2019; line 7 in the 2020+ redesign) = FAGI + additions − subtractions. Then: sliding standard deduction, $700/$250 exemptions, tax from table/rate schedule, nonrefundable credits (itemized deduction credit, school property tax credit, married couple credit, etc.), refundable credits (EITC, homestead).

**Form redesigns to be aware of:**
- **2020**: addition/subtraction modifications moved off Form 1 onto new **Schedule AD** (additions) and **Schedule SB** (subtractions) (2020 booklet "New in 2020").
- **2022**: Form 1 line 1 is now FAGI **as filed with the IRS**; Schedule I conformity adjustments appear explicitly on line 2, and "FAGI for Wisconsin purposes" is line 3 (2022 booklet "New in 2022"). Before 2022 the Schedule I adjustment was folded into line 1.

### 1.2 Fixed-date conformity: IRC reference date by tax year

Wisconsin is **fixed-date** conformity (Wis. Stat. 71.01(6)), updated legislatively with lags and with a standing list of never-adopted provisions. From each year's Schedule I instructions, General Instructions introduction:

| TY | IRC "enacted as of" | Updating act |
|---|---|---|
| 2017 | **Dec 31, 2016** (pre-TCJA) | — |
| 2018 | **Dec 31, 2017** (includes TCJA) | 2017 Wis. Act 231 (Apr 2018) — act number from secondary sources, **UNVERIFIED** |
| 2019 | Dec 31, 2017 | — |
| 2020 | Dec 31, 2017, plus selected CAA-2021 provisions adopted retroactively by **2021 Act 1** (Feb 18, 2021): EITC 2019-earned-income lookback; PPP loan-forgiveness exclusion + expense deductibility (2020 Sch I inst., front-page updates) | |
| 2021 | **Dec 31, 2020** | 2021 Act 1 |
| 2022 | Dec 31, 2020 | — |
| 2023 | **Dec 31, 2022** | 2023 Acts 35/36 (per Oct-2023 update note in the 2022 Sch I instructions) |
| 2024 | Dec 31, 2022 | — |
| 2025 | **Dec 31, 2022** — i.e., **Wisconsin has NOT conformed to OBBBA (P.L. 119-21)** as of the 2025 forms (2025 Sch I inst., R. 12-25) | — |

**Standing never-adopted items (all years):** federal bonus depreciation (sec. 168(k)) — WI requires straight recomputation on Sch I; exclusion for discharge of qualified-principal-residence indebtedness; (pre-2019) sec. 1202 small-business-stock exclusion (WI follows 1202 for tax years beginning after 12/31/2018 — 2022 Sch I update note); (2017 only) DPAD.

**Era-by-era individual-side Schedule I content:**

- **TY2017**: IRC 12/31/2016, so provisions enacted during/after 2017 that were retroactive to 2017 federally were NOT adopted — notably the TCJA's retroactive **7.5% medical-expense floor: Wisconsin used 10%** for the 2017 itemized deduction credit (2017 Sch I inst. revision note, Feb 2018).
- **TY2018–2020**: TCJA fully in the reference date, so **Wisconsin conformed to essentially all TCJA AGI-level provisions from 2018 on** (alimony repeal, moving-expense suspension, etc.) — no M1NC-style rebuild needed (contrast MN). Divergences are the *post-2017* federal acts: for **2019–2020 the medical floor is again 10% for WI vs 7.5% federal** (extension acts enacted after 12/31/17; 2019 Sch I inst. Part II item 5, verified); mortgage-insurance-premium deduction never usable for the WI credit (same, item 4); **2020**: CARES $300 above-the-line charitable deduction NOT adopted (addback), CARES 100%-of-AGI charitable limit NOT adopted (WI keeps 60%), and the **ARPA $10,200 UI exclusion NOT adopted — addback on 2020 Sch I** (2020 Sch I inst., front-page notice); PPP exclusion adopted via 2021 Act 1.
- **TY2021–2022**: IRC 12/31/2020 picks up CARES/CAA permanently (7.5% medical floor now conforms). Not adopted: ARPA items — student-loan-discharge exclusion (108(f)(5)), restaurant revitalization grants (separately exempted by 2021 Act 156 as a state subtraction), 100% restaurant-meal deduction, and the **ARPA EITC changes (investment-income limit etc.) — WI EITC must be computed off a pre-ARPA federal EITC for 2021–2022** (2021 Sch I inst.).
- **TY2023–2025**: IRC 12/31/2022 picks up ARPA and SECURE 2.0 (enacted 12/29/2022). 2023 booklet "New in 2023": **Wisconsin has adopted the P.L. 117-2 (ARPA) EITC changes from 2023** (but still requires a qualifying child — see §9). **TY2025**: OBBBA not adopted; the 2025 Sch I lists P.L. 119-21 differences, which are almost entirely business-side (168(k) 100% bonus — already a WI difference; §174/70302 R&E; §70303 §179 interplay; etc.). The headline OBBBA individual items (tips/overtime/senior/car-loan-interest deductions) are federal *below-the-line* deductions and never touch FAGI, so they create **no Wisconsin effect and no Schedule I entry**.

### 1.3 Recommended modeling treatment

**Rolling group 0 (use our federal-calculator FAGI as Form 1 line 1) with documentation** — do **not** build an SC-style excluded-group conformity layer. Rationale: Wisconsin adopted TCJA whole for 2018+, so there is no pre/post-TCJA regime break to reconstruct (unlike MN 2018). The AGI-level gaps that matter for a PUF-based simulator, by year:
1. **2020 UI**: since WI both (a) rejected the ARPA $10,200 exclusion and (b) has its own UI worksheet anyway (§6.4), compute the WI UI subtraction off **gross** federal-includable UI (i.e., before any ARPA exclusion) — this simultaneously implements the Sch I addback and the WI worksheet.
2. **2020(–2021) above-the-line charitable ($300/$600)**: if the federal calculator deducts it in AGI, add back for WI.
3. **WI EITC base**: pre-ARPA federal EITC parameters for 2021–2022; conformed 2023+ (§9).
4. **2017/2019/2020 medical floor 10% vs 7.5%** — only affects the 5% itemized deduction credit; apply a 10% floor in the credit's medical term those years.
Everything else on Schedule I (bonus depreciation, QPRI COD, business items) is either invisible in our data or de minimis — document as known differences.

---

## 2. Rates and brackets, TY2017–TY2025

Four brackets all years. Filing statuses: Single and HoH share one schedule (also estates/trusts); MFS = half of MFJ everywhere. **Verified rate history** (booklet "New in ..." pages + DOR rate pages + LFB budget paper 325, 2025-27 cycle):

| TY | Rates (b1/b2/b3/b4) | Change and act |
|---|---|---|
| 2017–2018 | 4.00 / 5.84 / 6.27 / 7.65 | — |
| 2019 | **3.86 / 5.04** / 6.27 / 7.65 | 2019 Act 9 cut b2 5.84→5.21 permanently; 2019 Act 10 (Wayfair-revenue offset) then reduced b1/b2 **for 2019 only** to the certified 3.86/5.04. The 3.86/5.04 rates are what the 2019 tax tables and the DOR rate schedule use (2019 booklet "New in 2019"; DOR rate page 2019 capture). Do NOT encode 4.00/5.21 for 2019. |
| 2020–2022 | 3.54 / 4.65 / 6.27 (→5.30 in 2021) / 7.65 | 2019 Act 10 made the Wayfair-funded cuts permanent at **3.54/4.65 from TY2020** (2020 booklet). **2021 Act 58** cut b3 6.27→**5.30** from TY2021 (2021 booklet). |
| 2023–2025 | **3.50 / 4.40 / 5.30 / 7.65** | 2023 Act 19 cut b1/b2 to 3.50/4.40 from TY2023 (2023 booklet; LFB IP-2 Jan-2025). (Act 19's larger b3 cut was partially vetoed.) No rate change 2024 or 2025; the Feb-2024 bracket-expansion bills were vetoed (**bill numbers UNVERIFIED**). |

**Bracket thresholds** (top of brackets 1/2/3; bracket 4 is open-ended). Source: DOR "Individual Income Tax Rates" FAQ page for each year (archived captures 2017–2024; live page for 2025), cross-checked against each booklet's Tax Computation Worksheet (top-bracket start and subtraction constants match in every year checked: 2017, 2019, 2021, 2023, 2025). Indexed annually under 71.06(2e) (CPI, rounded to nearest $10; no indexation of the top-bracket threshold pre-2013 history not relevant here).

**Single / Head of household (and estates & trusts):**

| TY | b1 top | b2 top | b3 top (7.65% starts) |
|---|---|---|---|
| 2017 | 11,230 | 22,470 | 247,350 |
| 2018 | 11,450 | 22,900 | 252,150 |
| 2019 | 11,760 | 23,520 | 258,950 |
| 2020 | 11,970 | 23,930 | 263,480 |
| 2021 | 12,120 | 24,250 | 266,930 |
| 2022 | 12,760 | 25,520 | 280,950 |
| 2023 | 13,810 | 27,630 | 304,170 |
| 2024 | 14,320 | 28,640 | 315,310 |
| 2025 | 14,680 | **50,480** | 323,290 |

**Married filing jointly:**

| TY | b1 top | b2 top | b3 top |
|---|---|---|---|
| 2017 | 14,980 | 29,960 | 329,810 |
| 2018 | 15,270 | 30,540 | 336,200 |
| 2019 | 15,680 | 31,360 | 345,270 |
| 2020 | 15,960 | 31,910 | 351,310 |
| 2021 | 16,160 | 32,330 | 355,910 |
| 2022 | 17,010 | 34,030 | 374,600 |
| 2023 | 18,420 | 36,840 | 405,550 |
| 2024 | 19,090 | 38,190 | 420,420 |
| 2025 | 19,580 | **67,300** | 431,060 |

**Married filing separately:** exactly half the MFJ figures every year (2017: 7,490/14,980/164,900 … 2024: 9,550/19,090/210,210; 2025: 9,790/**33,650**/215,530).

**2025 second-bracket expansion:** the b2 top jumps from ~$29.4k (indexed) to $50,480 single / $67,300 MFJ / $33,650 MFS in TY2025 — this is the enacted bracket-2 expansion in the 2025-27 budget act (**2025 Act 15**, signed July 2025; act number consistent with the 2025 retirement subtraction below but **UNVERIFIED as the act cite for the bracket change**; the *values* are verified from the live DOR rate page and the 2025 booklet's Tax Computation Worksheet, which prints the $215,530 MFS top-bracket start). The user-prompt's "2024 Act expanding bracket 2" is this 2025 change; nothing was enacted for 2024.

Note: taxpayers with taxable income < $100,000 use the tax table (built from these schedules on $100/$500 cells at midpoints); encode the continuous schedule — the table rounding is a ≤ few-dollar artifact.

---

## 3. Sliding standard deduction (Wis. Stat. 71.05(22))

Wisconsin's standard deduction is a function of **Wisconsin income** (Form 1 line 14 in 2017–19 / line 7 in 2020+), phasing down linearly to zero. Statutory phase-out **rates are fixed**: Single **12.0%**, MFJ **19.778%**, MFS **19.778%**, HoH **22.515%**, with the HoH deduction floored at the single-filer deduction once its formula falls below it (so HoH converges to the single schedule at higher incomes). Maxima and phase-out start incomes are indexed under 71.05(22)(ds) (rounded to nearest $10). No aged/blind add-on (unlike federal; the aged get the $250 exemption instead, §4).

**Formula: SD = clamp( MAX − rate × (WI income − PO_start), 0, MAX )**, HoH additionally floored at the single amount.

Even-year rows below are transcribed from LFB Informational Paper 2 Table 1 (Jan-2019: TY2018; Jan-2021: TY2020; Jan-2023: TY2022; Jan-2025: TY2024). Odd-year rows were recovered from the official DOR Standard Deduction Table printed in that year's Form 1 booklet: the maxima are printed literally in the table's first row ("$0 to X → max"), and the phase-out starts were solved from ~170–250 declining-region table cells using the statutory slopes (every year's fit is exact to ≤$3, and the method reproduces the LFB values perfectly in all four even years). Odd-year PO-start values are rounded to the nearest $10, consistent with 71.05(22)(ds); flagged (t).

**Single** (zero at PO_start + max/0.12):

| TY | Max | Phase-out start |
|---|---|---|
| 2017 | 10,380 | 14,960 (t) |
| 2018 | 10,580 | 15,250 |
| 2019 | 10,860 | 15,660 (t) |
| 2020 | 11,050 | 15,940 |
| 2021 | 11,200 | 16,150 (t) |
| 2022 | 11,790 | 16,990 |
| 2023 | 12,760 | 18,400 (t) |
| 2024 | 13,230 | 19,070 |
| 2025 | 13,560 | 19,550 (t) |

**Married filing jointly:**

| TY | Max | Phase-out start |
|---|---|---|
| 2017 | 19,210 | 21,590 (t) |
| 2018 | 19,580 | 22,010 |
| 2019 | 20,110 | 22,600 (t) |
| 2020 | 20,470 | 23,000 |
| 2021 | 20,730 | 23,300 (t) |
| 2022 | 21,820 | 24,520 |
| 2023 | 23,620 | 26,550 (t) |
| 2024 | 24,490 | 27,520 |
| 2025 | 25,110 | 28,210 (t) |

**Married filing separately:**

| TY | Max | Phase-out start |
|---|---|---|
| 2017 | 9,130 | 10,250 (t) |
| 2018 | 9,300 | 10,450 |
| 2019 | 9,550 | 10,730 (t) |
| 2020 | 9,720 | 10,920 |
| 2021 | 9,850 | 11,060 (t) |
| 2022 | 10,370 | 11,640 |
| 2023 | 11,220 | 12,600 (t) |
| 2024 | 11,630 | 13,060 |
| 2025 | 11,930 | 13,390 (t) |

**Head of household** (same phase-out start as single each year; floor = single SD):

| TY | Max | Phase-out start |
|---|---|---|
| 2017 | 13,400 | 14,960 (t) |
| 2018 | 13,660 | 15,250 |
| 2019 | 14,030 | 15,660 (t) |
| 2020 | 14,280 | 15,940 |
| 2021 | 14,470 | 16,150 (t) |
| 2022 | 15,230 | 16,990 |
| 2023 | 16,480 | 18,400 (t) |
| 2024 | 17,090 | 19,070 |
| 2025 | 17,520 | 19,550 (t) |

Sanity check against user's prompt: TY2024 single max $13,230 / PO start $19,070 / 12%; MFJ $24,490 / 19.778% (the prompt's "$13,230, $15,3xx" figures pair a 2024 max with a ~2020 threshold — use the table above).

**Dependent-filer limitation** (2017 booklet, "Standard Deduction Worksheet for Dependents"; same worksheet in later years): if claimable as a dependent, SD = min( table SD, max( floor, earned income + $350 ) ). For 2017 the floor is $1,050. The floor/addition track the federal dependent-SD parameters; the 2025 booklet's dependent filing test uses $1,350 (matching the 2025 federal floor), so encode the federal-per-year dependent floor with the $350 earned-income add-on (**mid-year worksheet amounts UNVERIFIED individually; endpoint years verified**).

---

## 4. Personal exemptions (71.05(23))

- **$700** per taxpayer, spouse, and each person who qualifies as the filer's federal **dependent** — all years 2017–2025 (2017 booklet line 17 instructions; 2025 booklet line 10 instructions: "Multiply that number by the amount indicated ($700 or $250)"). Post-TCJA years use the federal dependent *count* even though federal exemptions are $0.
- **Additional $250** for the taxpayer and/or spouse **age 65+** (only persons "allowed the $700 exemption" as filer/spouse — not dependents). All years.
- No phase-out. Amounts unchanged since TY2001 (LFB IP-2 Jan-2023, p.12).

---

## 5. Capital gains and losses (Schedule WD; 71.05(6)(b)9, 9m; 71.05(10)(c))

- **30% long-term capital gain exclusion**: "you may exclude 30% of the net capital gain from assets held more than one year (**60% in the case of farm assets**)" — identical sentence in every Schedule WD instruction 2017–2025 (verified 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025). Mechanically, WD nets ST and LT per federal rules on Wisconsin basis, and the exclusion applies to net LTCG (WD Part III lines 19–27; farm assets get the 60% rate via lines 21–25). Result flows to Form 1 as a capital gain/loss subtraction (or addition if WI gain > federal).
- **Net capital LOSS offset against ordinary income**:
  - **TY2017–2022: $500/year** (Sch WD inst. each year: "The amount of net capital loss that can be applied against other income after offsetting capital gains is limited to $500"). No MFS split stated — $500 per return; the line-28 computation is min(loss, $500, WI ordinary income).
  - **TY2023+: $3,000 ($1,500 MFS)** (2023 booklet "New in 2023"; 2023/2024/2025 Sch WD inst.). Amendment to 71.05(10)(c) effective for taxable years beginning after 12/31/2022; enacting act appears to be **2021 Act 157 §4** (docs.legis search result; **act number UNVERIFIED**).
- Unused losses carry forward (WD Part V computes the Wisconsin carryover). **Known-difference candidate**: pre-2023 the WI-vs-federal loss cap ($500 vs $3,000) plus separate WI carryover stocks; see §13.

---

## 6. Retirement, elderly, and income-exclusion subtractions

All on Form 1 lines 9–11 (2017–2019) / Schedule SB (2020+). Verified from 2017 booklet and 2025 Schedule SB instructions; provisions stable across the window except as noted.

### 6.1 Social Security — fully exempt, all years
"Social security benefits are not taxable for Wisconsin. You may subtract any social security benefits that were taxable on your federal [return]" (2017 booklet, Line 9; 2025 Sch SB Line 4). Subtraction = federally taxable SS. Railroad Retirement Board benefits likewise fully exempt (2025 Sch SB line 15).

### 6.2 $5,000 retirement income exclusion, 65+, income-limited (all years)
2017 booklet Line 11 code 26; 2025 Sch SB Line 17 ("$5,000 Retirement Income Subtraction (Income Restricted)") — identical parameters both endpoints:
- Age: taxpayer (and separately spouse) **65+** at year-end; per-qualifying-spouse cap of **$5,000** each.
- Income test: **FAGI < $15,000 single/HoH ($30,000 MFJ)**; MFS: the **sum of both spouses' FAGI < $30,000**. Cliff, not phase-out. Not indexed.
- Base: taxable IRA distributions + taxable pension/annuity income from a qualified plan, minus retirement benefits already subtracted elsewhere (military/uniformed, pre-1964 systems, railroad, and — 2025 — the new line-16 subtraction). Statute: 71.05(1)(ae) per secondary sources (**subsection cite UNVERIFIED**).

### 6.3 Fully exempt pensions (all years)
- **Military & uniformed services retirement**: retirement payments from the U.S. military retirement system (incl. RSFPP/SBP survivor payments) and U.S. government payments for Coast Guard / NOAA corps / PHS corps service are fully subtractable — **already in place in TY2017** (2017 booklet, Line 11 code 04). Not a 2021 change; what was new in 2021 was an **active-duty pay** subtraction (2021 booklet "New in 2021"; 2025 Sch SB line 18: basic/special/incentive pay under 37 USC ch. 3 & 5). Federal *civilian* (CSRS/FERS) pensions are NOT exempt (only via 6.2 or the pre-1964 rule).
- **Pre-1964 state/local systems** (Milwaukee city/county systems, Wisconsin State Teachers): exempt only if retired/member before 1/1/1964 (2017 booklet code 05). Unobservable in our data and a dying cohort — note-only.

### 6.4 Unemployment compensation partial exclusion (all years)
Wisconsin taxes UI under the pre-1987 federal formula (71.05(6)(b)8): WI-taxable UI = min( federally taxable UI, ½ × [ (income + UI) − base ] ), base = **$12,000 single / $18,000 MFJ / $0 MFS-lived-with-spouse / $12,000 MFS-apart-all-year**; subtraction = federal-taxable UI − WI-taxable UI (2017 booklet Line 8 worksheet; 2025 Sch SB line 3). Bases not indexed. For 2020, apply the worksheet to **pre-ARPA** federal-includable UI (§1.2).

### 6.5 NEW TY2025: $24,000 retirement income subtraction, 67+, credit-restricted (2025 Act 15)
2025 Sch SB Line 16, "Retirement Income Subtraction (Credits Restricted)" (2025 booklet "New in 2025"):
- Taxpayer (or spouse on MFJ) **67+** at end of 2025: subtract up to **$24,000** of federally taxable qualified-plan/IRA retirement income not already subtracted on lines 12–15; **$48,000** on MFJ if **both** spouses are 67+ ("regardless of how much retirement income each spouse received" — i.e., a joint cap, not per-spouse).
- **No income limit**, but claimants **forgo ALL credits** on Schedule CR and Form 1 lines 13–20 and 30–35 — i.e., itemized deduction credit, school property tax credit, married couple credit, additional CDCTC, EITC, homestead, etc., including carryforwards in/out.
- Encoding: compute liability both ways (subtraction-no-credits vs credits-no-subtraction) and take the minimum-tax branch.
- History: a similar exclusion passed the legislature in 2023-24 (AB 1021 / SB 435 era) and was **vetoed**; current law is the 2025 enactment. (Veto bill numbers **UNVERIFIED**.)

### 6.6 Other subtractions (note-only, mostly unobservable)
- **Medical care insurance** (Sch SB line 6) and **long-term care insurance** (line 7): 100% of premiums subtractable throughout 2017–2025 (subject to earned-income-type limits for employees/self-employed). Amounts subtracted must be excluded from the itemized-deduction-credit medical term (§7). Data-limited in the PUF — known difference.
- **Tuition & fees subtraction** (SB line 8): per-student cap $6,958 (2017) → $6,974 (2023) → $7,333 (2024) → $7,649 (2025), AGI phase-out (2017 single: full below $85,050, gone at $106,310; MFS half). Not modelable (no tuition data); note.
- **Private school tuition** (SB line 9): up to $4,000 elementary / $10,000 secondary per pupil (2017 booklet; unchanged). Note-only.
- **College savings (Edvest)** (SB line 10): per-beneficiary cap $3,280 (2019), $3,340 (2020), $3,560 (2022), $3,860 (2023), $5,000 (2024), $5,130 (2025) (booklet "New in" pages; 2017/2018/2021 values not pulled — **UNVERIFIED**). Note-only.
- Adoption expenses: $15,000/child from 2025 (was $5,000). Note-only.

---

## 7. Itemized deduction credit (71.07(5); Form 1 page 4, "Schedule 1")

Nonrefundable credit, **all years 2017–2025**: **5% × max(0, allowable-items − Wisconsin standard deduction)** (Form 1 Schedule 1 line 8 "Rate of credit is .05 (5%)" on both the 2017 and 2025 forms; the SD subtracted is the taxpayer's own sliding SD from §3).

**Allowable items (Schedule 1 lines 1–4) — federal Schedule A amounts with WI carve-outs; NO taxes-paid component:**
1. **Medical and dental** — federal Sch A *allowable* amount (i.e., after the AGI floor), MINUS any medical-care/LTC insurance claimed as a WI subtraction (worksheet in booklet). WI floor differs from federal in 2017, 2019, 2020 (10% WI vs 7.5% federal — §1.2); 7.5% both in 2018 and 2021+.
2. **Interest paid** — home-mortgage interest + points + investment interest (2017: Sch A lines 10–12, 14), EXCLUDING: interest on a second home located outside Wisconsin; a residence that is a boat; interest to purchase/hold U.S. government securities; mortgage-insurance premiums (never allowed for WI). Federal TCJA-era acquisition-debt caps flow through automatically 2018+ (conformity).
3. **Gifts to charity** — federal Sch A allowable amount; WI limitation percentages follow the WI IRC (60% cash limit 2018+; the CARES 100% election for 2020–21 not adopted — recompute at 60% for affected records).
4. **Casualty losses** — federally-declared-disaster losses only (so already only that post-TCJA; the 2017 WI form also says "only if ... federally-declared disaster").
- If the filer didn't itemize federally, they may compute a pro-forma federal Schedule A ("Write 'Wisconsin' at the top") — encode as: always compute the credit from itemizable amounts regardless of federal itemizing choice.
- 2017 only: federal Pease-limited amounts are used (worksheet for limited itemized deductions, 2017 booklet p.31); no Pease 2018+.
- Nonrefundable; no carryover.

---

## 8. Married couple credit (71.07(6); Form 1 page 4, "Schedule 2")

MFJ two-earner credit, **all years 2017–2025**: **3% of the lesser-earning spouse's qualified earned income, capped at $16,000 of income → max credit $480** (2017 Form 1 Schedule 2: "x .03", "Do not fill in more than $480", line 6 cap $16,000; 2025 Form 1 identical). Cap unchanged since TY2001 (LFB IP-2 Jan-2025).

**Qualified earned income** (per spouse) = taxable wages/salaries/tips/other employee compensation (**excluding deferred compensation**, interest, dividends, pensions, UI, other unearned income) + self-employment net profit (Sch C/F, K-1 SE income) − that spouse's employment-connected federal adjustments (2017: 1040 lines 24, 28, 32 — i.e., form-2106-type expenses, self-employed retirement contributions, IRA deduction; 2025: Sch 1 lines 12, 16, 20, 24e/f/g) − 403(b)/501(c)(18)(D) contributions − WI disability income exclusion. Nonrefundable. Encoding from PUF: use wages1/wages2 + SE splits; the deferred-comp exclusion is a small known difference.

---

## 9. Wisconsin EITC (71.07(9e))

Refundable; **requires at least one federal qualifying child** and full-year WI residency. Credit = percentage of the **federal EITC**, by number of qualifying children — **unchanged 2017–2025** (verified in 2017, 2021, 2025 booklets):

| Qualifying children | % of federal EITC |
|---|---|
| 0 | **0% (not eligible)** |
| 1 | **4%** |
| 2 | **11%** |
| 3+ | **34%** |

Conformity wrinkles: (a) 2020 — 2019-earned-income lookback allowed (2021 Act 1); (b) **2021–2022 — federal EITC must be recomputed under pre-ARPA rules** (investment-income limit, etc.) before applying the WI percentage (Sch I Part III / booklet step 2 references the Schedule I-adjusted federal credit); (c) **2023+ — ARPA EITC changes adopted** (2023 booklet "New in 2023"), childless expansion still irrelevant because WI requires a child.

---

## 10. Other credits

### 10.1 School property tax / rent credit (71.07(9); Form 1 line 22 in 2017, line 16 in 2025)
Nonrefundable, **all years**: **12% of the first $2,500 of property taxes** paid on the principal residence, plus **12% of "rent constituting property taxes" = 25% of rent (heat NOT included) or 20% of rent (heat included)** — combined renter+homeowner credit capped at **$300** per return (**$150** each for MFS and married-filing-as-HoH). Verified: 2017 booklet lines 22a/22b (+ tables: rent $3,500–3,600 → $85 heat-included / $107 heat-not-included = 12%×20%/25%×3,550, exact) and 2025 booklet line 16 ($300/$150 caution). Max reached at $2,500 of property taxes / $10,000 rent (no heat) / $12,500 rent (heat). Not claimable if taking the veterans property tax credit; rent on tax-exempt housing ineligible. **Modelable for owners from salt_prop** (12% × min(salt_prop, 2500)); renters unobservable in the PUF → known one-sided gap (see §13).

### 10.2 Working families tax credit (71.07(5m); Form 1 line 23 in 2017)
Eliminates net tax if WI AGI < **$9,000** (single/HoH/MFS) / **$18,000** (MFJ); phases out linearly over the next $1,000 (gone at $10,000/$19,000); not available to dependents or (per statute) those 65+ hitting other relief. Statute values verified (docs.legis 71.07(5m)); the 2017 booklet worksheet confirms the $9,000/$10,000 single range. Near-zero revenue; taxpayers in this range typically owe ~$0 after the SD anyway. Encode or skip — recommend encoding (cheap: multiply net tax by phase-out factor).

### 10.3 Homestead credit (71.51–71.55; Schedule H) — out of scope, document only
Refundable, income-tested property-tax/rent circuit breaker: household income (a broad concept incl. nontaxable SS, SSI, etc.) must be < **$24,680** — frozen at $24,680 in both 2017 and 2023 booklets (verified); credit = 80% of property taxes / rent-constituting-taxes (25%/20% of rent, capped at **$1,460** of taxes) less an offset of ~**8.785%** of household income above ~**$8,060** ($1,460/8.785%/$8,060 from memory — **UNVERIFIED**). Requires rent + nontaxable-income data we don't have; **recommend excluding** and documenting. NOTE: PolicyEngine models it (§12) → expected one-sided divergence on low-income records.

### 10.4 Child and dependent care — subtraction then credit
- **TY2017–2021: SUBTRACTION** equal to the federal Form 2441 line 6 amount (qualifying expenses actually used for the federal credit), capped **$3,000 / $6,000** (1 / 2+ qualifying persons) (2017 booklet Line 11; still a subtraction for 2021 — the credit begins 2022).
- **TY2022–2023: nonrefundable CREDIT = 50% of the federal CDCTC** (2022 booklet "New in 2022" + Line 14: "Multiply the amount of your federal credit by 50%"; subtraction repealed). Enacted by 2021 Act 58 (**act attribution UNVERIFIED**).
- **TY2024+: expanded credit via new Schedule WI-2441** (2023 Act 101 — **act number UNVERIFIED**; 2024 booklet "New in 2024"): credit = federal-style computation (federal applicable percentage 20–35% from the federal Form 2441 AGI table) but with **expense caps $10,000 (one qualifying person) / $20,000 (two+)** ($20,000 verified in WI-2441 instructions; **$10,000 one-person cap UNVERIFIED** — inferred, the instructions text extracted only the $20,000 sentence). Nonrefundable (Form 1 line 14, within the lines-13–20 nonrefundable block).
- MFS generally ineligible (must file joint unless the HoH-for-Wisconsin separation rules are met).

### 10.5 Armed forces member credit (71.07(6m)) — note-only
Up to $300 for active-duty pay earned while stationed outside the U.S. (2017 booklet line 21); largely mooted by the 2021 active-duty pay subtraction. Unobservable; skip.

---

## 11. Filing requirement (gross income thresholds)

Threshold = SSSD max (§3) + $700 × (1 filer or 2 spouses) (+$250 per 65+ spouse) — verified identity in 2017, 2023, 2025 tables. Booklet-verified anchors ("Who must file" table; gross income = all WI-reportable income before expenses, **excluding** exempt items like SS and U.S. gov interest):

| TY | Single <65 | MFJ both <65 | MFS <65 | HoH <65 |
|---|---|---|---|---|
| 2017 | 11,080 | 20,610 | 9,830 | 14,100 |
| 2023 | 13,460 | 25,020 | 11,920 | 17,180 |
| 2025 | 14,260 | 26,510 | 12,630 | 18,220 |

(+$250 per 65+ filer/spouse; MFS applies per spouse.) Intermediate years follow the identity from §3 tables (**not individually re-verified**). Nonresidents/part-year: $2,000 combined gross income (Form 1NPR) — out of scope. Note the model should compute returns regardless; thresholds matter only for non-filer imputation.

---

## 12. Cross-model coverage (TAXSIM 2017–2020; PolicyEngine 2021+)

**PolicyEngine-US** (parameter tree `gov/states/wi/tax/income/`, checked 2026-07-24 on main):
- `rates/`, `deductions/standard/` (sliding SD), `exemption/` — core mechanics present.
- `credits/`: `childcare_expense`, `earned_income`, **`homestead`**, `itemized_deduction`, `married_couple`, `property_tax` (school property tax credit).
- `subtractions/`: `capital_gain` (30% exclusion), `childcare_expense` (pre-2022 subtraction), `plan_529`, `retirement_income`, `unemployment_compensation`.
- Implications: (1) **PE includes the homestead credit** — if we exclude it (recommended), expect a one-sided divergence (PE lower net tax) on low-income records, concentrated among renters/elderly; classify before triaging. (2) No explicit SS-subtraction parameter — PE likely handles SS inside its WI AGI variable; verify at triage rather than assuming a gap. (3) Whether PE has the 2019 3.86/5.04 one-time rates, the 2023 $3,000 loss limit, the 2025 bracket-2 expansion, and the 2025 credits-restricted $24k subtraction is **UNVERIFIED** — check parameter values at encoding time.

**TAXSIM-35** (covers our 2017–2020 comparison years): WI calculator is expected to include brackets, the sliding standard deduction, exemptions, the married couple credit, WI EITC percentages, the 30% LTCG exclusion, and the property-tax/rent credits (TAXSIM has `rentpaid` and property-tax inputs used for state circuit breakers/credits, incl. homestead) — **all UNVERIFIED**; TAXSIM state internals are undocumented. Triage checks to run: (a) 2019 rates — TAXSIM may use statutory 4.00/5.21 instead of the certified 3.86/5.04 (a known trap this packet resolves — the tables used 3.86/5.04); (b) $500 capital-loss limit (TAXSIM state logic may just take federal capped −3,000); (c) itemized deduction credit medical floor 10% in 2019–2020; (d) whether TAXSIM applies the UI worksheet; (e) homestead inclusion when rent/property-tax inputs are fed.

---

## 13. Known differences / encoding notes for the PUF

1. **Capital loss limit** ($500 pre-2023 vs federal $3,000; $3,000/$1,500 after): we observe the federal net loss capped at −3,000. Pre-2023, recap at −$500 (−$250 MFS? no — $500 per return) and ignore the divergent WI carryover stock (we can't track it). Post-2023 the only wedge is MFS $1,500 and WI-basis differences — ignore. Document as known difference (pre-2023 direction: WI taxable income higher than truth for loss-carryover holders, but our recap corrects the first-year effect).
2. **30/60% LTCG exclusion**: modelable from kg_lt (30%); farm-asset 60% unobservable → use 30% for all (known small understatement of the exclusion). Loss years: apply §5 limit.
3. **SS exemption**: full subtraction of taxable SS — modelable exactly (gross_ss/taxable SS).
4. **UI partial exclusion**: modelable exactly from ui and other income (§6.4 worksheet).
5. **Retirement**: military retirement is inside txbl_pens_dist and unobservable → options: ignore (overstates WI tax on veteran retirees) or impute a military-pension share; pre-1964 systems ignore. $5,000 65+/low-income exclusion modelable (age1/age2, FAGI test, IRA+pension base). 2025 $24k/48k subtraction: implement the credits-forfeiture election as min-liability branch (§6.5).
6. **Medical care insurance subtraction**: premiums not separately observed (some info in e-file medical expense?); ignoring it overstates WI income for self-employed/retiree purchasers and slightly overstates the itemized credit medical term (which should net it out) — partially offsetting. Document.
7. **Itemized deduction credit**: build from PUF Schedule A components — medical (apply WI floor by year: 10% in 2017/2019/2020, else 7.5%), mortgage+investment interest (can't identify out-of-state second homes/boats — accept), charitable (federal allowable), casualty (federally-declared only); minus WI SD; ×5%. Use itemizable amounts even for federal standard-deduction takers (pro-forma allowed) — this matters a lot post-TCJA when most filers stopped itemizing federally but the WI credit is still available.
8. **School property tax credit**: owners modelable via salt_prop (12% of first $2,500, $300/$150 caps); renters unobservable → understated credits unless a renter imputation is added (candidate: impute rent for non-itemizers by income/age from ACS, as with other states' renter credits). Homestead credit: exclude, document (PE divergence expected).
9. **Married couple credit**: modelable from wages1/2 + SE income splits; deferred-comp and adjustment-allocation subtleties are noise at the $480 cap (binding for most two-earner couples: cap binds once the lesser earner exceeds $16,000).
10. **WI EITC**: trivial multiplier of federal EITC by child count; use pre-ARPA federal EITC for 2021–2022; 0% childless.
11. **Conformity**: rolling group 0 with the four targeted adjustments in §1.3.
12. **Dependent-filer SD limitation**: encode with federal dependent-SD floor + $350.
13. **2020 UI**: compute WI on full UI (ARPA exclusion addback nets against the WI worksheet) — §1.3.

---

## 14. UNVERIFIED items summary

| # | Item | Status |
|---|---|---|
| 1 | Odd-year SSSD phase-out starts (2017/19/21/23/25), marked (t) in §3 | Derived from official DOR Standard Deduction Tables by exact linear fit (≤$3 residual, $10-rounded); method reproduces LFB values in all even years. High confidence, but not printed as formulas in a primary source for those years. |
| 2 | Act numbers: 2017 Act 231 (TCJA conformity), 2021 Act 157 §4 (capital-loss $3,000), 2021 Act 58 (50% CDCTC), 2023 Act 101 (expanded CDCTC), 2025 Act 15 (bracket-2 expansion + $24k retirement subtraction), 2023 Acts 35/36 (IRC 12/31/2022), vetoed 2023-24 retirement-exclusion bill numbers | Effects and effective years are form-verified; act attributions from secondary sources/search results. |
| 3 | WI-2441 one-qualifying-person expense cap $10,000 (2024+) | $20,000 two+ cap verified; $10,000 inferred. Read Schedule WI-2441 form itself at encoding. |
| 4 | Statute subsection cites 71.05(1)(ae) ($5,000 exclusion) and exact cites for military retirement subtraction | Provisions verified from forms; subsection numbers not independently checked. |
| 5 | Homestead credit internal parameters ($1,460 tax cap, 8.785% offset rate, $8,060 threshold) | From memory; $24,680 income ceiling verified (2017 & 2023 booklets). Out of scope anyway. |
| 6 | Dependent-SD worksheet floor for 2018–2024 (tracks federal $1,050→$1,350) | Endpoints verified (2017: $1,050+$350; 2025 threshold text implies $1,350). |
| 7 | Filing thresholds for 2018–2022, 2024 | Identity (SSSD max + $700/$1,400) verified in 2017/2023/2025; intermediate years follow arithmetically. |
| 8 | 529 subtraction caps for 2017, 2018, 2021 | Other years verified from booklets. |
| 9 | TAXSIM WI internals (2019 one-time rates, $500 loss limit, UI worksheet, medical floor, homestead) | Expectations only — verify at cross-model triage. |
| 10 | PolicyEngine handling of: 2019 rates (pre-2021 not in scope for PE anyway), 2023 loss limit, 2025 bracket expansion, 2025 credits-restricted subtraction, SS subtraction | Parameter directory names verified; values not read. |
| 11 | 2020 Schedule I medical-floor item (10% vs 7.5%) | Verified explicitly for 2019 (Sch I Part II item 5); 2020 inferred from identical IRC posture (same reference date, extension acts post-12/31/17). |

Key URLs: booklets `https://www.revenue.wi.gov/TaxForms2017through2019/2017-Form1-Inst.pdf` (pattern per year); Schedules `.../{year}-ScheduleWD-Inst.pdf`, `.../{year}-ScheduleI-Inst.pdf`, `.../{year}-ScheduleSB-Inst.pdf` (2020+); rate page `https://www.revenue.wi.gov/Pages/FAQS/pcs-taxrates.aspx` (+ web.archive.org captures per year); LFB IP-2 `https://docs.legis.wisconsin.gov/misc/lfb/informational_papers/january_{2019,2021,2023,2025}/0002_individual_income_tax_informational_paper_2.pdf`; WFTC statute `https://docs.legis.wisconsin.gov/statutes/statutes/71/i/07/5m`; LFB budget paper #325 (2025-27) for the rate-cut act history.
