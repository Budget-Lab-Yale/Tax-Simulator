# Maryland Individual Income Tax — Core Mechanics Research Packet (TY2017–TY2025)

**Prepared:** 2026-07-24, for Tax-Simulator state-tax module encoding. **STATE-level tax only** (Form 502 through line 21/state credits). The county/local income tax (Form 502 line 28) is deferred to the locality phase; its structure and — critically — its treatment in TAXSIM and PolicyEngine are documented in §11 and the cross-model section because they determine the comparison design.

**Method:** Year-specific Form 502 Resident Booklets for TY2017–TY2025 were downloaded as PDFs from the NBER historical archive (`taxsim.nber.org/historical_state_tax_forms/MD/{year}/Resident_Booklet.pdf` — these are byte-mirrors of the marylandtaxes.gov / marylandcomptroller.gov booklets) and text-extracted with PyMuPDF. Every dollar parameter below was transcribed from the year's own booklet unless flagged. Statute cites (Md. Code, Tax-General) are given where the booklet or PolicyEngine's parameter references identify the section; subsection-level cites not independently pulled from the code are flagged. Cross-model facts were established **empirically**: TAXSIM-35 via the local WASM engine (`usincometaxes` R package, same engine as the cross-model harness) and PolicyEngine US v1.775.7 (the harness's pinned venv at `/nfs/roberts/project/pi_nrs36/ji252/venvs/policyengine`) by reading its installed variable/parameter tree directly.

**Source inventory (all fetched and read):**

| TY | Booklet | Pages | Key sections read |
|---|---|---|---|
| 2017 | NBER `MD/2017/Resident_Booklet.pdf` | 60 | Instr. 10 (exemption chart 10A), 13 (subtractions, worksheets 13A/13D/13E), 14 (itemized), 16 (std ded), 17 (rate schedules I/II), 18 (EITC/poverty), 21 (refundable EIC 27%), 19 (local chart) |
| 2018 | NBER `MD/2018/Resident_Booklet.pdf` | 46 | Same + "New for 2018" (HB296, HB856, SB996) |
| 2019 | NBER `MD/2019/Resident_Booklet.pdf` | 42 | Same + "New for 2019" (local rate changes, nonresident rate 1.75→2.25 for 2020); worksheet 16A values |
| 2020 | NBER `MD/2020/Resident_Booklet.pdf` | 43 | Same — **original print, pre-RELIEF Act** (shows 28% refundable EIC, no UI subtraction) |
| 2021 | NBER `MD/2021/Resident_Booklet.pdf` | 45 | Same + RELIEF Act (SB496) / SB218 summary, UI subtraction code yy, EITC 18A/18A.1, CTC (502CR Part CC), exemption instr. 10 |
| 2022 | NBER `MD/2022/Resident_Booklet.pdf` | 45 | Same + Senior Tax Credit (SB405), public safety code v |
| 2023 | NBER `MD/2023/Resident-Booklet.pdf` | 53 | Same + Family Prosperity Act (45% EITC permanent, CTC $15k/under-6), military $12.5k/$20k |
| 2024 | NBER `MD/2024/Resident-Booklet.pdf` | 62 | Same + childless EITC cap removal |
| 2025 | NBER `MD/2025/resident-booklet.pdf` | 61 | "New for 2025" (OBBBA decoupling, BRFA HB352: new brackets, flat std ded, itemized phase-out 14A, 2% cap-gain surtax/502CG, CTC phase-out, AA/Frederick local brackets) |

Supplemental: Comptroller **RELIEF Act Tax Alert** (`marylandcomptroller.gov/.../alerts/RELIEF_Act_Tax_Alert.pdf`); Comptroller **Tax Alert 03-11-2021** (SB218 EITC/CTC); Md. Tax-Gen §10-751 statute text (mgaleg.maryland.gov).

---

## 1. Starting point, return structure, conformity

- **Form 502 line 1 = federal AGI** all years 2017–2025 (2021 booklet Instr. 11: "Copy the figure for federal adjusted gross income from line 11 of your federal Form 1040"). MD then applies its own additions (line 6), subtractions (lines 8–15, incl. Form 502SU), deduction (line 17), exemptions (line 19) → **Maryland taxable net income (line 20)** → state tax (line 21) → local tax (line 28, out of scope).
- **Filing statuses:** 1 Single, 2 MFJ, 3 MFS, 4 HoH, 5 Qualifying widow(er)/surviving spouse, 6 Dependent taxpayer. Follows the federal election except MFJ couples may file separate MD returns in limited cases (Instr. 7). **MFS is NOT half-MFJ**: MFS uses the same rate schedule and standard-deduction bounds as Single (Schedule I / "filing status 1, 3, 6" grouping — verified in every year's Instruction 16/17).
- **Conformity:** rolling IRC conformity, with an *automatic decoupling* mechanism for federal amendments with large in-year MD revenue impact (Tax-Gen §10-108; subsection detail UNVERIFIED from the code itself, but the mechanism is confirmed by the 2025 booklet: "Maryland is **automatically decoupled** from certain business provisions of the One Big Beautiful Bill Act (P.L. 119-21)… full expensing of domestic research and experimental expenditures, modification of limitation on business interest, and special depreciation allowance for qualified production property" — 2025 booklet, New for 2025). Long-standing decoupling from bonus depreciation/§179 enhancements via Form 500DM (booklet codes cd/dm/dp) — business-side, note-only.
- **TCJA (TY2018):** no individual-side decoupling of the AGI concept. MD's own deduction/exemption stack never depended on federal deductions/exemptions, so the TCJA suspension of federal personal exemptions did NOT flow through — MD kept its $3,200 exemptions. Only structural coupling: **you may itemize on the MD return only if you itemized federally** (§4 below), so the doubled federal standard deduction mechanically pushed MD filers to the (small) MD standard deduction. 2018 MD legislative responses visible in the booklets: HB296 (correctional officers added to public-safety pension exclusion), HB856 (childless EITC without federal minimum-age requirement), SB996 (military retirement 55+ threshold); the 2018 increase in std-deduction maxima to $2,250/$4,500 with CPI indexation from 2019 (Acts of 2018 — bill number UNVERIFIED, commonly cited as SB318; the per-year values below are from the booklets and are authoritative).
- **CARES/ARPA (TY2020):** MD conformed to the federal $10,200 UI exclusion (it reduces FAGI, the line-1 input). On top, the **RELIEF Act (SB496, Ch. 39, enacted 2021-02-15)** added a **full state subtraction for unemployment compensation** included in FAGI for **TY2020 and TY2021**, cliff-gated: FAGI < **$75,000** (single/MFS/dependent) or < **$100,000** (MFJ/HoH/surviving spouse); UI must be from MD DOL or a reciprocal jurisdiction (PA, VA, WV, DC). 502SU code **yy** (2021 booklet, Instr. 13; RELIEF Act Tax Alert). TY2020 application is retroactive — the original 2020 booklet does not show it. RELIEF also subtracted MD state economic-impact payments (TY2021) and Coronavirus relief grants/forgiven loans (code zz).
- **OBBBA (TY2025):** decoupled from the three business provisions above only; individual-side OBBBA provisions (e.g., the raised federal SALT cap, which raises the 17b cap in §4) flow through via rolling conformity. (2025 booklet, New for 2025.)

## 2. Rates and brackets (Tax-Gen §10-105(a))

**Unindexed and unchanged TY2017–TY2024** (verified: full schedule transcribed from the 2017 booklet; the top-bracket constant $12,760.00 and absence of any 6.25% rate confirmed in every booklet 2018–2024). **TY2025 (BRFA of 2025, HB352): two new top brackets + a capital-gains surtax.**

**Schedule I — Single, MFS, Dependent taxpayers** (also fiduciaries):

| Taxable net income over | 2017–2024 rate | 2025 rate |
|---|---|---|
| $0 – 1,000 | 2.00% | 2.00% |
| 1,000 – 2,000 | 3.00% | 3.00% |
| 2,000 – 3,000 | 4.00% | 4.00% |
| 3,000 – 100,000 | 4.75% | 4.75% |
| 100,000 – 125,000 | 5.00% | 5.00% |
| 125,000 – 150,000 | 5.25% | 5.25% |
| 150,000 – 250,000 | 5.50% | 5.50% |
| 250,000+ | 5.75% | 5.75% (250,000–500,000) |
| 500,000 – 1,000,000 | — | **6.25%** |
| 1,000,000+ | — | **6.50%** |

(2017 booklet: tax at 250,000 = $12,760.00; 2025 booklet: tax at 500,000 = $27,135.00, at 1,000,000 = $58,385.00.)

**Schedule II — MFJ, HoH, Qualifying widow(er)/surviving spouse:**

| Taxable net income over | 2017–2024 rate | 2025 rate |
|---|---|---|
| $0 – 1,000 | 2.00% | 2.00% |
| 1,000 – 2,000 | 3.00% | 3.00% |
| 2,000 – 3,000 | 4.00% | 4.00% |
| 3,000 – 150,000 | 4.75% | 4.75% |
| 150,000 – 175,000 | 5.00% | 5.00% |
| 175,000 – 225,000 | 5.25% | 5.25% |
| 225,000 – 300,000 | 5.50% | 5.50% |
| 300,000+ | 5.75% | 5.75% (300,000–600,000) |
| 600,000 – 1,200,000 | — | **6.25%** |
| 1,200,000+ | — | **6.50%** |

(2017 booklet: tax at 300,000 = $15,072.50; 2025: tax at 600,000 = $32,322.50, at 1,200,000 = $69,822.50.)

**TY2025 2% net capital gain surtax (Form 502CG; Form 502 lines 20a/21b):** filers with **FAGI > $350,000** pay an additional **2%** on certain net capital gain income ("Multiply the amount on Line 20a by .02" — 2025 booklet Instr. 16/17). Exceptions (per BRFA; e.g., certain retirement-account and primary-residence gains) are on Form 502CG — **UNVERIFIED detail** (502CG instructions not pulled; fetch `MD/2025/502cg.pdf` from NBER when encoding).

Tax tables (obligatory below $100,000 taxable) are straight applications of these schedules — encode the continuous schedules.

## 3. Standard deduction (Tax-Gen §10-217)

**TY2017–TY2024: 15% of Maryland AGI (line 16), bounded by a min and max** that depend on filing-status group; **TY2025: flat amount** (BRFA HB352). Group A = filing statuses 1, 3, 6 (Single, MFS, Dependent); Group B = statuses 2, 4, 5 (MFJ, HoH, QW/QSS). Values transcribed from each year's Instruction 16 (and Worksheet 16A):

| TY | Group A min–max | Group B min–max |
|---|---|---|
| 2017 | 1,500 – 2,000 | 3,000 – 4,000 |
| 2018 | 1,500 – 2,250 | 3,000 – 4,500 |
| 2019 | 1,500 – 2,250 | 3,050 – 4,550 |
| 2020 | 1,550 – 2,300 | 3,100 – 4,650 |
| 2021 | 1,550 – 2,350 | 3,100 – 4,700 |
| 2022 | 1,600 – 2,400 | 3,200 – 4,850 |
| 2023 | 1,700 – 2,550 | 3,450 – 5,150 |
| 2024 | 1,800 – 2,700 | 3,650 – 5,450 |
| 2025 | **flat 3,350** | **flat 6,700** |

Notes: (i) the 2018 act raised the maxima and indexed **both bounds** to CPI from TY2019, $50 rounding — hence the uneven drift (2019 single min stayed $1,500 while joint min moved to $3,050; both transcribed from the 2019 booklet text AND worksheet 16A, so not a typo). (ii) HoH gets the JOINT-level bounds — unusual; TAXSIM implements this too. (iii) 2025 flat amounts from the 2025 booklet Instruction 16 ("If your filing status is single, dependent, or married filing separately, your standard deduction amount is $3,350… married filing jointly, head of household, or qualifying surviving spouse… $6,700").

## 4. Itemized deductions (Form 502 lines 17a/17b, 2025 +17c)

- **Coupled election:** "You may itemize your deductions only if you itemized deductions on your federal return" (every year, Instr. 16). You are NOT required to itemize on the MD return if you itemized federally — a federal itemizer takes max(MD standard, MD itemized).
- **Computation:** line 17a = federal Schedule A total itemized deductions (Sch. A line 29 pre-TCJA, line 17 after); line 17b = **state and local income taxes used as a deduction for federal purposes**, plus any amounts deducted as contributions of Preservation/Conservation Easements for which the MD credit (502CR Part F) is claimed. MD itemized = 17a − 17b. (Instr. 14, all years.)
- **SALT-cap era (2018–2024):** Comptroller FAQ (printed in the booklets, e.g. 2021 p. iii and 2025 p. iii): "Due to the State and local tax limitations (SALT), the state and local tax Line 17b of Form 502 is **capped at $10,000 or $5,000 if married filing separately** plus [the easement amount]." I.e., 17b = min(SALT income-tax component deducted, federal cap). **2025:** same FAQ prints the cap as **$40,000/$20,000 MFS** — tracking the OBBBA federal cap via conformity. **Allocation ordering under a binding cap** (income-first vs pro-rata when income + property taxes exceed the cap) is NOT specified in the booklet beyond the cap sentence — **UNVERIFIED**; practical encoding: 17b = min(state+local income tax deducted-eligible, capped Sch. A SALT amount, $10k/$5k [2018–2024] or $40k/$20k [2025]). Pre-2018: no cap; 17b = full income taxes deducted.
- **TY2025 phase-out (line 17c, Worksheet 14A, BRFA HB352):** if FAGI > **$200,000** ($100,000 MFS), itemized deductions are reduced by **7.5% × (FAGI − threshold)**. (2025 booklet Instr. 14 + Worksheet 14A; the reduction plausibly floors at zero via "do not enter less than 0" on the form — floor mechanics UNVERIFIED beyond worksheet arithmetic.)
- Circularity note for encoding: 17b subtracts the state/local income-tax component back out, so MD itemized is *not* a function of current-year MD liability (no fixed-point iteration needed) — but the *federal* Sch. A amount feeding 17a is capped SALT-inclusive.

## 5. Personal exemptions (Tax-Gen §10-211, §10-212)

**$3,200 per taxpayer, spouse, and dependent**, phased DOWN in cliff bands by FAGI. **Chart identical TY2017–TY2025** (transcribed from Exemption Amount Chart 10A in 2017, 2021, and 2025 booklets; interior years spot-checked):

| FAGI | Single or MFS: each exemption | MFJ/HoH/QW(QSS): each exemption | Dependent taxpayer (status 6) |
|---|---|---|---|
| ≤ $100,000 | $3,200 | $3,200 | $0 |
| $100,000 – 125,000 | $1,600 | $3,200 | $0 |
| $125,000 – 150,000 | $800 | $3,200 | $0 |
| $150,000 – 175,000 | $0 | $1,600 | $0 |
| $175,000 – 200,000 | $0 | $800 | $0 |
| > $200,000 | $0 | $0 | $0 |

- **Age-65+/blind add-on: $1,000 each** for taxpayer and spouse (not dependents), **NOT subject to the phase-down** ("it does not apply to the taxpayer's age or blindness exemption of $1,000" — chart 10A header, all years).
- **Dependent 65+: one EXTRA exemption of up to $3,200** (i.e., a 65+ dependent counts twice at the phased $3,200 rate, via Form 502B boxes 4 and 5): "If any other dependent claimed is 65 or over, you also receive an extra exemption of up to $3,200" (2021 booklet Instr. 10).
- A dependent-taxpayer (status 6) gets no personal exemption for self.
- Interaction with U.S.-obligation interest: the exemption is recomputed excluding US-obligation income, delta claimed as subtraction code hh (Worksheet 13C) — note-only.

## 6. Pension exclusion (Tax-Gen §10-209; Form 502 line 10a, Worksheet 13A)

- **Eligibility (per spouse):** age **65+** OR **totally and permanently disabled** OR spouse totally disabled, on the last day of the year, AND taxable income from an **"employee retirement system" qualified under IRC §401(a), §403, or §457(b)**. **Explicitly NOT qualifying: traditional IRA, Roth IRA, SEP, Keogh, ineligible deferred comp plans, foreign retirement income** (2021 booklet Instr. 13 line 10a — bracketed list; same text all years). Distribution amounts claimed under the military (u) or public-safety (v) subtractions are excluded from worksheet line 1.
- **Mechanics (Worksheet 13A, per person):** exclusion = min( qualifying taxable pension/annuity in FAGI , **cap − total Social Security/Railroad Retirement benefits received** (Tier I + Tier II + supplemental, **whether or not taxable federally** — i.e., GROSS SS) ), floored at 0. On a joint return where both get SS but only one has a pension, only the pensioner's SS counts against the cap. Each qualifying spouse computes a separate column; combined total to line 10a.
- **Cap per year** (= "maximum annual benefit under the Social Security Act", indexed; from each year's Worksheet 13A Line-2 instruction):

| TY | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 |
|---|---|---|---|---|---|---|---|---|---|
| Cap | $29,900 | $30,600 | $31,100 | $33,100 | $34,300 | $34,300 | $36,200 | $39,500 | $41,200 |

- **Public-safety variant (Worksheet 13E; 502SU code v — "Hometown Heroes", expanded to correctional officers by HB296 of 2018):** retired correctional / law-enforcement / fire / rescue / EMS personnel aged **55+** (and under 65/not disabled — otherwise use 13A) may exclude up to **$15,000** (statutory, unindexed, all years since 2017 era) of employee-retirement-system income attributable to that service, with the same SS-offset structure (13E line 2 = the 13A cap). One spouse may use 13A and the other 13E. **Source-limited for us** (occupation of pension not in PUF) — encode as not-modeled, document.
- **Military retirement subtraction (502SU code u)** — separate from the pension exclusion, no SS offset:

| TY | Under threshold age | At/over threshold |
|---|---|---|
| 2017 | $5,000 (<65) | $10,000 (65+) |
| 2018–2022 | $5,000 (<55) | $15,000 (55+) — SB996 (2018) |
| 2023–2025 | $12,500 (<55) | $20,000 (55+) |

  (2017/2018/2019/2021/2022 vs 2023/2024/2025 booklets, Instr. 13 code u.) Source-limited (military pension not identified in PUF) — document.

## 7. Social Security: fully exempt

**Verified.** Form 502 line 11 subtracts the **entire federally taxed amount** of Social Security and Railroad Retirement (Tier I, Tier II, supplemental) benefits included in FAGI: "Social Security and railroad retirement benefits are exempt from state tax" (2021 booklet, Instr. 13 Line 11; same all years). No income limit, no phase-out. (The gross benefit separately offsets the pension-exclusion cap, §6.)

## 8. Two-income married couple subtraction (Form 502 line 14, Worksheet 13D)

Up to **$1,200** for MFJ returns where **both spouses have income subject to MD tax**. All years 2017–2025, amount unchanged. Mechanics (Worksheet 13D, transcribed 2021; identical structure 2017 and 2025): for each spouse compute (share of FAGI) + (share of line-6 additions) − (share of lines 8–13 subtractions attributable to that spouse); the subtraction = min( **$1,200** , the SMALLER spouse's result, floored at 0 ). Statute: Tax-Gen §10-207 (subsection UNVERIFIED). Note: it is the lesser earner's *modified income*, not just wages. TAXSIM implements it (probe: MFJ 30k/20k wages 2019 → state AGI 48,800).

## 9. Earned income credits (Tax-Gen §10-704)

Three pieces: a nonrefundable state EIC (line 22), a refundable state EIC (line 42; line 44 in 2025), and the poverty-level credit (line 23, §12). A local EITC exists against the county tax (out of scope).

**(a) Nonrefundable (line 22):** = **50% of the federal EIC**, all years 2017–2025, for (2021+) married couples and filers with at least one qualifying child; before 2021, all eligible filers. Offsets state tax; remainder does not refund (the refundable piece below handles that).

**(b) Refundable (line 42):** for filers whose 50% credit ≥ pre-credit tax (worksheet 18A line 3 = 0): refundable credit = **p × federal EIC − MD state tax**:

| TY | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 |
|---|---|---|---|---|---|---|---|---|---|
| p | 27% | 28% | 28% | **45%*** | 45% | 45% | 45% | 45% | 45% |

\* RELIEF Act (SB496, Ch. 39 of 2021) set 45% for **TY2020–2022 retroactively**; the original-print 2020 booklet shows 28% (pre-enactment). The **Family Prosperity Act of 2023 (SB552)** made 45% permanent from TY2023 (2023 booklet, What's New). TAXSIM applies 45% for TY2020 (post-RELIEF law) — probe confirmed.

**(c) Childless single filers (Worksheet 18A.1), TY2021+:** single/HoH/QW(QSS) filers **without a qualifying child** instead claim **100% of the federal EIC**, capped at:

| TY | 2021 | 2022 | 2023 | 2024 | 2025 |
|---|---|---|---|---|---|
| Cap | $530 | $530 | $600 | none (100%) | none (100%) |

Fully refundable: the excess of the (capped) credit over MD state tax goes to line 42 (18A.1 lines 3–4). Originated in SB218 (2021) at $530 for TY2020?–2022 — the **TY2020 applicability of the childless-100% piece is UNVERIFIED** (the RELIEF-era alert describes it as TY2020–2022 with the $530 max, but the 18A.1 worksheet first appears in the 2021 booklet); 2023's $600 equals that year's federal childless maximum; 2024–2025 booklets drop the dollar cap entirely.

**(d) Eligibility expansions:** federal minimum-age requirement for childless filers disregarded from **TY2018** (HB856 of 2018); **ITIN filers** allowed from **TY2020** (SB218 of 2021, TY2020–2022; permanent thereafter — 2021–2025 booklets all carry the ITIN text). MFJ filing separate MD returns: combined claims capped at 50% of federal.

## 10. Maryland Child Tax Credit (Tax-Gen §10-751; Form 502CR Part CC line 8, refundable via Worksheet 21C)

**Refundable**, residents only, per qualified child, no limit on number of children:

| Period | Amount | Qualified child | FAGI limit | Notes |
|---|---|---|---|---|
| TY2020(?)–TY2022 | $500 | dependent **under 17 WITH a disability** (defined via Educ. Art. §8-401 assessment list) | ≤ **$6,000** (cliff) | Reduced by the **federal CTC claimed** (1040 line 19) — 2021/2022 booklets. First booklet appearance TY2021; enacted with SB218 (Ch. 40 of 2021, per Comptroller Tax Alert 03-11-2021). **TY2020 applicability UNVERIFIED** (not in the original 2020 booklet). |
| TY2023–TY2024 | $500 | dependent **under 6**, OR dependent over 5 & under 17 **with a disability** | ≤ **$15,000** (cliff) | Family Prosperity Act (SB552, 2023); made permanent. The federal-CTC offset sentence is gone from the 2023+ booklets (offset repeal UNVERIFIED against statute). |
| TY2025 | $500, **phased** | same as 2023 | threshold **$15,000**, credit reduced **$50 per $1,000 (or fraction, rounded up)** of FAGI above it → $0 at FAGI ≥ **$24,001** | BRFA HB352; Worksheet 21C (2025 booklet). |

Requires disability assessment documentation for the disabled-child category. For our data: the under-6 rule (2023+) is modelable; the disabled-child rule is not (and is negligible at a $6,000 cliff).

## 11. County ("local") income tax — structure only (locality phase deferred)

- Every county + Baltimore City levies an income tax on **MD taxable net income (line 20)** — same base as the state tax (Form 502 line 28; booklet Instr. 19). Resident rate set by county of residence on the last day of the year.
- **Flat county rates through 2024** (with county-by-county changes over time). Range: TY2017–2019 **1.75% (Worcester) – 3.20%**; TY2020–2024 **2.25% – 3.20%** (2019 booklet What's-New enumerates the 2020 increases: Worcester 1.75→2.25, Washington 2.8→3.2, Baltimore Co. 2.83→3.2, Dorchester 2.62→3.2, Kent 2.85→3.2, St. Mary's 3.0→3.17, Anne Arundel 2.5→2.81). TY2025 range **2.25% – 3.30%** (Dorchester 3.30).
- **Bracketed county schedules (new authority):** Anne Arundel adopted a bracketed schedule (from TY2021: 2.7%/2.81% two-bracket per PE params; TY2025: **marginal** 2.70% ≤$50k / 2.94% $50–400k / 3.20% >$400k single, joint breakpoints $75k/$480k). Frederick (TY2025): **fixed-rate-by-bracket** (whole income at one rate) 2.25/2.75/2.96/3.20% with single breakpoints $25k/$50k/$150k (joint $25k/$100k/$250k). (2025 booklet, "New Local Tax Brackets for 2025".)
- **Special nonresident rate** (Form 505, in lieu of a county tax): **1.75%** TY2017–2019, **2.25%** TY2020+ (2019 booklet What's-New: "The special nonresident income tax rate has increased from 1.75% in 2019 to 2.25% in 2020"; equals the lowest county rate).
- **Local credits** (against county tax only, out of scope but needed later): local EITC = federal EIC × 10 × county rate, capped at county tax (Tax-Gen §10-704(d); PE implementation); local poverty-level credit = earned income × county rate for §12-eligible filers (§10-709(d)); Anne Arundel's local tax uses its lowest bracket rate for the local-EITC computation (§10-704(d)(2)).

## 12. Other items

- **State poverty level credit (Tax-Gen §10-709; line 23, Worksheet 18B):** **nonrefundable** credit = **5% of earned income** (wages + net self-employment profit, no losses) if max(FAGI + MD additions, earned income) < the federal poverty guideline for the household size on the federal return. Not available to dependent filers (status 6). Practically relevant only when MD tax exceeds 50% of federal EIC (18A line 3 > 0) — booklet text, all years. Poverty guideline table printed per year in Worksheet 18B (year-specific HHS guidelines; encode from federal poverty guidelines for the relevant year).
- **Senior tax credit (SB405, Acts of 2022; TY2022+; Form 502CR — nonrefundable):** residents **age 65+**: **$1,000** if single/MFS(*) FAGI ≤ **$100,000**; **$1,750** for MFJ/QSS/HoH FAGI ≤ **$150,000** (reduced to **$1,000** if only one joint spouse is 65+). Cliffs, unindexed, unchanged TY2022–2025 (booklets, Instr. 18 item m). PolicyEngine lists `md_senior_tax_credit` under **non-refundable** credits from 2022 — consistent with the booklet placement (Line 24 / 502CR nonrefundable part). Statute section (10-754?) UNVERIFIED. (*) The $1,000/$100k tier's booklet text says "the taxpayer's" without listing statuses; single & MFS assumed — MFS inclusion UNVERIFIED.
- **Filing requirement:** must file if required to file federally and gross income (FAGI + MD additions) ≥ the minimum filing level — which equals the **federal gross-income filing thresholds** (federal std deduction incl. age add-ons; e.g., TY2025 Table 1: single $15,750, MFJ $31,500, HoH $23,625; Table 2 for 65+). Dependent taxpayers: threshold = single amount. Booklet Instruction 1, per year. (Encoding: harmless to compute tax directly; the threshold matters only for non-filer imputation.)
- **Century-club subtraction (code ya, TY2024+ (first seen in 2025 booklet — start year UNVERIFIED)):** residents aged 100+ subtract up to $100,000 of income. Note-only.
- **Quality Teacher Incentive / Volunteer Fire ($4,500–$7,000, code va) / first-time homebuyer savings / 529 / ABLE etc.:** note-only, not modeled.
- **Additions** (line 6/502SU codes) — main mass-relevant one: **non-MD state/municipal bond interest**; also the 502TP tax-preference addback and pass-through entity member addback (code r, 2021+, mirrors the PTE SALT-workaround credit). Note-only for encoding priorities.

## 13. Recommended encoding order (state-only)

FAGI → + additions (skip; small) → − subtractions: SS/RR (line 11), two-income (line 14, $1,200), pension exclusion 13A (per-person, SS-offset, IRA-excluded), UI subtraction (TY2020–2021, cliff), military (u; if military pension identifiable — else document) → MD AGI (line 16) → deduction: max(std [15% bounded / 2025 flat], if federal itemizer: fed itemized − 17b [− 17c in 2025]) → − exemptions ($3,200 banded + $1,000 aged/blind + extra $3,200 for 65+ dependents) → taxable net income → Schedule I/II rates (+2025 502CG 2% surtax if FAGI > $350k) → − nonrefundable: EIC 50%, poverty 5%, senior credit (2022+), CDCC (not researched here — note) → − refundable: EIC (p×fed − tax), childless 100% piece (2021+), CTC.

---

## Known differences / PUF-observability notes

1. **Pension vs IRA split IS observable** in the PUF (`txbl_pens_dist` vs `txbl_ira_dist`) — MD's IRA exclusion from the pension exclusion is directly modelable. Gross SS (`gross_ss`) available for the offset. The per-person (not per-couple) computation needs a primary/secondary pension split — same limitation as MI/MN; use the elderly-survey split machinery.
2. **Disability-based eligibility not observable:** (a) pension exclusion for under-65 totally-disabled filers → undercount (small); (b) CTC 2021–2022 (disabled child, FAGI ≤ $6k) → effectively zero revenue either way; (c) disabled-child category of 2023+ CTC → undercount; under-6 category is modelable.
3. **Occupation-specific subtractions not modeled:** military retirement (code u), public-safety $15k (code v/13E). Both reduce line-1 pension amounts for a minority of retirees → we will overstate MD tax for those units; direction is one-sided. Document in known_differences.
4. **Two-income subtraction** needs each spouse's *modified income*; approximate the split with earnings + pro-rata capital income, cap binds at $1,200 for nearly all two-earner couples (income of lesser earner > $1,200), so error is confined to very-low-secondary-earner couples.
5. **Itemized 17b under a binding SALT cap:** allocation ordering unspecified (§4) — choose income-first (equivalently 17b = min(income taxes, 10k)) and document; affects itemizers with property tax + income tax > $10k, 2018–2024.
6. **Local tax entirely excluded** from `liab_st_iit` (matches Form 502 line 21 vs line 28 split). Local EITC/poverty credits do NOT touch the state lines.
7. RELIEF Act UI subtraction is a **cliff** — expect MTR spikes at $75k/$100k FAGI in 2020–2021.

## Cross-model comparison design (CRITICAL: county-tax treatment)

**TAXSIM-35 (`siitax`, WASM engine used by the harness): MD state tax EXCLUDES any county tax.** Established empirically, not from docs: single, $50,000 wages, TY2017 → `siitax` = **2,075.50** = exactly 90 + 4.75% × (50,000 − 2,000 std − 3,200 exemption − 3,000), i.e. the pure Schedule I state formula; `srate` = 4.75 (no local component). Any county piggyback (≥1.75%) would add ≥$780. No local-tax output variable exists in the 45-column full output. → **Compare our `liab_st_iit` to TAXSIM `siitax` directly (2017–2020 window), no adjustment needed.**

TAXSIM MD implementation notes from the same probes (for triage expectations):
- **Standard deduction bugs:** TY2019 uses **1,550/3,100** (≈2020 minimums) instead of the correct maxima 2,250/4,550 → TAXSIM overstates 2019 tax by up to ~4.75% × 1,450 ≈ $33 (single) / $69 (joint) for mid-income filers. TY2020 uses the 2019 maxima (2,250/4,550) instead of 2,300/4,650 (≤ $5 effect). 2017, 2018, 2021 correct; 2022–2023 stale at 2,350/4,700 (but ≥2021 is outside the TAXSIM window anyway).
- Two-income subtraction: applied ✓. Poverty level credit: applied ✓ (probe: HoH $8k earned → $400 = 5%). Refundable EIC: 28% in 2019 ✓, **45% in 2020** ✓ (post-RELIEF law — good, matches what we will encode). HoH gets joint-type std ded bounds ✓.
- **Pension exclusion over-generous:** TAXSIM ignores the SS offset — probe (2019 MFJ, both 70, $30k pensions, $20k gross SS) returned state AGI ≈ 0, i.e., full $30k excluded; correct law caps the exclusion at 31,100 − 20,000 = $11,100. Expect TAXSIM to *understate* MD tax for SS-receiving pensioners; classify as external-model issue (single-probe evidence — corroborate during triage).

**PolicyEngine US (v1.775.7, the harness venv): `state_income_tax` INCLUDES the MD county tax.** From the installed tree: `parameters/gov/states/household/state_income_tax_before_refundable_credits.yaml` lists BOTH `md_income_tax_before_refundable_credits` AND **`md_local_income_tax_before_refundable_credits`** (the county tax net of local EITC + local poverty-line credit). County resolution: household `county`/`county_fips` input; **if unset (as in our harness's `pe_state_tax.py`, which passes no county), PE falls back to `first_county_in_state` = ALLEGANY (3.05% 2021–22, 3.03% 2023+)** — i.e., PE would silently add ~3% of taxable income to every MD record. County parameters exist from **2021-01-01 only** (consistent with our PE window 2021–2024); Anne Arundel and Frederick are modeled with their bracketed schedules. Additional wrinkle: `md_refundable_credits` includes `md_montgomery_eitc` (a Montgomery County local supplement) — zero for non-Montgomery counties, so harmless under the Allegany default, but it means even `md_income_tax` is not perfectly local-free for Montgomery residents.

**Recommended design for the MD cross-model cell:**
1. **TAXSIM leg (2017–2020):** compare `liab_st_iit` vs `siitax` as-is. Pre-register the TY2019 (and small TY2020) standard-deduction divergence and the pension/SS-offset issue in `known_differences.csv` / the external-model issues doc.
2. **PE leg (2021–2024): do NOT compare against `state_income_tax`.** Request the MD-specific variable **`md_income_tax`** (= state-only: before-refundable minus `md_refundable_credits`) in `pe_state_tax.py` for MD records, or equivalently request `md_local_income_tax_before_refundable_credits` alongside `state_income_tax` and subtract. The first option is cleaner; keep an eye on `md_montgomery_eitc` (irrelevant under the default county).
3. Do not "fix" the comparison by assigning counties — locality is deferred, and the state-only concept is exactly Form 502 line 21 ± state credits, which `md_income_tax` matches.
4. When the locality phase arrives: MD is the flagship piggyback state (same base, county rate matrix per year, two bracketed counties from 2021/2025, local EITC/poverty credits, special nonresident 2.25%) — PE's `gov/local/md/` parameter tree is a ready-made rate source for 2021+, but pre-2021 rates must come from the booklets' Instruction-19 charts (2017–2020 charts read; transcribe when needed).

## UNVERIFIED summary

1. **2018 std-deduction bill number** ("SB318 of 2018") — values themselves are booklet-verified; only the session-law attribution is unconfirmed.
2. **Tax-Gen subsection cites**: §10-108 automatic-decoupling mechanics; two-income subtraction subsection within §10-207; senior-credit section number (10-754?); SS subtraction subsection within §10-207. Booklet mechanics are verified; statute mapping is not.
3. **SALT 17b allocation ordering** under a binding cap (income-first vs pro-rata), 2018–2024 — booklet/FAQ states only the cap.
4. **2025 Form 502CG exceptions** to the 2% net-capital-gain surtax (which gains are exempt) — 502CG instructions not pulled.
5. **MD CTC TY2020 applicability** (enacted by SB218 in Feb 2021; absent from the original 2020 booklet; whether TY2020 returns could claim it retroactively) — and whether the federal-CTC offset was formally repealed for 2023+ (the booklet language simply disappears).
6. **Childless-EITC (100%/$530) TY2020 applicability** — alert says TY2020–2022, worksheet first appears TY2021.
7. **Senior tax credit**: MFS inclusion in the $1,000/$100k tier; statute section.
8. **2025 itemized phase-out floor** (whether 17c reduction can exceed 17a−17b; assumed floored at 0).
9. **Century-club subtraction (code ya) start year** (2024 vs 2025).
10. **TAXSIM pension/SS-offset issue** rests on a single probe — corroborate with more records during MD triage before filing it in the external-issues doc.
