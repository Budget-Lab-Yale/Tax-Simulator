# Connecticut Personal Income Tax: Income Modifications and Credits, TY2017-2025

Research for Budget Lab Tax-Simulator `state-tax` module. Primary sources: DRS Form CT-1040
instruction booklets for tax years 2017-2025 (all nine booklets downloaded from portal.ct.gov and
text-extracted; exact worksheet language quoted below), cross-checked against CT OLR reports and
CGS § 12-701(a)(20). Where a value comes only from an OLR summary rather than an opened booklet,
that is flagged.

Structural note for the simulator: CT starts from **federal AGI** (Form CT-1040 Line 1), applies
Schedule 1 additions (Lines 31-38) and subtractions (Lines 39-50) to get **CT AGI**, subtracts a
personal exemption to get CT taxable income, applies rates, then applies credits (personal credit,
property tax credit, CT EITC, etc.). All modifications below are Schedule 1 items unless noted.

---

## 1. Social Security Benefit Deduction (Schedule 1, Line 41)

**Mechanics (identical structure all years 2017-2025).** Below the AGI threshold, 100% of
federally taxable Social Security is subtracted. At or above the threshold, the Social Security
Benefit Adjustment Worksheet applies. Exact worksheet lines (2025 booklet, p. 24; same A-F
structure verified in 2017, 2021, and 2025 booklets):

> A. Enter the amount reported on your 2025 federal Social Security Benefits Worksheet, Line 1. [total SS benefits]
> B. Enter the amount reported on your 2025 federal Social Security Benefits Worksheet, Line 9. However, if filing separately and you lived with your spouse at any time during 2025, enter the amount reported on Line 7 of your federal Social Security Benefits Worksheet. [excess of provisional income over federal base amount]
> C. Enter the lesser of Line A or Line B.
> D. Multiply Line C by 25% (.25).
> E. Taxable amount of Social Security benefits reported on your 2025 federal Social Security Benefits Worksheet, Line 18. [federally taxable SS]
> F. Social Security Benefit Adjustment - Subtract Line D from Line E. Enter the amount here and on Form CT-1040, Line 41. If Line D is greater than or equal to Line E, enter "0."

So for above-threshold filers, the **subtraction = max(0, federally taxable SS − 0.25 × min(total
benefits, excess over federal base amount))**; equivalently, CT taxes at most 25% of benefits
received (and at most 25% of the federal "excess over base amount"), i.e. at least 75% of the
federally taxable amount is effectively deducted — usually more, since federally taxable SS can be
up to 85% of benefits while CT caps taxable SS at 25% of benefits. Statute: CGS
§ 12-701(a)(20)(B)(x)(III)-(IV).

**It is a cliff, not a gradual phase-out**: crossing the threshold by $1 switches the filer from
100% subtraction to the worksheet.

**AGI thresholds (federal AGI, Form CT-1040 Line 1) — these DID change in the window:**

| Tax year | Single / MFS | MFJ / QW / **HoH** | Source |
|---|---|---|---|
| 2017 | $50,000 | $60,000 | 2017 booklet worksheet (opened) |
| 2018 | $50,000 | $60,000 | 2018 booklet worksheet (opened) |
| 2019 | $75,000 | $100,000 | 2019 booklet worksheet (opened) |
| 2020-2025 | $75,000 | $100,000 | 2020, 2021, 2025 booklets (opened); 2022-2024 confirmed via booklet Line 41 text and OLR 2023-R-0129 / 2024-R-0130 |

The increase was enacted by the 2017 budget act (PA 17-2, June Special Session) effective TY2019
(OLR 2019-R-0098: "The new thresholds take effect in 2019, rising from (1) $50,000 to $75,000 …
and (2) $60,000 to $100,000").

**Watch out:** for Social Security, **head of household uses the higher ($100,000) threshold**,
grouped with MFJ. This differs from the pension/annuity/IRA deduction, where HoH is grouped with
single at $75,000.

---

## 2. Pension and Annuity Income Deduction (Schedule 1, Line 48b)

**Qualifying income:** taxable pensions and annuities per federal 1040 Line 5b (defined benefit,
401(k), 403(b), 457(b)), **minus** military retirement pay, Tier 1/Tier 2 railroad retirement, and
CT Teachers' Retirement System pay (those have their own 100%/100%/50% subtractions elsewhere).
IRA distributions were **explicitly excluded** through 2022 (2022 booklet: "Distributions from
traditional IRAs, Roth IRAs, SEP … IRAs" are not included); from 2023 IRAs join Line 48b at their
own percentage (see § 3).

**Deduction percentage by year (all from opened booklets):**

| Tax year | % of qualifying pension/annuity income deductible | Eligibility |
|---|---|---|
| 2017-2018 | 0% (no deduction existed) | — |
| 2019 | 14% | AGI cliff |
| 2020 | 28% | AGI cliff |
| 2021 | 42% | AGI cliff |
| 2022 | 100% | AGI cliff |
| 2023 | 100% | AGI cliff |
| 2024 | 100% × phase-out factor | gradual phase-out |
| 2025 | 100% × phase-out factor | gradual phase-out |

Legislative history: PA 17-2 JSS enacted a 7-year phase-in — 14% (2019), 28% (2020), 42% (2021),
56% (2022), 70% (2023), 84% (2024), 100% (2025) (schedule transcribed in OLR 2019-R-0098). The
FY22-23 budget act, **PA 21-2 JSS**, accelerated it to **100% beginning TY2022** (confirmed: 2022
booklet Line 48b is titled "100% of Pension or Annuity Income"). So 56%/70%/84% never took effect.

**Eligibility cliff, 2019-2023:** federal AGI **less than $75,000 (single, MFS, and HoH)** or
**less than $100,000 (MFJ)**. At or above: zero deduction. (2022 booklet: "If your filing status
is single, married filing separately, or head of household with federal AGI … of less than $75,000
or married filing jointly with federal AGI of less than $100,000 … you qualify.")

**2024+ gradual phase-out (PA 23-204 § 377, CGS § 12-701(a)(20)(B)(xxi)-(xxii)).** The cliff was
replaced with the Pension and Annuity Worksheet: Line 2 = qualifying income (× the IRA percentage
if IRA), Line 3 = decimal from the Phase-Out Table, Line 4 = Line 2 × Line 3 → Line 48b.
Transcribed exactly from the 2024 booklet p. 28 (identical in the 2025 booklet pp. 24-25):

| Federal AGI, Single/MFS/HoH | Federal AGI, MFJ | Deduction factor |
|---|---|---|
| $0 - $74,999 | $0 - $99,999 | 1.00 |
| $75,000 - $77,499 | $100,000 - $104,999 | 0.85 |
| $77,500 - $79,999 | $105,000 - $109,999 | 0.70 |
| $80,000 - $82,499 | $110,000 - $114,999 | 0.55 |
| $82,500 - $84,999 | $115,000 - $119,999 | 0.40 |
| $85,000 - $87,499 | $120,000 - $124,999 | 0.25 |
| $87,500 - $89,999 | $125,000 - $129,999 | 0.10 |
| $90,000 - $94,999 | $130,000 - $139,999 | 0.05 |
| $95,000 - $99,999 | $140,000 - $149,999 | 0.025 |
| $100,000 and up | $150,000 and up | 0.00 |

(Booklet table prints the brackets as "Greater Than or Equal To / Less Than or Equal To" with
decimals 1, .85, .70, .55, .40, .25, .10, .05, .025, 0 — a step function on AGI, not linear
interpolation.)

---

## 3. IRA Distribution Deduction (part of Schedule 1, Line 48b since 2023)

Applies to distributions from IRAs **other than Roth IRAs** (federal 1040 Line 4b). Enacted by the
FY22-23 budget act (PA 21-2 JSS); statute now CGS § 12-701(a)(20)(B)(xxviii)-(xxix)/(xxvii)-(xxx)
as renumbered.

| Tax year | % of taxable IRA distributions deductible | Source |
|---|---|---|
| 2017-2022 | 0% (fully taxable; excluded from pension deduction) | booklets opened |
| 2023 | 25% (with the same $75k/$100k **cliff** as pensions) | 2023 booklet: "If the distribution is from an IRA (other than a Roth IRA), enter 25%" |
| 2024 | 50% × phase-out factor | 2024 booklet worksheet: "Enter 50% of the amount of IRA…" |
| 2025 | 75% × phase-out factor | 2025 booklet worksheet: "enter as a subtraction modification 75% of the amount…" |
| 2026+ | 100% (scheduled) | OLR 2025-R-0152; CGS § 12-701(a)(20)(B)(xxix) |

**The 2024+ gradual phase-out applies to IRAs too, and it is multiplicative with the phase-in
percentage**: deduction = (phase-in %) × (phase-out factor from the § 2 table) × taxable IRA
distributions. OLR worked examples: single filer, $80,000 AGI, $50,000 IRA income → 2024 deduction
= 50% × $50,000 × 0.55 = **$13,750**; 2025 deduction = 75% × $50,000 × 0.55 = **$20,625**
(OLR 2023-R-0129; 2025-R-0152).

**Discrepancy note:** OLR 2025-R-0152 (Sept. 2025) contains a parenthetical saying the deduction
"was previously 25% for the 2024 tax year" — this is an OLR drafting error. The 2024 CT-1040
booklet (primary) and OLR 2024-R-0130 both say **50% for 2024**; 25% was the 2023 value.

---

## 4. Connecticut EITC (Form CT-1040 Line 20a; Schedule CT-EITC)

Percentage of the **federal EITC claimed and allowed** for the same taxable year. All rates below
read directly from the year's booklet/Schedule CT-EITC instructions ("For the tax year 20XX, the
CT EITC is X% of the federal earned income credit"):

| Tax year | CT EITC rate | Enacting law |
|---|---|---|
| 2017 | 23% | (rate set by PA 17-2 JSS) |
| 2018 | 23% | |
| 2019 | 23% | |
| 2020 | 23% statutory — see one-time enhancement below | |
| 2021 | 30.5% | PA 21-2 JSS § 430 (booklet What's New: "increased to 30.5%") |
| 2022 | 30.5% | |
| 2023 | 40% | PA 23-204 § 378 |
| 2024 | 40% | |
| 2025 | 40% **+ flat $250 if the filer has at least one federal qualifying child** | PA 25-168 § 371, applicable TY2025+ |

2025 booklet (Line 20a): "The amount of the CT EITC is 40% of the earned income credit claimed and
allowed on the federal income tax return … Any taxpayer eligible for CT EITC that has at least one
qualifying child for federal income tax purposes for the same taxable year, is eligible to receive
an additional two hundred fifty dollars ($250)." Schedule CT-EITC Line 15a: "If you list a
qualifying child on Line 5, enter $250."

**One-time retroactive enhancement (not a statutory rate change):** in December 2021 Gov. Lamont
directed DRS to retroactively enhance the **2020** CT EITC from 23% to **41.5%** using federal
Coronavirus Relief Funds; DRS mailed supplemental checks (~199k households, ~$75M) in early 2022.
For simulating 2020 *liability* under law, 23% is correct; the 18.5-point top-up was an
administrative transfer outside the return.

**Refundability and eligibility:** fully refundable ("If the CT EITC exceeds the taxpayer's
Connecticut income tax liability, the excess is considered an overpayment and will be refunded
without interest" — all booklets). Eligibility tracks the federal EITC (must claim and be allowed
the federal credit, same investment-income limit), with one deviation: **must be a full-year CT
resident** — part-year and nonresidents do not qualify. Statute: CGS § 12-704e.

---

## 5. Property Tax Credit (Form CT-1040 Line 11; Schedule 3)

Credit for property taxes paid to CT political subdivisions on a **primary residence and/or motor
vehicle** (one vehicle for single/MFS/HoH; two for MFJ/QSS). **Nonrefundable, no carryforward**
("This credit can be used to offset only your 20XX income tax. You may not carry this credit
forward and it is not refundable" — booklets), and cannot exceed pre-credit tax (Line 10).

**Maximum credit and eligibility by year (all from opened booklets):**

| Tax year | Max credit per return | Who qualifies |
|---|---|---|
| 2017 | $200 | **Only** filers where taxpayer/spouse is 65+ by year-end OR claimed ≥1 dependent on the federal return |
| 2018 | $200 | same restriction |
| 2019 | $200 | same restriction |
| 2020 | $200 | same restriction |
| 2021 | $200 | same restriction |
| 2022 | $300 | all filers (restriction removed) |
| 2023 | $300 | all filers |
| 2024 | $300 | all filers |
| 2025 | $300 | all filers |

Note: the age/dependent restriction **already applied in TY2017** (2017 booklet: "To qualify for
the property tax credit, you, or your spouse if married filing jointly, must be 65 years of age or
older by the end of the taxable year, OR you must have claimed at least one dependent on your
federal income tax return"). PA 22-118 § 408 raised the max to $300 and extended the credit to all
filers starting TY2022; the $300/all-filers parameters have simply continued since (2023-2025
booklets identical). Gov. Lamont's Feb. 2025 proposal to raise it to **$350** was **not enacted**
— PA 25-168 (FY26-27 budget) contains no property tax credit change, and the 2025 booklet still
says $300. Statute: CGS § 12-704c.

**Phase-out mechanics — 15% steps, not 10%.** The credit (after capping at min(tax, property tax
paid, max credit)) is reduced by a decimal factor from the Property Tax Credit Table based on
**CT AGI** (Line 5, not federal AGI). The table is a step function: the credit is reduced 15% for
each $10,000 (or fraction) of CT AGI above the threshold ($5,000 brackets for MFS), reaching 100%
reduction in the 7th bracket. Thresholds and brackets are **identical in the 2017 and 2024/2025
booklets** (frozen, not indexed):

| Filing status | Full credit if CT AGI ≤ | Decimal steps (each next bracket) | No credit if CT AGI > |
|---|---|---|---|
| Single | $49,500 | .15/.30/.45/.60/.75/.90/1.00 per $10,000 | $109,500 |
| MFJ / QSS | $70,500 | same per $10,000 | $130,500 |
| MFS | $35,250 | same per $5,000 | $65,250 |
| Head of household | $54,500 | same per $10,000 | $114,500 |

Worksheet flow (Schedule 3): Line 65 = min(property tax paid, max credit); Line 66 = decimal from
table; Line 67 = Line 65 × Line 66; **Line 68 (credit) = Line 65 − Line 67**.

---

## 6. Other Additions and Subtractions (Schedule 1) relevant to a broad microsim

Line numbers per the 2025 booklet (stable since ~2019; 2017 numbering nearly identical).

**Additions (Lines 31-38):**
- **Line 31 — Interest on non-CT state/local government obligations** (federally exempt muni
  interest from other states; PR/Guam/AS/USVI obligations excluded from the addback). All years.
- **Line 32 — Exempt-interest dividends from mutual funds** attributable to non-CT state/muni
  obligations (pro-rata share if mixed fund). All years.
- **Line 33 — Taxable lump-sum distributions** (Form 4972) not in federal AGI.
- **Line 35 — Loss on sale of CT state/local bonds** (mirror of the Line 47 subtraction).
- **Line 36 — 100% of the § 168(k) federal bonus depreciation deduction** (added back; recovered
  via a 25%-per-year subtraction in each of the four succeeding years, Line 48a).
- **Line 36a — 80% of the § 179 deduction** (added back; recovered at 25% of the addback per year
  over the four succeeding years).

**Subtractions (Lines 39-50):**
- **Line 39 — Interest on U.S. government obligations** (Treasuries, savings bonds; NOT Fannie
  Mae/Ginnie Mae/Freddie Mac interest, which stays taxable). All years.
- **Line 40 — Exempt dividends from certain qualifying mutual funds** derived from U.S. gov
  obligations.
- **Line 41 — Social Security benefit adjustment** (§ 1 above).
- **Line 42 — Refunds of state and local income taxes** (amount from federal Schedule 1 Line 1 —
  i.e., federally taxable refunds are fully subtracted). All years.
- **Line 43 — Tier 1 and Tier 2 railroad retirement benefits** and supplemental annuities: 100%,
  all years.
- **Line 44 — Military retirement pay: 100%, all years 2017-2025** (armed forces or National
  Guard retirees and survivor-benefit beneficiaries; ex-spouse shares under divorce decrees do NOT
  qualify). History: 50% exemption in TY2014, 100% since TY2015 (PA 14-47; cross-checked 2017
  booklet Line 44 = full subtraction, OLR 2014-R-0289 for the prior 50%).
- **Line 45 — CT Teachers' Retirement System (TRS) income**: **25% for 2017-2020, 50% for 2021+**
  (booklets each year; the scheduled increase to 50% for TY2019 was postponed to TY2021 by the
  2019 budget act — 2019 booklet notes the postponement enacted June 26, 2019). CT-TRS income
  only (not other states' teacher pensions). **Interaction with the pension deduction:**
  2019-2020: a taxpayer may not claim both the TRS subtraction and the pension/annuity subtraction
  on the *same* income (may claim each on different income). From 2021 (PA 21-2 JSS § 433):
  taxpayer takes **whichever of the two is more favorable** on TRS income (relevant in 2021 when
  pension was 42% < 50%, and again in 2024+ for phase-out-range filers, since the TRS 50% has NO
  income limit). Note the Pension and Annuity Worksheet excludes TRS pay from Line 48b income, so
  the choice is implemented by putting TRS income on Line 45 (50%, no AGI limit) vs. Line 48b
  (100% × phase-out factor).
- **Line 47 — Gain on sale of CT state and local government bonds** (subtracted; CT does not tax
  gains on its own obligations).
- **Line 48 — CHET (CT Higher Education Trust / 529) contributions**: max **$5,000
  single/MFS/HoH, $10,000 MFJ/QSS** per year, all years 2017-2025; excess carries forward up to
  five years. (2021 and 2025 booklets quoted; unchanged parameters.)
- **Line 48a — 25% of § 168(k) addback from each of the four preceding years.**
- **Line 48b — pension/annuity/IRA** (§§ 2-3 above).
- **Line 48c — cannabis-business ordinary/necessary expenses** (Chapter 420f/420h licensees,
  2023+; niche).
- **Line 48d — ABLE account contributions** (NEW for 2025): max $5,000 single/MFS/HoH, $10,000
  MFJ/QSS.
- **Line 49 — Other** (tribal-member income, CT individual development account interest, etc.).

**Capital gains:** no CT-specific exclusion or preferential rate — capital gains flow through
federal AGI and are taxed as ordinary income. The only capital-gain modifications are the CT-bond
gain subtraction / loss addition (Lines 47/35). No capital gains surcharge was enacted through the
2025 session (proposals only). Confirmed by absence of any other gain-related line in all nine
booklets.

---

## 7. Other Credits (documented as known differences; not fully parameterized)

- **Personal tax credit (CGS § 12-703)**: 1%-75% of tax, phasing out by CT AGI (max 75% credit up
  to ~$18,800 single, gone above $64,500 single / $100,500 MFJ for 2024). This is part of CT's
  core rate structure (in lieu of a standard deduction) and is presumably already in the
  simulator's CT tax-calculation module; listed here for completeness.
- **Credit for income taxes paid to qualifying jurisdictions** (CGS § 12-704, Schedule 2) — out
  of scope for the state module per project convention; note only.
- **CT child tax credit: NONE enacted through TY2025.** Proposals (2023-2025 sessions) did not
  pass. The closest enacted item is the **$250 EITC add-on for filers with a qualifying child,
  TY2025+** (PA 25-168 § 371; see § 4). Also NEW for TY2026 (not 2025): a $500 refundable credit
  for owners of state-licensed **family child care homes** (PA 25-168 § 372). The 2021 "child tax
  rebate" ($250/child, paid summer 2022 under PA 22-118) was a one-time rebate program outside
  the income tax return.
- **Credit for birth of a stillborn child**: $2,500 (CGS § 12-704i; on Schedule CT-IT since
  TY2022).
- **Refundable on the return**: CT EITC, PE (pass-through entity) tax credit (87.5% of PE tax
  share, 2024+; 87.5% since 2019), historic homes rehabilitation credit (2024+), claim-of-right
  credit. Angel investor, real estate conveyance (2.25%-rate payers, 3-year spread), theater
  production, workforce housing (2025+), ABLE-employer etc. are nonrefundable Schedule CT-IT
  items — all niche for a microsim.

---

## 8. No Standard or Itemized Deductions — CONFIRMED

CT has **no standard deduction and no itemized deductions**. The only step between CT AGI and
taxable income is the **personal exemption** (2024: max $15,000 single / $12,000 MFS / $19,000 HoH
/ $24,000 MFJ, phased out dollar-for-dollar-style until $44,000/$35,000/$56,000/$71,000 CT AGI;
CGS § 12-702), plus the personal tax credit against tax (CGS § 12-703). Federal itemized
deductions never enter the CT calculation. (OLR 2024-R-0130; Form CT-1040 structure, all years.)

---

## Sources used

**DRS Form CT-1040 instruction booklets (primary; all nine opened and text-extracted):**
- 2017: https://portal.ct.gov/-/media/DRS/Forms/... (2017 booklet; local copy `ct1040_2017.pdf`)
- 2018: local copy `ct1040_2018.pdf` (portal.ct.gov DRS prior-year library)
- 2019: local copy `ct1040_2019.pdf`
- 2020: https://portal.ct.gov/-/media/DRS/Forms/2020/Income/CT-1040-Online-Booklet_0221.pdf
- 2021: https://portal.ct.gov/-/media/DRS/Forms/2021/Income/CT-1040-Online-Booklet_1221.pdf
- 2022: https://portal.ct.gov/-/media/DRS/Forms/2022/Income/2022-CT-1040-Instructions_1222.pdf
- 2023: https://portal.ct.gov/-/media/drs/forms/2023/income/2023-ct-1040-instructions_1223.pdf
- 2024: https://portal.ct.gov/-/media/drs/forms/2024/income/2024-ct-1040-instructions_1224.pdf
- 2025: https://portal.ct.gov/-/media/drs/forms/2025/income/2025-ct-1040-instructions_1225.pdf

**OLR reports (quasi-primary, for law history and statute cites):**
- 2025-R-0152, Income Tax Exemptions for Retirement Income (Sept. 2025): https://www.cga.ct.gov/2025/rpt/pdf/2025-R-0152.pdf
- 2023-R-0129 (rev. Dec. 2023), Income Tax Exemptions for Retirement Income: https://www.cga.ct.gov/2023/rpt/pdf/2023-R-0129.pdf
- 2024-R-0130, A Guide to Connecticut's Personal Income Tax: https://cga.ct.gov/2024/rpt/pdf/2024-R-0130.pdf
- 2019-R-0098, Income Tax Deductions for Social Security and Pension Income: https://www.cga.ct.gov/2019/rpt/pdf/2019-R-0098.pdf
- 2025 Acts Affecting Taxes (OLR 2025AA-0093): https://www.cga.ct.gov/olr/Documents/year/AA/2025AA-0093_2025%20Acts%20Affecting%20Taxes.pdf
- 2014-R-0289 (military retirement 50% pre-2015): https://www.cga.ct.gov/2014/rpt/2014-R-0289.htm

**Statutes / acts:** CGS § 12-701(a)(20) (modifications), § 12-702 (exemption), § 12-703
(personal credit), § 12-704c (property tax credit), § 12-704e (EITC); PA 14-47; PA 17-2 JSS;
PA 19-117; PA 21-2 JSS (§§ 430, 433); PA 22-118 (§ 408); PA 23-204 (§§ 377, 378); PA 25-168
(§§ 371, 372). PA 22-118 text: https://www.cga.ct.gov/2022/act/pa/pdf/2022PA-00118-R00HB-05506-PA.pdf ;
PA 25-168 text: https://www.cga.ct.gov/2025/act/pa/pdf/2025PA-00168-R00HB-07287-PA.pdf ;
PA 21-2 JSS text: https://www.cga.ct.gov/2021/act/pa/pdf/2021PA-00002-R00SB-01202SS1-PA.pdf

**Other:** Gov. Lamont press release on the retroactive 2020 EITC enhancement:
https://portal.ct.gov/office-of-the-governor/news/press-releases/2021/12-2021/governor-lamont-directs-eitc-for-2020-to-be-retroactively-enhanced

## Confidence and gaps

**High confidence (read directly from the year's booklet):** SS worksheet mechanics and thresholds
for 2017, 2018, 2019, 2020, 2021, 2025; pension percentages 2019 (14%), 2020 (28%), 2021 (42%),
2022-2023 (100%); IRA 2023 (25%), 2024 (50%), 2025 (75%); the full 2024/2025 phase-out table; EITC
rates for every year 2017-2025 incl. the 2025 $250 child add-on; property tax credit max/eligibility
and the full phase-out table for 2017 and 2022-2025; TRS 25%/50% for every year; CHET limits;
military/railroad/US-interest/refund/muni-bond modifications.

**Moderate confidence (booklet grep confirmed the key number, or OLR-only):**
- SS worksheet thresholds for 2022-2024 were confirmed via the Line 41 instruction text and OLR,
  not by printing each year's worksheet block; no year 2019-2025 shows anything but $75k/$100k.
- The exact section number of PA 17-2 JSS for the SS threshold change (§ 66 per common citation)
  was not verified against the act text.
- Property tax credit 2018-2021 phase-out tables were not printed individually, but the 2017 and
  2024 tables are identical and every booklet grep shows the same thresholds ($49,500 etc.), so
  frozen-parameters is safe.
- PA 22-118's original sunset structure for the PTC expansion (whether the all-filer/$300 rule was
  2022-only on paper and later extended by PA 23-204) was not pinned down; operationally the
  2022-2025 booklets are dispositive ($300, all filers, every year).

**Known source conflict (resolved):** OLR 2025-R-0152 says the IRA deduction "was previously 25%
for the 2024 tax year" — contradicted by the 2024 booklet (50%) and OLR 2024-R-0130 (50%). Treat
as an OLR typo; use 50% for 2024.

**Gaps / to watch:**
- TY2026: IRA reaches 100%; family child care home credit ($500 refundable) begins; no property
  tax credit or rate changes enacted as of the 2025 session. Re-check after the 2026 session
  (Republican property-tax-credit expansion proposals were active in March 2026 per news reports).
- The $250 EITC add-on (2025+) sits on Schedule CT-EITC Line 15a; confirm whether it is per-return
  (it is per-return, not per-child, per the booklet text) when parameterizing.
- Part-year/nonresident apportionment of these modifications (CT-1040NR/PY) was out of scope.
