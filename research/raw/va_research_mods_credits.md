# Virginia Individual Income Tax — Additions, Subtractions, Deductions from AGI, and Credits (TY2017–TY2025)

**Research packet for Tax-Simulator state-tax module (Budget Lab at Yale).**
Prepared 2026-07-18. Primary sources: Virginia Form 760 instruction booklets for **each** tax year 2017–2025 (PDFs downloaded from tax.virginia.gov; all nine years obtained from the live site — no Wayback fallback was needed), Va. Code §§ 58.1-322.01/.02/.03/.04, and Va. Code § 58.1-339.8. Every year-specific value below was verified against that year's booklet text unless flagged in the Verification Notes.

**Microsim input context.** The model operates on IRS PUF-style records and observes: wages, interest (incl. tax-exempt), dividends, business income, capital gains, pensions/IRA distributions, Social Security (incl. taxable portion), itemized-deduction components, age, dependents, and federal EITC/CTC/CDCTC amounts. It does NOT observe: military status, disability income, 529 contributions, education expenses, tuition.

**Return architecture (constant 2017–2025).** Form 760: Line 1 FAGI → Line 2 additions (Sch ADJ Line 3) → Line 4 **age deduction** → Line 5 **taxable Social Security & Tier 1 RR subtraction** → Line 6 **state income tax refund subtraction** → Line 7 other subtractions (Sch ADJ Line 7) → Line 9 VAGI → Line 10/11 itemized or standard deduction → Line 12 exemptions → Virginia taxable income. Schedule ADJ: Line 1 non-VA muni interest addition; Line 2a conformity additions + Lines 2b–2c coded additions; Line 4 U.S.-obligation income subtraction; Line 5 disability income subtraction; Line 6 coded other subtractions; Lines 8a–8c coded deductions from VAGI; Lines 10–17 CLI/EITC credit block. The CLI/EITC credit lands on Form 760 Line 24 (2017–2018) / Line 23 (2019–2025).

**Filing thresholds (all years 2017–2025, verified in each booklet):** if VAGI < $11,950 (single or MFS) / $23,900 (MFJ), Virginia tax is $0 (return still filed for refund of withholding). Unchanged all nine years.

---

## 1. Age Deduction (Form 760, Line 4) — **MODELABLE**

**Statute:** Va. Code § 58.1-322.03(5). Verbatim (a) "$12,000 for individuals born on or before January 1, 1939." (b) "$12,000 for individuals born after January 1, 1939, who have attained the age of 65. This deduction shall be reduced by $1 for every $1 that the taxpayer's adjusted federal adjusted gross income exceeds $50,000 for single taxpayers or $75,000 for married taxpayers. For married taxpayers filing separately, the deduction shall be reduced by $1 for every $1 that the total combined adjusted federal adjusted gross income of both spouses exceeds $75,000." AFAGI is defined in the statute as "federal adjusted gross income minus any benefits received under Title II of the Social Security Act and other benefits subject to federal income taxation solely pursuant to § 86 of the Internal Revenue Code, as amended."

**Booklet definition of AFAGI (2017 and 2024 booklets, identical in substance):** "A taxpayer's AFAGI is the taxpayer's federal adjusted gross income, modified for any fixed date conformity adjustments and reduced by any taxable Social Security and Tier 1 Railroad Benefits." (2017 booklet p. 11; 2024 booklet p. 11 — 2024 says "modified for any conformity adjustments".)

### Mechanics (verified verbatim in 2017 AND 2024/2025 booklets; structure identical)

1. **Born on or before January 1, 1939** → flat $12,000 per qualifying person, **no income test**, regardless of filing status. Each spouse born on/before 1/1/1939 gets a full $12,000.
2. **Born January 2, 1939 – January 1 of (tax year − 64)** (i.e., 65+ by Jan 1 following the tax year but born after 1/1/1939) → income-based deduction of up to $12,000 per person:
   - Single: $12,000 reduced $1-for-$1 by AFAGI over **$50,000**.
   - **All married taxpayers (MFJ and MFS alike): the reduction is computed on the couple's COMBINED (joint) AFAGI over $75,000 — always.** Booklet (2017 p. 11, identical 2024 p. 11): "For all married taxpayers, whether filing jointly or separately, the maximum allowable age deduction of $12,000 each is reduced $1 for every $1 the married taxpayers' joint AFAGI exceeds $75,000. ... A married taxpayer's income-based age deduction is always determined using the married taxpayers' joint AFAGI. ... If both spouses are claiming an income-based age deduction, regardless of whether filing jointly or separately, the married taxpayers must compute a joint age deduction first, then allocate half of the joint deduction to each spouse."
3. **Answer to the married question:** when both spouses claim the income-based deduction, the phase-out applies to the couple's combined **$24,000** (worksheet Line 12 = number of claimants × $12,000), not per person; the surviving amount is then split 50/50. When only one spouse claims (e.g., other spouse is under 65, or is born pre-1939 and takes the flat $12,000), the claimant's $12,000 is reduced by the full joint-AFAGI excess.
4. **Interactions:** a person claiming the age deduction may NOT claim the disability income subtraction (per-person choice: "each eligible spouse may take either an age deduction or a disability income subtraction"). NEITHER spouse may claim any age deduction if either spouse claimed the Credit for Low-Income Individuals or the (nonrefundable or refundable) Virginia EITC — "even if filing separate returns" (2017 & 2024 booklets, Line 4 instructions).

### Age 65 and Older Deduction Worksheet — line-by-line (2024 booklet, p. 10; 2017 worksheet on p. 10 is line-for-line identical except "fixed date conformity (FDC)" wording and year-specific dates)

Header: "Only taxpayers born on or between January 2, 1939, and January 1, 1960 [year-specific], claiming an income-based age deduction ... are required to complete this worksheet. Married taxpayers must enter the combined income of both spouses, regardless of filing status or whether one or both spouses claim an income-based age deduction."

| Ln | Instruction |
|----|-------------|
| 1 | Number of taxpayers born in the income-based window claiming an income-based age deduction. Single: 1. Married: 1 if one spouse claims; **2 if both spouses claim** (regardless of joint/separate filing). |
| 2 | FAGI — single: yours; married: **combined FAGI of both spouses** from federal return(s). |
| 3 | Conformity (fixed-date conformity) addition — combined if married. |
| 4 | Line 2 + Line 3. |
| 5 | Conformity subtraction — combined if married. |
| 6 | Line 4 − Line 5. |
| 7 | Taxable **Social Security and Tier 1 Railroad Benefits** — combined if married. |
| 8 | Line 6 − Line 7 = **AFAGI**. |
| 9 | Income limit: single **$50,000**; all married **$75,000**. |
| 10 | If Line 8 < Line 9: enter **$12,000 per claiming spouse** — done. |
| 11 | Else Line 8 − Line 9 (the excess). |
| 12 | **Line 1 × $12,000** (i.e., $12,000 or $24,000 pool). |
| 13 | If Line 11 > Line 12: **no age deduction** (if computing for both spouses, neither qualifies). |
| 14 | Else Line 12 − Line 11. Single or one-claimant married: this is the deduction. Both-claimant married: go to 15. |
| 15 | Both spouses claiming: **divide Line 14 by 2**; enter half in "You" and "Spouse" columns. |

### Year-by-year (each row verified in that year's booklet, Form 760 Line 4 instructions + worksheet)

| TY | Flat-$12,000 birth cutoff | Income-based window (born) | Amount | Single threshold | Married (joint AFAGI) threshold | Source |
|----|---------------------------|----------------------------|--------|------------------|-------------------------------|--------|
| 2017 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1953 | $12,000 | $50,000 | $75,000 | 2017 booklet pp. 9–11 |
| 2018 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1954 | $12,000 | $50,000 | $75,000 | 2018 booklet |
| 2019 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1955 | $12,000 | $50,000 | $75,000 | 2019 booklet |
| 2020 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1956 | $12,000 | $50,000 | $75,000 | 2020 booklet |
| 2021 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1957 | $12,000 | $50,000 | $75,000 | 2021 booklet |
| 2022 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1958 | $12,000 | $50,000 | $75,000 | 2022 booklet |
| 2023 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1959 | $12,000 | $50,000 | $75,000 | 2023 booklet |
| 2024 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1960 | $12,000 | $50,000 | $75,000 | 2024 booklet pp. 9–11 |
| 2025 | ≤ 1/1/1939 | 1/2/1939 – 1/1/1961 | $12,000 | $50,000 | $75,000 | 2025 booklet pp. 9–11 |

**UNINDEXED and constant all nine years** ($12,000 / $50,000 / $75,000 appear verbatim in every booklet). Eligibility rule in practice: age 65+ by January 1 following the tax year (the cutoff is Jan 1 of tax-year − 64).

**Verdict: MODELABLE.** Requires age, filing status, FAGI, taxable SS (all observed). The pre-1939 flat group is age ≥ 79 in TY2017 rising to age ≥ 87 in TY2025 (born ≤ 1/1/1939) — implementable from age. Conformity adjustments to AFAGI (worksheet Lines 3/5) are ignorable for PUF-style records. Note the credit interaction: model must enforce age-deduction vs. CLI/EITC mutual exclusivity at the *return* (household) level and age-vs-disability exclusivity at the person level.

---

## 2. Social Security / Tier 1 Railroad Retirement Subtraction (Form 760, Line 5) — **MODELABLE**

- **Statute:** Va. Code § 58.1-322.02(3): subtraction for "Benefits received under Title II of the Social Security Act and other benefits subject to federal income taxation solely pursuant to § 86 of the Internal Revenue Code."
- **Booklet (2017 Line 5, verbatim; 2024/2025 identical):** "Enter the amount of taxable social security and/or Tier 1 Railroad Retirement Act Benefits that you included in your federal adjusted gross income. Do not include Tier 2 Railroad Retirement Benefits..." (Tier 2 goes on Schedule ADJ as Other Subtraction code 22.)
- **Every year 2017–2025:** Form 760 **Line 5**, full subtraction of the federally taxable amount. Verified present with identical line number/wording in all nine booklets.
- **Verdict: MODELABLE** — subtract taxable SS (observed). 100% of federally taxable SS is exempt in Virginia in all years.

---

## 3. State Refund Subtraction; U.S.-Obligation Interest; Municipal Bond Addition

### 3a. State income tax refund (Form 760, Line 6) — **MODELABLE**
Va. Code § 58.1-322.02(5). Booklet (all years, Line 6): "Enter the amount of any state income tax refund or overpayment credit that you reported as income on your federal return." Constant 2017–2025.

### 3b. U.S.-obligation income (Schedule ADJ, Line 4) — **MODELABLE with imputation**
Va. Code § 58.1-322.02(1). Booklet (all years): subtract "income (interest, dividends and gain) from obligations of the U.S. that are included in your federal adjusted gross income, but are exempt from Virginia state tax." Booklet lists exempt issuers (U.S. Treasury bills/notes/bonds/savings bonds, TVA, FDIC, FHLB, Federal Land Bank, Farm Credit Bank, Ex-Im Bank, USPS, Guam/PR/VI, etc.) and **taxable** issuers (FHLMC, FNMA, GNMA, Inter-American Development Bank, IBRD). Constant 2017–2025. *PUF caveat:* the U.S.-obligation share of taxable interest/dividends is not separately observed — needs an imputed share (data-limited in magnitude, not in mechanics).

### 3c. Interest on obligations of other states (Schedule ADJ, Line 1 — ADDITION) — **MODELABLE with imputation**
Va. Code § 58.1-322.01(1) (addition for "interest, less related expenses..., on obligations of any state other than Virginia"). Booklet (all years, Sch ADJ Line 1): "Enter the amount of any interest on obligations of other states not included in your Federal Adjusted Gross Income, which is taxable in Virginia, less related expenses." **Own-state exemption:** Virginia-obligation interest is not added back; additionally Sch ADJ subtraction code 20 ("Income from Virginia Obligations") subtracts any VA-obligation income that *did* land in FAGI (e.g., gains). Constant 2017–2025. *PUF caveat:* tax-exempt interest is observed in total; the non-Virginia share must be imputed (standard practice: national out-of-state share).

---

## 4. Disability Income Subtraction (Schedule ADJ, Lines 5a/5b) — **DATA-LIMITED**

- **Statute:** Va. Code § 58.1-322.02(4): "Up to $20,000 of disability income, as defined in § 22(c)(2)(B)(iii) of the Internal Revenue Code; however, any person who claims a deduction under subdivision 5 of § 58.1-322.03 [the age deduction] may not also claim" this subtraction.
- **Booklet (all years, Sch ADJ Line 5; verbatim from 2024):** "Enter the amount of disability income reported as wages (or payments in lieu of wages) on your federal return for permanent and total disability. On joint returns, each spouse can qualify... Individuals can subtract up to **$20,000** of disability income... A taxpayer cannot claim an age deduction on Line 4 of Form 760 and a subtraction for disability income. Claim the one that benefits you the most. For married taxpayers filing a joint return, each taxpayer may claim, if applicable, an age deduction or a subtraction for disability income." Booklets also note SSDI must not be double-counted (taxable SS is already subtracted on Form 760 Line 5) and that disability income becomes retirement income at mandatory retirement age.
- **$20,000 cap verified in all nine booklets, 2017–2025.** Per-person cap; per-person mutual exclusivity with the age deduction.
- **Verdict: DATA-LIMITED** — disability wages are not identified in PUF-style data. Add to known-differences list. (Second-order interaction: some 65+ taxpayers optimally take disability subtraction instead of age deduction; unmodelable, small.)

---

## 5. Military Subtractions — **DATA-LIMITED** (document only)

All are Schedule ADJ "Other Subtractions" codes; all bar the same-income double-dip. Codes 28 and 38 disqualify the household from CLI/EITC (see §7).

| Provision (code) | 2017–2022 | 2023 | 2024 | 2025 | Notes |
|---|---|---|---|---|---|
| **Virginia National Guard (28)** | Lesser of income for 39 calendar days or **$3,000**; ranks **O3 and below** | Lesser of 39 days or **$5,500**; ranks **O6 and below** (effective TY2023, per 2023/2024 booklets) | same | same | VA National Guard only; wages for active/inactive service |
| **Military basic pay (38)** | Up to **$15,000** of basic pay if on extended active duty > 90 consecutive days; phased out $1-for-$1 as basic pay exceeds $15,000 (zero at $30,000) | same | same | same | Per person on joint returns; constant all years |
| **Combat zone pay (30)** | Full subtraction of combat-zone / qualified-hazardous-duty pay & allowances (IRC § 112) to extent in FAGI | same | same | same | Constant |
| **Military Benefits Subtraction (60)** — Va. Code § 58.1-322.02(24) | n/a before TY2022; TY2022: **$10,000**, **age 55+ only** | **$20,000**, age 55+ | **$30,000**, **age requirement removed** (booklet: "For taxable years beginning on and after January 1, 2024, certain military benefits received by an individual may be subtracted" — no age clause; 2022/2023 booklets say "age 55 or older") | **$40,000** (TY2025 and after) | "Military benefits" = military retirement income, IRC § 134 qualified benefits, Survivor Benefit Plan payments to surviving spouses. Per-spouse maximum on joint returns. Not allowed if any other credit/subtraction/deduction claimed on same income. |

Ramp verified verbatim in 2022, 2023, 2024, and 2025 booklets (code 60): "$10,000 in Taxable Year 2022; $20,000 in Taxable Year 2023; $30,000 for Taxable Year 2024; and $40,000 for Taxable Year 2025 and after."

**Verdict: DATA-LIMITED** — military status, military retirement income, and Guard pay are unobserved. Material for VA (large military population): note esp. the TY2022+ Military Benefits Subtraction (up to $40k per person of military pensions by 2025) as a growing known difference on the pension margin.

---

## 6. Child and Dependent Care Expenses Deduction (Schedule ADJ deduction code 101) — **MODELABLE**

- **Statute:** Va. Code § 58.1-322.03(4) (deduction): "the amount of employment-related expenses upon which the federal credit is based under § 21 of the Internal Revenue Code for expenses for household and dependent care services necessary for gainful employment." It is a **deduction from VAGI equal to the federal credit's expense base, not the credit**.
- **Booklet (code 101; 2017 verbatim, and identical operative text in 2018–2025):** "You may claim this deduction on your Virginia return only if you were eligible to claim a credit for child and dependent care expenses on your federal return. Enter the amount on which the federal credit for child and dependent care is based. This is the amount on federal Form 2441 that is multiplied by the decimal amount — **up to $3,000 for one dependent and $6,000 for two or more**. DO NOT ENTER THE FEDERAL CREDIT AMOUNT."
- **Caps verified $3,000/$6,000 in every booklet 2017–2025 — including TY2021**, when the federal (ARPA) expense cap was temporarily $8,000/$16,000. The 2021 Virginia booklet retained the $3,000/$6,000 language (see Verification Notes).
- **Verdict: MODELABLE.** Recover the federal expense base from the observed federal CDCTC (expenses = credit ÷ applicable rate, or use the qualified-expense field if carried), cap at $3,000/$6,000 by number of qualifying dependents, and allow as a deduction from VAGI. Applies whether the taxpayer itemizes or not (it is a Schedule ADJ deduction, additive to standard/itemized).

---

## 7. Credit for Low-Income Individuals / Virginia EITC (Schedule ADJ Lines 10–17) — **MODELABLE**

**Statute:** Va. Code § 58.1-339.8 (originally 2000, c. 397). Subdivision 1: CLI. Subdivision 2 (effective TY2006+): nonrefundable 20% of federal EITC. Subdivision 3: refundable EITC — **15% for TY2022–2024** (enacted 2022 Acts of Assembly, Special Session I, cc. 1 and 2 — the 2022 budget); **20% for TY2025–2026** (2025 amendments; statute history cites 2025 c. 725 and 2026 c. 7). Statute: "In no case shall a household be allowed a credit pursuant to this subdivision 3 and subdivision 1 or 2 for the same taxable year."

### The choice set

| TY | Options (claim exactly ONE per household) | Booklet |
|----|-------------------------------------------|---------|
| 2017–2021 | max of: (a) **CLI** = $300 × (personal + dependent exemptions), nonrefundable; (b) **20% of federal EITC**, nonrefundable. Worksheet takes greater of the two (Sch ADJ Line 16 = max(L13, L15)), then caps at tax liability. | e.g. 2017 booklet pp. 26–27 |
| 2022–2024 | three-way: (a) CLI nonrefundable; (b) 20% federal EITC nonrefundable; (c) **15% of federal EITC, REFUNDABLE**. Sch ADJ: L15 = 20%×EITC; L16a = max(CLI, L15); L16b = 15%×EITC; **L17 = choose** min(L16a, tax) OR refundable L16b. 2022 booklet: "For taxable years beginning January 1, 2022, but before January 1, 2026, instead of claiming one of the two nonrefundable credits above, Virginia residents may claim the Refundable Virginia Earned Income Tax Credit... equal to 15 percent of the federal EITC... You can only claim one of these three credits." | 2022 booklet pp. 25–26; same in 2023, 2024 |
| 2025 | 2025 booklet: "For taxable years 2025 and 2026, new legislation increased the refundable portion of the Virginia Earned Income Tax Credit from 15% to **20%**." Sch ADJ restructured: L15/L16a "Reserved for Future Use"; **L16b = 20% × federal EITC (refundable)**; L17 = choose refundable L16b OR nonrefundable = min(CLI Line 13, tax liability net of Schedule OSC credit). (The nonrefundable-20% option is dropped from the form since it is dominated by the refundable 20%.) | 2025 booklet pp. 24–25 |

Credit is entered on Form 760 Line 24 (2017–2018) / Line 23 (2019–2025).

### CLI mechanics (constant 2017–2025)

1. **Family VAGI test:** family VAGI = sum of VAGI of taxpayer + spouse (always, even MFS — spouse VAGI must be reported) + all dependents with income. Eligible if family VAGI ≤ federal poverty guideline for family size (number of family members listed: taxpayer, spouse, dependents).
2. **Credit amount:** $300 × number of personal + dependent exemptions on the return (**excluding** the 65+/blind add-on exemptions). Nonrefundable, capped at tax liability (net of Schedule OSC credit in recent years).
3. **MFS:** only one spouse may claim the CLI. For the % -of-EITC options, MFS spouses split the Virginia credit in proportion to each spouse's share of the earned income that qualified for the federal EITC.
4. **Eligibility exclusions (verbatim, constant 2017–2025):** the credits "may NOT be claimed if you, your spouse, or any dependents claimed on your return or on your spouse's return claim any of the following: Age deduction; Exemption for taxpayers who are blind or age 65 and over; Virginia National Guard subtraction (Code 28); Basic military pay subtraction (Code 38); Federal & state employee subtraction (Code 39); OR you are claimed as a dependent on another taxpayer's return."

### Poverty Guideline Table by tax year (booklet-printed; = HHS guidelines for the 48 contiguous states published in January of the same calendar year as the tax year)

| Family size | TY2017 | TY2018 | TY2019 | TY2020 | TY2021 | TY2022 | TY2023 | TY2024 | TY2025 |
|---|---|---|---|---|---|---|---|---|---|
| 1 | $12,060 | $12,140 | $12,490 | $12,760 | $12,880 | $13,590 | $14,580 | $15,060 | $15,650 |
| 2 | 16,240 | 16,460 | 16,910 | 17,240 | 17,420 | 18,310 | 19,720 | 20,440 | 21,150 |
| 3 | 20,420 | 20,780 | 21,330 | 21,720 | 21,960 | 23,030 | 24,860 | 25,820 | 26,650 |
| 4 | 24,600 | 25,100 | 25,750 | 26,200 | 26,500 | 27,750 | 30,000 | 31,200 | 32,150 |
| 5 | 28,780 | 29,420 | 30,170 | 30,680 | 31,040 | 32,470 | 35,140 | 36,580 | 37,650 |
| 6 | 32,960 | 33,740 | 34,590 | 35,160 | 35,580 | 37,190 | 40,280 | 41,960 | 43,150 |
| 7 | 37,140 | 38,060 | 39,010 | 39,640 | 40,120 | 41,910 | 45,420 | 47,340 | 48,650 |
| 8 | 41,320 | 42,380 | 43,430 | 44,120 | 44,660 | 46,630 | 50,560 | 52,720 | 54,150 |
| each add'l | +4,180 | +4,320 | +4,420 | +4,480 | +4,540 | +4,720 | +5,140 | +5,380 | +5,500 |

(Each column transcribed from that year's booklet "Poverty Guideline Table".)

**Verdict: MODELABLE.** Inputs: VAGI (computed), exemption counts (from filers/dependents), federal EITC (observed), tax liability. Implement as: if household claims age deduction or 65+/blind exemption → ineligible; else credit = max over available options for the year (respecting refundability). Dependent income entering family VAGI is unobserved — minor understatement of ineligibility. Note the joint optimization with the age deduction / 65+ exemption: a 65+ low-income filer must pick one side; the model should take the max-benefit branch.

---

## 8. Other credits (document as NOT modeled)

- **Political contributions credit** (former § 58.1-339.6, 50% of contributions up to $25/$50 credit): **sunset January 1, 2017** — the 2017 booklet states "Political Contributions No Longer Allowed: Legislation enacted by the 2016 General Assembly imposed a January 1, 2017 sunset date." Not available in any study year → nothing to model.
- **Credit for tax paid to another state** (Schedule OSC): real but out of scope for a resident-only VAGI simulation; note it reduces the liability cap used for the 2025 nonrefundable CLI comparison.
- **Schedule CR credits** (nonrefundable, mostly narrow/certificated): Land Preservation (large aggregate; transferable; per-taxpayer annual claim cap — $20,000 in 2017 rising to $50,000 later; capital-gain-on-sale subtraction code 51 exists), Historic Rehabilitation, Education Improvement Scholarships, Research & Development (refundable), Agricultural Best Management Practices (refundable from 2021), Firearm Safety Device (2023+), Pass-Through Entity Elective Tax Payment credit (2022+, with addition code 20 addback). All DATA-LIMITED / not modeled — list as known differences.
- **Spouse Tax Adjustment** (Form 760 Line 17, up to $259): a rate-structure adjustment for MFJ couples — full worksheet mechanics in §10 below.

---

## 9. Other broad-incidence items (brief)

| Item | Where | Years | Verdict |
|---|---|---|---|
| **529 / Commonwealth Savers contributions** (deduction code 104) | ≤ $4,000 per account per year, unlimited carryforward; **age 70+: unlimited** in year contributed. Renamed Virginia529 → Commonwealth Savers in 2024/2025 booklets; $4,000 cap constant 2017–2025. Va. Code § 58.1-322.03(7). | all | **DATA-LIMITED** (contributions unobserved) — known difference, skews high-income. |
| **Long-term health care premiums** (deduction code 106) | Deduct LTC insurance premiums **provided no federal deduction of any amount** for them was claimed. Constant 2017–2025. Va. Code § 58.1-322.03(10). | all | **DATA-LIMITED** (premiums unobserved in PUF). |
| **Federal WOTC wage disallowance** (subtraction code 21) | Subtract wages/salaries eligible for the federal Work Opportunity Credit that were included in FAGI (business-side wage addback relief). Constant 2017–2025. | all | DATA-LIMITED, tiny incidence. Note: this is a *subtraction* in VA (no addition exists). |
| **Tier 2 Railroad Retirement** (code 22) | Full subtraction. | all | Effectively bundled with pension treatment; unobserved separately — ignore. |
| **Unemployment compensation** (code 37) | Fully subtracted from VA income (Va. Code § 58.1-322.02(11)). Constant, incl. 2020–2021. | all | MODELABLE **if** UI benefits are on the record (not in the stated input list — else DATA-LIMITED; material for 2020–2021). |
| **Virginia Lottery prizes < $600** (code 24), foster care ($1,000/child, code 102), bone-marrow fee (103), first-time home buyer accounts (54/17), student-loan discharge on death (55), REIT (57), ABLEnow (code 115 area), etc. | narrow | all | Not modeled — de minimis. |
| **Standard deduction / itemized context** (Form 760 Lines 10–11): Virginia requires the **same itemize-vs-standard choice as federal**. Itemized = federal itemized less state and local income tax claimed (via Virginia Schedule A from TY2019 on; direct subtraction on Form 760 before). Standard deduction: **$3,000/$6,000 (S,MFS / MFJ) 2017–2018; $4,500/$9,000 2019–2021; $8,000/$16,000 2022–2023; $8,500/$17,000 2024; $8,750/$17,500 2025** (each verified in-year; 2019 and 2022 increases and the 2024/2025 bumps are described in the respective booklets; statute § 58.1-322.03(1)). | all | MODELABLE (core). |
| **Personal exemptions**: $930 per filer/dependent + $800 each for 65+ and blind — constant 2017–2025 (verified every booklet). 65+/blind add-on barred if CLI claimed. | all | MODELABLE. |

---

## 10. Spouse Tax Adjustment — worksheet mechanics (Form 760 Line 18 in 2017–2018; Line 17 in 2019–2025) — **MODELABLE**

**Statute:** Va. Code § 58.1-324 (married individuals' computation) implemented on the return as the STA. **Purpose (booklet, all years):** "Using the STA, couples filing joint returns will not pay higher taxes than if they had filed separate returns... The STA lets both incomes reported on jointly filed returns benefit from the lower tax rates" (2%/3%/5%/5.75% brackets at $3,000/$5,000/$17,000, unchanged all years).

**Eligibility (booklet Line 17/18 instructions, verbatim-constant 2017–2025):** "Couples filing jointly under Filing Status 2 may reduce their tax by up to **$259** with the STA if **both have taxable income to report** and their **combined taxable income ... is more than $3,000**." (Combined taxable income = Form 760 Line 16 in 2017–2018, Line 15 in 2019–2025.) There is no other minimum-income test; the operative both-spouses test is worksheet Line 3 below (each spouse's separate VAGI must exceed that spouse's own personal exemptions).

**Worksheet structure verified line-for-line identical in ALL NINE booklets 2017–2025** (transcribed from 2024 booklet p. 12; 2017 booklet p. 14 identical except Form 760 line references 16/17/18 instead of 15/16/17; structural anchors — the "stop if 0 or less" rule, the ÷2 line, the smaller/larger tax lines, the $17,000/$34,000 shortcut, the $259 cap, and the $800/$930 exemption arithmetic — grep-confirmed in every year).

### Feeder: "Worksheet for Determining Separate Virginia Adjusted Gross Income" (same page, all years)

STEP 1 — separate FAGI, two columns (You / Spouse), allocating each federal item to the spouse who earned/owns it:
1. Wages, salaries, etc.
2. Taxable interest and dividend income
3. Taxable refunds/adjustments/offsets of state and local income tax
4. Business income
5. Capital gains/losses and other gains/losses
6. Taxable pensions, annuities and IRA distributions
7. Rents, royalties, partnerships, estates, trusts, etc.
8. Other income (farm income, taxable social security, etc.)
9. Gross income = sum of 1–8
10. Adjustments to gross income (allocated)
11. Separate FAGI = 9 − 10. "(The total of both columns should equal your joint FAGI reported on your 1040)"

STEP 2 — separate VAGI:
12. Total additions to FAGI (Form 760 Line 2), allocated
13. Line 11 + Line 12
14. **Age Deduction (Form 760 Line 4)** — allocated to the spouse claiming it (per §1, an income-based deduction computed on joint AFAGI and, if both claim, already split 50/50)
15. Taxable Social Security / Tier 1 RR benefits (Form 760 Line 5), allocated by recipient
16. State income tax refund (Form 760 Line 6), allocated
17. Other Subtractions (Form 760 Line 7), allocated
18. Total subtractions = 14+15+16+17
19. **Separate VAGI = Line 13 − Line 18** → STA Worksheet Line 1. "(The total of both columns should equal your combined VAGI reported on Line 9 of your 760)." Spouse's separate VAGI must also be entered on Form 760 Line 17 (Line 18 in 2017–18).

Note what is **NOT** allocated: the standard/itemized deduction, dependents' exemptions, and Schedule ADJ deductions-from-VAGI (Lines 8a–8c) never enter the separate computation — the split stops at VAGI net of each spouse's own personal exemption.

### Spouse Tax Adjustment Worksheet (transcribed; Form 760 line refs are the 2019–2025 vintage, 2017–2018 in brackets)

PART 1 — SEPARATE YOUR INCOME AND EXEMPTIONS (You / Spouse columns):
| Ln | Instruction |
|----|-------------|
| 1 | Each spouse's share of VAGI (Form 760 Line 9), from the feeder worksheet above. |
| 2 | Separate personal exemption amounts per spouse: (count of "65 or over" + "Blind" boxes for that spouse) × **$800**, plus **$930** = that spouse's exemptions. (Own $930 only — no dependents.) |
| 3 | Line 1 − Line 2 per spouse. "**If either amount is 0 or less, stop here; you do not qualify for this credit.**" |

PART 2 — CALCULATE YOUR TAX ADJUSTMENT:
| Ln | Instruction |
|----|-------------|
| 4 | Joint Virginia taxable income from Form 760 Line 15 [2017–18: Line 16]. |
| 5 | The **smaller** amount from Line 3. **Shortcut: "If this amount is larger than $17,000 and Line 4 is larger than $34,000, skip to Line 12 and enter $259 as the credit."** |
| 6 | Line 4 − Line 5 (if ≤ $0, enter $0). |
| 7 | Line 4 ÷ 2. |
| 8 | Tax (tax table / rate schedule) on the **smaller** of Line 5 or Line 7. |
| 9 | Tax on the **larger** of Line 6 or Line 7. |
| 10 | Line 8 + Line 9. |
| 11 | Joint tax from Form 760 Line 16 [2017–18: Line 17]. |
| 12 | **TAX ADJUSTMENT = Line 11 − Line 10**; enter on Form 760 Line 17 [2017–18: Line 18]. "The Spouse Tax Adjustment cannot exceed $259." |

**Interpretation for coding.** The joint taxable income (after joint standard/itemized deduction, all exemptions, and Sch ADJ deductions) is notionally split into the lower-earning spouse's piece — proxied by min-spouse (separate VAGI − own personal exemptions), but never more than half of joint taxable income (the min/max against Line 7 enforces a 50/50 ceiling) — and the remainder. Tax is recomputed on the two pieces with the single-filer rate schedule and compared with the joint-schedule tax; the saving is the credit. Maximum benefit = tax(2×$17,000) − 2×tax($17,000) ≈ $257.50, printed as **$259** (tax-table rounding), reached when each piece ≥ $17,000 — hence the $17,000/$34,000 shortcut.

**Year-by-year:** cap **$259**, shortcut thresholds **$17,000/$34,000**, eligibility text (MFJ only; both spouses with income; combined taxable income > $3,000), the $930/$800 exemption arithmetic, and every worksheet line verified present and unchanged in each booklet 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025 (STA worksheet on p. 14 of the 2017 booklet; p. 12 of the 2019–2025 booklets).

**Age-deduction interaction:** the age deduction reduces the claiming spouse's separate VAGI (feeder Line 14) — so a large age deduction can zero out an older spouse's Line 3 and disqualify the couple from the STA; likewise SS benefits (Line 15) are removed from the recipient spouse's separate VAGI. Retirees whose income is mostly SS + age-deduction-sheltered income often fail the Line 3 test.

**Verdict: MODELABLE.** Requires only an earner-level split of income items already observed (PUF primary/secondary earner splits for wages; joint asset income needs an allocation assumption — booklet allocates by ownership, common practice is 50/50 or primary-earner for joint assets), plus the already-modeled additions/subtractions per spouse. Since the credit is bounded at $259 and is a smooth function of the min-spouse income share, a split assumption for non-wage income is low-stakes.

---

## Sources

**Instruction booklets (primary; all downloaded 2026-07-17/18 from tax.virginia.gov):**
- TY2017: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2017-form-760-instructions.pdf
- TY2018: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2018-form-760-instructions.pdf
- TY2019: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2019-760-instructions.pdf
- TY2020: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2020-form-760-instructions.pdf
- TY2021: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2021-760-instructions.pdf
- TY2022: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2022-760-instructions.pdf
- TY2023: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2023-760-instructions.pdf
- TY2024: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2024-760-instructions.pdf
- TY2025: https://www.tax.virginia.gov/sites/default/files/vatax-pdf/2025-760-instructions.pdf

**Statutes (law.lis.virginia.gov, current text as of fetch):**
- Additions: https://law.lis.virginia.gov/vacode/title58.1/section58.1-322.01/
- Subtractions: https://law.lis.virginia.gov/vacode/title58.1/section58.1-322.02/
- Deductions (incl. age deduction subdiv. 5, CDCTC deduction subdiv. 4, standard deduction subdiv. 1): https://law.lis.virginia.gov/vacode/title58.1/section58.1-322.03/
- Other modifications (fiduciary adjustment etc.; nothing relevant to this packet): https://law.lis.virginia.gov/vacode/title58.1/section58.1-322.04/
- CLI / Virginia EITC: https://law.lis.virginia.gov/vacode/title58.1/section58.1-339.8/

**Schedule ADJ forms (layout reference):** https://www.tax.virginia.gov/sites/default/files/taxforms/individual-income-tax/{year}/schedule-adj-{year}.pdf

---

## Verification notes (anything not fully primary-verified)

1. **TY2021 CDCTC deduction vs. ARPA.** The 2021 Virginia booklet's code-101 text retains "up to $3,000 for one dependent and $6,000 for two or more" even though the federal § 21 expense limits were $8,000/$16,000 for 2021. The statute says the deduction equals the expense base of the federal credit, which for 2021 would exceed the booklet caps. **Recommendation: follow the booklet ($3,000/$6,000 caps in all years)**; I did not locate a VA ruling resolving the conflict (Tax Bulletin 21-4/22-1 conformity guidance not checked line-by-line). Flag if 2021 precision matters.
2. **Statute text is current-vintage.** law.lis.virginia.gov serves the current codification; historical per-year statute text was not pulled. Year-by-year values are instead verified from the year's booklet — treated as controlling for form mechanics.
3. **Refundable-EITC enacting act.** The statute's history line and the 2022 booklet were the sources for TY2022 refundability (2022 Sp. Sess. I, cc. 1–2, budget). The 15%→20% change for TY2025–2026 is verified in the 2025 booklet ("new legislation increased the refundable portion... from 15% to 20%") and reflected in the current statute (history cites 2025 c. 725; 2026 c. 7). Chapter attribution taken from the statute page footer, not read in session-law text.
4. **Post-2025 standard deduction.** Current § 58.1-322.03 shows $9,200/$18,400 (2027) and $9,300/$18,600 (2028–29) with reversion to $3,000/$6,000 in 2030 — i.e., legislation after the 2025 booklet (which still described a post-2026 sunset) extended the elevated amounts. Irrelevant to 2017–2025 values but relevant to out-year baselines.
5. **National Guard change date.** The $3,000/O3 → $5,500/O6 change is stated in the 2024 booklet as "effective for taxable years beginning on and after January 1, 2023," and the 2023 booklet already prints O6/$5,500 — treated as TY2023+.
6. **Military Benefits Subtraction age-55 removal.** 2022 and 2023 booklets say "age 55 or older"; 2024 and 2025 booklets restate the provision without any age clause ("on and after January 1, 2024 ... an individual"). Treated as: age 55+ required for TY2022–2023, no age requirement TY2024+.
7. **Wayback Machine** was not needed; all nine PDFs were live on tax.virginia.gov (the 2017/2018/2020 files use the older `{year}-form-760-instructions.pdf` filename).
8. Local copies of all nine booklet PDFs and extracted text are in the session scratchpad (`...\scratchpad\va\760instr_YYYY.pdf/.txt`) — transient; re-download from the URLs above if needed.
