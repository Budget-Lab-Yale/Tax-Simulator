# Michigan Retirement & Pension Benefits Subtraction — Research Notes

- **State:** Michigan (MI)
- **Date:** 2026-07-23
- **Status:** Complete for TY2017–TY2025 + enacted future law (PA 4 of 2023 phase-in through TY2026+, incl. the 2026–2028 statutory quirk in MCL 206.30(9)(e)). Every headline dollar parameter was read directly off the printed Form 4884 / Schedule 1 instructions PDF for the relevant year (downloaded from michigan.gov). Items not verifiable from a primary source are flagged **UNVERIFIED** inline.
- **Author:** Claude research pass for Tax-Simulator state module (resident PIT calculation on IRS-PUF-style data).

## Scope

Michigan's subtractions from AGI for retirement/pension income: the Form 4884 retirement and pension benefits subtraction (birth-year tier structure), the age-67 "Michigan Standard Deduction" (Schedule 1 lines 25/26 in current numbering), the PA 4 of 2023 phase-in restoration, the Tier-1 senior dividends/interest/capital-gains subtraction, and the fully exempt categories (Social Security, military, Michigan National Guard, railroad retirement). Cross-model notes (TAXSIM, PolicyEngine) and PUF-modeling limitations at the end. Homestead credit, city taxes, and nonresident apportionment are out of scope.

## Primary sources

Michigan Treasury form/instruction PDFs (all verified working 2026-07-23; michigan.gov blocks generic fetchers — use a browser User-Agent):

| Year | Form 4884 | 4884/Schedule 1 instructions used |
|---|---|---|
| 2017 | mirrored at `https://www.taxformfinder.org/forms/2017/2017-michigan-form-4884.pdf` (identical to Treasury form, Rev. 04-17) | Treasury guidance page "2017 Retirement & Pension Information": `https://www.michigan.gov/taxes/questions/iit/accordion/pension/2017-retirement-pension-information` |
| 2018 | `https://www.taxformfinder.org/forms/2018/2018-michigan-form-4884.pdf` (Rev. 05-18) | "2018 Retirement & Pension Information": `https://www.michigan.gov/taxes/questions/iit/accordion/pension/2018-retirement-pension-information` |
| 2019 | `https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT/TY2019/4884_ty2019.pdf` | "2019 Retirement & Pension Information" (Treasury, legacy URL `https://michigan.gov/taxes/0,4676,7-238-43513-511137--,00.html`) |
| 2020 | `https://www.michigan.gov/-/media/Project/Websites/taxes/Forms/IIT/TY2020/4884_ty2020.pdf` | `https://www.michigan.gov/-/media/Project/Websites/taxes/Forms/IIT/TY2020/Schedule_1_Instructions.pdf` |
| 2021 | `https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT/TY2021/4884.pdf` | `https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT/TY2021/Schedule-1-Instructions.pdf` |
| 2022 | `.../TY2022/4884.pdf` | `.../TY2022/Schedule-1-Instructions.pdf` |
| 2023 | `.../TY2023/4884.pdf` | `.../TY2023/4884-Instructions.pdf`, `.../TY2023/Schedule-1-Instructions.pdf` |
| 2024 | `.../TY2024/4884.pdf` | `.../TY2024/4884-Instructions.pdf`, `.../TY2024/Schedule-1-Instructions.pdf` |
| 2025 | `.../TY2025/4884.pdf` | `.../TY2025/4884-Instr.pdf`, `.../TY2025/Schedule-1-Instr.pdf` |

(`...` = `https://www.michigan.gov/taxes/-/media/Project/Websites/taxes/Forms/IIT`.)

Statute and guidance:

1. **MCL 206.30** — definitions of taxable income; retirement/pension deductions at 206.30(1)(e) (military/National Guard/railroad), (1)(f)(i)–(iv) (public, reciprocal-state public, Social Security, capped private), (1)(p) (senior investment income); tier limitations at 206.30(9); PA 4 phase-in at 206.30(10); fire/police/corrections at 206.30(11). Text: `https://www.legislature.mi.gov/Laws/MCL?objectName=MCL-206-30`.
2. **PA 4 of 2023 ("Lowering MI Costs Plan")** — amended MCL 206.30 to add subsections (10) and (11). Signed March 7, 2023; per Treasury, "the law change took effect on February 13, 2024" (no immediate effect) but the new options apply **beginning tax year 2023** and were incorporated into the 2023 forms. Treasury tier pages: `https://www.michigan.gov/taxes/iit/tax-guidance/tax-situations/retirement-and-pension-benefits/{2023,2024,2025}/{year}-tier-{i,ii,iii}`.
3. **Revenue Administrative Bulletin 2023-22**, "Individual Income Tax – Treatment of Retirement Income Under Public Act 4 of 2023" (cited by name in the 2025 Form 4884 instructions as the controlling guidance).
4. Secondary cross-checks (flagged): PolicyEngine-US GitHub parameters `policyengine_us/parameters/gov/states/mi/tax/income/deductions/`; TaxFormFinder mirrors.

---

## 1. Architecture (how the pieces fit)

Michigan starts from federal AGI (MI-1040 line 10) and applies Schedule 1 additions/subtractions, then personal exemptions (MI-1040 line 9; $5,800/exemption in 2025 — already encoded in the model). Retirement income relief comes through **five distinct channels**:

1. **Schedule 1, line 14 (2023+ numbering):** taxable **Social Security** subtracted in full. Never capped, never interacts with the Form 4884 caps (but taxable SS *does* reduce the Tier-3 Michigan Standard Deduction — see §3.3).
2. **Schedule 1, line 11:** **military retirement (U.S. Armed Forces), Michigan National Guard retirement, and taxable railroad retirement (Tier 1 & 2 RRB)** subtracted in full. These amounts **reduce** the Form 4884 private caps and the Michigan Standard Deduction dollar-for-dollar.
3. **Schedule 1, line 27 / Form 4884:** the **retirement and pension benefits subtraction** (the tier structure, §3; plus the PA 4 phase-in election, §5).
4. **Schedule 1, lines 25–26:** the age-67 **"Michigan Standard Deduction"** for Tier 2/Tier 3 (against ALL income, not just pension income) — mutually exclusive with the line-27 subtraction (§3.2–3.3).
5. **Schedule 1, line 28:** the **senior dividends/interest/capital-gains subtraction**, born-before-1946 only (§4).

**Which spouse controls (verified):** "Married couples filing a joint return should complete Form 4884 based on the year of birth of the **older spouse**." (2023 & 2025 4884 instructions, "Which Benefits are Taxable"; statutorily MCL 206.30(9)(f) and (10)(e): "For a joint return, the limitations and restrictions in this subsection shall be applied based on the date of birth of the older spouse filing the joint return.")

**Per-return, not per-person:** all Form 4884 caps and the Michigan Standard Deduction are filing-status amounts applied to the return's combined benefits (single limits apply to MFS: "For purposes of this form, single limits apply to taxpayers who are married filing separately," 2025 4884 instructions). The only per-person elements are the SSA-exempt add-ons ($15,000 per eligible spouse, Section C "up to $15,000 per eligible taxpayer").

**Age-67 convention:** "Per federal guidelines, you are considered to have reached the age of 67 the day before your 67th birthday" (Treasury Tier II pages). Hence the Tier-3 birth window for TY2025 runs through **January 1, 1959** (turns 67 on Dec 31, 2025 under the convention).

---

## 2. Per-year parameter table

### 2.1 Tier-1 private retirement cap (Form 4884 Section A; MCL 206.30(1)(f)(iv))

Statutory base $42,240 single / $84,480 joint (2007), indexed annually by CPI-U ("adjusted by the percentage increase in the United States Consumer Price Index for the immediately preceding calendar year"). Printed amounts, read off each year's Form 4884:

| Tax year | Single/MFS | Joint | Source (exact line) |
|---|---|---|---|
| 2017 | $50,509 | $101,019 | 2017 Form 4884, Section A line 8 (also Section B line 16) |
| 2018 | $51,570 | $103,140 | 2018 Form 4884, line 9 (and line 17) |
| 2019 | $52,808 | $105,615 | 2019 Form 4884, line 9 (and line 17) |
| 2020 | $53,759 | $107,517 | 2020 Form 4884, line 9 (and line 17) |
| 2021 | $54,404 | $108,808 | 2021 Form 4884, line 9 |
| 2022 | $56,961 | $113,922 | 2022 Form 4884, line 9 |
| 2023 | $61,518 | $123,036 | 2023 Form 4884, line 9; Worksheets 3.1–3.3 line 1 |
| 2024 | $64,040 | $128,080 | 2024 Form 4884, line 9 |
| 2025 | $65,897 | $131,794 | 2025 Form 4884, line 9 |

Note: the commonly-cited "$51,570/$103,140 for 2017" is wrong — that is the **2018** amount; 2017 is $50,509/$101,019 (verified on both the 2017 form and Treasury's 2017 guidance page).

### 2.2 PA 4 phase-in election amounts (Form 4884 Section D, 2023+; MCL 206.30(10))

Phase-in percentage × that year's Tier-1 cap (after reduction for military/railroad amounts on Schedule 1 line 11 — Worksheet 3.3 subtracts line 11 amounts from the cap *before* applying the percentage):

| Tax year | % | Single/MFS max | Joint max | Eligible cohort (older spouse) | Source |
|---|---|---|---|---|---|
| 2023 | 25% | $15,380 | $30,759 | born after 1945 and before 1959 (form: "born on or after January 1, 1946 but before January 1, 1959") | 2023 Form 4884 line 19; 2023 instr. Line 19 + Worksheet 3.3 ("Multiply line 3 by 25% (0.25)"); MCL 206.30(10)(a) |
| 2024 | 50% | $32,020 | $64,040 | born after 1945 and before 1963 (form: "January 1, 1946 through December 31, 1962") | 2024 Form 4884 line 19; 2024 instr. Line 19 + Worksheet 3.3 (50%); MCL 206.30(10)(b) |
| 2025 | 75% | $49,423 | $98,846 | born after 1945 and before 1967 (form: "January 1, 1946 through December 31, 1966") | 2025 Form 4884 line 19; 2025 instr. Line 19 + Worksheet 3.3 (75%); MCL 206.30(10)(c) |
| 2026+ | 100% | = Tier-1 cap | = Tier-1 cap | everyone (no birth-year restriction) | MCL 206.30(10)(d) |

Confirmed: the base is **that year's indexed Tier-1 cap** (Worksheet 3.3 line 1 is the same dollar figure as Section A line 9; 25% of $61,518 = $15,379.50 → $15,380 printed; 75% of $65,897 = $49,422.75 → $49,423 printed). The old tier rules **remain available** — MCL 206.30(10): "a taxpayer may **elect** to deduct retirement or pension benefits as provided under subsection (1)(f) with the following limitations ... **or elect to apply the limitations and restrictions in subsection (9)**, or subsection (11) if applicable"; Treasury Tier II/III pages: "retirees have the option to choose the best taxing situation ... by opting into any one of the following calculation methods **each year**." Form 4884 Part 4: "Complete only one of the sections below."

**2026+ nuance (enacted future law):** under the fully phased-in option, MCL 206.30(10)(d) caps **public and private combined** at the Tier-1 maximum ("the amounts deductible under subsection (1)(f)(i) and (ii) combined are subject to the same maximum amounts allowed under subsection (1)(f)(iv)"). A born-before-1946 taxpayer with large MI/federal public pensions would still elect the subsection (9)(a) tier rule (unlimited public) instead — both remain electable.

### 2.3 Flat amounts (NOT indexed — frozen; unchanged 2017–2025, no 2025 change)

| Parameter | Amount | Statute |
|---|---|---|
| Tier-2 under-67 retirement cap / Tier-2 & Tier-3 Michigan Standard Deduction | $20,000 single / $40,000 joint | MCL 206.30(9)(b), (9)(e) |
| SSA-exempt add-on (one spouse / both spouses) | +$15,000 / +$30,000 | 4884 instructions Line 17 (2023–2025); Sched 1 line 25 instructions |
| SSA-exempt, retired as of 1/1/2013 (born after 1952/1945 window) | $35,000 single / $55,000 joint / $70,000 joint if both qualify | MCL 206.30(9)(c) |
| SSA-exempt, born after 1952, age 62–66 | $15,000 per eligible taxpayer ($30,000 joint if both) | MCL 206.30(9)(d); Form 4884 Section C |

### 2.4 Tier-1 senior dividends/interest/capital-gains subtraction (Schedule 1 line 28 in 2023+; line 26 in 2020–2022 numbering)

Statutory base $9,420/$18,840 (2007), CPI-indexed; **born before 1946 only** — MCL 206.30(1)(p): "The deduction under this subdivision is **not available to a senior citizen born after 1945**." ("Senior citizen" = 65+, MCL 206.514 — automatically satisfied for born-before-1946 in this window.)

| Tax year | Single/MFS | Joint | Source |
|---|---|---|---|
| 2017 | $11,259 | $22,518 | Treasury "2017 Retirement & Pension Information" page ("Subtraction for dividends, interest, and capital gains is limited to $11,259 for single filers and $22,518 for joint filers") |
| 2018 | $11,495 | $22,991 | Treasury 2018 page, same sentence |
| 2019 | $11,771 | $23,542 | Treasury "2019 Retirement & Pension Information" page ("limited to $11,771 for single filers and to $23,542 for joint filers for 2019") |
| 2020 | $11,983 | $23,966 | 2020 Schedule 1 instructions, Line 26 |
| 2021 | $12,127 | $24,254 | 2021 Schedule 1 instructions |
| 2022 | $12,697 | $25,394 | 2022 Schedule 1 instructions |
| 2023 | $13,712 | $27,424 | 2023 Schedule 1 instructions |
| 2024 | $14,274 | $28,548 | 2024 Schedule 1 instructions |
| 2025 | $14,688 | $29,376 | 2025 Schedule 1 instructions, Line 28 worksheet line 1 |

Cap is reduced (per the 2025 Line 28 worksheet, identical structure all years) by: (a) military/National Guard/railroad subtraction (line 11), (b) the public+private retirement subtraction (line 27/Form 4884), and (c) the elderly-and-disabled-credit-amount subtraction claimed on line 23. Social Security does **not** reduce it. Subtraction = min(interest+dividends+capital gains in AGI, reduced cap).

---

## 3. Tier mechanics (pre-2023 law; still electable in 2023+ as "subsection (9)" rules)

### 3.1 Tier 1 — born before 1946

- **Public** (MI state/local, federal civil service) retirement benefits: **unlimited** subtraction. MCL 206.30(9)(a): "For a person born before 1946, this subsection provides no additional restrictions or limitations under subsection (1)(f)."
- **Private** benefits: up to the indexed cap (§2.1), where the cap is first reduced by military/National Guard/railroad amounts (Schedule 1 line 11) and then by public benefits subtracted. Form mechanics (2025 Section A): line 9 cap → line 10 subtract military/railroad → line 12 public benefits → line 13 = remaining headroom → line 15 = min(headroom, private benefits) → line 16 = public + line 15. "If the public retirement benefits are greater than the maximum amount, the recipient ... [is] not entitled to claim an additional subtraction for private retirement benefits" (2025 instr.).
- Public pensions **from other states** count as *private* (capped) unless the other state offers a reciprocal exemption — MCL 206.30(1)(f)(ii); 2025 instr.: "All other qualifying benefits are considered private benefits (including public benefits from other states that offer a similar or reciprocal subtraction...)". Treasury tier page states the limitation the other way ("subtraction of public sources of pension income derived from other states is limited to private retirement maximums").
- **Social Security does NOT reduce the caps** — only military/MI National Guard/railroad do ("NOTE: Private pension limits for all filers are reduced by the following from Schedule 1, line 11: Military retirement from the U.S. Armed Forces; Retirement from the Michigan National Guard; Railroad retirement." — 2023 & 2025 4884 instructions). Confirmed: the reduction list never includes SS.
- Plus the senior investment-income subtraction (§2.4) — Tier 1 only.

### 3.2 Tier 2 — born 1946 through 1952

- **Before age 67:** retirement subtraction capped at **$20,000 single / $40,000 joint** (sum across public+private; MCL 206.30(9)(b): "the sum of the deductions under subsection (1)(f)(i), (ii), and (iv) is limited to $20,000.00 ... and $40,000.00..."). On the 2017–2020 forms this was Section C ("maximum $20,000 if single or $40,000 if filing jointly"); 2021–2022 Section B; 2023+ Section B via Worksheet 3.1 (which now also layers the Tier-1 cap net of public benefits first, then the $20k/$40k+SSA add-on ceiling — lines 9–12). SSA-exempt filers add $15k/$30k.
- **At 67+ (all Tier 2 reached 67 during TY2019; born 1952 turned 67 in 2019):** the pension subtraction is replaced by the **Tier 2 Michigan Standard Deduction** of $20,000/$40,000 **against ALL income types**. MCL 206.30(9)(b): "After that person reaches the age of 67, the deductions under subsection (1)(f)(i), (ii), and (iv) do not apply and that person is eligible for a deduction of $20,000.00 ... which deduction is available against all types of income and is not restricted to income from retirement or pension benefits."
  - Reduced by: military active-duty pay (Sched 1 line 14 component) and military/National Guard/railroad retirement (line 11). Worksheet 2 lines 4–6 (2025); 2020 Line 23 instr.: "The standard deduction is reduced by any amounts reported on line 11 and any military pay included on line 14."
  - **NOT reduced by Social Security or personal exemptions** (contrast Tier 3). Tier 2 claimants keep the SS subtraction and personal exemptions in full.
  - Claimed on **Schedule 1 line 25** (2023+; line 23 in 2020–2021 numbering, line 24 on the 2017/2018 forms' cross-reference, line 23 on 2019's). "Do not complete this line if you claim an amount on line 27" — mutually exclusive with the Form 4884 subtraction.
  - SSA-exempt add-on: +$15,000 if either box 24C or 24G checked; +$30,000 if both (Worksheet 2 line 2a).
  - 2023+ interaction: a 67+ Tier 2 filer picks the best of (i) Worksheet 2 standard deduction or (ii) the Section D phase-in subtraction or (iii) Worksheet 3.1 (SSA-exempt cases). 2019-era form header (pre-PA 4 baseline): "If the older of you or your spouse was born during the period January 1, 1946 through December 31, 1952, and reached age 67 on or before December 31, 2019, **do not complete this form. Instead, complete Schedule 1, line 23**" — i.e., pre-2023, 67+ Tier 2 filers with pension income were *forced* onto the standard deduction (which is never worse than $20k/$40k of pension subtraction, since it covers all income).

### 3.3 Tier 3 — born after 1952

- **Before age 67: NO retirement subtraction** (MCL 206.30(9)(e): "for a person born after 1952, the deduction under subsection (1)(f)(i), (ii), or (iv) does not apply."), except the two SSA-exempt carve-outs (§2.3) — 2017 form header: "If the filer and spouse ... were born after December 31, 1952, STOP; you are not entitled to a pension subtraction *unless* you have reached age 62 and receive Social Security exempt retirement benefits."
- **At age 67+ (first cohort: born 1953 reached 67 in TY2020):** eligible for the **Tier 3 Michigan Standard Deduction**, $20,000/$40,000 against all income, but — this is the critical difference from Tier 2 — **reduced by taxable Social Security and by the personal exemption amounts**, in addition to military pay and military/railroad retirement.
  - **Exact statutory election language**, MCL 206.30(9)(e): "When that person reaches the age of 67, that person is eligible for a deduction of $20,000.00 for a single return and $40,000.00 for a joint return, which deduction is available against all types of income... For tax years that begin before January 1, 2026 and after December 31, 2028, if a person takes the deduction ..., that person **shall not take the deduction under subsection (1)(f)(iii) [Social Security] and shall not take the personal exemption under subsection (2)**. ... that person may elect not to take the deduction ... and elect to take the deduction under subsection (1)(f)(iii) and the personal exemption under subsection (2) if that election would reduce that person's tax liability."
  - So the statutory election is: (a) $20k/$40k against all income, **forgoing the SS subtraction AND personal exemptions**, vs (b) keep SS subtraction + personal exemptions with **no** retirement subtraction. **What is forgone: the Social Security subtraction and the personal exemptions. NOT the military/railroad subtraction** (that instead reduces the deduction amount).
  - **Form implementation nets it instead of forcing a forgo** — Worksheet 2 (2025 4884/Sched 1 instructions): line 1 = $20,000/$40,000 (+SSA add-ons at line 2) → subtract military pay (line 4) and military/NG/railroad retirement (line 5) → for Tier 3, additionally subtract "taxable Social Security benefits included in AGI from Schedule 1, line 14" (line 8) and "the amounts from MI-1040, lines 9a and 9d" [personal exemptions + disabled-veteran exemption] (line 9) → line 11 residual = Tier 3 Michigan Standard Deduction, claimed on **Schedule 1 line 26**. Explicit note: "**Worksheet 2 has been set up such that a taxpayer claiming the Tier 3 Michigan Standard Deduction will still complete the personal exemption and applicable subtractions normally.**" (2020 and 2025 instructions, identical sentence.) I.e., model it as: `tier3_std_ded = max(0, 20k/40k (+15k/30k SSA) − mil_pay − mil_RR_ret − taxable_SS − personal_exemption_amounts)`, claimed **in addition to** the normal SS subtraction and exemptions. The netting makes the worksheet mathematically identical to the statutory either/or election (it can never hurt).
  - 2026–2028 statutory quirk (enacted future law): for tax years beginning on/after 1/1/2026 and before 1/1/2029, MCL 206.30(9)(e) as amended by PA 4 drops the SS-forgo — "if a person takes the deduction ..., that person shall not take the personal exemption" only. Expect the TY2026 Worksheet 2 to stop subtracting taxable SS (worksheet line 8). **UNVERIFIED** how Treasury will implement (no 2026 forms yet); mostly moot because the 100% phase-in dominates for pension income, but the Tier-3 std deduction still matters for 67+ filers with non-pension income.
- Pre-2023 Tier 3 line numbers: Schedule 1 line 24 (2020–2021), claimed via the same Worksheet-2-style netting (2020 worksheet lines: 20k/40k − line 11 amounts − taxable SS/military pay − MI-1040 9a & 9d).

### 3.4 Special rules (noted for completeness; mostly unobservable in PUF)

- **SSA-exempt employment** (police/fire retirees, CSRS federal hires pre-1984, some state/local): three carve-outs — (i) born 1946–1952 (later extended to born-after-1945) AND retired as of 1/1/2013: caps become $35,000/$55,000 ($70,000 if both spouses qualify), and at 67+ the standard deduction itself becomes $35k/$55k/$70k (MCL 206.30(9)(c)); (ii) born after 1952, age 62–66, SSA-exempt benefits: $15,000/$15,000 ($30,000 if both) (9)(d) — Form 4884 Section C; (iii) the $15k/$30k add-on to the Tier 2/3 standard deduction via Schedule 1 checkboxes 24C/24D/24G/24H (23C/23F/23G, 22C/22F/22G in earlier years).
- **Surviving spouse:** a "qualifying surviving spouse" (claimed a retirement/SS subtraction on the final joint return, not remarried) may claim the subtraction based on the **older of self or deceased spouse's** birth year, at single-filer limits; if 67+ and born 1946–Jan 1 1959 window, may elect the better of the Michigan Standard Deduction or the Form 4884 subtraction (2025 instr. "Retirement and Pension Benefits Subtraction as a Qualifying Surviving Spouse"; MCL 206.30(9)(f), (10)(e)). 2017–2020 Section A also let a Tier-1 filer add a deceased 1946–1952 spouse's benefits up to $20k/$40k (2020 form line 13).
- **Fire/police/county-corrections (2023+, PA 4):** NOT a blanket full exemption. MCL 206.30(11): recipients of retirement benefits "for services as a public police or fire department employee [1969 PA 312], a state police trooper or sergeant [1980 PA 17], or a corrections officer employed by a county sheriff..." may elect to deduct under (1)(f) "**without any additional limitations or restrictions**" — i.e., **regardless of age/birth year** they get the Tier-1 treatment: unlimited MI/federal public benefits + private benefits up to the Tier-1 cap (cap reduced by public claimed and by military/railroad). 2025 instr.: "(a) All qualifying retirement benefits received from federal or Michigan public sources, and qualifying private retirement benefits up to $65,897/$131,794 ... (b) If eligible, a Michigan Standard Deduction based on their year of birth" — claim the most beneficial. Federal employment "substantially similar" also qualifies (instructions NOTE). Checkbox: Form 4884 line 6a (2023+).

---

## 4. What counts as "retirement and pension benefits" on Form 4884

From the 2023/2025 4884 instructions, "What are Retirement and Pension Benefits" (language stable across years):

**Qualifying:**
- Defined-benefit pension plans ("plans that define eligibility for retirement and set contribution and benefit amounts in advance").
- Qualified retirement plans for the self-employed (Keogh/SEP).
- **401(k)/403(b) distributions ONLY to the extent "attributable to employer contributions or attributable to employee contributions that result in additional employer contributions (e.g., matching contributions)"** — i.e., unmatched employee-only 401(k)/403(b) money does NOT qualify. (403(b) exception: plans purchased by a 501(c)(3) org or public school system qualify even if employee-funded — 2025 instr. exclusion list wording.)
- **IRA distributions after age 59½** or IRC 72(t)(2)(A)(iv) substantially-equal-periodic-payments-for-life. (So yes, IRA distributions qualify, age-conditioned.)
- Benefits received due to disability, or as surviving spouse (conditions in §3.4).
- Life annuity policies paid to a senior citizen (65+) for life.
- Qualifying foreign plans.

**NOT qualifying (excluded from Form 4884; fully taxable):**
- "Amounts received **before the recipient could retire under the plan provisions**" — early distributions (in practice: 1099-R distribution codes 1/2, pre-59½/early separation) do not qualify.
- Early-retirement incentives not paid from a pension trust.
- Deferred-comp plans where the employee sets the amount aside with no retirement age/service requirements: **401(k) attributable to unmatched employee contributions alone; 403(b) unmatched employee contributions (with the 501(c)(3)/public-school exception); 457 plans; Thrift Savings Plan distributions** (TSP exclusion explicit in the 2025 instructions list).
- Social Security, military, Michigan National Guard, railroad retirement — **never on Form 4884** ("Do not enter Social Security, military or railroad retirement/pension benefits here (see Schedule 1)" — Form 4884 Part 3 header, all years). Confirmed again: military/railroad reduce the 4884 caps; **Social Security does not reduce any 4884 cap** (it only reduces the Tier-3 standard deduction).

**Public = ** State of Michigan, Michigan local governments (counties, cities, school districts), federal civil service. Rollovers keep the character of the original plan. Everything else (including other states' public pensions absent reciprocity) = private.

---

## 5. The 2023+ restoration (PA 4 of 2023; MCL 206.30(10))

- Statutory phase-in, quoted (MCL 206.30(10)(a), the 2024/2025 paragraphs are identical except year/cohort/percentage): "For the 2023 tax year, a taxpayer who was born after 1945 and before 1959 may deduct an amount of retirement or pension benefits not to exceed **25%** of the maximum amount of retirement or pension benefits that the taxpayer would be allowed to deduct for the tax year under subsection (1)(f)(iv) if the taxpayer's retirement or pension benefits were subject to the limitations of that subsection only." → 2024: born after 1945/before 1963, **50%**; 2025: born after 1945/before 1967, **75%**; 2026+: everyone, **100%** (public+private combined capped at the (1)(f)(iv) max — see §2.2 nuance).
- **It is an election, per year, per taxpayer** ("may elect ... or elect to apply the limitations and restrictions in subsection (9)"); the taxpayer takes the better of the old tier rules and the phase-in. Applies to **public and private benefits combined** without distinction (Worksheet 3.3 line 5: "Enter total public and private retirement and pension benefits").
- The percentage applies to the cap **net of military/railroad amounts** (Worksheet 3.3: line 3 = cap − Sched 1 line 11 amounts; line 4 = line 3 × pct; subtraction = min(line 4, total benefits)).
- The birth-year windows mean the phase-in never covers under-59-ish new cohorts early: born after 1958 get nothing in 2023, born after 1962 nothing in 2024, born after 1966 nothing in 2025 ("Recipients born after December 31, 1966 ... do not qualify ...; all benefits included in AGI are taxable" — 2025 instr.). Born-before-1946 taxpayers are outside the §(10) cohorts in 2023–2025 but are always weakly better off under §(9)(a) anyway.
- **Tier 2/3 Michigan Standard Deduction remains available** under the new law (it's part of subsection (9), which remains electable; the 2023+ Worksheet 2 and Schedule 1 lines 25/26 are unchanged). Treasury Tier II page: "retirees who have reached the age of 67 have the option to choose ... Michigan Standard Deduction [or] Phase-In subtraction. The Tier structure subtraction can not be taken with the phase-in subtraction, you must elect one or the other." The 4884 questionnaire (2025 Q6) has 67+ filers "complete Worksheet 2 and Worksheet 3.3 and claim the most beneficial subtraction."
- Fire/police/corrections full-restoration election (MCL 206.30(11)) also effective for tax years beginning on/after 1/1/2023 — see §3.4.
- Michigan's flat rate context: 4.25% (2017–2022, 2024+), 4.05% in TY2023 only (automatic trigger) — relevant for revenue weighting of this provision, encoded elsewhere.

---

## 6. Modeling formulas (per return; `yob` = birth year of older spouse)

Let `cap` = Tier-1 cap (§2.1), `milRR` = Schedule 1 line 11 subtraction (military + MI National Guard + railroad retirement), `pub`/`priv` = qualifying public/private benefits in AGI, `pct`/`window` = §2.2, `ssded` = taxable SS, `exemps` = MI-1040 9a+9d amounts, `milpay` = active-duty pay in AGI. Ignoring SSA-exempt/fire-police/surviving-spouse flags (unobservable):

```
tier1_sub  (yob<1946):            pub + min(priv, max(0, cap − milRR − pub))
tier2_sub  (1946≤yob≤1952, <67):  min(pub+priv, 20k/40k)          # pre-2023 exact; 2023+ Worksheet 3.1 adds
                                                                   # an outer Tier-1-cap layer (binds only above cap)
std_ded_t2 (1946≤yob≤1952, 67+):  max(0, 20k/40k − milpay − milRR)          # against ALL income; excl. 4884 sub
std_ded_t3 (yob>1952, 67+):       max(0, 20k/40k − milpay − milRR − ssded − exemps)  # against ALL income
phasein    (2023+, yob in window): min(pub+priv, pct × max(0, cap − milRR))
line27_sub = best available among {tier rules, phasein}; std_ded claimed only if no line27_sub
senior_inv (yob<1946):            min(int+div+kg, max(0, invcap − milRR − line27_sub − elderly_credit_sub))
```

Decision rule per return: choose max-tax-benefit combination subject to mutual exclusivity (std deduction XOR Form 4884 subtraction; senior_inv stacks with Tier-1 subtraction).

---

## 7. Known differences / limitations for PUF-based modeling

1. **Public vs private pension source is unobservable** in the PUF (single taxable-pension amount). Binds for: Tier-1 (unlimited public vs capped private), Worksheet 3.1's Tier-1 layer, and the 2026+ combined cap. Options: impute a public share (SOI/ACS industry-of-longest-job style), or treat all pensions as private (understates Tier-1 subtraction for public retirees; conservative on revenue). Post-2026 the distinction mostly stops mattering (combined cap) except for born-before-1946 electors.
2. **Employer-contribution 401(k)/403(b) rule, 457/TSP exclusions, and the retire-under-plan-provisions test are unobservable** — the PUF pension amount includes 457/TSP/unmatched-401(k) money that Michigan would tax. Treating all 1099-R-style pension income as qualifying overstates the subtraction. Early distributions: partially controllable via age (deny the subtraction below ~59½; IRA distributions before 59½ never qualify).
3. **IRA distributions**: qualify (after 59½), so the MI concept ≈ PUF taxable pensions (e01700) + taxable IRA distributions (e01400) for 59½+ filers — matches TAXSIM's `pensions` definition ("Taxable Pensions and IRA distributions").
4. **Older-spouse rule vs per-person modeling:** MI is a per-return, older-spouse-birth-year system. With head/spouse ages available, use `max(age1, age2)`; do NOT model per-person caps (only the SSA-exempt add-ons are per-person, and those are unobservable anyway).
5. **The Michigan Standard Deduction is against ALL income**, not pension-only. Approximating it as a pension subtraction understates relief for 67+ filers whose pension < $20k/$40k but who have wages/interest/etc. Implement as a separate deduction with the §6 netting formulas (including the Tier-3 SS+exemption offsets — otherwise Tier-3 relief is grossly overstated, since most 67+ filers' taxable SS + exemptions exceed $20k/$40k... note taxable SS is the *federal* taxable portion, and MI exemptions are ~$5.8k/person in 2025, so a joint Tier-3 return with $20k taxable SS + 2 exemptions ≈ $31.6k offset against $40k → ~$8.4k residual deduction).
6. **SSA-exempt employment, fire/police/corrections, surviving-spouse elections: unobservable.** All expand relief for small populations (police/fire/CSRS retirees). Omitting them biases MI tax slightly upward for those groups.
7. **Interaction ordering:** military/railroad amounts (Schedule 1 line 11) must be modeled before the 4884 caps and standard deductions (they reduce every cap). If the model lacks a military-pension flag, the reduction drops out (military retirement is then implicitly in the pension pool — it would be over-subtracted for Tier 2/3 and the caps under-reduced; roughly offsetting, but flag it).
8. **Senior investment-income subtraction** (§2.4) is implementable from PUF variables (interest + dividends + capital gains, age 65+/born<1946 cohort) and should be included — it phases itself out as the birth-before-1946 population shrinks.

---

## 8. Cross-model notes (secondary sources, flagged)

### PolicyEngine-US (cross-check only)

- Parameters at `policyengine_us/parameters/gov/states/mi/tax/income/deductions/`: `retirement_benefits/{tier_one, tier_three, expanded}`, `standard/{tier_two, tier_three}`, `interest_dividends_capital_gains/`. Variables: `mi_pension_benefit.py`, `mi_standard_deduction.py`, plus tier variables.
- **Their per-year values match this document exactly**: `tier_one/amount.yaml` SINGLE 2017–2025 = 50,509 / 51,570 / 52,808 / 53,759 / 54,404 / 56,961 / 61,518 / 64,040 / 65,897; `expanded/rate.yaml` = 0.25/0.5/0.75/1.0 (2023/24/25/26); `expanded/birth_year.yaml` windows 1946–<1959/<1963/<1967; `interest_dividends_capital_gains/amount.yaml` matches §2.4 (2017–2024 shown, 2025 uprated). Good independent corroboration.
- Expected gaps: no `retirement_benefits/tier_two` parameter dir — the under-67 Tier-2 $20k/$40k pension cap appears unmodeled (moot for TY2020+, since all born-≤1952 are 67+ from 2019 on; matters only for TY2017–2019 simulation). `tier_three/ss_exempt` exists (SSA-exempt carve-outs partially modeled). **UNVERIFIED** how PE handles the public/private split on microdata (their MI Tier-1 logic needs a public-pension identifier that CPS/PUF lack).

### TAXSIM (NBER)

- Inputs relevant to MI: `pensions` ("Taxable Pensions and IRA distributions"), `page`/`sage` (ages), `gssi` (gross SS). No public/private pension split, no SSA-exempt or fire/police flags, no deceased-spouse info.
- **UNVERIFIED whether/how TAXSIM v35 implements the MI tier structure for 2017–2020** — taxsim.nber.org is blocked from this host (403) and the state calculator source is not publicly inspectable. TAXSIM does maintain the MI forms in its historical archive (`taxsim.nber.org/historical_state_tax_forms/MI/...`) and its documentation says state age-conditioned retirement exclusions are modeled, so the *expected* signature is: an age-based (birth-year) exclusion applied to the single `pensions` input, presumably treating all pensions as private (capped) — which would **undertax nobody but overtax born-before-1946 public pensioners** relative to true law, and cannot capture the Tier-3 SS/exemption offset precisely unless they implement the Worksheet 2 netting.
- Cross-model triage guidance (per the divergence policy): MI pension divergences vs TAXSIM should be classified before triage; divergences concentrated in (a) 65+ records with pensions > ~$50k (public/private split), (b) 67+ records with small pensions and large other income (all-income standard deduction), and (c) TY2023+ phase-in election records, are expected-by-construction.

---

## 9. UNVERIFIED items (summary)

1. TAXSIM's internal MI implementation (blocked host; expected signature noted in §8).
2. How Treasury will implement the 2026–2028 MCL 206.30(9)(e) SS-forgo suspension on the TY2026 Worksheet 2 (no 2026 forms exist yet).
3. PolicyEngine's public/private pension identification on microdata (parameters verified; simulation-input handling not inspected).
4. 2017–2019 senior investment-income caps are from Treasury's official per-year guidance pages rather than the printed Schedule 1 instructions PDFs (2019 page retrieved via search excerpt of the michigan.gov legacy page); values independently corroborated by PolicyEngine parameters. 2020–2025 are from the printed Schedule 1 instructions.
5. Exact Schedule 1 line numbers for the standard deduction / investment subtraction in 2017–2019 and 2021–2022 (line numbering drifted year to year; the numbers given for 2020 and 2023–2025 are verified verbatim, others are as cross-referenced on the Form 4884 of that year). Retirement-subtraction destination lines verified from each Form 4884: Sched 1 line 25 (2017–18), 24 (2019), 25 (2020–21), 26 (2022), 27 (2023–25).
