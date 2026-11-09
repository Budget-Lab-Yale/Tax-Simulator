# Pennsylvania Personal Income Tax — Research Notes (Core)

- **State:** Pennsylvania (PA)
- **Date:** 2026-07-23
- **Status:** Complete for TY2017–TY2025 + enacted future changes. All headline parameters verified against PA-40 instruction booklets (revenue.pa.gov PDFs, TY2017–TY2025) and statute. Items not verifiable from a primary source are flagged **UNVERIFIED** inline.
- **Author:** Claude research pass for Tax-Simulator state module (resident PIT calculation on IRS-PUF-style data).

## Scope

Resident personal income tax only. Covers: flat rate; the eight statutory income classes and loss-offset rules; income PA does not tax; the three-to-five allowed deductions (Schedule O); Tax Forgiveness (Schedule SP) in full detail; credits (Child & Dependent Care Enhancement, new 2025 Working Pennsylvanians Tax Credit, resident credit note); filing requirement; local-tax documentation note; PUF-modeling limitations; cross-model notes (TAXSIM, PolicyEngine). Nonresident/part-year apportionment, estates/trusts as filers, use tax, and Schedule OC business credits are out of scope.

## Primary sources

PA DOR form/instruction PDFs follow a stable URL pattern (verified working for every year 2017–2025):

`https://www.pa.gov/content/dam/copapwp-pagov/en/revenue/documents/formsandpublications/formsforindividuals/pit/documents/{YYYY}/{YYYY}_pa-40in.pdf`

1. **PA-40 Instructions Booklets, TY2017–TY2025** (`{YYYY}_pa-40in.pdf` per pattern above). Each booklet: rate on p.1; "Who Must File" p.4–5; income class instructions p.10–20; Schedule SP instructions + Eligibility Income Tables near the back (2024: p.37–39).
2. **PA-40 Schedule SP** (form + instructions), e.g. 2025: `.../2025/2025_pa-40sp.pdf`; 2024: `.../2024/2024_pa-40sp.pdf`.
3. **PA-40 Schedule O (Other Deductions)**, 2024: `.../2024/2024_pa-40o.pdf`; 2025: `.../2025/2025_pa-40o.pdf`.
4. **PA-40 Schedule A (Interest)**, 2024: `.../2024/2024_pa-40a.pdf`.
5. **PA-40 Schedule DC (Child & Dependent Care Enhancement Tax Credit)**, 2022/2023/2024/2025: `.../{YYYY}/{YYYY}_pa-40dc.pdf`.
6. **Statutes** — Tax Reform Code of 1971, Article III: 72 P.S. § 7302 (rate), § 7303 (classes of income), § 7304 (special tax provisions for poverty). Read via FindLaw mirrors: `https://codes.findlaw.com/pa/title-72-ps-taxation-and-fiscal-affairs/pa-st-sect-72-7302/` (also `.../72-7303/`, `.../72-7304/`).
7. **PA PIT Guide** chapters (DOR guidance): Tax Forgiveness `https://www.pa.gov/agencies/revenue/forms-and-publications/pa-personal-income-tax-guide/tax-forgiveness`; Gross Compensation `https://www.pa.gov/agencies/revenue/forms-and-publications/pa-personal-income-tax-guide/gross-compensation`.
8. **DOR topic pages**: CDCTC `https://www.pa.gov/agencies/revenue/resources/tax-types-and-information/personal-income-tax/child-and-dependent-care-credit`; Working Pennsylvanians Tax Credit `https://www.pa.gov/agencies/revenue/resources/tax-types-and-information/personal-income-tax/working-pennsylvanians-tax-credit`.
9. **NBER state tax forms archive** `https://taxsim.nber.org/state-tax-forms/` — historical form mirror (not needed here; all years pulled directly from pa.gov, which archives 2017+ under the same URL pattern).
10. Secondary cross-checks (flagged where used): PolicyEngine-US GitHub parameters (`policyengine_us/parameters/gov/states/pa/`); Spotlight PA budget reporting.

---

## 1. Rate

Flat rate on each dollar of taxable income of every resident individual: **3.07% (0.0307) in every year 2017–2025**. No brackets, no separate rate by class, no preferential capital gains rate.

| Tax year | Rate | Source (verified verbatim) |
|---|---|---|
| 2017 | 3.07% | 2017 PA-40 booklet p.1: "The state income tax rate for 2017 is 3.07 percent" |
| 2018 | 3.07% | 2018 booklet p.1, same sentence |
| 2019 | 3.07% | 2019 booklet p.1, same sentence |
| 2020 | 3.07% | 2020 booklet p.1, same sentence |
| 2021 | 3.07% | 2021 booklet p.1, same sentence |
| 2022 | 3.07% | 2022 booklet p.1, same sentence |
| 2023 | 3.07% | 2023 booklet p.1, same sentence |
| 2024 | 3.07% | 2024 booklet p.1, same sentence |
| 2025 | 3.07% | 2025 booklet p.1, same sentence |

- Statute: 72 P.S. § 7302 — "every resident individual, estate or trust shall be subject to … a tax upon each dollar of income received … at the rate of three and seven hundredths per cent." (FindLaw text current through the Dec 14, 2023 amendment, which changed grantor-trust attribution, not the rate.)
- Mechanics: PA-40 Line 12 = Line 11 (adjusted PA taxable income) × 0.0307 (2024 booklet Line 12 instruction: "Multiply Line 11 by 3.07 percent (0.0307)").
- Background (not needed for 2017–2025): rate has been 3.07% since Jan 1, 2004 (raised from 2.8% by Act 46 of 2003). **UNVERIFIED** from a primary source in this pass; irrelevant to the modeled window.
- No enacted rate change through TY2026 as of 2026-07-23 (2025-26 budget, signed Nov 2025, left the rate at 3.07%).

## 2. Tax base — the eight classes of income (72 P.S. § 7303(a))

PA taxes eight enumerated classes, each computed separately (PA-40 Lines 1–8):

| PA-40 line | Class |
|---|---|
| 1a | Compensation (wages, salaries, tips, bonuses, fees; gross class — only Schedule UE unreimbursed employee expenses deductible) |
| 2 | Interest (Schedule A) |
| 3 | Dividends and capital gains distributions (Schedule B) — mutual fund capital gain distributions are taxed as *dividends*, not gains |
| 4 | Net income/loss from operation of a business, profession, or farm (Schedules C/F; includes partnership and PA S-corp distributive shares via RK-1) |
| 5 | Net gain/loss from sale, exchange, or disposition of property (Schedule D) |
| 6 | Net income/loss from rents, royalties, patents, copyrights (Schedule E) |
| 7 | Estate or trust income (Schedule J; beneficiaries cannot report a loss) |
| 8 | Gambling and lottery winnings (Schedule T) |

Sources: 72 P.S. § 7303(a) (FindLaw); 2024 booklet p.8 "PA INCOME CLASSES". Statute anti-overlap language: "To the extent that income or gain is subject to tax under one of the classes … such income or gain shall not be subject to tax under another of such enumerated classes."

### Loss rules (critical for the calculator)

2024 booklet p.15, "REPORTING NET INCOME, GAINS, AND LOSSES ON LINES 4, 5, AND 6 ONLY" (identical language in earlier booklets):

- **Losses may only be reported on Lines 4, 5, and 6** (business, property disposition, rents/royalties). Lines 1, 2, 3, 8 are gross or non-negative classes; Line 7 cannot be negative.
- **No cross-class offset:** "You may not offset income in one PA income class with a loss in any other PA income class."
- **No carryforward/carryback:** "You cannot carry forward or carry back gains or losses to other tax years."
- **No spousal offset:** "Spouses, whether filing jointly or separately, may not use each other's expenses to reduce income or offset each other's income and losses… even when both have activity in the same income class." On a joint return, per class: if both spouses have income, add; if both have losses, add; **if one has income and one has a loss, report only the income and ignore the loss** (2024 booklet p.15 rules 1–3; also p.6 "Joint Income – Joint Returns"). Joint filing is "for convenience only" — there is no marriage benefit or penalty in the base.
- Gross classes: "PA-taxable interest income (Line 2), dividend income (Line 3), and gambling and lottery winnings (Line 8) are gross taxable income classes. You may not deduct any expenses in computing these classes" (2024 booklet p.9). Exception: gambling losses (wagering costs only) are deductible against winnings within Line 8; PA Lottery ticket costs deductible only for tickets bought on/after Jan 1, 2016.
- Line total: PA-40 Line 9 "Total PA Taxable Income" = sum of positive Lines 1–8 (each line floored at 0 for cross-class purposes; Lines 4/5/6 can individually show a loss but a loss line contributes 0 to Line 9).

### What PA does NOT tax

2024 booklet p.8, "INCOME NOT TAXABLE FOR PA PIT PURPOSES" (list stable across 2017–2025 booklets):

- **Social Security and Railroad Retirement benefits**
- **Unemployment compensation and public assistance**
- **Commonly recognized pension/old age/retirement benefits paid after becoming eligible to retire and retiring** (see retirement detail below); United Mine Workers pensions; military pension benefits; Civil Service annuities
- **Workers' compensation** (occupational disease acts, Heart and Lung pensions), payments for injuries received while working, personal injury damage awards
- **Child support**
- **Alimony** — confirmed: alimony received is NOT taxable under PA PIT (it is, however, counted in Schedule SP eligibility income — §3 below). PA treatment does not depend on the federal TCJA pre/post-2019 divorce distinction.
- Sick pay and disability benefits (third-party; not sick *leave* wages), employer-paid group term life premiums, inheritances/death benefits/IRD (as compensation), active-duty military pay earned outside PA, federal stimulus/economic impact payments, 529 earnings used for qualified education expenses, damage awards for physical injury/sickness.

### Retirement distributions (booklet 2024 p.11, "Distributions from Eligible Employer-Sponsored Retirement or Deferred Compensation Programs" and "Individual Retirement Accounts")

- **Employer plans:** all amounts received are taxable compensation *except*: (1) payments received **after qualifying for retirement (plan age or years-of-service conditions) and retiring**; (2) rollovers that are federally nontaxable; (3) distributions representing previously-taxed contributions; (4) death benefit payments to estate/beneficiary; (5) **all** distributions (any 1099-R code) from PA SERS, PSERS, PA Municipal Employees' Retirement System, and the U.S. Civil Service Commission Retirement Disability Plan; (6) uniformed-services retired pay.
- **Early/nonqualifying distributions — cost-recovery method:** because PA never allowed a deduction for the contributions, an early distribution is taxable only **to the extent it exceeds previously taxed contributions (basis)** ("Cost Recovery," 2024 booklet p.9: PA "will not tax your distributions … until you have recovered an amount equal to your contributions").
- **IRAs (incl. Roth):** contributions never deductible; undistributed earnings not taxed; distributions taxable to the extent they exceed previous contributions, **unless** received on/after **age 59½** or paid on account of death. "Distributions you receive after retiring but before age 59½ are taxable" (over basis) and **PA has no analogues to the federal early-withdrawal penalty exceptions**. Qualifying rollovers (trustee-to-trustee or 60-day, 100%) nontaxable.
- **Elective deferrals:** employee contributions to 401(k)/403(b)/457/thrift plans **are PA-taxable compensation when made** (2024 booklet p.11 "Contributions"), i.e., PA state wages (W-2 Box 16) exceed federal Box 1 wages by the deferral amount.

### Interest (Line 2, PA-40 Schedule A)

2024 Schedule A (`2024_pa-40a.pdf`) instructions:

- Taxable interest includes "**Obligations of other states and countries**" — i.e., **other states' municipal bond interest IS taxable**. Mechanically, Schedule A starts from federal interest, Line 2 adds "Tax-exempt interest income included in Line 2a of your federal return," then Line 6 subtracts "Interest income from direct obligations of the Commonwealth of Pennsylvania [and] political subdivisions" and Line 7 subtracts "Interest income from direct obligations of the U.S. government." Net effect: **US-obligation interest exempt; PA state/local bond interest exempt; other-state/local and foreign government bond interest taxable.** (Same for exempt-interest dividends via Schedule B.)
- GNMA/FNMA and other federally *guaranteed* (not direct) obligations are taxable.
- Also swept into PA interest: non-qualifying 529/ABLE withdrawals, life insurance/endowment contract income, taxable MSA/HSA distributions, forfeited-interest penalty offset rule (offset only against same-account interest; excess is a Schedule D loss).

### Capital gains (Line 5)

- **Fully taxable at 3.07% as net gains class — no preferential rate, no exclusion, no netting against other classes, no carryover.** Federal Schedule D concepts (LT/ST, $3,000 loss allowance) do not apply; PA gains/losses net only within Line 5 (and per spouse).
- Losses recognized only on transactions entered into for profit — **no loss on personal-use property** (car, furniture, nonqualifying residence sale loss = 0, not negative).
- **Principal residence:** 100% gain exclusion under PA's own rule — "This exclusion is not identical to the federal exclusion. Generally, if during the five years preceding the sale of your home, you owned it for at least two years, and used it as your principal residence for at least two years, you are eligible" (2024 booklet p.17). Qualifying sale: don't report at all. **PA's exclusion has no federal-style $250k/$500k dollar cap — it is a full exclusion when the 2-of-5 test is met** (PA-19 used for nonqualifying/partial-business-use sales). Note: the excluded gain counts in Schedule SP eligibility income (Line 8 of SP Section III).
- Involuntary conversions after Sept 11, 2016 follow IRC §1033 non-recognition; IRC §1035 insurance-contract exchanges tax-free.

## 3. Deductions — PA allows almost none (PA-40 Line 10, Schedule O)

"PA law does not allow standard deductions, deductions for personal exemptions, itemized deductions, or deductions for personal expenses" (2024 booklet p.9, "Deductions"). **No standard deduction, no itemized deductions, no personal or dependent exemptions, in any year.**

The only deductions (PA-40 Line 10 via Schedule O):

1. **IRC §529 qualified tuition program contributions** — capped per beneficiary, per taxpayer and per spouse separately, at the **federal annual gift-tax exclusion** (2025 Schedule O instructions: "current annual federal gift tax exclusion amount is $19,000"):

   | Tax year | 529 cap per beneficiary (per spouse) | Source |
   |---|---|---|
   | 2017 | $14,000 | 2017 booklet: "A taxpayer and spouse each may deduct $14,000 per beneficiary" |
   | 2018 | $15,000 | 2018 booklet, same construction |
   | 2019 | $15,000 | 2019 booklet: "maximum deduction of $15,000 per beneficiary" |
   | 2020 | $15,000 | 2020 booklet |
   | 2021 | $15,000 | 2021 booklet |
   | 2022 | $16,000 | 2022 booklet |
   | 2023 | $17,000 | 2023 booklet |
   | 2024 | $18,000 | 2024 booklet + 2024 Schedule O ("Limit $18,000 per beneficiary, per taxpayer-spouse") |
   | 2025 | $19,000 | 2025 Schedule O ("Limit $19,000 per beneficiary, per taxpayer-spouse") |

2. **Medical Savings Account (Archer MSA) contributions** "allowed for federal purposes" (Schedule O Line 5).
3. **Health Savings Account contributions** "allowed for federal purposes" (Schedule O Line 6) — i.e., the federal HSA deduction amount.
4. **ABLE (IRC §529A) contributions** (Schedule O Section II) — limit follows the federal §529A annual limit (= gift exclusion).
5. **NEW TY2025: Student loan interest, up to $2,500/year** (2025 booklet p.1 "2025 STATE TAX CHANGE"; 2025 Schedule O Line 7: "Student Loan Interest is now an allowable deduction, up to $2,500 per taxable year"). Enacted 2025; part of the 2025 state tax changes. Specific act number **UNVERIFIED** (secondary sources tie it to the 2025–26 budget package effective Jan 1, 2025).

**Limitation:** Schedule O Lines 8–9 cap the deduction at each spouse's own PA-40 Line 9 income — the deduction **cannot create or increase a loss** and cannot be shifted between spouses. PA-40 Line 11 = Line 9 − Line 10.

## 4. Tax Forgiveness (Schedule SP; 72 P.S. § 7304)

The dominant low-income feature. A percentage (100% down to 0% in 10-point steps) of net PA tax liability is forgiven based on **eligibility income** and dependents.

### Statutory parameters — unchanged 2017–2025 (not indexed)

72 P.S. § 7304 (FindLaw): single claimant poverty income limit "$6,500 or less"; married claimant "$13,000 or less" (joint); "$9,500 for each dependent." Partial forgiveness: 90% if excess ≤ $250, 80% if ≤ $500, …, 10% if ≤ $2,250 — i.e., **forgiveness drops 10 percentage points per $250 (or fraction) of eligibility income above the 100% limit; zero above limit + $2,250.**

Verified against the published Eligibility Income Tables in every booklet 2017–2025 (all identical):

- **Table 1 (Unmarried, Separated, Deceased claimants):** 100% column starts at $6,500 (0 dependents), $16,000 (1), $25,500 (2), $35,000 (3), … +$9,500 per dependent; columns step $250 across to the 10% column ($8,750 for 0 dependents).
- **Table 2 (Married claimants, even if filing separately):** 100% column $13,000 (0 dep), $22,500 (1), $32,000 (2), $41,500 (3), …; 10% column $15,250 (0 dep).
- Decimal equivalents printed: 1.0, .90, .80, .70, .60, .50, .40, .30, .20, .10.

Cite: 2024 booklet p.39 tables; 2025 Schedule SP tables (identical values); 2017–2023 booklets (base cells $6,500/$13,000 and row increments verified each year).

### Eligibility income — exact definition (2024 booklet p.38–39; 2025 Schedule SP instructions identical)

Eligibility income = **PA taxable income (PA-40 Line 9) PLUS** the following nontaxable items (SP Section III, Lines 1–10):

1. **Line 1** — PA taxable income (PA-40 Line 9).
2. **Line 2** — Nontaxable interest, dividends, and gains (+ annualized decedent income): tax-exempt interest from Schedule A Lines 6–7 (US, PA, PA-local obligations), exempt-interest dividends from Schedule B, exempt gains on federal/PA direct obligations, **the nontaxable portion of gain from the sale of any property**, nontaxable estate/trust income, KOZ-exempt income.
3. **Line 3** — **Alimony received** (not child support).
4. **Line 4** — **Insurance proceeds and inheritances**: total life/other insurance proceeds, inherited cash or value of property, 1099-R death distributions (Box 7 code 4), survivor annuity benefits (code 7 per instructions text).
5. **Line 5** — **Gifts, awards, prizes**: total nontaxable cash/property gifts, civic/social achievement awards, **noncash PA Lottery prizes**. (No de-minimis floor appears in the 2017–2025 SP instructions — the "gifts over a floor" idea is **UNVERIFIED/not found**; instructions say "total amount.")
6. **Line 6** — Non-PA income of part-year residents/nonresidents (incl. Servicemembers Civil Relief Act-exempt amounts); reciprocal-state compensation for reciprocal-state residents.
7. **Line 7** — **Nontaxable military income** (total military pay minus PA-taxed pay), **excluding combat zone and hazardous duty pay**.
8. **Line 8** — **Gain excluded from the sale of a principal residence**.
9. **Line 9** — **Nontaxable educational assistance**: scholarships, fellowships, stipends, federal/state educational grants, employer tuition reductions. **Not student loans.**
10. **Line 10** — **Cash received for personal use from outside your home**: spousal support from a spouse/former spouse outside the household (not child support); payments/cash/property from persons outside the household (e.g., parent's cash for clothing, gifts from grown children; not cost-sharing); **nontaxable payments to employer cafeteria plans** for hospitalization/sickness/disability/death, supplemental unemployment, or strike benefits; **foster-care payments** (if claiming the foster child); personal use of employer property **if included in federal taxable income**; value of government education grants; bankruptcy-estate income (§1398).

**Explicitly EXCLUDED from eligibility income** ("Do not include the following types of nontaxable income in Line 10," 2024 booklet p.39 / 2025 SP instructions):

- Social Security and Railroad Retirement benefits
- Retirement benefits from PA-eligible retirement plans **after becoming eligible to retire and retiring** (i.e., qualifying pension/IRA-after-59½ distributions do NOT count)
- United Mine Workers pensions; military pension benefits; Civil Service annuity payments
- Child support (unless the claimant is a dependent child, who must include support paid on their behalf)
- Workers' compensation (incl. Heart and Lung), payments for work injuries and personal-injury damages
- Sick pay and disability benefits (third-party insurers)
- Damage awards/settlements for physical injury or sickness
- Personal use of employer property **not** included in federal taxable income; long-term-care insurance contract income
- Per the PIT Guide Tax Forgiveness chapter (DOR guidance): **unemployment compensation** ("payments by any governmental agency"), **public assistance/welfare**, and **noncash government assistance (e.g., surplus food)** are likewise not eligibility income. (The booklet's exclusion list doesn't name UC explicitly; the PIT Guide does — treat UC/welfare as excluded.)

### Structural/eligibility rules

- **Who can claim:** anyone subject to PA PIT who is **not claimed as a dependent on another person's federal return** and meets the income test. Nonresidents/part-year residents may claim (must include worldwide income in eligibility income).
- **Dependent claimants:** a dependent child CAN claim Tax Forgiveness **only if their parents (grandparents, foster parents) themselves qualify** on their own Schedule SP; the child files their own PA-40 + SP and includes child support received in eligibility income. A dependent full-time student cannot claim unless parents qualify. No adult (non-child) dependents may be claimed.
- **Dependent definition:** minor or adult child claimable as a dependent on the claimant's federal return (child/grandchild/foster child); no age cap if federally claimable.
- **Married claimants — JOINT eligibility income always:** "When filing separately, the taxpayer and spouse must combine their eligibility incomes… A married taxpayer cannot claim Tax Forgiveness independently of their spouse"; both MFJ and MFS use **Table 2 with joint eligibility income**. "There is no advantage to filing separately." Unmarried-for-SP status ("Separated") applies to taxpayers separated and living apart for the last six months of the year (use Table 1). If one spouse is another person's federal dependent, the couple must file separate SPs.
- **Credit computation (SP Section IV):** Line 12 = PA-40 **Line 12** tax liability; Line 13 = resident credit (PA-40 Line 22); Line 14 = net liability; Line 15 = forgiveness decimal from table; **Line 16 = Line 14 × Line 15 → PA-40 Line 21** ("Tax Forgiveness Credit"). So forgiveness applies to tax **after** the other-state resident credit but **before** withholding/estimated payments; it is effectively refundable (refunds withheld tax).

## 5. Credits

### 5.1 Child and Dependent Care Enhancement Tax Credit (PA-40 Schedule DC → Line 23)

| Tax year | Credit | Refundable? | Authority |
|---|---|---|---|
| 2017–2021 | none (credit did not exist) | — | — |
| 2022 | **30% of the federal §21 CDCTC amount** (Schedule DC Line 3 = 30% × federal Form 2441 **Line 9a**) | Yes (flows into PA-40 Line 23 → Line 24 "Total Payments and Credits") | Act 53 of 2022 (Tax Reform Code Article XIX-G); 2022 PA-40 booklet p.1 & Line 23; 2022 Schedule DC |
| 2023 | **100% of the federal §21 credit amount** | Yes — "This credit is refundable" (2024/2025 DC instructions; DOR CDCTC page) | Act 34 of 2023 (signed Dec 2023, effective TY2023); 2023 Schedule DC (Line 2 entered directly on PA-40 Line 23) |
| 2024–2025 | 100%, same design | Yes | 2024/2025 Schedule DC |

Key modeling facts (2024 Schedule DC instructions, verified):

- Base = **federal Form 2441 Line 9a** — the *tentative* federal credit (expenses capped $3,000/$6,000 × 35%→20% AGI-based rate) **before** the federal tax-liability limitation. PA maxima: $1,050/$2,100 (income ≤ $43,000), minima $600/$1,200 above — the published PA chart reproduces the federal 35%→20% schedule.
- Taxpayer "must also have claimed the expenses on their Federal 1040 return" (federal 2441 + Schedule 3 must be attached); eligibility follows IRC §21 and its regulations (qualifying child <13, incapacitated spouse/dependent; MFJ generally required; §21 separated/living-apart exceptions incorporated).
- Refundability: explicit in 2023+ materials; for 2022, the credit sits in PA-40 Line 23/24 payments-and-credits structure (refund-generating). DOR describes the program as refundable from inception. 2022-specific "refundable" wording in a primary 2022 document: **UNVERIFIED** (2022 DC instructions don't use the word; structure + DOR statements imply it).
- Cross-check (secondary): PolicyEngine `gov/states/pa/tax/income/credits/cdcc/match.yaml` = 0.30 (2022-01-01), 1.00 (2023-01-01) — matches.

### 5.2 State EITC — none through TY2024; NEW for TY2025: Working Pennsylvanians Tax Credit (WPTC)

- **TY2017–TY2024: PA has NO state EITC and NO state CTC.** Confirmed by absence from every booklet's credit lines (Line 21 forgiveness, Line 22 resident credit, Line 23 Schedule OC/DC only). Tax Forgiveness is PA's low-income mechanism instead.
- **TY2025+: Working Pennsylvanians Tax Credit** — **10% of the federal EITC, refundable**, enacted in the 2025–26 budget signed Nov 12, 2025; **applies beginning with TY2025 returns** (filed in the 2026 season). DOR calculates it automatically from the federal EITC claimed (paper filers attach the 1040). Sources: DOR WPTC page (10%, refundable, "beginning with the 2026 tax season"); Spotlight PA (2025-11): "The Working Pennsylvanians Tax Credit will apply to 2025 returns, Shapiro said"; max ≈ $805 (10% × $8,046 max federal EITC for 2025). Statutory cite per PolicyEngine reference: Tax Reform Code **Article XVI-W.2, § 1603-W.2(c)** (HB 416) — act number **UNVERIFIED** in this pass.
- CAUTION: the April-2025 revision of the 2025 PA-40 booklet (fetched here) **predates enactment** and does not mention the WPTC; expect a later revision/updated forms. Model TY2025 with WPTC = 10% × federal EITC, refundable.
- **No state CTC in any year** (the CDCTC above is a *care-expense* credit, not a child credit).

### 5.3 Resident credit (PA-40 Line 22, Schedules G-L) — note only, not modeled

Credit for income tax paid to other states on income also taxed by PA (not for reciprocal-state wages — PA/IN/MD/NJ/OH/VA/WV compensation is instead exempt/refundable at source). Interacts with Tax Forgiveness (SP Line 13 subtracts it before applying the forgiveness percentage). Out of scope for a resident-only model without multi-state income.

## 6. Filing requirement ("Who Must File," 2024 booklet p.4; same rule 2017–2025)

File a PA-40 if either:
1. "You received total PA gross taxable income **in excess of $33** during [year], even if no tax is due" (that's $33 × 3.07% ≈ $1 of tax); **and/or**
2. "You incurred a loss from any transaction as an individual, sole proprietor, partner in a partnership or PA S corporation shareholder."

Verified verbatim in the 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, and 2025 booklets. A dependent child with PA taxable income > $33 must file their own return. PA filing requirement is independent of the federal one.

## 7. Local taxes (documentation only — OUT OF SCOPE)

- **Act 32 local Earned Income Taxes:** nearly all PA municipalities/school districts levy a flat local EIT (commonly 1%, higher in some places) on compensation and net profits, collected by county tax officers via employer withholding; PA-40 booklets carry school district codes for this purpose. Not part of the PA-40 liability.
- **Philadelphia Wage Tax / NPT:** Philadelphia levies its own wage tax (resident rate ≈ 3.7–3.9% over this window) and net profits tax in lieu of an Act 32 EIT. Philadelphia residents get no PA-40 interaction (and Tax Forgiveness does not offset local tax; Philadelphia has its own income-based wage tax refund tied to Schedule SP eligibility — also out of scope).

## 8. Other items material to a resident PIT calculation on PUF-style data

- **PA Lottery winnings (2016 change):** cash prizes from the *Pennsylvania* Lottery **paid on or after Jan 1, 2016 are taxable** (Line 8); noncash PA Lottery prizes remain nontaxable (but count in SP eligibility income). PA Lottery ticket costs deductible against winnings only for tickets bought on/after Jan 1, 2016. (2024 booklet Line 8; enacting act (2016) number **UNVERIFIED**.) All other gambling and out-of-state lottery winnings taxable throughout.
- **IRC §125 cafeteria plans:** employee pre-tax contributions under a *federally qualified* §125 plan for **hospitalization, sickness, disability, or death coverage, supplemental unemployment, or strike benefits** are excluded from PA compensation to the extent federally excluded; contributions under non-qualified plans are PA-taxable (PIT Guide, Gross Compensation; DOR FAQ). Practical effect: W-2 Box 16 ≈ Box 1 + elective deferrals (401(k) etc.) with health-premium exclusions matching federal.
- **Elective deferrals taxable:** 401(k)/403(b)/457/TSP employee contributions are PA compensation when made (see §2). This is the single largest wage-base difference from federal on PUF data.
- **IRC §129 dependent care assistance:** PA-taxable compensation through TY2022; **excluded from PA compensation beginning TY2023** (Act 34 of 2023, same act as the CDCTC expansion; DOR guidance "Act 34 of 2023 – Dependent Care Plan (Section 129)"). Cross-check secondary: Ballard Spahr 2024 alert.
- **No federal AGI linkage:** PA starts from its own class definitions, not federal AGI; federal above-the-line deductions (IRA, SE tax, alimony paid, etc.) have no PA analogue. Alimony *paid* is not deductible.
- **Depreciation:** no federal bonus depreciation; §179 allowed with PA limits (72 P.S. § 7303(a.3)); straight-line election differences — negligible for PUF-level modeling of Lines 4/6.
- **Estimated tax trigger:** ≥ $9,500 of non-withheld PA-taxable income (2024→2025 booklets; earlier years $8,000+) — not a liability parameter.

## 9. Known differences / data limitations for a PUF-based model

1. **Wage base:** PUF wages (e00200) = federal Box 1, which excludes 401(k)-type deferrals that PA taxes. Deferrals are unobserved on the PUF → PA compensation is understated for deferring workers. (Pre-tax §125 health premiums are excluded by both, so no adjustment needed there.)
2. **Retirement distributions:** model the standard approach — exempt all pension/IRA distributions (e01400/e01500/e01700 etc.); age/1099-R codes and basis for early distributions are unobserved. This overstates the exemption slightly (early distributions above basis are taxable); no reliable proxy. Taxable-vs-gross pension fields don't map to PA's cost-recovery basis.
3. **Interest:** PUF tax-exempt interest (e00400) mixes PA/local-PA (exempt), US (exempt via Sch A), and **other-state munis (taxable)** — the other-state share is unobserved. Options: national ownership shares or treat all as exempt (both flagged assumptions). Taxable interest (e00300) maps directly.
4. **Capital gains:** PUF reports net federal gains (e01000 with $3,000 loss floor, carryovers). PA disallows carryovers, uses its own within-year netting, and excludes principal-residence gain (already excluded from federal too, mostly). Practical rule: PA Line 5 = max(0, federal-style net gain recomputed without carryover); carryover component unobservable → use net gain, floor at 0. No preferential rate — just include in base.
5. **No-cross-class / no-spousal-offset:** PUF gives netted schedule totals per return, not per class per spouse. Floor each mapped class (Sch C, Sch D, Sch E, farm) at 0 separately to approximate the no-offset rule; spousal split is unobservable (secondary-earner wage split helps only for compensation). This understates PA base slightly relative to true per-spouse flooring.
6. **Schedule O deductions unobserved:** 529 and ABLE contributions do not exist on the PUF; HSA deduction (e03270-style field) and, for 2025, student loan interest (e03210) exist on federal returns and can proxy the PA deduction. 529 omission overstates PA tax modestly (statewide 529 deductions are small relative to base).
7. **Tax Forgiveness eligibility income proxy:** observable adds: alimony received (e00800), tax-exempt interest (e00400), part of excluded residence gain (unobserved), nontaxable military pay (unobserved). **Unobservable:** gifts, insurance proceeds, inheritances, outside-household support, foster payments, educational assistance. Excluding these **overstates forgiveness** (more units look eligible than are). Importantly: **do NOT add Social Security (e02400), unemployment comp (e02300), or (modeled-as-exempt) retirement distributions** to eligibility income. Dependent count: use CTC-style dependents (federal-claimable children incl. adult children — n_dep proxies).
8. **Married rules:** joint eligibility income for both MFJ and MFS — with tax-unit data, treat all married units with Table 2 on joint income; the MFS wrinkles are immaterial.
9. **CDCTC base:** compute the federal §21 tentative credit (2441 Line 9a — before federal liability cap) inside the federal model and apply 30%/100%. Using the *limited* federal credit would understate the PA credit for low-liability filers.
10. **WPTC (TY2025):** 10% × federal EITC as computed — clean to model; EITC take-up assumptions carry through.
11. **Local EIT / Philadelphia wage tax:** out of scope; note PA-40 liability alone understates total PA income-tax burden by roughly 1–4% of wages depending on locality.
12. **$33 filing threshold:** effectively universal filing for any positive income — model everyone with PA-source-class income.

## 10. Cross-model notes

Harness context: **TAXSIM covers PA 2017–2020** in our cross-validation window; **PolicyEngine covers 2021+**.

### TAXSIM (NBER)

- TAXSIM computes PA liability from its 21/27/35-variable input; the PA page at `taxsim.nber.org/state-tax-rates/r/pa.html` returned HTTP 403 in this pass. **UNVERIFIED whether TAXSIM implements Tax Forgiveness (§7304)** — its flat-rate-state handling sometimes omits state-specific low-income provisions. Recommended probe before trusting 2017–2020 comparisons at the bottom of the distribution: single, 0 deps, wages $6,000 (expect $0 net PA tax with forgiveness vs ≈$184 without); wages $8,000 (expect 30%-forgiveness step ≈ $172 vs $246); married 2 deps, wages $31,000 (expect 100% forgiveness ≈ $0 vs ≈ $952).
- The PA CDCTC (2022+) and WPTC (2025) postdate our TAXSIM window — no TAXSIM concern.
- TAXSIM's pension/UI inputs default to state-exempt treatment for PA-type states; its handling of 401(k) deferrals (not an input) matches our PUF limitation (both understate PA wages).

### PolicyEngine-US (2021+)

Verified from the GitHub repo (`policyengine_us/parameters/gov/states/pa/tax/income/`), secondary source:

- `rate.yaml` (3.07%), `nontaxable_income_sources.yaml`, `nontaxable_retirement_distribution_sources.yaml`, `retirement_age_threshold.yaml` (59½ IRA rule), `deductions/plan_529/`, `forgiveness/` (Tax Forgiveness modeled), `credits/cdcc/match.yaml` (**0.30 from 2022-01-01, 1.00 from 2023-01-01** — matches primary sources), `credits/eitc/match.yaml` (**0.10 from 2025-01-01**, "Working Pennsylvanians Tax Credit," ref. HB 416 Article XVI-W.2 § 1603-W.2(c)), `credits/refundable.yaml`.
- So PolicyEngine models: flat rate, exemptions for SS/UC/retirement, 529 deduction, **Tax Forgiveness**, **CDCTC enhancement**, and the **2025 WPTC**. Note PolicyEngine dates the WPTC 2025-01-01, consistent with the "applies to 2025 returns" reporting (our reading: TY2025). Divergences to expect vs our model: PolicyEngine's eligibility-income construction and per-class loss flooring details; check its treatment of other-state muni interest (likely exempts all tax-exempt interest).

### Bottom line for the harness

For PA, the three headline validation-sensitive features are (1) the 401(k)-deferral wage-base gap (all models share it on PUF-type data), (2) Tax Forgiveness at low incomes (TAXSIM status unverified; PolicyEngine models it), and (3) the 2022→2023 CDCTC jump and 2025 WPTC (PolicyEngine has both; TAXSIM window ends before both).

---

## UNVERIFIED items (summary)

1. Pre-2004 rate history / Act 46 of 2003 (background only).
2. Explicit "refundable" wording for the **2022** CDCTC in a 2022-primary document (structure + DOR statements imply refundable; 2023+ explicit).
3. Act numbers: the 2016 PA Lottery taxation act; the 2025 student-loan-interest deduction act; the WPTC act number (2025–26 budget tax code bill; PolicyEngine cites HB 416, Article XVI-W.2).
4. Whether TAXSIM implements PA Tax Forgiveness (probe recommended).
5. Any de-minimis floor on gifts in SP eligibility income (none found in 2017–2025 instructions; instructions say total amount — the "floor" premise appears incorrect for this window).
6. WPTC first-year ambiguity: DOR says "beginning with the 2026 tax season"; Spotlight PA quotes the Governor that it applies to **2025 returns**; PolicyEngine uses 2025-01-01. Modeled as TY2025. Watch for the updated 2025 PA-40 forms revision.
