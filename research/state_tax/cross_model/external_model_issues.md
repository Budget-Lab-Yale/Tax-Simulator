---
title: "Potential issues in TAXSIM-35 and PolicyEngine US"
role: evidence
workstream: state_tax
status: current
updated: 2026-09-30
sot: research/state_tax/state_parameter_rollout.csv
supersedes: []
superseded_by: null
---

# Potential issues in TAXSIM-35 and PolicyEngine US

Findings from the Tax-Simulator state cross-model validation harness
(Budget Lab at Yale, 2026-07-18) that may be worth reporting upstream —
either genuine errors, or intended behavior worth a documentation
clarification. Each item was verified at the record level against state
forms/statutes; reproduction details are available from the harness
(`research/state_tax/cross_model/`, per-record output on request).

Versions tested: TAXSIM-35 as bundled in `usincometaxes` 0.7.1 (local WASM
build); `policyengine-us` 1.775.7.

**Submitting the TAXSIM items:** `research/state_tax/cross_model/taxsim_bug_reports.do` (this directory)
operationalizes the NBER bug-reporting protocol (one-observation exemplar,
`taxsimid = -1`, `idtl = 5`, emailed with a statement of what is wrong) for
the probe-verified TAXSIM issues T6–T10 and T12–T14. It writes, per issue, a
web-tool-ready input CSV, TAXSIM's live response via the `taxsim35` ado, and
the statement text, under `bug_reports/`. Every response was confirmed to
reproduce its bug on 2026-08-15 (e.g. T8 `v34` = 1,550; T10 `v35` = 10,000;
T12 `v32` = −1,999.99; T14 `v32` = 50,000 on 55,000 of federal AGI). The
email itself stays manual: one issue per message to feenberg@nber.org.

## TAXSIM-35

### T1. Illinois exemption disallowance above the AGI threshold not modeled

IL denies the personal/dependent exemption allowance entirely when federal
AGI exceeds $250,000 (single/HoH/MFS) or $500,000 (MFJ) — 35 ILCS 5/204(g),
in force since 2017. TAXSIM grants the exemption regardless: in our 2017–
2020 samples, 98% of exemption-stage mismatches sit above the threshold,
with `v33_state_exemption_amount` equal to the full allowance (multiples of
$2,275 in 2019). Effect: TAXSIM understates IL tax by ~$113 per exemption
(2019) for high-AGI filers.

### T2. `staxbc` (state tax before credits) unpopulated for some states

For IL, `staxbc` returns 0 while `siitax` is positive and correct
(verified on records with $4–10k of IL taxable income and no credits).
If `staxbc` is not meant to be populated for flat-tax states, a
documentation note would help; we initially misread this as a liability
discrepancy.

### T3. New Hampshire I&D rate stale in 2021+ vintage (known limitation,
instance report)

NBER documents that 2021+ state law is inflated prior law; a concrete
instance: for tax year 2023 TAXSIM applies the 5% Hall-type rate on a
deflated base, while the enacted NH rate for 2023 was 4% (RSA 77:1, per the
2021 phase-down). Listed for completeness since users of recent-year state
results may not appreciate the size of such gaps. (TAXSIM's 2017–2020 NH/TN
Hall-tax coding is exact in our tests.)

### T4. Washington capital-gains excise absent (coverage note)

`siitax = 0` for WA in all years; the LTCG excise (RCW 82.87, effective
2022) and the Working Families Tax Credit (2023+) are not modeled. Possibly
out of scope by design (excise, not income tax) — a documentation note
would remove ambiguity.

### T5. Ohio Business Income Deduction not modeled

The OH IT BUS deduction (R.C. 5747.01(A)(31)) — first $250,000 ($125,000
MFS) of business income deducted, excess taxed at a flat 3% — is absent:
TAXSIM taxes business income at regular schedule rates. In our 2017–2020
samples, 74% of federally aligned OH mismatches have BID income, and for
57% TAXSIM's `v32_state_agi` exceeds ours by exactly the BID amount. The
resulting overstatement is large for pass-through owners (median $4.5k on
affected records, unbounded in the tail). Given the BID's size (Ohio's
largest income-tax expenditure), a coverage note would help users.

### T6. Michigan: home-heating credit granted on a collapsed household-income base

For ~370–410 MI records per year (2017–2020), `v30_state_household_income`
returns exactly $1.01 and TAXSIM nets a flat refundable credit into
`siitax` — the MI-1040CR-7 home heating credit standard-allowance ladder at
90% of the allowance ($349/$351/$386/$418 for one exemption in 2017–2020,
larger-household steps above). The $1.01 base looks like a sentinel or
underflow: it appears on records with multi-million-dollar AGI, which then
receive the full credit, and zero-income records return a −$386 "liability."
Two distinct concerns: (a) the household-income computation is wrong on
these records, and (b) the home heating credit is an energy-assistance
transfer paid outside MI-1040 liability, so netting it into `siitax` mixes
concepts (same class as P1/P2 below). Stage decomposition confirms AGI,
exemptions, and taxable income agree exactly on affected records; the
entire wedge is the credit.

Input limitations we worked around (not errors, but they bound what state
validation TAXSIM can support): no tax-exempt interest input (state
exempt-interest addbacks and the federal EITC investment-income test can
never fire), and no state-refund input (state own-refund subtractions
cannot be represented).

### T20. Minnesota 2019–2020: head-of-household filers get the single standard deduction

Minnesota adopted the federal standard deduction amounts from tax year 2019 (M1 booklet, Standard Deduction
Table): $18,350 for head of household in 2019 and $18,650 in 2020. TAXSIM-35 gives Minnesota heads of household
the **single** amount instead, $12,200 and $12,400. One-observation probe (2026-09-30): `mstat` single,
one dependent aged 8, $40,000 of wages, Minnesota:

| year | federal deduction implied by v18 | v34 (Minnesota standard deduction) | Minnesota form |
|---|---|---|---|
| 2018 | 18,000 | 9,550 | 9,550 |
| 2019 | 18,350 | **12,200** | 18,350 |
| 2020 | 18,650 | **12,400** | 18,650 |

TAXSIM infers head of household federally from the same input, so this is the Minnesota schedule picking up
the single column. It is Minnesota-specific: Arizona, Maine and DC also give heads of household the federal
amount in 2019, and TAXSIM matches them. Effect: TAXSIM overstates Minnesota tax by $329–$490 for every head
of household in 2019–2020; in our sample those records match at 0.28–0.31, against 0.74 in 2017–2018.
Excluded in the harness (`known_differences.csv`, MN taxsim 2019–2020, `filing_status == 4`).

### T21. Minnesota 2019–2020: the high-income limitation is not applied to the standard deduction

From tax year 2019 Minnesota reduces the standard deduction, as well as itemized deductions, by 3% of AGI above
a threshold ($194,650 in 2019, $197,850 in 2020; half for married filing separately), by at most 80% of the
deduction (Minn. Stat. 290.0123 subd. 1: the amount "is reduced in accordance with subdivision 5"; subd. 5 new
in Laws 2019 ch. 6 art. 1 s. 17; the Line 4 worksheet in each booklet). TAXSIM-35 gives the full deduction.
One-observation probe (2026-09-30): joint filer, $600,000 of wages, Minnesota: v36 = $575,600 in 2019 and
$575,200 in 2020, exactly AGI minus the full $24,400 / $24,800. The form reduces the deduction by 80%, to
$4,880 / $4,960. Effect: TAXSIM understates Minnesota tax for high-income standard-deduction takers by up to
~$1,900. In our sample those records match at 0.03–0.06. Excluded where the missing reduction exceeds $100.

### T22. Minnesota Working Family Credit: two-or-more-child non-joint filers phased out from the one-child threshold

Minn. Stat. 290.0671 subd. 1 (2017 and 2018 editions) sets separate phase-out starts by number of children: a base
$21,190 for one qualifying child and $25,130 for two or more, indexed under subd. 7 ($22,230 and $26,360 in 2018).
TAXSIM-35 phases out single and head-of-household filers with two or more children from the one-child threshold.
Sweep (2026-09-30), 2018, head of household, two children, earned income = AGI:

| earned | form | TAXSIM v39 |
|---|---|---|
| 22,000 | 2,104.00 | 2,104.00 |
| 24,000 | 2,104.00 | 1,912.48 |
| 28,000 | 1,926.55 | 1,479.68 |
| 34,000 | 1,277.35 | 830.48 |

From $28,000 the shortfall is constant at $446.87 = 10.82% × ($26,360 − $22,230). The same holds in 2017 ($438.21), 2019
($444.15) and 2020; joint filers are right in every year. Effect: TAXSIM understates the credit, and so overstates
Minnesota tax, by up to ~$448 for non-joint families with two or more children in the phase-out range.

### T23. Minnesota 2019–2020: the childless Working Family Credit paid at age 65 and over

From tax year 2019 Minnesota extends the childless Working Family Credit to filers who have "attained the age of 21,
but not attained age 65 before the close of the taxable year" (Minn. Stat. 290.0671 subd. 1(a)(1), as amended by
Laws 2017 1Sp ch. 1 art. 1 s. 20, effective for tax years after December 31, 2018). TAXSIM-35 pays it to childless
filers aged 65 and over in 2019–2020 (77 and 76 records in our samples, median $108), where the federal EITC is zero
on every one. In 2017–2018 TAXSIM ties the credit to federal eligibility and is right. Effect: TAXSIM understates
Minnesota tax for low-earning childless seniors by up to the childless maximum ($279 / $284).

### T32. Minnesota 2019–2020: the Working Family Credit ignores the EITC investment-income limit

Minnesota's Working Family Credit requires eligibility for the federal EITC (Minn. Stat. 290.0671 subd. 1(a)), and
that includes the IRC 32(i) limit on investment income. The 2019 Schedule M1WFC line 1 instructions send the filer
through Steps 1–5 of the federal EIC instructions and waive only the AGI and earned-income limits (Steps 1, 4 and
5), not Step 2, the investment-income test. The 2020 instructions print the limit itself ($3,650). TAXSIM-35 pays
the credit regardless. Probe (2026-10-07), single, age 40, no children, $6,000 of wages:

| year | dividends | v25 (federal EITC) | v39 (MN WFC) |
|---|---|---|---|
| 2018 | 5,000 | 0 | **0** |
| 2019 | 5,000 | 0 | **188.60** |
| 2020 | 5,000 | 0 | **191.40** |

As with T23, TAXSIM tied the credit to federal eligibility in 2017–2018 and decoupled it from 2019. Effect: TAXSIM
understates Minnesota tax by up to the family's credit for filers over the investment-income limit.

### T33. Minnesota marriage credit: business income pooled across the spouses

Minnesota's marriage credit (Minn. Stat. 290.0675, Schedule M1MA) is keyed to the lesser-earning spouse's own
income: wages, that spouse's self-employment income from their own Schedule SE (line 2), taxable pensions and
taxable Social Security. TAXSIM-35 takes self-employment income per spouse (`pbusinc`/`pprofinc` and
`sbusinc`/`sprofinc`) but treats it as the couple's jointly for this credit. Probe (2026-10-07), joint, both 45:

| year | input | v40 (credits) |
|---|---|---|
| 2017 | $100,000 primary business income, spouse nothing | 230.10 |
| 2017 | $50,000 / $50,000 wages | 230.10 |
| 2017 | $100,000 primary wages, spouse nothing | 0.00 |
| 2017 | $486,691 primary business income, spouse nothing | 1,430.91 (the maximum) |
| 2019 | $100,000 spouse business income, primary nothing | 206.92 |

The credit for one spouse's business income equals the credit for an even wage split. Effect: TAXSIM understates
Minnesota tax for couples where one spouse has the business income, by up to the credit maximum ($1,433 in 2017,
$1,533 in 2020).

### T34. Nebraska low-income child care credit capped at federal tax

Neb. Rev. Stat. 77-2715.07(2): at or below $29,000 of federal AGI the credit is a refundable percentage of the federal
credit allowable "whether or not the federal credit was limited by the federal tax liability", and Form 2441N recomputes
it from expenses (capped expenses x federal decimal x state decimal; the state decimal is 1.00 at or below $22,000).
TAXSIM-35 caps it at the filer's federal tax whenever there is some. Probe (2026-10-08), head of household, one child
aged 4, $2,080 of care:

| year | wages | v38 | Form 2441N | federal tax before credits |
|---|---|---|---|---|
| 2018 | 21,000 | 300.00 | 665.60 | 300 |
| 2019 | 21,000 | 265.00 | 665.60 | 265 |
| 2020 | 21,000 | 235.00 | 665.60 | 235 |
| 2018-2020 | 15,000 | 728.00 | 728.00 | 0 |
| 2017 | 9,125 | 79.32 | 728.00 | 0 |

With no federal tax TAXSIM pays the full credit (2018-2020), so the cap is applied only where it binds partway; 2017 has
its own error at zero tax. Effect: TAXSIM understates Nebraska refunds for low-income families with care costs.

### T35. North Dakota marriage penalty credit ignores retirement income

Form ND-1 instructions define each spouse's qualified income as wages, net self-employment income and the "Taxable
portion of IRAs, pensions, annuities, and social security benefits reported on Form 1040 or 1040-SR, lines 4b, 5b, and
6b", less the North Dakota Social Security subtraction. TAXSIM-35 counts wages only. Probe (2026-10-08), 2019 joint,
both 60: wages $60,000 / $50,000 -> `v40` 106.69; wages $60,000 plus $100,000 of pensions -> 0.00, the same as wages
alone. Effect: TAXSIM understates the credit for couples whose lesser qualified income is retirement income.

### T36. Connecticut: the pension and Social Security subtractions are phased out, not cut off

Connecticut's 14% (TY2019) / 28% (TY2020) pension and annuity subtraction is allowed only to filers "with federal AGI of
less than $100,000" (joint) or "less than $75,000" (single), and its Social Security Benefit Adjustment Worksheet switches
to the 25%-taxable rule at the same amounts (2019 CT-1040 booklet, Line 48b and Line 41). TAXSIM-35 phases both out
linearly from the threshold to twice the threshold. Probes (2026-10-08), both filers aged 68/66:

| filer | pensions | AGI | TAXSIM subtraction | share of 14% |
|---|---|---|---|---|
| joint | 80,000 | 95,000 | 11,200 | 1.00 |
| joint | 80,000 | 105,000 | 10,640 | 0.95 |
| joint | 80,000 | 150,000 | 5,600 | 0.50 |
| single | 60,000 | 70,000 / 100,000 / 140,000 / 160,000 | 8,400 / 5,600 / 1,120 / 0 | 1.00 / 0.67 / 0.13 / 0 |

Effect: TAXSIM understates Connecticut tax for retirees just above the thresholds by up to the full subtraction times
the rate, and overstates nothing. PolicyEngine applies the cliff.

### T37. Iowa 2020: the recovery rebate credit is netted out of the federal tax deduction

Iowa deducts federal income tax paid. The 2020 IA 1040 Expanded Instructions ("New for 2020") say "Economic impact
payments (EIPs) whether in the form of a rebate or refundable tax credit will not be included in Iowa taxable income, or
added back as part of your reportable federal income tax refund", and the Step 7 list of refunds not to report begins
with "Economic Impact Payment (stimulus payment)". TAXSIM-35 nets the TY2020 recovery rebate credit ($1,800 per adult,
the two 2020 payments) out of the federal tax it lets Iowa deduct. On our TY2020 comparison the Iowa taxable income
TAXSIM computes exceeds ours by exactly $1,800 on 848 single and 31 separate returns and by $3,600 on 265 joint returns;
TY2019 shows no such mode. Effect: TAXSIM overstates 2020 Iowa tax by the rebate times the Iowa rate ($100-$300), on
nearly every return.

### T38. Iowa pension exclusion applied only at 65 and over

Iowa Code 422.7(31) excludes up to $6,000 ($12,000 joint) of pension and retirement income for taxpayers "who are 55
years of age or older", disabled, or surviving spouses. TAXSIM-35 applies it at 65. Probe (2026-10-08), single, $10,000
of wages and $20,000 of pensions: `v32` equals federal AGI at ages 50 and 57 and is $6,000 lower at 66, in 2017 and
2019 alike. Same pattern as T29 (Arkansas). Effect: TAXSIM overstates Iowa tax for 55-64-year-old retirees by up to
$1,024.

### T39. Iowa earned income credit cut off at fixed income points

Iowa's earned income credit is 15% of the federal credit for anyone who qualifies federally (IA 1040 Expanded Instructions
2020, line 61: "multiply your federal EITC by 15% (.15)"; Iowa Code 422.12B), with no state income limit. TAXSIM-35 pays
it only up to fixed amounts of income. Probes (2026-10-09), 2019:

| family | last wages with v39 > 0 | first wages with v39 = 0 | federal EITC there |
|---|---|---|---|
| joint, 3 children | 40,000 | 41,000 | 3,149 |
| head of household, 2 children | 40,000 | 41,000 | 1,201 |
| joint, 1 child | 35,000 | 36,000 | 1,739 |
| single, none | 13,000 | 14,000 | 120 |

2018 shows the same cutoff. Effect: TAXSIM understates Iowa refunds for families in the upper part of the EITC phase-out
by up to 15% of their federal credit.


### T40. Maryland 2020 childless earned income credit dropped above about $10,000

The RELIEF Act (Laws of Maryland 2021 ch. 39) made Maryland's credit for filers without a qualifying child 100% of
the federal credit, capped at $530 and refundable, from tax year 2020 (Worksheet 18A.1). TAXSIM-35 applies it only at
low incomes. Probe (2026-10-09), 2020 single aged 30, wages only: at $8,000-$10,000 `v39_state_eitc` equals the
federal credit (530 / 530 / 521.93 / 483.68 / 445.43) and siitax goes negative; from $10,500 up it falls back to 50%
(203.59 at $10,500, 107.97 at $13,000) and siitax is 0.00 and 212.41, where the form gives about -$188 and $104. The
switch does not follow the poverty-credit guideline ($12,760) or the point where the credit stops exceeding the tax. In
the 2020 clean subset, the 75 childless non-joint records above $10,000 with a state credit worth over $100 all miss.
Effect: TAXSIM overstates 2020 Maryland tax for childless filers by up to half the federal credit.

### T41. Maryland extra exemption for dependents 65 or over not applied

Form 502B gives each dependent aged 65 or over a second exemption, valued like any dependent exemption under the
federal-AGI banding ($3,200 below $100,000 single / $150,000 joint). TAXSIM-35 ignores the dependent's age. Probe
(2026-10-09), 2019 head of household, $50,000 of wages, one dependent aged 10, 40, 70 or 80: `v33_state_exemption_amount`
6,400 and siitax 1,871.25 in every case, where the form gives 9,600 at ages 70 and 80. Effect: TAXSIM overstates
Maryland tax by $3,200 times the marginal rate (about $152) per aged dependent.

### T42. Iowa self-employment tax counted twice against the filer

IA 1040 subtracts the deductible part of self-employment tax in net income (line 17, as federal AGI does), and its
federal tax deduction covers federal tax paid, with self-employment tax added back on line 28 because the payments
include it; net, the deduction is federal income tax. TAXSIM-35 instead (a) leaves the deductible half of
self-employment tax in Iowa net income and (b) deducts federal income tax LESS the whole self-employment tax. Probe
(2026-10-09), single aged 40: $80,000 of wages gives v32 80,000 and siitax 3,963.82; $80,000 of self-employment income
gives federal AGI 74,348.18 but v32 80,000.01, and v36 77,577.85 = 80,000 - 2,000 - (fiitax 11,725.80 - SE tax
11,303.65), siitax 5,042.98. Same in 2020. Effect: TAXSIM overstates Iowa tax for self-employed filers by roughly the
Iowa rate times one and a half times their self-employment tax.

### T43. Iowa single-filer low-income threshold at 65 or older not applied

Form 41-146 (Iowa Income Tax Reduction Worksheet) caps a single filer's tax at income (net income plus the pension
exclusion plus reportable Social Security) less $9,000, or less $24,000 if 65 or older. TAXSIM-35 applies the $9,000
threshold at every age. Probe (2026-10-09), single, pensions only: at age 66, $20,000 of pensions (test income
$20,000 < $24,000) gives siitax 254.07 in 2017 and 247.99 in 2020, where the form gives zero; at age 40, $8,500 gives
zero as it should. Effect: TAXSIM overstates Iowa tax for single filers 65 or older with test income between $9,000 and
$24,000.
### T24. New Jersey: non-property income (`nonprop`) left out of state AGI

TAXSIM-35 keeps `nonprop` in federal AGI but drops it from New Jersey gross income. New Jersey taxes the income it
carries: alimony and separate maintenance received (NJ-1040 line 24) and other income (line 25). Probe (2026-10-01),
2019, single, $60,000 of wages:

| input | federal AGI | v32 (state AGI) |
|---|---|---|
| wages only | 60,000 | 60,000 |
| + $1,900 `nonprop`, NJ | 61,900 | **60,000** |
| + $1,900 `nonprop`, NY (control) | 61,900 | 61,900 |
| + $1,900 `otherprop`, NJ | 61,900 | 61,900 |

On our New Jersey misses the state-AGI gap equals the record's positive other income to the dollar. Effect: TAXSIM
understates New Jersey tax by that income times the marginal rate (1.4–10.75%).

### T25. New Jersey: net capital losses reduce state income

New Jersey's "net gains or net income from the disposition of property" is a single category floored at zero; a net
loss offsets nothing else. TAXSIM-35 reduces New Jersey state AGI by the net capital loss less the federal $3,000.
Probe (2026-10-01), 2019 single, $60,000 of wages:

| capital gains input | federal AGI | v32 | New Jersey law |
|---|---|---|---|
| none | 60,000 | 60,000 | 60,000 |
| long-term −50,000 | 57,000 | **13,000** | 60,000 |
| long-term −50,000, short-term +10,000 | 57,000 | **23,000** | 60,000 |
| long-term +20,000, short-term −30,000 | 57,000 | **53,000** | 60,000 |
| long-term −50,000, **Pennsylvania** (control) | 57,000 | 60,000 | 60,000 |

TAXSIM floors Pennsylvania's equivalent class correctly. Effect: TAXSIM understates New Jersey tax for anyone with a
net capital loss above $3,000, without limit; on our largest misses its New Jersey AGI reaches −$24 million.

### T26. New Jersey child and dependent care credit not modeled (2018–2020)

New Jersey introduced a child and dependent care credit for tax year 2018: a share of the federal credit stepped by
New Jersey taxable income (50% at $20,000 or less down to 10% at $60,000, none above), capped at $500 for one
qualifying person or $1,000 for two or more (NJ-1040 Worksheet J). TAXSIM-35 returns `v38_state_child_care_credit` = 0
on every New Jersey record in 2018–2020. Effect: TAXSIM overstates New Jersey tax for working families with care
expenses, by up to $1,000. (Coverage note, like T4/T5.)

### T27. New Jersey pension exclusion: age 65 instead of 62, and the $100,000 cliff tested on federal AGI

New Jersey's pension exclusion (NJ-1040 line 27a in TY2017, line 28a later) is open to filers "62 or older" (or
disabled) whose "income on line 27", New Jersey total income, which excludes Social Security, is $100,000 or less
(TY2017-2020 cliff; TY2017 and TY2019 booklets). TAXSIM-35 uses age 65 and tests federal AGI. Probe (2026-10-01),
2019 single:

| case | federal AGI | v32 | form |
|---|---|---|---|
| age 61, $25,000 pension | 25,000 | 25,000 | 25,000 |
| age 62, $25,000 pension | 25,000 | **25,000** | 0 (excluded) |
| age 64, $25,000 pension | 25,000 | **25,000** | 0 |
| age 65, $25,000 pension | 25,000 | 0 | 0 |
| age 66, $70,000 pension + $40,000 Social Security | 104,000 | **70,000** | 70,000 less the exclusion |

Effect: TAXSIM overstates New Jersey tax for retirees aged 62-64, and for older retirees whose Social Security pushes
federal AGI over $100,000 while New Jersey income stays under it.

### T28. New Jersey: the 10.75% bracket is never applied (TY2018-2020)

P.L. 2020, c. 95 (the 2020 "millionaires tax") moved New Jersey's 10.75% bracket floor from $5,000,000 down to
$1,000,000 for tax year 2020 (NJ-1040 Tax Rate Schedules, Tables A and B, TY2020 booklet). TAXSIM-35 uses the 2019
schedule for 2020. Probe (2026-10-01), single, $2,000,000 of wages (NJ taxable income $1,999,000): `siitax` =
164,184.05 in both 2019 and 2020; the TY2020 schedule gives $17,782 more. Effect: TAXSIM understates 2020 New Jersey
tax by 1.78% of taxable income between $1 million and $5 million.

**Wider than first recorded (2026-10-08).** TAXSIM applies no 10.75% bracket in any year: probe, single, $5,000,000 and
$10,000,000 of wages, marginal rate between them 8.97% in 2017, 2018, 2019 and 2020 (`v41` = 8.97). P.L. 2018 c.45 created
the 10.75% bracket above $5,000,000 from TY2018, so TAXSIM also understates 2018-2019 tax by 1.78% of taxable income above
$5 million (record 400580, 2018: $1,538,717 on $91.5 million of AGI).

### T29. Arkansas $6,000 retirement exemption applied only at 65 and over

Arkansas exempts the first $6,000 of retirement benefits per taxpayer (Ark. Code Ann. 26-51-307). For employer-related
plans there is no age test (AR1000F instructions: "The recipient does not have to be retired"); traditional IRA
distributions qualify from age 59 1/2. TAXSIM-35 applies the exemption only at 65 and over. Probe (2026-10-01), 2019
single, $20,000 of wages and $20,000 of pension: v32 = 40,000 at ages 59, 60, 62 and 64, and 34,000 at 65 and 66.
Effect: TAXSIM overstates Arkansas tax for retirees under 65 by up to $6,000 x the rate per taxpayer.

### T44. Kansas food sales tax credit not paid on the age prong

K.S.A. 79-32,271 and the K-40 booklets (2018 and 2020, "Food Sales Tax Credit", lines A-C) grant the $125-per-qualified-
exemption credit to a full-year resident with federal AGI of $30,615 or less who answers YES to any of three questions:
55 or older all year, blind or disabled, or a dependent child under 18 living in the home. TAXSIM-35 pays nothing to
filers who qualify through age alone: in the 2026-10-13 re-score, 114 single and 18 joint clean 2019 records aged 55-63
with no dependents and AGI between $10,000 and $26,000 carry our $125 or $250 against TAXSIM's zero. Effect: TAXSIM
overstates Kansas tax by $125 per taxpayer or spouse for childless filers aged 55-64 under the AGI limit, 2017-2024.

### T45. Kansas food sales tax credit applied before the EITC

The K-40 (2018, 2020 and 2022 booklets, lines 16-19) takes the earned income tax credit first -- line 17, limited to the
line 16 tax balance, with the remainder refunded on line 25 -- then the food sales tax credit on line 18, and line 19
"cannot be less than zero". A refundable EITC that already covers the tax therefore leaves nothing for the nonrefundable
food credit. TAXSIM-35 nets the food credit against tax before the EITC: head-of-household EITC claimants with a child
(2019 records 27516, 66438) show our liability $249-250 above TAXSIM's, the credit TAXSIM applied to tax the EITC had
absorbed. Effect: TAXSIM understates Kansas tax by up to $125 per qualified exemption for low-income EITC claimants.

### T46. Kentucky combined returns: the standard deduction of a no-income spouse

Form 740 computes a combined return in two columns and floors each column at zero, so a spouse whose column income
is below the standard deduction ($2,590 in 2019) cannot use the rest of it. TAXSIM-35 grants both spouses' standard
deductions unconditionally (WASM probe 2026-08-11: a $70,000 one-earner couple returns the same siitax as a
$40,000/$30,000 couple). The 2026-10-13 re-score puts the class at 541 of 1,368 clean TY2019 mismatches (514 joint
returns, 81% with spouse earned income under the deduction), a flat 5% x $2,590 = $129.50 of liability. Effect:
TAXSIM understates Kentucky tax by up to the rate times the standard deduction for every one-earner married
return, 2017-2024. (Previously annotated as a sub-$100 item; promoted once measured.)

### T47. Maine dependent exemption tax credit only for child tax credit children

36 M.R.S. 5219-SS gives $300 for each dependent for whom the federal child tax credit OR the credit for other dependents
is claimed (1040ME line 13a, TY2018+). TAXSIM-35 counts only CTC children: in the 2026-10-13 re-score 335 clean TY2019
records sit at -$300 and 63 at -$600 (ours lower), e.g. a joint return with three dependents, two aged 18+, gets TAXSIM
credits of $300 against our $900. Effect: TAXSIM overstates Maine tax by $300 per 17-year-old, adult student or
dependent parent, 2018-2020.

### T48. Maine dependent exemption tax credit not phased out

The credit phases out by $7.50 per $1,000 of Maine AGI over $200,000 ($400,000 joint), gone $40,000 above the
threshold. TAXSIM-35 pays it in full at any income: joint returns with $1.6M-$5.9M of AGI carry TAXSIM credits of
exactly $300 per dependent. Effect: TAXSIM understates Maine tax by $300 per dependent above the phase-out.

### T49. Vermont: an unallocated flat $111 credit

TAXSIM-35 reports a Vermont credit of exactly $111.00 inside v40 (total state credits) that appears in none of the
component columns v37 (property tax), v38 (child care) or v39 (EITC). A synthetic probe (2026-10-14,
`output/probe/vt_111_probe.R`) finds it in every year 2017-2020, identical for single and joint filers, unaffected by
rent paid, paid at wages up to $22,500 (under 65) or $27,500 (65 or older) and zero above. No Vermont credit on Form
IN-111 or in Title 32 has that shape; the renter rebate and the property tax adjustment are separate claims keyed on
rent and property tax, which do not move it. It carried roughly 63% of the clean Vermont TAXSIM mismatches. Effect:
TAXSIM understates Vermont tax by $111 for low-income returns.

### T50. Vermont charitable contribution credit not modeled

Act 11 of the 2018 special session (32 V.S.A. 5822(d)) gives a nonrefundable credit of 5% of the first $20,000 of
charitable contributions, up to $1,000, from TY2018. TAXSIM-35 does not compute it: in the 2026-10-14 re-score
2,197-2,361 clean records a year carry the credit on our side and 84-85% of them differ from TAXSIM by exactly that
amount. Effect: TAXSIM overstates Vermont tax by up to $1,000 for donors, 2018 onward.

### T51. Vermont capital gains exclusion applied to all gains

Vermont's exclusion (32 V.S.A. 5811(21)(B)(ii); Schedule IN-153) is the larger of a flat $5,000 and 40% of gain on
assets held over three years that are not publicly traded securities, real estate or depreciable property (capped at
$350,000), never more than 40% of federal taxable income. TAXSIM-35 applies the 40% branch to all net capital gains,
over-excluding securities gains; records with net gains above $12,500 match at 0.17-0.38 in 2017-2020. Effect:
TAXSIM understates Vermont tax on securities gains. (The qualifying subset is unobservable in the PUF either way; we
and PolicyEngine apply the flat branch.)


### T52. West Virginia Family Tax Credit income measured after the senior modification

Schedule FTC-1 lines 1-3 measure income as federal AGI plus the increasing modifications plus tax-exempt interest
(11-21-22a(d)); no decreasing modification is subtracted. TAXSIM appears to test the poverty-guideline table on income
after the $8,000 senior citizen modification: single filers 65+ with $15,000-$17,000 of federal AGI (120-135% of the
guideline, a 0-30% credit on the form) receive 100% forgiveness (v40 equal to the whole pre-credit tax). Effect:
TAXSIM understates West Virginia tax for low-income seniors by up to the full liability ($110-$280 in the 2026-10-14
re-score; 73 TY2019 records).

### T53. West Virginia senior citizen modification not capped at the spouse's own income

Schedule M line 47 box (c) limits each qualifying person's $8,000 to that person's own income ("ONLY THE INCOME OF THE
SPOUSE WHO MEETS THE ELIGIBILITY REQUIREMENTS QUALIFIES"). TAXSIM grants $8,000 to every spouse aged 65+ regardless
(record 101833, TY2020: the 69-year-old primary earns $19,378, the 70-year-old spouse has no income; TAXSIM subtracts
$16,000, the form $8,000). Effect: TAXSIM understates tax for couples where a 65+ spouse has little own income.


### T54. Hawaii EITC limited by refundable credits applied first

TAXSIM applies Hawaii's refundable credits (child and dependent care, food/excise) against the tax before the
nonrefundable EITC (HRS 235-55.75; Form N-356), so the EITC is capped at whatever tax they leave. On Form N-11
nonrefundable credits reduce the tax first and refundable credits are payments. Probe (HoH, two children, TY2019):
`v39_state_eitc` falls 294 -> 0 -> 0 at $20,000 of wages as care expenses go $0 -> $3,000 -> $6,000, and 704 -> 257 -> 0
at $30,000. Effect: TAXSIM overstates Hawaii tax for low-income working families with care expenses by up to the full
EITC (TY2018-2020; 291-351 clean records a year).


### T55. D.C. childless EITC paid outside the federal age band

TAXSIM grants the D.C. childless-worker EITC (D.C. Code 47-1806.04(f)(1)(C)) to childless filers aged 18-24 (and 65+),
usually at the full maximum ($529 in TY2019). The D.C. credit builds on federal childless eligibility, which requires
age 25-64; PolicyEngine applies that test too. Effect: TAXSIM understates D.C. tax for young childless workers by up to
the childless maximum (171-206 clean records a year, TY2017-2020).


### T56. D.C. childless EITC phased out on AGI instead of the greater of earnings or AGI

The D.C. childless-worker credit is reduced by its phase-out rate times the excess of the greater of earned income or
federal AGI over the threshold (the federal construction; PolicyEngine implements it the same way). TAXSIM uses federal
AGI alone, so a childless worker whose losses pull AGI below earnings keeps a credit the form takes away ($100-$500;
TY2018 records 436084, 297193, 426393).


### T57. Delaware age-60 rules applied from 65

30 Del. C. 1110(b)(2) grants an additional $110 personal credit to each resident 60 or over (PIT-RES Line 27b), and
1106(b)(3) raises the pension exclusion to $12,500 with eligible retirement income at 60 or over (PIT-RES Line 6).
TAXSIM applies both from 65: filers and spouses aged 60-64 get $110 less credit and only the under-60 $2,000 exclusion
(couples aged 60-64 with pensions: a -$21,000 state-AGI gap = 2 x ($12,500 - $2,000)).


### T58. Georgia retirement income exclusion applied only from 65

O.C.G.A. 48-7-27(a)(5) and Form 500 Schedule 1 grant the retirement income exclusion from age 62 ($35,000 at 62-64,
including up to $4,000 of earned income; $65,000 at 65+). TAXSIM applies none of it before 65. Probe (single, $30,000
of wages, TY2019): Georgia AGI 30,000 at ages 61 and 63, 26,000 at 66; with $20,000 of pension, 50,000 / 50,000 /
26,000. Effect: TAXSIM overstates Georgia tax for filers aged 62-64 with wages or retirement income.


### T59. Arizona itemized deduction carries TAXSIM's own state income tax

TAXSIM's Arizona itemized amount (v35) includes its iterated state income tax inside the SALT component -- exactly the
federal $10,000 cap on high-income federal non-itemizers with no property tax -- although Arizona Schedule A subtracts
state income taxes. Concentrated in TY2018 (289 clean records); TAXSIM's deduction used then exceeds the form's by
$1,000 or more.

### T60. Georgia retirement exclusion extended to an under-62 spouse

When either spouse on a joint return is 65 or older, TAXSIM treats both spouses as eligible for Georgia's retirement
income exclusion, including the earned-income portion (up to $4,000 each). O.C.G.A. 48-7-27(a)(5) and Form 500
Schedule 1 test each taxpayer separately: only a taxpayer aged 62 or older may exclude his or her own earned income.
Probe (2026-10-16, TY2019 joint, $30,000 of wages each): $8,000 excluded at ages 70/55, $0 at 55/58, the same in
either spouse order. Effect: TAXSIM understates Georgia tax for couples with one spouse 65+ and a working spouse
under 62.

### T61. Arizona net long-term capital gain subtraction not applied

A.R.S. 43-1022(22) and Form 140 lines 22-24 subtract 25% of net long-term capital gain from assets acquired after
December 31, 2011 (10% TY2013, 20% TY2014, 25% from TY2015). TAXSIM's Arizona AGI carries the whole gain. Probe
(2026-10-20, single, $50,000 of wages, TY2017/2019/2020): v32 = 50,000 with no gain and 90,000 with $40,000 of
`ltcg`, where the form gives 80,000. Effect: TAXSIM overstates Arizona tax by 25% of long-term gain times the
marginal rate (about $450 per $40,000 at 4.5%). PolicyEngine applies the subtraction.

### T62. Oregon EITC young-child rate never applied

ORS 315.266 and OR-17 set Oregon's earned income credit at 11% of the federal credit (12% from TY2020) where a
qualifying dependent is under 3 at the end of the year, against 8% (9%) otherwise. TAXSIM applies the base rate to
everyone. Probe (2026-10-21, TY2017 single, $12,000 of wages, one dependent): v39 = 272.00 with the dependent aged 1
and aged 5, 8% of the federal credit both times. Effect: TAXSIM overstates Oregon tax by 3% of the federal EITC for
families with a child under 3.

### T63. Oregon WFHDC credit ignores the youngest child's age

The Working Family Household and Dependent Care credit percentage (Publication OR-WFHDC-TB Part B) depends on income
as a share of the poverty guideline and on the youngest qualifying individual's age (under 3, 3-5, 6-12, and two
columns for disabled individuals). TAXSIM always uses the last column (disabled, 18 or older). Probe grid
(2026-10-21, head of household, one dependent aged 1 to 10, $4,000 of care, TY2017 and TY2019): the implied rate is
the same at every age and equals that column at every income band -- TY2019 wages $14,000 (row I) give 50% where a
child under 3 earns 70%. The credit is also booked outside v38. Effect: TAXSIM understates the credit by 5 to 45
percentage points of qualifying expenses.

### T64. Montana partial pension, annuity and IRA exemption not applied

15-30-2110(2)(c) MCA exempted up to $4,110 per taxpayer of pension, annuity and IRA income in TY2017 (indexed;
reduced $2 per $1 of federal AGI over the threshold; repealed from TY2024). TAXSIM's Montana AGI carries the whole
pension. Probe (2026-10-21, single, $10,000 of wages, TY2017/2019/2020): v32 = 20,000 with $10,000 of pensions, where
the Form 2 schedule gives 15,890 in TY2017. Effect: TAXSIM overstates Montana tax for retirees by up to the exemption
times the marginal rate.

### T65. South Carolina TY2017 deduction for dependents under 6 not applied

SC1040 line t (12-6-1160) deducts, for each dependent under 6 at year end, the federal personal exemption amount in
TY2017 ($4,050; 2017 instruction packet) and the SC dependent exemption from TY2018. TAXSIM applies it from TY2018
only. Probe (2026-10-21, joint, $60,000 of wages, one dependent aged 3 vs 8): v36 = 35,150 for both in TY2017; the
age-3 return is lower by exactly $4,110 / $4,190 / $4,260 in TY2018-20. Effect: TAXSIM overstates TY2017 South
Carolina tax by $4,050 times the rate for each young child.

### T30. Arkansas AGI ignores nonprop entirely

TAXSIM-35's Arkansas AGI (v32) takes no part of `nonprop`, positive or negative. Arkansas taxes other income
(AR1000F line 22, Form AR-OI) and alimony received (line 19), and allows a net operating loss carryforward as a
line 22 subtraction. Probe (2026-10-01), 2019 single, $30,000 of wages: v32 = 30,000 with `nonprop` +10,000, where
the same amount in `otherprop` gives 40,000; with $60,000 of wages and `nonprop` -20,000, v32 = 60,000 while
federal AGI is 40,000. Effect: TAXSIM understates Arkansas tax for filers with other income or alimony and
overstates it for filers with an NOL.

### T31. Alabama federal income tax deduction counts the NIIT twice

Alabama deducts federal income tax (Form 40 line 12). The worksheet starts from 1040 tax after nonrefundable credits
and, on line 2, adds the net investment income tax from Form 8960, because the NIIT is not in that figure. TAXSIM's
`fiitax` already includes the NIIT, and TAXSIM adds it again. Probe (2026-10-02), 2019 single, no wages: with
$500,000 of long-term gain the deduction implied by v32 - v34 - v33 - v36 is 92,726 against `fiitax` 81,326, a
difference of 11,400 = 3.8% x $300,000; with $2,000,000 it is 509,166 against 440,766 (68,400). Effect: TAXSIM
understates Alabama tax by about 5% of the NIIT.

## PolicyEngine US

> The 2026-09-30 status notes below cite reproductions and drafts under
> `bug_reports/policyengine/`. That folder is deliberately **not committed**
> until the upstream items are sent (JI, 2026-09-30); it lives on the cluster copy.

### P1. Colorado: TABOR refunds netted into `state_income_tax`

`co_income_tax` nets `co_sales_tax_refund` (the TABOR refund claimed on
DR 0104): verified 2022 six-tier refund ($153+ by AGI tier, doubled for
joint), 2023 flat $800/$1,600, 2024 tiers ($177+). On a plain 2022 single
filer with $100k wages, `co_income_tax_before_refundable_credits` =
$3,830.20 (= 4.40% × federal taxable, matching our calculator exactly);
`state_income_tax` = $3,596.20 after the $234 refund. Whether the refund
belongs inside "state income tax" is a concept choice, but (a) it makes
`state_income_tax` diverge from the liability concept most revenue analysis
uses, and (b) `co_tabor_cash_back` ($750, 2022) is simultaneously modeled
as a separate variable, so the 2022 TABOR surplus appears split across two
mechanisms — worth confirming both the split and the intended semantics.
A stable pre-refund state liability output (uniform across states) would
make cross-model comparison much easier.

### P2. Illinois: one-time 2021 rebate netted into 2021 `state_income_tax`

The 2022-enacted IL individual income tax rebate ($50/filer + $100/dep,
capped) is netted into tax year 2021 `state_income_tax` — nearly every
2021 IL record shifts by $50–$400 in round amounts. Same concept question
as P1: one-time rebates inside the recurring liability variable.

### P3. Alaska: Permanent Fund Dividend imputed into federal AGI by default

AK households receive an imputed PFD in federal AGI (verified: constant
+$2,622/record in 2022 vs byte-identical FL households). Defensible as a
default, but it changes *federal* results based on state of residence even
when the user supplies a complete income specification — an off switch or
prominent documentation would help users doing controlled comparisons.

### P4. Ohio Business Income Deduction not modeled

> **Status 2026-09-30:** still absent in policyengine-us 2.18.2 (the $100,000 self-employment case owes $2,147.89; the deduction gives 0). Already tracked upstream as open issue #4056; our evidence is drafted as a comment there (`bug_reports/policyengine/drafts/comments_9630_4056.md`), not a new issue.

Same gap as T5 on the PolicyEngine side, verified in `policyengine-us`
1.775.7 package source: no IT BUS variable or parameter exists under
`gov/states/oh` (modeled deductions are 529, medical, educator expense,
federal conformity, §179 add-back, uniformed-services retirement). Business
income is therefore taxed at regular rates. In our 2021–2024 samples, 104
of 133 federally aligned OH mismatches carry BID income (95 with
PolicyEngine higher, as expected). Flagging because the BID is central to
any Ohio pass-through analysis.

### T7. Utah retirement credit paid to any Social Security recipient,
unphased and un-gated

TAXSIM grants Utah's retirement tax credit (Utah Code 59-10-1019, born
before 1953, phased out at 2.5¢/$ of MAGI above the threshold) to any
record with Social Security income: a 40-year-old with $2M of wages and
any positive `gssi` receives a flat $288 (= 6% × $4,800; $576 per couple;
$271 under the 2017 vintage constant), verified across seven probe cases.
The credit should be zero for anyone born after 1952 and for any filer at
that income. In our 2017–2020 samples this is the dominant UT wedge
(point masses of ~200–260 records per year at exactly +$288/+$576).

Related input-representation note: TAXSIM derives head-of-household
treatment from the presence of dependents and ignores `mstat` (single+deps
and HoH+deps return identical results to the cent; HoH without mapped
dependents computes as single). Any state credit keyed to the federal
standard deduction or a filing-status threshold — Utah's taxpayer tax
credit is the clean example — inherits symmetric errors on returns whose
actual filing status differs from the dependents-derived one (±$464 UT
masses in 2019).

### T8. Maryland 2019 standard deduction: the minimum is applied where the
maximum belongs

For tax year 2019 only, TAXSIM returns the MD standard-deduction *minimum*
($1,550 single / $3,100 joint) for filers whose 15%-of-AGI computation
should cap at the maximum ($2,250 / $4,550): probe-verified
`v34_state_std_deduction_amount` = 1,550 at $100k single wages. 2018 and
2020 probe correctly (2020 uses the 2019 maxima — one indexing step stale,
~$2–5 of tax). Effect: flat +$33/+$69/+$83 overstatement of MD tax on
every 2019 standard-deduction return (~3,900 records in our sample).

### T9. Wisconsin 2017–2018 bracket thresholds stale

TAXSIM's WI 2017 and 2018 rate schedules use thresholds ~3% below the
published DOR tables (empirical top-bracket entry ≈ $320,250 MFJ vs the
published $329,810 for 2017), and the 2018 schedule returns tax
byte-identical to 2017 despite different published thresholds. Effect:
flat overtaxation of ~$12.8 for 6.27%-bracket records and ~$143.6 for
top-bracket records in both years (~1,190 records/yr in our sample).
The 2019–2020 vintages are correct.

### T10. Delaware itemized deduction granted to filers with none, at the
SALT cap

TAXSIM reports a positive DE itemized deduction (`v35_state_itemized_deduction`)
for filers whose federal itemized deductions are **entirely zero** — no
mortgage interest, no charity, no property tax, no state income tax, and
`itemizing` false on the federal return — and then uses it, since it exceeds
Delaware's small standard deduction. `v34_state_std_deduction_amount` is
correct throughout ($3,250 single / $6,500 joint, matching 30 Del. C. 1108(a)),
so the standard deduction is not the issue; the itemized figure is fabricated.

The amount is the SALT cap: `v35` = **$10,000**, and **$5,000** for married
filing separately, on the affected records. Worked example (TY2019, single,
AGI $1,733,137, every federal itemized component zero): `v34` = 3,250,
`v35` = 10,000, and `v36_state_taxable_income` = state AGI − 10,000, so the
$10,000 is what was used. Our deduction is the $3,250 standard, which is what
Delaware's PIT-RES Line 20a allows.

There is a **discontinuity at 2019**: the share of our sampled DE records with
`v35` > 0 is 54.6% (2017) and 54.3% (2018) but **97.0%** in both 2019 and 2020,
which suggests a vintage change rather than a long-standing modelling choice.

Effect: TAXSIM's DE tax runs LOW by the marginal rate on the excess deduction —
a flat $445.50 for single filers in the 6.6% top bracket (a $6,750 base gap)
and $231.00 for joint filers ($3,500), which are the two largest point masses
in our DE comparison at 3.2% and 1.3% of federally aligned records. Restricting
to records where TAXSIM did not use a fabricated itemized deduction lifts our
TY2019 DE match@$100 from 0.633 to 0.711.

Checked and NOT an issue, recorded so it is not re-investigated: TAXSIM does
grant Delaware's additional $110 personal credit for filers aged 60 or over
(30 Del. C. 1110(b)(2)) — `v40_state_total_credits` is 220 for 60+ single
filers with no dependents against 110 for younger ones, and those records agree
with us to the cent at the median.

### T11. Delaware itemized deduction omits the Schedule A "other" class

The companion to T10, and opposite-signed. For DE filers who DO itemize on both
sides, TAXSIM's `v35_state_itemized_deduction` equals our Delaware itemized
deduction MINUS the federal Schedule A "other itemized deductions" class
(`other_item_ded`) — **exactly, to the dollar, on 44.9% of the affected records**
(2,504 records with a nonzero other class, TY2019). Delaware's PIT-RSA carries
that class on its own Line 16, and Delaware's itemized deduction is the federal
Schedule A total less state and local income taxes, so the class belongs in the
base and we include it.

Effect: TAXSIM's deduction is smaller, so its DE tax runs HIGH — the reverse of
T10, which is why DE's mean signed difference flips sign across years. The
magnitudes reach the extreme tail (one TY2019 return carries $16.9M of Schedule
A "other", where TAXSIM's `v35` of $66,649 equals our $16,997,406 less that
class to the cent). Setting these records aside would lift TY2019 DE from 0.711
to 0.820 and TY2017 from 0.597 to 0.764.

NOT generalized beyond Delaware, deliberately. The same test on CA, DC, MD, MN,
NM and VA gives exact-identity shares of 2-20% with residuals of both signs, so
the arithmetic does not isolate a single component in states whose itemized base
carries more of its own modifications. Whether TAXSIM omits the class for those
states too is open, and it matters because MD and MN have already been through
residual attribution — which is why this is annotated rather than excluded
rather than being made to move their scores as a side effect.

### T12. Delaware pension exclusion granted where Delaware disqualifies it,
and to filers with no retirement income at all

The third and last identified driver of the DE residual, in the state-AGI stage.
Delaware allows up to $2,000 of pension for filers under 60 and $12,500 of
pension plus eligible retirement income at 60 or over (30 Del. C. 1106(b)(3);
PIT-RES Line 6). The gaps between our state AGI and TAXSIM's `v32_state_agi` are
exact multiples of those amounts — +2,000 (529 records, TY2019), +4,000 (310),
−12,500 (225), −10,500 (94), ±25,000 — so both sides are applying the same
provision and disagreeing about eligibility, not about the amounts.

Two TAXSIM behaviours account for the `+` direction, where TAXSIM excludes more
than we do:

- **Early IRA distributions.** Records at +2,000 with retirement income are
  under-60 filers (ages 32, 43, 48, 59 in the sampled cases) whose only
  retirement income is an IRA distribution. PIT-RES Line 6 states that "an early
  distribution from an IRA or pension fund ... does not qualify for the pension
  exclusion", and every distribution to a filer under 59½ is early, so we
  correctly grant nothing. TAXSIM grants the $2,000.
- **Filers with no retirement income, driving state AGI NEGATIVE.** The rest of
  the +2,000 mass is filers with zero pension, zero IRA and, in the clearest
  cases, zero total income — two sampled age-80 records have AGI 0 and our state
  AGI 0, while TAXSIM reports `v32_state_agi` = **−1,999.99**. There is nothing
  to exclude. **619–642 DE records per year** have TAXSIM state AGI below zero
  where ours is at or above zero.

The `−` direction is ours and is the one place in this investigation where we
could be over-granting: at 60+ we apply the $12,500 to pension PLUS eligible
retirement income, and the −12,500 group is 86% interest, 64% dividends and 56%
capital gains with only 32% holding a pension. Our base follows the PIT-RES
Line 6 worksheet as transcribed across seven booklet years in the DE packet
("$12,500 per person of pension plus eligible retirement income"). If Delaware's
definition of eligible retirement income is narrower than that reading, we
over-exclude up to $12,500 of base (about $825 of tax) for 60+ investment-income
holders — **a booklet re-read of the Line 6 worksheet would settle it**, and it
is the one DE item still capable of being an our-side error.

Sizing: ~26–27% of DE records carry a state-AGI disagreement. Restricting to
records where state AGI agrees lifts TY2019 from 0.711 to 0.773; requiring both
state AGI and the deduction to agree gives **0.953**, which is the acceptance
bar — so the DE schedule, credits and combined-separate handling are sound and
the whole residual lives in these two stages.

### T13. Oklahoma $17,000 itemized cap applied without the statutory charity
and medical exemptions

68 O.S. 2358(D)(1) caps Oklahoma itemized deductions at $17,000 from TY2018 but
EXEMPTS charitable contributions and medical expenses from the cap, so the
allowed amount is `min(17,000, base − charity − medical) + charity + medical`.
TAXSIM applies a flat $17,000: `v35_state_itemized_deduction` equals exactly
17,000 on **91%** of Oklahoma itemizer records in every cap year, and

    our itemized  =  TAXSIM's 17,000  +  charity  +  medical

holds **exactly, to the dollar, on 69%** of them (median residual 0). Worked
records (TY2019): ours 17,132.07 against 17,000 with charity 132.07; ours
17,996.30 with charity 996.30; ours 18,133.05 with charity 1,133.05; ours
59,684.66 with charity 42,684.66.

TY2017 is the control and it behaves: the cap did not exist that year, TAXSIM
never sits at 17,000, and the identity has no hits at all.

Effect: TAXSIM's deduction is too small, so its Oklahoma tax runs HIGH.
Excluding the affected records — TAXSIM pinned at the flat cap while ours
exceeds it, which identifies exactly the failure — lifts our OK match@$100 from
**0.727/0.719/0.720 to 0.872/0.869/0.873** in TY2018/2019/2020. This is the
single largest attribution any one item has produced in this project.

### T14. District of Columbia: unemployment compensation subtracted from DC
AGI in years when DC taxed it

TAXSIM removes unemployment compensation from DC state AGI in every year of
the 2017–2020 window. Probe (single, $50,000 wages + $5,000 UI): `v10` =
55,000 and `v32_state_agi` = 50,000 in 2017, 2018 and 2019. In 2020 the
subtraction stacks on the federal ARPA exclusion TAXSIM also applies (`v10` =
50,000, `v32` = 45,000 — the same $5,000 comes out twice).

The booklets say the opposite. No line of Schedule I Calculation B (the
exhaustive subtraction list) mentions unemployment, and the instructions state
it expressly — 2017: "All unemployment compensation received in 2017 is
taxable"; 2020: "All unemployment compensation received in 2020 is taxable."
The District first exempted UI benefits in TY2021 — after the window in which
TAXSIM's state law is actually coded.

On our DC validation sample, `v32 − our state AGI == −UI` holds exactly on
76–79% of federally-aligned UI recipients with a state-AGI gap in 2017–2019
(the remainder carry a second, unrelated wedge).

Effect: TAXSIM's DC AGI is too low by the UI amount, so its DC tax runs LOW
by 4–8.95% of UI on every DC return with unemployment income in 2017–2020.

### T15. California 2017 CalEITC paid outside the pre-expansion age band

Through TY2017 the CalEITC followed the federal childless age band: a filer
without qualifying children had to be 25–64 (FTB 3514; AB 1809 expanded
eligibility to 18–24 and 65+ only from TY2018). TAXSIM pays the 2017 credit
to childless filers past the ceiling: on our 2017 CA validation sample,
fed-aligned childless records aged 67–73 with $50–$5,500 of earned income
are paid `v39_state_eitc` of $60–$157 (e.g. age 68, $4,786 self-employment
earnings → $156.76; age 73, $5,457 → $116.21). The amounts are small
(the 2017 childless maximum was $223), so this class annotates rather than
excludes in our harness.

Effect: TAXSIM grants a small refundable 2017 CalEITC to 65+ childless
filers the FTB tables exclude.

### P5. One-time rebates netted into eligibility-year `state_income_tax`
(NY, VA, GA, AZ, NM — generalizing P2)

The P2 pattern is systematic across states (all verified in 1.775.7 package
source plus record-level point masses in our 2021–2024 samples):

- **NY 2023**: `ny_inflation_refund_credit` books the 2025 inflation refund
  checks (S.3009-C; $200 single/$400 joint, tiered by NY AGI) into tax year
  2023 — the source comments the choice ("the tax effect belongs to the
  eligibility year"). Every low/mid-AGI 2023 record shifts; our NY 2023
  clean match collapses to 0.160 vs 0.833 (2022)/0.797 (2024).
- **VA 2021, 2023, 2024**: `va_rebate` books the fall-2022 rebate
  ($250/$500) into 2021 and the HB6001 2023 rebate ($200/$400) into 2023
  AND, via the HB 1600 reauthorization, 2024.
- **GA 2021**: `ga_surplus_tax_rebate` (HB 1302, $250/$375/$500) enters the
  2021-only nonrefundable-credit list (liability-capped via max(0, ·)).
- **AZ 2021**: `az_families_tax_rebate` (SB 1734, paid fall 2023; $250 per
  dependent under 17, max three) books into tax year 2021.
- **NM 2021**: THREE rebates at once —
  `nm_2021_income_rebate` ($250), `nm_additional_2021_income_rebate` ($500)
  and `nm_supplemental_2021_income_rebate` ($500), all mailed checks under
  Laws 2021 ch.4 and the 2021 special session rather than credits claimed on
  the PIT-1. Doubled for joint filers this is a flat $2,500, which is
  precisely the MEDIAN difference in our NM 2021 cell — the clean match rate
  there is 0.000, the most complete collapse this class has produced.
  Worth noting for the upstream report: unlike the other four, the NM rebate
  variables keep computing nonzero values in 2022–2024 but are NOT included
  in `nm_refundable_credits` those years (verified 2026-08-13 by direct
  probe: `nm_refundable_credits` equals LICTR alone from 2022). So the
  netting is genuinely 2021-only even though the variables are not, and any
  exclusion keyed on those columns must be year-scoped or it will wrongly
  drop 2022–2024 records.

Whether eligibility-year booking is right is a concept choice (the checks
arrive one to two calendar years later), but as with P1/P2 it makes
`state_income_tax` diverge from the recurring-liability concept most
revenue analysis uses, and it does so retroactively for years that were
already final. A uniform pre-rebate liability output would resolve the
whole class.

### P6. California CalEITC paid to married-filing-separately filers
unconditionally

> **Status 2026-09-30:** still present in 2.18.2, and wider than recorded here. PolicyEngine's FEDERAL EITC has the same gap: from 2021 `eitc.eligibility.separate_filer` admits every separate filer, so a childless one is paid $600 in 2023, though IRC 32(d)(2) requires a qualifying child. The CalEITC also pays separate filers in 2020, when FTB 3514 barred them ("Is your filing status married filing separately? Yes, stop here"). A fix for both is drafted as a PR on a local branch (`bug_reports/policyengine/drafts/pr_mfs_eitc.md`); not yet opened. Note the same rule exposed an OUR-SIDE gap: our federal `eitc.mfs_eligible` was 0 in every year, so from 2021 we denied separate filers who do have a qualifying child. *(Fixed 2026-09-30 in `22c259749`: `mfs_eligible` is 1 from 2021, with a qualifying child required, in `config/scenarios/tax_law/baseline/eitc.yaml` and `eitc.R`. Still inert in practice, because Tax-Data's EITC child count is zero for every separate filer.)*

FTB 3514 bars MFS filers from the CalEITC (and through its qualifying-child
requirement, the YCTC) unless they meet the ARPA-style conditions adopted
from TY2021: a qualifying child who lived with the filer for more than half
the year, and living apart from the spouse for the last six months (or a
separation decree). PolicyEngine 1.775.7 pays the credit to MFS filers with
no conditions at all: a synthetic MFS filer, age 40, $8,639 of wages and no
children — who fails the conditions on their face — is paid `ca_eitc` =
$203.94 (2023). The conditions are unobservable in most microdata, so some
default is unavoidable, but the federal `eitc` variable resolves the same
problem in the restrictive direction; `ca_eitc` granting by default is
internally inconsistent with it.

Effect: PolicyEngine's CA liability runs LOW by the CalEITC/YCTC amount
(~$100–$1,200) on low-income MFS records.

### P7. California addback of non-California municipal-bond interest not
modeled

Interest on non-California municipal bonds is taxable in California
(Schedule CA, interest additions). PolicyEngine takes a
`tax_exempt_interest_income` input but applies no CA addback: a synthetic
single filer with $100,000 of wages and $50,000 of tax-exempt interest shows
`ca_agi` = $100,000 exactly (2023). The true own-state share of a filer's
exempt interest is unobservable — our model assumes 75% California / 25%
addback — but modeling zero addback prices ALL municipal interest as
California-source, which is the one assumption the form rules out for a
diversified holder.

Effect: PolicyEngine's CA liability runs LOW by up to 9.3–13.3% of a
filer's non-California municipal interest; on our high-exempt-interest 2023
records the gap reached five figures.

### P8. California CalEITC: the FTB 3514 earned-vs-AGI second lookup is
skipped

> **Status 2026-09-30: FIXED UPSTREAM** in PolicyEngine/policyengine-us#9363 (merged 2026-09-01). 2.18.2 pays $390.62 on the documented case (form: $390). Nothing to file; our exclusion stays until the harness moves off the 1.775.7 pin.

FTB 3514 (Step 6 / Worksheet instructions) requires that when federal AGI
is at or above the safe-harbor threshold, the CalEITC is the SMALLER of
the table amount at California earned income and the table amount at
federal AGI. PolicyEngine 1.775.7 pays on earned income alone. Verified
exactly against the published 2022 tables on our sample: a one-child
filer with earned income $10,118 and federal AGI $17,016 is paid $763 by
PE (the earned-income table row is $761) where the form pays $390 (the
AGI row, the smaller); a second record matches the same way ($320 vs the
form's $64). TAXSIM-35 skips the same rule (see the CA annotate row in
`src/tests/state/cross_model/known_differences.csv`).

Effect: PolicyEngine's CA liability runs LOW by $100–400 on low-income
records whose AGI exceeds earned income above the safe harbor.

### P9. State per-dependent benefits denied for dependents aged 18 and over

> **CORRECTION 2026-09-30: this is not twelve state bugs, and most of it was our harness.** The age-18 cliff comes from PolicyEngine's head/spouse inference, not from any state rule. `is_tax_unit_spouse` makes the oldest non-head adult (18+) the spouse, ignoring marital units and a supplied filing status, and `is_tax_unit_dependent` is simply "not head and not spouse". So an 18+ child stops being a dependent, losing the federal $500 other-dependents credit and every state per-dependent benefit. Our PE driver never supplied `is_tax_unit_dependent`, which is what exposed us to it. With the flag supplied (driver fixed 2026-09-30), Illinois tax is identical at dependent ages 17/18/19/23, and in the harness records with a dependent aged 18+ match at the same rate as the rest of their state (e.g. 2023 SC 0.842 vs 0.855, GA 0.912 vs 0.927). Reproduced on 1.775.7 and 2.18.2 (`bug_reports/policyengine/dependency_default_probe.py`). Upstream, PolicyEngine's own open PR #9630 addresses the flagged-dependent case; our evidence for the unflagged case is drafted as a comment there. The per-state analysis below is kept as the record of how the finding was first read; the state tables are correct measurements of the wrong mechanism.

**The broadest PolicyEngine finding so far, and the one most worth
reporting.** PolicyEngine 1.775.7 appears to gate state per-dependent
benefits on a dependent being under 18, in states whose own law has no age
condition. Confirmed in three states, with an age cliff that is identical
in each: the benefit is paid at dependent ages 5, 10, 16 and 17 and
disappears at 18.

Probed on single filers at 2023 law differing only in one dependent's age
(inputs in `output/probe/{il,ca,ut}_pe_ages.csv`):

| state | benefit | no dependent | dep aged 17 | dep aged 18 |
|---|---|---|---|---|
| IL | personal exemption, 35 ILCS 5/204 | 2,354.96 | 2,234.93 | 2,354.96 |
| CA | dependent exemption credit, R&TC 17054(d) | 1,768.36 | 1,322.36 | 1,768.36 |
| UT | personal exemption, UC 59-10-1018(1) | 2,521.35 | 2,404.89 | 2,521.35 |

Each gap is exactly the statutory amount: $120.03 = the $2,425 Illinois
exemption at 4.95%; $446 = the California dependent credit; $116.46 =
the $1,941 Utah exemption at 6%.

None of the three states restricts by age. Illinois allows an exemption for
each person claimed as a dependent on the federal return and IL-1040 simply
carries the federal count. California defines a dependent by IRC 152.
Utah's qualifying dependent is one for whom a credit is allowed under IRC
24, which since TCJA houses the $500 other-dependents credit at 24(h)(4) --
so an 18-year-old or an adult dependent still qualifies.

TAXSIM-35 does not share the behaviour, which is what makes this a
PolicyEngine-side finding rather than an open question. Illinois is the
sharpest control: our IL cells match TAXSIM at 1.0000 in all four years on
roughly 10,000 federally aligned records each.

Scale: sweeping all 48 enabled jurisdictions at dependent ages 10 and 20
(2023) finds an age effect in 23 of them -- IL, UT, CA plus NY, NM, MA, MN,
ME, OK, ID, GA, VT, KS, AZ, NJ, MS, NC, IN, IA, LA, and in the other
direction MO, AL, AR, OR, where the older dependent is treated more
favourably. Several of those are cases where the state genuinely does
restrict by age (a child credit limited to children under 17, for instance),
so the count of 23 was an upper bound on the problem, not a claim.

**That check is now done for nine more states (2026-08-22 sweep).** Each was
probed on 2023 head-of-household returns with one dependent, identical but
for the dependent's age, at two income levels; a state is counted only where
the PE rise resolves EXACTLY to the encoded per-dependent amount times a
published state rate at that income.

| state | benefit and cite | PE rise at $60k | = amount x rate |
|---|---|---|---|
| SC | dependent exemption, S.C. Code 12-6-1140 | 295.04 | 4,610 x 6.40% (top rate) |
| GA | dependent exemption, Form 500 instructions | 172.50 | 3,000 x 5.75% (flat) |
| VT | dependent exemption, 32 V.S.A. 5811(21)(C) | 162.48 | 4,850 x 3.35% (first bracket) |
| KS | per-exemption amount, K.S.A. 79-32,121 | 128.25 | 2,250 x 5.70% (top rate) |
| MS | dependent exemption, Form 80-105 line 10 | 75.00 | 1,500 x 5.00% (flat) |
| NY | dependent exemption, Tax Law 616 | 55.00 | 1,000 x 5.50% |
| MA | dependent exemption, Form 1 line 2b | 50.00 | 1,000 x 5.00% (flat) |
| LA | per-exemption add-on, R.S. 47:294 | 18.50 | 1,000 x 1.85% (BOTTOM bracket) |
| AZ | $25 other-dependent credit, Form 140 worksheet | 25.00 | the credit itself |

None of the nine restricts by age. Arizona is the sharpest of them for
reporting purposes, because its worksheet grants the $25 tier expressly to a
dependent aged 17 or over -- the very case PolicyEngine denies.

**Five of the nine are below the harness's $100 tolerance per dependent** (MS
75.00, NY 55.00, MA 50.00, AZ 25.00, LA 18.50) and are therefore annotated
rather than excluded, per the sub-$100 standard: a single dependent aged 18 or
over cannot produce a $100 mismatch there, so an exclusion would remove
matching records -- which the 2026-08-23 rerun demonstrated, Arizona's cells
moving DOWN by 0.3-0.5pp while a tenth of its subset was dropped. They still
bite on returns with two or more such dependents. The four above the tolerance
(SC 295.04, GA 172.50, VT 162.48, KS 128.25) carry exclusions, and are where
the measured movement is: GA +5.7 to +8.4pp, KS +6.5 to +7.7pp, VT +5.8 to
+8.1pp, SC +2.7 to +7.1pp on the clean subset.

Being below a validation tolerance is not being unimportant: the amounts are
per dependent per year and scale with the dependent count, and for a
distributional or revenue estimate they do not net out.

Louisiana deserves a note of its own. A $60,000 Louisiana filer's marginal
rate is 4.25%, so the exemption is worth $42.50 if relieved at the margin.
The measured rise is $18.50, the exemption at the bottom bracket -- which is
the R.S. 47:32(A)(1)/294/295(B) bottom-bracket relief mechanic reproduced
from outside, by a different model, on a quantity nobody was testing for.

**Two things the sweep did NOT resolve, recorded so the next pass does not
re-run them.** First, the $30,000 income level is not usable as the test: a
dependent turning 18 also stops being an EITC/CTC qualifying child under IRC
32(c)(3)/24, which cascades into state piggyback credits and is correct law,
so the deltas there are large and irregular (MA 1,108.52, NJ 1,084.77, MN
2,006.80). Second, seven states show a rise that does NOT resolve to their
encoded per-dependent amount at any published rate, and are left open: MN
(implies 6.012%, and its child credit is phasing out at that income), NM
(8.00% against a 5.9% top rate), OK (7.25% against 4.75%), NJ (2.45%), IN
(resolves to the $1,500 child add-on, which Indiana itself age-gates), and
ME and IA, whose benefit is not the per-dependent exemption at all. The
opposite-direction states (MO, OR, AR) remain unexamined.

Full record: `research/state_tax/cross_model/class_sweep_2026_08_22.md`.

Effect where it bites: PolicyEngine's state liability runs HIGH by the
per-dependent amount on returns claiming a dependent aged 18 or over. On our
samples that is 47-59% of such returns in IL and the dominant residual in
all three states; excluding the class moves IL to 0.993-0.995, CA to
0.979-0.988 and UT to 0.970-0.988 on the clean subset.

### T18 scope: probed and REJECTED for MD, MA and WI

T18 does not generalize to every state encoding a care deduction off the
federal IRC 21 base. Five states encode one; all five have now been probed.

- **MD** — probed 2026-08-22 and rejected: the Maryland care effect varies
  correctly with spouse earnings, so TAXSIM applies the 21(d) limit there.
- **WI** — probed 2026-08-22 and rejected. On 2019 joint returns with two
  dependents and $6,000 of care expenses, the care effect is $930.60 when both
  spouses earn, **$0.00 when the spouse earns nothing**, and $210.20 when the
  spouse earns $2,000. That is the limitation working.
- **MA** — a different finding, in the opposite direction. TAXSIM's
  Massachusetts liability is invariant to care expenses entirely: siitax is
  5,050.00 and the implied deduction 22,000 in all four probe cases, including
  the case with no care expenses at all. The Form 1 line 12 deduction appears
  not to be modelled, so TAXSIM runs HIGH relative to us on Massachusetts
  returns claiming care expenses. Recorded rather than filed: the record-level
  confirmation has not been done.

### T18. Virginia and Idaho child/dependent care deduction granted without the IRC 21(d) earned-income limit

Virginia's Schedule ADJ code 101 deducts "the amount on which the federal
child and dependent care credit is based" (Va. Code 58.1-322.03(4)). That base
is limited by IRC 21(d)(1)(B) to the **lesser of the two spouses' earned
income** for a married couple, so a couple with one non-earning spouse has a
base of zero. TAXSIM-35 applies the federal dollar cap ($3,000 for one
qualifying person, $6,000 for two or more) and skips the limitation.

Probed on VA 2019 joint returns, two dependents, $6,000 of `childcare`, with
the deduction read off as `v32 - v33 - v34 - v36`:

| case | implied extra deduction | correct under 21(d)(1) |
|---|---|---|
| both spouses earn (80k / 40k) | 6,000 | 6,000 |
| **spouse earns nothing** | **6,000** | **0** |
| spouse earns nothing, no care expenses | 0 | 0 |
| **spouse earns $2,000** | **6,000** | **2,000** |

The third row rules out the deduction being something else: it disappears when
`childcare` is zero.

The residual on real records matches exactly. Among Virginia non-itemizers the
2019 miss modes are +172.50 and +345.00 -- $3,000 and $6,000 at Virginia's
5.75% rate, the one- and two-dependent caps -- and `min(ei1, ei2)` is zero on
26 of 28 and 25 of 35 of those records. Non-itemizers with care expenses match
at 0.718 against 0.951 without. TAXSIM also grants it where the dependents are
over 12 and so are not qualifying persons under IRC 21(b)(1)(A).

Effect: TAXSIM's Virginia liability runs LOW by up to $345 a return on
single-earner couples claiming care expenses.

**Idaho behaves the same way; Maryland does not.** Both were probed on
2026-08-22 rather than assumed. For Idaho (Idaho Code 63-3022(o)) siitax is
identical at 4,873.16 whether the spouse earns $40,000, nothing, or $2,000, and
rises to 5,288.66 only when care expenses are removed -- a flat $415.50 =
$6,000 x Idaho's 6.925%, taken regardless of the limitation. Idaho records with
care expenses match at 0.440 against 0.741 without.

Maryland is the counter-example and is NOT part of this issue: its care effect
varies correctly with the spouse's earnings (siitax 4,446.25 with both spouses
earning, 4,607.25 with a non-earning spouse, 4,572.58 with a spouse earning
$2,000), so TAXSIM applies the limitation there. Maryland's own care residual
is real but has a different, still-undiagnosed cause. So the bug is per-state
in TAXSIM rather than a single shared code path, and Massachusetts (which had
such a deduction before 2021) has not been checked.

### T19. Colorado pension/annuity subtraction: the 55-64 tier is not modeled

C.R.S. 39-22-104(4)(f) subtracts pension and annuity income included in federal
taxable income, up to **$24,000** for a taxpayer 65 or older and up to
**$20,000** for one aged **55 to 64**. TAXSIM-35 models only the 65-plus tier.

Probed on CO 2019 single filers with $20,000 of wages and $30,000 of pension
income, varying only age (CO is TAXSIM state 6):

| age | siitax | subtraction implied |
|---|---|---|
| 50 | 1,701.00 | none |
| 57 | 1,701.00 | none |
| 60 | 1,701.00 | **none — should be $20,000** |
| 64 | 1,701.00 | **none — should be $20,000** |
| 65 | 546.75 | applied |
| 70 | 546.75 | applied |

The identical figure at 50 and at 64 is the finding: nothing happens anywhere
in the 55-64 band.

The records agree to the cent. Miss modes are -926 and -1,852 in 2017 and -900
and -1,800 in 2019 -- $20,000 and $40,000 (one and two qualifying people) at
Colorado's 4.63% and 4.50% rates. The mode group is 73 of 76 aged 55-64, with
median retirement income of $44,402 and a $20,013 subtraction on our side. By
age band the clean subset matches at 0.693 for 55-64 against 0.922 for 65-plus
and 0.921 for under-55; among 55-64 filers with retirement income it is 0.221.

Effect: TAXSIM's Colorado liability runs HIGH by up to $900 per qualifying
person (up to $1,800 on a joint return where both spouses are in the band).
Colorado is a federal-taxable-income-start state, so this lands directly and
undiluted in state liability.

### P10. Hawaii: the state's OWN income tax deducted on the Hawaii return, and the Worksheet A-2 disallowance not applied

> **Status 2026-09-30: FIXED UPSTREAM** in PolicyEngine/policyengine-us#9597 (merged 2026-09-28). 2.18.2 returns `hi_salt_deduction` = 0 on the documented case and an exactly 11% marginal rate at $600,000. Nothing to file; our exclusion stays until the harness moves off the 1.775.7 pin.

**The largest PolicyEngine finding in this set by revenue effect**, because it
understates Hawaii's top marginal rate by 0.88 points for every high-income
filer, in every year tested.

PolicyEngine 1.775.7 grants Hawaii an itemized deduction to a pure-wage filer
with no deductible items of any kind. Its own intermediates, single filer,
$501,000 of wages, 2022:

| variable | value |
|---|---|
| `hi_salt_deduction` | 49,246.60 |
| `hi_withheld_income_tax` | 49,246.60 |
| `hi_itemized_deductions` | 39,220.60 |
| `hi_standard_deduction` | 2,200.00 |

The SALT deduction equals PE's own computed Hawaii withholding to the cent.
Two problems compound:

1. **Hawaii disallows the deduction above income thresholds and PE does not
   apply them.** State, local and foreign income taxes are deductible only where
   federal AGI is under $100,000 (single or married filing separately),
   $150,000 (head of household) or $200,000 (joint) -- Worksheet A-2 note,
   permanent for taxable years beginning after December 31, 2010. PE's
   deduction runs smoothly through the cliff: 6,909.60 at 95,000, 7,239.60 at
   99,000, 7,404.60 at 101,000, 7,734.60 at 105,000. There is no
   discontinuity anywhere.
2. **The deducted amount is Hawaii's own tax**, which makes the base circular.

**The arithmetic closes exactly, which is what confirms the mechanism.** Each
additional $1,000 of wages raises PE's withholding, which raises the deduction,
so Hawaii taxable income rises by only $920. And 0.92 x the statutory 11% top
rate = 0.1012 -- precisely the effective marginal rate we measure at $500,000,
$600,000 and $1,001,000, in both 2019 and 2022. PE's rate PARAMETERS are the
correct HRS 235-51 ladder (verified in its own `rates/single.yaml`); the base is
what is wrong, so the error grows without bound in income.

Effect on our comparison: above the statutory thresholds Hawaii matches at
0.021-0.034 with a median difference of +$2,964 to +$3,384, while **below them
the median difference is exactly zero**. Excluded on the above-threshold
population only.

Hawaii-specific: probing the same filer in ten states, MT and NY report a zero
`salt_deduction`, and ME/MA/SC/CA/VT/AR/IA do not expose the variable.

### P5 (continued). Five more one-time rebates netted into TY2021

The 2026-08-23 PE-window pass asked why 2021 is systematically the weakest
PolicyEngine year (median clean match 0.887 against 0.923-0.934 for 2022-2024,
five cells below 0.60 against nought or one elsewhere) and found five further
instances of the P5 class. Probed at $80,000 single:

| state | TY2021 amount | rebate |
|---|---|---|
| HI | 300.00 refundable | Act 115 (2022) constitutional tax refund |
| ME | 850.00 refundable | $850 relief payment, LD 1995 (2022) |
| MA | 516.35 refundable | Chapter 62F taxpayer refund -- **proportional**, 14.0312% of TY2021 liability |
| MT | 1,250.00 nonrefundable | HB 192 (2023) income tax rebate |
| SC | 800.00 nonrefundable | 2022 income tax rebate, capped at 800 |

Each shows the same behaviour as the New Mexico trio: the variable computes a
nonzero value in **every** year but is inside that state's credit total only in
2021, so a predicate keyed on the column alone would over-exclude 2022-2024.

**A note on our own treatment, recorded because it bears on how the numbers
read:** these rebates reached nearly every filer, so excluding them removes most
of the 2021 cell (95.6% in HI, 83.8% in ME, 68.3% in MT, 65.7% in MA, 52.1% in
SC). Those cells are DROPPED rather than passed. And in Montana the exclusion
moved the cell DOWN (0.300 -> 0.287), so its 2021 residual is dominated by
something else we have not yet identified -- the rebate divergence is real, but
it is not Montana's main 2021 problem.

### P11. New Jersey child tax credit paid before it existed (2020–2021)

New Jersey's child tax credit (N.J.S.A. 54A:4-17.1; NJ-1040 line 65) begins in tax year 2022. PolicyEngine's
parameters for it (`gov.states.nj.tax.income.credits.ctc.amount`, `.age_limit`) start at 2022-01-01, and the first
value is applied to earlier years. Probe (2026-10-01), head of household, $20,000 of wages, one child:

| year | child aged 3: `nj_ctc` | child aged 10 |
|---|---|---|
| 2020 | **500** | 0 |
| 2021 | **500** | 0 |
| 2022 | 500 | 0 |

Reproduced on 1.775.7 and 2.18.2. Effect: PolicyEngine's 2021 New Jersey liability is low by $100–$500 per child
under 6. In our 2021 cell this was most of the residual (0.818 → 0.942 once excluded). A fix is a zero value
from an earlier date (or an `in_effect` gate) on the CTC parameters.

### P12. Arkansas 2023 Inflationary Relief credit paid at $50 instead of $150

The DFA Inflationary Relief Income-Tax Credit worksheet for TY2023 pays $150 ($300 joint) up to $89,600 ($179,200)
of net income, then steps down $10 per $1,000 ($20 per $2,000) to zero at $103,600 ($207,200), the same maximum as
TY2022. policyengine-us 1.775.7 sets `gov.states.ar.tax.income.credits.inflationary_relief.max_amount` to $50 ($100
joint) from 2023-01-01, and leaves the joint `reduction.start` at the TY2022 $174,000 where the TY2023 table
starts its step-down at $179,200 (the single-filer start was updated to $89,600). Effect: PolicyEngine's 2023 Arkansas liability is high by $100 / $200 for most filers under
the threshold; in our 2023 cell these are the most common gaps (126 of 186 misses).

### P13. Arkansas gross income omits partnership, S-corporation, estate and other income

`gov.states.ar.tax.income.gross_income.sources` (individual and joint) maps AR1000F line 19 to `rental_income`
only. Line 19 is "rents, royalties, partnerships, estates and trusts", and line 22 (Form AR-OI) carries other
income and the net operating loss carryforward subtraction. None of `partnership_s_corp_income`, estate income or
`miscellaneous_income` enters Arkansas income. Probe (2026-10-01, policyengine-us 1.775.7, 2024 single, $100,000 of
wages): `ar_agi_indiv` is 100,000 with partnership/S-corp income of +50,000, -50,000 and -2,000 while federal AGI is
150,000 / 50,000 / 98,000, and `ar_income_tax` is 3,689.61 in every case. Upstream main (40ce0012a5, 2026-09-30) has
the same source list. Effect: Arkansas tax wrong in both directions for every filer with pass-through income or loss.

### P14. Arkansas TY2024 low income table, head of household with two or more dependents, skips a row (minor)

The TY2024 booklet's table pays $0 through $24,176 and $92 on $24,177-24,200. In
`low_income_tax_tables/head_of_household/two_or_more_dependents.yaml` the first two 2024 thresholds are both
24,200, so the $92 row disappears and those filers owe nothing. Found by checking our extraction of all 45 tables
against PolicyEngine's for TY2021-2025: 1,128 of 1,129 rows agree, this is the only difference. Under the $100
tolerance; no exclusion row.

### P15. Arkansas taxes 2021 unemployment benefits (fixed upstream)

Act 154 of 2021 exempted unemployment compensation from Arkansas income for calendar years 2020 and 2021.
policyengine-us 1.775.7 lists `unemployment_compensation` in `gov.states.ar.tax.income.gross_income.sources` from
2021-01-01. Fixed on upstream main by #9615 (2026-09-26), which gives TY2020-2021 their own source list without
it. Nothing to send; the exclusion row retires when the pinned version moves past the fix. (Separately, the 2021
Tax-Data records carrying it include implausible unemployment amounts of $100,000-$200,000, 14 of 371 aligned
Arkansas records above $50,000 -- a Tax-Data input question, not a PolicyEngine one.)

### P16. Montana dependent exemption limited to qualifying children

`mt_dependent_exemptions_person` multiplies the exemption by `is_qualifying_child_dependent` (IRC 152(c)), so a
qualifying relative under 152(d) -- an adult child past the age tests, a parent -- gets none. MCA 15-30-2114 and
the Form 2 instructions (TY2023 p.9: "each dependant counts as one exemption") make no such distinction. Found
2026-10-02: once our driver stopped letting PolicyEngine read adult dependents as spouses, Montana 2022-23 tax for
single and separate filers with an adult dependent rose by one exemption (about $183). Effect: PolicyEngine
overstates Montana tax through TY2023 for filers claiming adult dependents.

### P17. Alabama AGI omits miscellaneous income

Alabama taxes other income (Form 40 page 2 Part I). In policyengine-us 1.775.7 `al_agi` does not include
`miscellaneous_income`, while the federal tax deduction does reflect the federal tax on it. Probe (2026-10-02),
2024 single, $60,000 of wages: adding $20,000 of miscellaneous income raises federal AGI to 80,000 and
`al_federal_income_tax_deduction` from 5,216 to 9,441, leaves `al_agi` at 60,000, and lowers `al_income_tax` from
2,395 to 2,183. (Partnership/S-corporation income is included correctly.) Effect: Alabama tax understated, by
more than the omitted income alone, for filers with other income.

### P18. Mississippi AGI omits miscellaneous income

The P17 pattern in Mississippi. Probe (2026-10-02, policyengine-us 1.775.7), 2024 single, $60,000 of wages: adding
$20,000 of `miscellaneous_income` raises federal AGI to 80,000 but leaves `ms_agi` at 60,000 and `ms_income_tax` at
1,960; the same amount as partnership or rental income raises both. Mississippi taxes other income (Schedule N).

### P19. Mississippi: `state_income_tax` omits the child and dependent care credit

In policyengine-us 1.775.7 the generic `state_income_tax` for a Mississippi filer does not net the TY2023+
child and dependent care credit (25% of the federal credit, federal AGI up to $50,000) that `ms_income_tax`
does. Probe (2026-10-02), four TY2023 records: `state_income_tax` 159 / 108 / 216 / 1,113 against `ms_income_tax`
0 / 0 / 96 / 963, the gap being `ms_cdcc` each time. Our harness now reads `ms_income_tax` for Mississippi (as it
reads `md_income_tax` for Maryland). Anything built on `state_income_tax` overstates Mississippi tax for these
filers.

### P20. Minnesota M1CWFC: an 18-year-old is never a qualifying older child

Minn. Stat. 290.0671 subd. 1 defines a "qualifying older child" as an IRC 32(c) qualifying child "that attained
at least the age of 18 in the taxable year". Under IRC 152(c)(3)(A)(i) a child under 19 at year end is a qualifying
child with no student test. policyengine-us 1.775.7 (`mn_child_and_working_families_credits`) counts an older
child only where `age > wfc.additional.age_threshold` (18) **and** the child is a full-time student or disabled.
An 18-year-old therefore never counts, student or not. The comparison is ">= 18", and the student test should
apply only from 19. Effect: PolicyEngine omits the older-child amount ($925 / $970 / $1,000 for one child in
2023 / 2024 / 2025) for families whose older child is 18, for example harness records 15386, 19779 and 29976
(2023): the gap is −925 each time, and `mn_child_and_working_families_credits` equals our credit less that
amount.

For ages 19–23 the student requirement is the law. Our harness cannot tell PolicyEngine who is a student, and we
assume a dependent aged 19–23 is one (a dependent of that age who is not a student is usually a qualifying
relative, not a qualifying child). That part is an assumption difference, not a PolicyEngine bug.

### P21. Kentucky 2021 child and dependent care credit built on the ARPA federal credit

Kentucky conforms to the IRC as of December 31, 2018, and Form 2441-K (2021) says "Kentucky does not conform to Section
9631 of the federal American Rescue Plan (ARP) of 2021": it recomputes the credit on pre-ARPA terms ($3,000 / $6,000,
the .35-.20 decimal), limits it by federal tax, and takes 20% (line 12). policyengine-us 1.775.7 takes 20% of the ARPA
federal credit. Probe (2026-10-08, harness records): `cdcc` 3,392 / 1,672 / 2,822, `ky_cdcc` 678 / 334 / 564 (exactly
20%). Effect: PolicyEngine overstates Kentucky 2021 care credits, often several-fold.

### P22. New York 2021 child and dependent care credit built on the ARPA federal credit

IT-216-I (2021): "NYS has decoupled from federal changes made to the Internal Revenue Code (IRC) after March 1, 2020";
IT-216 recomputes the credit before any federal limitation on pre-ARPA terms ($3,000 / $6,000, New York's caps for three
or more persons, the .35-.20 decimal) and applies the New York share. policyengine-us 1.775.7 uses the ARPA-era amount.
Probe (2026-10-08, harness records 18023 / 157421 / 138587): `ny_cdcc` 3,000 / 2,400 / 1,693, where IT-216 gives
630 / 360 / 360. Effect: PolicyEngine overstates New York 2021 care credits.

### P23. Iowa net income ignores the federal capital-loss limitation

Iowa net income starts from federal AGI (IA 1040 line 1; Iowa Code 422.7), which carries the IRC 1211(b) $3,000 limit on
net capital losses. policyengine-us 1.775.7 lets the whole loss through. Per-record rerun (2026-10-08): record 430290
(wages $135,568, pensions $243,633, a $24.4 million long-term capital loss) gets `ia_net_income` of -24,041,686,
`ia_is_tax_exempt` = 1 and `ia_income_tax` = 0 where the Iowa return owes about $20,600; record 94929 (wages $347,859,
$439,301 of losses) likewise pays nothing. Effect: PolicyEngine understates Iowa tax for filers with capital losses over
$3,000, to zero for large losses.

### P24. Nebraska Child Care Tax Credit paid without the application cap or the licensed-care condition

Neb. Rev. Stat. 77-7203 (LB 754, 2023; TY2024+) gives a parent a refundable $2,000 ($1,000 above $75,000 of household
income, nothing above $150,000) per child aged five or less, but only on application to the Department of Revenue, which
approves credits "in the order in which they are received ... until the total amount of credits approved for the year
equals fifteen million dollars", and only where the child is in a licensed child care program (or a subsidy provider, or
the household is at or below poverty). policyengine-us 1.775.7 (`ne_refundable_ctc`) pays it to every tax unit with any
reported care expense or poverty-level income, with no cap. In our TY2024 Nebraska comparison it adds exactly $2,000 or
$4,000 to 23 of 462 records. Effect: PolicyEngine overstates Nebraska refunds by far more than the $15 million program
can pay.

### P25. Wisconsin school property tax credit: separate filers get the joint cap

Wisconsin's renter's and homeowner's school property tax credit is 12% of up to $2,500 of property tax or rent-constituting
tax, "not ... more than $300 ($150 if married filing a separate return or married filing as head of household)"; "The
maximum credit allowable to each spouse is $150" (Form 1 instructions 2023, line 15). policyengine-us 1.775.7 allows a
separate filer $300. Per-record rerun (2026-10-09): record 116220, a separate filer, `wi_property_tax_credit` 300 where
the form allows 150. Effect: PolicyEngine understates Wisconsin tax for separate filers with more than $1,250 of property
tax by up to $150.

### P26. Nebraska itemized deductions keep property tax instead of subtracting the full income tax

Form 1040N line 7 takes the federal itemized total (Schedule A line 17, SALT cap included) and line 8 subtracts "the amount
of state and local income taxes reported on Federal Schedule A, line 5a even if the total amount of state and local taxes
was limited to $10,000" (2022 booklet). policyengine-us 1.775.7 (`ne_itemized_deductions`) instead takes itemized
deductions less all SALT plus property taxes up to the cap. The two agree when the cap does not bind; when it does,
Nebraska keeps less than the property tax and often nothing. Per-record rerun (2026-10-09): record 294403, 2022 single,
federal itemized $37,379 ($10,000 of capped SALT, $690,766 of income tax): `ne_itemized_deductions` 37,379 where the
form gives zero and the $7,350 standard deduction applies. Effect: PolicyEngine understates Nebraska tax for itemizers
over the cap.

### P27. Maryland 2021 unemployment insurance subtraction missing

The RELIEF Act (Laws of Maryland 2021 ch. 39, SB 496) subtracts unemployment compensation from Maryland AGI for tax
years 2020 and 2021 when federal AGI is under $100,000 (married filing jointly, head of household, surviving spouse) or
$75,000 (other filers); Form 502SU code yy. policyengine-us 1.775.7 has no such subtraction: the Maryland subtraction list
(`md_total_subtractions`) holds dependent care, pensions, Social Security, military retirement, two-income and 529
contributions only. Per-record rerun (2026-10-09): record 75235, 2021 separate filer, federal AGI $73,933 of which $43,633
is unemployment compensation: `md_agi` 73,933 and `md_total_subtractions` 0 where the form gives $30,300. Record 49111
(single, $30,150 of unemployment compensation) has the same shape. Effect: PolicyEngine overstates 2021 Maryland tax by
the state rate times unemployment compensation below the cliff, about $1,400-$2,700 on the affected records.

### P28. State retirement exclusions never see IRA distributions

policyengine-us 1.775.7 carries IRA distributions as `taxable_ira_distributions` (inside federal gross income via
`taxable_retirement_distributions`), but no state income tax variable or parameter reads it: every state pension or
retirement exclusion takes `taxable_pension_income` only (for example the Illinois subtraction list,
`gov.states.il.tax.income.base.subtractions`, is taxable Social Security plus taxable pension income). States whose
exclusion covers IRA distributions therefore tax them in PolicyEngine. Illinois exempts all qualified retirement
income, IRA distributions included (IL-1040 line 5). Harness evidence (2026-10-09): passing IRA distributions as
`taxable_ira_distributions` instead of pension income dropped the Illinois 2024 clean match from 0.989 to 0.924, with
IRA records matching 0.268 against 0.988 for the rest; record 334315 (2024 head of household, $106,106 of IRA
distributions) owes $5,266 of Illinois tax in PolicyEngine where the subtraction leaves $14. New York, Oklahoma, Iowa,
Maine and Michigan moved the same way. The harness now folds IRA distributions into pension income except for Maryland
and Rhode Island, whose exclusions do not cover IRAs. Effect: PolicyEngine overstates state tax on IRA income wherever
the state exclusion includes it.

### P29. Maryland earned income credit paid to filers past the federal maximum age

Maryland's credit is a share of the federal credit, and Tax-General 10-704(b)(3) waives only the federal MINIMUM age
for filers without a qualifying child ("calculated without regard to the minimum age requirement under section
32(c)(1)(A)(ii)(II)"). Filers whose only adults are 65 or older get no federal credit outside TY2021 and so no Maryland
credit. policyengine-us 1.775.7 pays them one. Per-record rerun (2026-10-09), 2023: record 297237 (single, 68, $7,937
of wages) `md_unmarried_childless_non_refundable_eitc` 600 and `md_unmarried_childless_refundable_eitc` 549; record
132613 (joint, both 80, $13,495 of wages) `md_married_or_has_child_refundable_eitc` 227. Effect: PolicyEngine
understates Maryland tax for low-earning filers aged 65 and over by up to the childless cap.

### P30. Iowa federal tax deduction subtracts payroll taxes that were never in it

`ia_fedtax_deduction` starts from `income_tax_before_refundable_credits`, which is regular tax plus capital gains tax,
AMT, the net investment income tax and recapture, less nonrefundable credits. It contains no self-employment tax and no
Additional Medicare Tax. The formula then subtracts both ("remove SECA and Additional Medicare taxes"), so the Iowa
deduction is understated by the filer's whole self-employment tax plus Additional Medicare Tax. Per-record rerun
(2026-10-09), 2022 record 293990: `income_tax_before_refundable_credits` 690,444, `self_employment_tax` 73,084,
`additional_medicare_tax` 15,224, `ia_fedtax_deduction` 602,135 (= 690,444 - 88,308); our deduction 690,474. Effect:
PolicyEngine overstates Iowa tax in 2021-2022 by the Iowa rate times those payroll taxes ($7,535 on this record). The
deduction is repealed from TY2023.

### P31. Iowa joint pension exclusion granted per spouse

Iowa Code 422.7(31) excludes pension income "up to a maximum of six thousand dollars for a person ... who files a
separate state income tax return and up to a maximum of twelve thousand dollars for a husband and wife who file a joint
state income tax return". policyengine-us 1.775.7 `ia_pension_exclusion` is a Person variable that applies the
filing-status cap to each person (`min_(pension, p.maximum_amount[filing_status])`), so on a joint return each spouse
excludes up to $12,000 and the couple up to $24,000. Per-record rerun (2026-10-09), 2021 record 51314 (joint, 74 and
67, $26,196 of pensions split between spouses): `ia_pension_exclusion` 24,000 where the statute allows 12,000. Effect:
PolicyEngine understates Iowa tax for two-pensioner couples by up to $12,000 times the Iowa rate, through TY2022 (the
retirement income exclusion replaces it from TY2023).

### P32. 2025 state itemized deductions move with the federal $40,000 SALT cap (Minnesota, Virginia)

Minnesota's itemized deductions (Schedule M1SA) allow real property taxes up to $10,000 ($5,000 separate), the limit
Minnesota adopted for its own Schedule A from TY2019. policyengine-us 1.775.7 builds `mn_itemized_deductions` from
`capped_property_taxes`, which caps property tax at the FEDERAL `salt_and_real_estate.cap` -- $40,000 for TY2025 under
the 2025 federal act, without the federal phase-down to $10,000 above $500,000 of modified AGI. Per-record rerun
(2026-10-09), 2025 record 135223 (joint, AGI $686,971, $33,928 of property tax): `mn_itemized_deductions` 31,307 =
35,787 + 33,928 less the 38,407 high-income reduction; with the $10,000 cap the deduction is 9,157. Even if Minnesota
conformed to the federal increase (a 2026 conformity bill, HF 2438, is reported; its treatment of the M1SA property tax
line is not verified here), this filer's federal cap is $10,000 after the phase-down. Effect: PolicyEngine understates
Minnesota tax for 2025 itemizers with more than $10,000 of property tax.

Virginia shows the same dependence. Virginia's Schedule A has not used the federal state and local tax cap since
TY2019: property and personal property taxes are allowed uncapped and income taxes are excluded, so the 2025 federal
cap increase cannot move a Virginia deduction. PolicyEngine's `va_itemized_deductions` for 2025
record 167921 (single, AGI $723,648) is 10,622, against 4,666 for the same inputs run as TY2024; our deduction, after
Virginia's limitation, is 2,822.

### P33. Mississippi income omits miscellaneous income and nonqualified dividends

`ms_agi` sums `gov.states.ms.tax.income.income_sources`: wages, self-employment, partnership and S corporation
income, loss-limited capital gains, rents, farm income, interest, QUALIFIED dividends, alimony, taxable pensions,
taxable Social Security and unemployment compensation. It omits `miscellaneous_income` and
`non_qualified_dividend_income`, though Form 80-105 taxes other income, Form 4797 gains and all dividends. Per-record
rerun (2026-10-09), 2025 record 157169 (head of household, $102,555 of other income and Form 4797 gains passed as
miscellaneous income): `ms_agi` 607,494 against federal AGI 710,048 with no Mississippi adjustment. Effect:
PolicyEngine understates Mississippi tax by the state rate times those incomes.

### P34. Massachusetts household dependent / child and family credit paid to married-filing-separately filers

`ma_child_and_family_credit` (the $180/$360 Household Dependent Tax Credit for TY2021-22; the $310/$440 Child and
Family Tax Credit from TY2023) counts dependents under 12, 65 or over, or disabled, caps the count at two for 2021-22,
and takes the greater of that and the Dependent Care Tax Credit -- with no filing-status test. M.G.L. c. 62 s. 6(y), as
inserted by St. 2021 c. 24 s. 29, allows a married taxpayer the credit only on a joint return or as head of household
under IRC s. 2(b); the 2021 and 2022 Form 1 instructions for line 46 say "You cannot claim this credit if married
filing a separate return", and the 2024 booklet (p. 15) bars the CFTC to married filing separately. Record-level
comparison (2026-10-13): every +180 residual in 2022 (22 records, 15 clean, e.g. 98396, 276512, 40285, 21512, 87632
with an 80-year-old dependent) is a separate filer with one qualifying dependent; 2021 shows the same pattern (2670,
342959, 372511) under the Chapter 62F exclusion. Effect: PolicyEngine overpays separate filers by $180 per qualifying
dependent (max $360) in 2021-22 and by $310/$440 per child from 2023.

### P35. New Mexico modified gross income omits partnership, S-corporation, farm, estate and other income

`nm_modified_gross_income` sums a source list that floors each source at zero (correct: NMSA 7-2-2(L) "undiminished by
losses", PIT-RC line 8 "If a loss, enter zero") but omits the distributive share of partnership and S-corporation
income, farm income, estate and trust income and miscellaneous income, all of which 7-2-2(L) and PIT-RC lines 8-12
include ("all other income"). PolicyEngine therefore understates MGI and overpays the Low Income Comprehensive Tax
Rebate (7-2-14) for low-income records with those sources, by one or more table bands. Exposed 2026-10-13 when the
rebate was encoded on our side; record-level pulls in `scratchpad` of that date.

### P36. New Mexico LICTR table, TY2023, five exemptions, $14,501-16,500

`parameters/gov/states/nm/tax/income/rebates/low_income_comprehensive/five_exemptions.yaml` (line 112) carries 305 for
the TY2023 $14,501-16,500 band; the published PIT-RC Table 1 (PITbook2023) row reads 308. The other 275 cells of every
2021-2025 vintage match the published tables cell for cell. De minimis.

### P37. Maine 2025 affordability payment netted into liability

`me_refundable_credits` for 2025 includes `me_affordability_payment` ($300 per person; H.P. 1491 Part T Sec. T-1),
whose own parameter note calls it a direct payment from a Special Revenue Fund, not a tax credit. In the 2026-10-13
re-score 82 of 95 clean TY2025 mismatches sit at +$300 or +$600 (PE lower), each with the payment present. The same
one-time-rebate class as P5. Effect: PolicyEngine understates Maine TY2025 liability by $300 per person.

### P38. New Mexico net capital gains deduction halved twice for separate filers

`nm_net_capital_gains_deduction` divides the whole deduction by two for a married-filing-separately return, computed on
that filer's own gains. NMSA 7-2-34 gives each separate filer "one-half of the deduction ... that would have been allowed
on the joint return" -- half of a deduction computed on the couple's combined gain. On own gains the faithful reading
halves the flat $1,000 ($2,500 from 2025) floor and leaves the 40% share alone (exact when spouses' gains are equal);
halving 40% of own gains takes the separate filer to 20%. Effect: PolicyEngine overstates New Mexico tax for separate
filers with gains by 5.9% x 20% of the gain (2026-10-14 probe: 195026, 326212, 363293 at $4,200-$4,950).

### P39. New Mexico net capital gains deduction includes short-term gain

The same variable is computed on `capital_gains`, which includes net short-term gain. 7-2-34 defines net capital gain by
IRC 1222(11): net long-term gain less net short-term loss. A filer with only short-term gain gets no deduction (record
409524, $170,644 of short-term gain and no long-term gain: PolicyEngine deducts $34,129). Effect: PolicyEngine
understates New Mexico tax by 5.9% x 40% of short-term gain through 2024, and up to $148 from 2025.

### P40. Washington Working Families Tax Credit paid for TY2021

PolicyEngine pays `wa_working_families_tax_credit` in 2021. RCW 82.08.0206 (ESHB 1297, Laws 2021 ch.195) first made the
credit available for tax year 2022; the Department of Revenue opened applications on 1 February 2023. Effect:
PolicyEngine understates TY2021 Washington liability by the full credit ($300-$1,200) for every eligible filer (90 of 90
clean TY2021 mismatches in the 2026-10-14 re-score).

### P41. Colorado federal deduction addback does not net the state income tax addback

`co_federal_deduction_addback` returns the federal itemized or standard deduction less the $12,000 / $16,000 exemption,
and `co_additions` adds it to `co_state_addback`. The 2024 Colorado 104 Book (line 4 instructions) requires the deduction
addback to be "reduced by any state income tax deduction from Schedule A of your federal return that is already added
back on line 2". PolicyEngine therefore adds the state income tax back twice for itemizers above $300,000 of AGI.
Effect: Colorado liability overstated by 4.4% (4.25% in 2024) of the line 2 addback (2026-10-14: records 80184,
291197, 157169, $154-$435).

### P42. D.C. EITC paid to childless married-filing-separately filers

PolicyEngine pays the D.C. EITC to childless separate filers. D.C. Code 47-1806.04(f)(1)(C) grants the childless credit
to an individual "who is eligible for an earned income tax credit" under IRC 32, and 32(d) requires married individuals
to file jointly (the ARPA separated-spouse exception needs a qualifying child); the 2019 D-40 booklet also states the
credit is not allowed if married filing separately. Effect: D.C. tax understated by up to the childless maximum for
childless separate filers (about 10 clean records a year, 2021-2024).

### P43. Delaware additional standard deduction granted for aged dependents

`de_additional_standard_deduction` is computed for every person aged 65+ or blind, and `de_standard_deduction_joint`
sums it onto the head, so a filer claiming an elderly parent as a dependent receives an extra $2,500. 30 Del. C. 1108(b)
limits the additional amount to the taxpayer and spouse. Effect: Delaware tax understated by $2,500 x the marginal rate
per aged dependent (probe 2026-10-15: records 324233, 41551, 88133 at 5,750 against the 3,250 base).

### P44. Oklahoma Schedule 511-A additions are always zero

`ok_additions` has no formula in the pinned policyengine-us, so it returns 0 and none of the Schedule 511-A additions
are applied -- in particular interest on other states' municipal bonds (68 O.S. 2358(A)(2)). Every gap in the class
is the top rate times the addback (TY2025 record with $186,455 of exempt interest: $46,614 added back by us, gap
$2,202 = 4.72%). Effect: PolicyEngine understates Oklahoma tax for holders of out-of-state municipal bonds.

### P45. Arizona additions are always zero

The same defect as P44: `az_additions` (`variables/gov/states/az/tax/income/az_additions.py`) declares the variable
with no formula, so `az_agi` adds nothing back -- in particular interest on other states' municipal bonds (A.R.S.
43-1021; Form 140 line 14). Every gap in the class is the Arizona rate times the addback (TY2023 record with $187,650
of exempt interest, 25% of it treated as non-Arizona: our base $46,652 higher, gap $1,166 = 2.5%). Effect:
PolicyEngine understates Arizona tax for holders of out-of-state municipal bonds.

### P46. Oregon TY2025 itemized-tax limit not reduced at high income (concept question)

For TY2025 PolicyEngine caps the income and property taxes on Schedule OR-A at a flat $40,000 ($20,000 married
filing separately). The 2025 OR-A instructions say "The same limit that applies to your federal deduction also
applies to your Oregon deduction", which we read as including the OBBBA reduction of the federal limit toward
$10,000 ($5,000) above $500,000 ($250,000) of modified AGI; the 2025 form's line 9 prints only $40,000 / $20,000.
Our reading was confirmed internally (2026-10-21); raised upstream as a reading to confirm rather than a bug: 23 TY2025 itemizers in our sample, each differing by exactly the
cap difference times the Oregon rate.

### P47. Montana capital gain credit goes negative on a net capital loss

`mt_capital_gain_credit` is `0.02 * capital_gains` with no floor at zero; the code comment says the variable "is
capped at 0", but `capital_gains` is negative for a net loss. The negative credit then raises Montana tax: TY2022
record with federal AGI of -$203,518 and no Montana taxable income gets `mt_non_refundable_credits` = -6,650 and
`mt_income_tax` = 6,650. 15-30-2301 MCA credits 2% of net capital GAIN. Effect: PolicyEngine overstates Montana tax
by 2% of the net loss for TY2021-23 filers with capital losses.

### P48. Montana partial pension, annuity and IRA exemption not modeled

The Montana subtractions list for 2021-2023 has no pension exemption (15-30-2110(2)(c) MCA: $4,640 per taxpayer in
TY2022 against pension, annuity and IRA income, reduced $2 per $1 of federal AGI above the threshold; repealed from
TY2024). Effect: PolicyEngine overstates Montana tax by up to about $310 per eligible taxpayer for TY2021-23.

### P49. Montana old-age subtraction given to dependents

`mt_old_age_subtraction` ($5,500 for a person 65 or older, TY2024+) is computed for every person in the tax unit
with no head-or-spouse condition, so a dependent parent's age earns the filer the subtraction. TY2024 records with a
filer under 50 and a dependent aged 78-80: PolicyEngine's Montana taxable income is the federal figure less
exactly 5,500. Effect: PolicyEngine understates Montana tax by $5,500 times the marginal rate (about $260-$325) for
filers claiming an elderly dependent.

### P50. Montana TY2025 itemized SALT limit not reduced at high income

Montana's TY2025 Worksheet B starts from federal Schedule A line 5e and its instructions describe the OBBBA
reduction of the $40,000 limit toward $10,000 above $500,000 of modified AGI. PolicyEngine applies the flat
$40,000 ($20,000 MFS): TY2025 married-filing-separately record with AGI $544,265 gets
`mt_itemized_deductions_joint` = 62,421, which is the federal itemized total of 47,421 with its phased-down $5,000 of
SALT replaced by $20,000. Effect: PolicyEngine understates Montana tax for high-income itemizers. The same choice
appears for Oregon (P46).

## Corroboration worth passing along

Where concepts align, agreement is excellent: IL matches TAXSIM at 100%
within $100 (federally aligned records, 2017–2020) and PolicyEngine at
99.2–99.5% (2022–2024); NH/TN Hall tax matches TAXSIM exactly 2017–2020;
PE's CO pre-refund liability matches our independent encoding exactly.
