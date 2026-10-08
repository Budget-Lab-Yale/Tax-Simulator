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

### T28. New Jersey 2020: the 10.75% bracket still starts at $5 million

P.L. 2020, c. 95 (the 2020 "millionaires tax") moved New Jersey's 10.75% bracket floor from $5,000,000 down to
$1,000,000 for tax year 2020 (NJ-1040 Tax Rate Schedules, Tables A and B, TY2020 booklet). TAXSIM-35 uses the 2019
schedule for 2020. Probe (2026-10-01), single, $2,000,000 of wages (NJ taxable income $1,999,000): `siitax` =
164,184.05 in both 2019 and 2020; the TY2020 schedule gives $17,782 more. Effect: TAXSIM understates 2020 New Jersey
tax by 1.78% of taxable income between $1 million and $5 million.

### T29. Arkansas $6,000 retirement exemption applied only at 65 and over

Arkansas exempts the first $6,000 of retirement benefits per taxpayer (Ark. Code Ann. 26-51-307). For employer-related
plans there is no age test (AR1000F instructions: "The recipient does not have to be retired"); traditional IRA
distributions qualify from age 59 1/2. TAXSIM-35 applies the exemption only at 65 and over. Probe (2026-10-01), 2019
single, $20,000 of wages and $20,000 of pension: v32 = 40,000 at ages 59, 60, 62 and 64, and 34,000 at 65 and 66.
Effect: TAXSIM overstates Arkansas tax for retirees under 65 by up to $6,000 x the rate per taxpayer.

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

## Corroboration worth passing along

Where concepts align, agreement is excellent: IL matches TAXSIM at 100%
within $100 (federally aligned records, 2017–2020) and PolicyEngine at
99.2–99.5% (2022–2024); NH/TN Hall tax matches TAXSIM exactly 2017–2020;
PE's CO pre-refund liability matches our independent encoding exactly.
