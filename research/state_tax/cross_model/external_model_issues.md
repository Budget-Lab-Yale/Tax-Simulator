---
title: "Potential issues in TAXSIM-35 and PolicyEngine US"
role: evidence
workstream: state_tax
status: current
updated: 2026-08-22
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

## PolicyEngine US

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
favourably. We have NOT checked the other twenty against their own statutes,
and several are cases where the state genuinely does restrict by age (a
child credit limited to children under 17, for instance), so the count of 23
is an upper bound on the problem, not a claim. The three above are verified.

Effect where it bites: PolicyEngine's state liability runs HIGH by the
per-dependent amount on returns claiming a dependent aged 18 or over. On our
samples that is 47-59% of such returns in IL and the dominant residual in
all three states; excluding the class moves IL to 0.993-0.995, CA to
0.979-0.988 and UT to 0.970-0.988 on the clean subset.

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

## Corroboration worth passing along

Where concepts align, agreement is excellent: IL matches TAXSIM at 100%
within $100 (federally aligned records, 2017–2020) and PolicyEngine at
99.2–99.5% (2022–2024); NH/TN Hall tax matches TAXSIM exactly 2017–2020;
PE's CO pre-refund liability matches our independent encoding exactly.
