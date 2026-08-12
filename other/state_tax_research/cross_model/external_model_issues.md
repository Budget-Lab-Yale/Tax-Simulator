# Potential issues in TAXSIM-35 and PolicyEngine US

Findings from the Tax-Simulator state cross-model validation harness
(Budget Lab at Yale, 2026-07-18) that may be worth reporting upstream —
either genuine errors, or intended behavior worth a documentation
clarification. Each item was verified at the record level against state
forms/statutes; reproduction details are available from the harness
(`other/state_tax_research/cross_model/`, per-record output on request).

Versions tested: TAXSIM-35 as bundled in `usincometaxes` 0.7.1 (local WASM
build); `policyengine-us` 1.775.7.

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

### P5. One-time rebates netted into eligibility-year `state_income_tax`
(NY, VA, GA, AZ — generalizing P2)

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

Whether eligibility-year booking is right is a concept choice (the checks
arrive one to two calendar years later), but as with P1/P2 it makes
`state_income_tax` diverge from the recurring-liability concept most
revenue analysis uses, and it does so retroactively for years that were
already final. A uniform pre-rebate liability output would resolve the
whole class.

## Corroboration worth passing along

Where concepts align, agreement is excellent: IL matches TAXSIM at 100%
within $100 (federally aligned records, 2017–2020) and PolicyEngine at
99.2–99.5% (2022–2024); NH/TN Hall tax matches TAXSIM exactly 2017–2020;
PE's CO pre-refund liability matches our independent encoding exactly.
