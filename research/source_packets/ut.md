# Utah State Source Packet

State: `UT`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-22`

> **Status note (as of 2026-08-11), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete

## Scope

- Tax years covered: 2017-2035. Published TC-40 values transcribed through 2025;
  enacted 2026 changes (SB 60 rate 4.45%, HB 290 CTC thresholds) encoded; 2026+
  indexed values projected by the CPI rule.
- Resident individual income tax only.
- Major features: flat rate on Utah taxable income built from federal AGI with
  NO deductions/exemptions in the base; the Taxpayer Tax Credit (6% of federal
  deduction + Utah personal exemption, phased out 1.3 cents/$); retirement and
  Social Security credits (mutually exclusive, 2.5 cents/$ phase-outs);
  nonrefundable EITC (2022+) and young-child CTC (2024+).

## Primary sources

- TC-40 instruction booklets 2017-2025:
  `https://files.tax.utah.gov/tax/forms/{year}/tc-40inst.pdf` (2025 at
  `.../current/tc-40inst.pdf`); TC-40/TC-40A/TC-40C form PDFs; incometax.utah.gov
  credit pages.
- Utah Code Title 59 Ch. 10: §59-10-104 (rate), §59-10-114 (additions/
  subtractions), §59-10-1018 (taxpayer tax credit), §59-10-1019 (retirement
  credit), §59-10-1042 (SS credit), §59-10-1043 (military retirement credit),
  §59-10-1044 (EITC), §59-10-1047 (CTC), §59-10-1005 (at-home parent).
- Enrolled bills (le.utah.gov): HB 293 (2018), HB 2003 (2018 2nd SS), SB 153
  (2021), HB 86 (2021), SB 11 (2021), SB 59 (2022), HB 54 (2023), HB 170 (2023),
  HB 153 (2024), SB 69 (2024), HB 106 (2025), SB 71 (2025), SB 60 (2026),
  HB 290 (2026). All rate/credit changes are retroactive to Jan 1 of the tax year.

## Structure (TC-40, identical flow 2017-2025)

FAGI (line 4) + additions (TC-40A Part 1) − state tax refund in FAGI (line 7)
− subtractions (TC-40A Part 2) = Utah taxable income (line 9) × flat rate =
tax (line 10) − taxpayer tax credit (lines 11-20) − apportionable nonrefundable
credits (TC-40A Part 3: retirement 18, SS AH, military AJ, EITC AM, CTC AT,
my529 20) − nonapportionable credits (Part 4). Federal deductions/exemptions
enter ONLY through the taxpayer tax credit.

## Parameter inventory

### `ord.yaml` — flat rate (§59-10-104)

5.00% (2017), 4.95% (2018, HB 293), 4.85% (2022, SB 59), 4.65% (2023, HB 54),
4.55% (2024, SB 69), 4.50% (2025, HB 106), 4.45% (2026+, SB 60). Each verified
in the year's booklet line 10 and the enrolled bill.

### `agi.yaml` — base construction (§59-10-114)

- Federal AGI start (line 4).
- Addition: non-Utah municipal bond interest (code 57; post-1/1/2003 bonds,
  reciprocity carve-out — approximation: full exempt-interest addback with
  own-state exemption).
- Subtractions: US-government-obligation interest (code 71); state tax refund
  (line 7, §59-10-114(2)(c)).
- Omitted (documented): lump-sum distribution addition (Form 4972 only), MSA/
  my529 addbacks, Native American/railroad/military-nonresident subtractions,
  FDIC premiums, previously-taxed retirement income.

### `ded.yaml` / `exempt.yaml`

None — no state deduction or exemption reduces taxable income (neutral
defaults). The federal deduction and Utah exemption live in `credits.yaml`.

### `credits.yaml`

**Taxpayer Tax Credit (§59-10-1018)** — 6% × [Utah personal exemption + federal
std deduction OR (federal itemized − state/local income tax deducted, capped at
$10,000 from 2018; uncapped 2017)] − $.013 per $1 of Utah taxable income (line
9) over the base amount. Nonrefundable, no carryforward. Rates constant 2017-2025.

- Utah personal exemption: 2017 $3,038 per federal exemption (75% × $4,050;
  taxpayer + spouse + dependents; federal PEP interaction irrelevant — the
  1.3% phase-out zeroes the credit far below PEP thresholds). 2018+ dependents
  only (IRC §24-eligible incl. other-dependents): $565 (2018, HB 2003), $579
  (2019), $590 (2020), $1,750 (2021, SB 153), $1,802 (2022), $1,941 (2023),
  $2,046 (2024), $2,111 (2025). 2023+ newborns count twice (HB 54 2023;
  modeled as dependents age 0).
- Base amounts (single & MFS / HoH / MFJ & QSS): 2017 13,978/20,968/27,956;
  2018 14,256/21,384/28,512; 2019 14,601/21,902/29,202; 2020 14,879/22,318/
  29,758; 2021 15,095/22,643/30,190; 2022 15,548/23,322/31,096; 2023 16,742/
  25,114/33,484; 2024 17,652/26,478/35,304; 2025 18,213/27,320/36,426.
  Indexed annually (see Indexation); MFJ = 2 × single statutorily.

**Retirement credit (§59-10-1019, code 18)** — $450 per person born on/before
Dec 31 1952 (frozen cohort → encode as year-keyed minimum age = year − 1952);
combined credit reduced $.025 per $1 of MAGI (total income + tax-exempt
interest, approximated as FAGI + state additions) over $25,000 single /
$16,000 MFS / $32,000 MFJ, QSS & HoH. Not indexed; identical 2017-2025.

**Social Security benefits credit (§59-10-1042, code AH; HB 86 2021, TY2021+)** —
credit = year's tax rate × taxable SS in AGI; reduced $.025 per $1 of MAGI over:
2021 30,000/25,000/50,000 (single/MFS/MFJ & HoH); 2022 37,000/31,000/62,000
(SB 59); 2023-24 45,000/37,500/75,000 (HB 54); 2025+ 54,000/45,000/90,000
(SB 71). Not indexed. Return-level mutual exclusivity with the retirement
credit (booklets bar claiming both; model takes the larger).

**Utah EITC (§59-10-1044, code AM; SB 59 2022)** — 15% of federal EITC (2022),
20% (2023+, HB 54); nonrefundable, no carryforward. 2023+ additionally capped
at total W-2 wages (worksheet: lesser of 20% × federal EITC or Utah box-16
wages — SE-only filers get zero from 2023; modeled as cap at wages1+wages2).
No intergenerational-poverty restriction ever operative (HB 307 2022 failed).

**Utah CTC (§59-10-1047, code AT; HB 170 2023, TY2024+)** — $1,000 nonrefundable
per qualifying child (IRC §24-eligible): ages 1-3 in 2024; ages 0-5 from 2025
(HB 106). Reduced $.10 per $1 of MAGI over $43,000 single & HoH / $27,000 MFS /
$54,000 MFJ; 2026+ $49,000 / $30,500 / $61,000 (HB 290). Not indexed.

**Omitted credits (documented):** my529 (rate × capped contributions; per-year
caps live on TC-675H, not pinned), at-home parent ($100/child ≤12 months,
earned income ≤$3,000, FAGI ≤$50,000), health benefit plan, military retirement
credit (§59-10-1043 — would double-count with the SS/retirement exclusivity
choice; PUF cannot identify military pensions), solar/historic/other.

### `filing.yaml`

Must file if required to file federally (all years) → federal-filer proxy.
"Qualified exempt taxpayer" (line 21: no tax if FAGI ≤ federal standard
deduction; 2017 + federal exemptions; 2025 + enhanced senior deduction) is NOT
separately modeled — the taxpayer tax credit already zeroes standard cases;
documented approximation for itemizer/addition edge cases.

## Indexation (§59-10-1018(5),(6))

Taxpayer-credit base amounts (single, HoH) and the personal exemption index
annually: CPI (IRC §1(f)(4)-(5) chained-CPI machinery) of the preceding year
vs base year, nearest $1, increase-only; MFJ base = 2 × single. Base year 2007
through TY2020 values; SB 153 (2021) reset base to CPI-2020 (listed $15,095/
$22,643/$30,190; exemption $1,750). Encoding: transcribe published 2017-2025
values; 2026+ index from the 2025 anchor with the model's CPI series
(projection, documented). Rate, retirement/SS/EITC/CTC parameters not indexed.

## Worksheet tests to add

- Taxpayer credit, standard-deduction filer with phase-out partially binding
  (hand-computed lines 11-20).
- Itemizer with SALT income-tax addback at the $10,000 cap (2018+) vs uncapped
  2017.
- 2023+ newborn double exemption (dependent age 0).
- Retirement vs SS credit exclusivity: 65+ filer with taxable SS where the SS
  credit wins; frozen-cohort boundary (age 72 in 2024 eligible, 71 not).
- EITC wage cap: 2023 SE-only filer (credit = 0) vs wage filer.
- CTC age bands: 2024 (age 0 ineligible, 1-3 eligible) vs 2025 (0-5); MAGI
  phase-out arithmetic.

## Known differences

- MAGI for retirement/SS/CTC phase-outs approximated as FAGI + state additions
  (tax-exempt interest beyond the code-57 addback not fully reconstructed).
- EITC W-2 wage cap uses total wages (Utah-source share unobserved; residents
  assumed all-Utah).
- Newborn double-count proxied by dependents age 0 (mid-year adoptions etc.
  unobserved).
- Municipal-interest reciprocity carve-out and post-2003 acquisition limit not
  observable.
- Qualified-exempt-taxpayer line 21 floor not modeled (see filing.yaml note).
- 2026+ base amounts/exemption are CPI projections until the 2026 booklet.
- my529, at-home parent, health, military retirement, solar credits omitted.
- 2017 PEP-reduced federal exemption interaction ignored (phase-out zeroes the
  credit below PEP range).

## Triage 2026-08-22 — localized to the taxpayer tax credit on federal itemizers; NOT closed

UT was not fixed in this round. What is now known, so the next attempt does not
repeat the search:

**The whole difference is the taxpayer tax credit.** State AGI and state
taxable income agree with TAXSIM to the dollar (median `st_agi - v32` = 0.0,
median `st_txbl_inc - v36` = 0.0) in both the miss and match groups. Our
`st_ded_credit` is lower than TAXSIM's `v40` by a median $280 in 2017 and $330
in 2018, and `diff == -(our credit - their credit)` on 1,296 of 1,633 misses.
We zero the credit on 468 records where TAXSIM does not; TAXSIM zeroes it on 41.

**It is entirely federal itemizers.** Splitting the 2017 clean subset:

| federal `itemizing` | n | credit gap > $1 | match@$100 miss rate |
|---|---|---|---|
| FALSE | 4,995 | 263 | **0.04%** |
| TRUE | 4,165 | 2,172 | **39.2%** |

Non-itemizers are essentially perfect. That is the shape to expect, because
UC 59-10-1018(1) builds the credit base from the **federal** deduction --
itemized less the state/local income-tax addback, or the standard deduction --
so anything that moves TAXSIM's federal itemized deduction moves the Utah
credit even though Utah itself has no state itemization.

**The formula is confirmed correct on both sides for non-itemizers.** Probed
against TAXSIM on hand-computable cases (`output/probe/ut_credit_probe.R`):
single $50k/0 dep gives credit 94.96 and tax 2,405.04, reconciling exactly with
6% x (6,350 + 0.75 x 4,050) - 1.3% x (50,000 - 13,978). Cases with dependents
and joint filers reconcile too, confirming the $13,978 / $20,968 / $27,956 base
amounts and that exemptions count taxpayer, spouse and dependents at 75% of
$4,050.

**Two candidate mechanisms, NOT separated.**
1. TAXSIM's federal itemized deduction differs from ours by construction --
   SALT circularity (it iterates federal-state three rounds using its own
   computed state income tax) and the Pub. 600 sales-tax imputation, both
   already ALL-state `annotate` rows. Either changes the credit base.
2. Our own addback may be too wide. UC 59-10-1018(1)(e) adds back state or
   local **income** tax; `st_credits_household.R` strips
   `salt_item_ded - salt_prop - salt_pers`, which is the PUF's combined
   income-or-sales field. For a filer who elected the sales-tax deduction,
   Utah requires no addback and we over-strip, shrinking the base and the
   credit -- the right sign, since 1,545 of 1,633 misses are positive.

Inverting the credit formula on real records gives an implied base gap of 0.92
/ 1.19 / 1.42 / 1.64 exemption-equivalents at 0-3 dependents -- smoothly
growing, so **not** a missing exemption count, which was the first hypothesis
and is wrong.

**Deliberately not excluded.** The exposed population is every federal
itemizer, 45% of the clean subset, and 61% of them match anyway. Excluding all
of them would clear the cell while hiding an unresolved question about our own
addback, which is the wrong trade. Next step is to separate mechanism 2 from
mechanism 1 -- compare our `ded_credit_salt` against the state income tax
TAXSIM actually deducted, on records where the two federal itemized deductions
otherwise agree.

## Cross-model validation notes

- TAXSIM years: 2018 (post-TCJA restructure), 2021 (SB 153 exemption jump +
  new SS credit), 2023 (EITC 20% + wage cap), 2025.
- Expect mismatches: TAXSIM's UT MAGI construction, SS/retirement exclusivity
  election, CTC age bands.

## Aggregate validation notes

- HT2 state × AGI-class totals once weights land; Utah State Tax Commission
  annual report individual income tax collections benchmark.

## Corrections vs. earlier working notes (agent-verified)

2018 rate bill = HB 293 (not HB 54 2018); 2020 exemption $590 (not $606); 2017
base amounts 13,978/27,956 (not 13,805/27,610); $1,750 exemption = SB 153 2021
(not HB 1 2020 5th SS); newborn double = HB 54 2023 (not SB 45); EITC = SB 59
2022 (HB 307 failed; no IGP restriction); SS credit = HB 86 2021 (SB 11 2021 =
military retirement credit); CTC phase-out = $0.10/$1; 2025 SS MFS threshold
$45,000.
