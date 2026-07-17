# Connecticut State Source Packet

State: `CT`
Status: `baseline encoded; worksheet tests complete`
Last updated: `2026-07-17`

## Scope

- Tax years covered: 2017-2035. Published parameters transcribed from all
  nine CT-1040 instruction booklets 2017-2025; nothing is inflation-indexed
  (Tables A and E are hash-identical across all nine booklets; Tables C/D and
  the rate schedule changed exactly once, TY2024 under PA 23-204), so
  post-2025 years carry the 2025 law forward except the enacted 2026 IRA
  phase-in step.
- Full-year resident IIT baseline only. Nonresident/part-year apportionment
  (CT-1040NR/PY), AMT-related credits, and the Schedule 2 credit for taxes
  paid to other jurisdictions are excluded.
- Major features: federal-AGI start with retirement-income subtractions;
  per-return personal exemption with a stepped dollar-for-dollar phase-out;
  graduated 7-bracket schedule with a lowest-rate phase-out add-back (Table
  C) and cumulative stepped tax recapture (Table D, two tiers through 2023
  and three from 2024); percentage-of-tax personal credit (Table E); CT EITC
  match; capped property tax credit.

## Primary sources

- DRS Form CT-1040 instruction booklets, TY2017-TY2025 (all nine opened and
  machine-verified; Tax Calculation Schedule and Tables A-E transcribed in
  full from the 2017 and 2025 booklets and hash-compared across years):
  - 2017: portal.ct.gov /-/media/drs/forms/1-2017/income/booklets/ct1040onlineinstructionbooklet1217pdf.pdf (NBER mirror: taxsim.nber.org/historical_state_tax_forms/CT/2017/)
  - 2018: /-/media/DRS/Forms/1-2018/Income/CT-1040-Online-Booklet_1218.pdf
  - 2019: /-/media/DRS/Forms/2019/Income/CT-1040-Online-Booklet_1219.pdf
  - 2020: /-/media/DRS/Forms/2020/Income/CT-1040-Online-Booklet_0221.pdf
  - 2021: /-/media/DRS/Forms/2021/Income/CT-1040-Online-Booklet_1221.pdf
  - 2022: /-/media/DRS/Forms/2022/Income/2022-CT-1040-Instructions_1222.pdf
  - 2023: /-/media/drs/forms/2023/income/2023-ct-1040-instructions_1223.pdf
  - 2024: /-/media/drs/forms/2024/income/2024-ct-1040-instructions_1224.pdf
  - 2025: /-/media/drs/forms/2025/income/2025-ct-1040-instructions_1225.pdf
- DRS Tax Information page (TY2025 Who Must File / Gross Income Test):
  portal.ct.gov/drs/individuals/resident-income-tax/tax-information
- CT OLR reports (quasi-primary for law history): 2024-R-0130 (income tax
  guide), 2025-R-0080 (rate history since 1991), 2019-R-0098 and
  2023-R-0129 / 2025-R-0152 (retirement subtractions), 2025AA-0093 (2025
  acts). Statutes: CGS 12-700, 12-701(a)(20), 12-702, 12-703, 12-704c,
  12-704e; PA 17-2 JSS, PA 21-2 JSS, PA 22-118, PA 23-204, PA 25-168.
- Full research record (agent transcriptions with verification method):
  `../raw/ct_research_core.md` and `../raw/ct_research_mods_credits.md`.

## Parameter inventory

- `agi.yaml`: federal-AGI start (rolling conformity, group 0); non-CT muni
  interest addback (own-state exempt); US-interest and state-refund
  subtraction flags; SS subtraction (100% below AGI limits with the
  25%-of-gross taxable cap above; limits $50k/$60k 2017-18, $75k/$100k
  2019+, HoH grouped with MFJ); pension/annuity share (14/28/42% 2019-21,
  100% 2022+) and IRA share (25/50/75% 2023-25, 100% 2026) with the shared
  AGI-banded factor (cliff through 2023, PA 23-204 ten-band step table
  2024+, HoH grouped with single); CHET 529 caps (inert, data-limited).
- `ord.yaml`: 7-bracket rate schedule (3/5/5.5/6/6.5/6.9/6.99% through 2023;
  2/4.5/... from 2024); stepped recapture segments [Table C, Tier 0 (2024+),
  Tier 1, Tier 2] as filing-status-mapped start/incr/amount/max vectors.
- `exempt.yaml`: per-return personal exemption 15,000/24,000/12,000/19,000
  (S/MFJ/MFS/HoH), stepped $1,000-per-$1,000 phase-out on CT AGI from
  30,000/48,000/24,000/38,000 (po_type 1, po_agi_base 2 = state AGI).
- `credits.yaml`: Table E percentage-of-tax credit (28 CT-AGI bounds x 27
  rates, 75% down to 1%, identical all years); CT EITC match
  (23%/30.5%/40%) + $250 qualifying-child bonus (2025+); property tax
  credit ($200 restricted 2017-21, $300 all filers 2022+, 15% per
  $10,000/$5,000-MFS step over frozen thresholds, on CT AGI).
- `ded.yaml`: zeros -- CT has no standard or itemized deductions.
- `filing.yaml`: Gross Income Test thresholds 15,000/24,000/12,000/19,000
  (all years), approximated as a state-AGI test (req_type 2); no
  federal-filer rule.

## New generic calculator components introduced by CT

1. Stepped recapture segments (`st_ord.step_recap_*`, calc_st_tax): sum over
   segments of min(ceil((state AGI - start)/incr) x amount, max). Encodes
   CT Tables C and D exactly; reusable for any stepped add-back.
2. Stepped exemption phase-out (`st_exempt.po_type = 1` with po_step,
   po_reduction_per_step, po_agi_base): CT Table A; the po_agi_base switch
   also lets any state phase out on state rather than federal AGI.
3. Percentage-of-tax credit table (`st_credits.pct_credit_*`, calc_st_credits):
   CT Table E; band lookup on state AGI applied to tax before credits.
4. Share-based retirement subtraction with AGI-banded factor
   (`st_agi.pension_sub_share`, `st_agi.ira_sub_share`,
   `st_agi.retire_sub_factor_*`): phase-in shares times a step-table factor
   that expresses both the 2019-2023 cliff and the 2024+ phase-out table.
5. Taxable-SS gross cap (`st_agi.ss_taxable_gross_cap_share`): CT's
   25%-of-benefits cap above the SS AGI limit.
6. CT-style capped property tax credit (`st_credits.prop_tax_credit_max`,
   `_po_thresh/_po_step/_po_rate`, `_restrict_aged_dep`) alongside the IL
   rate-style credit.
7. Flat EITC child bonus (`st_credits.eitc_child_bonus`): per-return $250
   for EITC claimants with a federal qualifying child (2025+).

## Worksheet tests

- CT-1: 2024 single, mid-income: Table A phase-out band, 2%/4.5% schedule,
  Table E rate, no recapture.
- CT-2: 2025 single high-income: Table C add-back max, Table D tiers 0+1
  stepped recapture, no Table E credit.
- CT-3: 2017 MFJ: old rates, Table C add-back, exemption, Table E 2017
  identity check.
- CT-4: 2025 MFJ retirees: SS 25%-cap above threshold, pension/IRA shares
  with the 0.85 phase-out factor, property tax credit reduction.
- CT-5: 2023 MFJ low-income: full exemption, 75% Table E credit, 40% EITC.
- CT-6: 2025 EITC child bonus and MFS Table C increments.

## Known differences

- SS worksheet approximation: the subtraction above the AGI limit uses
  max(0, taxable SS - 0.25 x gross benefits); the form caps CT-taxable SS at
  25% of min(gross benefits, federal excess over base). Above the CT AGI
  limits the excess-over-base is essentially always larger than gross
  benefits, so the approximation binds only in contrived cases.
- Military retirement (100% all years), railroad retirement (100%), and
  Teachers' Retirement System income (25% 2017-20, 50% 2021+; take-the-better
  vs the pension deduction from 2021) are unobservable subsets of pension
  income in the PUF; all pensions are treated as Line 48b private pensions.
  This understates subtractions pre-2022 and for above-threshold filers.
- Property tax credit uses salt_prop (real-estate tax), which underobserves
  non-itemizers' property tax and omits motor-vehicle tax; the credit is
  therefore undercounted (cap is only $200-$300).
- Filing requirement approximates the Gross Income Test with state AGI
  (req_type 2); subtractions make state AGI smaller than gross income, so
  some retiree filers with zero liability are not flagged as filers.
- US-obligation interest (Line 39) is flagged but not subtracted (share of
  taxable interest unobserved; module-wide convention).
- CHET/ABLE contributions, bonus-depreciation/179 addback-recovery cycles,
  CT-bond gain/loss modifications, cannabis-expense subtraction, and niche
  Schedule CT-IT credits (stillbirth, PE tax credit, angel investor, etc.)
  are data-limited or out of scope; documented only.
- The December 2021 retroactive enhancement of the 2020 CT EITC to 41.5%
  was an off-return administrative payment; 2020 law is encoded at 23%.
- 2026+: IRA share reaches 100% (encoded); the TY2026 family child care home
  credit ($500, PA 25-168 372) is not modeled; re-verify when the 2026
  booklet publishes.

## Batch role and validation

- Completes the NY/CT graduated-federal-AGI recapture cohort. NY's
  recapture is a continuous phase-in (worksheet identity); CT's is stepped
  band tables -- the two implementations are independent calc paths.
- Cross-model: compare 2017, 2021, 2024, and 2025 resident returns against
  TAXSIM/PolicyEngine, focusing on the recapture bands, Table E steps, and
  the 2024 pension/IRA phase-out factors.
- Aggregate: after weights land, compare liability, CT EITC, and property
  tax credit totals to DRS annual reports and SOI HT2; expect property tax
  credit undercount per known differences.
