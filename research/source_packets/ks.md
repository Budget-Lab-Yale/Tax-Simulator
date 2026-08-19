# Kansas State Source Packet

State: `KS`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-12`

> **Status note (as of 2026-08-12), kept from the packet's former Status line:**
> baseline encoded; worksheet tests KS-1..KS-5 pass

## Scope

- Tax years covered: 2017-2025; 2024 SB 1 structure ("and all tax years
  thereafter") carries forward to 2026+.
- Resident individual income tax only (K-40). No nonresident allocation
  (Schedule S Part B), no PTE credit.
- Major features: federal-AGI start, rolling conformity; SS subtraction with
  a $75,000 federal-AGI CLIFF through 2023 (unlimited 2024+); graduated
  3-bracket schedule with a low-income ZERO-TAX CLIFF through 2023 (encoded
  via the OH-style base_amounts family); two-bracket 5.2/5.58% from 2024
  (SB 1, 2024 special session); SB 30 (2017) RETROACTIVE mid-year rate raise
  and pass-through-exemption repeal — 2017 encodes final enacted law;
  component-style itemized deduction with the SB 30 50/75/100% phase-in and
  no income/sales-tax component; per-person exemptions with an extra HoH
  exemption ALL years (not just 2024); 17% refundable EITC; CDCTC match
  0/12.5/18.75/25/50%; food sales tax credit documented-not-modeled.

## Primary sources

- K-40 booklets (ksrevenue.gov/pdf/ip{YY}.pdf), transcribed via pdftotext:
  ip17, ip18, ip19, ip21, ip23, ip24, ip25 (2020/2022 bridged by statute
  continuity + PE cross-check; no legislation changed the bridged values).
- [Notice 24-08](https://www.ksrevenue.gov/taxnotices/notice24-08.pdf) — SB 1
  (2024 special session): exact statutory schedule ("Over $23,000: $1,196
  plus 5.58% of excess"), SS limit removal, std ded, exemptions.
- Statutes: K.S.A. 79-32,110 (rates), 79-32,117 (modifications; (c)(xviii)
  SS), 79-32,119 (standard deduction), 79-32,120 (itemized), 79-32,121
  (exemptions), 79-32,205 (EITC), 79-32,111c (CDCTC), 79-32,271 (food credit).
- Session laws: SB 30 (2017; retroactive TY2017 rates, pass-through
  exemption repeal, itemized/CDCTC phase-ins), SB 50 (2021; std ded raise,
  itemize-decoupling), HB 2106 (2022; food sales tax phase-out), SB 1 (2024
  special session).

## Secondary cross-checks

- PolicyEngine US local package (gov/states/ks/tax/income/): matches primary
  on std ded, extra std, EITC 17%, CDCTC vintages, SS $75k limit, food
  credit params. DISAGREEMENTS (primary wins): PE has no 2017 rate schedule
  at all; 2024 bracket thresholds off by $1 (23,001/46,001 vs statutory
  "over $23,000/$46,000"); HoH additional exemption missing pre-2024; 2024+
  "zero_tax_threshold" wrongly carries the gross-income minimum-filing table
  as a taxable-income zero-tax rule (the 2024 tax table taxes from $26 —
  SB 1 repealed the $2,500/$5,000 rule). Pre-registered for the PE window.

## Parameter inventory

- `agi.yaml`: start_point 1; rolling conformity (group 0); non-KS muni
  addback with own-state carve-out (Sch S A1); US-obligation interest flag
  (A11, not modeled); state income-tax refund subtraction (A12); SS
  subtraction of TAXABLE benefits, all ages, federal-AGI cliff $75,000
  2017-2023 (79-32,117(c)(xviii)), unlimited 2024+ (SB 1 sec. 18).
- `ded.yaml`: std ded 3,000/7,500/5,500 (2017-2020), SB 50 3,500/8,000/6,000
  (2021-2023), SB 1 3,605/8,240/6,180 (2024+); aged/blind add-ons $850
  single/HoH, $700 MFJ/MFS per instance (constant, worksheet-verified 2017/
  2021/2024); dependent-filer std = max($500, earned) capped at the base;
  itemized: component style (medical/mortgage/property/charity ONLY;
  income/sales taxes never deductible; federal SALT cap does NOT apply),
  SB 30 phase-in via fractional item_include_* shares; federal-itemizer
  gate 2017-2020 (79-32,120 pre-SB 50), free election 2021+ (SB 50).
- `exempt.yaml`: $2,250 per person through 2023; HoH one EXTRA exemption in
  ALL years (booklet-verified 2017/2019/2021/2024/2025; PE misses this
  pre-2024); SB 1 (2024+): $9,160/taxpayer ($18,320 MFJ), HoH extra $2,320
  (form-level; statute silent — form governs), $2,320 per dependent;
  dependent filers zero exemptions.
- `ord.yaml`: 2017 (SB 30, retroactive): 2.9/4.9/5.2% at 15k/30k (30k/60k
  MFJ); 2018-2023: 3.1/5.25/5.7% same thresholds; 2024+ (SB 1): 5.2/5.58%
  at 23k (46k MFJ); MFS/HoH use the "all other individuals" schedule.
  Low-income ZERO-TAX CLIFF through 2023 encoded via base_amounts (cliff
  thresholds $5,000/$12,500 in 2017; $2,500/$5,000 2018-2023; REPEALED
  2024+). Verified against booklet tax-table rows and worksheets.
- `credits.yaml`: EITC 17% refundable all years (79-32,205); CDCTC match
  0 (2017) / 12.5 (2018) / 18.75 (2019) / 25 (2020-2023) / 50% (2024+),
  nonrefundable; food sales tax credit DOCUMENTED-NOT-MODELED ($125 per
  qualified exemption, AGI <= $30,615, 55+/disabled/child-under-18 gates
  not representable; ends TY2025).
- `filing.yaml`: req_type 2 with booklet under-65 gross-income thresholds
  by status/year + req_if_fed_filer 1.

## Worksheet tests

- KS-1: 2017 single top-bracket SB 30 schedule (worksheet constant $390).
- KS-2/KS-2b: 2017 MFJ zero-tax cliff pair straddling $12,500.
- KS-3: 2024 SB 1 two-bracket + new std/exemptions + 50% CDCTC.
- KS-4/KS-4b: 2021 HoH 17% EITC + SS $75k cliff pair (granted/denied).
- KS-5: 2018 itemized component phase-in (50% mortgage/property).

## Known differences

- Food sales tax credit omitted (2017-2024): overstates liability for
  eligible low-AGI units by <= $125 x exemptions (capped at remaining
  liability); ~O($10M)/yr statewide. TAXSIM and PE both model it ->
  expected cross-model point masses at $125 multiples. Gone TY2025.
- KPERS: retiree benefit subtraction (A13) and employee contribution
  ADDBACK (A2) both unobservable; omitted in offsetting directions.
- US-obligation interest subtraction flagged, not subtracted (model-wide).
- Learning Quest 529, military pay/pension, disabled-veteran exemption
  ($2,250), first-time homebuyer accounts: unobservable, omitted.
- Tax-table midpoint rounding for TI <= $100,000 (+-$3).
- Federal QSS units keep the MFJ schedule/std here; the K-40 files QSS as
  HoH (small, directionally mixed).
- Dependent-filer std rule (Worksheet II) printed through 2019 booklets
  only; encoded all years pending a direct read of current 79-32,119 (see
  uncertainty note).
- Boundary convention: at TI exactly at a cliff threshold the form gives 0;
  st_band_index_lower places the exact bound in the taxed band
  (measure-zero in continuous microdata; OH convention).
- 2026+: SB 269 (2025) revenue-triggered rate cuts are contingent and NOT
  encoded (SB 1 rates carry forward).

## Cross-model validation notes

- TAXSIM 2017-2020 / PE 2021-2024. Pre-registered wedges: the food-credit
  omission (both models grant it); PE's 2024+ zero-tax-threshold bug,
  missing pre-2024 HoH exemption, absent 2017 schedule, $1-off 2024
  brackets.
- Aggregate: blocked on weights; compare KS DOR annual report individual
  income tax receipts and HT2.
