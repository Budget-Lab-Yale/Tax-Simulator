# West Virginia State Source Packet

State: `WV`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-12`

> **Status note (as of 2026-08-12), kept from the packet's former Status line:**
> baseline encoded; worksheet tests WV-1..WV-7 pass

## Scope

- Tax years 2017-2025, plus the enacted TY2026 rate schedule. Every rate,
  bracket, exemption amount and Family Tax Credit anchor is transcribed from
  the published IT-140 booklet for that year -- all nine years, no gaps and no
  interpolation.
- Resident individual income tax only (IT-140). No nonresident/part-year
  allocation, no IT-140NRC composite, no PTET.
- Major features: federal-AGI start; **NO standard deduction and NO itemized
  deduction of any kind**; flat $2,000-per-exemption allowance ($500 for a
  zero-exemption filer); five brackets with an exact half-bracket MFS mirror;
  a percentage-of-tax **Family Tax Credit** keyed to the federal poverty
  guideline (the PA Schedule SP shape exactly); a two-track Social Security
  phase-in; an $8,000-per-person senior modification REDUCED by the SS
  subtraction claimed; no state EITC and no state CTC in any year.

## HEADLINE: the 2023-2025 rate sequence, and a corrected premise

**There was NO 2024 rate cut.** The TY2023 and TY2024 IT-140 "TAX RATE
SCHEDULES" pages are identical in rate content, and the two years' tax tables
print the same dollar amounts in every row (taxable 5,900-6,000 -> $140 in
both; 2025 -> $132). The HB 2526 rates ran for two years. A reviewer or
harness expecting a 2024 change is mistaken.

| TY | Statute | Rate Schedule I | Verified |
|---|---|---|---|
| 2017-2022 | 11-21-4e | 3 / 4 / 4.5 / 6 / 6.5% | Primary, each booklet |
| **2023-2024** | **11-21-4g** (HB 2526, 2023) | **2.36 / 3.15 / 3.54 / 4.72 / 5.12%** | Primary, both booklets |
| **2025** | **11-21-4i** (SB 2033, 2024 2nd Special Session) | **2.22 / 2.96 / 3.33 / 4.44 / 4.82%** | Primary, 2025 booklet |
| 2026 | 11-21-4j (SB 392, 2026 RS) | 2.11 / 2.81 / 3.16 / 4.22 / 4.58% | Statute |

**The TY2025 rates are LEGISLATED, not certified.** 11-21-4i(f) records that
the August 2024 trigger certification under 11-21-4h had produced a 4% cut for
TY2025 and that SB 2033 **supersedes** both 11-21-4g and that administrative
notice, delivering a 6% cut off the 4g rates instead. Anyone reconstructing
2025 from the certification alone lands on 2.27/3.02/3.40/4.53/4.92 and is
~2% wrong. Nothing in TY2017-2025 rests on an uncertified or projected rate.

The 11-21-4h trigger first tests collections on 2026-08-15 and applies from
TY2027, capped at 10% of the 4e rates -- contingent, not projected.

MFS (Rate Schedule II) is the exact half-bracket mirror in every year:
thresholds 5,000/12,500/20,000/30,000, base amounts 150/450/787.50/1,387.50
(2017-22), 118/354.25/619.75/1,091.75 (2023-24), 111/333/582.75/1,026.75
(2025). Arithmetic checks against the booklets' own EXAMPLE boxes: TY2023
taxable 117,635 -> 2,183.50 + 5.12% x 57,635 = 5,134.41; TY2025 -> 2,053.50 +
4.82% x 57,635 = 4,831.51, both exactly as printed.

**Two statute citations corrected during this research**: the Family Tax
Credit is **11-21-22 / 22a / 22b** (11-21-21 is the Senior Citizens' *property
tax* credit), and the senior modification is **11-21-12(c)(9)** (not (c)(8),
which is Social Security).

## Primary sources

- Form IT-140 "Personal Income Tax Forms and Instructions" booklets, all nine
  years (tax.wv.gov, fetched directly; no NBER mirror needed), transcribed
  with `pdftotext -layout`. Pages used every year: TAX RATE SCHEDULES, WEST
  VIRGINIA TAX TABLE, WHO MUST FILE, EXEMPTIONS, ITEMIZED DEDUCTIONS,
  Schedule M, the LOW-INCOME EARNED INCOME EXCLUSION WORKSHEET, Schedule
  FTC-1 and the two FAMILY TAX CREDIT TABLES.
- Statutes (code.wvlegislature.gov, read in full): 11-21-4e, 4f, 4g, 4h, 4i,
  4j; 11-21-9 (conformity, incl. (f) preserving the WV exemption post-TCJA);
  11-21-12 (modifications; (c)(8) SS, (c)(9) senior); 11-21-10 (low income
  exclusion); 11-21-16 (exemptions); 11-21-20 (other-state credit); 11-21-21
  (SCTC); 11-21-22a/22b (family tax credit); 11-21-23 (HEPTC); 11-21-26 (CDCC).

## Verified value tables

**Exemptions**: $2,000 per exemption every year (unindexed); $500 for a
zero-exemption filer. Count = box (a) self + box (b) spouse (MFJ ONLY) + box
(c) dependents + box (d) surviving spouse.

**Social Security, two tracks.** Track 1 (gated on federal AGI <= $50,000 /
$100,000 MFJ): 35% (2020), 65% (2021), 100% (2022+). Track 2 (HB 4880 of 2024,
for ABOVE-limit taxpayers): 35% (2024), 65% (2025), 100% (2026). Note MFS gets
the FULL $50,000 limit, not half -- booklet-explicit, and PE agrees.

**Family Tax Credit** -- FPG(1) | FPG(2) | increment, per year:
2017 `12,060 | 16,240 | 4,180`; 2018 `12,140 | 16,460 | 4,320`;
2019 `12,490 | 16,910 | 4,420`; 2020 `12,760 | 17,240 | 4,480`;
2021 `12,880 | 17,420 | 4,540`; 2022 `13,590 | 18,310 | 4,720`;
2023 `14,580 | 19,720 | 5,140`; 2024 `15,060 | 20,440 | 5,380`;
2025 `15,650 | 21,150 | 5,500`. MFS table = exactly half of each. Steps are
$300 ($150 MFS) at 10 percentage points each, zero above guideline + $2,700.

**CDCC**: 50% of the federal Form 2441 credit, **first appearing TY2024** --
no care line or Form 2441 reference exists anywhere in the 2017-2023 booklets.

## Parameter inventory

- `agi.yaml`: start_point 1; conformity_group 0; non-WV muni addback with
  own-state carve-out; US-obligation flag; state-refund subtraction; the
  two-track SS encoding (`ss_full_sub_allages` + AGI limits from TY2022, plus
  a flat `ss_sub_share` carrying 0.35/0.65 in 2020/2021 and again in 2024/2025
  for the above-limit track); `age_ded_amount` 8,000 at min age 65 with the
  NEW `age_ded_less_ss_sub` 1.
- `ded.yaml`: `std_amount` 0 and `item_allowed` 0 -- **WV has neither**;
  `std_dependent` 500 carries the zero-exemption filer's allowance (an
  exemption on the form, but no exemption parameter expresses a dependent-filer
  floor, whereas std_dependent targets exactly that population).
- `exempt.yaml`: $2,000 personal and dependent; `dep_filer_zero` 1.
- `ord.yaml`: 5 bands padded to 10, Schedule I / Schedule II mapped; four rate
  vintages including the enacted TY2026.
- `credits.yaml`: Family Tax Credit via the PA `forgive_*` family (FPG base
  and increment filing-status mapped, `forgive_income_base` 3,
  `forgive_add_exempt_int` 0.75); 50% nonrefundable CDCC from TY2024; explicit
  `eitc_match` 0 documenting the absence.
- `filing.yaml`: `req_type` 1 (state base above the exemption allowance -- WV's
  test verbatim) + `req_if_fed_filer` 1.

## Machinery added for this state

`st_agi.age_ded_less_ss_sub` (default 0). Schedule M line 47 box (d) reduces
each person's $8,000 by that person's lines 29-34, which include the Social
Security subtraction. `age_ded_less_pension_sub` reaches only `st_sub_pens`,
which is zero for WV (we do not encode the $2,000 public-pension subtraction),
so without the new offset the encoding understated liability by up to
$520/person -- **$546.90 on the WV-5b test couple, roughly half their true
liability.** Test WV-5b now passes at the form-true $1,097.90.

## Worksheet tests

- WV-1 / WV-1b: 2019 five-bracket top band; MFS half-schedule with one
  exemption.
- WV-2 / WV-2b: 2023 HB 2526 rates with the 100% SS subtraction, and the
  $50,000 cliff pair.
- WV-3: TY2025 SB 2033 rates + the 65% above-limit SS track.
- WV-4: Family Tax Credit at 70% (3 steps above the family-size-3 guideline),
  cross-checked against the published 2024 table row.
- WV-5: $8,000 x 2 senior modification in a no-SS year (offset inert, exact).
- WV-5b: the same couple WITH taxable SS -- pins the new SS netting.
- WV-6: dependent filer -- $500 allowance, credit barred.
- WV-7: the 50% care credit in TY2024, its first year of existence.

## Known differences

- **TY2020-2021 partial SS share is not AGI-gated.** Those phase-in years
  applied only below $50k/$100k; encoded as a flat share, so above-limit units
  get it too. Understates liability; bound `6.5% x share x taxable SS`. Two
  years only. Closable with a share for the all-ages subtraction in place of
  the hard 1.
- **Low-income earned income exclusion omitted** (IT-140 line 5): up to
  $10,000 of earned income when federal AGI <= $10,000 ($5,000 MFS), computed
  as min(FAGI, earned income, cap) behind a cliff -- not expressible, and the
  only dependent-filer deduction slot already carries the $500 allowance.
  **Mostly inframarginal, provably**: any non-dependent filer with FAGI
  <= $10,000 also sits at or below the family-size-1 poverty guideline
  ($12,060 rising to $15,650), so the Family Tax Credit already forgives 100%
  of the tax. Live only for dependent filers (whom the worksheet allows but
  the family credit bars), units whose additions lift modified AGI above the
  guideline, and federal AMT payers. Bound $285 (2017-22) / $224 (2023-24) /
  $211 (2025). Both TAXSIM and PE model it.
- **SCTC (11-21-21) and HEPTC (11-21-23) omitted** -- both refundable, both
  Tier-1 blocked (property tax paid, homestead assessed value, participation).
  Understates refunds for low-income aged/disabled homeowners; PE models both.
- **$2,000 public pension, WV police/fire, federal law enforcement, military
  retirement and railroad retirement subtractions omitted** -- unobservable
  subsets of pension income. Net effect overstates WV AGI. These partly
  OFFSET the senior-modification treatment, since they also belong in box (d).
- **Credit for tax paid to another state omitted** -- larger in WV than most
  states given commuting to OH/PA/MD/VA/KY.
- Tax-table rounding (statuses 1/2/4 under $100,000 use a banded table):
  about +/-$2. Verified: TY2025 band 99,650-99,700 prints $3,966 against
  3,965.84 continuous.
- Surviving-spouse extra exemption modeled only for QSS-mapped units (a
  widow(er) without a dependent child files single on the IT-140 and still
  claims box (d)): understates by $2,000, up to $130.
- Under-65 disability route to the senior modification omitted; surviving-
  spouse $8,000 deduction omitted; gambling-loss subtraction (TY2023+)
  omitted; 529/ABLE/Jumpstart, long-term care premiums, PBGC and IRC 1341
  omitted against the offsetting omission of the matching additions;
  increasing modifications omitted (understates WV AGI); own-state muni share
  at the model's convention; AMT-payer bar on the family credit not encoded
  (inframarginal).
- TY2026 rates ARE carried, departing from the RI/KS practice of stopping at
  the window edge, because this is a discrete ~5% enacted cut rather than
  indexation -- letting 2025 rates carry forward would be a material
  misstatement. Drop the '2026' keys if the window is held strictly to TY2025.

## Uncertainties

1. Every rate vintage is pinned to a published schedule; there is no rate in
   TY2017-2025 that could not be verified. The three things to hold onto are
   the absence of a 2024 cut, the legislated (not certified) TY2025 rates, and
   the enacted TY2026 schedule.
2. `forgive_step` and `forgive_dep_amount` are filing-status mapped here for
   the first time (PA maps only `forgive_base`). The mapper is generic and the
   suite passes, but the MFS half-width path is exercised only indirectly --
   worth a dedicated MFS family-credit case later.
3. `conformity_group: 0` is a modeling choice: 11-21-9(a) is a fixed-date
   clause re-enacted annually, materially the ID pattern this repo already
   treats as rolling. WV is arguably a better candidate for the
   `fixed_date_annual` group than ID is, but that group is `ready: false`, so
   group 0 is the only shippable choice.
4. `std_dependent: 500` puts an exemption-shaped amount in the deduction slot.
   Arithmetically right (dependent filers get $500, everyone else $0, and
   taxable income subtracts both), and the only consequence is that
   `req_type 1`'s filing test compares against `st_exempt = 0` rather than
   $500 -- filer counts only.
5. MFS gets the FULL $50,000 SS AGI limit, not half. Booklet-explicit and PE
   agrees. Do not "fix" it.
6. The Family Tax Credit boundary arithmetic matches the published tables
   exactly at both edges (ceiling() puts modified AGI at the guideline in the
   100% band and at guideline+$300 in the 90% band, and reaches zero precisely
   above guideline+$2,700, which is also the statutory eligibility ceiling).
   Checked rather than assumed.
7. `st_band_index_lower` is immaterial for WV: every base amount equals the
   schedule value at the bracket floor, so taxable income exactly on a
   threshold gives the same tax either way (verified at $40,000 in WV-5).

## Cross-model and aggregate validation

- TAXSIM 2017-2021 (PE has no WV values before TY2022) / PE 2022-2024.
  **No substantive PE disagreements were found -- a first among the batch-C
  states.** Cross-model wedges will therefore come from our omissions, not
  from PE vintage error.
- Pre-register: the TY2020-2021 above-limit SS share; the low-income earned
  income exclusion on dependent filers; SCTC and HEPTC (PE models both); the
  $2,000 public-pension and military-retirement subtractions.
- Aggregate: blocked on weights; benchmark against WV Tax Division / State
  Budget Office personal income tax collections and IRS HT2.
