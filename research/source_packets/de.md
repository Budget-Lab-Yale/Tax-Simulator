# Delaware State Source Packet

State: `DE`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-12`

> **Status note (as of 2026-08-12), kept from the packet's former Status line:**
> baseline encoded; worksheet tests DE-1..DE-5 pass

## Scope

- Tax years covered: 2017-2025. Every rate, bracket, deduction, credit and
  exclusion amount is CONSTANT across the window (verified against the 2017,
  2019, 2020, 2021, 2022, 2024 and 2025 primary documents). The only time
  variation in DE law inside the window is (a) the TY2021 arrival of the
  refundable-EITC election, (b) the TY2020-2021 unemployment exclusion, and
  (c) the federal SALT cap flowing into the itemized computation from 2018.
- Resident individual income tax only. No nonresident allocation, no PTET.
- Major structural features: federal-AGI base; ONE graduated rate schedule
  (0/2.2/3.9/4.8/5.2/5.55/6.6%) shared by every filing status; a
  per-exemption $110 nonrefundable CREDIT in place of a personal exemption; a
  $2,000/$12,500 age-60 retirement exclusion measured on pension PLUS
  "eligible retirement income"; fully UNCOUPLED itemization with a statutory
  Delaware-income-tax exclusion; and a married-filing-combined-separate
  status that materially reduces two-earner couples' tax.

## Primary sources

- Form PIT-RES (formerly 200-01) instruction booklets from
  revenue.delaware.gov / revenuefiles.delaware.gov: TY2024, TY2025, TY2022,
  TY2021, TY2020, TY2019, and the TY2017 Form 200-01 plus TY2017 AMENDED
  instructions (which reprint the full rate schedule, standard deduction,
  additional-standard worksheet, $110 credits, 50% CDCC and 20% EITC; the
  TY2017 base booklet PDF 404s).
- Tax tables / rate schedule: TY2017 and TY2019 tax-table booklets, both
  printing the identical "$2,943.50 plus 6.60% for the portion over $60,000"
  schedule. DOR software-developer "Tax Rate Changes" page states the
  schedule is "for tax years 2014 and later" and prints all seven base
  amounts (0/0/66/261/741/1,001/2,943.50).
- Statute (30 Del. C. ch. 11): 1102 (rates), 1106(a) additions /
  1106(b)(1) US-obligation interest / (b)(2) elderly-disabled exclusion /
  (b)(3) pension exclusion / (b)(10) unemployment, 1108 (standard
  deduction), 1109 (itemized), 1110 (personal credits), 1114 (child care
  credit), 1117 (EITC).
- Legislation: HB 80 (150th GA) and HB 16 (151st GA, signed 2021-08-10)
  creating the 4.5%-refundable / 20%-nonrefundable EITC election.

## Secondary cross-checks

PolicyEngine US local package: the rate schedule, standard deduction, $110
personal credit, $2,000/$12,500 pension exclusion at age 60, eligible-income
list, elderly/disabled exclusion and UI subtraction all AGREE with the
primary documents. FOUR vintage disagreements, primary wins in each:

| Item | PE | Primary | Encoded |
|---|---|---|---|
| Refundable EITC start | 2020 | TY2020 booklet offers 20% NONrefundable only; TY2021 is the first with the election | **2021** |
| Nonrefundable EITC start | 2022 | 20% in force from at least TY2017 | **2017** |
| CDCC match start | 2023 | 50% since at least TY2017 (1114) | **2017** |
| Additional std / aged credit start | 2021 | both present in TY2017 | **2017** |
| 2022 relief rebate | $300 refundable credit dated 2020 | HB 360 is a 2022 MAILED rebate, not an on-return credit | **not modeled** (P5 class) |

## Parameter inventory

- `agi.yaml`: start_point 1; rolling conformity; non-DE muni addback with
  own-state carve-out; US-obligation flag; DE refund subtraction; full SS
  subtraction; UI subtraction 2020-2021 only; the two-tier age-60 retirement
  exclusion split across the `pension_excl_band` family (under 60: $2,000 of
  pension only, amounts going to zero AT 60 so the tiers cannot stack) and
  the `retirement_excl` family (60+: $12,500 per person of pension plus
  eligible retirement income, `earned_cap` 0).
- `ded.yaml`: std 3,250/6,500 (HoH takes the single amount); $2,500 per
  aged/blind box; UNCOUPLED itemization (`item_fed_gate` 0 -- 1109 lets a
  federal std claimant itemize for DE); `salt_addback` 1 with the
  income/sales component excluded (DE has no sales tax, so this is
  near-exact); federal SALT cap on property taxes from 2018; `pease` in
  2017 only (the DE computation began at federal Schedule A line 29).
- `exempt.yaml`: all zero -- DE replaced the exemption with the $110 credit.
- `ord.yaml`: one 7-band schedule for all statuses; `combined_sep` 1 with
  the new `combined_sep_std_share` 0.5 (DE's joint std is twice the
  per-column amount, unlike KY's per-person std).
- `credits.yaml`: `exempt_credit_*` $110 personal and dependent; the age-60
  additional $110 via `age_credit_*` (NOT `exempt_credit_aged`, whose age
  test is fixed at 65); 50% nonrefundable CDCC; 20% nonrefundable EITC with
  the 4.5% refundable alternative from 2021 (VA `eitc_match_alt` greater-of
  machinery).
- `filing.yaml`: `req_type` 2 with DE-AGI thresholds 9,400/15,450 and 5,250
  for dependent filers; `req_if_fed_filer` 0 (DE's test is DE-AGI only).

## Worksheet tests

- DE-1: graduated schedule + three $110 personal credits.
- DE-2: age-60 retirement exclusion (pension + interest + dividends, capped
  12,500) + $2,500 additional std + both $110 credits.
- DE-3: the EITC election, where 4.5% refundable beats 20% nonrefundable.
- DE-4: married filing combined separate -- pins `combined_sep_std_share`
  (without it, liability was understated by $360.75 on this case).
- DE-5: 50% child and dependent care credit.

## Known differences

- **EITC election comparison base**: our machinery compares the
  nonrefundable option against `st_tax_pre_credit`; DE's PIT-RSS compares
  against tax AFTER the personal, aged and care credits. Units whose $110
  credits wipe out their tax are assigned the worthless 20% option instead
  of the 4.5% refund -- understates refunds by up to 4.5% of the federal
  EITC (<= ~$300) for low-liability EITC filers, 2021+. Largest our-side item.
- **Retirement-income attribution**: DE's $12,500 is per person on OWN
  pension; we split non-wage income equally between spouses (the form's own
  convention for joint accounts, but not for 1099-R income). Over-excludes
  for one-pension couples.
- **Under-60 tier cap is per unit** rather than per spouse's own pension
  (<= ~$132 for under-60 couples with one-sided pension income).
- **Combined-separate aged/blind split**: `combined_sep_std_share` halves the
  accumulated add-ons, so the TOTAL deduction across columns is right but its
  split is approximate where only one spouse is 65+/blind.
- **SALT cap applied to real-estate taxes only** (the form caps the sum of
  income/sales + real estate + personal property).
- Elderly/disabled exclusion (1106(b)(2)) NOT modeled -- and provably
  INFRAMARGINAL: at the income ceiling a 60+ single owes $134 of schedule tax
  against $220 of personal credits ($429 vs $440 for a couple), so it changes
  no liability. Also unobservable (disability status, earned-income test).
- Under-60 military pension exclusion ($12,500 not $2,000), 529/ABLE
  subtraction, volunteer firefighter credit, PIT-CRS credits: unobservable.
- **Credit for taxes paid to another state not modeled** -- larger in DE than
  most states (substantial commuting to PA/MD/NJ).
- DE tax TABLE rounding below $60,000 of taxable income (banded in $50
  increments, rounded to whole dollars) vs our continuous schedule: ~$1.50.
- Filing thresholds encode the under-60 row only (filer counts only; the
  age/blind increments equal the additional standard deduction).

## Uncertainties

1. **TY2021 vs TY2022 start of the refundable-EITC election -- highest-risk
   vintage call.** The TY2021 booklet prints the election and the PIT-RSS
   4.5%/20% worksheet, and the form was renamed 200-01 -> PIT-RES that year,
   consistent with the IRAS rollout on which HB 16's effective date was
   contingent. But codified 1117 carries "[Effective upon fulfillment of
   83 Del. Laws, c. 118, s. 2]" and Justia's 2022 code is the first showing
   the election. Encoded 2021 on the strength of the booklet (forms govern
   administration). VERIFY with a TAXSIM TY2021 DE probe before relying on it
   -- this is the year-early anchor class that bit GA and NC.
2. TY2017/TY2018 filing thresholds inferred (TY2019-2025 verified; the TY2017
   base booklet is unavailable). Unindexed, so drift unlikely; filer counts
   only.
3. **Medical floor contradiction**: the TY2024 PIT-RSA prose says 10% of AGI
   while the adjacent worksheet says 7.5%. We follow the worksheet (federal
   conformity). If DE enforces 10%, we overstate itemized deductions for
   filers with medical expenses between 7.5% and 10% of AGI.
4. Rolling conformity inferred (no fixed-date clause found in ch. 11), not
   primary-source confirmed verbatim.
5. Under-60 IRA eligibility set to ineligible (early-distribution reasoning);
   the 59.5-59 cohort with penalty-free distributions arguably qualifies
   (bounded by $2,000 x 6.6% = $132).

## Cross-model and aggregate validation

- TAXSIM 2017-2020 / PolicyEngine 2021-2024. Pre-register the four PE vintage
  disagreements above plus the relief-rebate dating.
- Aggregate: blocked until weights land; benchmark against DE DOF revenue
  reports (personal income tax net collections).
