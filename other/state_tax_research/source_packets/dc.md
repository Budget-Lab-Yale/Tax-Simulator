# District of Columbia Source Packet

State: `DC`
Status: `research COMPLETE and primary-verified; NOT yet encoded (YAML drafted, tests drafted)`
Last updated: `2026-08-12`

> Read §Uncertainties before encoding: the TY2025 dependent-filer standard
> deduction and the TY2025 CDCC rate are both open calls, and the TY2023
> booklet is font-mangled (the same encoding class as four of the 2026-08-11
> R1 close-out fixes).

## Scope

- TY2017-2025. Two rate regimes (2017-2021, 2022+), three deduction regimes
  (2017 own amounts / 2018-2024 federal conformity / 2025 own amounts), and a
  three-step EITC ladder (40% / 70% / 100%).
- Resident individual income tax only (D-40). DC is barred from taxing
  nonresident wages, so resident-only is nearly complete coverage of the DC IIT.
- Major features: federal-AGI start; ONE graduated schedule for every filing
  status (47-1806.03(e) makes each separately-filing spouse "a single person");
  a married-filing-separately-on-the-SAME-return status (Calculation J) that is
  therefore very valuable; **full** SS / tier-1 RR subtraction at every age
  with no income test; **NO pension exclusion of any kind**; itemization
  coupled to the federal election with income taxes stripped and real property
  taxes UNCAPPED, plus a DC-specific 5%-of-AGI-over-$200,000 reduction of the
  non-protected components; a 2017-only per-exemption allowance with a stepped
  phase-out; and a 40/70/100% federal EITC match for filers WITH children
  alongside a completely separate DC-formula credit for CHILDLESS workers.

## Verified value tables

**Rates.** TY2017-2021 (47-1806.03(a)(10)): `4 / 6 / 6.5 / 8.5 / 8.75 / 8.95%`
at `10k / 40k / 60k / 350k / 1M`. TY2022-2025 ((a)(11), D.C. Law 24-45 s. 7222):
`4 / 6 / 6.5 / 8.5 / 9.25 / 9.75 / 10.75%` at `10k / 40k / 60k / 250k / 500k /
1M`. The four lowest bands are identical across both regimes -- only the top end
was restructured. Published base amounts (400 / 2,200 / 3,500 / 28,150 / 85,025
and 400 / 2,200 / 3,500 / 19,650 / 42,775 / 91,525) reproduce exactly from the
marginal rates, so no base_amounts family is needed.

**Standard deduction** (single / MFJ / HoH): 2017 `5,650 / 10,275 / 7,800`
(DC's own, no aged/blind add-on at all); 2018-2024 the FEDERAL amounts
(12,000/24,000/18,000 rising to 14,600/29,200/21,900) plus the federal
additional amounts; **2025 DC decouples** and refuses the OBBBA increase:
`15,000 / 30,000 / 22,500` with $1,600/$2,000 add-ons.

**Additional standard deduction per box** (married | unmarried): 2018
`1,300 | 1,600`; 2019-2020 `1,300 | 1,650`; 2021 `1,350 | 1,700`; 2022
`1,400 | 1,750`; 2023 `1,500 | 1,850`; 2024 `1,550 | 1,950`; 2025
`1,600 | 2,000`. Zero in 2017.

**2017 exemptions**: $1,775 per exemption, with an EXTRA exemption for HoH and
one per 65+/blind filer; stepped phase-out of 2% per $2,500 (or fraction) of
FEDERAL AGI over $150,000, zeroing exactly at $275,000 (50 steps x 2% = 100%).
All zero from 2018 (TCJA conformity, explicit in the booklet).

**EITC with children**: 40% (2017-2021) -> 70% (2022-2024) -> 100% (2025).
Statute (f)(1)(B)/(B-1)/(B-2) with **(B-3) [Repealed]** -- the FY2025 BSA
collapsed the original 85%/100% steps into a single 100% step, so **no year in
2017-2025 uses 85%**.

**Childless-worker EITC** -- an independent DC formula, not a match: credit
percentage x earned income up to the earned income amount, less **8.48%** of
the excess of max(AGI, earned income) over an indexed threshold. Per year
(max | phase-out start | zero-out): 2017 `510 | 18,622 | 24,630`; 2018
`519 | 18,862 | 24,982`; 2019 `529 | 19,239 | 25,477`; 2020
`538 | 19,489 | 25,833`; **2021 `1,502 | 19,743 | 37,455`** (DC followed the
ARPA relaxation, phase-in 15.3% and age 19); 2022 `560 | 20,532 | 27,136`;
2023 `600 | 21,888 | 28,963`; 2024 `632 | 22,566 | 30,019`; 2025
`649 | 23,288 | 30,941`. Phase-in 7.65% in all other years.

**CDCC**: 32% of the federal section 21 credit, nonrefundable.

## Critical structural point for encoding

`calc_st_credits` SUMS `st_eitc` and `st_earned_credit`. DC's two EITCs are
mutually exclusive by child count, so the match must be loaded through
`eitc_match_by_kids` with **slot 1 (zero qualifying children) set to ZERO**,
and the independent-credit vectors carry the DC formula in slot 1 only.
Encoding the match as a scalar `eitc_match` would double-count every childless
EITC recipient.

## Primary sources

- D-40 / D-40EZ instruction booklets, TY2017-2025, all nine years read:
  2017-2022 and 2025 via the NBER mirror, 2023-2024 from otr.cfo.dc.gov.
- **The TY2023 booklet (and parts of TY2020/2021/2025) carries a broken font
  ToUnicode map.** The mangling is a fixed one-to-one glyph substitution
  (`0->ä 1->£ 2->Ó 3->Î 4->{ 5->x 6->È 7->Ç 8->n 9->dropped $->f ,->]`) and was
  decoded programmatically; every decoded 2023 figure was cross-checked against
  the TY2024 booklet's "increasing from $X to $Y" line and the statute.
- Statutes (code.dccouncil.gov, full text): 47-1806.03 (rates; (e) the
  separate-filer rule), 47-1806.04 (credits: (c) CDCC, (e) Low Income Credit
  killed after TY2017, (f) EITC incl. (f)(1)(C) the childless formula and
  (f)(1)(D) ITIN filers from TY2023, (g) non-custodial parent EITC),
  47-1806.15 (Keep Child Care Affordable), 47-1801.04(44)/(3A),
  47-1803.03(b-4) (the 5% itemized limitation), 47-1803.04(b) (coupling),
  47-1806.06 (Schedule H).
- OTR release "District of Columbia Tax Changes Take Effect October 1st 2024".

## Secondary cross-checks

**No PE-vs-primary disagreements were found for DC** -- unusual for this
project and worth noting: DC's PE parameters are unusually clean. PE covers
TY2021+ for most items (so 2017-2020 rests on the booklets alone, all four
read and identical to 2021), and its one apparent divergence -- the itemized
limitation starting in 2021 -- is a coverage artifact, not a vintage claim
(the 5%-over-$200,000 rule has been in force since 2011 and is printed in
every 2017-2025 booklet). **The real risk in DC is entirely on our side.**

## Known differences (largest first)

1. **Unincorporated business carve-out (D-30) not modeled -- largest single
   item.** A DC resident with >$12,000 of DC gross income from an
   unincorporated business (including rents and royalties) files Form D-30 and
   **excludes** that income from the D-40. We tax sole-prop, partnership and
   rental income on the individual schedule. Direction ambiguous: D-30 is a
   flat 8.25% with a $5,000 salary allowance and a 30% exemption against
   individual rates of 4-10.75%. Overstates for high-bracket owners,
   understates for low-bracket ones; affects a large share of DC Schedule C/E
   filers. Paired with the omitted >$12,000 pass-through-loss addback, which
   PE does model.
2. **Low Income Credit omitted, TY2017 only** (repealed after 2017).
   Nonrefundable, $119-$1,311 capped at remaining tax, table-driven, and
   mutually exclusive with the DC EITC -- so it bites non-EITC low-income
   filers, especially seniors. TAXSIM models it -> pre-register a 2017 wedge.
3. **Keep Child Care Affordable credit omitted, TY2018+**: refundable, up to
   $1,000 (2018-19) rising to $1,200 (2025) per dependent under 4, DC-taxable-
   income capped. Requires expenses paid to a LICENSED DC facility (a strict
   subset of federal section 21 expenses) and disqualifies subsidy recipients.
   PE models it.
4. **Schedule H property tax credit omitted, all years**: refundable, up to
   $1,025 (2017) rising to $1,425 (2025); rent- and household-income-based ->
   Tier-1 blocked. PE models it.
5. **Government-annuitant survivor benefits (62+) not subtracted** --
   overstates DC AGI for a population materially over-represented in DC.
6. **Childless-EITC phase-out measure**: DC reduces the tentative credit by
   8.48% of the excess of `max(earned income, federal AGI)` over the threshold;
   `earned_credit_style 1` computes `min(curve(earned), curve(AGI))`, exact
   when both sit in the same region but **overstating** (by up to the full
   maximum, <= $649) for units whose earned income is still phasing in while
   AGI is past the phase-out start. Compounded by the absent investment-income
   disqualifier, which also overstates eligibility. (The age-65 ceiling was a
   third such gap; `earned_credit_age_max` closed it on 2026-08-12.)
7. **CDCC base**: DC allows 32% "regardless of the amount actually used to
   offset federal tax liability"; we apply 32% to the post-limitation federal
   credit, understating for units whose federal tax cannot absorb it.
8. **2017 Pease interaction**: DC pro-rates the income-tax subtraction by the
   post-limitation allowed share; we apply Pease to the DC component base.
   We understate the deduction by `R x I / T` -- e.g. ~$1,250 of deduction
   (~$106 of tax) on T=100,000 / I=25,000 / R=5,000. 2017 only.
9. **Separate-on-same-return deduction split**: Calculation J lets couples
   allocate the joint deduction "as you wish"; we split the standard deduction
   50/50 and the itemized by income share. The TOTAL is exact, the split is
   not optimized -> overstates tax for unequal-income electors. In 2017 the
   combined columns also receive no dependent exemptions and no phase-out,
   which only makes the `pmin(joint, combined)` election less attractive --
   conservative, never over-crediting.
10. **DC Health Care Shared Responsibility payment (TY2019+) omitted** -- it
    sits inside "Total tax" on the D-40. Understates DC total tax for uninsured
    units; no suitable parameter (`st_surtax.per_return_amount` is
    unconditional and would misstate it).
11. QHTC 3% capital-gains rate (TY2019+); personal property tax dropped from
    the itemized base 2018+ (per Calculation F's structure); ITIN-filer EITC
    (TY2023+) not granted since our federal `eitc` is zero for those units;
    2025 filing thresholds test gross income inclusively where `req_type 2`
    tests `st_agi` strictly (filer counts only); DC tax-table rounding
    (+/-$2-3); non-custodial parent EITC (Schedule N); other-state credit;
    Schedule U credits.
12. **TY2026+ not encoded**: the DC Child Tax Credit ($1,000/child under 18,
    47-1806.17) and the CDCC drop to 24.25% both begin TY2026. The 2025 booklet
    confirms the CTC is "repealed for 2025".

## Uncertainties (in priority order)

1. **TY2025 dependent-filer standard deduction is inferred from ABSENCE, and
   is the highest-magnitude open call.** The TY2025 Calculation G-1 contains no
   dependent computation (unlike 2018-2024's separate worksheet), the
   standard-deduction text reads "Single individuals, **dependents** and
   married ... filing separately are allowed a standard deduction amount of
   $15,000", and the filing chart lists "Dependent filer $15,000". The draft
   therefore sets `std_dependent_style: 0` for 2025 -- a flat $15,000, a large
   decoupling from the federal $1,350/earned+$450 formula. If OTR intends the
   federal formula to survive by reference to 47-1801.04(44), we **overstate**
   dependent filers' deductions by up to ~$13,650. **Probe with a 2025
   dependent case before encoding.**
2. **TY2025 CDCC rate is a statutory gap resolved by the form.**
   47-1806.04(c)(1)(A) covers years "before January 1, 2025" and (c)(1)(B)
   years "after December 31, 2025" -- TY2025 is covered by **neither**. The
   TY2025 D-40 Line 21 prints `X .32`, so 32% is encoded and PE agrees. If OTR
   later reads the gap as 24.25%, DC CDCC in 2025 is overstated by ~24% of the
   credit (<= ~$250).
3. **The TY2023 booklet was decoded, not read.** Every 2023 value used (std
   13,850/20,800/27,700; add-ons $1,500/$1,850; childless-EITC max $600,
   start $21,888, zero-out $28,963; EITC 70%; KCCATC $1,115; Schedule H
   $1,325 and $61,300/$83,700) came from the glyph-substitution decode and was
   corroborated against TY2024's "increasing from $X" line, the statute, or PE.
   **This is the encoding class that produced four R1 close-out fixes -- worth
   one visual spot-check of pages 13 and 23 before encoding.**
4. **The EITC ladder has no 85% step** -- both endpoints are booklet-confirmed
   (TY2024 prints `.70`, TY2025 "100% of federal EIC"), so no year in the
   window uses 85%. But the statute page carries "*includes amendments by
   temporary legislation that will expire on September 25, 2026*", so the
   TY2026 treatment is unsettled (outside the window).
5. TY2020/2021/2023 dependent-worksheet floor and increment values are the
   federal amounts DC conforms to by statute; those pages are font-mangled.
   TY2018/2019/2022/2024 were read directly and match federal exactly.
6. **Rolling conformity is inferred**, not verbatim-confirmed: 47-1801.04(28)
   defines the IRC without a fixed date and DC deconforms by named provision
   (TY2018 QBI; TY2025 OBBBA items). None of the 2025 carve-outs affect a
   federal-AGI start, so group 0 is safe within the window. (The TY2018 QBI
   denial needs no adjustment at all -- section 199A is below-the-line and DC
   starts from federal AGI. Recorded so the non-issue is not rediscovered.)
7. **The 2017 HoH extra exemption** rests on Schedule S Calculation G line b
   read off the TY2017 form; the narrative instructions mention it only
   obliquely, and PE has no 2017 exemptions, so there is **no cross-check**.
   Worth $71-$159 per affected HoH filer. Same construction as the KS HoH
   additional exemption, which was booklet-verified across five years.
8. `item_include_pers_tax` flipping to 0 in 2018 is derived from Calculation
   F's structure (it subtracts Schedule A Line 7 and adds back only Lines 5b
   and 6), not from an explicit disallowance. Form governs; small magnitude
   (DC has no individual vehicle personal property tax).
9. The childless-EITC eligibility gate is now MOSTLY encodable. **The age-65
   ceiling was resolved 2026-08-12**: `st_credits.earned_credit_age_max`
   (default `.inf`) was added and tested (test MACH-6), and it applies only to
   filers without qualifying children, matching the federal rule -- so encode
   DC with `earned_credit_age_max` 64 alongside the age-25 minimum (and 18 in
   TY2021, where DC followed the ARPA relaxation). MN's packet flags the same
   ceiling, so it is the second consumer. Still NOT encodable and still
   overstating eligibility: the investment-income limit and the Schedule E /
   Form 4797 exclusions.
10. 2017-2020 rate schedules rest on the booklets alone (PE starts at 2021).
    All four were read and print the identical (a)(10) schedule, and the
    statute confirms (a)(10) governs "taxable years beginning after December
    31, 2015".

## Cross-model and aggregate validation

- TAXSIM 2017-2020 / PolicyEngine 2021-2024. Pre-register: the 2017 Low Income
  Credit (TAXSIM models it), Schedule H, KCCATC, and the D-30 unincorporated-
  business carve-out.
- Aggregate: blocked on weights. Benchmark against the DC CFO's *Tax Facts*,
  the OTR annual report individual income tax collections, and HT2.
