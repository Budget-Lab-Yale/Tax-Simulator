# Minnesota State Source Packet

State: `MN`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-07` (WFC/M1CWFC eligibility: childless age 19 from
2021, M1CWFC age band and investment-income limits; previous: `2026-09-30`
separate-filer rule for WFC and M1CWFC resolved; earlier: `2026-08-11` TAXSIM triage: clean, 11 exact probe cases;

> **Status note (as of 2026-08-11), kept from the packet's former Status line:**
> baseline encoded; record-level worksheet tests complete
childless M1CWFC phase-out corrected to the general 12% per the 2024 form)

Full research notes with per-year tables and citations:
[research/raw/mn_research_core.md](research/raw/mn_research_core.md) (Form M1 booklets and
schedules 2017-2025, DOR inflation memos and algorithm sheets, Minn. Stat.
ch. 290; PolicyEngine corroboration).

## Scope

- Tax years 2017-2035; parameters transcribed through TY2025, carried
  forward beyond (indexed in law; documented).
- Resident Form M1 only.
- Major features: federal-TAXABLE-income start in 2017 (with SALT addback)
  switching to federal-AGI start from TY2018 (year-keyed `start_point`);
  the TY2018 TCJA-nonconformity year encoded as TCJA FAGI + MN's own
  pre-TCJA deduction/exemption stack; four-bracket graduated schedule
  (6.80% second tier from 2019); MN standard/itemized deductions with the
  high-income limitation (two-tier + flat-80% from 2023, applying to BOTH
  deductions); dependent exemptions with a 2%-per-$2,500 phase-out; dual
  Social Security subtraction regimes (sliding 2017-2022; greater-of
  simplified/frozen-sliding 2023+); WFC 2017-2022 and the combined
  CTC+WFC (M1CWFC) 2023+; marriage credit; capped dependent-care credit;
  1% NIIT over $1M (2024+).

## Machinery introduced (all generic)

1. Two-tier Pease with flat-80% override and standard-deduction inclusion
   (`st_ded.pease_thresh2/rate2/flat_thresh/pease_incl_std`).
2. Share-based exemption phase-out (`st_exempt.po_share_per_step`).
3. Sliding partial SS subtraction (`st_agi.ss_partial_*`, provisional
   income = AGI − taxable SS + 50% gross SS + exempt interest) and a
   stepped phase-out for the all-ages full subtraction
   (`st_agi.ss_allages_po_step/_share`); greater-of election automatic.
4. Non-itemizer charitable share (`st_agi.sub_char_nonitem_share`).
5. Combined child + working-family credit (`st_credits.cwfc_*`, joint
   phase-out on max(earned, AGI)).
6. Two-earner marriage credit (`st_credits.mc_*` + `mc_single_brackets`
   family: joint-schedule tax less single-schedule tax on imputed shares).
7. Dependent-care income cap (`st_credits.cdctc_cap_*`).
8. Net-investment-income add-on tax (`st_surtax.inv_income_*`).

## Worksheet tests (src/tests/state/test_state_calc.R, MN-1 .. MN-11)

Basic 2024 return; 2017 taxable-income start + SALT addback; 2018
pre-TCJA stack on FAGI; sliding SS + aged standard add-ons (2021);
simplified SS stepped phase-out beating the frozen alternative (2024);
two-tier deduction limitation + exemption phase-out (2023); WFC
triangular schedule (2021); M1CWFC combined credit (2024); marriage
credit (2022); NIIT + flat-80% limitation (2024); dependent-care cap
(2023).

## Known differences

- **2017 only:** MN's incremental Pease and exemption-phase-out addbacks
  (M1M lines 1-2, thresholds below federal) not modeled — affects
  AGI ~$186-314k itemizers.
- **2018:** M1NC residual items (post-retroactivity: tuition/fees, CARES
  business items, moving expenses, opportunity zones) skipped; the
  pre-TCJA restorations are on MN's own lines and ARE encoded.
- **M1SA components:** medical uses the federal-floor amount (MN floor is
  10% of AGI); misc-2% deductions zero in post-TCJA PUF data (MN allows);
  casualty is federal disaster-only (MN allows non-disaster); the property
  tax cap is applied to property taxes alone rather than the combined line.
- **2021 dependent care:** our federal CDCTC is ARPA-law; MN computed its
  own pre-ARPA credit — overstates the MN credit for 2021. *(Resolved
  2026-10-07: M1CD is now computed on its own pre-ARPA terms in every year;
  see "Dependent care credit (M1CD)" below.)*
- **WFC eligibility:** the 2017-18 federal-EIC gate approximated by age
  alone; the childless upper age limit (64) unmodeled; M1CWFC older
  children proxied by dependents aged 18-23 (students/disabled
  unobserved); dependent slots cap tracked children at three (the credit
  has no child limit). *(Resolved 2026-08-11: the childless M1CWFC
  phase-out is the GENERAL 12%, not 9% — 2024 form line 13 gives 9% only
  to older-child-only units; we had applied 9% to childless units.
  Fixed in st_credits_child.R; test MN-12.)*
- **Separate filers (married filing separately), resolved 2026-09-30.**
  Minn. Stat. 290.0671 subd. 1(a) makes the WFC follow IRC 32 eligibility, and
  290.0661 subd. 2 makes M1CWFC follow 290.0671. Which IRC applies is set by
  290.01 subd. 31: the Code as amended through **December 31, 2018** in the
  2021 and 2022 editions, with 290.0111 subd. 5 adopting only ARPA **sec. 9042**
  (the unemployment exclusion), not sec. 9623. So the pre-ARPA 32(d) bar holds
  through TY2022 and separate filers get no WFC. From TY2023 the Code as
  amended through **May 1, 2023** applies, so the IRC 32(d)(2)
  separated-spouse rule reaches M1CWFC. The 2024 Schedule M1CWFC instructions
  ("Exception for Those Who are Married and Filing Separately") confirm it: at
  least one qualifying child on Schedule M1DQC who lived with the filer for
  over half the year, and not sharing a principal abode with the spouse for
  the last six months or legally separated; the form carries a checkbox.
  Before this fix M1CWFC hardcoded separate filers as ineligible. Now
  `st_credits.cwfc_mfs_eligible` = 1 from 2023; the child test uses the
  credit's own age-based child counts (not `n_dep_eitc`, which Tax-Data zeroes
  for every separate filer), and the living-apart condition is unobserved and
  assumed met. Tests MN-8b/8c/8d. The 2023 M1CWFC instructions were not
  retrieved (the DOR URL pattern for 2023 404s); 2023 rests on the statute.
- **WFC and M1CWFC eligibility, resolved 2026-10-07.** Prompted by the
  State-EITC-Align cross-check (`research/state_tax/notes/state_eitc_align_crosscheck_2026_10_02.md`,
  used as a check, not a source), and verified here against primary law:
  - *Childless minimum age 19 from TY2021, not 21.* Laws 2021 1Sp ch. 14
    art. 1 s. 10 strikes 21 and inserts 19 in 290.0671 subd. 1(a)(1),
    "effective for taxable years beginning after December 31, 2020"; Schedule
    M1WFC 2021 and 2022: "None (if between the age of 19 and 64)". We had kept
    21 through 2022 (`earned_credit_age_min`). Tests MN-7d/7e.
  - *M1CWFC applies the WFC's EITC-style eligibility to the whole schedule.*
    290.0661 subd. 2 (2024): the child credit requires eligibility "under
    section 290.0671, except a taxpayer whose earned income was insufficient".
    The 2023-2025 M1CWFC instructions ("Am I eligible?", which covers both
    credits) list: investment income below $11,000 / $11,600 / $11,950; and
    with no qualifying child on M1DQC rows 10-11, "you or your spouse must be
    between the ages of 19 and 64". Both tests had been missing, so the 4%
    childless amount was paid at any age and no unit was screened on
    investment income (new `cwfc_age_min/_max`, `cwfc_inv_inc_limit`; tests
    MN-12b-12e).
  - *WFC investment-income limit, 2017-2022.* 290.0671 subd. 1(a) requires
    federal-EIC eligibility, including IRC 32(i). The 2017-2019 M1WFC sends the
    filer through the federal EIC worksheet ($3,450 / $3,500 / $3,600); the
    2020-2022 instructions print $3,650 / $10,000 / $10,300. The 2021 value
    is DOR's printed figure, the ARPA amount, even though 290.01 subd. 31 then
    read the IRC as of December 31, 2018 (which would give about $3,650); we
    follow the form. Unmodeled before (`earned_credit_inv_inc_limit`; tests
    MN-7f/7g).
  - *Qualifying older child must be younger than the filer.* 290.0671 subd. 1:
    an IRC 32(c) qualifying child aged 18 or over; IRC 152(c)(3)(A) requires
    the child to be younger than the filer (or than either spouse on a joint
    return). The older-child count now caps the age at the oldest filer's age
    less one (tests MN-12f/12g). Ages 19-23 still assume full-time student
    status, which is unobserved.
  - Generic: the investment-income measure and the head-or-spouse age-band
    test are now single helpers (`st_eitc_inv_inc`,
    `st_head_or_spouse_in_age_band`, `src/calc/state/st_utils.R`) shared by
    the independent earned credit, M1CWFC and the WA WFTC.
- **Dependent care credit (M1CD), resolved 2026-10-07.** The credit is the
  federal-formula credit before the federal tax-liability limit: the 2017
  M1CD takes Form 2441 line 9 ("complete federal Form 2441 even if you did not
  claim the federal credit"), and from 2021 the schedule computes it itself
  (lines 1-7: expenses up to $3,000 / $6,000 and the lower spouse's earned
  income, times the Table 3 decimal, .35 falling .01 per $2,000 of AGI over
  $15,000 to .20). We had taken a share of the federal credit as claimed,
  which (a) is zero for a family with no federal tax, because the federal
  credit is nonrefundable outside 2021, (b) carries the federal 90% take-up
  draw, and (c) applied the ARPA expansion in 2021, which Minnesota did not
  adopt (the 2021 M1CD prints the pre-ARPA table and limits). Now an own-rate
  credit (`cdctc_style` 2) every year, with the income cap on top (tests
  MN-11b/11c). This closed the 2021 known difference listed above. The
  newborn-without-expenses rule and the student/disabled-spouse deemed income
  stay unmodeled.
- **Marriage credit income (M1MA lines 3-4), resolved 2026-10-07.** Each
  spouse's M1MA income includes taxable pensions and IRA distributions (1040
  lines 4b and 4d) and taxable Social Security, not only earnings (2017, 2019
  and 2024 schedules). Their owner is unobserved, so each spouse is credited
  half (`mc_retirement_split_share` = 0.5, the ST_SPLIT_HALVE convention);
  retiree couples previously got no credit (test MN-9b). This supersedes the
  "earned income only" note under Marriage credit below.
- **Cross-model triage, 2026-09-30/10-01: the TAXSIM gap was four TAXSIM bugs and
  one of ours.** The 2026-08-11 note put the residual in itemizers. On the
  current board they are almost all excluded by crosswalk-exposure rows; what
  remained was:
  - **T20** (TAXSIM): 2019-2020 heads of household get the single standard
    deduction ($12,200 / $12,400 against $18,350 / $18,650; 290.0123 subd. 1).
  - **T21** (TAXSIM): 2019-2020, no high-income limitation on the standard
    deduction (290.0123 subd. 5, new in Laws 2019 ch. 6).
  - **T22** (TAXSIM): 2017-2020, non-joint families with two or more children
    are phased out of the WFC from the one-child threshold (290.0671 subd. 1:
    base $21,190 vs $25,130).
  - **T23** (TAXSIM): 2019-2020, the childless WFC paid at age 65+.
  - **Ours, fixed:** the childless WFC had no upper age (`earned_credit_age_max`
    now 64; tests MN-7b/7c). Confirmed in passing that the age-21 floor starts
    in TY2019, not 2018 (Laws 2017 1Sp ch. 1 art. 1 s. 20: "effective for
    taxable years beginning after December 31, 2018").
  - PolicyEngine 2023-2025: separate filers with a child get M1CWFC from us and
    not from PolicyEngine (an unobservable living-apart condition; excluded on
    the records where we pay more than $100).
  Probe scripts: `research/state_tax/scripts/mn_*.R`. Issues doc: T20-T23.
- **Marriage credit:** lesser earner's share uses earned income only (the
  M1MA lines 1-5 pension/SS elements unobserved); the printed lookup
  table's midpoint rounding ignored.
- **Renter's credit (2024+):** ON the M1 via Schedule M1RENT but requires
  rent data we lack — STRUCTURAL totals difference from TY2024;
  PolicyEngine includes it (expected one-sided divergence).
- **MN AMT (6.75%)** document-only per the no-state-AMT policy (PE models
  it — expect residuals on high-SALT itemizers). NIIT encoded; its
  agricultural-land carve-out unobservable, threshold treated unindexed.
- **Skipped subtractions:** QPEN public-safety pensions (2023+), military
  pensions, K-12 education, 529, M1R elderly/disabled, US-obligation
  share, bonus-depreciation 80%/5-year mechanics.
- **Conformity:** fixed-date (May 1, 2023 currently) modeled as rolling;
  2025 OBBBA nonconformity is below-AGI for the marquee items and does
  not reach MN's own deduction stack.

## Cross-model validation notes

- TAXSIM 2017-2020: **triage 2026-08-11 — eleven probe cases match TAXSIM
  to the cent (or ~$1 indexed rounding) across all three regimes,
  dependents, the sliding SS subtraction, the marriage credit, and the
  WFC at phase-in/phase-out/childless/2-child edges.** No MN encoding
  defects on definable shapes; the clean-match residual concentrates in
  itemizers (M1SA components + TAXSIM SALT circularity) and the
  documented 2017 M1M addbacks (KD row).
- PolicyEngine 2021+: models everything we encode PLUS the MN AMT,
  renter's credit (2024+), and QPEN — three expected one-sided divergence
  sources.

## Aggregate validation notes

- Blocked on Phase 1 weights; compare with MN DOR income tax statistics.
  Note the renter's credit exclusion (2024+) when reading totals.
