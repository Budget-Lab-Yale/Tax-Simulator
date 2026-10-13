# State EITC cross-check with State-EITC-Align (2026-10-02)

State-EITC-Align is the state EITC parameter repo (`johniselin-econ/State-EITC-Align`).
It adjudicates state EITC parameters against primary law: statutes, session
laws, and the revenue department's forms. It compared those values with this
branch at commit `1f39b771b` (`cross-model-refit-filers`). It reads the state
credits read-only, from `config/scenarios/tax_law_state/baseline/*/credits.yaml`
and `wa/transfers.yaml`.

The two repos agree on 961 overlapping state × year × parameter keys, apart
from the items below. Packet IDs (for example `MN-2022-01`) refer to that
repo's `adjudication/episodes/<ST>/`. Each packet quotes its primary sources
and links the saved copy under `data/raw/primary/<ST>/`. Confidence follows
that repo's scale: A means statute and a same-year form agree; B means primary
law but no same-year form.

**Please keep `1f39b771b` reachable.** State-EITC-Align pins this commit.
Rebasing or squashing `cross-model-refit-filers` before it merges would make
that repo's build stop. Its build checks that the pinned commit exists and is
on this branch.

## For review: our evidence contradicts the encoding

1. **MN Working Family Credit: childless minimum age is 19, not 21.**
   - `mn/credits.yaml` `earned_credit_age_min` is 21 from TY2019, with no later
     change.
   - The 2022 edition of Minn. Stat. 290.0671 subd. 1(a)(1) reads: "a taxpayer
     with no qualifying children who has attained the age of 19, but not
     attained age 65 before the close of the taxable year".
   - The TY2022 Schedule M1WFC agrees: "None (if between the age of 19 and 64)".
   - Packet `MN-2022-01`, confidence A, covers TY2022 only.
   - The section history lists 1Sp2021 c 14 art 1 s 10 after the 1Sp2017
     change that set 21. That amendment is probably the move to 19, so check
     whether TY2021 is affected as well.
   - Packet `MN-2023-01` keeps 19-64 for the TY2023 working family credit
     inside the M1CWFC (the combined Child and Working Families Credit), so
     check which age the CWFC machinery applies from 2023.

2. **MD: the 100% childless refundable credit starts in TY2020, not TY2021.**
   - `md/credits.yaml` `eitc_childless_match` starts in 2021, with the note
     "TY2020 applicability UNVERIFIED".
   - Laws of Maryland 2021, ch. 39 (SB 496, the RELIEF Act) enacts the
     100% credit, capped at $530 (§ 10-704(c)(3)(ii)-(iii)).
   - Its Section 14 reads: "Section 2 of this Act shall be applicable to all
     taxable years beginning after December 31, 2019." It was approved
     February 15, 2021.
   - The TY2021 booklet confirms the credit. Packet `MD-2020-01`, confidence B.
   - The same act's 45% refundable rate is already encoded here from TY2020
     (`eitc_match_alt`). As encoded, TY2020 applies the act's rate change but
     not its childless credit.

3. **MD: the childless credit disregards the federal minimum age, from TY2018.**
   - § 10-704(c)(3)(i) computes the credit for a filer without a qualifying
     child "without regard to: 1. the minimum age requirement under
     § 32(c)(1)(A)(ii)(II) ... or 2. the limitation under § 32(m)".
   - `st_credits_earned.R` (around lines 286-292) takes
     `eitc_childless_match × tax_unit$eitc`, which is the federal credit with
     its age test. So childless filers under 25 get nothing in Maryland.
   - Packet `MD-2018-02`, confidence B. We have not yet retrieved the enacting
     chapter.

## For review: our reading of the code, not checked against primary sources

4. **WA Working Families Tax Credit (`wa/transfers.yaml`, `st_special.R`).**
   - `wftc_mfs_eligible` is 1 from 2023, with no qualifying-child requirement.
     That differs from the IRC 32(d)(2) logic added for federal, CA and MN in
     `22c259749`.
   - The childless age is a flat 25-64 in every year.
   - State-EITC-Align has adjudicated the WFTC amounts, minimum credit and
     phase-out starts for 2022-23, and they match here. It has not adjudicated
     these eligibility rules.

5. **State bars on separate filers after `22c259749`.**
   - The federal EITC now reaches married-filing-separately filers with a
     qualifying child from 2021.
   - These state bars are noted as "not gated" on the grounds that a separate
     filer rarely has a federal credit to match, which no longer holds: NJ
     `eitc_mfs_rule` (barred TY2018-21), MA, NE, the MO WFTC, VA's proration,
     and MT.
   - NJ TY2021 may now pay a match the booklet bars.

6. **Housekeeping.** The P6 status note in `external_model_issues.md`
   (`8ab92d98c`) still says federal `eitc.mfs_eligible` is 0 in every year.
   `22c259749` changed that.

## For information

- **MD TY2023 $600 cap.** It equals the federal childless maximum, so it never
  binds. Laws of Maryland 2023, ch. 4 removed the State cap from TY2023
  (packet `MD-2023-01`). This needs no change.
- **DE refundable option start (2021).** State-EITC-Align's value is currently
  taken from this repo, so the two agree without independent evidence. It is
  not yet adjudicated.
- **Fixes that already agree with State-EITC-Align:**
  - CalEITC for separate filers from 2021 (`22c259749`).
  - The MN childless age cap of 64 (`9aa8b4758`).
- **Items here that State-EITC-Align lacks and is now researching:**
  - The NJ TY2020 childless age band (`abcae2764`).
  - The M1CWFC 9% phase-out for families with only older children.
  - ME TY2019 at 5%: this repo and the co-author panel agree on 5%, while
    State-EITC-Align's provisional value is 12%.
