# State Source Packet: New Jersey

State: `NJ`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-10-08` (high-income pass); previous: `2026-10-07` (State EITC cross-check); previous: `2026-08-18`

## Scope

- Tax years covered: 2017-2025
- Baseline only
- Major structural features: own base built from enumerated gross-income
  CATEGORIES with no cross-category loss offsets and no carryovers; no
  standard deduction; a pension exclusion that changed from a cliff to a
  tiered step-down in TY2021; a capped property tax deduction with a flat
  credit alternative

## Primary sources

- All nine NJ-1040 instruction booklets TY2017-TY2025, with PyMuPDF text
  extractions, plus the standalone form PDFs. NOTE `2017_1040.pdf` is
  field-only with no text layer, so TY2017 line references come from the
  booklet
- N.J.S.A. Title 54A for the rules that generate values
- The research pass also machine-compared all 10,015 rows of the printed Tax
  Table against the rate schedule, which is how the $0.50 schedule defect
  below was found

## Parameter inventory by file

### `agi.yaml`

- Encoded: `start_point` 0 with the `ob_*` class shares and `ob_class_floor` 1;
  `ob_ss_share` and `ob_ui_share` 0; the income-banded pension exclusion
  (`pension_excl_tier_*`) with per-element filing-status mapping
- Known approximations: elective deferrals; the qualifying-spouse pension
  split; the "other retirement income exclusion"

### `ord.yaml`

- Encoded: Table A and Table B rate schedules for all nine years, padded to
  eight elements

### `ded.yaml`

- Encoded: `prop_tax_ded_cap` ($10,000 then $15,000)
- Known approximations: the medical deduction's 2% floor on New Jersey gross
  income; the 18%-of-rent rule; the $50 credit alternative

### `exempt.yaml`

- Encoded: personal $1,000 per taxpayer, dependent $1,500, age-65 $1,000,
  blind/disabled $1,000
- Known approximations: the veteran and college-dependent exemptions

### `credits.yaml`

- Encoded: the earned income credit (35% to 40%) and the Child Tax Credit
  (tiered on state taxable income, refundable)
- Documented: the child and dependent care credit (the top follow-up)

### `filing.yaml`

- Encoded: $10,000 single/separate, $20,000 joint/head of household/surviving
  spouse, unchanged and unindexed

## New generic machinery introduced for New Jersey

1. `st_agi.pension_excl_tier_{bounds,caps,shares}` -- an income-banded pension
   exclusion computed as min(cap, share x eligible pension income) per band,
   tested on TOTAL income before the exclusion. Covers both the pre-2021 flat
   maximum behind a cliff and the post-2021 tiered step-down. The tier income
   is computed from the starting point ahead of the mutate, because testing
   anything downstream would be circular.
2. `st_ded.prop_tax_ded_cap` -- a capped property tax deduction.
3. `st_credits.ctc_tier_income_base` plus a ninth `st_income_base` enum
   (state taxable income) -- the tiered child credit now selects its tier on
   an enum base rather than hard-coded federal AGI.

## Worksheet tests added

NJ-1 the personal exemption alone; NJ-2 the property tax deduction; NJ-3a and
NJ-3b the cap at $15,000 and its $10,000 predecessor; NJ-4 the pension
exclusion inside the band; NJ-5a and NJ-5b the TY2020 cliff against the TY2021
37.5% tier on the same unit; NJ-6 a business loss failing to offset wages;
NJ-7 the Child Tax Credit tier; NJ-8 the earned income credit at 40%.

## Research findings worth flagging

- **The pension exclusion became tiered in TY2021, not TY2020.** The TY2020
  booklet still prints the flat "$100,000 or less" test at the fully phased-in
  maxima. Getting this wrong shifts a whole year of retiree liability.
- **The age test is 62, not 65**, and it is disjunctive with blindness or
  disability.
- **The Child Tax Credit was introduced in TY2022, not TY2023**, at $500 per
  child, and doubled to $1,000 for TY2023.
- **New Jersey excludes 401(k) deferrals from wages but TAXES 403(b), 457,
  federal Thrift Savings and SEP contributions.** The booklets state both
  sides explicitly. This is why New Jersey W-2 box 16 routinely exceeds box 1.
- **The printed rate schedule carries a $0.50 defect** relative to the Tax
  Table at band boundaries, found by comparing all 10,015 table rows.
- Only the top bracket ever moved (TY2018 and TY2020); nothing is indexed.

## High-income pass, 2026-10-08

After the October 1 close the remaining misses sat above $500,000. Three causes:

- **Ours, fixed: business, partnership and S-corporation income are separate categories.** Schedule NJ-BUS-1 Part I (NJ-1040 line 18), Part II (line 21) and Part III (line 22) are each summed with "If loss, make no entry", and the instructions say "You cannot apply a net loss in one category of income against income or gains in a different category." The model had one floored business class, so partnership losses erased sole-proprietorship profit (record 434680, 2024: $512,451 dropped). New generic `st_agi.ob_bus_split`; test NJ-6b.
- **TAXSIM never applies the 10.75% bracket (T28, widened).** Its marginal rate above $5 million is 8.97% in every year 2017-2020, so 2018-2019 tax above $5 million is understated by 1.78% as well as 2020's $1 million-$5 million band.
- **Form 4797 gains and losses (crosswalk).** New Jersey nets them with capital gains in the disposition-of-property category, floored at zero; both crosswalks hand them over as generic other/pass-through income. Input-coverage row on both legs.

## State EITC cross-check, 2026-10-07

The separate-filer bar (TY2018-TY2021 booklets) had been documented but not encoded, on the ground that a
separate filer rarely holds a federal credit. That stopped holding in TY2021, when IRC 32(d)(2) gave
separated spouses with a qualifying child a federal credit (`22c259749`). Now encoded through the generic
`st_credits.eitc_mfs_barred`: 1 for TY2018-TY2021, then 0 (from TY2023 the booklet's separated-spouse rule
is the federal one; TY2022 is undocumented and follows the federal credit). Tests NJ-1o/1p. The shared
childless recomputation that the age band uses moved to `childless_fed_credit_no_age` (Maryland uses it too);
NJ values are unchanged.

## Known differences

- **The child and dependent care credit is not modeled** -- a percentage of
  the FEDERAL credit tiered on New Jersey taxable income, nonrefundable and
  dollar-capped through TY2020 and refundable with a $150,000 ceiling from
  TY2021. It needs a banded percentage-of-federal-credit mechanism the
  calculator does not have. The top New Jersey follow-up.
- **Elective deferrals are not modeled**, so New Jersey wages are federal
  wages. This understates the base for 403(b), 457 and Thrift Savings
  participants -- teachers, public employees and non-profit staff. A Tier 2
  imputation target.
- **The medical expense deduction is not modeled**: New Jersey uses a 2% floor
  on its own gross income against the federal 7.5%, and the extra expenses
  cannot be recovered from the federal post-floor amount.
- **The "other retirement income exclusion" (line 28b) is left out entirely**
  rather than half-encoded. Its unclaimed-pension component turns on an
  ambiguity the booklets do not resolve -- Worksheet D takes a percentage of
  line 27 while line 28a takes a percentage of line 20a -- which determines
  whether that component is dead above $100,000 of total income. A follow-up
  agent was tasked with resolving it against N.J.S.A. 54A:6-15 and bulletin
  GIT-1 & 2.
- The 18%-of-rent property tax rule and the $50 property tax credit
  alternative are not modeled (rent is a Tier 1 target; the credit only beats
  the deduction for very low liabilities).
- The veteran ($3,000, then $6,000 from TY2019) and college-dependent
  exemptions, and the NJEITC's childless flat minimums and sub-federal age
  floor, are all unobserved or unencoded.

## Cross-model validation notes

- TAXSIM years to compare: 2017-2020; PolicyEngine 2021-2024
- Expected mismatch reasons: the care credit will show in family cells; the
  deferral treatment in public-employee cells; the pension exclusion tiers are
  worth checking carefully either side of TY2021 since an external model that
  dated the change to TY2020 would diverge sharply for that one year.

## Aggregate validation notes

- HT2 targets once weights land; the New Jersey Division of Taxation publishes
  statistics of income for a revenue-agency benchmark.

## Cross-model triage, 2026-10-01

Two causes found and fixed, both ours:

- **Losses were not netted within a category.** The own-base build used the
  PUF's gross positive fields (`part_active`, `part_passive`, `rent`), whose
  losses sit in separate fields, and floored short-term gains apart from
  long-term ones. The NJ-1040 nets within a category (net gains from the
  disposition of property is one category; partnership and rental income are
  net). Fixed model-wide in `st_agi.R` for all six own-base states; tests NJ-1b
  and PA-2b. NJ cells +2 to +3pp.
- **No tax at or below the filing threshold** (N.J.S.A. 54A:2-4: New Jersey gross
  income, line 29, at or below $10,000 single/separate or $20,000 otherwise) was
  missing. This was the "decile 3" pocket: we charged ~$220-250 just under
  $20,000. Encoded with the VA-style `st_filing.no_tax_below_thresh`; tests
  NJ-1c/1d/1e. TAXSIM cells +5 to +8pp, PolicyEngine +4 to +13pp. The statute
  text was not retrieved directly; TAXSIM and PolicyEngine both apply the rule,
  and the booklet's "Do You Have to File" page confirms its effect.

Result: TAXSIM 0.916 / 0.846 / 0.849 / 0.826 (2017-2020), PolicyEngine 0.818 /
0.924 / 0.928 / 0.911 / 0.924 (2021-2025).

Second pass (same day), all three external-model bugs, probe-verified:

- **T24 (TAXSIM):** `nonprop` (other income and alimony received) dropped from NJ
  state AGI. On the misses the AGI gap equalled positive `other_inc` to the dollar.
- **T25 (TAXSIM):** net capital losses (less the federal $3,000) reduce NJ state
  AGI, though the property-disposition category is floored at zero; the source of
  the very negative TAXSIM NJ AGIs. Pennsylvania's equivalent is right.
- **P11 (PolicyEngine):** the NJ child tax credit paid in 2020-2021, before the
  TY2022 start (parameters begin 2022-01-01); still present in 2.18.2.

After both passes: TAXSIM 0.942 / 0.923 / 0.927 / 0.908 (2017-2020), PolicyEngine
0.942 / 0.924 / 0.928 / 0.911 / 0.924 (2021-2025). The NOL records match far
better than they looked because TAXSIM drops negative `nonprop` too (T24), which
mimics our floor.

Third pass (same day), three of ours, each from the TY2018-2025 booklets:

- **Child tax credit paid to separate filers.** Line 65: "If your filing status is
  married filing separately, you are not eligible." New generic
  `st_credits.ctc_mfs_eligible` (default 1), NJ 0. Tests NJ-1j/1k.
- **The NJEITC for childless filers outside the federal age band.** Line 58: ages
  21-24 in TY2020; from TY2021 "at least 18 ... The maximum age limit has been
  eliminated", for a filer meeting every federal EIC test except age. The amount is
  flat, 40% of the federal childless maximum ($215, $601, $224, $240, $253, $260).
  New generic `st_credits.eitc_ageband_*` component (age band, share of the federal
  childless maximum; the other federal tests applied by recomputing the federal
  childless credit without the age test). Tests NJ-1f..1i. The earlier note that
  this was a *minimum* where the percentage match was smaller was wrong: the
  booklet pays the flat amount outright.
- **The child and dependent care credit,** formerly the top follow-up. Worksheet J:
  a stepped share of the FEDERAL credit on NJ taxable income. TY2018-2020: 50/40/30/
  20/10% across $20k-$60k, capped at $500 / $1,000, nonrefundable. TY2021+: 50% down
  to 10% across $30k-$150k, no cap, refundable. Built on the existing NY share-table
  style with flat bands. Tests NJ-1l/1m/1n.
- **T26 (TAXSIM):** v38 is zero on every NJ record in 2018-2020, so TAXSIM does not
  model the care credit; excluded where ours exceeds $100.

After the third pass: TAXSIM 0.942 / 0.923 / 0.927 / 0.907, PolicyEngine 0.958 /
0.968 / 0.971 / 0.946 / 0.941. PolicyEngine 2021-2023 clear 95%.

Fourth pass (same day):

- **Ours: estate and trust income was missing.** The booklet puts the NJK-1 total
  on the Other income line; it entered no own-base category. Now its own floored
  class (`st_agi.ob_estate_share`, default 0, NJ 1), so a disallowed NOL cannot
  offset it. Test NJ-1b2. On the misses where we were below TAXSIM, the shortfall
  equalled estate income one-for-one.
- **T27 (TAXSIM):** the pension exclusion starts at 65 (the form says 62) and tests
  the $100,000 cliff on federal AGI (the form tests line 27, NJ total income, which
  excludes Social Security). Confirmed in the TY2017 and TY2019 booklets.

After the fourth pass: TAXSIM 0.952 / 0.934 / 0.940 / 0.921, PolicyEngine 0.958 /
0.968 / 0.971 / 0.946 / 0.941. Four of nine cells clear 95%.

Harness fix (same day): **the TAXSIM crosswalk folded a negative `nonprop` (a
federal NOL deduction) into `otherprop`**, on the belief that TAXSIM rejects
negative `nonprop`. It does not (probe: -$20,000 gives the right federal AGI). Folded
into `otherprop`, the NOL netted against rent and other Schedule E income inside
TAXSIM's New Jersey category and floored it away, which was most of the remaining
NJ TAXSIM misses. Passed as is (`src/tests/test_taxsim.R`), NJ TAXSIM went to 0.969
/ 0.968 / 0.969 / 0.946; six of nine NJ cells now clear 95%.

Final pass (same day): **T28 (TAXSIM)**, the 2019 rate schedule applied in 2020 (the
10.75% bracket still from $5,000,000 instead of the TY2020 $1,000,000), and a
PolicyEngine assumption row for separate filers with care expenses (federal IRC
21(e)(4) living-apart condition: assumed met by us, not by PolicyEngine).

**NJ cross-model CLOSED for 2017-2024:** TAXSIM 0.969 / 0.968 / 0.969 / 0.975,
PolicyEngine 0.968 / 0.972 / 0.975 / 0.955. 2025 (0.941, 7 misses on 119 records)
sits outside the canonical window and is still under review.

Open, with evidence:

- **Net operating loss carryforwards.** Tax-Data carries them as negative
  `other_inc`; New Jersey allows no carryforwards, so our flooring of the other
  category is right, but neither outside model has an NOL input. Records with an
  NOL of $2,000 or more match at 0.68-0.72 in TAXSIM against 0.85-0.95 for the
  rest; about 70% still match, so no clean exclusion predicate yet. Also, 17% of
  sampled records carry such a loss, which looks high (a Tax-Data question).
- **Business categories lumped.** `ob_bus_share` pools net business profits,
  partnership and S corporation income (NJ-1040 lines 18, 21, 22), which the form
  keeps separate. This nets more than New Jersey allows (understates).
- **Estate and trust income** enters no own-base category (understates).
- **Alimony paid** is listed as not separable, but the federal calculation now
  carries `alimony_exp` (0.1% of misses).
- **TAXSIM's state AGI sits below ours** on ~17% of misses (median ~$4,400, AGI
  ~$200k, not pensions). Unexplained.
