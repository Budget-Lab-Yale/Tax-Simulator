# Elderly and retirement-income provisions across modeled states

Survey date: 2026-07-23. Scope: the 21 encoded jurisdictions with an
individual tax program (18 broad-IIT + NH/TN narrow + WA), from the parsed
2025 baseline law (`elderly_params_2025.csv` regenerable from
`build_state_tax_law`) plus each source packet's omissions list. "Elderly
break" = any provision keyed on age or on retirement-type income (Social
Security, pensions/IRA/annuities).

States reach the elderly through three channels, in descending revenue
importance: (1) Social Security exclusion, (2) pension/retirement-income
exclusion, (3) age-keyed exemptions/deductions/credits.

## 1. Social Security treatment (of the federally taxable amount)

| State | Law | Modeled |
|---|---|---|
| AZ CA GA IL IN KY MI **NC** NY SC VA ID | Fully exempt (subtraction of taxable SS) | `ss_sub_share = 1` — **NC's was MISSING until 2026-07-23** (G.S. 105-153.5(b)(6)); found by this survey, fixed with test NC-4 |
| CO | Full at 65+; full at 55-64 under $75k AGI (2025 law; 2022+ expansion) | `ss_full_sub_65plus/5564` + AGI limit |
| CT | Full under $75k/$100k AGI; above, taxable SS capped at 25% of gross benefits | `ss_full_sub_allages` + `ss_taxable_gross_cap_share = 0.25` |
| ND | None 2017-18; AGI-capped full 2019-20 ($50k/$100k); full 2021+ | year-keyed `ss_sub_share` / `ss_full_sub_allages` |
| UT | Credit, not exclusion: 4.5% (=rate) x taxable SS, phased 2.5c/$ above $54k MAGI (2025) | `ss_credit_*`, exclusive with the retirement credit |
| OH | Fully exempt | `ss_sub_share = 1` |
| PA | Never in the base (own-base class design) | `ob_ss_share = 0` |
| NH TN WA AK FL NV SD TX WY | No broad IIT — SS untouched by design | n/a |

Bottom line: every modeled income-tax state shelters SS wholly or largely,
and all of it is now encoded. This is the single largest elderly break
everywhere.

## 2. Pension / retirement-income exclusions

Encoded (per person unless noted):

| State | Provision (2025 values) | Machinery |
|---|---|---|
| GA | Retirement exclusion $65,000 at 65+ / $35,000 at 62-64, incl. up to $5,000 earned income — the most generous modeled | `retirement_excl_*` style 1 |
| KY | $31,110 pension/IRA exclusion, any age ($41,110 pre-2018) | `pension_excl_*` |
| CO | $24,000 at 65+ / $20,000 at 55-64, cap SHARED with fully-subtracted SS | `pension_excl_*` + `pension_cap_incl_ss` |
| NY | $20,000 private pension/IRA at 59.5+ (gov pensions fully exempt — treated as private, understates; known-diff) | `pension_excl_*` |
| SC | $10,000 at 65+ / $3,000 under 65 retirement deduction | `pension_excl_*` |
| CT | 100% pension + 75% IRA (2025 phase-in) x AGI-banded factor table (cliff pre-2024, published phase-out 2024+) | `pension_sub_share`/`ira_sub_share` + dense factor table |
| IL | ALL federally-taxed retirement income (pensions, IRA, SS) subtracted, any age | `ss_sub_share` + full pension subtraction |
| OH | Banded retirement-income credit ($25-$200 by retirement income, <$100k base) + $50 senior credit at 65+ | `retire_credit_*`, `senior_credit_*` |
| UT | $450 age credit (1952-cohort frozen, so 73+ in 2025) OR the SS credit, phased 2.5c/$ | `age_credit_*`, return-level exclusivity |
| PA | ALL qualifying retirement distributions outside the base (59.5+ IRA / plan-age employer distributions) | `ob_retirement_share = 0` |

Documented as OMITTED (all bias modeled liability UP for affected retirees):

| State | Omitted provision | Why | Materiality |
|---|---|---|---|
| **MI** | The retirement/pension subtraction: pre-2023 birth-cohort tiers (born <1946: private-pension deduction ~$61k/$122k indexed; 1946-52: $20k/$40k all-income at 67; younger: $20k/$40k at 67 in lieu of SS+exemption), then the 2023 PA-4 restoration phasing 25/50/75/100% over TY2023-26 toward the Tier-1 cap for ALL cohorts | Cohort+source+election complexity during the phase-in | **LARGEST GAP in the module.** By TY2026 the law converges to a near-universal pension deduction at the Tier-1 caps — exactly encodable with existing `pension_excl_*` machinery for 2026+, and approximable for earlier years via year-keyed `pension_excl_min_age` (birth cohort = year − age). Recommend encoding. |
| **SC** | $15,000/person age-65 deduction, reduced dollar-for-dollar by the retirement deduction claimed (12-6-1170(B)) | **ENCODED 2026-08-11** (`age_ded_less_pension_sub` coupling on the VA-style machinery; tests SC-8/SC-9; TAXSIM retiree probes agree within $3.20) | Was the second-largest gap (~$700-900/person at 65+). Unit-level offset is exact for singles and both-65+ couples; over-offsets up to $3,000 when only an under-65 spouse claims (A). |
| **ID** | Retirement-benefits deduction (63-3022A): caps $68,796 MFJ / $45,864 single (2024), 65+ (62+ disabled), reduced $1:$1 by SS received, and ONLY for CSRS-pre-1984, military, and Idaho police/fire annuities | Pension source unobservable in the PUF; the SS offset zeroes it for most SS recipients (average SS benefit exceeds half the single cap) | Small-to-moderate; recommend KEEP OMITTED. Cross-model evidence: TAXSIM does not model it either (pension-age records are UNDER-represented in ID residual mismatches — 2026-07-23 dive), so it costs nothing in validation. Revisit at aggregate validation vs STC stats. |
| NC | Bailey-settlement pensions (pre-1989-vested gov) and military retirement (2021+) fully deductible | Pension source unobservable | Moderate for NC's large federal/military retiree population; keep documented. |
| CT | Teacher (25/50%) and military/railroad pension carve-outs | Source unobservable subsets | Small (main pension subtraction IS modeled). |
| KY | Pension-source distinctions (govt pre-1998 service fully exempt above the $31,110 cap) | Source unobservable | Small; cap modeled. |
| AZ | $2,500 US/AZ government-pension exclusion; military retirement fully exempt 2021+ | Source unobservable | Small. |
| OH | Uniformed-services retirement exclusion; lump-sum retirement credits | Source unobservable / data limits | Small. |
| UT | Military retirement credit | Source unobservable | Small. |

## 3. Age-keyed exemptions / deductions / credits (non-pension)

Encoded:

| State | Provision | Machinery |
|---|---|---|
| VA | $12,000/person age deduction at 65+ ($1:$1 AFAGI phase-out above $50k/$75k; born-before-1939 grandfather), mutually exclusive with EITC/CLI; + $800 aged exemption | `age_ded_*`, `aged_addl`, exclusivity machinery |
| IL | $1,000 aged exemption per 65+ filer | `exempt.aged_addl` |
| CA | $153 aged exemption credit per 65+ filer (phase-out above high AGI) | `exempt_credit_aged` |
| OH | $50 senior credit (65+, <$100k modified income, once per return) | `senior_credit_*` |
| ID | Grocery credit aged add-on: +$20 per 65+ filer/spouse through 2024 (eliminated 2025) | `percap_aged_addl` |
| NH | $1,200 age-65 exemption against the I&D tax | `investment_income.age_exemption` |
| TN | Full Hall-tax exemption at 65+ under $37k/$68k income; age-100 exemption | `full_age_*`, `age_100_full_exempt` |
| AZ CT GA + std-deduction states | Federal-style aged standard-deduction add-ons where states carry them | `std_aged_addl` (state-specific values) |
| CO ND ID SC | Federal aged std add-on flows through the federal-taxable-income start automatically | structural |

Omitted (small): IN's income-tested $500 elderly exemption (65+ under $40k
AGI — needs a generic income-tested exemption; flagged since the IN packet);
MI's homestead credit (needs property tax/rent/household-resources inputs);
CA senior head-of-household credit; SC's 65+ filing threshold add-on.

## How the elderly are taxed, in one paragraph

In every modeled income-tax state, Social Security is now fully or largely
exempt (worth up to ~$1,100-2,300 per retiree household at 4.5-6.9% rates on
typical taxable-SS amounts), and that channel is fully encoded including
NC's previously missing deduction. The second channel — pension/IRA
exclusions — splits the states into full-exclusion (IL, PA structurally,
MS-style later), capped-exclusion (GA $65k, KY $31k, CO $24k w/ SS
interplay, NY $20k, SC $10k, CT share-based), credit-style (UT, OH), and
none (AZ beyond sources, IN, ND, ID beyond narrow sources). The third
channel — age-keyed allowances — is mostly small ($50-$1,200 equivalents)
except VA's $12,000 income-tested age deduction. The two material encoding
gaps are MI's restored retirement subtraction (near-universal by TY2026)
and SC's $15,000 age-65 deduction; both are encodable with modest
extensions of existing machinery. ID's 63-3022A stays omitted deliberately:
its qualifying sources are unobservable, its SS offset mostly zeroes it,
and TAXSIM omits it too.

## Recommended actions (priority order)

1. **DONE (this survey): NC SS deduction** — encoded, test NC-4. Rerun
   verified the fix: TAXSIM-window clean match@$100 rose 59.4→66.9 (2017),
   66.0→75.0 (2019), 65.8→74.9 (2020); the PolicyEngine window now sits at
   93.4–97.0% clean, with 2021–22 above the 95% acceptance bar. 2018
   remains an NC-specific outlier (53.5%) for the standing triage.
2. **DONE (2026-07-23): MI retirement subtraction** — encoded via a new
   per-return, older-spouse age-band family (`pension_excl_band_*`):
   birth-year tiers as year-keyed ages (Tier 1 born <1946 at the indexed
   Form 4884 cap; Tier 2 $20k/$40k), the PA 4 of 2023 phase-in with its
   expanding birth-year windows (NOT age-free: born 1946-1958/-62/-66 for
   2023/24/25; everyone 59+ from 2026), and the senior investment income
   subtraction (born <1946). Research: raw/mi_retirement_research.md
   (PolicyEngine parameter values match ours exactly for every year).
   Tests MI-2..MI-10. Key corrections from research: the phase-in
   percentages apply to that year's indexed cap with expanding cohort
   windows (not age-free), and the older spouse's birth year controls the
   whole return (per-return caps, not per-person). The 67+ Michigan
   Standard Deduction was then also implemented properly as an ALL-income
   deduction with the Tier-3 SS/exemption netting after the first rerun
   showed +$850/+$1,700 point masses (20k/40k x 4.25%) on 67+ no-pension
   records. Cumulative cross-model effect: TAXSIM-window clean match@$100
   58.9/58.1/57.9/57.3 -> 64.7/64.3/64.2/63.9; PE window 56.1-62.6 ->
   56.3-68.4. Largest remaining MI residual: an unexplained +$386 point
   mass (~370 records/yr in 2019, a constant ~$9,082 TAXSIM-side
   deduction) plus the homestead credit — next MI triage items.
3. **SC age-65 deduction** — add a `age_ded_offset_pension` coupling to the
   VA-style machinery ($15k/person less the retirement deduction claimed).
4. **IN income-tested elderly exemption** — fold into a generic
   income-tested exemption add-on if another state needs one; alone it is
   ~$500 x 3.05% ≈ $15/return.
5. **ID 63-3022A** — keep omitted; revisit against STC aggregates when
   weights land.
