# Oklahoma State Source Packet

State: `OK`
Status: `research COMPLETE and primary-verified; NOT yet encoded (YAML drafted, tests drafted)`
Last updated: `2026-08-13`

> The $17,000 itemized-cap gap is RESOLVED (st_ded.item_flat_cap landed
> 2026-08-12) and the child-credit greater-of is now encodable
> (st_credits.ctc_cdctc_greater_of, 2026-08-13 — see §Explicit encoding
> decisions #2); the credit-proration gap remains open (§Machinery gaps). The
> rate-schedule recovery below is the load-bearing research: **no Form 511
> packet prints a bracket schedule in any year 2017-2025.**

## Scope

- TY2017-2025 plus the enacted TY2026 schedule (HB 2764, 2025).
- Resident individual income tax only (Form 511). No Form 511-NR allocation.
- Major features: federal-AGI start, rolling conformity; a SIX-band
  MICRO-BRACKET schedule whose top band begins at only $7,200 of taxable
  income, so essentially every filer with liability sits on the top marginal
  rate; **HEAD OF HOUSEHOLD SHARES THE MARRIED COLUMN**; a standard deduction
  FROZEN in statute at pre-TCJA federal levels since TY2017; $1,000 flat
  exemptions plus a $1,000 blind add-on and a $1,000 age-65 add-on carrying a
  hard FEDERAL-AGI CLIFF; full SS subtraction; a $10,000-per-person retirement
  exclusion shared across public and private plans; a coupled itemized
  deduction with an income-and-sales-tax addback and (from TY2018) a $17,000
  CAP excluding charity and medical; a 5% EITC, NONREFUNDABLE 2017-2021 and
  REFUNDABLE from TY2022 on a FROZEN TY2020-rules federal base; and a
  greater-of 20%-CDCC / 5%-CTC child credit with a $100,000 AGI cliff.

## Rate-schedule recovery (the load-bearing work)

Tax is published only as (i) a Tax Table in $50 ranges to $100,000 and (ii) a
printed top-bracket constant. Three vintages, established by one table cell
read in all nine packets:

| TY | 50,000-50,050 S/MFS | MFJ/HoH/QSS | 15,000-15,050 S/MFS | MFJ |
|---|---|---|---|---|
| 2017-2021 | 2,313 | 2,146 | 563 | 396 |
| 2022-2023 | 2,188 | 2,021 | 525 | 359 |
| 2024-2025 | 2,188 | **1,999** | 525 | **337** |

TY2024 moves ONLY the married column. Printed top-bracket constants:
2017-2021 `$4,812 + 5%` single / `$4,645 + 5%` married; 2022-2023
`$4,562 + 4.75%` / `$4,395 + 4.75%`; 2024-2025 `$4,562 + 4.75%` /
`$4,373 + 4.75%`. Back-solving each to cumulative tax at the top-bracket knot
and independently summing the recovered schedule agree in all five cases
($171.50 / $255.00 for V1 single/MFJ, $153.50 / $224.50 for V2, $307.00 for V3
MFJ). Table cells then reproduce at the **midpoint of each $50 range** rounded
to whole dollars -- verified at eight cells across TY2017/2019/2022/2024/2025.
There is no separate low-income table and **no zero-tax cliff** (unlike KS/OH).

**Rates**: 2017-2021 `0.5 / 1 / 2 / 3 / 4 / 5%`; 2022-2025 every rate cut
0.25pp by HB 2962 (2021) -> `0.25 / 0.75 / 1.75 / 2.75 / 3.75 / 4.75%` (the
enrolled text strikes and replaces EACH of the six; a partial cut is the
plausible error); 2026 `0 / 2.5 / 3.5 / 4.5%` by HB 2764.

**Brackets**, single/MFS (unchanged 2017-2025):
`0 / 1,000 / 2,500 / 3,750 / 4,900 / 7,200`; TY2026 collapses the first three
into a 0% band ending at 3,750.
**MFJ/HoH/QSS**: `0 / 2,000 / 5,000 / 7,500 / 9,800 / 12,200` through TY2023,
then **`14,400`** from TY2024 -- HB 1040X (2023 1st Ex. Sess.) changed ONE
number, the fifth increment, from $2,400 to $4,600. Note the married column is
**NOT** 2x single before TY2024.

## Primary sources

- Form 511 resident packets, ALL NINE YEARS, via the NBER mirror
  (`taxsim.nber.org/historical_state_tax_forms/OK/{year}/`); TY2025 also from
  oklahoma.gov (md5-identical). The `oklahoma.gov/.../{year}/` archive pattern
  404s; only `current/` exists.
- Forms 538-S (sales tax relief) and 538-H (property tax relief).
- Statute read directly (`oklegislature.gov/OK_Statutes/CompleteTitles/os68.pdf`):
  68 O.S. 2358 v3 subsections A, E(1)(a)-(c), E(2)(g), E(3)(b), E(8), E(9),
  E(13), E(17), E(18), E(21), E(23), F; 2357(B)(2); 2357.43; 2368(C);
  5010-5012.
- Session laws, enrolled text: **HB 2962 (2021)** (0.25pp cut to all six
  rates; EITC refundability restored with the TY2020 frozen base);
  **HB 1040X (2023 1st Ex. Sess.)** (the one-number married-bracket change,
  corroborated by the OTC *Summary of 2023 Tax Legislation*); **HB 2764
  (2025)** (TY2026 schedule + trigger).

## Corrections to prior assumptions

- The **HoH** age-65 special-exemption AGI limit is **$19,000**, not $20,000.
- There is **no broader gross-income variant** of that test (the footnote is a
  Roth-conversion carve-out).
- The EITC clause reads "**in effect for the 2020 income tax year**", not "as
  it existed on January 1, 2020".
- **PE's TY2024 $14,400 married threshold is NOT a PE bug** -- it is HB 1040X,
  confirmed in enrolled text, the OTC summary, and the TY2024/2025 tax tables.

## Explicit encoding decisions

1. **Age-65 special exemption -> `st_agi.age_ded_*`, not
   `st_exempt.aged_addl`.** The exemption is $1,000 per 65+ person but only if
   FEDERAL AGI is at or below 15,000 / 25,000 / 12,500 / 19,000 (single / joint
   / MFS / HoH). `st_exempt` has no per-add-on income test (its `po_thresh`
   zeroes the WHOLE allowance, which is not the law), so `aged_addl = 1000`
   with no test would grant ~$47.50 to every 65+ Oklahoman regardless of income
   (~$25M/yr of spurious tax cut). The VA `age_ded_*` family instead subtracts
   $1,000 per qualifying person reduced $1-for-$1 above the mapped threshold:
   form-exact outside a $1,000-per-person AGI ramp where the statute has a
   cliff, bounded error $47.50/person. Consequence: the $1,000 lands in
   `st_subtractions`, so reported `st_agi` runs $1,000-$2,000 below Form 511
   line 7 for these units -- nothing else in the OK encoding reads state AGI.
2. ~~**Child credit: encode the 5%-of-federal-CTC leg only.**~~
   **SUPERSEDED 2026-08-13 — encode BOTH legs with
   `st_credits.ctc_cdctc_greater_of = 1`.** The original decision existed only
   because no greater-of machinery did: `st_cdctc` and `st_ctc` are SUMMED, so
   encoding both would have overstated by `min(20%CDCC, 5%CTC)` for every
   care-expense unit, while the CDCC leg alone would have zeroed the credit for
   the far larger population of families with children and no care expenses
   (~$40M/yr). The CTC-only fallback was exact for that majority and understated
   care-expense units by `max(0, 20%CDCC - 5%CTC)` (<= $40 for a two-child
   family 2018-2025, <= ~$140 in TY2017 when the federal CTC was $1,000). The
   flag now zeroes the smaller leg, so both `st_ctc` and `st_cdctc` report what
   was claimed and no residual remains (tests MACH-9 / MACH-9b).
3. **Sales tax relief credit: documented, not modeled.** Three blockers.
   (a) `st_credits.percap_*` has no income test at all. (b) The measure is
   gross HOUSEHOLD income -- "the total amount of gross income received by ALL
   persons living in the same household whether the income was taxable or
   not", explicitly including public assistance, child support, workers' comp,
   scholarships, VA disability, gross SS including Medicare, dependents'
   income and the EIC received, with negative amounts prohibited -- matching no
   `st_income_base` enum and not a tax-unit concept. (c) The $50,000 tier needs
   a dependent / 65+ / disability gate, and TANF receipt in any month
   disqualifies. Encoding `percap_amount = 40` without the test would pay
   ~$95M/yr against ~$36M actual. The KS food-sales-tax-credit precedent.
4. **`item_coupling` 1, both directions**: "If you claimed itemized deductions
   on your federal return, you must claim itemized deductions on your Oklahoma
   return. This is true even if the Oklahoma itemized deductions are less than
   the Oklahoma standard deduction." Not a best-of election -- if a harness
   flags OK units taking a $3,000 itemized deduction against a $6,350 standard
   deduction, that is correct behavior.
5. **TY2026 encoded, trigger not.** 2355(D) is enacted law with a stated tax
   year. 2355(E)'s 0.25pp-per-certification reductions are contingent on Board
   of Equalization certifications that have not occurred (earliest effect
   TY2028) -- the NC / KS SB 269 precedent.
6. **EITC on the CURRENT federal credit**, with the frozen-base wedge
   quantified below rather than building reference-law machinery.

## Machinery gaps (no representable parameter)

1. ~~**$17,000 itemized cap (TY2018+), charity and medical exempt.**~~
   **RESOLVED 2026-08-12.** `st_ded.item_flat_cap` (default `.inf`) plus
   `item_flat_cap_excl_medical` / `_excl_charity` were added and tested (tests
   MACH-4 / MACH-4b), applied as `min(cap, base - exempt) + exempt` after the
   existing limitations and before the itemize election. OK's cap is now
   encodable exactly, so the ~$10-20M/yr omission below no longer applies.
   (Why nothing existing worked: `item_limit_style` 1 is income-based,
   `item_limit_protect_*` has no CHARITY flag, and `addback_cap` measures the
   federal deduction including income/sales taxes and charity.)
2. **OK AGI / federal AGI proration of the EITC and child credit** (Schedules
   511-F and 511-G). Applies to FULL-YEAR residents whenever OK AGI is below
   federal AGI -- i.e. anyone with an SS, retirement, military, tribal, 529 or
   out-of-state modification. No machinery scales a credit by a base ratio.
   PE models it; this is our-side.
3. **Per-person retirement caps pooled**: `st_sub_pens_raw` pools cap1 + cap2
   at unit level, so a one-pension couple can reach $20,000 where OK allows
   $10,000. Same limitation DE documents.

## Known differences (direction and magnitude)

- **Military retirement exclusion omitted -- the largest single item.** OK
  excludes the greater of 75% or $10,000 (TY2017-2021) and 100% (TY2022+) of
  Armed Forces retired pay; status is unobserved, so these units get only the
  general $10,000. **Overstates** their liability; order of magnitude on
  ~40,000 OK resident returns at ~$28,000 average retired pay: ~$550/return
  (~$22M/yr) pre-2022 and ~$855/return (~$34M/yr) from TY2022, i.e. roughly
  0.6-1.0% of OK individual income tax receipts. Both TAXSIM and PE model it.
- **Sales tax relief credit omitted**: understates refunds by up to $40 x
  (filers + dependents), ~$30-40M/yr statewide. Both models grant it -> expect
  point masses at $40 multiples on low-income units in every year.
- **Frozen-TY2020 EITC base ignored (TY2022+)**: we overstate the OK EITC.
  Plateau wedge = 5% x (current max - 2020 max):

  | TY | 0 kids | 1 kid | 2 kids | 3+ kids |
  |---|---|---|---|---|
  | 2022 | $1.10 | $7.45 | $12.20 | $13.75 |
  | 2023 | $3.10 | $20.55 | $34.20 | $38.50 |
  | 2024 | $4.70 | $31.45 | $52.00 | $58.50 |
  | 2025 | $5.55 | $37.20 | $61.60 | $69.30 |

  Larger in the phase-out range (the frozen phase-out start is also
  unindexed), and the frozen $3,650 investment-income limit vs $11,600 (2024)
  makes some federally eligible units entirely ineligible for the OK credit
  where we pay the full 5%. Aggregate ~5-7% of a ~$42M/yr credit by 2024-2025.
  **TY2021 is deliberately NOT frozen** -- no vintage clause existed then, so
  OK's 5% applied to the ARPA-expanded credit (and was nonrefundable).
- **$17,000 itemized cap omitted (TY2018+)**: overstates the deduction,
  understates liability, for itemizers whose non-charitable non-medical
  deductions exceed $17,000. ~$10-20M/yr. TY2017 is exact (no cap existed).
- **Itemized SALT convention**: `st_item_default` replaces the capped federal
  SALT with UNCAPPED property and personal-property taxes; Schedule 511-D
  instead starts from the federal Schedule A total (SALT already capped) and
  removes only the income/sales component. Overstates OK itemized deductions
  post-2018 for high-property-tax filers. Model-wide v1 convention.
- **EITC/child-credit proration not applied** -> overstates both credits for
  units with any OK subtraction.
- **Retirement cap pooled** -> over-excludes up to $10,000 (<= $475) for
  couples where one spouse holds all the retirement income.
- **Aged-exemption cliff replaced by a $1,000-per-person ramp** -> overstates
  by <= $47.50/person inside a $1,000 AGI band. The Roth-conversion carve-out
  from the AGI test is unobservable and ignored.
- **Tax-table discretization**: filers below $100,000 use $50-range midpoints;
  we apply the continuous schedule. Deterministic, up to ~+/-$1.20.
  **Sub-$2 residuals below $100k are this, not an encoding error.**
- Federal CSRS-in-lieu-of-SS exclusion (100%), Railroad Retirement, exempt
  tribal income (potentially material in Oklahoma post-*McGirt*), active-duty
  military pay, the OK capital gain deduction (Form 561), OK depletion at 22%,
  529/OklahomaDream and ABLE contributions, bonus-depreciation decoupling, OK
  NOL, out-of-state losses and PTE-election income: all unobservable, omitted,
  net effect overstates the OK base.
- **Form 538-H property tax relief credit** omitted (refundable, <= $200, 65+
  or disabled, gross household income <= $12,000): Tier-1 blocked. PE models it.
- QSS files with the MFJ standard deduction but only ONE $1,000 exemption; we
  map QSS to status 2 and give two. Overstates their deduction by $1,000.
- Credit for tax paid to another state, the Form 511-CR menu, Parental Choice
  credits (TY2024+), natural disaster credit, Form 578 zero-emission credit,
  Form 573 farm income averaging, the HSA 10% additional tax and IRC 965(h)
  installments: not modeled.

## Uncertainties

1. **The married/HoH/QSS top bracket moves $12,200 -> $14,400 at TY2024 and
   nowhere else.** The state's highest-risk vintage line: invisible in the
   single column, no rate changes, no printed schedule. Triple-confirmed
   (enrolled text, OTC summary, and the TY2025 cell 14,400-14,450 = $308,
   which $12,200 would make $330). Do not simplify it away.
2. HoH takes the MARRIED rate column and the MARRIED threshold structure but
   only ONE personal exemption -- verified on the table column header and
   2355(C)(2) in all three vintages.
3. **`pension_excl_min_age = 0` is a substantive reading**: the age-65 and
   OK-AGI conditions in 2358(E)(13)(a)-(b) expired for tax years beginning
   after 2004 and 2009, and no 2017-2025 packet imposes either. Worth a probe.
4. **TY2017 `pease = 1`** rests on Form 511 line 10A reading "the amount of
   your **allowable** itemized deductions" plus the packet's own high-income
   worksheet. Our calculator applies Pease AFTER removing the income/sales
   component; the form prorates that component by the Pease cut first.
   Second-order, high-income only.
5. **`req_if_fed_filer` = 0**: 2368(C) (TY2017+) states a pure OK gross-income
   test; the federal-filer trigger in 2368(A) covers only "tax years ending
   before January 1, 2017". Our test reads `st_agi` where the law reads GROSS
   income, and the HoH threshold ($10,350) omits the dependent exemption a HoH
   necessarily has (true $11,350) -- both filer counts only.
6. **TY2026 has no published Form 511 yet.** Forward check when it appears:
   the printed top-bracket constants should be **$4,285 + 4.5%** (single) and
   **$4,071 + 4.5%** (married).
7. **`dep_filer_zero = 1`** rests on the form and is corroborated by Chart B's
   $6,350 dependent threshold (= the standard deduction alone) against Chart
   A's $7,350 (= deduction plus one exemption). PE does NOT zero it
   (`ok_count_exemptions` counts the head unconditionally) -> expect a
   $1,000-base PE wedge on dependent filers.

## Cross-model and aggregate validation

- **TAXSIM 2017-2020 (PE has NO Oklahoma income-tax values before TY2021 --
  every file begins 2021-01-01, and PE returns None before a parameter's first
  value, so PE cannot compute OK tax at all for 2017-2020) / PE 2021-2025.**
- Pre-register before any run: the military retirement omission (largest); the
  538-S sales tax relief omission ($40 multiples); the 538-H property tax
  credit; the frozen-EITC wedge (we run high from TY2022); the credit
  proration (we run high on units with subtractions); the $17,000 cap and the
  uncapped-property-tax convention (we run low on high-deduction itemizers);
  the tax-table +/-$1.20 discretization; and in the PE direction, PE's missing
  muni addback, missing state-refund subtraction, and un-zeroed dependent-filer
  exemption.
- Aggregate: blocked on weights. Benchmark against the OTC *Annual Report*
  individual income tax collections and the OTC *Tax Expenditure Report*
  entries for the retirement exclusions, the sales tax relief credit and the
  EITC.
