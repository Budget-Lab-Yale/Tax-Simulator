# California State Source Packet

State: `CA`  
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-08-16`

> **Status note (as of 2026-08-16), kept from the packet's former Status line:**
> resident regular IIT encoded; cross-model DONE 2026-08-16 -- all eight canonical-window cells clear the 95% bar (TAXSIM 0.969-0.985, PE 0.965-0.995); P1 production readiness still blocked

> **Cross-model triage 2026-08-15** (see `cross_model/results/reports/ca.md`
> and the CA rows of `src/tests/state/cross_model/known_differences.csv`):
> 1. The triage surfaced a CROSS-STATE calculator defect: `do_taxes.R` zeroed
>    all `*_item_ded` components for federal standard-deduction takers, so the
>    state pass could never itemize state-only. Fixed via as-if-itemizing
>    `*_item_ded_potential` columns (do_taxes.R preserves; st_ded.R, the WI
>    credit, and the TAXSIM state-mode crosswalk consume for
>    independent-election states only). PolicyEngine replays pinned our
>    Schedule CA component math to the cent once fed the right inputs.
> 2. Our-side CA fixes: `sub_ui_share` (Schedule CA UI subtraction was
>    missing), the 2017 CalEITC age band (25-64 pre-AB 1809; was flat 18+),
>    and year-keyed CalEITC income gates (were pinned at the 2025 value).
> 3. The old report's "state EITC" stage was 97% a harness artifact (CalEITC
>    lives in `st_earned_credit`, which matches TAXSIM's v39 on 97.4% of the
>    full fed-aligned sample); the classifier now counts it.
> 4. External-model issues filed: T15 (TAXSIM pays 2017 CalEITC to 65+
>    childless, live-verified at -195.10 for a 68-year-old), P6 (PE pays
>    CalEITC to MFS unconditionally), P7 (PE models no CA addback of
>    non-CA municipal interest).
> 5. 2026-08-16 follow-up: the CalEITC residual dig found a table-lookup
>    rounding bug (fractional incomes fell in the one-dollar crack between
>    whole-dollar FTB bins and got $0, ~20-45 records/yr); fixed with
>    floor(x + 0.5) per the sibling KY/CT callers (test CA-13).
> 6. 2026-08-16 batch (post-close-out hardening): HSA-deduction addback
>    (`add_hsa`; IRC 223 nonconformity), the CalEITC investment-income
>    ceiling (`earned_credit_inv_inc_limit`; FTB 3514 Worksheet 1 limits
>    read from each year's instructions, $3,561-$4,814), and the
>    model-wide US-obligation interest subtraction (`sub_us_int` x 15%
>    share assumption; ALL-states KD exclude on txbl_int > 5,000 since
>    neither external model takes the subtraction). Tests CA-14/15/16.
> 7. 2026-08-16 credit-stack close: the PE residual dig found California's
>    CDCTC (FTB 3506, stepped 50/43/34% of the federal credit by federal-AGI
>    tier, nonrefundable) missing from our encoding -- verified to the
>    dollar on five records -- now encoded via the generic share-band
>    machinery (tests CA-17/17b/17c). PE itemizer-exposure KD row added
>    (the PE crosswalk hands only property tax/mortgage/charity/childcare);
>    P8 filed (PE skips the FTB 3514 earned-vs-AGI second lookup,
>    table-exact on two records).
> 8. CROSS-MODEL DONE: all eight canonical-window cells clear the 95% bar
>    -- TAXSIM 0.985/0.977/0.972/0.969 (from 0.61-0.73 at the start of
>    triage), PE 0.995/0.965/0.984/0.967. One unexplained recurring PE
>    record (154685, ~-2.5k/yr, zero crosswalk exposure) documented in the
>    report. Misc itemized + CA AMT follow-up planned in
>    research/state_tax/notes/ca_misc_amt_plan.md.

## Scope

- Tax years targeted: 2017-2035. The 2017-25 standard deductions, exemption credits, rate brackets, and exemption-credit thresholds are transcribed from FTB Form 540 packets.
- Resident Form 540 baseline only. No nonresident allocation, AMT, or special-credit carryovers. The 1% Behavioral Health Services Tax is included.
- Major features: federal-AGI start, independent state itemization, graduated rates, exemption credits, refundable CalEITC, and the taxable-income surtax. A generic safety gate rejects federal-reform runs for California until reference-law results are available.

## Primary sources

- [2025 Form 540 booklet](https://www.ftb.ca.gov/forms/2025/2025-540-booklet.html), including 2025 schedules X/Y/Z and exemption-credit worksheet.
- [2025 FTB 3514 booklet](https://www.ftb.ca.gov/forms/2025/2025-3514-booklet.html) for CalEITC, YCTC, and FYTC.
- [2017 Form 540 booklet](https://www.ftb.ca.gov/forms/2017/17-540-booklet.html) for the rollout-window rate schedules.
- [2018 Form 540 booklet](https://www.ftb.ca.gov/forms/2018/18-540-booklet.html), [2019 Form 540 booklet](https://www.ftb.ca.gov/forms/2019/2019-540-booklet.html), [2020 Form 540 booklet](https://www.ftb.ca.gov/forms/2020/2020-540-booklet.html), [2021 Form 540 booklet](https://www.ftb.ca.gov/forms/2021/2021-540-booklet.html), [2022 Form 540 booklet](https://www.ftb.ca.gov/forms/2022/2022-540-booklet.html), [2023 Form 540 booklet](https://www.ftb.ca.gov/forms/2023/2023-540-booklet.html), and [2024 Form 540 booklet](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.html) for the missing historical schedules and worksheets.
- [FTB conformity guidance](https://www.ftb.ca.gov/tax-pros/law/conformity.html).
- [FTB CalEITC eligibility and credit information](https://www.ftb.ca.gov/file/personal/credits/caleitc/eligibility-and-credit-information.html).

## Parameter inventory

- `agi.yaml`: federal AGI start, fixed-date conformity metadata, municipal-interest adjustment, Social Security/state-refund/unemployment subtractions, the HSA-deduction addback (IRC 223 nonconformity; account earnings remain unobserved), and the U.S.-obligation interest subtraction (15% model-wide share assumption of taxable interest; source split unobserved).
- `ded.yaml`: full 2017-25 standard-deduction history, dependent-filer worksheet, and generic component-based California itemization without the federal SALT cap. It includes Schedule CA's high-income limitation for observed protected components.
- `ord.yaml`: 1 to 12.3 percent rates and full 2017-25 Schedule X/Y/Z bracket series.
- `credits.yaml`: full 2017-25 exemption-credit and phaseout series, annual CalEITC AGI-lookup safe harbors, and the 2019-25 YCTC formula and zero-income thresholds.
- `credit_tables.csv`: 20,738 nonzero annual CalEITC lookup rows for 2017-25, including FTB's 2017 footnote tails. `research/state_tax/scripts/refresh_ca_3514_tables.ps1` reproducibly imports those rows from official FTB pages.
- `surtax.yaml`: the 1% Behavioral Health Services Tax over $1 million of taxable income, calculated after nonrefundable credits.

## FTB 3514 CalEITC Table Inventory

The official FTB 3514 pages contain one annual, machine-readable Earned Income Tax Credit table. Each row is an inclusive $50 income band with four credit columns: zero, one, two, and three-or-more qualifying children. These pages are the source of record for the eventual exact CalEITC parameter file; do not reconstruct the schedule from a rate summary or use the table maxima as a triangular-credit schedule.

| Year | Official FTB 3514 source | Rows | Income bands | Maximum credit: 0 / 1 / 2 / 3+ children |
| --- | --- | ---: | --- | ---: |
| 2017 | [instructions](https://www.ftb.ca.gov/forms/2017/17-3514-instructions.html) | 447 | $1-$22,350 | $223 / $1,495 / $2,467 / $2,775 |
| 2018 | [instructions](https://www.ftb.ca.gov/forms/2018/18-3514-instructions.html) | 499 | $1-$24,950 | $232 / $1,554 / $2,559 / $2,879 |
| 2019 | [instructions](https://www.ftb.ca.gov/forms/2019/2019-3514-instructions.html) | 600 | $1-$30,000 | $240 / $1,605 / $2,651 / $2,982 |
| 2020 | [instructions](https://www.ftb.ca.gov/forms/2020/2020-3514-instructions.html) | 600 | $1-$30,000 | $243 / $1,626 / $2,691 / $3,027 |
| 2021 | [instructions](https://www.ftb.ca.gov/forms/2021/2021-3514-instructions.html) | 600 | $1-$30,000 | $255 / $1,698 / $2,809 / $3,160 |
| 2022 | [instructions](https://www.ftb.ca.gov/forms/2022/2022-3514-instructions.html) | 600 | $1-$30,000 | $275 / $1,843 / $3,037 / $3,417 |
| 2023 | [booklet](https://www.ftb.ca.gov/forms/2023/2023-3514-booklet.html) | 619 | $1-$30,950 | $285 / $1,900 / $3,137 / $3,529 |
| 2024 | [booklet](https://www.ftb.ca.gov/forms/2024/2024-3514-booklet.html) | 639 | $1-$31,950 | $294 / $1,958 / $3,239 / $3,644 |
| 2025 | [booklet](https://www.ftb.ca.gov/forms/2025/2025-3514-booklet.html) | 658 | $1-$32,900 | $302 / $2,016 / $3,339 / $3,756 |

When California earned income differs from federal AGI, Form 3514 requires an earned-income lookup and, above a child-count-specific safe harbor, an AGI lookup. The allowed credit is the lower of the two lookup results. The safe harbors for zero / one / two-or-more qualifying children are: 2017 $3,446 / $5,175 / $7,265; 2018 $3,580 / $5,376 / $7,547; 2019 $3,705 / $5,564 / $7,811; 2020 $3,757 / $5,642 / $7,920; 2021 $3,922 / $5,890 / $8,268; 2022 $4,248 / $6,379 / $8,954; 2023 $4,380 / $6,577 / $9,232; 2024 $4,525 / $6,794 / $9,537; and 2025 $4,661 / $6,998 / $9,823.

2017 has table-footnote tails that must be retained as explicit input ranges, rather than inferred from the final $50 band: the final $1 credits extend through $15,008 for zero children, $22,322 for one child, $22,309 for two children, and $22,302 for three-or-more children. The later tables end at the published income limits shown above.

The 2020 FTB HTML has one malformed printed lower bound (`086`) in the row ending at $1,150. The adjacent $50-band invariant and the published table establish the intended $1,101-$1,150 range. The refresh script repairs only endpoints that violate the $50-band invariant, and the loader rejects overlapping ranges so source-markup errors cannot silently change a lookup result.

The generic implementation now stores row data keyed by state, tax year, inclusive lower income, inclusive upper income, and capped qualifying-child count. It selects the most recent published table on or before the simulation year, so the 2025 schedule is an explicit carry-forward proxy for 2026 onward pending new FTB forms. The published table amounts and the second-lookup/safe-harbor rule are exact through 2025; legal eligibility remains constrained by available microdata.

## Worksheet tests

- 2025 two-child filer at $9,825 earned income: validates $1,103 exemption credits and the $3,339 CalEITC table peak.
- 2025 one-child filer with $7,000 California earned income and $10,000 federal AGI: validates the mandatory second FTB 3514 lookup and its $1,162 lower result.
- 2025 YCTC phaseout and the 2022 zero-income YCTC expansion are worksheet-tested, alongside the 2018 scalar-history and 2025 Social Security/state-refund cases.
- The 2025 dependent standard-deduction worksheet, high-income itemized-deduction limitation, and Behavioral Health Services Tax threshold are worksheet-tested.

## P1 readiness

See [California parameter analysis](research/state_tax/notes/ca_parameter_analysis.md) and [California P1 readiness analysis](research/state_tax/notes/ca_p1_readiness_analysis.md). A generic conformity gate now prevents invalid federal-reform results, but the required reference-law mechanism, material Schedule CA adjustments, and CalEITC/YCTC eligibility inputs remain release blockers. California should not be included in a production `states = all` run until those blockers are closed or a generic release gate excludes it.

## Known differences

- CalEITC table amounts and FTB's second-lookup rule are exact for 2017-25. The current model still proxies California earned income with `ei1 + ei2`, qualifying children with `n_dep_eitc`, and does not observe California residence/withholding, ITIN status, investment income, prior disallowance, separated-spouse, or all child-identification rules. The 2025 table is carried forward after 2025 until FTB publishes later instructions.
- YCTC's resident 2019-25 amount and phaseout formula are encoded as a single per-return credit. Its 2022+ zero-income path uses available wages and a Schedule C/E/F current-loss proxy; FTB's complete Form 540 current-year-loss measure and claimant eligibility inputs remain unavailable.
- Foster Youth Tax Credit, renter's credit, child/dependent-care credit, special credits, AMT, and health-coverage penalties are omitted because their eligibility inputs are absent or incomplete.
- Schedule CA income adjustments, especially state-specific federal-conformity adjustments, capital-loss carryovers, and retirement treatment, are incomplete.
- The high-income itemized-deduction limitation is encoded for medical, investment-interest, and casualty components. California also protects gambling losses, but the current `other_item_ded` input does not isolate them; mortgage, charitable, and other deductions retain their documented reconstruction limitations.
- The generic reference-law framework and California's 2015/2025 group assignments are encoded, but California's selective-adoption overlays are not yet validated. Federal-reform runs that include California therefore fail clearly rather than letting altered federal-law outputs flow through.
- The PUF does not separately identify California municipal obligations, U.S. obligation interest, California withholding, ITIN eligibility, or California residence duration; shared or documented proxies apply.

## Cross-model and aggregate validation

- Cross-model: TRIAGED 2026-08-15 (see the status blockquote above). TAXSIM window clears the acceptance bar in all four years; PE 2021/2023 clear and 2022/2024 sit at 0.943/0.936 with the residual characterized as scattered low-income credit-margin records. CalEITC validated against TAXSIM's v39 at 97.4% full-sample agreement.
- Aggregate: blocked until weights land; compare HT2 income/returns/liability and FTB annual statistics, separately reporting omitted refundable credits.
