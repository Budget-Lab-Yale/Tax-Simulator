# California Individual Income Tax Parameter Analysis

State: CA
Scope: resident Form 540 individual income tax, tax years 2017-2025
Last updated: 2026-07-13
Status: research baseline; not ready for production state totals or federal-reform analysis

## Decision And Boundary

California has a conventional broad individual income tax, but it is not a rolling-conformity state. The present configuration is appropriate for developing the generic state framework and testing the regular rate structure. It is not a complete California Form 540 calculator.

This analysis separates the regular resident tax from other amounts reported on Form 540. The first is the production target. The second is catalogued so that we do not accidentally represent an incomplete liability as comprehensive.

| Component | Current status | Release disposition |
| --- | --- | --- |
| Rates, brackets, standard deduction, exemption credits | 2017-25 scalar series transcribed | Exact, subject to worksheet validation |
| Federal-AGI start and simple Schedule CA subtractions | Partial | Social Security and state refunds encoded; other adjustments remain |
| State itemization | Partial | Independent election, dependent worksheet, and high-income limitation exist; source-component reconstruction remains incomplete |
| CalEITC | 2017-25 table schedule and safe-harbor rule encoded | Amount calculation is exact; legal eligibility uses documented PUF proxies |
| YCTC | 2019-25 resident formula encoded; FYTC omitted | Zero-income YCTC path uses a current-loss proxy; FYTC needs foster-care history |
| Renter and child/dependent-care credits | Omitted | Need housing/care-expense and eligibility inputs |
| AMT, credit limitation, special credits, health penalty, use tax | Omitted | Outside the regular-tax release; separately scoped |
| Behavioral Health Services Tax | Encoded | 1% taxable-income surtax after nonrefundable credits; validated at the $1 million threshold |
| Fixed/selective conformity | Framework implemented; CA bridges unavailable | Federal-reform runs fail until the 2015- and 2025-based California overlays are researched and validated |
| 540NR/part-year allocation and RDP pro formas | Omitted | Resident Form 540 scope only |

The current state weights are still placeholders. No California aggregate can be treated as a revenue estimate until the weight work is complete.

## Sources And Research Rule

The annual Form 540 booklets and Schedule CA instructions are the parameter source of record. Use the FTB's [2017](https://www.ftb.ca.gov/forms/2017/17-540-booklet.html), [2018](https://www.ftb.ca.gov/forms/2018/18-540-booklet.html), [2019](https://www.ftb.ca.gov/forms/2019/2019-540-booklet.html), [2020](https://www.ftb.ca.gov/forms/2020/2020-540-booklet.html), [2021](https://www.ftb.ca.gov/forms/2021/2021-540-booklet.html), [2022](https://www.ftb.ca.gov/forms/2022/2022-540-booklet.html), [2023](https://www.ftb.ca.gov/forms/2023/2023-540-booklet.html), [2024](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.html), and [2025](https://www.ftb.ca.gov/forms/2025/2025-540-booklet.html) packets. Use the annual FTB 3514 instructions/booklets for refundable credits, not a secondary rate summary.

Every future California feature must first be classified as one of: an existing generic parameter, a new generic primitive, an unavailable microdata item, or an out-of-scope filing/payment item. Do not add a California-name conditional to a calculator.

## Confirmed Scalar Parameters

The regular tax uses stable marginal rates of 1, 2, 4, 6, 8, 9.3, 10.3, 11.3, and 12.3 percent. Schedule X (single/MFS), Y (joint/QSS), and Z (head of household) brackets for every year 2017-25 are now in ord.yaml.

The following values are the standard deduction for single/MFS and joint/HOH/QSS, the per-person personal/blind/senior credit, the per-dependent credit, and the AGI at which exemption-credit limitation begins (single, joint, head). Credits are nonrefundable and the AGI worksheet reduces each credit by $6 for each rounded-up $2,500 of excess AGI ($1,250 for MFS).

| Year | Standard: single | Standard: joint | Personal | Dependent | Exemption phaseout: S/J/H |
| --- | ---: | ---: | ---: | ---: | --- |
| 2017 | 4,236 | 8,472 | 114 | 353 | 178,706 / 357,417 / 268,065 |
| 2018 | 4,401 | 8,802 | 118 | 367 | 194,504 / 389,013 / 291,760 |
| 2019 | 4,537 | 9,074 | 122 | 378 | 200,534 / 401,072 / 300,805 |
| 2020 | 4,601 | 9,202 | 124 | 383 | 203,341 / 406,687 / 305,016 |
| 2021 | 4,803 | 9,606 | 129 | 400 | 212,288 / 424,581 / 318,437 |
| 2022 | 5,202 | 10,404 | 140 | 433 | 229,908 / 459,821 / 344,867 |
| 2023 | 5,363 | 10,726 | 144 | 446 | 237,035 / 474,075 / 355,558 |
| 2024 | 5,540 | 11,080 | 149 | 461 | 244,857 / 489,719 / 367,291 |
| 2025 | 5,706 | 11,412 | 153 | 475 | 252,203 / 504,411 / 378,310 |

The prior configuration had stale 2017-18 standard deductions and several 2018-23 exemption-credit values. These values, all phaseout thresholds, and all annual rate brackets have now been corrected/transcribed. Test CA-2 protects the 2018 values against an accidental carry-forward.

## Tax Base And Schedule CA

California starts from federal AGI but uses Schedule CA to apply its own tax base. The 2025 Schedule CA instructions explicitly require adjustments to federal AGI and federal itemized deductions. The general IRC conformity date is January 1, 2015 for 2017-24 and January 1, 2025 for 2025, with important selective exceptions. See the FTB [conformity guidance](https://www.ftb.ca.gov/tax-pros/law/conformity.html) and [2025 Schedule CA instructions](https://www.ftb.ca.gov/forms/2025/2025-540-ca-instructions.html).

### Encoded Or Directly Supported

| Provision | Model treatment | Important limitation |
| --- | --- | --- |
| Federal AGI starting point | Encoded | Must use reference-law results when a federal reform is modeled |
| Other-state municipal-bond interest | Added back using generic municipal-interest rule | PUF does not identify California municipal interest; existing 75% own-state-share proxy is only a sensitivity assumption |
| U.S. obligation interest | Not encoded | A generic U.S.-obligation-interest input/share is required |
| Taxable Social Security/Tier 1 railroad benefits | Fully subtracted | Encoded as ss_sub_share = 1; Tier 1 cannot be separated from the PUF Social Security field |
| State income-tax refunds | Fully subtracted | Encoded as sub_state_ref = 1 |
| Long-term capital gains | Taxed at ordinary rates | California basis, installment-sale, and carryover differences remain unmodeled |

The current generic engine does not cover California's exclusion of unemployment and paid-family-leave benefits, railroad Tier 2 benefits, and numerous form-specific income adjustments. Those include HSA treatment, foreign earned income, alimony by agreement date, California lottery winnings, California NOLs, depreciation/amortization, federal CFC/GILTI inclusions, 529 differences, qualified stock, disaster payments, tribal income, military exceptions, and historic IRA/pension basis. Most require either a separate source variable or a generic adjustment ledger keyed to a tax-law reference definition. They must not be approximated by a state-specific residual.

### Conformity Requirement

For rolling-conformity groups the state pass consumes the scenario's federal AGI, taxable income, and post-federal deduction fields directly (conformity_year is a documentation field, not a calculator input). California is instead assigned to fixed/selective conformity groups (1, 2), so this direct pass-through no longer applies to it: the reference-context framework and `validate_state_federal_conformity()` now prevent an unadopted federal reform from flowing into California.

The generic design is a post-behavior reference federal calculation, cached once per (year, conformity group, static/conventional pass), whose record-level outputs form a state-base context. This machinery is implemented (`build_state_reference_tax_laws()`, `build_state_reference_contexts()`, `state_tax_context_for_group()`). California needs a 2015-based reference/overlay for 2017-24 and a 2025-based reference/overlay thereafter. The context includes every federal result consumed by state rules, not only AGI, so behavioral income changes survive while federal-law definitions do not. The remaining blocker is that conformity groups 1 and 2 have no `reference_tax_law_id` overlay defined yet (`ready: false`); until they do, `validate_state_federal_conformity()` rejects federal-reform runs that request California.

## Deductions

California allows an independent itemization choice. The current component model correctly uses state/local real and personal property taxes but now excludes state/local income and general sales taxes, which Schedule CA expressly disallows. It also intentionally does not apply the federal SALT cap.

The following rules prevent the present implementation from being exact:

| Rule | Needed generic/data work |
| --- | --- |
| Dependent standard-deduction worksheet | Encoded with the generic min(max(earned income plus annual add-on, floor), status maximum) formula; uses available earned-income inputs |
| MFS election | Generic filing-status-specific requirement that spouses use the same itemization election |
| Mortgage interest | Reconstruct the California $1 million acquisition-debt and $100,000 home-equity rules from debt/use data; federal deductible interest is not enough |
| Charitable contributions | Reconstruct California's 50% AGI limitation, conservation rule, and carryovers; federal char_item_ded is not a California measure |
| Casualty and miscellaneous deductions | Preserve California's nonconformity to federal TCJA suspensions; required expense/loss inputs are not in the current state context |
| Investment interest and other itemized adjustments | Need California forms/carryovers or a separate state itemized input |
| High-income limitation | Encoded generically: reduce the lesser of 80% of observed unprotected deductions and 6% of federal AGI above the annual threshold. Medical, investment interest, and casualty deductions are protected; gambling losses remain unseparated inside `other_item_ded`. |

The high-income threshold is the same annual S/J/H series shown above. The regular-form tax table below $100,000 also has whole-dollar table rounding, whereas the current rate-schedule calculation is continuous; validate that difference separately and document the accepted rounding tolerance.

## Credits

### Exemption Credits

The personal, blind, senior, and dependent credits are now configured with the annual values and common AGI limitation worksheet. The PUF still cannot identify the California alternative-ID dependent procedure and California RDP pro formas; the federal dependent count is the documented proxy.

### California Earned Income Tax Credit

CalEITC is refundable, has up to three qualifying-child categories, and uses a published annual $50 lookup table. It requires California withholding/self-employment earned income, a federal-AGI and California-earned-income limit, California residence conditions, and special rules for ITINs, combat pay, IHSS, and separated spouses. When earned income and AGI differ, FTB 3514 performs a second lookup subject to child-count-specific safe-harbor amounts, then chooses the specified result. The [2025 FTB 3514 booklet](https://www.ftb.ca.gov/forms/2025/2025-3514-booklet.html) documents this process; the [2022 instructions](https://www.ftb.ca.gov/forms/2022/2022-3514-instructions.html) show the same two-input method.

The reusable, versioned independent-earned-credit table is now implemented. `config/scenarios/tax_law_state/baseline/ca/credit_tables.csv` stores all 20,738 nonzero FTB 3514 rows for 2017-25, including the 2017 footnote tails; `research/state_tax/scripts/refresh_ca_3514_tables.ps1` refreshes it from the official annual HTML. The generic calculator performs the earned-income lookup, the federal-AGI lookup above the annual child-count safe harbor, and selects the lower result. It uses the most recent published table for future years, so the 2025 table is an explicit carry-forward proxy after 2025. `n_dep_eitc`, `ei1 + ei2`, and federal AGI remain clearly documented proxies for California qualifying children, California earned income, and state eligibility.

### Young Child And Foster Youth Credits

The YCTC is a refundable per-return credit, not a per-child credit. It requires CalEITC eligibility plus a qualifying child under age six. The generic young-child-credit primitive now encodes its maximum/start/zero values: $1,000 / $25,000 / $30,000 for 2019-21; $1,083 / $25,000 / $30,000 in 2022; $1,117 / $25,775 / $30,932 in 2023; $1,154 / $26,626 / $31,951 in 2024; and $1,189 / $27,425 / $32,901 in 2025. It follows Form 3514's continuous per-$100 reduction, $1 minimum positive credit, and nearest-dollar final rounding. From 2022, the zero-or-negative-earned-income path is also encoded with the annual wage, loss, and AGI limits. The model uses `wages1 + wages2` and a Schedule C/E/F current-loss proxy; FTB's complete Form 540 total-net-loss definition, residency, ITIN, and other CalEITC eligibility data remain unavailable.

FYTC began in 2022. It has the same annual dollar schedule as YCTC, applies per eligible taxpayer age 18-25, and requires California foster-care history from age 13. Foster-care history is not available in the PUF, so this credit is correctly omitted rather than imputed. The 2022 and 2024 FTB 3514 instructions describe the eligibility and annual schedules.

### Other Credits

| Credit class | Status and required input |
| --- | --- |
| Nonrefundable renter's credit | $60 single/MFS or $120 joint/HOH/QSS in 2025, with AGI limits; requires California rent, taxability of property, dependency, and property-tax-exemption information not in the PUF |
| Child and dependent care credit | Nonrefundable; uses care expenses, qualifying-person tests, California care location, earned income, and AGI at or below $100,000. The model's generic CDCTC machinery may be reusable, but it needs a California rate table and validated care-expense input. |
| Other-state tax, PTE elective-tax, prior-year AMT, adoption, business/investment, and carryover credits | Omitted. These are form/K-1/carryover driven and many are credit-limited under Schedule P. Add a generic credit ledger only when the source fields and carryover scope are defined. |

## Other Form 540 Liabilities

The California Behavioral Health Services Tax (formerly Mental Health Services Tax) adds 1% of taxable income above $1 million. It is now encoded through a generic taxable-income-surtax component in `surtax.yaml`, using whole-dollar taxable income and adding the amount after nonrefundable credits. The 2025 Form 540 line 62 worksheet and the $1 million threshold are covered by `CA-10`; refundable credits continue to apply at the final liability stage.

California AMT and credit limitation are also omitted. Schedule P recomputes alternative minimum taxable income, has state-specific adjustments and carryovers, and limits credits. The [2025 Schedule P instructions](https://www.ftb.ca.gov/forms/2025/2025-540-p-instructions.html) confirm that it is not a simple federal-AMT match. Tax on children's investment income, lump-sum distributions, retirement-plan penalties, recaptures, the individual health-coverage penalty, and use tax are likewise outside the regular IIT baseline and need separate scope decisions.

## Filing Population And Geography

The current filing rule is a conservative federal-filer-or-liability proxy. California's statutory filing thresholds vary by gross income or AGI, filing status, age, and number of dependents. A reusable threshold table would permit an exact resident filing requirement. Resident Form 540 is the current boundary: Form 540NR allocation, part-year residency, California-source income, and California RDP federal pro formas are not represented by the PUF/state-weight setup and must remain excluded from the release claim.

## Completion Sequence And Validation

1. Research and encode the 2015- and 2025-based California selective-conformity overlays, add primary-source Form 540 cases, and then mark their generic conformity groups ready.
2. Expand CalEITC/YCTC record-level validation to every annual table boundary and eligibility gate that current microdata can observe; document the remaining residency, identity, investment-income, and loss-definition exclusions.
3. Add a generic state-adjustment ledger/source-field contract, then implement the material California Schedule CA items that the PUF can support.
4. Add the MFS joint-election primitive and distinguish reconstructable California deductions from unavailable data; the dependent worksheet and high-income limitation are now encoded.
5. Scope AMT and credit limitation only after their necessary preference, K-1, and carryover inputs are available.
6. Validate Form 540 resident worksheets in 2017, 2020, and 2025: each filing status, each rate boundary, standard vs itemized election, exemption-credit phaseout, Social Security/refund adjustments, CalEITC/YCTC schedule boundaries, and the $1 million surtax threshold. Then compare aggregate returns, AGI, tax before credits, refundable credits, and liability with FTB statistics once state weights are available.
