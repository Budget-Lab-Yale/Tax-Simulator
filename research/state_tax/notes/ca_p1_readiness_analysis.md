# California P1 Readiness Analysis

State: `CA`  
Status: `blocked pending tax-base completion, credit-eligibility inputs, and California reference-law bridges`  
Last updated: `2026-07-13`

## Decision

California is useful as a development configuration, but it is not production-ready
for `states = all` or for federal-reform analysis. The two P1 findings are
independent release blockers:

1. The 2017-25 scalar series and CalEITC lookup tables are transcribed, but
   Schedule CA and California itemized-deduction rules remain partial and the
   credit eligibility inputs rely on documented PUF proxies.
2. The generic fixed/selective-conformity framework is now implemented, but
   California's 2015- and 2025-based selective-adoption overlays are not yet
   researched or validated. The two California groups are therefore marked
   unavailable, and federal-reform runs reject California rather than use the
   scenario's federal results.

The immediate metadata correction is encoded in `baseline/ca/agi.yaml`: California's
general IRC conformity date is January 1, 2015 through tax year 2024 and January 1,
2025 from tax year 2025. Both periods retain statutory exceptions. The FTB states the
2025 change directly in its [conformity guidance](https://www.ftb.ca.gov/tax-pros/law/conformity.html), and the [2024 Form 540 booklet](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.html) confirms the pre-2025 January 1, 2015 date.

## P1-A: Historical Parameters

### What Is Complete

- Standard deductions, Schedule X/Y/Z brackets, exemption-credit amounts, and
  exemption-credit phaseout thresholds are transcribed for 2017-25.
- The nine marginal rates are stable at 1, 2, 4, 6, 8, 9.3, 10.3, 11.3, and 12.3
  percent.

### What Blocks Release

- `credit_tables.csv` now contains all 20,738 nonzero FTB 3514 lookup rows from
  2017-25, including 2017 footnote tails. The generic calculator selects the
  annual earned-income table, uses the federal-AGI lookup above the annual
  child-count safe harbor, and takes the required lower result. The 2025 table
  is the explicit carry-forward proxy after 2025.
- CalEITC still requires California withholding/self-employment income,
  residency, investment-income, identity, prior-disallowance, and filing-status
  information that the PUF does not fully identify. The current `ei1 + ei2`,
  federal AGI, and `n_dep_eitc` inputs are documented proxies, not legal
  entitlement determinations.

The primary annual packet is available from the FTB: [2018](https://www.ftb.ca.gov/forms/2018/18-540-booklet.html), [2019](https://www.ftb.ca.gov/forms/2019/2019-540-booklet.html), [2020](https://www.ftb.ca.gov/forms/2020/2020-540-booklet.html), [2021](https://www.ftb.ca.gov/forms/2021/2021-540-booklet.html), [2022](https://www.ftb.ca.gov/forms/2022/2022-540-booklet.html), [2023](https://www.ftb.ca.gov/forms/2023/2023-540-booklet.html), and [2024](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.html). The FTB's [2025 3514 booklet](https://www.ftb.ca.gov/forms/2025/2025-3514-booklet.html) and [2020 3514 instructions](https://www.ftb.ca.gov/forms/2020/2020-3514-instructions.html) confirm the lookup-table mechanics and historical eligibility differences.

### Generic Implementation And Remaining Validation

The reusable independent-earned-income-credit table primitive is now general,
not California-specific. Its rows are keyed by:

```text
state, year, income_lower, income_upper, qualifying_children_capped, credit
```

The generic calculator caps qualifying children at the table's terminal category,
performs the earned-income lookup, conditionally performs the AGI lookup, and
applies the published safe-harbor rule before taking the lower result. Eligibility
flags and proxy assumptions belong in generic input handling and the source packet,
not in a state-name conditional. The source table remains versioned row data rather
than hundreds of YAML scalar columns.

The validation suite should include, for each table year, zero, one, two, and
three-or-more qualifying children; the start and end of each phase-in, plateau, and
phaseout segment; a differing earned-income/AGI pair below and above the safe harbor;
and zero-credit boundary rows. Current tests cover the 2025 peak, a two-input
lookup, the 2017 footnote tail, table loading through state aggregation, the normal
YCTC phaseout, and the 2022 zero-income YCTC expansion; exhaustive boundaries
remain a record-level validation gate.

## P1-B: Fixed And Selective Federal Conformity

### Motivating Behavior (Now Resolved By The Guard)

`run_one_year()` calculates federal tax first and passes those scenario outputs to
`get_state_totals()`. That function joins a state law and calls `do_state_taxes()`;
`calc_st_agi()` reads the scenario `agi` and `txbl_inc` for rolling-conformity
states. Absent a guard, a federal reform would flow into California even when
California does not adopt that provision. The reference-context framework below,
together with `validate_state_federal_conformity()`, now prevents this: the sole
remaining blocker is that California's conformity groups (1, 2) still lack a
`reference_tax_law_id` overlay (`ready: false`).

The existing plan's proposed baseline-record join is not correct either. It would
freeze a conventional-run tax unit at its pre-behavior income and eliminate legitimate
behavioral changes. A correct implementation must retain changes to wages and other
input income while holding only the applicable federal-law definition fixed.

### Implemented Generic Design

1. State law now supplies a numeric group, while
   `config/scenarios/tax_law_state/conformity_groups.yaml` maps ready groups to a
   federal overlay. A date is an identifier, not a complete policy specification.
2. `build_tax_law_from_id()` parses a federal baseline plus any overlay without
   scenario-output side effects. The state runner builds one reference law and one
   calculated context per `(year, group, pass)`, not per state.
3. The reference pass begins with the scenario's post-calculation tax unit, retains
   payroll pass-through and behavioral changes, removes scenario federal results,
   and recalculates federal variables under the reference overlay. Behavioral modules
   are never run a second time.
4. `get_state_totals()` selects the complete cached context by group before calling
   `do_state_taxes()`, so state rules can consume AGI, taxable income, credits, and
   deduction components without a two-column snapshot or a state-name branch.
5. The unchanged `run_one_year()` signature keeps the SLURM worker synchronized:
   reference law and context creation occur inside the existing year task.

This design avoids state-name branches in the calculator. California-specific work is
limited to the reference-law/bridge configuration and Form 540 validation cases.

### Guard And Tests

Until California's reference-law bridges are complete,
`validate_state_federal_conformity()` hard-stops a federal-reform run that requests
California. The `states = all` registry should exclude California from production
runs until both P1 blockers close, or a generic release gate must do so.

Required generic tests:

1. Generic unit tests now cover rolling versus fixed routing, missing-context failure,
   identical normal/reference contexts, and a reusable reference-law overlay.
2. California still needs Form 540 cases for a provision it adopts and one it does
   not, demonstrating each legal bridge.

## Closure Criteria

Close P1 only after CalEITC/YCTC eligibility proxies and 2017-25 table boundaries
are fully validated, material resident Schedule CA and itemized-deduction differences
are resolved or explicitly excluded, and the
California's selective-conformity bridges pass
federal-reform tests. Cross-model and aggregate validation remain separate gates.
