# The mechanical pass

2026-07-29. Signed off in conversation; this note records the decisions and the
build plan. Built on branch `mechanical-pass`, except the gains elasticity
re-derivation -- see As built at the end.

## Motivation

The model reports two counterfactual numbers per scenario: static and
conventional. The gap between them mixes two different things: mechanical
base interactions (the corporate tax landing in shareholders' income, taxes
financed out of wealth draining the estate base, the employer payroll tax
coming out of wages) and behavioral responses (deferral, recharacterization,
avoidance). The top-tax report needs those separated, and the treatment today
is inconsistent: the corporate and decumulation channels sit on the
conventional side, but the employer payroll wage offset -- the same kind of
channel -- is applied inside the static pass.

The fix is a third reportable pass. The ladder becomes:

1. Static: new law priced on baseline behavior and baseline bases. No
   cross-base transmission of any kind. The employer payroll wage adjustment
   moves out of this pass.
2. Mechanical: static plus every mechanical transmission channel -- corporate
   incidence flows, the wealth-financing drawdown, the employer payroll wage
   adjustment. No behavior modules.
3. Conventional: mechanical plus behavior. Unchanged in meaning; its wedge
   relative to static is now decomposable as (mechanical - static) +
   (conventional - mechanical), with one fixed ordering: transmission first,
   behavior on the transmitted frame. That is already the order of operations
   inside the conventional pass, so the decomposition introduces no new
   order-dependence.

The kg frozen-realization content stays in static: it is baseline realization
behavior priced under the new law, which is law arithmetic, not transmission.

## The grid

Formally the passes are cells of law {baseline, reform} x bases {baseline,
mechanical, behavioral}. Five cells are used:

| cell | is |
|---|---|
| (baseline, baseline)   | the baseline run |
| (reform, baseline)     | static |
| (reform, mechanical)   | the new pass |
| (baseline, mechanical) | priced only for MTRs, inside the mechanical pass |
| (reform, behavioral)   | conventional |

(baseline, behavioral) is empty: behavior is a response to the reform.

## Decisions

1. Employer payroll wage adjustment moves from static to mechanical. The
   mechanism is unchanged: total employer cost held fixed, wages absorb 85
   percent of the employer tax change and untaxed fringe the rest, first
   order, in do_taxes. The only change is which passes receive the baseline
   employer payroll table -- static gets NULL, mechanical and conventional-side
   passes get the table.

2. Behavior modules price against the mechanical frame. The MTR delta feeding
   an elasticity is MTR(reform law) - MTR(baseline law), both evaluated on the
   mechanical-frame records: the price change from the law, at the economic
   circumstances the taxpayer has after everything outside his control has
   happened to him. This replaces the current convention, where both MTRs are
   evaluated on baseline-frame records. The two differ only for records that a
   mechanical income change pushes across a bracket, so the mechanical pass
   emits a crossing diagnostic (count and weight of records whose marginal
   rate differs between frames). The kg eta is re-pinned after the switch.

3. Distribution tables keep their current construction: built from the static
   frame, with per-channel overlays added on top. The corporate overlay is
   unchanged -- the revenue delta distributed by the existing rules, labor
   share included. A new payroll overlay carries, per record, the liability
   difference between the mechanical and static frames, which for a payroll
   reform is the income tax offset from that worker's own wage adjustment;
   this reproduces today's table values exactly, with the offset visible as a
   labeled column instead of baked into the frame. Decumulation gets no
   overlay: the later-year income dip is the echo of taxes already counted as
   burden in the years they were paid, and counting it again double-counts in
   present value. Behavioral income changes are excluded as always.

   The burden identity for employer payroll: with total employer cost fixed,
   the worker's compensation loss equals the employer tax change, so
   attributing the employer tax to the worker's record and measuring his
   income loss net of tax offsets are the same number, provided the income
   tax change is computed on adjusted wages. The overlay supplies that term.

4. The wealth drawdown forcing includes the wage adjustment as external
   income. The forcing is extra taxes less external income changes; the
   corporate dividend cut already enters as external income and the payroll
   wage cut now does too, accumulated into the same detail column the
   corporate channel writes. This makes the two channels consistent
   everywhere, not only in the distribution tables.

5. Passes are skipped when they cannot differ from the pass below. The
   mechanical pass runs only if a transmission channel is live: corporate
   incidence active, a wealth-financing profile with a nonzero saving share,
   or a reform that changes employer-side payroll parameters (a new
   parse-time predicate). The conventional pass runs only if the behavior
   stack is non-empty. A skipped pass records its rung as equal to the rung
   below. A plain tax law reform with no behavior runs one counterfactual
   pass, as today.

## Architecture

run_one_year gains a pass type, mechanical: corporate applier, wealth
haircut, payroll adjustment, do_taxes, no behavior modules. Output lands in
{scenario}/mechanical/ with detail, totals and receipts alongside static and
conventional.

The wealth channel inside the mechanical pass needs its own forcing
measurement, mirroring the existing conventional-side pattern: a
mechanical-no-wealth pass (corporate on, payroll adjustment on, haircut off,
behavior off) to measure the tax delta, and a mechanical wealth pre-pass to
build the drawdown state. Both are copies of the 2N/2W pattern.

The MTR block for behavior runs on the mechanical pass output, twice: once
under reform parameters and once under baseline parameters. The conventional
pass consumes those two MTR frames in place of the current baseline/static
pair.

Per-scenario-year DAG, full case (kg + wealth + corporate):

  static -> mech-no-wealth -> mech wealth pre-pass -> mechanical (+ MTR
  blocks) -> kg bathtub -> conv-no-wealth -> conv wealth pre-pass ->
  conventional

SLURM: three new phases (mechanical-no-wealth, mechanical wealth pre-pass,
mechanical), gated by the same skip predicates. Touches the usual set from
the sync table in CLAUDE.md: worker.R dispatch, setup.R manifests and phase
counts, slurm_run.sh phases and dependencies, aggregate.R totals and receipts
for the new pass, stacked reports gaining a mechanical leg.

Config layer: transmission channels become readable on the mechanical and
conventional passes; reading one on the static pass remains an error.

Compute cost: the mechanical pass needs no behavior and, beyond the two MTR
blocks that move here from the static pass, no additional MTR work. Roughly
one lean calculator pass per scenario-year for corporate-only scenarios, two
to three plus the pre-pass for wealth scenarios. Scenarios with no live
transmission channel pay nothing.

## Build order

1. The payroll gate, as its own commit. Regression at full sample: all
   outputs byte-identical except payroll-touching reforms, which is itself
   the test that the gate did only what it claims.
2. The mechanical pass for corporate-only scenarios (no state, pure
   sequencing), with the skip predicates and the payroll parse-time
   predicate.
3. The mechanical-side wealth forcing mirror, and the wage adjustment added
   to the forcing's external-income column.
4. The MTR relocation and the bracket-crossing diagnostic; kg eta re-pin.
5. SLURM phases, aggregation, stacked reports, distribution payroll overlay.
6. Regression against the smoke-diff harness; re-pin any invalidated
   calibration hashes only after verifying identical output on non-payroll,
   non-transmission scenarios.


## As built

Six commits on `mechanical-pass`, off `wealth`. Regression record:
other/mechanical_pass/regression_notes.md.

Departures from the plan, both places where the plan's recipe would not have given
what it described:

1. The plan had a skipped mechanical rung write nothing, with the reporting layer
   falling back to the static rung. Its totals and receipts are written instead,
   from the static totals where the rung did not run, so every counterfactual
   carries all three rungs and no report needs a special case. Detail is still not
   copied: it would be a large file carrying no information, so the products built
   from detail guard on its presence.

2. The plan read the payroll overlay as the difference between the mechanical and
   static rungs. That difference carries every channel live on the rung, not the
   wage adjustment alone, so it would have included the wealth drawdown the same
   plan excludes. The overlay is read off the rung's no-wealth tree where the
   wealth channel is on. Corporate incidence cannot be separated this way at all,
   and the corporate overlay already carries it, so a scenario running both
   channels gets no payroll overlay and a warning.

Baseline law is the runscript's baseline row, not the default layer. Both the
employer payroll predicate and the mechanical rung's second set of marginal rates
first read the default layer, which is the same thing only where the baseline row
names it. A retrospective run scores against prior law by naming an alternative
there, so the rates handed to the behavior modules would have been differenced
against the wrong law. Both sites read baseline_tax_law_id(); the predicate
compares effective values, each side's own entry where it has one and the
default's otherwise, because both laws are sparse deltas over that layer.

Two things found along the way. The wage rescale ended by setting wages to the sum
of the two earners' wages, which is not bit-identical to the column Tax-Data
supplies, and it ran on the reform static pass but not the baseline one; taking it
off the static pass removes that disagreement. And
other/performance/test_slurm_dependency_graph.sh had been passing the retired
user_id argument since 2026-07-25, tripping the arity guard so that every case
exited 1 unnoticed.

## Outstanding

The gains elasticity has not been re-derived under the new marginal rate
convention. Both eta entries and both timeable_share entries in
config/calibrations/kg/bathtub.yaml are pinned to src/sim/kg/inputs.R as it was
before the change, so they report stale and any kg run stops until they are
re-derived. That is the intended state: the numbers were measured with the
reform-side cell rates read off the static rung, and they are now read off the
mechanical rung.

Re-deriving means three full-sample thirty-year vintages per form and then the
inversion, through the entry's own `rederive` script -- for the shipped form,

    other/kg_model_tests/form_ab/measure_efull_logs.R

with the sweep launched by its launcher first. Until that is done the branch
should not merge: kg results on it are not comparable with anything.

The plan expected the drift to be small, on the reasoning that the two
conventions part company only for records a mechanical income change moves across
a bracket. The mechanical rung writes mtr_crossing_diag_{year}.csv, which counts
and weights those records per MTR variable and reports the mean rate change each
convention hands a behavior module, so the expectation is checkable before the
sweep is paid for. On a five point gains increase with the wealth channel on it
holds: a quarter of a percent of weight sits on a moved record, and the mean rate
change moves by under one percent of itself. Numbers in
other/mechanical_pass/regression_notes.md.
