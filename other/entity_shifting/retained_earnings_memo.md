# Retained corporate earnings and the stock of unrealized gains

2026-07-30. Written to be implemented while the top-tax report is out for review.
Every measured figure here predates the income-conversion fix of the same day and
is marked for refresh; the refresh table at the end says which numbers to replace
and from where.

## The defect

When a reform makes the corporate form cheaper, entity shifting moves business
income out of the pass-through base and into the corporate base. The corporate
layer is charged, and what remains splits two ways: a share paid out currently,
and a share retained. Retained earnings raise share values, which is to say they
are new unrealized capital gains.

The income conversion channel treats its own equivalent of this correctly. Salary
repackaged as equity appreciation enters the stock of unrealized gains, compounds,
is realized over later years at each holder's own rate, sits in the estate at
death, and meets whatever the death regime is.

Entity shifting does not put its retained earnings in that stock. It prices them
instead. The price is `tau_eq`, the expected present-value tax on a dollar newly
entering the unrealized-gain state, which the gains pre-pass computes by age, year
and death regime. The retained dollars are then folded into the current year's
realized gains through a scaled adjustment, at a factor chosen so the tax
collected matches that present value.

## What the price does and does not carry

Stating this precisely matters, because the channel is better than "the deferral is
ignored" and worse than "the deferral is modelled".

Carried:

- The tax on the retained dollars, at an expected present value.
- The death regime. `tau_eq` is computed separately under baseline and under the
  scenario, as `tau_eq_B` and `tau_eq_S`, so moving from step-up to carryover or
  deemed realization does change what these dollars are priced at.
- Age and year variation in that price, at cell resolution.

Not carried:

- The dollars themselves. The stock of unrealized gains is the same size whether
  or not a reform shifted income into corporate form, so those gains never
  influence any other holder's realization decision.
- The estate tax base. Gains that are not in the stock are not in anyone's estate,
  so the estate tax and the death-treatment comparison are both measured on a base
  that omits them.
- Compounding. A dollar retained in 2030 does not grow in the stock through 2057.
- Timing within the reporting window. The tax arrives in the shifting year rather
  than spread across the years the gain would have been realized in. The present
  value is right and the profile is not, which matters for a ten-year score.
- Its own feedback. See below.

## The circularity

`tau_eq` prices a dollar entering the stock, and it is computed from the stock: its
composition, its realization rates, its mortality, its death regime. The stock it
is computed from does not contain the retained dollars. So the price of adding a
dollar is derived under the assumption that no dollars were added.

Adding them would raise the stock, change the cell composition, change realization
rates and move `tau_eq` itself. Nothing feeds back.

The important consequence is not the size of that feedback but that the model as
built cannot measure it. There is no configuration in which the dollars are in the
stock, so "the effect is second order" is an assumption rather than a finding. The
first task below is what turns it into a finding.

## Size, on pre-fix numbers

Measured from the 2026-07-30 grid run, phase 2N logs, over 13,794 scenario-years:

| quantity | pre-fix value | refresh |
|---|---|---|
| retained earnings not entering the stock, mean | $7.4B a year | [v6: ___] |
| the same, heavy packages (q02, q08, q15) | $20B to $30B a year | [v6: ___] |
| the same, worst single scenario-year | $157B | [v6: ___] |
| baseline stock of unrealized gains, 2035 | $96.8T | [v6: ___] |
| baseline realizations, 2035 | $1.6T | [v6: ___] |
| cumulative omission over 31 years, typical | about $230B | [v6: ___] |
| cumulative omission over 31 years, heavy packages | $700B to $900B | [v6: ___] |

Against a stock near $97T the cumulative omission is a quarter of one percent,
reaching about eight tenths of a percent in the heaviest packages. In aggregate
that is small.

It is not spread evenly. Retained corporate earnings accrue to the shareholders of
C corporations, which is the top of the wealth distribution, which is the
population whose estates and whose death treatment the report is about. A quarter
of a percent distributed across the whole stock would be immaterial; the same
quarter of a percent sitting in the cells that carry the result is not the same
thing. The omission also runs one way: the estate base is understated, so every
comparison of step-up against deemed realization is measured on a base missing the
gains this channel creates.

## Implementation

The work turns entity shifting from a reader of the stock into a writer of it.
That is the whole cost: a reader can run inside the year pass, because the stock is
already solved by then, while a writer has to be known before the stock is solved
at all.

Follow the shape the conversion channel now uses, which exists precisely because
computing the same quantity in two places went wrong once already.

1. **Compute the shift in the pre-pass.** A new context alongside `sigma_ctx` in
   `run_bathtub_pass()`, taking the same inputs entity shifting takes today: the
   pass-through legs from Tax-Data, the corporate rate by year, and the two
   marginal rate frames the mechanical rung publishes. It produces, per record and
   per year, the amount shifted, the distributed share and the retained share.

2. **Inject the retained share into the recurrence.** `kg_dyn_step_recurrence()`
   already takes a `conv_inflow_vec` argument and adds it at year end. The
   retained earnings are a second inflow of the same kind and should arrive the
   same way, either summed into that vector or as a companion argument if the two
   need to be told apart in the diagnostics.

3. **Have the module apply what the pre-pass wrote.** The per-record amounts
   travel in the year's state file, as `conv_records` now does, and
   `do_entity_shifting()` reads and applies them rather than recomputing. The
   dollars entering the stock and the dollars leaving the pass-through legs are
   then the same dollars by construction, and no tolerance is required to say so.
   Clamp each leg against the live frame and report the clamped share, since the
   wealth drawdown erodes the pass-through legs at the head of the pass.

4. **Retire the price.** With the dollars in the stock, `tau_eq` is no longer the
   retained leg's tax. It stays in the behavioral wedge, which is a different
   thing: what the shifter expects to pay is still what drives the decision to
   shift. Read the wedge and the booking separately and do not let one stand in
   for the other.

5. **Remove the current-year realized-gain adjustment for the retained share.**
   The `retained_equiv_factor` route exists only to collect the deferred tax
   immediately. Once the dollars defer properly, keeping it would tax them twice.
   The distributed share keeps its own adjustment.

Both execution paths need it: `run.R` passes the context through on one machine,
and `src/slurm/bathtub.R` assembles it on the cluster, exactly as it now does for
the mechanical rung's rate frames. No new phase and no change to the dependency
graph, because the pre-pass already runs after the mechanical rung.

## What it costs in calibration

Two calibrated values are derived under the current arrangement and will move.

- `eta_logs`, the gains elasticity, is measured on a full-simulation realization
  response. A larger stock realizes more, so the elasticity that hits the target
  moment changes. Re-derive through
  `other/kg_model_tests/form_ab/measure_efull_logs.R`.
- `conv`, the income conversion response, is a residual: entity shifting and
  evasion supply most of the top-income elasticity target and `conv` closes the
  gap. Changing what entity shifting delivers changes the gap directly. Re-derive
  through `other/top_tax/sigma_calib/measure_sigma.R`.

Order matters. `conv` is measured given the rest of the stack, so entity shifting
settles first, then `conv`, then `eta_logs` if the realization response has moved
enough to matter. Each is three full-sample vintages.

## The payout share

`alpha = 0.45`, the share of corporate earnings assumed paid out currently rather
than retained, is described in its own comment as a judgment call with no recorded
derivation. It scales the whole retained leg, so it scales this defect and it will
scale the correction. It is the cheapest thing here to firm up, and worth doing
before the rebuild rather than after, so the rebuild is measured against a payout
share someone can defend.

Aggregate payout ratios for the corporate sector, or the retained-earnings share
of after-tax corporate profits from the national accounts, would both serve.

## What to say in the report meanwhile

The report ships before this is built, so the methodology memo should state it
rather than leave it to be found:

Retained corporate earnings arising from entity shifting are priced rather than
tracked. The price is the expected present-value tax on a deferred gain, computed
under each scenario's own death regime, so a change in the treatment of gains at
death is reflected in it. The dollars themselves do not enter the stock of
unrealized gains, and so do not enter the estate tax base. The effect is an
understatement of gains at death concentrated among corporate shareholders, on the
order of a quarter of one percent of the stock of unrealized gains cumulated over
the projection, reaching about eight tenths of a percent under the packages that
combine a large corporate rate change with a large ordinary rate change.

## Verification

- The identity to check is the one the conversion channel now satisfies by
  construction: dollars leaving the pass-through legs equal dollars entering the
  stock, per year, with the clamped share reported separately.
- Run the five packages that combine a corporate rate change with an ordinary rate
  change, since they carry the largest retained leg: q02, q08, q10, q15 and
  t_ord_cg_wealth.
- The estate base is the point of the exercise, so difference `totals/estate.csv`
  against the pre-change vintage on those packages and report the level change,
  not just that the identity holds.
- Confirm the retained leg is not taxed twice by checking that current-year
  realized gains fall by the retained share against the pre-change run.

## Refresh table

Every figure in the size section was measured before the income conversion fix of
2026-07-30, which changed both the frame the conversion wedge is measured on and
the dollars the module applies. The conversion and entity shifting run in sequence
over the same pass-through legs, so entity shifting sees what conversion left
behind and its retained leg moves when conversion moves.

Refresh from the re-run vintage once it exists:

- the three retained-earnings figures, from the phase 2N logs, the
  `do_entity_shifting()` conservation line
- the stock and realizations, from any scenario's
  `conventional/supplemental/kg_dynamics_state/{year}.rds`, summing `G_B` and
  `R_B` over the cell table
- the two cumulative figures, by summing the per-year retained residual over the
  projection rather than multiplying the mean by the horizon, since the residual
  grows with nominal income
