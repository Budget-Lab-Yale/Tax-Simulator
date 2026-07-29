# Regression record

Full-sample SLURM smoke runs of `config/runscripts/tests/mech_smoke.csv`, six
scenarios over 2025:2027, comparing the branch point against the branch.

## Commits 1 and 2 (pass specification, payroll gate)

Reference `mech_pre` at c354662ab, test `mech_post` at 5fc7549f1. Compared with
`other/simplify_cleanup/compare_smoke.sh`.

Scenarios with no employer payroll change -- `sd_bump_plain`, `sd_bump_wealth`,
`ee_hi_1pp` -- agree on every aggregate to four decimals in billions. Their
detail files differ at 1.49e-08 per record, in `wages` and the columns
downstream of it.

The cause is the wage rescale's last line. It set

    wages = wages1 + wages2

which is not bit-identical to the `wages` column Tax-Data supplies, and it ran on
the reform static pass while the baseline static pass, which is handed no
employer payroll table, kept the supplied column. The two static legs therefore
disagreed on wages by that amount. With the rescale off the static pass, both
legs keep the supplied column and the disagreement is gone.

Employer payroll fixtures move in the expected direction, 2025 levels in $B:

| scenario | static payroll, before | after | static income tax, before | after |
|---|---|---|---|---|
| `er_oasi_1pp`    | 1715.8 | 1726.2 | 2309.9 | 2324.5 |
| `er_oasi_taxmax` | 1647.6 | 1648.5 | 2319.8 | 2324.6 |
| `ee_hi_1pp`      | 1745.9 | 1745.9 | 2325.4 | 2325.4 |

Payroll revenue rises because the base is no longer shaved before it is taxed,
and income tax rises because the offset from the wage cut is no longer in the
static frame. The employee-side control does not move, which is the test that the
predicate reads the employer side alone.

`er_oasi_1pp` still shows a small static income tax response to the reform, from
the deduction for half of self-employment tax: the employer OASDI rate enters the
self-employment rate, so the reform changes an above-the-line deduction. That is
law arithmetic and belongs on the static rung.

## Commits 3 to 5 (the mechanical rung, SLURM, reporting)

Run `mech_c5` at 34d4bf88b, same runscript and sample. Phase gating came out as
intended: the mechanical rung ran for three of the five counterfactuals, and its
drawdown forcing for the two with the wealth channel on.

Income tax by rung, $B, 2025 to 2027:

| scenario | static | mechanical | conventional |
|---|---|---|---|
| `sd_bump_plain`  | 2325.42 2246.95 2339.20 | same | same |
| `sd_bump_wealth` | 2325.42 2246.95 2339.20 | 2325.42 2247.45 2340.14 | same |
| `er_oasi_1pp`    | 2324.47 2464.59 2565.89 | 2309.94 2449.45 2549.97 | same |
| `er_oasi_taxmax` | 2324.58 2464.70 2566.00 | 2319.68 2459.47 2560.42 | same |
| `ee_hi_1pp`      | 2325.42 2465.61 2566.97 | same | same |

`er_oasi_1pp`'s mechanical rung reproduces the pre-gate static level of 2309.94
to the reported precision. That rung is the static one plus the wage adjustment
and nothing else for this scenario, so it should, and the payroll relocation is
a relocation rather than a change.

A scenario with no transmission channel has all three rungs equal. A scenario with
the wealth channel and a tax cut shows a higher mechanical income tax than static:
the forcing is negative, so wealth accumulates above baseline and carries more
capital income.

The conventional rung equals the mechanical one throughout, this runscript binding
no behavior modules.

### The payroll overlay restores the offset

Comparing each scenario's `static/supplemental/distribution.csv` against the
pre-gate table on their twenty-seven shared columns, `net_change_pr_overlay`
being new:

| scenario | shared cells beyond tolerance | worst absolute difference | sum of absolute overlay, $B |
|---|---|---|---|
| `er_oasi_1pp`    | 0 | 0 | 582.7 |
| `er_oasi_taxmax` | 0 | 0 | 192.6 |
| `ee_hi_1pp`      | 0 | 2.2e-16 | 0 |
| `sd_bump_plain`  | 0 | 0 | 0 |
| `sd_bump_wealth` | 0 | 0 | 0 |

The payroll fixtures reproduce their pre-gate tables while carrying a nonzero
overlay, so the overlay restores exactly what the gate took out of the frame. The
scenarios that change no employer payroll parameter carry no overlay.

`er_oasi_taxmax` reproduces its pre-gate table although the wealth channel is on,
because the overlay is read off the no-wealth tree and so excludes the drawdown.

## Commit 6 (the marginal rate convention)

Run `mech_kg` at 6c43e0dc6, `config/runscripts/tests/mech_kg_smoke.csv`: a five
point gains increase over 2026 to 2029 at full sample, once with the wealth
channel off and once on, both binding the gains bathtub. The gains recurrence
completed on both, which exercises each source of its reform-side cell rates: the
static rung for the scenario with no transmission channel live, the mechanical
rung for the other.

The crossing diagnostic, from the scenario whose mechanical rung ran:

| year | mtr_var | records moved | share of weight | mean rate change, mechanical | mean rate change, static |
|---|---|---|---|---|---|
| 2026 | `mtr_kg_lt`  | 6495 | 0.22% | 0.030671 | 0.030939 |
| 2027 | `mtr_kg_lt`  | 6571 | 0.25% | 0.031039 | 0.030973 |
| 2028 | `mtr_kg_lt`  | 6571 | 0.24% | 0.031178 | 0.031380 |
| 2029 | `mtr_kg_lt`  | 6568 | 0.25% | 0.032026 | 0.032074 |
| 2026 | `mtr_estate` | 11092 | 0.10% | 0 | -2.3e-08 |

A quarter of a percent of weight sits on a record whose marginal gains rate the
mechanical income change moves, and the rate change handed to a behavior module
moves by under one percent of itself in every year, in both directions. The
elasticity is close to linear in that rate change, so the re-derived eta should
move by about as much.

The estate rows are a consistency check rather than a finding: this reform changes
no estate parameter, so the reform-law and baseline-law estate rates agree on
every record and the mechanical mean change is exactly zero. The static column's
1e-08 is the noise of the two frames.

Revenue over the four years run, in billions, which is the decomposition the
ladder exists to produce:

| scenario | static | mechanical | conventional |
|---|---|---|---|
| `kg_plain`  | 324.46 | 324.46 | 144.70 |
| `kg_wealth` | 324.46 | 320.76 | 144.29 |

With the saving channel off the first two rungs agree, no mechanical channel being
live, and the whole loss is the realization response. With it on, 3.70 of the loss
is tax financed out of wealth draining the later capital income and estate bases,
and the remainder is the realization response. The two used to be reported as one
number.

## Outstanding

The gains elasticity has not been re-derived under the mechanical-frame marginal
rate convention. See other/mechanical_pass/DESIGN.md.
