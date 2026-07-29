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
