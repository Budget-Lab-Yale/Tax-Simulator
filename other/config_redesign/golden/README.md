# The reference results, and how to compare against them

Six simulations cover the model's main machinery between them. Nothing runs them
automatically -- there is no hook and no trigger. Somebody launches them and
compares the output to a saved reference. That is the whole mechanism.

| | Runscript | How | Reference tree |
|---|---|---|---|
| S1 | `baseline/baseline` | `main.R`, 5% sample | `golds2_s1` |
| S2 | `rebate_2025` | SLURM, full sample | `golds2_s2` |
| S3 | `tests/multi_module_smoke` | SLURM, full sample | `golds2_s3` |
| S4 | `tests/corp_kgwealth_verify` | SLURM, full sample | `golds2_s4` |
| S6 | `wealth_tax`, scenario `wealth_tax_warren` | SLURM, full sample | `golds2_s6` |
| S7 | `estate_2009` | SLURM, full sample | `golds2_s7` |

Between them these exercise the capital-gains bathtub, the corporate incidence
channel, the wealth bathtub, the estate tax, and a four-family behavior stack.

**S6 must name `wealth_tax_warren` as its scenario_id.** That is how its reference
was produced; running all four wealth-tax scenarios changes the stacked reports and
the comparison fails for a reason that is not a model difference.

The trees live at
`/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/golds2_s*`.

## Running it

```bash
# S1
sbatch --job-name=TAG other/config_redesign/gate_smoke.sbatch  . baseline/baseline TAG 0.05
# the other five
sbatch --job-name=TAG other/config_redesign/gate_verify.sbatch . RUNSCRIPT TAG 1 [SCENARIO_ID]

# then, per scenario
bash other/config_redesign/gate_diff.sh <candidate tree> <golds2_sN tree>
```

## The md5 files here

One per reference tree, covering every file except `.xlsx` (a spreadsheet's zip
carries a timestamp, so it is never byte-identical and `gate_diff.sh` compares those
by unzipped sheet payload instead).

They exist because scratch is not permanent. If the trees are purged, these are the
surviving record of what the model produced at commit `93967a480`, and a re-run can
be checked against them with:

```bash
cd <candidate tree> && md5sum -c /path/to/golds2_sN.md5
```

That is weaker than `gate_diff.sh`, which knows which differences are sanctioned, so
prefer the trees while they exist.

## Why this set replaced the first one

The original references (`golds*`, still on scratch) were taken at the branch point
of the config rebuild and every phase of that rebuild was verified byte-identical
against them. Then two calibrated values were deliberately re-derived on 2026-07-26
and 2026-07-27:

- `sigma.conv` 0.16 -> 0.2002 (re-derived under charity/50, the elasticity product
  runs actually use)
- `bathtub.timeable_share_logs` 0.2542 -> 0.2452 (the value solved onto its target
  rather than hand-iterated into a tolerance band)

No re-run can match the old references again, and a permanent failure is a check
nobody reads. So this set was taken at `93967a480`.

**What was verified before adopting it**, which is the only thing that makes it
trustworthy: every one of the six was compared against its predecessor, and **every
CSV was byte-identical in all six**. The only differences anywhere were

- the two calibrated values above, flagged by `mapping_check.py` -- that check
  noticing them is it working, not failing
- xlsx content in S3 and S4, the two scenarios that read the live timing share, at a
  largest relative difference of 3.4e-05 (S3) and float noise near zero (S4). The
  spreadsheets carry more decimals than the CSVs report, which is why the change
  shows there and nowhere else.

So nothing unexplained came along for the ride. The old references remain on scratch
as the pre-re-derivation record.
