# Estate Tax Calibration Side Script

This directory contains a standalone calibration script for the planned
on-model estate tax module. It does not change simulator tax calculations.

## Usage

```sh
Rscript other/estate_tax/calibrate_estate_tax.R \
  --tax-data-root /path/to/Tax-Data/v1/<vintage>/baseline \
  --soi-file other/estate_tax/estate_tax_filed_2019_2023.csv \
  --output-dir /path/to/output \
  --macro-root /path/to/Macro-Projections/v3/<vintage>/baseline
```

`--score-targets` is optional. If omitted, the script uses
`other/estate_tax/score_targets_estate_gift.csv`.

## Model

The script calibrates a small tournament of reporting and taxable-estate forms:

```text
economic_gross_i = sum estate wealth value columns
m_i = q_death1 for non-joint records; q_death1 * q_death2 for joint records
reported_gross_i = economic_gross_i * r(economic_gross_i; theta_r)
taxable_estate_i = reported_gross_i * t(reported_gross_i; theta_t)
expected_revenue = sum(weight_i * m_i * estate_tax_i)
```

Calendar-year deaths are mapped to next fiscal-year receipts, approximating the
nine-month estate return due date.

## Fixed Assumptions

- CBO/JCT score targets are estate-and-gift targets.
- The script applies a fixed 10% gift-tax haircut once:
  `estate_target = 0.90 * estate_and_gift_target`.
- The baseline/sunset exclusion is $7.2 million for 2026 deaths, indexed after
  2026 when `--macro-root` is provided.
- The OBBBA exclusion is $15 million for 2026 deaths, indexed after 2026 when
  `--macro-root` is provided.
- Historical exclusions are hard-coded for 2018-2025 for SOI filing-year
  calibration and the FY2026 score target.

## Outputs

- `estate_calibration_parameters.csv`: fitted parameter sets by candidate form.
- `estate_calibration_moments.csv`: target vs modeled moments.
- `estate_calibration_pareto.csv`: form comparison across SOI and score fit.
- `estate_calibration_diagnostics.md`: concise human-readable diagnostics.
