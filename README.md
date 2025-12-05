# Tax-Simulator

A policy microsimulation model for analyzing the budgetary, distributional, and economic impacts of U.S. federal tax policy changes.

Developed by [The Budget Lab at Yale](https://budgetlab.yale.edu/).

## Overview

Tax-Simulator calculates individual income tax liability for a representative sample of U.S. tax units under current law and hypothetical policy reforms. The model produces:

- **Revenue estimates**: Static and conventional (with behavioral feedback) projections
- **Distribution tables**: Policy impacts across income groups and family types
- **Marginal tax rates**: Effective rates on additional income by source

The model operates on microdata representing the U.S. tax-filing population, applying detailed tax law parameters to each record and aggregating results for policy analysis.

## Quick Start

```bash
# Run a simulation (Windows)
Rscript src/main.R public/my_scenario NULL user_test 1 NULL 1 1 NULL 0 none

# Arguments:
#   1. runscript path (relative to config/runscripts/, omit .csv)
#   2. scenario_id (NULL for all scenarios in runscript)
#   3. user_id
#   4. local (1=local, 0=production)
#   5. vintage (NULL for timestamp)
#   6. pct_sample (0-1, fraction of records)
#   7. stacked (1=produce stacked estimates)
#   8. baseline_vintage (NULL to rerun baseline)
#   9. delete_detail (1=delete large files)
#  10. multicore (none/scenario/year - use 'none' on Windows)
```

## Project Structure

```
Tax-Simulator/
├── config/
│   ├── runscripts/          # Simulation configuration files (CSV)
│   └── scenarios/
│       ├── tax_law/         # Tax law parameters (YAML)
│       │   ├── baseline/    # Current law
│       │   ├── public/      # Published reforms
│       │   └── private/     # Internal scenarios
│       └── behavior/        # Behavioral feedback modules (R)
├── src/
│   ├── main.R               # Entry point
│   ├── calc/                # Tax calculation functions
│   ├── sim/                 # Simulation control
│   ├── data/                # Data and post-processing
│   └── misc/                # Utilities
├── other/
│   └── analysis_scripts/    # Policy analysis tools
├── docs/                    # Documentation
└── resources/               # Supporting data files
```

## Configuration

### Runscripts

Runscripts are CSV files that define what to simulate. Each row specifies a scenario:

| Column | Description |
|--------|-------------|
| `ID` | Scenario identifier (use "baseline" for current law) |
| `tax_law` | Path to tax law YAML folder |
| `behavior` | Behavioral feedback module (optional) |
| `years` | Simulation period (e.g., "2024:2034") |
| `dist_years` | Years for distribution tables |
| `mtr_vars` | Variables for marginal rate calculation |
| `mtr_types` | MTR types (nextdollar/extensive) |

### Tax Law

Tax parameters are stored as YAML files organized by provision (income, deductions, credits, etc.). Reforms override specific parameters from the baseline—only changed values need to be specified.

Key parameter files in `baseline/`:
- `ord.yaml` – Ordinary income rates and brackets
- `std.yaml`, `item.yaml` – Standard and itemized deductions
- `ctc.yaml`, `eitc.yaml` – Major credits
- `amt.yaml`, `niit.yaml`, `pr.yaml` – AMT, NIIT, payroll taxes

### Behavioral Feedback

Optional R modules in `config/scenarios/behavior/` model taxpayer responses to policy changes. Modules receive baseline and counterfactual marginal tax rates and adjust input variables accordingly.

## Outputs

Results are organized by scenario and run type:

```
output_root/
└── scenario_name/
    ├── static/              # No behavioral response
    │   ├── detail/          # Tax unit-level records by year
    │   ├── total/           # Aggregated totals
    │   └── supplemental/    # Revenue deltas, distributions
    └── conventional/        # With behavioral feedback
        └── ...
```

## Requirements

- R 4.0+
- Required packages: tidyverse, yaml, data.table, parallel, and others listed in `requirements.txt`

## Documentation

- `CLAUDE.md` – Detailed technical reference
- `docs/website/` – User guide and module documentation

