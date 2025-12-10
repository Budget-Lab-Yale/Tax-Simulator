# Tax-Simulator - Budget Lab at Yale

## Project Overview

Tax-Simulator is a policy microsimulation model for analyzing the budgetary, distributional, and economic impacts of tax policy changes. It simulates individual tax units and calculates tax liability under different policy scenarios, then aggregates results for revenue and distributional projections.

**Key Capabilities:**
- Calculate tax liability for individual tax units under current law and policy reforms
- Generate static and conventional revenue estimates (latter means with behavioral feedback)
- Produce detailed distribution tables showing policy impacts across income groups
- Support scenario analysis for policy changes, economic projections, and behavioral assumptions

**Model Components:**
1. **Policy Calculator**: Deterministic function representing tax law (inputs: individual characteristics → output: tax liability)
2. **Population Simulator**: Projects population and behavioral responses to policy changes

## Project Structure

```
Tax-Simulator/
├── config/
│   ├── interfaces/
│   │   ├── interface_versions.yaml  # Dependent model versions
│   │   └── output_roots.yaml        # Output path configuration
│   ├── runscripts/                  # Simulation configuration CSV files
│   └── scenarios/
│       ├── behavior/                # Behavioral feedback modules (.R files)
│       └── tax_law/                 # Tax law configuration directories
│           ├── baseline/            # Current law baseline tax parameters
│           ├── baseline_2024/       # 2024 baseline tax parameters
│           ├── public/              # Public policy reform scenarios
│           ├── private/             # Internal/private policy scenarios
│           └── tests/               # Test scenarios
├── src/
│   ├── sim/
│   │   └── behavior.R               # Behavioral helper functions (e.g., apply_mtr_elasticity)
│   └── misc/
│       └── config_parser.R          # Runscript parser (parse_globals, get_scenario_info)
└── other/                           # Utility scripts and analysis tools
```

## Core Concepts

### Runscripts (Simulation Configuration)

Runscripts are CSV files that define simulation parameters. Think of them as "recipes" for running the model.

**Required Columns:**
- `ID`: Scenario identifier (e.g., "TCJA_full_extension"). Reserved word: "baseline"
- `tax_law`: Path to tax law YAML directory (relative to `config/scenarios/tax_law/`)
- `behavior`: Path to behavioral feedback module (relative to `config/scenarios/behavior/`, omit `.R` extension)
- `years`: Simulation years in format `{start_year}:{end_year}` (e.g., "2024:2034")
- `dist_years`: Years for full distribution table calculation (computationally intensive)
- `mtr_vars`: Space-delimited list of variables for marginal tax rate calculation (e.g., "wages kg_lt tips1")
- `mtr_types`: Space-delimited types matching `mtr_vars`. Options: "nextdollar" (or "nextdollar:X" for arbitrary dollar amount, e.g., "nextdollar:1000"), "pct:X" (percent change, e.g., "pct:10"), or "extensive"

Note: if you want to run a policy change starting in t, always start the simulation via `years` earlier -- at least t - 1. 

**Optional Columns:**
- `dep.{MODEL_NAME}.vintage`: Override dependent model vintage
- `dep.{MODEL_NAME}.ID`: Override dependent model scenario ID
- Custom columns can be added if you modify `get_scenario_info()` in `src/misc/config_parser.R`

**Runtime Parameters (command-line arguments, not in runscript CSV):**

You need to supply these cmd line args when running from bash:

- `runscript_names`: path of runscript, relative to ./config/runscripts, include the name but exclude the ".csv" 
- `scenario_id`: name of a single scenario to run; if NULL, all scenarios in runscript execute
- `user_id`: User identifier for tracking runs (string, typically use "user_test" for local runs)
- `local`: whether to write to local (1) or production (0). Do local unless prompt specifies otherwise
- `vintage`: specific folder name to write to; if NULL, uses timestamp. Do NULL unless prompt specifies otherwise
- `pct_sample` = fraction of records to use in simulation, floating point number b/n 0 and 1. Do 1 unless prompt specifies otherwise
- `stacked` = whether to produced stacked revenue estimates. Do 1 unless prompt specifies otherwise
- `baseline_vintage` = specific folder where baseline runs live, used for situations when the runscript doesn't specify a baseline; if NULL, uses current timestamp and re-runs baseline and writes there. Do NULL unless prompt specifies otherwise
- `delete_detail` = whether to delete large tax unit detail files. Do 0 unless prompt specifies otherwise
- `multicore`: Dimension across which to parallelize execution. Three options:
  - `'none'`: No parallelization (default, safest option, required if on windows)
  - `'scenario'`: Parallelize across scenarios (good when running multiple scenarios)
  - `'year'`: Parallelize across years (generally fastest when running single scenario across many years)

  Examples:

  # Local run with 10% sample
  Rscript src/main.R private/my_scenario NULL user_test 1 NULL 0.1 1 NULL 0 none

  # Production run with full sample
  Rscript src/main.R public/my_scenario scenario_id user_test 0 vintage_name 1 1 baseline_vintage 1 none

  **Important Notes:**
  - **Windows limitation**: Multicore parallelization is not supported on Windows due to R's parallel processing limitations. Always use `'none'` on Windows.
  - Some behavioral feedback modules require sequential year calculation. Using `'year'` with these modules will cause race conditions and incorrect results. Review module requirements before enabling year-level parallelization.
  - Generally choose the dimension with the largest N for best performance (when safe to do so).

**Example:**
```csv
ID,tax_law,behavior,years,dist_years,mtr_vars,mtr_types
baseline,baseline,,2024:2034,,tips1 tips2,nextdollar nextdollar
income_tax,public/tips/income_tax,,2024:2034,2025:2026,tips1 tips2,nextdollar nextdollar
```

### Tax Law Configuration

Tax law is represented as collections of **tax parameters** (thematically related provisions) stored in individual YAML files. Each parameter contains **subparameters** with:
1. Time series of values (year-value pairs for policy changes)
2. Optional inflation indexation rules (measure, base year, rounding step, direction)

**Examples of Tax Law Files in `config/scenarios/tax_law/baseline/`:**
- `agi.yaml`: Adjusted Gross Income parameters
- `below.yaml`: Below-the-line deductions and adjustments (tips deduction, overtime deduction, senior deduction)
- `amt.yaml`: Alternative Minimum Tax
- `ctc.yaml`: Child Tax Credit parameters
- `eitc.yaml`: Earned Income Tax Credit
- `ord.yaml`: Ordinary income tax (rates, brackets)
- `pref.yaml`: Preferred rate income (capital gains, dividends)
- `qbi.yaml`: Qualified Business Income (Section 199A)
- `pr.yaml`: Payroll tax parameters
- `std.yaml`: Standard Deduction
- `item.yaml`: Itemized Deductions
- `char.yaml`: Charitable Deduction policy
- `cdctc.yaml`: Child and Dependent Care Tax Credit
- `niit.yaml`: Net Investment Income Tax
- `pe.yaml`: Personal exemptions
- `rebate.yaml`: Generic per-person refundable credit (for stimulus, UBI modeling)

**YAML Structure:**
```yaml
# Optional: default indexation for all subparameters
indexation_defaults:
  i_measure: CPI-U
  i_base_year: 2023
  i_direction: up
  
# Optional: filing status mapper for aggregating subparameters
filing_status_mapper:
  parameter_name:
    '1': value_single
    '2': value_married
    '3': value_married / 2    # Married filing separately
    '4': value_head           # Head of household

# Subparameter example
parameter_name:
  value:
    '2018': [value1, value2, ...]
    '2026': [value1, value2, ...]
  i_measure: default  # or CPI-U, CPI-U-RS, etc.
  i_base_year: default
  i_direction: default  # up, down, nearest
  i_increment: [25, 50, 100]  # rounding steps per bracket
```

### Behavioral Feedback Modules

Behavioral modules are R scripts that simulate taxpayer responses to policy changes. They enable conventional and partial dynamic revenue estimates.

**Module Organization:**
- Location: `config/scenarios/behavior/`
- Organized in subfolders by behavior type (e.g., `/charity`, `/employment`)
- Module filename describes what it does (e.g., `100.R` = elasticity of -1.0)

**Module Structure:**
Every module must contain a function named `do_{subfolder_name}()`:

```r
do_behavior_name = function(tax_units, ...) {
  #----------------------------------------------------------------------------
  # [Required formatted documentation]
  # Brief description of what the module does
  # 
  # Parameters: 
  #   - tax_units (df)     : tibble of tax units with calculated variables
  #   - baseline_mtrs (df) : year-id indexed tibble of MTRs under baseline
  #   - static_mtrs (df)   : year-id indexed tibble of MTRs under static
  #                          counterfactual scenario
  #
  # Returns: tibble of tax units with adjusted values
  #----------------------------------------------------------------------------
  
  # Module logic here
  
  # Must return full tax_units dataframe with modified variables
  return(tax_units)
}
```

**Available Arguments:**
- `tax_units`: Dataframe of all tax records and attributes (marital status, income components, tax parameters, etc.)
- `baseline_mtrs`: Dataframe with baseline marginal tax rates for variables specified in runscript
- `static_mtrs`: Dataframe with MTRs under static policy simulation
- Both MTR dataframes are `NULL` if no MTRs requested in runscript

**Helper Function: `apply_mtr_elasticity()`**
Located in `src/sim/behavior.R`. Applies elasticity to variable based on MTR changes.

**Usage:**
```r
tax_units %>%
  mutate(
    e_{varname} = elasticity_value,
    e_{varname}_type = 'taxprice'  # or 'semi', 'arc', 'netoftax'
  ) %>%
  apply_mtr_elasticity('{varname}', baseline_mtrs, static_mtrs, max_adj = 1)
```

**Elasticity Types:**
- `"semi"`: Log-lin semi-elasticity (% change in Y for percentage point change in EMTR)
- `"arc"`: Log-log elasticity at midpoint (% change in Y for % change in EMTR)
- `"netoftax"`: Log-log elasticity on (1 - EMTR)
- `"taxprice"`: Log-log elasticity on (1 + EMTR)

**Calculation formulas:**
```r
pct_chg = case_when(
  e_type == "semi"     ~ exp((mtr - mtr_baseline) * e) - 1,
  e_type == "arc"      ~ (e * (mtr / ((mtr + mtr_baseline) / 2) - 1)),
  e_type == "netoftax" ~ (e * ((1 - mtr) / (1 - mtr_baseline) - 1)),
  e_type == "taxprice" ~ (e * ((1 + mtr) / (1 + mtr_baseline) - 1))
)
```

## Model Execution Flow

1. **Static Mode** (for non-baseline scenarios):
   - Input attributes held fixed at baseline levels
   - Calculate taxes under policy reform
   - Compute effective marginal tax rates (EMTRs) under new policy

2. **Non-Static Mode** (with behavioral feedback):
   - Execute behavioral feedback modules at start
   - Modules modify input attributes based on logic/elasticities
   - Re-calculate taxes with adjusted attributes
   - Generate final estimates with behavioral feedback

## Output files

There is lots of post-processing for each scenario out of the box. If the user is
asking you a question about a specific variable, it likely exists already in the output 
folders. Files and key things to know:
  - within the interface root, you will see all scenario folders
  - within a scenario folder, you will see two options: static and conventional
  - within each of those run-types, you will see:
    - detail: individual tax unit detail files by year
    - total: level aggregations of the detail files
    - supplemental: other files including revenue estimates (deltas), distribution (exists only for static because distribution is a static concept), etc
  - note that baseline lacks the deltas-related files because deltas are relative to baseline

## Coding Conventions

### Variable Naming
- Primary earner variables: suffix `1` (e.g., `wages1`, `male1`)
- Secondary earner variables: suffix `2` (e.g., `wages2`)
- MTR variables: prefix `mtr_` (e.g., `mtr_wages`, `mtr_char_cash`)
- Elasticity variables: prefix `e_` (e.g., `e_char_cash`, `e_char_cash_type`)

### Tax Unit Variables
Common variables in `tax_units` dataframe:
- **Demographics**: `filing_status`, `male1`, `male2`, `n_dep_ctc`, `age1`, `age2`
- **Income**: `wages`, `wages1`, `wages2`, `txbl_int`, `div_ord`, `div_pref`, `kg_lt`, `kg_st`
- **Business Income**: `sole_prop`, `part_active`, `part_passive`, `scorp`, `rent`, `farm`
- **Other**: `ui`, `gross_ss`, `txbl_ira_dist`, `txbl_pens_dist`, `other_inc`
- **Deductions**: `char_cash`, itemized deduction components
- **Tax Parameters**: Access subparameters via `{parameter}.{subparameter}` (e.g., `eitc.po_thresh_1`)

### Module Requirements
- **Documentation**: Use formatted comment blocks with Parameters and Returns sections
- **Random seed**: Call `set.seed(globals$random_seed)` before any RNG usage
- **Return value**: Always return full `tax_units` dataframe, not just modified columns

## Policy Reform Workflow

### Creating a Reform

1. **Identify parameters to change**: Determine which tax law files need modification
2. **Create reform directory**: `config/scenarios/{reform_name}/`
3. **Copy and modify YAML files**: Only include parameters that differ from baseline
4. **Important**: Reforms OVERWRITE baseline, they don't add to it
   - Include all relevant years, not just the change year
   - Missing years will have no values for those parameters

**Example - Extending TCJA:**
```yaml
# In config/scenarios/tcja_extension/ord.yaml
rates:
  value:
    '2014': [0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
    '2018': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
    # TCJA rates continue (no 2026 expiration)

brackets_single:
  value:
    '2014': [0, 7000, 22100, 53500, 115000, 250000, 400000]
    '2018': [0, 9525, 38700, 82500, 157500, 200000, 500000]
    # TCJA brackets continue
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: [25, 25, 25, 25, 25, 25, 25]
```

**Common Mistakes:**
```yaml
# WRONG - This overwrites 2014-2026 and leaves no rates before 2030
rates:
  value:
    '2030': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
```

### Adding New Tax Law Features

1. Create YAML file in `config/scenarios/tax_law/baseline/` with new parameters
2. For reforms that only add this feature: set baseline values to "off" (e.g., credit value = 0, flag = false)
3. Create reform YAML files with "on" values
4. Modify calculator code if needed to incorporate new parameter logic

## Common Tasks

### Running a Simulation
1. Create or modify runscript CSV with scenario parameters
2. Ensure tax law YAML files exist for each scenario's `tax_law` value
3. If using behavioral feedback, ensure module exists at specified `behavior` path
4. Execute model with runscript as input

### Creating a Behavioral Module
1. Choose appropriate subfolder in `config/scenarios/behavior/` (or create new)
2. Create `.R` file with descriptive name
3. Implement `do_{subfolder_name}()` function with required signature
4. Add formatted documentation
5. Reference module in runscript's `behavior` column as `{subfolder}/{filename}`

### Debugging Tax Calculations
- Check relevant YAML files for parameter values in simulation years
- Verify filing status mappers are correctly specified
- Ensure indexation rules are appropriate
- Review intermediate calculation variables in tax_units

### Analyzing MTR-based Behavioral Responses
1. Add variables to `mtr_vars` in runscript (space-delimited) -- note that when a variable is indexed by 1 and 2, 
   that is, it is associated with primary and secondary earnings, an individual level variable, typical of labor earnings and its
   components, then you need to do both, for example tips1 and tips1 or ot1 and ot1 or wages1 and wages2
2. Specify types in `mtr_types` ("nextdollar", "nextdollar:X", "pct:X", or "extensive")
3. Access MTRs in behavioral module via `baseline_mtrs$mtr_{varname}` and `static_mtrs$mtr_{varname}`
4. Use `apply_mtr_elasticity()` for standard elasticity applications

## Notes and Best Practices

- **Runscripts are recipes**: They coordinate all model inputs in one place
- **YAML reforms overwrite baseline**: Always include full time series for modified parameters
- **Behavioral modules are flexible**: Don't limit yourself to elasticity functions—implement any logic
- **MTR calculation is powerful**: Microsimulation computes precise EMTRs for any variable via $1 marginal adjustments
- **Documentation is required**: All behavioral modules must have formatted comment blocks
- **Heterogeneous elasticities**: Assign different elasticities to different demographic groups/income levels using `case_when()`
- **Extensive margin simulation**: Use probabilities and RNG for binary outcomes (e.g., employment exit)
- **Global random seed**: Always reset with `set.seed(globals$random_seed)` before stochastic operations
