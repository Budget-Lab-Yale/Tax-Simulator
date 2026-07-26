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

**A runscript names FILES, never values.** The schema is exactly eight columns
and any other column is a parse error whose message names the replacement:

- `ID`: Scenario identifier (e.g., "TCJA_full_extension"). Reserved word: "baseline"
- `tax_law`: `default`, or a path under `config/scenarios/tax_law/alternatives/`
- `economy`: `default`, or a path under `config/scenarios/economy/alternatives/`
- `behavior`: Path to behavioral feedback module (relative to `config/scenarios/behavior/`, omit `.R` extension)
- `years`: Simulation years in format `{start_year}:{end_year}` (e.g., "2024:2034")
- `dist_years`: Years for full distribution table calculation (computationally intensive)
- `mtr_vars`: Space-delimited list of variables for marginal tax rate calculation (e.g., "wages kg_lt tips1")
- `mtr_types`: Space-delimited types matching `mtr_vars` ("nextdollar" or "extensive")

Note: if you want to run a policy change starting in t, always start the simulation via `years` earlier -- at least t - 1. 

Retired columns and where they went: `dep.{MODEL}.vintage` / `dep.{MODEL}.ID`
→ an economy alternative's `interfaces.yaml`; `s` / `wealth_financing` → an
economy alternative's `wealth.yaml` (`financing_profile`, which accepts a
profile folder name, `none`, or the `flat:<s>` shorthand); `assumptions` and
`assumption.{channel}.{name}` → an economy alternative folder;
`excess_growth*` → nothing, the machinery was removed from the model.

**Runtime Parameters (command-line arguments, not in runscript CSV):**

You need to supply these cmd line args when running from bash:

- `runscript_names`: path of runscript, relative to ./config/runscripts, include the name but exclude the ".csv" 
- `scenario_id`: name of a single scenario to run; if NULL, all scenarios in runscript execute
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
  Rscript src/main.R private/my_scenario NULL 1 NULL 0.1 1 NULL 0 none

  # Production run with full sample
  Rscript src/main.R public/my_scenario scenario_id 0 vintage_name 1 1 baseline_vintage 1 none

  (The old `user_id` argument — position 3 — was retired 2026-07-25; it was never read.)

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
- `estate.yaml`: Estate tax (exemption, graduated rate schedule, portability switch, income-tax-at-death deduction switch)

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

### Override Mechanics

Reform YAML files override baseline at the **subparameter level** — the entire subparameter object (value + indexation fields) is replaced, not merged. Key rules:
- Always include `i_measure` when overriding an indexed subparameter (it's the gate for indexation)
- Always include complete time series in `value` blocks
- Do NOT include `filing_status_mapper` or `indexation_defaults` unless you need to change them
- Use `'default'` keyword to inherit from `indexation_defaults`; omitting a field sets it to NULL

**Detailed override rules, common mistakes, and examples are in the `/policy-config` skill** (`.claude/skills/policy-config/SKILL.md`).

### Scenario Configuration: the three legs

A scenario is three pointers, and each of them names a FOLDER:

| Leg | Answers | Where |
|---|---|---|
| `tax_law` | what is the policy | `config/scenarios/tax_law/` |
| `economy` | how does the world work | `config/scenarios/economy/` |
| `behavior` | how do agents respond | `config/scenarios/behavior/` |

Every leg has the same shape: a complete `default/` layer, plus sparse deltas
under `alternatives/` (nesting arbitrary, folders human-named). A runscript cell
is the reserved word `default` or a path under that leg's `alternatives/`.
Resolution machinery for the economy and behavior legs is
`src/misc/scenario_config.R`; tax law keeps its own subparameter-replacement
parser (`src/data/tax_law.R`), reached through `tax_law_path()`.

Economy values — η, σ, the corporate incidence shares, the evasion
elasticities, the estate reporting response, the interface vintages, the saving
profile — are read at the point of use through `economy_param('kg', 'eta')`,
never captured at source time, because they are scenario-scoped. Override
granularity is the whole named entry: an alternative's file replaces value,
kind and provenance together, never merges within an entry. `locked: true`
entries (the estate valuation bridge) refuse override outright.

Each economy channel file declares a `_channel` role. A `transmission` channel
is conventional-side only, and reading one on the static pass is an error —
which is what makes "static results are law-only" a machine-checked property
rather than a convention. `state` channels are readable on both passes.

**Every entry declares a `kind`, and each kind owes different provenance.** The
schema check at load time is what stops an undocumented number being added.

| kind | meaning | required | staleness-checked |
|---|---|---|---|
| `calibrated` | output of a procedure | `set`, `target`, `derived_under`, `invalidated_by`, `rederive` | yes |
| `sourced` | from a paper or convention | `citation` | no |
| `judgment` | someone chose it | `note` | no |
| `structural` | a model-form switch | `note` | no |

Only `calibrated` values can go stale, because only they have inputs. A stale
value **HARD STOPS the run** (`CONFIG_ENFORCE_STALENESS`), on three arms: the
data vintages in `derived_under` no longer match the run's, a file in
`invalidated_by` has changed content since the value was pinned, or a
`conditioned_on` configuration value has moved. The check runs ONCE, at parse
time (`parse_globals` → `resolve_all_scenarios`), which is also what covers the
SLURM path. Three legitimate ways past a stop, all visible in the output:
re-derive and re-pin, override the value in an alternative, or put a dated
`waiver: {date, reason}` block on the entry in the POINTING alternative file.
`active_when` marks an entry the live configuration does not read (the kg
per-form pairs).

**After a behavior-preserving refactor** that touches a file listed in some
`invalidated_by`, verify byte-identical output first, then re-pin the hash —
never the other way round.

**What does NOT go here:** numerical plumbing (epsilons, tolerances), structural
bounds (age topcodes), and operational toggles.

**Environment variables are retired.** The nineteen `KG_ETA`/`SIGMA_CONV`/
`CORP_KAPPA`-style back doors are gone. A sweep is now a scenario, which means
it is recorded in the vintage instead of vanishing with the shell.

**Every vintage carries a manifest** at its root: `dependencies.csv` (interface
versions), `scenarios.csv` (the three leg pointers per scenario),
`scenario_config.csv` (every resolved value, its kind and role, and whether the
scenario overrode it), `behavioral_assumptions.csv` (tax law + modules), and
`code_version.csv` (git commit + dirty flag).

**SLURM:** `globals` and `scenario_info` already serialize wholesale, so nothing
extra is staged — but every driver must call `config_activate(economy =
scenario_info$resolved_economy, behavior = scenario_info$resolved_behavior)`
after loading `config.rds`, because `economy_param()` is fail-closed and errors
if no scenario is active. This is already done in `worker.R`, `frozen.R`,
`bathtub.R`, `wealth.R` and both `aggregate.R` phases; a NEW driver must do it
too.

### On-Model Estate Tax

Estate tax liability is computed per record in the normal year pass (no pre-pass;
each death year is independent, expected-value). Key facts:

- **Law vs measurement are strictly separated.** Estate LAW (exemption, rate
  schedule, portability, deductibility of the decedent's income tax at death)
  lives in `estate.yaml` and is reform-overridable like any
  parameter. The MEASUREMENT bridge from Tax-Data economic wealth to reported
  gross estate (valuation factors r/rho_pt, per-bin deduction/DSUE fractions, gift
  add-back gamma, donor-clone cluster cap) lives in
  `config/calibrations/estate/bridge.yaml` and must NEVER be overridden by a
  reform. Regenerate it with `other/estate_tax/write_frozen_params.R` (sbatch)
  after any re-calibration; it is pinned to a Tax-Data vintage and the sim warns
  on mismatch.
- **Calculator is pure and weight-free** (`calc_estate()`,
  `src/calc/functions/tax/estate.R`): liability conditional on death, two full
  DSUE/no-DSUE branch calcs for singles (the unified-credit kink is nonlinear),
  joint records at the both-die event with 2x exemption. Mortality (`estate_m`,
  incl. the cluster cap — a population-level weights operation) is computed in
  `src/sim/estate.R` and applied only at aggregation.
- **Detail columns**: `estate_m`, `estate_p_dsue`, `liab_estate_nodsue`,
  `liab_estate_dsue`, `estate_distributable` on every detail file.
- **Receipts**: `revenues_estate_tax` = CBO level + on-model delta
  (scenario − model-baseline), booked in FY death-year + 1. The model never sets
  baseline estate LEVELS (a known growth-slope disagreement with CBO); it
  contributes reform deltas only. The off-model estate delta is superseded.
- **Heir-side distribution (stage 2)**: the rank-matching allocator
  (`allocate_estate_to_heirs()`, `src/data/post_processing/estate_allocator.R`)
  passes each leg-scenario's expected estate tax through to heirs by matching
  cumulative dollar mass (estates sorted by distributable value, heirs by
  inheritance; DSUE/no-DSUE branches enter as separate ladder entries). It runs
  on the fly inside `process_for_distribution()` for both legs — no
  cross-scenario file dependency. Heir structure (p, inheritance) comes from
  the baseline Estate-Tax-Distribution interface; inheritance is GROSS of
  estate tax (assumption, evidence in thoughts doc §12), so only the liability
  column varies by scenario. Aggregate identity Σw·p·λ = E[estate tax] per
  (leg, year) ties to `totals/estate.csv`; per-year diagnostics land in
  `static/supplemental/estate_allocator_diag_{t}.csv`. Heir-ladder exhaustion
  is a hard error. NOTE: kg_dynamics deemed-realization tax deliberately keeps
  the proportional-to-inheritance smear (it has no exemption threshold — the
  rank match exists because the estate tax is threshold'd).

### Wealth Dynamics (the wealth bathtub)

A **mechanical, conventional-side** saving-financing channel that lets cross-base
interactions surface: a share `s = 1 − MPC` of the net above-baseline
**during-life** tax (income + payroll − deemed + **wealth**) is financed out of
wealth rather than consumption, compounds over time, and **drains into the estate
(and capital-income) base at death**. Quantifies capital-gains-during-life ↔
estate tax and wealth-tax ↔ capital-income tax. Code: `src/sim/wealth_dynamics.R`
(+ generic cohort primitives in `src/sim/cohort_bathtub.R`). Key facts:

- **It is NOT a behavior module.** There is no `do_wealth_dynamics()` hook. The
  channel is configured by a per-scenario **FINANCING PROFILE** — a
  bracket-varying saving share `s(age, net-worth percentile)` plus a within-age
  transition matrix `M` — resolved by `wealth_dyn_resolve_profile()` from a
  **folder** under `config/calibrations/wealth_profiles/<name>/` (two files: `s.csv` =
  per-cell `s = 1 − MPC`; `M.csv` = the `n_pctiles × n_pctiles` transition,
  absent ⇒ identity). Two runscript columns select it: `wealth_financing` (a
  profile folder name, or `none`/`off` to force off) and the back-compatible
  scalar `s` (a FLAT shorthand: constant `s_mat`, identity `M`). Precedence:
  `none/off` > folder > scalar `s` > the **auto-applied `default` profile**
  (used when neither column is set). `scenario_uses_wealth_dynamics()` keys off
  `max(s_mat) > 0` (so a flat-zero profile — for example, an explicit `s = 0` —
  is dormant and skips the ~2× split-pass compute), NOT the `behavior` column.
  The shipped `default` is **calibrated**
  (2026-07-07, persistent-flow anchor: s ≈ 0.1 bottom → 0.80 top percentile,
  age-tilted; memo at `other/wealth_dynamics/default_s_calibration.md`), so the
  channel is ON model-wide for any scenario that doesn't set
  `wealth_financing = none`. `example_age_wealth/` is an illustrative (not calibrated) bracket
  profile. The haircut applier (`wealth_dyn_apply_to_records()`) is invoked
  **directly as a fixed step at the head of the final conventional pass** in
  `run_one_year()`, before the behavior modules and `do_taxes`; it consumes the
  precomputed deficit `P` (so only the pre-pass reads the profile).
- **Conventional-only.** Static stays the clean law-only counterfactual; the
  interaction surfaces as the `static − conventional` estate/capital-income
  delta, reported via **receipts** (distribution tables stay static-sourced, D20).
  Composes with kg: a scenario sets `behavior = kg_dynamics` *and* `s = 0.5`; the
  haircut applies first, kg runs on the haircut frame.
- **Cells** = (age cohort × within-age net-worth percentile). Joint key
  `pmax(age1,age2)` pre-80+-topcode (matches kg / distribution). Ranking **drops
  `net_worth ≤ 0`** (plan D17 — deliberately differs from `distribution.R`'s `< 0`).
- **Forcing is the GENERALIZED after-tax cash flow `F = ΔT⁰ − ΔY_exog`,
  CONVENTIONAL (wealth-excluding):** `ΔT⁰ = Δ(liab_iit_pr + liab_wealth)` =
  `Δ(liab_iit_net + liab_pr − liab_deemed + liab_wealth)`, scenario − baseline;
  `ΔY_exog` = the corporate channel's analytic external-income shock
  (`corp_dY_exog` detail column; 0 for every non-corp scenario — numerically
  identical to the old tax-only forcing there). Measured on a dedicated
  **conv-no-wealth pass** (behavior on, haircut off) → its own output root
  `{scenario}/conventional_no_wealth/detail/` (never clobbers final-conv detail;
  no totals/receipts written).
- **Kernel** `G(a,p,t) = (1 + r_total(t)) − s·(τ·y + τ_w)`: `r_total` = per-year
  **nominal GDP/capita** growth (Macro-Projections `gdp_c`); `τ` = `mtr_cap_bundle`
  (composition-weighted capital-income bundle MTR, measured through the
  calculator); `y` = cell-aggregate `ΣF/Σgross` (clamped ≥0); `τ_w` = marginal
  wealth rate (`mtr_net_worth`). Guard `0 < G ≤ 1+r_total` per cell.
- **Recurrence is per-living-record** (NO `(1−m)` factor — deaths handled only at
  aggregation via each record's `estate_m`, D1). State `P(a,p,t)` + percentile
  cutoffs written to `{scenario}/conventional/supplemental/wealth_dynamics_state/{year}.rds`.
- **`WEALTH_CAP_FLOWS`** (`src/sim/wealth_dynamics.R`) is the SINGLE SOURCE OF
  TRUTH for which capital flows the MTR bump and the haircut scale, and at what
  weight (pure-capital 1.0; pass-through slice 0.2, the `economy.R` raw list).
  Scale `kg_lt_basis` with `kg_lt`. value.* assets scale **uniformly** by `(1−f)`
  (so `s_pt`/`rho_pt` stay invariant); pass-through flows by `(1 − 0.2·f)`.
  `net_worth` is recomputed from the eroded balance sheet so `calc_wealth`
  reprices `liab_wealth` and `calc_estate` the estate base.
- **`net_worth` pass semantics:** under the channel, CONVENTIONAL `net_worth` is
  post-haircut; STATIC is un-haircut. Pin the heir-allocator `Σw·p·λ` identity to
  **static** `totals/estate.csv`.
- **Guards** (mirror kg): `wealth_dyn_check_run_compat()` hard-stops `pct_sample ≠
  1` / VAT / excess-growth; `wealth_dyn_check_provenance()` (`WEALTH_DYN_PROVENANCE`
  stamp) warns on a stale Macro vintage (`WEALTH_STRICT_CALIB=1` stops).
- **Operational params** in the economy leg's `wealth.yaml` (`fmax`,
  `n_pctiles`, `r_total_additive_delta`) — structural knobs, not a reform file. `s` and `M` are NOT here: they live in the per-scenario
  financing **profile** folder (above). Profiles are regenerated by
  `other/wealth_dynamics/write_profiles.py`. (NB: the old global
  `transition_matrix_file` YAML knob and the `bounding_orchestrator.sh` that
  `sed`-swapped it are superseded — identity-vs-uniform M is now just two profile
  folders, with no pipeline serialization.)
- **Run one year past the reporting window** (estate/wealth deltas are FY
  death-year+1 lagged).

### On-Model Corporate Incidence

A **mechanical, conventional-side, REVENUE-side** channel (`src/sim/corp/`)
that maps the gross corporate receipts delta from Off-Model-Estimates onto records:
flow cuts (dividends/interest/rent/pt), an equity markdown on exposed `value.*`
stocks, kg gain adjustments, bathtub dissaving, and an **endogenous individual-tax
offset that simply materializes in conventional receipts deltas**. Static stays the
clean law-only counterfactual and the distribution smear is untouched (D4/D5);
the channel contributes reform **deltas only**. Design docs:
`other/corporate_incidence/{CONSIDERATIONS,FORMAL_MODEL}.md` (rulings D1–D18, P1–P14).

- **Activation is FAIL-CLOSED and automatic** (no runscript column): the scenario's
  OME vintage must carry `corporate_meta.yaml` next to `revenues.csv`
  (`gross_of_offset: true`, `provision_type: rate`, `beyond_horizon: extend|zero`).
  Absent metadata + nonzero corporate wedge → channel OFF, status quo, one loud
  warning. Present-but-invalid metadata, a depreciation-signature (seesaw) wedge
  path, or enactment before the sim window → **hard stop**. A/B runs use the
  existing `dep.Off-Model-Estimates.vintage`/`.ID` overrides.
- **Parameters are hardcoded `CORP_*` constants** (WEALTH_CAP_FLOWS style,
  provenance-commented; several are Phase-0c placeholders — update in place when
  measured). Sweep corners via env vars ONLY: `CORP_SIGMA_N`, `CORP_KAPPA`,
  `CORP_PRICED_AS_PERMANENT=1`.
- **Paths are analytic** (`corp_resolve_paths`, memoized; no serialized state, no
  new SLURM phase): π_t = `gdp_corp − rev_corp` (Macro-Projections); wedge split by
  σ_N; η(t) vintaging at 0.057 (`do_capital_adjustment` convention); debt-rollover
  ramp for interest; D15 κ-split (corporate flows retain κ·η·w_norm; noncorporate
  lines get (1−κ)·η·w_norm ∝ Macro `gdp_interest`/`gdp_rent`/0.2·`gdp_proprietors`);
  perfect-foresight markdown M_t/μ_t by backward recursion (constant nominal
  r = tsy_10y(enactment) + ERP, Gordon terminals, telescoping hard-asserted).
- **Record applier runs at the head of EVERY conventional-side pass** (incl.
  conv-no-wealth), BEFORE the wealth haircut and behavior modules. D16 contract:
  dividends/interest/rent/pt flows are external income → accumulate analytically
  into the **`corp_dY_exog`** detail column; kg and retirement distributions are
  internal conversions (tax leg only — adding them to ΔY_exog double-counts the
  markdown). `value.*` markdown is **column-specific** (ω_a exposure; basis never
  scales; `value.db` never debited (D10); pt stocks untouched (P14) so ρ_pt/s_pt
  stay frozen); `net_worth` recomputed. Retirement distributions scale with their
  source-balance markdown (P7; dc-share split for pensions).
- **kg composition (D18, one rule, two entry points):** non-kg runs get the exact
  per-record form `Δkg = ω_kg[φ·kg_lt − μ·(kg_lt + kg_lt_basis)]` inside the
  applier (basis co-scales with φ only). kg_dynamics runs skip that block: the
  PRICE margin enters as a bathtub **gain-state debit**
  (`corp_kg_state_debit_by_year`: D_a(t) = μ_t·V_corp_exposed_a, recomputed each
  year, consumed by `extra_R` ONLY — recurrence stays clean, deemed gains already
  see the markdown through record `value.*`), and the QUANTITY margin is the
  post-behavior φ step (`corp_apply_kg_quantity_to_records`). Never both.
- **Wealth bathtub forcing generalized** to F = ΔT⁰ − ΔY_exog (see the wealth
  section). The wealth applier ranks cells on the RAW pre-corp `net_worth`
  (`rank_value` argument) so the markdown never shifts records across cells.
- **Guards:** `corp_check_run_compat` hard-stops `pct_sample ≠ 1` / VAT /
  excess-growth. **Conservation diagnostic** (WARN-level reconciliation REPORT):
  `conventional/supplemental/corp_conservation_diag_{t}.csv` — per-line
  analytic-vs-realized is the testable content; the three-way identity is a report.
- **Receipts plumbing unchanged:** corporate input keeps its 0.75/0.25 CY→FY
  booking; the endogenous offset rides ordinary receipts deltas; estate/wealth
  legs book FY+1 → **run one year past the reporting window**. Stacking caveat:
  with an endogenous offset the corporate row is NOT stacking-order-invariant
  (§8.13).

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

Use the `/policy-config` skill to create reform configurations. It contains detailed override rules, common mistakes, examples, and a complete baseline file reference.

**Quick summary:**
1. Create reform directory under `config/scenarios/tax_law/{public|private|tests}/{reform_name}/`
2. Add YAML files that override only the subparameters you're changing
3. Reforms OVERWRITE baseline subparameters — include full time series and all indexation fields
4. Create a runscript CSV referencing the reform's `tax_law` path

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
2. Specify types in `mtr_types` ("nextdollar" or "extensive")
3. Access MTRs in behavioral module via `baseline_mtrs$mtr_{varname}` and `static_mtrs$mtr_{varname}`
4. Use `apply_mtr_elasticity()` for standard elasticity applications

## SLURM Multi-Node Pipeline

`slurm_run.sh` is an alternative entry point that distributes the simulation across SLURM cluster nodes. It produces identical output to `main.R` but runs year-tasks in parallel across nodes rather than sequentially (or via `mclapply` on one node).

**Usage:**
```bash
bash slurm_run.sh <runscript> <scenario_id> <local> <vintage> <pct_sample> <stacked> <baseline_vintage> <delete_detail>
```
Arguments are the same as `main.R` except `multicore` is omitted (SLURM handles parallelism).

**Pipeline phases:**
1. Phase 0 (login node): `src/slurm/setup.R` — parses globals, builds configs, serializes to `.rds`
2. Phase 1 (SLURM array): `src/slurm/worker.R` — runs `run_one_year()` for each baseline year
3. Phase 1B (SLURM array): `src/slurm/frozen.R` — kg_dynamics frozen mechanical pre-pass per scenario (writes static-side mech state consumed by Phase 2A; no-op for non-kg scenarios)
4. Phase 2A/2B/2C (SLURM array): `src/slurm/worker.R` (2A static, 2C conventional) + `src/slurm/bathtub.R` (2B kg bathtub) — `run_one_year()` for each counterfactual × year
5. Phase 2N/2W (SLURM array, **only for `s>0` wealth scenarios**): `src/slurm/worker.R` 2N (conv-no-wealth, the `ΔT⁰`/`mtr_cap_bundle` pass) + `src/slurm/wealth.R` 2W (wealth bathtub pre-pass). DAG: 2A → 2B → 2N → 2W → 2C; 2W also depends on Phase 1 baseline
6. Phase 3a (SLURM array): `src/slurm/aggregate.R` — writes totals CSVs and receipts per scenario
5. Phase 3b (SLURM array): `src/slurm/aggregate.R` — post-processing (1040, revenue, distribution, time burden)
6. Phase 4 (single job): `src/slurm/aggregate.R` — stacked reports and optional detail purge

**CRITICAL — keeping the SLURM pipeline in sync:**

The SLURM pipeline duplicates orchestration logic from `main.R`, `run_sim()`, and `do_scenario()`. When modifying any of the following, you MUST update the corresponding SLURM file:

| If you change...                                    | Also update...                |
|-----------------------------------------------------|-------------------------------|
| `run_sim()` totals-writing or `calc_receipts()` call | `src/slurm/aggregate.R` Phase 3a |
| `do_scenario()` post-processing calls               | `src/slurm/aggregate.R` Phase 3b |
| `do_scenario()` pre-simulation setup (offsets, indexes, tax law) | `src/slurm/setup.R` |
| `run_frozen_pass()` or the kg mechanical state contract | `src/slurm/frozen.R` (Phase 1B) |
| `run_wealth_bathtub_pass()` or the wealth state/detail contract | `src/slurm/wealth.R` (Phase 2W) |
| `do_scenario()` wealth/conv-no-wealth pass sequencing or a new `pass_type` | `src/slurm/worker.R` (2N dispatch), `src/slurm/setup.R` (2N/2W manifest + `N_PHASE2N`/`N_PHASE2W`), `slurm_run.sh` (2N/2W phases + 2C deps) |
| `main.R` stacked post-processing or `purge_detail()` | `src/slurm/aggregate.R` Phase 4 |
| `parse_globals()` return structure                  | `src/slurm/setup.R` serialization |
| A new SLURM driver script                           | must call `config_activate(economy = scenario_info$resolved_economy, behavior = scenario_info$resolved_behavior)` after loading `config.rds` |
| New global free variables used by post-processing   | `src/slurm/common.R` `reconstitute_environment()` |
| `run_one_year()` signature                          | `src/slurm/worker.R` |

Safe changes that need NO SLURM updates: anything inside `run_one_year()`, tax calculation functions, behavioral modules, YAML configs, runscripts. The corporate-incidence channel (`src/sim/corp/`) is in this category by construction: its applier lives inside `run_one_year()`, its kg glue inside `run_bathtub_pass()`/`kg_dyn_run_bathtub_pass()`, its paths are analytic (recomputed per worker, no serialized state), and `reconstitute_environment()` sources all of `src/` recursively — no manifest, phase, or worker changes.

## Notes and Best Practices

- **Runscripts are recipes**: They coordinate all model inputs in one place
- **YAML reforms overwrite baseline**: Always include full time series for modified parameters
- **Behavioral modules are flexible**: Don't limit yourself to elasticity functions—implement any logic
- **MTR calculation is powerful**: Microsimulation computes precise EMTRs for any variable via $1 marginal adjustments
- **Documentation is required**: All behavioral modules must have formatted comment blocks
- **Heterogeneous elasticities**: Assign different elasticities to different demographic groups/income levels using `case_when()`
- **Extensive margin simulation**: Use probabilities and RNG for binary outcomes (e.g., employment exit)
- **Global random seed**: Always reset with `set.seed(globals$random_seed)` before stochastic operations
- **Report full paths for deliverables**: Whenever you finish producing a document, chart, table, or other output artifact, print its FULL absolute path (e.g., `/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/.../file.html`) in your summary — never just the filename or a relative path
