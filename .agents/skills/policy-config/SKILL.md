---
name: policy-config
description: Create tax law YAML configuration files (and optionally runscripts) for policy reform scenarios, with built-in multi-agent verification. Use when the user describes a tax policy change they want to model.
disable-model-invocation: true
argument-hint: <policy description, e.g. "5pp cap gains rate hike on top bracket starting 2026">
---

Create tax law configuration for a policy reform based on: $ARGUMENTS

# Policy Configuration Skill (with Verification)

You are creating tax law YAML files that the Tax-Simulator model will use to score a policy reform. This is the most error-prone part of the workflow -- follow these instructions precisely.

After creating the YAML, you will automatically verify it using two independent agents.

---

## Part A: Create the Reform YAML

### Step 1: Understand the policy

**If you are unsure how a provision works under current law** — e.g., phaseout mechanics, eligibility rules, interaction between provisions, or statutory dollar amounts — read the relevant IRC section in `resources/irc/`. These are full statutory text files covering the key provisions modeled by the simulator. See `resources/irc/README.txt` for the index mapping sections to YAML files. For sections not included, fetch the full text from `https://www.law.cornell.edu/uscode/text/26/{section_number}`.

Parse the user's policy description and identify:
- **Which tax provisions are affected** (income tax rates, capital gains, CTC, deductions, credits, etc.)
- **Which baseline YAML files need overrides** (ord.yaml, pref.yaml, ctc.yaml, etc.)
- **What changes**: rate changes, threshold changes, new provisions, repeals, extensions of expiring provisions
- **Effective date**: when the policy takes effect (default: next calendar year)
- **Sunset**: whether the provision expires (default: permanent -- no sunset)

If the policy description is ambiguous or you're not sure which parameters to modify, **ASK the user** using AskUserQuestion. Common ambiguities:
- "Raise the top rate" -- income tax? capital gains? both?
- "Expand the CTC" -- higher credit value? higher refundability? broader age eligibility? phaseout changes?
- "SALT cap repeal" -- full repeal or just raise the cap? Does Pease come back?
- Filing-status-specific thresholds -- same for all filers, or differentiated?

### Step 2: Read the relevant baseline YAML files

Before writing ANY reform YAML, you MUST read the baseline files you're overriding:
```
config/scenarios/tax_law/baseline/{parameter}.yaml
```

This is critical because:
1. You need the complete time series to include in your override
2. You need to know which subparameters are indexed and copy their indexation fields
3. You need to understand the filing_status_mapper structure to avoid clobbering it
4. You need to see vector subparameters to get array dimensions right

Also read the relevant **calc function** in `src/calc/functions/` (e.g., `deductions/item_ded.R` for itemized deductions, `deductions/std_ded.R` for the standard deduction, `credits/ctc.R` for CTC, etc.) to understand how each subparameter flows into the actual calculation logic. This helps you avoid unintended side effects — for example, a subparameter used via `filing_status_mapper` may affect multiple filing statuses, or a limit subparameter may interact with other provisions in ways not obvious from the YAML alone.

### Step 3: Create the reform directory and YAML files

**Default location:** `config/scenarios/tax_law/public/{reform_name}/`
- Use `tests/` if user says "test" or it's exploratory
- Use `private/` if user says "private" or "internal"

#### YAML Override Rules (CRITICAL -- violations cause silent bugs)

##### Rule 1: Subparameter-level replacement
When a reform YAML includes a subparameter, the ENTIRE subparameter object is replaced -- `value`, `i_measure`, `i_base_year`, `i_direction`, `i_increment` -- not merged field-by-field. You must include ALL fields.

##### Rule 2: The i_measure gate
`i_measure` is the gatekeeper for indexation. In `parse_subparam()` (tax_law.R:311):
```r
if (is.null(raw_input$i_measure)) { return(base_values) }
```
If `i_measure` is missing/NULL, base values are returned with NO indexation. Having `i_base_year`, `i_direction`, `i_increment` without `i_measure` does absolutely nothing.

**Therefore: when overriding ANY subparameter that has indexation in baseline, you MUST include `i_measure`.**

##### Rule 3: Always preserve baseline indexation
When overriding a subparameter that is indexed in baseline, copy ALL indexation fields from the baseline version. Specifically:
- If baseline has `i_measure: default`, use `i_measure: default` in the reform
- If baseline has explicit `i_measure` (e.g., `'1987': cpi`), copy it exactly
- Same for `i_base_year`, `i_direction`, `i_increment`
- If the reform changes the base year (e.g., new provision starting 2026), update `i_base_year` accordingly

##### Rule 4: Complete time series
The reform's `value` block replaces the baseline's entire value history. Always include:
- The baseline's historical values (copy from baseline YAML)
- The reform's new values at the effective date
- If the provision sunsets, include the reversion values

Example -- raising the top ordinary rate to 39.6% starting 2026 (permanent):
```yaml
rates:
  value:
    '2014': [0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
    '2018': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
    '2026': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.396]
```

Example -- same but sunsets after 2030:
```yaml
rates:
  value:
    '2014': [0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
    '2018': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
    '2026': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.396]
    '2031': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
```

##### Rule 5: Do NOT override filing_status_mapper or indexation_defaults
These are top-level YAML keys that are replaced ENTIRELY if present in the reform.
- If you include `filing_status_mapper`, it clobbers ALL mappings in the baseline file -- including ones you didn't intend to change (e.g., `bonus`, `bonus_other`, `po_range`, `salt_limit`, etc.)
- Same for `indexation_defaults`
- **Only include these if you are specifically changing them AND you include ALL entries from baseline plus your changes**

The only cases where you need `filing_status_mapper`:
- Creating a brand new provision (new YAML file) that has filing-status-varying thresholds
- Changing the filing status mapping formula itself (rare)

The only cases where you need `indexation_defaults`:
- Creating a brand new provision with its own indexation scheme
- Changing the inflation measure or base years for the entire parameter

##### Rule 6: Vector subparameters
YAML arrays like `[1000, 0]` create separate columns (e.g., `ctc.value_young1`, `ctc.value_young2`). When overriding:
- Match the array length exactly
- If indexed, indexation fields must also be arrays of the same length: `i_base_year: [2024, 2024]`, `i_increment: [100, 100]`

##### Rule 7: The 'default' keyword
Setting `i_measure: default` means "use the value from `indexation_defaults`". This ONLY works when:
- The string `'default'` is explicitly written
- `indexation_defaults` exists in the same file (either from baseline or your reform)
- Omitting a field entirely is NOT the same as `default` -- it sets the field to NULL

#### Turning provisions "on" from zero
Many provisions in baseline have `value: 0` or `value: Inf` (meaning "off"). To activate:
- Set `'2014': 0` (or whatever baseline has), then set the policy year to the active value
- Example: `'2014': 0` then `'2026': 5000` turns on a $5000 provision in 2026

#### Turning provisions "off" (repeal)
Set the value to 0 (for credits/deductions) or Inf (for thresholds/limits) at the repeal year:
- Example: `'2014': 10000` then `'2026': 0` repeals a $10,000 deduction in 2026
- For rate schedules: don't set rates to 0 -- that eliminates the tax. Instead modify the specific rate.

### Step 4: Show the user what you created

After creating the YAML files:
1. Show the full contents of each file
2. Explain which baseline subparameters are being overridden and why
3. Flag any decisions you made that the user should verify

**Save the exact original policy description text -- you need it for the verification step.**

---

## Part B: Verify with Independent Agents

Immediately after showing the created files (do NOT wait for user confirmation), run verification.

### Step 5: Spawn both verification agents IN PARALLEL

Use the Agent tool to spawn both agents simultaneously in a **single message** (two Agent tool calls):

**Agent A -- Round-trip (`policy-roundtrip`):**
Prompt: "Read the reform YAML files at `config/scenarios/tax_law/{reform_path}/` and describe the policy change in plain English. Compare each reform file against the corresponding baseline file at `config/scenarios/tax_law/baseline/`. Do NOT reference any original policy description -- describe only what the YAML files contain."

- This agent reads the YAML you just created and describes what policy it represents
- It must NOT see the original policy description -- that's the whole point

**Agent B -- Independent extractor (`policy-extractor`):**
Prompt: "Given this policy description: '{paste the EXACT original policy description}', independently produce reform YAML files. Write them to `config/scenarios/tax_law/tests/_verification/{reform_name}/`. IMPORTANT: Do NOT read any files under `config/scenarios/tax_law/{reform_path}/` -- you must work independently."

- This agent independently produces its own YAML from the same description
- It must NOT see your reform YAML files

### Step 6: Reconcile and present the verification report

Once both agents return:

**6a. Round-trip check:**
- Compare Agent A's English description against the original policy description
- Flag: provisions the agent described that the user didn't ask for, provisions the user asked for that the agent missed, wrong numbers/years/filing-status details

**6b. YAML diff:**
- For each file in both the original reform and `_verification/` directory, run:
  ```bash
  diff config/scenarios/tax_law/{reform_path}/{file}.yaml config/scenarios/tax_law/tests/_verification/{reform_name}/{file}.yaml
  ```
- Categorize differences: value disagreements, coverage disagreements, indexation disagreements, cosmetic only

**6c. Structural issues:**
- If either agent reports missing `i_measure`, array mismatches, incomplete time series -- flag prominently

**Present the report:**

---

## Verification Report

### Round-Trip Description
{Agent A's full English description of what the YAML contains}

### Round-trip vs. original intent
{Your comparison -- what matches, what doesn't}

### YAML Comparison (Your output vs. Independent extraction)
For each file:
- **{filename}**: MATCH or DISAGREEMENT
  - {If disagreement: specific parameter-level differences with both values}

### Coverage
- Files only in your output: {list or "none"}
- Files only in independent extraction: {list or "none"}

### Summary
- Files compared: {N}, Full matches: {N}, Disagreements: {N}

### Confidence
{One of: "All checks passed" / "Minor cosmetic differences only" / "Substantive disagreements found -- review above"}

---

### Step 7: Cleanup and next steps

- Offer to delete `config/scenarios/tax_law/tests/_verification/{reform_name}/`
- Ask whether the user wants a runscript CSV created
- If disagreements exist, offer to fix the original YAML based on user's adjudication

---

## Part C: Create Runscript (if requested)

If the user wants a runscript, ask:
- **Year range** (e.g., 2025:2035) -- the user will typically supply this
- **Behavior modules** -- if any (the user will specify)
- **Distribution years** -- which years need full distribution tables

Then create a CSV at `config/runscripts/{path}.csv` with columns:
```
ID,tax_law,behavior,years,dist_years,mtr_vars,mtr_types
```

Rules:
- Always include a `baseline` row
- The baseline `tax_law` should be `baseline` (or `baseline_2024` etc. if the user specifies)
- Start `years` at least 1 year before the policy's effective date (t-1 rule)
- If behavior modules are specified, baseline MUST have matching `mtr_vars` and `mtr_types`
- Match `mtr_vars` to behavior modules:
  - `charity/*` needs `char_cash` (nextdollar)
  - `kg/*` needs `kg_lt` (nextdollar)
  - `entity_shifting/*` needs `kg_lt` and `part_active` (nextdollar)
  - `employment/*` needs `wages1` and `wages2` (extensive)
  - `child_earnings/*` needs `wages1` and `wages2` (extensive)
  - `ot/*` needs `ot1` and `ot2` (nextdollar)
  - `tips/*` needs `tips1` and `tips2` (nextdollar)
  - `capital_income/*` needs `kg_lt` (nextdollar)

---

## Baseline YAML File Reference

These are the 26 baseline parameter files. Read the relevant ones before creating overrides.

| File | What it controls |
|------|-----------------|
| `agi.yaml` | Adjusted Gross Income computation |
| `alt_max.yaml` | Alternative maximum tax cap |
| `amt.yaml` | Alternative Minimum Tax |
| `below.yaml` | Below-the-line deductions: tips, overtime, senior deduction |
| `caregiver.yaml` | Caregiver credit |
| `cdctc.yaml` | Child and Dependent Care Tax Credit |
| `char.yaml` | Charitable deduction (above-the-line and limits) |
| `corp.yaml` | Corporate tax parameters |
| `credits.yaml` | Miscellaneous nonrefundable credits |
| `ctc.yaml` | Child Tax Credit (value, phaseouts, refundability, age limits) |
| `ed.yaml` | Education credits and deductions |
| `eitc.yaml` | Earned Income Tax Credit |
| `filing.yaml` | Filing status parameters |
| `item.yaml` | Itemized deductions (SALT, mortgage interest, Pease, medical, misc) |
| `niit.yaml` | Net Investment Income Tax |
| `ord.yaml` | Ordinary income tax (rates and brackets by filing status) |
| `pe.yaml` | Personal exemptions |
| `pref.yaml` | Preferred rates (capital gains/dividends rates, brackets, carryover basis) |
| `pr.yaml` | Payroll taxes (OASDI, HI/Medicare, SECA) |
| `qbi.yaml` | Qualified Business Income / Section 199A |
| `rebate.yaml` | Generic per-person refundable credit (stimulus, UBI) |
| `savers.yaml` | Saver's credit |
| `ss.yaml` | Social Security benefit parameters |
| `std.yaml` | Standard deduction |
| `surtax.yaml` | Income surtax parameters |
| `wagesub.yaml` | Wage subsidy parameters |

## Common Policy Patterns (quick reference)

**Rate change** (ord.yaml or pref.yaml): Override `rates` with full history + new rates at effective year.

**Bracket change** (ord.yaml or pref.yaml): Override `brackets_single`, `brackets_married`, `brackets_head` with full history + new values. Include indexation fields.

**Credit value change** (ctc.yaml, eitc.yaml, etc.): Override `value_*` subparameters. Watch for vector subparameters.

**Deduction limit change** (item.yaml): Override the specific limit subparameter (e.g., `salt_limit_married`, `mort_int_limit`).

**New provision from zero**: Create a new YAML file (or use `rebate.yaml` for simple per-person credits). Set baseline years to 0, policy year to active value.

**TCJA extension**: Remove the 2026 reversion by only including pre-2017 and 2018 values (no 2026 entry means 2018 values continue indefinitely via fill-forward).

**Phaseout structure**: Uses `po_thresh`, `po_rate`, `po_range`, `po_type` subparameters. `po_type: 1` means rate-based phaseout; `po_type: 0` means range-based. A near-cliff can be modeled with `po_rate: 999`.
