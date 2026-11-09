# State Source Packet Template

State: `XX`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `YYYY-MM-DD`

> **Do not restate status here.** The packet is the primary-source evidence record
> for one jurisdiction; per-state status is one row of
> `../state_tax/state_parameter_rollout.csv`, whose vocabulary is defined in
> `../state_tax/state_parameter_workflow.md` §Validation gates. Copy the two lines
> above verbatim and keep `Last updated:` current. A finding that happens to be
> about progress — "cross-model triage closed, all eight cells clear the bar" —
> belongs in the sections below or in the tracker's `notes` column, not in a
> `Status:` line that will drift.

## Scope

- Tax years covered:
- Baseline only or reform work too:
- Major structural features:

## Primary sources

### Current forms and instructions

- Form/booklet:
- Instruction PDF / page:
- DOR landing page:

### Historical forms and instructions

- NBER historical archive:
- State prior-year forms page:
- Missing years / gaps:

### Statutes and technical guidance

- Starting point / conformity:
- Rate schedule:
- Deductions / itemization rules:
- Credits:
- Filing requirement:
- Indexation:

## Secondary cross-checks

- Tax Foundation:
- TPC / ITEP:
- PolicyEngine / TAXSIM:

## Parameter inventory by file

### `agi.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

### `ded.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

### `exempt.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

### `ord.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

### `credits.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

### `filing.yaml`

- Parameters to encode:
- Source lines:
- Known approximations:

## Worksheet tests to add

- Base case:
- Threshold / cliff case:
- Phase-in / phase-out case:
- Any state-specific structural edge case:

## Known differences

- Data limitations:
- Future-year projections:
- Omitted provisions:

## Cross-model validation notes

- TAXSIM years to compare:
- PolicyEngine years to compare:
- Expected mismatch reasons:

## Aggregate validation notes

- HT2 targets to inspect once weights land:
- Revenue-agency benchmark if available:
