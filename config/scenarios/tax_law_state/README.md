# State Tax Law Conventions

This directory holds state individual income tax parameters. The baseline tree is:

`config/scenarios/tax_law_state/baseline/{st}/`

where `{st}` is the lowercase postal code for the jurisdiction, e.g. `il`, `co`,
`ny`.

`jurisdictions.yaml` is the runtime registry. `states = all` resolves to its
enabled entries, so adding a research directory alone cannot change a run.

## Required files

The registry's `profile` controls the required files:

- `broad_iit` and `zero`: `agi.yaml`, `credits.yaml`, `ded.yaml`,
  `exempt.yaml`, `filing.yaml`, `ord.yaml`.
- `narrow_investment_iit`: `programs.yaml`, `investment_income.yaml`.
- `capital_gains_and_transfer`: `programs.yaml`, `capital_gains.yaml`,
  `transfers.yaml`.

Use the standard profile whenever it fits. The special profiles exist for laws
that are not broad individual income taxes, not as a shortcut around research.
Additional YAML files are allowed only when they map to a reusable generic
calculator component (for example, `child_ded.yaml` for an AGI-tiered child
deduction or `surtax.yaml` for a taxable-income surtax). Document the component
in the state source packet and prefer it to state-specific code.

## Dense Credit Tables

An optional `credit_tables.csv` may sit beside a state's YAML files when a
credit uses a dense published lookup schedule. Its required columns are
`credit_id`, `state`, `year`, `income_lower`, `income_upper`, `child_count`,
and `amount`; ranges are inclusive. The loader selects the most recent table
on or before the simulation year. A reform directory can replace a baseline
table for a state, credit, and year without changing calculator code.

## Federal Conformity Contexts

`conformity_groups.yaml` is the shared contract for states that do not follow
federal law on a rolling basis. State `agi.yaml` files use numeric
`conformity_group` values: `0` means rolling conformity, and a positive value
selects a reusable reference-law package. A ready positive group must name a
federal overlay under `config/scenarios/tax_law/` in `reference_tax_law_id`.

At runtime, the model calculates one reference federal context per ready group
and per year, then `get_state_totals()` chooses that context for every state in
the group. Conventional contexts reuse post-behavior tax-unit inputs and do not
run behavioral feedback again. Never add state-name branches for conformity.
Mark a group `ready: false` until its full statutory/selective-adoption overlay
and form cases are validated; federal-reform runs then fail clearly for states
using that group. California's groups 1 and 2 are registered but intentionally
not ready pending those legal bridges.

## Component Ledger

State results retain the broad IIT measure as `liab_st_iit` and separately
report `liab_st_narrow_iit`, `liab_st_ltcg_excise`, and `st_refund_wftc`.
`liab_st_individual_net` is tax less standalone refunds and is the state
revenue-estimate and compact-detail measure. `st_tax_filer` is the combined
filer/claimant flag used for the state `returns` total.

## Encoding rules

1. Every encoded subparameter must carry a `reference:` field.
2. Every year-keyed `value:` series must begin at `2017` or earlier.
3. Use forms/instructions as the first source of truth; use statutes for the rules
   behind indexation, conformity, and scheduled changes.
4. Secondary sources are transcription checks only.
5. Omit parameters for nonexistent features when possible; the state calculator fills
   neutral defaults via `ensure_st_params()`.
6. Record data limitations in the state source packet rather than silently
   treating PUF proxies as legal tax bases or entitlement determinations.

## Workflow

Use the companion workflow and tracker in `research/` (start at `research/README.md`):

- `research/state_tax/state_parameter_workflow.md`
- `other/state_tax_research/state_parameter_rollout.csv`
- `research/source_packets/TEMPLATE.md`

These files exist so parameter research can proceed in parallel with the state-weights
work instead of waiting for the end of the rollout.
