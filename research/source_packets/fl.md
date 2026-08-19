# Florida Source Packet

State: `FL`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-forward.
- Baseline only or reform work too: baseline zero-tax stub.
- Major structural features: no broad individual income tax; no state EITC in v1 scope.

## Primary sources

### Current forms and instructions

- No individual income tax return packet identified for broad wage/income taxation.
- Florida Constitution, Article VII, Section 5: https://www.leg.state.fl.us/statutes/index.cfm?submenu=3#A7S05

### Historical forms and instructions

- No broad individual income tax return packet identified for 2017-forward.

### Statutes and technical guidance

- Florida Constitution prohibits a tax upon income of natural persons except as otherwise provided.

## Secondary cross-checks

- Tax Foundation / FTA no-broad-IIT lists should be used as transcription checks.

## Parameter inventory by file

### `agi.yaml`

- `start_point = 1` only to satisfy the generic state calculator contract.

### `ded.yaml`

- `std_amount = 0`, `item_allowed = 0`.

### `exempt.yaml`

- `personal_amount = 0`.

### `ord.yaml`

- Zero rate/bracket schedule.

### `credits.yaml`

- `eitc_match = 0`.

### `filing.yaml`

- `req_type = 3`, `req_if_fed_filer = 0`.

## Worksheet tests to add

- Zero-liability smoke test across income levels and filing statuses.

## Known differences

- Corporate income tax and local taxes are outside the individual income tax module.

## Cross-model validation notes

- TAXSIM state liability should be zero for broad individual income tax.

## Aggregate validation notes

- Aggregate liability should be zero once state weights land.
