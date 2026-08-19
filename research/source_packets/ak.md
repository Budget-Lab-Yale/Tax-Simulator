# Alaska Source Packet

State: `AK`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-forward.
- Baseline only or reform work too: baseline zero-tax stub.
- Major structural features: no broad individual income tax; no state EITC in v1 scope.

## Primary sources

### Current forms and instructions

- No individual income tax return packet identified for broad wage/income taxation.
- Alaska Department of Revenue Tax Division: https://tax.alaska.gov

### Historical forms and instructions

- No broad individual income tax return packet identified for 2017-forward.

### Statutes and technical guidance

- No broad individual income tax provision to encode in v1.

## Secondary cross-checks

- Tax Foundation / FTA no-broad-IIT lists should be used as transcription checks.

## Parameter inventory by file

### `agi.yaml`

- `start_point = 1` only to satisfy the generic state calculator contract.
- Source lines: Alaska DOR tax program listing.
- Known approximations: none for broad IIT; non-IIT taxes outside scope.

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

- Local sales/property taxes and Permanent Fund Dividend treatment are outside the state-IIT module.

## Cross-model validation notes

- TAXSIM state liability should be zero for broad individual income tax.

## Aggregate validation notes

- Aggregate liability should be zero once state weights land.
