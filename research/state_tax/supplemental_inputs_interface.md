---
title: "Supplemental inputs — optional data for the state calculator"
role: method
workstream: state_tax
status: current
updated: 2026-10-13
sot: self
supersedes: []
superseded_by: null
---

# Supplemental inputs — optional data for the state calculator

Decision S43 (`research/decisions_log.md`, 2026-10-13). This note is the
contract between a **producer** that publishes record-level variables the
pinned Tax-Data vintage does not carry — rent paid, tenure, pension source,
and the rest of `research/state_tax/tax_data_gaps.csv` — and the simulator
that joins them onto tax units for the state calculator. The first producer
is Affordability-Index; Tax-Data itself is the second, and when it absorbs a
variable natively the simulator needs no change.

## 1. What problem it solves

Roughly 600 state provisions across 45 jurisdictions cannot be computed as
written because the Tax-Data file lacks an input (the crosswalk). Waiting for
Tax-Data to impute each one would stall the state release; encoding each
provision with a hard-coded assumption would bury the assumption. The overlay
lets a provision be **encoded now against a named optional variable with a
neutral default** — inert today, live the day a producer publishes the
variable — and makes every run say, in a coverage file, which variables were
live and for what share of records.

Three invariants, each tested in
`src/tests/state/test_supplemental_inputs_reader.R`:

1. A run that does not opt in never pins, reads or depends on a producer.
2. Federal output is byte-identical with or without an overlay. The columns
   are joined only inside `get_state_totals()`, per state, after the federal
   pass.
3. A provision gated on an absent or all-default overlay produces exactly
   today's state liability.

## 2. The file contract (producer side)

A producer is any model with an entry in
`config/interfaces/interface_versions.yaml` carrying
`contract: supplemental_inputs`. It publishes under the ordinary model_data
layout:

```
shared/model_data/<Producer>/v1/<vintage>/
  dependencies.csv             # MUST contain a Tax-Data row (ID, interface, version, vintage, scenario)
  <ID>/                        # e.g. baseline
    README.md                  # prose: what, how built, caveats
    manifest.csv               # one row per published variable
    coverage.csv               # one row per (variable, year[, state]): n_rows, n_ids, sum_weight_covered, carried
    supplemental_unit_<year>.csv.gz    # wide, key = id;          columns id, <var>, ...
    supplemental_state_<year>.csv.gz   # wide, key = (id, state); columns id, state, <var>, ...
```

- `manifest.csv` columns: `variable, key, type, units, description,
  source_vars, method`. `key` is `unit`, `person` or `state`; `type` is
  `dbl`, `int` or `lgl`. Key and type must agree with the consumer schema
  (§3) or the run refuses at parse time.
- **Wide, one file per key type per year.** Variables have different types;
  a long file would lose them. `person` variables live in the unit file under
  their `1`/`2` names.
- Either year file may be absent when the manifest publishes no variable of
  that key type. A requested variable whose year file is missing stops the
  run (as a missing State-Weights year does).
- **NA means uncovered.** A record may also be omitted entirely; the two are
  equivalent. The producer, not the consumer, decides coverage: it may
  publish cell means for ids it did not match if it wants full coverage.
- `state` values must be in the 51 modeled jurisdictions plus `PR`/`OA`.
- The producer's `dependencies.csv` names the Tax-Data vintage its ids refer
  to. The simulator requires that vintage to equal the one the run reads, by
  name or by a declared equivalence (§5).

## 3. The consumer schema

`config/interfaces/supplemental_inputs_schema.yaml` lists every optional data
variable the calculator may read, with `key`, `type`, `default`, `split`
(how the married-separate election treats it), `shadow` (whether it may
replace an existing Tax-Data column — allowed only for the all-zero
`trad_contr_er1/2` placeholders) and an optional `tax_data_equivalent`.
Names are the `overlay_variable` column of the crosswalk. It is the data
twin of `params_schema.yaml`.

Variables added 2026-10-13 for the data-gap batch: `pension_er_funded_share`
(share, default 1), `pension_public_pre1998_share`, `spouse_agi_mfs` (gated on
`has_spouse_agi_mfs`, reset on married-separate half-units) and
`nontax_support_inc` (the PA Schedule SP concept, distinct from
`hh_resources_inc`).

The two model-wide constants the AGI stage used to carry — the 0.15
US-obligation share of taxable interest and the 0.75 own-state share of
exempt interest — are the first entries, with those values as defaults, so a
producer can publish record-level shares with no code change.

## 4. Runscript opt-in and validation

- `supplemental_inputs`: space-delimited variable names. No `all`: a run
  names exactly what it consumes.
- `dep.<Producer>.vintage` / `dep.<Producer>.ID`: the producer pin. No
  producer has a default vintage while provisional, so an opted-in row must
  pin explicitly.

`parse_globals()` enforces, in order (`src/data/supplemental_inputs.R`):

1. A row may opt in only if it sets `states`.
2. Rows that do not opt in have every producer pin blanked (as State-Weights
   is on federal-only rows), so the run neither records nor requires the
   producer directory.
3. A row that opts in without any producer pin stops.
4. Every state-mode counterfactual uses the baseline row's variable set and
   producer pins: the data is a baseline attribute, and a counterfactual
   carrying different data would score data as policy.
5. Each requested name is in the schema and is published by exactly one
   pinned producer, with matching key and type; and each pinned producer was
   built on the Tax-Data vintage the row reads.

## 5. Tax-Data vintage equivalence

Pin checks compare vintage names. `config/interfaces/tax_data_equivalences.yaml`
declares pairs of names that hold the same records and amounts for listed
years; `tax_data_vintages_match()` applies it to both the State-Weights and
the supplemental-inputs checks, and requires every run year to be covered.
The first entry pairs `2026092720_filer_refit_v2` (State-Weights' fit vintage)
with `2026100117_review_fixes` (Affordability-Index's pin) for TY2019 and
TY2023. Collapse to one vintage at Tax-Data P6 and remove the entry.

## 6. Runtime

- `build_supplemental_inputs()` runs once per year inside `run_one_year()`,
  beside `build_state_weights()`. It reads, validates (year file, required
  columns, key uniqueness, orphan ids on the full sample, known states,
  type coercion, column collisions), carries the columns under a `sup.`
  prefix, and returns unit-keyed rows, state-keyed rows and a coverage table.
- `attach_supplemental_inputs()` joins them inside the per-state loop of
  `get_state_totals()`, immediately before `do_state_taxes()`, for both
  key types. Reference-law contexts therefore need nothing extra.
- `ensure_st_data()` (`do_state_taxes.R`, after `ensure_st_params()`)
  resolves each schema variable: overlay present → value with the default
  for NA and a `has_<v>` flag; native Tax-Data column present → untouched,
  flag TRUE; neither → default, flag FALSE. Stages read bare names and gate
  on `has_<v>` only where the default is not self-evidently inert.
- `st_split_spouse_unit()` halves `split: halve` variables and moves
  `split: person` pairs between slots, from the schema.
- Coverage is written once per scenario to
  `static/supplemental/supplemental_inputs_coverage.csv` (`year, variable,
  key, interface, vintage, state, n_records_covered, share_records,
  share_weighted`), by `run_sim()` and by the SLURM aggregator alike. Nothing
  is written when no overlay was live.

## 7. Encoding a provision against an overlay variable

1. Find (or add) the crosswalk row and its `overlay_variable`.
2. If the variable is new, add it to the schema with a default that leaves
   the provision inert. Add it to the manifest contract table in the producer
   note.
3. List it in the stage's `req_vars` as `(dbl) optional data; schema default
   x` and write the provision against the bare name, gated on `has_<v>` if
   the default alone would not switch it off.
4. Add the worksheet case for the provision with the variable set, and rely
   on the invariance test for the variable unset.
5. In the cross-model harness the provision stays off (no producer is pinned
   in harness runs), so no known-differences row changes -- EXCEPT for a
   variable with a `tax_data_equivalent`: the resolver reads the native
   Tax-Data column, so the harness switches the provision on the day
   Tax-Data fills it (the elective-deferral case). Pre-register that
   known-difference row with a predicate on the harness record.

## 8. Affordability-Index bridge (specification, not code)

Affordability-Index matches ACS tax units to Tax-Data records many-to-one
through `donor_id` (= Tax-Data `id`), across five implicates with weights
`hhwt × p`, and carries a state per record. A publishing step there
(suggested `R/04_publish/publish_supplemental_inputs.R`, documented as its
`docs/30_supplemental_inputs_bridge.md`) would:

- read every implicate's `files/B_m<k>_<year>.parquet`, keep filer-classed
  rows for return concepts, and weight each (unit, part, implicate) row by
  `hhwt × p / 5`;
- collapse to `(donor_id, state)` for location-dependent variables
  (`rent_paid`, `property_tax_paid`, `tenure`) and to `donor_id` for
  location-free ones (`care_exp_uncapped`); amounts as weighted means, flags
  as weighted shares where the calculator can use an expected value or as the
  weighted mode where the law needs a discrete status (`tenure`);
  `rent_paid` conditional on renter rows so the calculator gates it on
  `tenure == 2`;
- write the §2 files for 2023 only (the year it has), with a
  `dependencies.csv` naming Tax-Data `2026100117_review_fixes`.

Open on that side: whether to publish expected values or modes for flags,
and how to represent partial coverage (omit versus cell mean). Neither
changes this contract.

## 9. What this does not do

- It does not impute anything. Producers do.
- It does not let a federal run read a producer.
- It does not change any state output until a provision is encoded against
  a variable and a producer is pinned.
- Locality (county/city) is not a supplemental input; it is a second-level
  weight split (plan §2.6).

## Revision history

- **2026-10-13** — First version, with the design decisions recorded in S43:
  per-producer interface entries under one contract; variable-list opt-in;
  wide files per key type; join inside the per-state loop; reader fills
  nothing; `ensure_st_data()` resolver; `sup.` prefix; shadow whitelist;
  equivalence list for the vintage-name mismatch; coverage file.
