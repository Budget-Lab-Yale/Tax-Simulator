# Scenario configuration: the three legs

A scenario is three pointers and a year range. Each pointer names a FOLDER:

| Leg | Answers | Lives in |
|---|---|---|
| `tax_law` | what is the policy | `tax_law/` |
| `economy` | how does the world work | `economy/` |
| `behavior` | how do agents respond | `behavior/` (module code in `src/behavior/`) |

Every leg has the same shape: a complete `default/` layer, plus sparse deltas
under `alternatives/`. A runscript cell is either the reserved word `default` or
a path under that leg's `alternatives/`. Nesting under `alternatives/` is
arbitrary and the folder names are human-chosen.

A runscript names files, never values. Its schema is exactly

```
ID, tax_law, economy, behavior, years, dist_years, mtr_vars, mtr_types
```

and any other column is a parse error whose message names the replacement. The
reason is provenance: a number in a CSV cell has no citation, no record of what
it was derived under, and nothing to check it against. An entry in a folder has
all three.

### Retired columns

Every one of these hard-errors. There are no fallbacks, silent or otherwise.
`<alt>` below means `config/scenarios/economy/alternatives/<name>/`.

| Retired column | Replacement |
|---|---|
| `dep.{Interface}.vintage` | `<alt>interfaces.yaml` → `{interface}_vintage` (name lowercased, hyphens to underscores) |
| `dep.{Interface}.ID` | `<alt>interfaces.yaml` → `{interface}_id` |
| `s` | `<alt>wealth.yaml` → `financing_profile: flat:<s>` |
| `wealth_financing` | `<alt>wealth.yaml` → `financing_profile` (a profile folder name, or `none`) |
| `assumptions` | the `economy` and `behavior` columns |
| `assumption.{channel}.{name}` | an entry in an economy alternative, or — if only a behavior module reads it — the module's own file |
| `behavior` holding module paths | a behavior alternative folder whose `behavior.yaml` lists them |
| `excess_growth`, `excess_growth_start_year`, `excess_growth_all_rev` | nothing: the machinery was removed from the model |
| `corp_incidence_phasein` | nothing: it was never read |
| `first_year`, `last_year` | the `years` column, as `{start}:{end}` |
| any dotted `{leg}.{channel}.{name}` | the same entry in an alternative folder |

To see what an old runscript would become:

```bash
python3 other/migrations/migrate_runscripts.py --check <runscript>
```

## Where a number goes

Three questions, in order.

1. **Does one behavior module read it, and nothing else?** Then it lives in that
   module's file, hardcoded, with its citation beside it. No exceptions — this is
   the rule that keeps modules drop-in. A variant is a separate file
   (`charity/50.R` beside `charity/100.R`), never a config override.
2. **Is it a number the capital-gains machinery calibrates, or a switch such a
   calibration was conditioned on?** Then it lives in `config/calibrations/`,
   written by the calibrator itself.
3. **Otherwise** it is economy-leg: a description of how the world works, or a
   mechanical rule for how a shock reaches records.

Numerical plumbing (tolerances, epsilons), structural bounds (age topcodes) and
operational toggles are none of the above and stay in code.

## The economy leg

`economy/default/` holds one file per channel: `interfaces.yaml` (the data
vintages), `corp.yaml`, `distribution.yaml`, `estate.yaml`, `wealth.yaml`. An
alternative supplies the same file names with only the entries it changes.

Override granularity is the whole named entry. An alternative's entry replaces
value, kind and provenance together; nothing merges within an entry, so a
scenario can never inherit half of a justification. `locked: true` entries — the
estate valuation bridge — refuse override outright.

Values are read at the point of use, through `economy_param('kg', 'eta')`, never
captured at source time, because they are scenario-scoped.

Each file declares a `_channel` role. A `transmission` channel is
conventional-side only, and reading one on the static pass is an error — which is
what makes "static results are law-only" a machine-checked property rather than a
convention. `state` channels are readable on both passes.

**Every entry declares a `kind`, and each kind owes different provenance:**

| kind | meaning | required fields | can go stale |
|---|---|---|---|
| `calibrated` | output of a procedure | `set`, `target`, `derived_under`, `invalidated_by`, `rederive` | yes |
| `sourced` | from a paper or convention | `citation` | no |
| `judgment` | someone chose it | `note` | no |
| `structural` | a model-form switch | `note` | no |

Only `calibrated` values can go stale, because only they have inputs.

## Staleness

A stale value hard-stops the run. The check runs once, at parse time, which is
also what covers the SLURM path. Three arms:

- the data vintages in `derived_under` no longer match the run's,
- a file in `invalidated_by` has changed content since the value was pinned,
- a `conditioned_on` configuration value has moved.

Three legitimate ways past a stop, all of them visible in the output: re-derive
and re-pin, override the value in an alternative, or put a dated
`waiver: {date, reason}` block on the entry in the POINTING alternative file.
Waivers print a banner and land in the run manifest.

`active_when` marks an entry the live configuration does not read.

## The behavior leg

One `behavior.yaml` per folder, two sections. The default is no response at all:

```yaml
kg_dynamics: none
modules: []
```

An alternative replaces both sections wholesale:

```yaml
kg_dynamics:
  bathtub:    config/calibrations/kg/bathtub.yaml
  conversion: config/calibrations/kg/conversion.yaml
modules:
  - src/behavior/conversion/sigma.R
  - src/behavior/evasion/debacker.R
  - src/behavior/charity/50.R
  - src/behavior/estate/avoidance.R
```

`modules` is a bare list of paths. **There is no registry and no list of known
names**: the loader takes any path that exists, sources it, and calls
`do_{family}` where family is the parent folder name. Adding a behavior is
writing one file and listing it. Closing that interface is what killed the
previous attempt at this redesign.

`kg_dynamics` is `none` or the pieces of the gains machinery the scenario binds,
each pointed at the calibration file carrying its value. The applier is injected
automatically and listing it by hand is an error.

**Execution order is not the listed order.** The loader stable-sorts against one
pinned family order, declared in `src/sim/behavior.R`, because later families
read what earlier ones wrote:

```
kg_dynamics -> conversion -> entity_shifting -> evasion -> wealth -> charity -> estate
```

Families outside that list are order-insensitive and run last, in the order
listed. Nothing is ever rejected for being unfamiliar.

`behavior_validate_spec()` stops the run before it starts on: a listed path that
does not exist, a duplicate, a bound piece with no module or a module with an
unbound piece, conversion without the bathtub, wealth without estate, or the
applier listed by hand. Evasion without an estate module warns loudly.

## Sweeps

A sweep is a family of generated folders, so it is recorded in the vintage rather
than vanishing with the shell that set it. Whatever writes the runscript rows
writes the folders too;
`other/kg_model_tests/form_ab/write_bathtub_sweep.py` is the worked example.
There are no environment-variable back doors — the nineteen that used to exist
are gone.

One trap: **a generated file must keep the BASE NAME of the file it stands in
for** (`.../sweeps/eta_logs_15/bathtub.yaml`, never `.../eta_logs_15.yaml`).
Entries are labelled `{file stem}.{entry}`, so renaming the file relabels every
entry and silently detaches any waiver written against it.

## What each run records

Every vintage carries a manifest at its root: `scenarios.csv` (the leg pointers),
`scenario_config.csv` (every resolved value, its kind and role, and whether the
scenario overrode it), `calibrations.csv` (every calibration value in use and how
it was reached), `behavioral_assumptions.csv` (the resolved module stack, not
just the cell), `dependencies.csv` and `code_version.csv`. A past run can be read
back without the code that produced it.

## Related

- `config/calibrations/` — machine-written values and their provenance. Never
  hand-edited, and never round-tripped through a YAML library: in those files the
  comments ARE the record of where each number came from. The two exceptions are
  `kg/settings.yaml` and `estate/settings.yaml`, which are person-owned.
- `src/misc/scenario_config.R` — the resolution engine for the economy and
  behavior legs.
- `src/data/tax_law.R` — tax law keeps its own subparameter-replacement parser,
  reached through `tax_law_path()`.
- `src/sim/behavior.R` — the behavior loader, the pinned family order, and the
  parse-time checks.
- `config/runscripts/archive/` — runscripts frozen on the pre-2026-07-26 schema.
  The parser rejects them, which is correct; that folder's README gives the
  revival recipe.
- `other/config_redesign/REBUILD_STATUS.md` — how this layout came to be, and
  what is still owed.
