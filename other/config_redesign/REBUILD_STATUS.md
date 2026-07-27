# Config rebuild v2 — full status

*Branch `config-rebuild`, off `wealth` at `324a7cd38`. **All six phases are built
and the verification is complete**, as of 2026-07-26 at `a15e24db9`. All three legs
are live: a runscript row is an ID, three folder pointers and the computational
scope, and nothing else. The kg calibrations live in config/calibrations/ and are
checked at parse time against the data, the code and the settings they were derived
under. Two of the four calibrators write their own values; the other two are blocked
on a full-sample run rather than on code, and one of those is deliberately deferred.
What is left is not code: the branch merge, those two calibrators, and the author's
repo-wide comment cleanup. Plan of record:
`~/.claude/plans/cheerful-zooming-star.md`, which carries a status header pointing
back here.*

This document is meant to be enough to resume cold. It is in seven parts: what
the branch contains now, what was verified and how, the decisions taken without
the author present, and what remains. Parts 5 through 7 cover the last session:
the eta sweep and the first calibrator to write its own value; the wealth profiles,
the estate chain and the retirement of the superseded staleness mechanisms; then
Phase 6, the closing gate, and the two verification gaps that turned out to want
rewriting rather than running.

---

# Part 1 — What the branch contains

## Commits

| Commit | Phase | Scope |
|---|---|---|
| `5a12e07d8` | 0 | Gate harness + migration tools recovered from tag `cfg-p3b`. 8 files, no model code. |
| `66c37a14c` | 1 | Excess-growth machinery removed. 20 files, −240 net lines. |
| `28ecf4f33` | 2 | `src/misc/scenario_config.R` (847 lines) landed, wired to nothing. |
| `09c7e54f4` | 3a | Tax law reshaped to `default/` + `alternatives/`. 4074 files (almost all pure renames). |
| `749a5a97a` | 3b | Economy leg live; assumptions layer deleted. 65 files, −234 net lines. |
| `1f87c583f` | 3b fix | SLURM Phase 0 activates the legs; smoke fixture waivers corrected. |
| `cbc0030d9` | 3b fix | kg channel reclassified `state`. |
| `7c30eb26a` | 3c | Three runscript families migrated, 71 archived, generators taught the new schema. |
| `180fbd3a2` | 3c | Generators taught the on-model corporate rate and the cap lift; tax_law trees reproduce. |
| `6891c48de` | 4.1 | Behavior module code moves `config/scenarios/behavior/` → `src/behavior/`. 22 renames. |
| `2014f7f31` | 4.2 | The behavior leg goes live: behavior.yaml loader, 19 alternatives, five order guards deleted. |
| `60038ed6c` | 4.3 | Nine module-only parameters inlined into their modules; `evasion.yaml` deleted. |
| `efc2f79f8` | 4 gate | Six scenarios byte-identical; the Phase 4 re-pins confirmed. |
| `e6f8955a5` | 5a | The four kg calibration files land, dormant, value-verified against the old copies. |
| `dcd7b21ce` | 5b | They go live; `kg.yaml`/`sigma.yaml` deleted; parse-time calibration check + negative test. |
| `160cd5f9f` | 5b fix | Entity shifting's numbers go back in its module (author ruling). |
| `ba26fa5be` | 5 gate | Six scenarios byte-identical; all three conditional re-pins confirmed. |
| `b4479729b` | 5c | The eta sweep becomes three files instead of three shell variables. |
| `ab5b85e9b` | 5c | An sbatch wrapper for the eta-dial launcher. |
| `181884ea4` | 5c | The eta_logs calibrator ends by writing its own entry; the writer and its 28 tests. |
| `161ec866f` | 5c gate | The eta spot check: the pinned value survives re-simulation. |
| `b58faeefe` | 5d | Wealth profiles get provenance files; their generator's output path fixed. |
| `1734452ce` | 5d | The estate fitting script writes its own two numbers; hand-written intermediate deleted. |
| `7a6ad2df5` | 5e | Four superseded staleness mechanisms retired; `config_repin_hashes()` rewritten rather than deleted. |
| `aa9915fc2` | 5 gate | Six scenarios byte-identical; the Part 6 re-pins stand. |
| `2c4313ce3` | 6 | The scenario README, the postmortem's closing note, and the interface-existence check in the parse gate. |
| `a15e24db9` | 6 | `mapping_check.py` rewritten for the design that shipped, and called by the gate comparator. |
| `823f8fd93`, `88a2e882c`, `c2ed2840d`, `8f9276792` | — | This document. |

## Phase 1 — what the excess-growth removal touched

Nothing live used it. Every surviving runscript left the offset at a flat
`income_factor` of 1, so the whole path was an identity transform threaded
through the parser, the workers, aggregation, and receipts.

Removed:
- `src/data/economy.R`: `get_excess_growth_offset()`, `do_excess_growth()`, and
  the `awi` adjustment inside `generate_indexes()` (whose signature loses one
  argument).
- `src/misc/config_parser.R`: the three default columns
  (`excess_growth`, `excess_growth_start_year`, `excess_growth_all_rev`) and
  the `scenario_info` fields they fed.
- The `excess_growth_offset` argument on `run_sim()`, `run_one_year()`,
  `run_bathtub_pass()`, `run_frozen_pass()`, `run_wealth_bathtub_pass()`,
  `write_pass_outputs()`, `kg_dyn_check_run_compat()`,
  `wealth_dyn_check_run_compat()`, `corp_check_run_compat()`,
  `check_raw_data_channel_compat()`, and every SLURM driver that passed it.
- The `supplemental/excess_growth_offset.csv` write in `run.R` and the matching
  read in `calc_receipts()` (`src/data/post_processing/revenue.R`), including
  the all-revenue scaling block.
- The growth arm of the raw-dollar channel guard in `src/sim/channels.R`.
- `config/runscripts/tests/excess_growth.csv` and
  `config/runscripts/tests/growth_exercise/`.

## Phase 2 — the engine, and how it differs from the recovered version

`src/misc/scenario_config.R` came from tag `cfg-p3b`. Changes made:

- Folder shape. The abandoned branch used `{leg}/sets/<name>`; this one uses
  `{leg}/default/` and `{leg}/alternatives/<path>`. One helper,
  `config_leg_path()`, is the only place that knows the shape.
- `default` is a reserved runscript cell value. A folder literally named
  `default` under `alternatives/` could never be selected, so
  `config_load_defaults()` refuses to load a leg that has one.
- `config_resolve()`'s third argument is `alternative`, not `set_name`; the
  manifest and error messages say `alternative:<name>`, not `set:<name>`.
- The behavior leg's active-response manifest reading is gone, along with
  `behavior_active_responses()`. That leg gets its own loader in Phase 4.
- New: dated waivers on alternatives — see decision 2 below.

Unit tests: `other/config_redesign/test_scenario_config.R`, 45 checks, run via
`sbatch other/config_redesign/run_tests.sbatch`. They build a synthetic leg tree
in a temp dir and swap `CONFIG_LEG_ROOTS`, so they need no real config.

## Phase 3a — the tax law move

`config/scenarios/tax_law/baseline/` → `default/`. Everything else
(`public/`, `private/`, `tests/`, `top_tax/`, `clausing/`, `clausing_2026/`, the
three `baseline_2024*` folders) → under `alternatives/`, names unchanged.

- `src/data/tax_law.R` gained `TAX_LAW_ROOT` and `tax_law_path()`; the two
  places that built a tax law path by hand now call it.
- 136 live runscripts had their `tax_law` cell changed from `baseline` to
  `default`. Archived runscripts were deliberately left alone.
- Referencers updated: `other/estate_tax/verify_estate_law_paths.R`,
  `other/estate_tax/write_frozen_params.R`,
  `other/tips_ot_index_fix/unit_check.R`,
  `other/param_generator/generate_params.R`, and the three top_tax generators'
  path constants.
- Verified no change needed: `corp_rate_read_series()`
  (`src/sim/corp_rate.R:70`) reads `supplemental/tax_law.csv` out of the OUTPUT
  tree, not config.

## Phase 3b — the economy leg

### The new config tree

```
config/
├── output_roots.yaml                    (moved up from config/interfaces/)
├── interfaces/interface_versions.yaml   (type + version only; vintages moved)
├── calibrations/
│   ├── estate/bridge.yaml               (was config/estate/estate_valuation_params.yaml)
│   └── wealth_profiles/                 (was config/wealth/profiles/)
│       ├── default/{s.csv, M.csv}
│       ├── example_age_wealth/
│       └── s1_uniform/
├── runscripts/
└── scenarios/
    ├── tax_law/{default, alternatives}/
    ├── economy/
    │   ├── default/  corp.yaml distribution.yaml estate.yaml evasion.yaml
    │   │             interfaces.yaml kg.yaml sigma.yaml wealth.yaml
    │   └── alternatives/  the three gate fixtures' folders, plus the nine
    │                      Phase 3c added (see below)
    └── behavior/
        ├── default/placeholder.yaml     (Phase 4 replaces with behavior.yaml)
        ├── alternatives/                (empty)
        └── {the module folders, unmoved until Phase 4}
```

Deleted outright: `src/misc/assumptions.R`, `config/assumptions/` (7 files),
`config/wealth/wealth_financing_params.yaml`,
`config/wealth/wealth_transition_uniform.rds`, `config/batch-submissions/`.

### Channel roles

Each economy channel file declares `_channel: {role: state|transmission}`.
Transmission channels are conventional-side only; reading one on the static pass
is a hard error, which is how "static results are law-only" became
machine-checked. Current assignment:

| Channel | Role | Why |
|---|---|---|
| `interfaces` | state | data vintages, read everywhere |
| `distribution` | state | reporting-table conventions |
| `estate` | state | the valuation bridge measures data, both passes |
| `kg` | state | see decision 7 — the mechanical injection is static-side |
| `corp` | transmission | corporate incidence is conventional-only |
| `sigma` | transmission | conversion runs through a behavior module |
| `evasion` | transmission | ditto |
| `wealth` | transmission | except `n_pctiles`, overridden to `state` |

`config_set_pass()` is called at the top of each pass block inside
`run_one_year()` (`src/sim/run.R:926` static, `:1082` conventional), with an
`on.exit()` clearing it so no label leaks across year-tasks. Pre-passes run
untagged (`NA`), which permits any read.

### The parser

`src/misc/config_parser.R` was taken wholesale from `cfg-p3b` and adapted.
Key facts:

- `RUNSCRIPT_FIXED_COLS` is the eight-column whitelist. Anything else is fatal,
  and `validate_runscript_columns()` collects every violation before stopping,
  each mapped to its replacement via `RUNSCRIPT_RETIRED` / `describe_retired()`.
  `excess_growth*` maps to "nothing — the machinery was removed".
- Only `ID`, `tax_law` and `years` are *required*. A runscript with no `economy`
  column resolves that leg to `default`, which is why 92 of the live runscripts
  parse unchanged.
- `resolve_all_scenarios()` runs per row at parse time: resolve the economy leg,
  build interface paths from the resolved vintages, run
  `config_check_staleness()`. **This is what finally covers SLURM** — only
  `main.R`-path runs ever checked staleness before, because the old check lived
  in `do_scenario()`.
- `get_scenario_info()` is a pure lookup with no filesystem side effects. The
  directory creation it used to do is now `ensure_scenario_dirs()`, called by
  `do_scenario()` (`src/sim/run.R:31`) and `src/slurm/setup.R:111`.
- `scenario_info` gained `resolved_economy` and `resolved_behavior`; it lost
  `excess_growth*`, `s`, `wealth_financing`, `assumptions`,
  `assumption_vintages`.
- Manifest: `assumptions.csv` is replaced by `scenario_config.csv` (every
  resolved value across both legs, with kind, role, override flag and source)
  plus `scenarios.csv` (the leg pointers per scenario). `dependencies.csv` is
  now derived from the resolved interfaces channel. `behavioral_assumptions.csv`
  and `code_version.csv` are unchanged.

### Value reads

Every `assumption(channel, name)` became `economy_param(channel, name)` — 22
files, including the four behavior modules, which see it through the caller
environment and did not have to move. `grep 'assumption('` is empty.

Activation sites (`config_activate(economy=…, behavior=…)`):
`src/sim/run.R:35`, `src/slurm/worker.R:57`, `src/slurm/frozen.R`,
`src/slurm/bathtub.R`, `src/slurm/wealth.R`, `src/slurm/aggregate.R` (both
phases), and `src/slurm/setup.R` twice (decision 8).

### The wealth financing profile

The `s` and `wealth_financing` columns collapsed into one economy value,
`economy.wealth.financing_profile`, taking one of three forms:

| Value | Meaning |
|---|---|
| a folder name | that profile under `config/calibrations/wealth_profiles/` |
| `none` / `off` | channel forced off |
| `flat:<number>` | flat share, identity transition — the old scalar `s` |

`wealth_dyn_profile_spec()` reads it. `WEALTH_PROFILES_ROOT` was repointed.
`wealth_dyn_load_params()` now builds its list from `economy_param()` reads of
`n_pctiles`, `fmax`, `r_total_additive_delta`, and `wealth_dyn_params_path()` is
gone.

## Phase 3c — the runscript library

The author narrowed the plan's liveness list on 2026-07-26: the only runscripts
worth migrating are the OBBBA retrospective, the top-tax batches, and the most
recent Kim Clausing runs.

### Migrated (11 files)

| File | Note |
|---|---|
| `clausing_v2.csv`, `_s25.csv`, `_s75.csv` | the 2026-07-06 re-run, most recent of the six Clausing scripts |
| `top_tax/dials.csv`, `factorial.csv` | generated; see below |
| `top_tax/carry_ab.csv`, `eta_dial.csv`, `eta_dial_ref.csv`, `eta_dial_repin.csv` | hand-written |
| `obbba_retrospective/stack_vs_2024.csv`, `stack_smoke.csv` | pulled back out of `archive/public/obbba/retrospective_2026/` and renamed. Their `tax_law: baseline` cells also needed `default`, since 3a deliberately left the archive on the old schema |

### The economy alternatives they need

Their retired columns were two orthogonal dimensions — an Off-Model-Estimates
pin and a saving share — and an alternative is a whole folder, so these are the
cross product actually used:

| Folder | OME pin | Saving share |
|---|---|---|
| `ome_20250925` | 20250925 / baseline | default profile |
| `ome_20250925_saving_0` | " | `flat:0` |
| `ome_20250925_saving_25` | " | `flat:0.25` |
| `ome_20250925_saving_50` | " | `flat:0.5` |
| `ome_20250925_saving_75` | " | `flat:0.75` |
| `ome_20260706_corporate_saving_25` | 20260706 / 07_corporate | `flat:0.25` |
| `ome_20260706_corporate_saving_50` | " | `flat:0.5` |
| `ome_20260706_corporate_saving_75` | " | `flat:0.75` |
| `ome_top_tax_corp_placeholder` | top_tax_corp_placeholder / corp_28_2027 | default profile |
| `no_saving_channel` | model default | `none` |

The plan says a human names each alternative folder. These names were proposed,
not authored — see decision 9. Renaming one is a folder rename plus the matching
runscript cells (and, for the two the generators use, the two constants in
`other/top_tax/levers.py`).

### The legend files

`config/runscripts/top_tax/{dials,factorial}_legend.csv` were never runscripts
and would fail the schema check for that reason. Both moved to
`other/top_tax/`, with five readers repointed: `build_dial_runs.py`,
`build_factorial.py`, `fit_surrogate.py`, `extract_atlas_data.py`, and
`report_prep/output_data_map.md`. Their line endings are CRLF, matching their
writer, and were deliberately left alone.

### The generators

`build_dial_runs.py` and `build_factorial.py` now emit the eight-column schema
(one `economy` cell in place of the three retired columns) and reproduce their
runscripts EXACTLY, proven by regenerate-and-diff. Three separate pieces of
drift had to be fixed to get there:

1. **Line endings.** `csv.DictWriter` defaults to CRLF; every committed
   runscript is LF. Regenerating rewrote all 199 and 128 lines for nothing, so
   no diff of a generated batch was ever readable. Both writers now pass
   `lineterminator="\n"`.
2. **The estate-avoidance split.** The estate reporting response became its own
   behavior module on 2026-07-16 and the 349 shipped runscript rows were patched
   by hand; the generators were not. Regenerating silently dropped
   `estate/avoidance` from the behavior list and `estate` from `mtr_vars`.
   Ported into `BEHAVIOR_BASE`, `BEHAVIOR_WEALTH` and `MTR_VARS` in both
   `other/top_tax/levers.py` and `build_factorial.py`.
3. **Tax law paths Phase 3a missed.** 3a rewrote string-literal paths; these
   generators build theirs with `os.path.join`, so `TAXLAW_ROOT` in both scripts
   and the `baseline/` reads in `build_dial_runs.py` and `levers.py` were still
   pointing at the pre-move tree.

`levers.py`'s `CORP_ON_VINTAGE` / `CORP_ON_ID` / `CORP_OFF_VINTAGE` /
`CORP_OFF_ID` / `S_COL` are replaced by `CORP_ON_ECONOMY` /
`CORP_OFF_ECONOMY`; `build_factorial.py` had its own copies of the same five and
they went the same way. The `baseline` row's `tax_law` cell now emits `default`,
and both scripts' header lists put `economy` in canonical position.

A fourth drift was then fixed as well, so the generators now reproduce their
tax_law trees too:

4. **The on-model corporate rate and the ordinary-rate cap lift.** The corporate
   rate went on-model on 2026-07-23; before that a corp scenario was purely an
   off-model switch and wrote no YAML, which both scripts still believed. The
   114 `corp.yaml` files the change needs, and `no_ord_cap: 1` in 123
   `pref.yaml` files, had been patched in by hand. The rules turned out to be
   exact, with zero exceptions across all 198 dial and 128 factorial scenarios:
   `corp.yaml` exists if and only if the corp lever is on, and `no_ord_cap`
   appears if and only if the cg lever is on. Both are now generated. The corp
   lever's `kind` becomes `'both'` (it writes YAML *and* names the off-model
   pin) and the YAML gate keys on `files is None` rather than on `kind`;
   `no_ord_cap` is appended to `pref.yaml` last, which is where the hand patch
   left it and where it has to stay for regenerate-and-diff to be clean.

**Both generators now reproduce everything they write.** The check is one
command, and an empty result is the pass:

```bash
python3 other/top_tax/build_dial_runs.py && python3 other/top_tax/build_factorial.py \
  && git status --short config/
```

### What was archived

The 71 runscripts that were not migrated moved to
`config/runscripts/archive/retired_2026_07_26/`, frozen on the old schema. The
archive README gained a table naming the ones most likely to be wanted back —
the refactor byte-diff harness, the kg regression pair, the sigma recalibration
stack, the corporate smokes, the performance probe, and the wealth bounding set
— plus the two-step revival recipe.

## Phase 4 — the behavior leg

Three commits, in the order they had to happen.

### 4.1 — the module files move

`config/scenarios/behavior/{family}/*.R` → `src/behavior/{family}/*.R`, 22 files,
every one a pure rename. config/ stops containing executable code.

They are still loaded BY PATH at scenario time, never sourced at startup, and
that distinction is load-bearing: `charity/50.R` and `charity/100.R` both define
`do_charity()`, so sourcing the folder would leave whichever came last in scope.
`main.R`, `src/slurm/setup.R` and `src/slurm/common.R` all skip `behavior/` when
they walk src/ recursively — three predicates that have to stay in lockstep, as
CLAUDE.md already says of the other two exclusions.

Path references followed the files: the legacy entity-shifting selector's
`sys.source`, the dependency lists in the economy leg's `sigma.yaml` and
`evasion.yaml`, one comment in `calc_estate`, and three harnesses under `other/`.
The four pinned md5 hashes in `sigma.yaml` were re-checked and unchanged, which
is the whole point of doing the move on its own: it touched locations, not
contents.

### 4.2 — the leg goes live

`behavior.yaml` in a folder the runscript names. Two sections:

```yaml
kg_dynamics: [bathtub, conversion, entity_shifting]
modules:
  - src/behavior/conversion/sigma.R
  - src/behavior/entity_shifting/pearce_prisinzano.R
  - src/behavior/evasion/debacker.R
  - src/behavior/charity/50.R
  - src/behavior/estate/avoidance.R
```

`modules` is a bare list of paths. There is no registry and no known-names list:
the loader takes any path that exists, sources it, and calls `do_{family}` where
family is the parent folder name. Closing that interface is what killed the
previous attempt at this redesign, and it has not been reintroduced.

`kg_dynamics` is `none` or the pieces of the gains machinery the scenario binds.
It accepts two written forms and treats them identically downstream: a LIST of
piece names, meaning the pieces are bound and their parameters are still wherever
they live today, or a MAPPING of piece to its stamped calibration file. Phase 5
turns the lists into mappings; the loader already handles both, so that is a
config edit with no code behind it. The bathtub applier
(`src/behavior/kg_dynamics/turnover.R`) is injected whenever the machinery is on
and listing it by hand is an error — binding the machinery and running its
applier were never two decisions.

**Order.** Execution order is not the listed order. The loader stable-sorts
against one pinned family order, declared once in `src/sim/behavior.R`:

    kg_dynamics → conversion → entity_shifting → evasion → wealth → charity → estate

Families outside it are order-insensitive and run last, in the order listed.
Nothing is ever rejected for being unfamiliar — unranked is not unwelcome.

That one sort replaced five hand-written guards, each enforcing a different
subset of the same rule from inside a module, an hour into the run:

| Deleted | What it enforced | Now |
|---|---|---|
| `conversion/sigma.R` guards 1–2 | kg required; pinned order | parse-time check + the sort |
| `entity_shifting/pearce_prisinzano.R` | pinned order | the sort |
| `evasion/debacker.R` | warn if no estate module | parse-time warning |
| `wealth/avoidance.R` × 3 | evasion before, kg before, estate after | the sort + parse-time check |
| `estate/avoidance.R` | upstream families before | the sort |
| `run.R` early stop | conversion without kg | parse-time check |

The new parse-time checks (`behavior_validate_spec`) run on every scenario in the
runscript before the run starts: a bound piece with no module and a module with
an unbound piece both stop, wealth without estate stops, evasion without estate
warns loudly, a listed path that does not exist stops, listing the applier stops,
and the error message prints the order the stack would have run in.

**`charity` had to join the pinned order.** In the top-tax stacks charity sits
between wealth and estate, and leaving it unranked would have moved it after
estate. This is the only judgement call in the order and it is verified rather
than asserted — see Part 2.

Nineteen behavior alternatives cover every distinct stack in the live runscripts,
and 33 runscripts had their behavior cell rewritten to name one. The two top-tax
generators emit folder names and still reproduce their runscripts and reform trees
byte-for-byte.

Also in this commit: the activation predicates key on the kg binding rather than
on a module prefix appearing in a list (`scenario_uses_kg_dynamics`,
`scenario_uses_sigma`); `behavioral_assumptions.csv` records the resolved stack,
not just the cell, so an old vintage still says what actually ran; the behavior
leg is permitted to carry no value entries, which is now its normal state; and
`check_runscripts.R` resolves and validates the behavior leg too, skipping
`private/` for the same reason it skips `archive/`.

### 4.3 — the module-only parameters come home

Nine values had exactly one reader each, and that reader was a behavior module:

| Was | Now |
|---|---|
| `evasion.e_schc`, `e_pt`, `e_rent`, `topend_mult` | `src/behavior/evasion/debacker.R` |
| `estate.report_eps` | `src/behavior/estate/avoidance.R` |
| `wealth.avoid_public_e`, `avoid_private_e`, `chi_pub`, `chi_priv` | `src/behavior/wealth/avoidance.R` |

`economy/default/evasion.yaml` is deleted; every entry in it was module-only.
`estate.yaml` keeps only the locked valuation bridge. `wealth.yaml` keeps the
three structural bathtub knobs, the financing profile, and
`cap_flows_pt_weight`, which the corporate channel also reads.

The citations moved with the values, not a pointer to them: the DHY source for
each evasion elasticity with its alternative anchors, the Kopczuk-Slemrod band
and the two accepted caveats, and the honest statement that the two wealth
elasticities are author-accepted rather than calibrated — the private one being
the largest single behavioral magnitude in the model.

A variant is now a copy of the module file with different numbers, listed by a
different alternative. There is no config cell and no environment variable to
override one silently, which is the property the whole rebuild is for.

Sixteen comments across `corp_alloc.R`, `corp_rate.R`, `corp/paths.R`,
`wealth_dynamics.R` and `kg/constants.R` still pointed at `config/assumptions/`,
which Phase 3b deleted. Fixed here, since these files were open and the re-pin
covers them.

### The conditional hash re-pins

Deleting the guards and moving the parameters changed files that calibrated
entries are pinned against. Ten hashes were re-pinned across the two commits —
`pearce_prisinzano.R`, `debacker.R` and `sigma_conversion.R` for `sigma.conv`,
`kg/constants.R` for the four kg entries, `wealth_dynamics.R` for
`financing_profile`.

Every underlying edit is a deleted guard, a moved value keeping its number, or
comment text. Each touched file carries a dated note saying exactly that, and
saying that **the re-pin stands only if the six-scenario gate is byte-identical
and becomes a real re-derivation if it is not.** That is the order CLAUDE.md
requires and the same pattern Phase 3b used (decision 1).

The re-pins were done by text substitution on the hash lines. `config_repin_hashes()`
round-trips the YAML through `write_yaml()` and would delete every comment in
files that are mostly comment, the comments being the provenance. **Still do not
call it.** Phase 5 retires it.

## Phase 5, first half — the calibrations move and get checked

Two commits plus a correction. The values did not change; where they live, and
whether anything notices when they go stale, did.

### The files

```
config/calibrations/kg/
├── bathtub.yaml      GENERATED. eta, eta_logs, timeable_share,
│                     timeable_share_logs -- the four calibrated response
│                     parameters
├── conversion.yaml   GENERATED. sigma, plus the SYZZ labor-content share
└── settings.yaml     HAND-EDITABLE. the model-form switches and judgment calls:
                      response_form, the two allocation rules, the timing window
                      and reference wedge, wealth_carry_scale, beta_fallback,
                      deemed_avoidance, the four char_* logits and their base year
```

The kg channel split along a line nobody had drawn before: numbers a PROCEDURE
produces, in a file a calibrator writes, versus choices a PERSON made, in a file a
person owns. Only the first kind can be stale, and conflating them is why three
provenance ledgers in this repo disagreed with each other.

`config/scenarios/economy/default/{kg,sigma}.yaml` are deleted.

### Bound versus fixed

The distinction that makes a sweep recordable:

| | Reached how | For | Read by |
|---|---|---|---|
| BOUND | the scenario's behavior leg names the file, via `kg_dynamics` | values a scenario may legitimately differ on | `kg_bathtub()`, `kg_conversion()` |
| FIXED | one path, every scenario | the switches the calibrations are conditioned on, which cannot vary underneath them | `kg_setting()` |

Both accessors are fail-closed: reading a bound value in a scenario that never
bound the file is an error, because the alternative is a number nobody chose.

This is what an eta sweep becomes. Today it is an environment variable
(`KG_ETA_LOGS=1.9`) that leaves no trace in the vintage and, on this branch, does
not work at all -- see Part 4. Tomorrow it is a generated file bound by its own
behavior alternative, and therefore recorded.

**A sweep file must keep the BASE NAME of the file it stands in for** --
`.../sweeps/eta_15/bathtub.yaml`, never `.../eta_15.yaml`. Entries are labelled
`{file stem}.{entry}`, so renaming the file renames every label and any waiver
written against `bathtub.*` silently stops applying. Found by the negative test,
now documented in `calibrations.R` and asserted in that test.

### The conditioning set became machine-readable

This is the part with teeth. The kg calibrations were derived under particular
settings, and that fact used to be prose inside a note -- "conditioned on
applier_allocation = 0.5, timing_ref_wedge = 0.05, timing_window = 1" -- which
cost nothing to violate. Each calibrated entry now carries a `conditioned_on`
block naming those settings and their values, and the parse-time check compares
the block against the live `settings.yaml`. Change a switch and every
capital-gains run stops until the affected value is re-derived or waived.

`conditioned_on` keys gained `settings` alongside `economy` and `behavior` as a
valid source.

### One staleness implementation, not two

A calibration file is handed to `config_check_staleness()` as the degenerate case
of a leg: one channel, named for the file, nothing overridden. So the three arms --
data vintages, dependency file contents, conditioned-on values -- are the same code
the economy leg runs, and there is no second implementation to drift.

`calib_check_staleness()` is called from `resolve_all_scenarios()`, next to the
economy leg's check, for the same reason: parse time is the one place both the
`main.R` path and the SLURM path pass through.

### Waivers live in the pointing file

`behavior.yaml` gains an optional `waivers` section keyed `{file stem}.{entry}`.
The reasoning: a calibration file is rewritten by its calibrator, so a waiver
inside it would be erased by the next re-derivation. That is RIGHT for a waiver the
re-derivation resolves, and WRONG for one that says "this scenario knowingly runs
against an older data vintage". The smoke fixture's four kg waivers moved there out
of its economy alternative, and its passing is what demonstrates the path works.

### The negative test

`other/config_redesign/test_stamp_negative.R`, 10 checks. Corrupt one thing at a
time in a scratch copy and confirm the run stops naming what moved: a data vintage,
a dependency file's contents, a conditioned-on setting. Then that a dated waiver
gets past the stop loudly, that waiving one entry does not waive its neighbours,
and that a waiver keyed to the wrong file stem does not apply.

Every other check in this suite proves a CLEAN configuration passes, which is the
less important half of the claim. A staleness check nobody has watched fail is a
check nobody should trust.

### The manifest

`calibrations.csv` at the vintage root: per scenario, every calibration value in
use, its file, its kind, and whether the scenario bound the file or read it from a
fixed path. So a past run can be read back without the code that produced it.

---

# Part 2 — What was verified

## The six-scenario gate, all passing at `cbc0030d9` (Phase 3b) and again at `60038ed6c` (Phase 4)

| | Runscript | How | Golden | 3b | Phase 4 | Phase 5 |
|---|---|---|---|---|---|---|
| S1 | `baseline/baseline` | `main.R`, pct 0.05 | `golds1` | pass | pass | pass |
| S2 | `rebate_2025` | SLURM, pct 1 | `golds2` | pass | pass | pass |
| S3 | `tests/multi_module_smoke` | SLURM, pct 1 | `golds3` | pass | pass | pass |
| S4 | `tests/corp_kgwealth_verify` | SLURM, pct 1 | `golds4` | pass | pass | pass |
| S6 | `wealth_tax`, scenario `wealth_tax_warren` | SLURM, pct 1 | `golds6` | pass | pass | pass |
| S7 | `estate_2009` | SLURM, pct 1 | `golds7` | pass | pass | pass |

Candidate vintages: `rb_p4_s{1..7}b` for Phase 4, `rb_p5b_s{1..7}` for Phase 5.

The Phase 4 column is what settles the ten conditional hash re-pins: they stand,
and the guard deletions and parameter relocation were behavior-preserving as
intended. The notes in `kg.yaml`, `sigma.yaml` and `wealth.yaml` say so and are
dated. Note this gate is stronger for Phase 4 than for 3b, because S3, S4 and S6
are exactly the scenarios that run behavior stacks — four families, the bathtub
with entity shifting, and the wealth/estate concealment pair.

The Phase 4 runs were executed from the project tree itself at `60038ed6c`, not
from a scratch worktree, so `code_version.csv` is the only thing that could
differ by construction and it is excluded.

Between them these exercise the kg bathtub, the corporate incidence channel,
the wealth bathtub, the estate tax, and the multi-module behavior stack.

Goldens live at `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/golds{1,2,3,4,6,7}`.
Candidates from this session are `rb_p3b_s{1,2,3,4,6,7}` in the same directory.
Re-compare with:

```bash
bash other/config_redesign/gate_diff.sh \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/rb_p3b_sN \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/goldsN
```

Launchers, both of which take a tree directory as their first argument:

```bash
# S1 (pct 0.05, in-process)
sbatch --job-name=TAG other/config_redesign/gate_smoke.sbatch  TREE RUNSCRIPT VINTAGE 0.05
# S2/S3/S4/S6/S7 (pct 1, submits the whole SLURM chain and returns)
sbatch --job-name=TAG other/config_redesign/gate_verify.sbatch TREE RUNSCRIPT VINTAGE 1 [SCENARIO_ID]
```

**S6 must be launched with `wealth_tax_warren` as the scenario_id.** That is how
its golden was produced; running all four wealth-tax scenarios changes the
stacked reports and the gate fails for a reason that is not a model difference.

The runs were executed from a worktree at
`/nfs/roberts/scratch/pi_nrs36/jar335/cfg_rb_p3b`, carrying the Phase 3b `src/`
and `config/` (rsynced) on a Phase 3a checkout. Its `other/` and `CLAUDE.md`
are a commit behind, which affects nothing the model reads, and
`code_version.csv` is excluded from comparison. A clean re-run from
`cbc0030d9` would be tidier if any result is ever doubted.

## The runscript parse gate

`other/config_redesign/check_runscripts.R`, new in 3c and extended in Phase 4.
For every CSV under `config/runscripts/` except `archive/` and `private/`, it
enforces the eight-column schema via `validate_runscript_columns()`, resolves each
row's economy leg and runs `config_check_staleness()`, then resolves and validates
each row's behavior leg — the folder exists, its module files exist, the stack is
a shape the model can run. That last part is what the deleted in-module order
guards used to catch, one scenario at a time, mid-run.

It deliberately stops short of `parse_globals()`, which reads Tax-Data and creates
output trees. That is a real limit and it cost something: the behavior leg having
no value entries broke `write_run_manifest()`, which is inside `parse_globals()`,
so the first Phase 4 gate launch died in setup with all six checks green. The fix
is one guard in `config_manifest()` and two unit tests that would have caught it.

```bash
sbatch other/config_redesign/run_tests.sbatch . other/config_redesign/check_runscripts.R
```

Result at `60038ed6c`: **77 parse, resolve and validate, 0 fail.** The count fell
from 103 because the 26 `private/` files are now skipped rather than passing a
weaker check.

## The behavior-order assertion

`other/config_redesign/check_behavior_order.R`, new in Phase 4, and the reason the
`charity` ordering decision is a verified claim rather than a hope. It reads the
PRE-MIGRATION behavior cells straight out of git at `6891c48de`, resolves each
one's new folder through the live loader, and compares the resulting module
sequence to the sequence that was written in the cell.

Result: **500 cells checked, 500 resolve to the identical order.** Not one
migrated runscript's stack was reordered.

Three stacks WOULD reorder, and all three are in untracked `private/` runscripts
that the rebuild migrates on demand: `private/peterson/with_behavior.csv` and
`private/wsj/ot.csv` write `charity/50` before `entity_shifting`, where the pinned
order puts it after; `private/vanhollen/surtax.csv` writes `kg/62` before
`entity_shifting`, and the simple `kg` family is unranked so it moves last. No
single family order satisfies both these and the top-tax stacks — charity is
before entity shifting in one and after it in the other — so the top-tax product
runs won. Whoever migrates those three should expect the reorder and decide
whether it moves anything.

## The behavior loader tests

`other/config_redesign/test_behavior_leg.R`, 27 checks, new in Phase 4. Builds a
synthetic behavior tree and throwaway module files in a temp dir, so the tests say
what the loader does rather than what today's stacks happen to contain. Covers
both written forms of `kg_dynamics`, the sort (pinned order, unranked-last,
stability), applier injection, and every parse-time refusal.

The engine tests (`test_scenario_config.R`) are now 47, the two added covering the
entry-less leg that broke the first gate launch.

## What the gate comparator excludes, and why each is safe

`other/config_redesign/gate_diff.sh`. Pre-existing exclusions: `.xlsx`
(compared by unzipped sheet payload instead, because the zip carries a
timestamp), `code_version.csv` (different by construction), and the manifest
family, which `mapping_check.py` is responsible for. Two added this session:

- `supplemental/excess_growth_offset.csv` is golden-only now. Before excusing
  it, the comparator asserts every golden copy has `income_factor == 1` in
  every year, so the exclusion cannot hide a real change. This assertion runs
  and reports its file count on every invocation.
- `assumptions.csv` is golden-only; the candidate writes `scenario_config.csv`
  in its place.
- `calibrations.csv` is the mirror case, candidate-only, added in Phase 5. **The
  Phase 5 gate reported all five full-sample scenarios as failures on the first
  pass for exactly this reason** -- a new file is a file-set mismatch -- and not
  one of them was a number. Excusing it is only safe if it is actually being
  written, so the comparator now ASSERTS it present with a header and at least one
  row. An exclusion that also tolerates absence would quietly cover the manifest
  disappearing, which is the failure this project exists to prevent.

## Verification NOT yet done

*Rewritten at the close of Phase 6. Items 1 and 4 are done, 2 is superseded, and
3 turned out to be wrong when written.*

1. ~~`mapping_check.py` has never been run against a 3b vintage.~~ **CLOSED**,
   but by rewriting the tool rather than running it — the recovered version
   targets the abandoned design. 495 relocated values checked across the six
   closing vintages, all equal. Part 7.
2. ~~The equivalence check has not been run.~~ **SUPERSEDED.** It targets the
   abandoned shape, and what it would prove is now proven better twice over:
   six byte-identical simulations plus the mapping check on the values
   themselves. Recommend deleting `equivalence_check.R`; see Part 7.
3. ~~The unit tests do not cover the waiver path.~~ This was wrong when written.
   `test_stamp_negative.R` covers three waiver behaviours: that a dated waiver
   gets past the stop under a banner, that waiving one entry does not waive its
   neighbours, and that a waiver keyed to the wrong file stem does not apply.
4. ~~No negative test exists yet for the staleness stop.~~ **DONE** in Phase 5:
   `test_stamp_negative.R`, 10 checks, one corruption at a time in a scratch
   copy.
5. **The three migrated runscript families have not been RUN.** Still true, and
   Part 7 says what happened when one was tried: 9 of the 80 live runscripts
   cannot run at all, because they pin Off-Model-Estimates vintages that exist
   only under v4. The parse gate now reports them. Nothing has been run from the
   OBBBA retrospective, which is the family this most matters for — its economy
   pin is the model default, so it is not affected by the OME problem, but its
   two scripts were pulled out of the archive and had their `tax_law` cells
   changed.
6. **`private/` is unmigrated and skipped by the parse check.** The 26 files
   there are untracked one-off work; their behavior cells still name module
   lists and would fail resolution. Three will reorder when migrated (see the
   order assertion above).

---

# Part 3 — Decisions taken without the author

Eleven. Numbers 1, 2 and 11 are the ones with teeth.

### 1. Twenty dependency hashes were re-pinned

Calibrated entries carry `invalidated_by` file lists and md5 hashes of those
files, so the model can tell you a number was calibrated against code that has
since changed. The `assumption()` → `economy_param()` rename changed the
content of `src/sim/kg/{constants,bellman,recurrence,timing,apply}.R`,
`src/sim/sigma_conversion.R`, `src/sim/wealth_dynamics.R`, and
`config/scenarios/behavior/evasion/debacker.R`. Repointing
`write_frozen_params.R`'s output path changed that file too. All of them appear
in some entry's `invalidated_by`, so the staleness check hard-stopped the run.

The rename is behavior-preserving. The order CLAUDE.md requires was followed:
the S1 gate confirmed byte-identical output *before* the re-pin was kept. No
calibrated value moved; `set` dates are untouched.

**The re-pin was done by surgical text substitution on the hash lines, not by
`config_repin_hashes()`.** That function round-trips the YAML through
`write_yaml()`, which would delete every comment in the file — and these files
are mostly comment, since the comments are the provenance. The hazard is still
live in the codebase. Phase 5 retires the function; until then, do not call it.

### 2. The engine gained dated waivers on alternatives

The plan called for a `waiver: {date, reason}` block in the pointing file. The
recovered engine only had `acknowledged`, which sits on a *default* entry — no
use to a scenario that needs to accept staleness for itself.

`config_resolve()` now accepts an entry in an alternative that carries a
`waiver` block and no `value`; it records it in a `waivers` list on the resolved
object. `config_check_staleness()` skips those entries and prints them under a
banner. A waiver is explicitly permitted on a `locked` entry, because waiving
accepts a finding rather than changing a value, so `locked` has nothing to
protect against — that is what lets the smoke fixture waive the estate
valuation bridge.

`config/scenarios/economy/alternatives/multi_module_smoke/` uses this. It pins
Tax-Data `2026050315` on purpose (plan ruling 11), which makes all six
calibrated entries stale, and waives each one dated 2026-07-26.

### 3. `config/batch-submissions/` deleted

It held generated SLURM scripts and their logs — run artifacts, not
configuration. `other/slurm_builder.R` now writes to
`/nfs/roberts/scratch/pi_nrs36/jar335/Tax-Simulator-batch-submissions/` via a
new `batch_root` variable. The old paths in that script were already
inconsistent (some project, some scratch); they are uniformly scratch now. The
script is not exercised by any gate, so this is untested.

### 4. The behavior leg has a placeholder default file

`config/scenarios/behavior/default/placeholder.yaml` exists so
`config_load_defaults('behavior')` has something to load while only the economy
leg is live. Nothing reads it. Phase 4 replaces it with `behavior.yaml`.

### 5. kg, sigma and evasion sit in the economy leg temporarily

Each file carries a header saying so. This is what the plan asked for — exactly
one live configuration engine per phase. Phase 4 inlines the module-only
parameters (evasion's four, estate's `report_eps`, wealth's `avoid_*`/`chi_*`)
into their module files; Phase 5 moves kg and sigma into calibration stamps.

### 6. `wealth_financing_params.yaml` absorbed as three structural entries

`n_pctiles`, `fmax`, `r_total_additive_delta` in the economy leg's
`wealth.yaml`. `n_pctiles` carries `role: state` because
`scenario_uses_wealth_dynamics()` reads it outside any tagged pass. The
`r_total.source` string is now hardcoded in `wealth_dyn_load_params()` rather
than configured — it was never anything but `macro_gdp_per_capita`, and a
one-valued setting is not a setting.

### 7. The kg channel is `state`, not `transmission`

The full-sample gate caught this; the pct-0.05 S1 run could not, because
baseline never runs the kg path. `kg_dyn_apply_mech_to_records()` runs inside
the static block by design — that is how a kg policy's mechanical content
reaches static liabilities, static MTRs and the distribution tables — so the
values it reads, including the charitable-bequest logits in
`src/sim/kg/inputs.R`, are read on the static pass. Only the bathtub's
behavioral response is conventional-only, and that runs through the modules.

The abandoned branch had split the logits into a separate `bequest.yaml` state
channel. Declaring the whole kg channel `state` reaches the same place with one
file. If a future reader wants the finer distinction back, splitting is still
available.

### 8. SLURM Phase 0 activates the legs itself

Also caught by the full-sample gate. `src/slurm/setup.R` reads configuration
before any worker starts: `build_tax_law()` in its first per-scenario loop, and
`scenario_uses_wealth_dynamics()` in the second, which decides whether the 2N
and 2W phases get emitted at all. Both loops now call `config_activate()`. The
second one matters on its own — without it, it would read whichever scenario
the first loop happened to leave installed.

### 9. The nine economy alternative folder names were proposed, not authored

The plan says the migration tool proposes which rows share a pin and a HUMAN
names each folder. The author was not available at that point in the session, so
the names in the Phase 3c table above are descriptive placeholders: for these
folders the pin genuinely is the meaning, so `ome_20250925_saving_50` says what
it does. Rename any that should read differently — it is a folder rename, the
matching runscript cells, and (for the two the generators reference) two
constants in `other/top_tax/levers.py`.

### 10. The 71 unmigrated runscripts were archived rather than left in place

The author's ruling narrowed migration scope but did not say what to do with the
rest. Archiving was chosen over leaving them because a broken file in the live
tree looks runnable, and because it keeps
`other/config_redesign/check_runscripts.R` meaningful — with them in place the
check would report 71 failures forever and nobody would read it. The plan
sanctions archiving wholesale, and it is a `git mv` to undo. The archive README
names the ones most likely to be wanted back.

### 11. The generators were taught the on-model corporate rate and the cap lift

Initially these were left as a warning, on the grounds that emitting them
changes what the scenarios mean. The author ruled to fix them, and it turned out
not to be a modelling judgment at all: both rules are exact functions of which
lever is on, with zero exceptions across all 326 scenarios in the two batches.
So the generators encode the rule rather than a guess, and both now reproduce
their tax_law trees as well as their runscripts.

What remains a judgment, and is recorded in the code: `no_ord_cap` is appended
to the END of `pref.yaml`, after the deemed block, which is not where it
logically belongs — it rides the cg lever and would read better inside the cg
block. It stays at the end because that is where the hand patch put it, and
moving it would break byte-identity against every shipped vintage for no gain.

### 12. `charity` was added to the pinned family order

The plan pinned six families; the top-tax stacks run charity between wealth and
estate, so leaving it unranked would have moved it after estate. Adding it is the
choice that reproduces every tracked runscript exactly, and the order assertion
in Part 2 is the evidence rather than the argument. The cost is that two private
runscripts, which write charity BEFORE entity shifting, will reorder when someone
migrates them — no single order satisfies both, and the product runs won.

### 13. The nineteen behavior alternative folder names were proposed, not authored

Same status as decision 9's economy folders. Most are stack-descriptive
(`kg_entity_shifting`, `employment_child_earnings`, `wealth_estate_avoidance`);
three are named for what uses them, because the stack itself is a mouthful
(`top_tax_full`, `top_tax_full_wealth`, `top_tax_no_estate`) and one keeps its
fixture name (`multi_module_smoke`). Renaming one is a folder rename, the matching
runscript cells, and — for the two the generators reference — two constants in
`other/top_tax/levers.py`.

### 14. The wealth-without-estate contract became a parse-time stop, not a warning

`wealth/avoidance.R` hard-stopped mid-run when no estate module followed it. That
contract is real — the concealment fractions it persists have to reach the
reported estate — so it stayed fatal, just moved earlier. Its two companion
ORDER guards became guarantees of the sort instead of checks, since the sort
cannot produce a violating order.

### 15. REVERSED BY THE AUTHOR -- entity shifting's numbers belong in its module

I put entity shifting's four constants in a calibration file of their own, on the
strength of a line in the 25 July plan that said to. The author reversed it on
2026-07-26, and was right: every behavioral number a pluggable module reads lives
in that module's file, with no exceptions.

The carve-out was wrong on its own terms. Evasion, estate and wealth all had their
parameters moved INTO their modules earlier the same day for exactly the reason that
applies here -- one module reads them -- so it turned one rule into a rule plus an
exception and left neighbouring modules doing the same thing two ways. It also had a
second defect: entity shifting runs in scenarios with no bathtub, so a per-scenario
binding had nothing to bind to there, and the module would have failed looking for a
number.

The numbers are back in `pearce_prisinzano.R` with their sources beside them,
`entity_shifting.yaml` and `kg_entity()` are gone, and `calibrations.R` carries a
WHAT IS NOT HERE note so the idea is not re-derived from the plan document.

**The rule, stated once:** if a pluggable behavior module reads a number, that
number is in the module's file. `config/calibrations/` holds only what the kg
machinery itself calibrates, and the switches those calibrations are conditioned on.

### 16. `kg_dynamics` accepts a list as well as a mapping

The plan describes the section as a mapping of piece to stamped file. Those files
do not exist until Phase 5, and pointing the mapping at today's homes would have
meant writing a path to a module file as though it were a calibration stamp, in
nineteen folders, to be rewritten in a fortnight. The list form says exactly what
is true now — these pieces are bound — and the loader accepts both from the
start, so Phase 5 adds paths without touching code. If a reader prefers only one
form to exist, deleting the list branch is a five-line change. (The 19 shipped
alternatives were converted to the mapping form in Phase 5b, so the list branch is
now unused by anything in the tree.)

---

# Part 4 — What is left

Phase 5's second half, and Phase 6. Nothing from Phases 0 through 4 is open, and
nothing from Phase 5's first half.

## Phase 5, second half — make the calibrators write their own files

This is the actual point of the project and it is still ahead. The inventory run for
the plan found that **not one of the five calibrated values in the model is written
by its calibration script** — all were hand-copied out of logs — and **four of five
"how to re-derive this" pointers were broken**. The files now exist, are read at the
point of use, and are checked before every run. But a human still put the numbers in
them, so the original defect is intact.

DONE, in Phase 5's first half:
- `bathtub.yaml`, `conversion.yaml`, `settings.yaml` written from the current pinned
  values with no re-derivation, and value-verified against the copies they replaced
- the conditioning set made machine-readable and enforced
- the parse-time check, the manifest, the waiver path, the negative test
- `kg.yaml` and `sigma.yaml` gone from `economy/default/`
- `entity_shifting.yaml` written, then DELETED — its numbers belong in the module
  (Part 3, decision 15)

STILL TO DO:

**(a) Rewire the calibrators.** Each ends by writing its own file, so no number is
ever hand-copied from a log again.

DONE — **the eta launcher port and the eta_logs calibrator** (`b4479729b`,
`ab5b85e9b`, `181884ea4`, `161ec866f`). Written up in Part 5 below.

STILL OWED:

- `other/top_tax/eta_dial/measure_efull_by_eta.R` → `bathtub.yaml`'s `eta` entry.
  The writer it needs already exists (`calib_write_entry`, below); what it does not
  have is a launcher, because the levels eta-dial was run by hand and never had
  one. It needs the same sweep treatment `eta_logs` just got — three generated
  `bathtub.yaml` files varying `eta`, bound by their own behavior alternatives.
  `write_eta_sweep.py` is the template and most of it generalizes.
- the sigma pipeline — two `topord_plus5` legs, the top-ETI measurement, the
  interpolation — → `conversion.yaml`.
- `calibrate_estate_v2.R` writes its fitted r / rho_pt / cluster cap directly, and
  `write_frozen_params.R` becomes the downstream merge emitting
  `config/calibrations/estate/bridge.yaml`, retiring the hand-written
  `other/estate_tax/estate_valuation_params.yaml` intermediate.
- `other/wealth_dynamics/write_profiles.py` emits a `provenance.yaml` sidecar per
  profile folder, which is how a TABLE-valued calibration joins the staleness net.
  Small and self-contained; a good first piece for the next session.

**Three sibling launchers are broken the same way the eta one was, and are NOT
ported.** `launch_form_laffer.sh`, `launch_form_memo.sh` and
`launch_timeable_logs.sh` all still export retired `KG_*` variables and pass the
retired `user_id` argument. Their runscripts (`tests/form_{laffer,memo,timeable}`)
are live and migrated, so the runscripts are fine; the launchers are not.

Two of the three need something this design does not currently offer, and it is an
author question rather than an oversight. `form_laffer` and `form_memo` are A/B
runs across `response_form`, which is now a FIXED setting in
`config/calibrations/kg/settings.yaml` — one path, every scenario, by deliberate
design, because the calibrations are conditioned on it. So a form A/B is no longer
expressible as two scenarios: it is edit `settings.yaml`, run one leg, edit back,
run the other. That may be the right answer (the two forms produce vintages that
are not comparable anyway, which is exactly what a fixed switch is for), but it
should be a ruling rather than a discovery. `launch_timeable_logs.sh` is portable
with the sweep pattern as it stands, and is deferred with the `timeable_share`
solver restoration it belongs to (follow-up 2).

**(b) The proving re-runs.** Each rewired calibrator runs once and must reproduce its
pinned value; drift goes to the author rather than being silently re-pinned.

DONE for eta_logs — see Part 5. Still owed: the estate calibration (one job), the
profile regeneration (seconds), and the sigma legs when that pipeline is rewired.

**(c) Retire what the new check absorbs.** DONE, `7a6ad2df5` — see Part 6.

The four that were deleted: the point-of-use warn in `src/sim/estate.R`;
`WEALTH_DYN_PROVENANCE` / `wealth_dyn_check_provenance()` / `WEALTH_STRICT_CALIB`;
`KG_DYN_SPEC_VERSION` (which turned out to be dead code); and
`other/kg_model_tests/calibration_reference.csv`, replaced by the much narrower
`moment_reference.csv`.

`config_repin_hashes()` was REWRITTEN rather than deleted, which is a deviation
from the plan and wants a ruling. Reasoning in Part 6. **It is now safe to call**
— it edits only the hash lines as text.

## Phase 6 — docs and final sweep

CLAUDE.md's config sections were already rewritten as part of 3b, so what
remains is: a final pass on `config/scenarios/README.md`, a closing pointer in
`POSTMORTEM.md`, pruning the `cfg_*` worktrees on scratch, and the final full
gate (six runs, parse-all, unit tests, `grep assumption(` empty).

Rough size: a few hours.

## Total

1. ~~Port the eta launcher~~ — DONE, `b4479729b` / `ab5b85e9b`.
2. ~~Launch the spot check~~ — DONE, `161ec866f`. Part 5.
3. Rewire the calibrators. TWO of four done: **eta_logs** (`181884ea4`) and
   **estate** (`1734452ce`), plus the **wealth profile** provenance files
   (`b58faeefe`). Two remain, and both are blocked on a full-sample run rather
   than on code:
   - **sigma.** The measurement script (`compute_top_eti.R`, currently under
     `other/top_tax/archive/tests/`) measures one leg; the interpolation onto the
     ETI target was done by hand. It needs the same sweep treatment `eta_logs`
     got — generated `conversion.yaml` files at trial sigma values, bound by their
     own behavior alternatives — plus a driver that interpolates and calls
     `calib_write_entry`. **Deliberately not run:** the plan defers sigma's proving
     run to the charity −0.5 re-derivation, which is the first follow-up after
     this project and will change the shipped value.
   - **levels eta.** `measure_efull_by_eta.R` needs the same treatment. It has no
     launcher at all — that dial was run by hand — so it needs a sweep generator
     too. `write_eta_sweep.py` is the template and most of it generalizes.
4. ~~Retire the superseded mechanisms~~ — DONE, `7a6ad2df5`. Part 6.
5. Phase 6: the docs sweep. Now the main thing left, and bigger than it was: see
   Part 6 for the comment-volume instruction, which is a repo-wide job the author
   intends to hand to a separate pass.

CLAUDE.md was already corrected for the behavior leg in `c2ed2840d`.

---

# Part 5 — the eta sweep, and the first calibrator that writes its own value

Four commits, 2026-07-26, closing items 1 and 2 of the list above and a quarter of
item 3.

## The launcher port

`launch_eta_dial_logs.sh` set the trial eta_tilde by exporting `KG_ETA_LOGS`,
`KG_RESPONSE_FORM` and `KG_TIMEABLE_SHARE_LOGS` into the submitting shell and
letting the sbatch phases inherit them, and passed the `user_id` argument retired on
2026-07-25. Both back doors are gone, so it had been unrunnable since — and nobody
would have found out until they tried to recalibrate.

A trial value is now three generated artifacts per grid point, written by
`other/kg_model_tests/form_ab/write_eta_sweep.py`:

| Artifact | What it is |
|---|---|
| `config/calibrations/kg/sweeps/eta_logs_<tag>/bathtub.yaml` | the shipped `bathtub.yaml` with `eta_logs` and only `eta_logs` replaced |
| `config/scenarios/behavior/alternatives/top_tax_full_eta_logs_<tag>/behavior.yaml` | the `top_tax_full` stack with the bathtub bound to that file |
| `config/runscripts/top_tax/eta_dial_logs_<tag>.csv` | `eta_dial_repin.csv` naming that alternative |

Regenerate-and-diff is the check; an empty `git status --short config/` is the pass.

Four things about the shape are load-bearing rather than stylistic:

- **The sweep file keeps the base name `bathtub.yaml`** inside a differently-named
  folder. Entries are labelled `{file stem}.{entry}`, so `eta_logs_15.yaml` would
  relabel every entry and detach any waiver written against `bathtub.eta_logs`.
- **The trial entry is `kind: judgment`, not `calibrated`.** It has no derivation to
  go stale: it is an INPUT to the calibration, not an output of one. The other three
  entries travel verbatim with their own provenance, so the sweep file goes stale
  under exactly the conditions the shipped one does.
- **One runscript per grid point**, not one runscript with three rows, because
  `measure_efull_logs.R` reads three separate VINTAGES whose shock scenario is
  called `s_cg_r25`, and the vintages on scratch already have that shape.
- **The launcher takes tags and a `VINTAGE_SUFFIX`.** The vintage name derives from
  the tag, so re-running tag 19 without a suffix would write over
  `eta_dial_logs_19` — the very thing the spot check compares against. A
  verification that overwrites its own reference is not one.

`launch_eta_dial_logs.sbatch` is new: `slurm_run.sh`'s Phase 0 runs `setup.R` in
process, which is real R work and does not belong on a login node.

## The Off-Model-Estimates pin had to go, and the reason is not the rebuild

The first launch died in Phase 0. `eta_dial_repin.csv` pins OME `20250925`, which
has been unreachable since the interface went to v5 on 2026-07-22: `20250925`
exists only under `v4`, and a v4 vintage hard-stops in post-processing anyway for
want of a `corporate_static` column (`revenue.R:59`). **That runscript had not been
runnable for four days, and resolving it does not reveal that**, because resolution
checks that a vintage is named, not that it exists. Verification gap 5 in Part 2 —
"the migrated families have not been RUN" — is exactly what caught it.

The generated runscripts therefore name `economy: default`. The dial does not care:
OME is read in receipts, the distribution smear and the corporate incidence
channel, none of which touches the per-record detail files E_full is measured from,
and the corporate wedge is identically zero here because both rows name the same
OME ID. The reasoning is recorded in the generator, next to the code that drops it.

Worth an author view, since it will recur: an economy alternative can pin an
interface VINTAGE but not the interface VERSION, which is repo-pinned in
`config/interfaces/interface_versions.yaml` as plumbing. A vintage lives *under* a
version, so the two are not independent, and every runscript pinning a
pre-v5 OME vintage is now silently unrunnable the same way. There is a real
argument for leaving it — v4 and v5 have different schemas, so the version is
code-coupled rather than scenario-coupled, and the hard stop is honest — but the
population of affected runscripts should be checked rather than discovered one at a
time.

## The writer

`src/misc/calibration_writer.R`. One entry of a calibration file replaced, every
other byte copied through. Textual, not a YAML round-trip, for the same reason
`config_repin_hashes()` is unusable: in these files the comments are the provenance
and `write_yaml()` deletes them. The round-trip is asserted byte-exact against the
real shipped file.

**The asymmetry is the design.** A re-run that REPRODUCES its pinned value writes in
place, refreshing the dependency hashes and the `set` date against the code that
just ran. One that does NOT writes `<file>.proposed` and stops with a banner naming
both numbers. A calibrated value moving means the model moved, the data moved, or
the calibration is less identified than it looks; the author reads the diff and
moves the file if the drift is real. Either way the number travels as a file rather
than a transcription, but no estimate changes without someone seeing it.

`other/config_redesign/test_calibration_writer.R`, 28 checks: the byte-exact round
trip, that only the named entry moves, that a vintage stays a string rather than
becoming an integer, that hashes are computed from the files at write time and land
directly after the list they describe, both directions of the reproduce/drift
branch, the tolerance, and the four refusals.

`measure_efull_logs.R` now ends in that call. Its scratch root and vintage suffix
come from the environment, so a spot check is measurable without editing the script
— editing the script being the old way a calibration got re-run. Its `rederive`
pointer, which named the LEVELS script, now names itself: one of the four broken
pointers the opening inventory counted.

## The spot check

Author ruled one of the three grid points re-run.
`sbatch other/kg_model_tests/form_ab/spot_check_eta19.sbatch` compares
`eta_dial_logs_19_spot` against `eta_dial_logs_19`.

**The measured moment reproduces to 1.4e-13 relative.** E_full agrees to ten
decimals with the value shipped in `efull_logs.csv`, which moves eta_tilde by about
3e-12 against a value carried to four decimals. Records align exactly, 221,831 of
them, in order.

**The detail files are NOT byte-identical, and expecting them to be was the wrong
frame.** 47 of 95 numeric columns differ at relative magnitudes of 1e-6 and below;
the two columns reported at relative 1 and 2 are sign noise on absolute magnitudes
of 1e-10 and 5e-6. The vintage on scratch was written on 2026-07-19 and the model
has moved since for reasons unrelated to this project — the `calc_mtrs` tips/OT
aggregate fix, the QBI and bracket-schedule rewrites, the corporate statutory rate
going on-model, the OME two-stream change. `mtr_cap_bundle` is the worst-differing
column and `calc_mtrs` is one of the things that changed, which fits. The rebuild's
own gate is byte-exact because it compares against goldens taken at the branch
point, not against a week-old vintage.

The script reports rather than asserts, because the useful output is the size of the
difference, not a pass light.

## Verification at this point

- `check_runscripts.R`: **80 parse, resolve and validate, 0 fail** (77 before, plus
  the three generated eta-dial runscripts).
- `test_calibration_writer.R`: **28 passed, 0 failed**.
- `test_stamp_negative.R`: 10/10. `test_behavior_leg.R`: 29/29.
- The spot check above.
- NOT re-run: the six-scenario gate. Nothing in these four commits changes a number
  — the new `src/` file only defines functions, and no shipped value moved — but the
  gate has not been re-run since `ba26fa5be` and the next batch of work should
  include it.

---

# Part 6 — the wealth profiles, the estate chain, and the mechanism retirements

Three commits, 2026-07-26, after Part 5.

## The wealth profile provenance files (`b58faeefe`)

`write_profiles.py` was writing to `config/wealth/profiles/`, which Phase 3b moved
to `config/calibrations/wealth_profiles/`. Running it would have created the old
directory again and left the live one untouched, printing success either way. That
is the third broken calibration path this session found by running something rather
than reading it.

Each profile folder now has a `provenance.yaml` written by the generator. A CSV
cannot say where it came from, so the file records what the surface targets, the
date, the md5 of `s.csv` and `M.csv`, and what invalidates it.

`derived_under` is empty on the default profile and that is the answer, not a gap.
The surface is a formula over published elasticities evaluated inside the generator
— Straub for the permanent-income elasticity, Mian-Straub-Sufi for
consumption-to-income by rank, Fagereng-Holm-Natvik for the age tilt, and
De Nardi-French-Jones for attenuating that tilt at the top. There is no Tax-Data or
Macro input anywhere in it, so no upstream vintage can make it stale. The generator
changing is what can, and that is what it declares.

Verified: regenerating reproduces both shipped `s.csv` and both `M.csv` byte for
byte. Coverage limit worth knowing: the parse-time check watches the DEFAULT
profile's tables, because that is what a default entry can see. A scenario pointing
at another folder gets that folder's `provenance.yaml`, which is recorded but not
compared.

`config/calibrations/wealth_profiles/s1_uniform/` has no generator and nothing in
the repository refers to it — the s=1, uniform-M corner from the June bounding
sweep. Left in place because CLAUDE.md describes it. It cannot be regenerated and
wants a decision.

## The estate chain (`1734452ce`)

It used to be: the fitting script printed r and rho_pt, a human typed them into
`other/estate_tax/estate_valuation_params.yaml`, and `write_frozen_params.R` read
that to build the bridge. The typing step is gone.

- `calibrate_estate_v2.R` ends by writing
  `config/calibrations/estate/valuation_fit.yaml`, recording the Tax-Data and Macro
  vintages it actually read and the cluster cap it was fitted under. Its data paths
  are overridable by environment variable, so a re-fit on a new vintage does not
  need the script edited — editing the script to re-run it is how these numbers came
  loose from their provenance in the first place.
- `config/calibrations/estate/settings.yaml` is new and person-owned: the four
  numbers somebody chose rather than measured (cluster cap 300, gift-pooling factor
  1.5, SOI death year 2022, exemption $12.06M). The cluster cap now has one home
  instead of two — the calibrator reads it from here rather than declaring its own.
- `write_frozen_params.R` reads those two files, and errors if the fit's two
  entries disagree about the run they came from. They are fitted jointly, so a
  disagreement means one is left over from an earlier run.
- The hand-written intermediate is deleted. `write_frozen_params.sbatch` is new;
  the script was documented as sbatch-run and had no wrapper.

Verified by regenerating `bridge.yaml`: every value the model reads is identical.
The only non-comment changes are `soi_death_year` 2022.0 → 2022 and
`soi_exemption` 1.206e+07 → 12060000 — same numbers, formatted differently now that
they come from YAML rather than R literals, and nothing in `src/` reads either
field.

`valuation_fit.yaml` is seeded from the values that were shipped, not re-derived,
the same treatment the kg files got. The estate re-fit is still owed.

## The mechanism retirements (`7a6ad2df5`)

Four deleted. Each held its own copy of expectations the parse-time check now
holds once.

| Removed | What it did | Why it could go |
|---|---|---|
| `WEALTH_DYN_PROVENANCE` + `wealth_dyn_check_provenance()` + `WEALTH_STRICT_CALIB` | kept its own Macro vintage, fmax and n_pctiles and warned mid-run | the profile is a calibrated entry now, checked at parse time |
| the Tax-Data warn in `src/sim/estate.R` | warned once per year-task on a vintage mismatch | `estate.valuation_bridge` carries the vintage and is checked at parse time, at warn level, before the run |
| `KG_DYN_SPEC_VERSION` | documented as bumped when the Bellman primitives changed, and carried on every state file | it was dead. Nothing read it, no state file wrote it |
| `calibration_reference.csv` | a per-constant ledger | it duplicated the shipped value, the invalidation list and the vintages, and had drifted from all three |

That last one is the "three disagreeing ledgers" problem visible in one file: its
sigma row named Tax-Data 2026050315 where `conversion.yaml` says 2026070814, and its
file paths still pointed at `config/scenarios/behavior/`, a tree Phase 4 moved. It
is replaced by `moment_reference.csv`, holding only the reference MOMENT — the one
thing that diagnostic uniquely knows. The shipped eta is read from the live
configuration instead of copied.

### The deviation: `config_repin_hashes()` was rewritten, not deleted

The plan lists it among the mechanisms to retire. I kept it, and this should be
ruled on.

Its defect was never redundancy. The parse-time check DETECTS staleness; re-pinning
ACKNOWLEDGES it, and somebody still has to do that: the two calibrated entries in
the economy leg — `financing_profile` and `valuation_bridge` — are not written by
any calibrator, so after a verified behavior-preserving refactor their hashes need
updating by hand. This commit needed exactly that, six times.

What was actually wrong with it is that it parsed each file with `read_yaml()` and
wrote it back with `write_yaml()`, deleting every comment. In these files the
comments are the record of where each number came from, so calling it destroyed
what the file is for. That is why every re-pin in this project was a manual `sed`.

It now edits only the hash lines, as text, and has a `dry_run` mode.
`other/config_redesign/test_repin.R` confirms it across every economy channel file:
line count unchanged, comments and structure unchanged, only hex values differ.
**It is safe to call.** If you would rather it not exist, deleting it is fine — the
alternative is a `sed`, which is what everyone was doing anyway.

### The six re-pins, and what licenses them

| File | Dependency | What moved |
|---|---|---|
| `wealth.yaml` | `src/sim/wealth_dynamics.R` | the provenance stamp and checker deleted |
| `wealth.yaml` | `write_profiles.py` | output path fixed, provenance files added; tables verified byte-identical |
| `estate.yaml` | `bridge.yaml` | header comment plus two reformatted record-keeping fields |
| `estate.yaml` | `valuation_fit.yaml`, `settings.yaml` | new inputs, added to the list |
| `estate.yaml` | `write_frozen_params.R` | reads the two new files instead of the old intermediate |
| the four kg files | `src/sim/kg/constants.R` | `KG_DYN_SPEC_VERSION` deleted (by hand — `config_repin_hashes` walks the economy leg, not `config/calibrations/`) |

Every one is a deletion of dead or superseded code, or a change whose output was
verified identical directly. Each carries a dated note in its file saying so, and
saying the re-pin stands only if the gate is byte-identical.

## Verification at this point

- 114 unit checks green: 47 engine, 29 behavior leg, 10 negative, 28 writer.
- 80 runscripts parse, resolve and validate with the new hashes — so the staleness
  check is clean, which is itself the check on the re-pins being complete.
- `test_repin.R`: the re-pinner changes hash values and nothing else.
- All four generators idempotent: the two top-tax batches (runscripts and tax law
  trees), the wealth profiles, and the eta sweep files.
- **The six-scenario gate PASSED**, vintages `rb_p5c_s{1,2,3,4,6,7}` vs
  `golds{1,2,3,4,6,7}`, all byte-identical under the sanctioned exclusions
  (`aa9915fc2`). That settles every conditional re-pin in Part 6; the notes in
  `wealth.yaml`, `estate.yaml` and the kg calibration files now say so
  unconditionally. Coverage is what makes it meaningful: S3 runs a four-family
  behavior stack, S4 the corporate, kg and wealth channels together, S6 the wealth
  tax, S7 the estate tax — so both deletions that could have changed behavior (the
  wealth provenance checker, the estate point-of-use warn) were exercised.

```bash
bash other/config_redesign/gate_diff.sh \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/rb_p5c_sN \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/goldsN
```

## An instruction about comments, recorded because it is repo-wide

The author's standing instruction is plain language in specs and code comments
(plan ruling 12). On 2026-07-26 they restated it much more sharply: there are far
too many comments, written in a register that reads as machine-generated. Volume is
part of the complaint, not only style — a long comment block explaining the
philosophy of a function is the problem even when every sentence is plain. A
separate cleanup pass is intended.

**One warning for that pass.** In `config/calibrations/**` and
`config/scenarios/economy/**` the comments are the data. They hold the provenance
of every calibrated number, which is why those files must never be round-tripped
through a YAML library and why `config_repin_hashes()` had to be rewritten as text
editing. Trimming comments there would delete the content the files exist to carry.
`src/` and `other/` are fair game.

---

---

# Part 7 — Phase 6, and two verification gaps closed

Two commits, 2026-07-26, after Part 6. **Phase 6 is done and the project's
verification is complete.** What is left is not code: the branch merge, the two
calibrators that are blocked on a full-sample run, and the author's repo-wide
comment cleanup.

| Commit | Scope |
|---|---|
| `2c4313ce3` | `config/scenarios/README.md`; the postmortem's closing note; the interface-existence check in the parse gate |
| `a15e24db9` | `mapping_check.py` rewritten for the design that shipped, and called by `gate_diff.sh` |

## The closing gate

Vintages `rb_p6_s{1,2,3,4,6,7}` against `golds{1,2,3,4,6,7}`: **all six
byte-identical** under the sanctioned exclusions, and each now also passing the
manifest mapping check inside the comparator. Run from the project tree at
`2c4313ce3`.

```bash
bash other/config_redesign/gate_diff.sh \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/rb_p6_sN \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/goldsN
```

Everything else in the closing sweep: 114 unit checks green (47 engine, 29
behavior leg, 10 negative, 28 writer, plus `test_repin`); 80 runscripts parse,
resolve and validate, 0 fail; all four generators idempotent; `grep
'assumption('` empty across `src/` and `config/`.

## Gap 1 closed: the mapping check

Running the recovered `mapping_check.py` was never the fix, which is why it sat
open through five phases. It encodes the ABANDONED branch's target: it maps
module-only parameters onto a behavior leg that carries VALUES, and expects to
find `behavior.evasion.e_schc` in a manifest. This design has no such row — by
author ruling those nine parameters live in their module's file, and no manifest
mentions them. The tool could not have validated the thing that shipped.

The question it should ask is not whether one manifest maps onto another but
**whether any assumption value changed while being relocated**. The old
`assumptions.csv` carried all 45 for every scenario; they now sit in three
places, and the rewrite follows each to whichever one owns it:

| Destination | Checked against |
|---|---|
| economy leg | `scenario_config.csv` in the candidate vintage |
| kg calibration | `calibrations.csv`, or the calibration FILE when the scenario binds no kg pieces |
| behavior module | a top-level constant in the module's `.R` file |

That middle row has a wrinkle worth keeping in mind: a bound calibration appears
in `calibrations.csv` only for the scenarios that bind it, so a baseline row's
`kg.eta` is legitimately absent from the manifest and is checked against the file.

**Result: 495 old manifest rows across the six vintages, every one located and
equal.** This is the first evidence that the relocation was value-preserving
rather than merely gated — the byte gate proves the model computes the same
numbers, not that each number is where its provenance says it is.

The check is not vacuous. Perturbing one value of each destination kind in a
scratch copy of a golden manifest produces three failures, each naming its
destination. And `gate_diff.sh` now CALLS it rather than carrying a comment
saying it is responsible: the manifest families are excused from the byte
comparison because their shape changed, which only excuses them if something
still checks their content.

## Gap 2: the equivalence check is superseded, not owed

`equivalence_check.R` compared the old parser against the new resolution. It is
obsolete for the same reason `mapping_check.py` had to be rewritten — it targets
the abandoned shape, calling `config_resolve()` with `set_name =` and iterating
over the deleted `excess_growth*` fields — and what it would prove is now proven
better twice over: six byte-identical simulations, and the mapping check on the
values themselves. Adapting it would be work in service of weaker evidence.
Recommend deleting it; left in place pending a view.

## The nine unrunnable runscripts

Ruling item 1 has an answer, and it is worse than the status header implied.
`check_runscripts.R` now asks whether the interface directories a row names
actually exist. `parse_globals()` already checked that, but only once a run is
under way, and the parse gate deliberately stops short of `parse_globals()` — so
a runscript pinning a vintage that is not there resolved clean and died in SLURM
Phase 0.

Off-Model-Estimates v5 holds exactly one vintage, `20260722`, with two IDs
(`baseline` and `corp_28_2027_2stream`). Every other pin in the tree — `20250925`,
`20260706`, `top_tax_corp_placeholder` — exists only under v4. So **9 of the 80
live runscripts cannot run**: all three `clausing_v2*` scripts and all six
`top_tax` batches, 229 rows on `ome_20250925` alone.

| Alternative | Pin | Live runscripts |
|---|---|---|
| `ome_20250925` (+ `_saving_{0,25,50,75}`) | 20250925 / baseline | 9 files, 229 rows |
| `ome_20260706_corporate_saving_{25,50,75}` | 20260706 / 07_corporate | 1 file each |
| `ome_top_tax_corp_placeholder` | top_tax_corp_placeholder | 3 files, 115 rows |
| `corp_kg_wealth_reform` | ID only, inherits the v5 default | runnable (gate fixture S4) |

The cause is structural and will recur: an economy alternative can pin an
interface VINTAGE but not its VERSION, which is repo-pinned in
`interface_versions.yaml` as plumbing, and a vintage lives UNDER a version. There
is a real argument for leaving it that way — v4 and v5 have different schemas, so
the version is code-coupled rather than scenario-coupled, and a hard stop is
honest. What was wrong was that nothing said so until launch. Now the parse gate
does, reported separately from the pass/fail tally and deliberately not fatal:
the fix is a decision about the pins (regenerate those vintages under v5, or
retire the runscripts), not about the runscript files.

## The scratch worktrees are pruned

All eight `cfg_*` worktrees removed, ~260MB. Two were dirty and were checked
first: `cfg_rb_p3b`'s changes were the rsynced Phase 3b tree, all superseded by
committed work, and `cfg_p3b_equiv`'s were abandoned-branch material in the
`sets/` shape this rebuild rejected — its `economy_sets.py` and
`migrate_runscripts.py` are already in the branch from Phase 0. The golden output
trees were NOT touched.

If the equivalence check is ever wanted, its pre-3b worktree is one
`git worktree add` at `28ecf4f33` (Phase 2), not the deleted one, which sat on
the abandoned branch.

## The author's rulings, 2026-07-26 (all but one closed)

**Off-model estimates leave the top-tax batches entirely** (`c254bf72e`). The
corporate rate is on-model as of 2026-07-23, so the off-model wedge was a second
source for revenue the model already produces, and receipts add both: a corp
scenario regenerated today would have counted the 21→28 hike TWICE, about $49B in
2027 rising to $96B. The generators' comment still claimed corporate revenue
"comes from the OME channel", written before the change and saying the opposite of
what the code does. Both lever states now name `default`. The corp-OFF pin cost
nothing to drop (v4 20250925/baseline is all zeros, same as the v5 default).

**Clausing gets a real v5 vintage**, because its corporate scenario carries
genuine off-model revenue ($337B in 2030). `20260726/07_corporate`: six columns
copied byte-for-byte from v4 `20260706/07_corporate`, `corporate_static` set EQUAL
to the conventional `corporate` column. Verified across all 25 years. **That
equality is deliberate and breaks the usual convention** — this scenario's static
leg carries the corporate behavioral response, where static is elsewhere law-only,
so its static-minus-conventional corporate reading is identically zero. The source
is a single published conventional series with no mechanical counterpart. Recorded
in the vintage's own `corporate_meta.yaml`.

Clausing's other seven scenarios pinned the all-zero OME and now name `default`;
their saving shares live in four new alternatives (`saving_0/25/50/75`) that
override the financing profile and nothing else. The nine orphaned `ome_*`
alternatives are deleted.

**The `private/` runscripts are archived**, all 26, into
`config/runscripts/private/retired_2026_07_26/`. That path is gitignored
("Code associated with private analyses"), so they were NOT moved into the tracked
`archive/` tree — doing so would have committed private client work. Their README
warns that three of them will reorder their behavior stack if revived.

**`s1_uniform` deleted; `equivalence_check.R` deleted; `config_repin_hashes()`
kept** (`58edd1740`). One correction to this document: it said no script could
regenerate `s1_uniform`. Wrong —
`other/wealth_dynamics/write_extreme_profile.py s1_uniform 1.0` rebuilds it
exactly, which is what made the deletion cheap.

**Still open:** the three sibling launchers (`launch_form_laffer.sh`,
`launch_form_memo.sh`, `launch_timeable_logs.sh`). Two A/B across `response_form`,
now a fixed setting, so a form A/B is two hand edits to `kg/settings.yaml` rather
than two scenarios. That may be right by design and wants a view.

## The calibrators, all four (2026-07-26, after the merge)

Every calibrated value in the model is now written by the script that measures it.
This was the actual point of the project and the last part of it to land.

| Value | Writer | State |
|---|---|---|
| eta_logs 1.6625 | `measure_efull_logs.R` | writes itself; re-simulation reproduced the moment to 1.4e-13 (`161ec866f`) |
| r 0.951, rho_pt 0.612 | `calibrate_estate_v2.R` | writes itself (`1734452ce`) |
| s (the saving surface) | `write_profiles.py` | writes a provenance file per profile (`b58faeefe`) |
| **eta 2.4825** | `measure_efull_by_eta.R` | **writes itself; REPRODUCED exactly** (`662471d81`) |
| **sigma 0.16** | `measure_sigma.R` | **pipeline built, deliberately not run** (`0b15fc9e0`) |
| timeable_share 0.2542 | — | solver still demoted; follow-up 2, dated waiver in place |

### eta needed no new simulation

The three vintages the 2026-07-12 re-pin wrote are still on scratch, so re-running
the measurement against them is a real proving run rather than a reconstruction. It
returned eta* = 2.4825, the shipped value, from slope 1.01551 and grid E_full
-2.0483 / -2.4395 / -3.0325 -- so `calib_write_entry` wrote in place rather than to
`.proposed`.

`write_eta_logs_sweep.py` became `write_eta_sweep.py` and serves both response forms
instead of being copied. The logs grid is unchanged; only the generator's own name
and two comment lines move in its output.

`launch_eta_dial_levels.sh` is new -- the levels dial had never had a launcher,
having been run by hand through an environment variable. It REFUSES to run unless
`response_form` is `levels` in `settings.yaml`, reading the setting rather than
trusting the operator, because a levels sweep executed under the logs form would
produce a plausible-looking number measured under the wrong model.

**One caution about generated provenance.** Rewriting an entry rewrites its prose,
and the first attempt silently dropped things the old hand-written note carried: the
value's history (4.4984 -> 2.3992 -> 2.4825) and the reason `apply.R` is in the
dependency list (an applier-rule change once biased every conventional kg estimate
by about 37% on a 5pp score). Both were put back into the script's note text. A
generated file is only as good as what its generator was told to say, and this is
the failure mode to watch for in the other three.

### sigma is built and NOT run, on purpose

Generating a calibration's inputs is not the same as deciding to re-derive it. The
shipped 0.16 was derived under charity/100 while every product run uses charity/50,
which is the dated waiver on the entry and the reason the re-derivation is follow-up
1. So the charity elasticity is an explicit flag: `--charity 100` reproduces the
conditions behind 0.16, `--charity 50` is the deferred job. The tree ships the 100
grid, unrun.

Two design choices in it are worth not re-litigating. The grid's floor is sigma = 0
with the conversion module still BOUND rather than dropped, so the leg writes its own
sigma gate thresholds -- the old no-sigma leg had none and borrowed them from another
run. And the interpolation is piecewise-linear, not through the origin, because the
ETI at sigma = 0 is already about 0.22; that intercept is what makes sigma a residual
and a through-origin fit would bury it.

## What is left

1. **Run sigma's re-derivation** when the author wants it (follow-up 1). Everything
   it needs exists; it is three full-sample legs plus one measurement job.
2. **Restore the `timeable_share` solver** (follow-up 2), which clears the last
   inherited waiver.
3. **The comment cleanup**, a separate repo-wide pass. Warn it: in
   `config/calibrations/**` and `config/scenarios/economy/**` the comments are the
   data.

The branch is MERGED: `wealth` and `origin/wealth` both carry this work.

---

## Follow-ups the plan puts outside this project

1. Sigma re-derivation at charity −0.5 on `tests/topord_plus5` — the first real
   use of the new calibration pattern; clears the sigma waiver.
2. Restore the `timeable_share` solver; clears its inherited waiver.
3. Migrate any untracked `private/` runscripts on demand, and any of the 71
   archived in 3c that turn out to still be wanted. The archive README names the
   likely ones and gives the two-step recipe. Three of the private files will
   reorder their behavior stack when migrated — see the order assertion in Part 2
   before assuming their old numbers still hold.
4. If the top-tax batches are ever rebuilt from scratch, consider moving
   `no_ord_cap` inside the cg lever's own block, where it belongs. It sits at
   the end of `pref.yaml` today only to preserve byte-identity with the shipped
   vintages.
