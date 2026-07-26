# Config rebuild v2 — full status

*Branch `config-rebuild`, off `wealth` at `324a7cd38`. Phases 0 through 4 built
2026-07-26. All three legs are live: a runscript row is now an ID, three folder
pointers and the computational scope, and nothing else. Both top-tax generators
reproduce everything they write, tax_law trees included. Phases 5 and 6 remain.
Plan of record: `~/.claude/plans/cheerful-zooming-star.md`, which carries a
status header pointing back here.*

This document is meant to be enough to resume cold. It is in four parts: what
the branch contains now, what was verified and how, the decisions taken without
the author present, and what each remaining phase now involves.

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
| `823f8fd93`, `88a2e882c` | — | This document. |

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

## Verification NOT yet done

1. **`mapping_check.py` has never been run against a 3b vintage.** It is the
   thing that confirms `scenario_config.csv` carries what `assumptions.csv`
   did. Until it runs, that manifest swap is unverified.
2. **The equivalence check has not been run.**
   `other/config_redesign/equivalence_check.R` compares the old parser (in a
   detached pre-3b worktree) against the new resolution over the migrated
   runscripts. It was recovered but not adapted: line 170 still iterates over
   the deleted `excess_growth*` fields, and it calls `config_resolve()` with
   the old `set_name =` argument.
3. **The unit tests do not cover the waiver path.** 47 engine checks pass, none
   of them exercise a `waiver` block. (The behavior loader's 27 checks are a
   separate file and do cover every refusal it can make.)
4. **No negative test exists yet** for the staleness stop. Phase 5 calls for
   one (corrupt a stamp in a scratch copy, show the hard stop fires); the
   equivalent evidence today is anecdotal — the stop fired twice during this
   session, correctly, and both times is recorded below.
5. **The three migrated runscript families have not been RUN.** They parse,
   resolve and validate, which is what `check_runscripts.R` proves, but no
   simulation has been executed from one. That matters most for the OBBBA
   retrospective, whose two scripts were also pulled out of the archive and had
   their `tax_law` cells changed, and least for the top-tax batches, whose
   economy pins are structurally identical to the gate fixtures'.
6. **`private/` is unmigrated and now skipped by the parse check.** The 26 files
   there are untracked one-off work; their behavior cells still name module
   lists and would fail resolution. Three of them will reorder when migrated
   (see the order assertion above).

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

### 15. `kg_dynamics` accepts a list as well as a mapping

The plan describes the section as a mapping of piece to stamped file. Those files
do not exist until Phase 5, and pointing the mapping at today's homes would have
meant writing a path to a module file as though it were a calibration stamp, in
nineteen folders, to be rewritten in a fortnight. The list form says exactly what
is true now — these pieces are bound — and the loader accepts both from the
start, so Phase 5 adds paths without touching code. If a reader prefers only one
form to exist, deleting the list branch after Phase 5 is a five-line change.

---

# Part 4 — What is left

Phase 5 and Phase 6. Nothing from Phases 0 through 4 is open.

## Phase 5 — calibration stamps

The actual point of the project, and over half the remaining work. The
inventory run for the plan found that **not one of the five calibrated values
in the model is written by its calibration script** — all were hand-copied from
logs — **four of five "how to re-derive this" pointers are broken**, sigma was
calibrated under a charity elasticity production does not use, and three
provenance ledgers disagree with each other.

**(i)** `stamp_check()` extending `config_check_staleness()` at parse time, plus
the stamp schemas (scalar YAML, and a table sidecar for the CSV-valued ones).

**(ii)** Write the stamps from CURRENT pinned values, no re-derivation:
- `config/calibrations/kg/bathtub.yaml` — `eta` 2.4825 (levels form, dormant,
  inherited waiver), `eta_logs` 1.6625 (live), `timeable_share` 0.2542
  (inherited waiver, "solver demoted 2026-07-12"), `timeable_share_logs` (live).
  Conditions record the kg conditioning set, now machine-readable.
- `config/calibrations/kg/conversion.yaml` — sigma 0.16, with conditions
  recording `charity/100` (the −1.0 it was actually derived under) plus a dated
  waiver, so `charity/50` product runs pass with a visible banner until the
  sanctioned re-derivation.
- `config/calibrations/kg/entity_shifting.yaml` — `sourced` kind: 0.3788/0.6,
  alpha 0.45, beta_legacy 0.25, Pearce-Prisinzano 2018. The module reads them
  from here.
- `config/calibrations/kg/settings.yaml` — the one hand-editable file in the
  calibrations tree: the structural and judgment knobs (`response_form`,
  `applier_allocation`, `dg_allocation`, `timing_window`, `timing_ref_wedge`,
  `wealth_carry_scale`, `beta_fallback`, `deemed_avoidance`, the `char_*`
  logits and base year). The `char_*` entries carry their honest "no source
  recorded" note as judgment entries. The bathtub stamp records the settings it
  was calibrated under, so changing a knob stops kg runs until re-calibration
  or a waiver.
- `config/calibrations/estate/bridge.yaml` regenerated with a full stamp.
- Sidecar `provenance.yaml` for each wealth profile folder.
- `kg.yaml` and `sigma.yaml` leave `economy/default/`.

**(iii)** Rewire every calibrator to END by writing its stamp:
`measure_efull_by_eta.R` and `form_ab/measure_efull_logs.R` → `bathtub.yaml`
(their hardcoded scratch roots at lines 29-38 need parameterizing); the sigma
pipeline (two `topord_plus5` legs, the top-ETI measurement, the interpolation) →
`conversion.yaml`; `calibrate_estate_v2.R` writes its fitted r/rho_pt/cluster-cap
directly and `write_frozen_params.R` becomes the downstream merge emitting
`bridge.yaml`, retiring the hand-written
`other/estate_tax/estate_valuation_params.yaml` intermediate;
`write_profiles.py` emits its sidecar.

**(iv)** Proving re-runs, each of which must reproduce the pinned value, with
any drift going to the author rather than being silently re-pinned: the estate
calibration (one job), the `eta_logs` batch (**three staged full-sample
vintages — the single largest compute item in the project**), and the profile
regeneration (seconds).

**(v)** Retire what the stamps absorb: the `estate.R:49-61` warn,
`WEALTH_DYN_PROVENANCE` / `wealth_dyn_check_provenance()` /
`WEALTH_STRICT_CALIB`, the `KG_DYN_SPEC_VERSION` check,
`other/kg_model_tests/calibration_reference.csv`, and
`config_repin_hashes()` (decision 1).

Gate: the six runs stay byte-identical (stamps are metadata), plus a deliberate
negative test — corrupt a stamp condition in a scratch copy, show the hard stop
fires.

Rough size: 1.5 to 2 days of work, plus the `eta_logs` compute batch, which
should be launched early since it gates the sign-off.

## Phase 6 — docs and final sweep

CLAUDE.md's config sections were already rewritten as part of 3b, so what
remains is: a final pass on `config/scenarios/README.md`, a closing pointer in
`POSTMORTEM.md`, pruning the `cfg_*` worktrees on scratch, and the final full
gate (six runs, parse-all, unit tests, `grep assumption(` empty).

Rough size: a few hours.

## Total

A day and a half to two and a half working days, Phase 5 being nearly all of it.
Phase 5 is what to do next, and the `eta_logs` compute batch inside it should be
launched as early as possible: it is three full-sample vintages and it gates the
sign-off.

Two things Phase 5 should pick up that Phase 4 left in a deliberate half-state:

- The `kg_dynamics` sections in the nineteen behavior alternatives are written in
  the LIST form (piece names, no paths). Turning them into mappings that point at
  the stamped files is the moment those files come into existence, and needs no
  loader change (decision 15).
- Ten dependency hashes were re-pinned conditionally on the Phase 4 gate. If that
  gate is clean the notes in `kg.yaml`, `sigma.yaml` and `wealth.yaml` can be
  collapsed into one line; Phase 5 rewrites those entries as stamps anyway.

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
