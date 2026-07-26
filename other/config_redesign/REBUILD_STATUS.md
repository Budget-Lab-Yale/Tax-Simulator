# Config rebuild v2 — full status

*Branch `config-rebuild`, off `wealth` at `324a7cd38`. Phases 0 through 3c built
2026-07-26. All six gate scenarios byte-identical; all 103 live runscripts parse
and resolve; both top-tax generators reproduce everything they write, tax_law
trees included. Nothing from 3c is left open. Phases 4, 5 and 6 remain. Plan of
record: `~/.claude/plans/cheerful-zooming-star.md`,
which carries a status header pointing back here.*

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

---

# Part 2 — What was verified

## The six-scenario gate, all passing at `cbc0030d9`

| | Runscript | How | Golden | Result |
|---|---|---|---|---|
| S1 | `baseline/baseline` | `main.R`, pct 0.05 | `golds1` | pass |
| S2 | `rebate_2025` | SLURM, pct 1 | `golds2` | pass |
| S3 | `tests/multi_module_smoke` | SLURM, pct 1 | `golds3` | pass |
| S4 | `tests/corp_kgwealth_verify` | SLURM, pct 1 | `golds4` | pass |
| S6 | `wealth_tax`, scenario `wealth_tax_warren` | SLURM, pct 1 | `golds6` | pass |
| S7 | `estate_2009` | SLURM, pct 1 | `golds7` | pass |

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

`other/config_redesign/check_runscripts.R`, new in 3c. For every CSV under
`config/runscripts/` except `archive/`, it enforces the eight-column schema via
`validate_runscript_columns()`, then resolves each row's economy leg and runs
`config_check_staleness()` on it. It deliberately stops short of
`parse_globals()`, which reads Tax-Data and creates output trees — what a bad
migration actually breaks is the schema check and the resolution, and both are
cheap.

```bash
sbatch other/config_redesign/run_tests.sbatch . other/config_redesign/check_runscripts.R
```

Result at `7c30eb26a`: **103 parse and resolve, 0 fail.**

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
3. **The unit tests do not cover the new waiver path.** 45 checks pass, none of
   them exercise a `waiver` block.
4. **No negative test exists yet** for the staleness stop. Phase 5 calls for
   one (corrupt a stamp in a scratch copy, show the hard stop fires); the
   equivalent evidence today is anecdotal — the stop fired twice during this
   session, correctly, and both times is recorded below.
5. **The three migrated runscript families have not been RUN.** They parse and
   resolve, which is what `check_runscripts.R` proves, but no simulation has
   been executed from one. That matters most for the OBBBA retrospective, whose
   two scripts were also pulled out of the archive and had their `tax_law` cells
   changed, and least for the top-tax batches, whose economy pins are
   structurally identical to the gate fixtures'.

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

---

# Part 4 — What is left

## Phase 4 — the behavior leg

Three sub-steps, each gated on the six runs:

**(i)** New loader in `src/sim/behavior.R`. `behavior.yaml` has two sections:
`kg_dynamics` (either `none` or a mapping of paths to stamped calibration
files) and `modules` (a bare list of file paths). The loader sources each listed
path — the existing `load_behavior_module()` / `sys.source` pattern carries over
— and calls `do_{family}` where family is the file's parent folder name. No
registry, no known-names list: that closure is what killed the previous
attempt. Execution order comes from stable-sorting the list against one pinned
family order (`kg_dynamics → conversion → entity_shifting → evasion → wealth →
estate`); unknown families run last, in listed order, and nothing is ever
rejected for being unknown. Migration safety: for every migrated runscript,
assert the sorted order equals today's literal order and flag any difference
rather than silently reordering.

**(ii)** Move the modules from `config/scenarios/behavior/{family}/*.R` to
`src/behavior/{family}/*.R` so config stops holding executable code. Delete the
five hand-written order guards (`conversion/sigma.R:70-97` guards 1–2 only —
input guards 3–4 stay; `pearce_prisinzano.R:52-65`; `run.R:84-90`;
`debacker.R`'s introspection, which becomes a parse warning; and the order
blocks in `wealth/avoidance.R` and `estate/avoidance.R`). Re-key the activation
predicates from "module prefix appears in the list" to "`kg_dynamics != none`":
`scenario_uses_kg_dynamics` (`src/sim/kg/state.R:36-40`), `run.R:77` and `:944`,
`frozen.R:55`, `bathtub.R:56`; `scenario_uses_sigma`
(`sigma_conversion.R:141-145`) keys on the conversion family being listed.

**(iii)** Inline the module-only parameters into their module files with
citations — evasion's `e_schc`/`e_pt`/`e_rent`/`topend_mult`, estate's
`report_eps`, wealth's `avoid_*`/`chi_*` — and delete those entries from the
temporary economy files.

New parse-time cross-checks replace the deleted guards: conversion listed
without kg_dynamics stops; a kg_dynamics sub-entry inconsistent with its module
stops; evasion without the estate module warns loudly; a listed path that does
not exist stops.

Rough size: a day.

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

Two and a half to three and a half working days, Phase 5 being over half.
Phase 4 is what to do next. The `eta_logs` compute batch inside Phase 5 should
be launched as early as possible, since it gates the sign-off and is three
full-sample vintages.

## Follow-ups the plan puts outside this project

1. Sigma re-derivation at charity −0.5 on `tests/topord_plus5` — the first real
   use of the new calibration pattern; clears the sigma waiver.
2. Restore the `timeable_share` solver; clears its inherited waiver.
3. Migrate any untracked `private/` runscripts on demand, and any of the 71
   archived in 3c that turn out to still be wanted. The archive README names the
   likely ones and gives the two-step recipe.
4. If the top-tax batches are ever rebuilt from scratch, consider moving
   `no_ord_cap` inside the cg lever's own block, where it belongs. It sits at
   the end of `pref.yaml` today only to preserve byte-identity with the shipped
   vintages.
