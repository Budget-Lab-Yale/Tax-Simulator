# Config redesign: what was attempted, what happened, and why it was abandoned

*Written 2026-07-25 by Claude, at the author's request, after the work was
reverted. This is the record; the plan of record it replaces is
`other/config_redesign/PLAN.md`, which now exists only on the abandoned
`config-redesign` branch.*

---

## The short version

Over 2026-07-25 a five-phase rewrite of scenario configuration was built on
branch `config-redesign`. Phases 0 through 3 landed and passed byte-identical
regression gates. Phase 4 was built, failed review, and the whole branch was
abandoned the same day.

`wealth` was never touched. One commit from the branch — Phase 1, which removed
dead configuration surface and unified command-line parsing — was cherry-picked
onto `wealth` as `5ad09fd69`. Everything else stays on `config-redesign`.

The proximate cause of abandonment was Phase 4 removing the pluggable
behavioral-module interface, which the author had named as the central feature
of the existing design and had said explicitly should be preserved. The deeper
cause is in §5.

---

## 1. What the redesign was trying to do

Before: a runscript row is a CSV line naming a tax law folder, plus an open-ended
set of extra columns carrying loose values — `dep.Tax-Data.vintage`,
`assumption.evasion.e_schc`, `s`, `wealth_financing`, `excess_growth`, and so
on. A number typed into a spreadsheet cell has no provenance: no citation, no
note, no record of what it was calibrated against.

The proposed shape: a scenario is three legs, each naming a FOLDER and never a
value.

| Leg | Answers |
|---|---|
| `tax_law` | what is the policy |
| `economy` | how does the world work (data vintages, incidence mechanics, measurement bridges) |
| `behavior` | how do agents respond to a reform |

The runscript schema becomes exactly eight columns — `ID, tax_law, economy,
behavior, years, dist_years, mtr_vars, mtr_types` — and any other column is a
parse-time error. Values that used to sit in CSV cells move into named folders
where each entry carries a `kind` (calibrated / sourced / judgment / structural)
and the provenance that kind requires.

## 2. What was built, phase by phase

| Phase | Commit | State reached |
|---|---|---|
| P0 golden reference | `d5fefe8cc` | md5 manifests of 6 regression runs at the pre-change tree, as the comparison baseline |
| P1 dead surface + CLI | `f78859d9f` | **KEPT** (as `5ad09fd69` on `wealth`) |
| P2 dormant machinery | `1c630f17d`, `60ed7e1db` | resolution engine written and unit-tested, not yet wired to anything live |
| P3 economy flip | `379d0bbc3` | economy leg live; superseded by P3b |
| P3b files-only rework | `efe692e11` … `3b8f3a6c0` | economy leg live under the files-only rule |
| P4 behavior flip | `p4-attempt-1` | built, gate never completed, reverted |
| P5 delete old machinery | — | never started |

### Phase 1 — the one that survived

Removed configuration surface nothing read, and unified argument parsing for
`main.R` and the SLURM entry point into `src/misc/cli.R`. This retired the
`user_id` positional argument, so both entry points now take one fewer argument.
Gate: byte-identical across all six regression runs.

This commit is independent of the three-leg idea and is the only part now on
`wealth`.

### Phase 2 — the engine

`src/misc/scenario_config.R`: a generalization of the assumptions layer
(`src/misc/assumptions.R`, already on `wealth` as `8abf0f0b4`) to serve two legs
instead of one channel set. Provenance schema, staleness checks on data vintages
and dependency-file hashes, dated waivers, `locked` entries, and an economy-only
`role: state | transmission` distinction that makes "static results are law-only"
a machine-checked property rather than a convention.

Written dormant — the live code path was untouched — so it could be tested
before anything depended on it. Engine unit tests 44/44.

### Phase 3 / 3b — the economy leg

P3 flipped the economy leg live with per-value override columns still available.
Mid-flight the author ruled that runscripts must name files only, never values,
so P3 was reworked into P3b: the dotted override mechanism was deleted and
sweeps became generated folders.

`other/migrations/migrate_runscripts.py` rewrote all 144 tracked runscripts from
the old schema to the eight-column one, sweeping the retired columns into
economy set folders named after their contents so identical pins collapse onto
one folder.

Results:
- Gate GREEN. All six regression runs byte-identical to the P0 golden trees,
  under the sanctioned exclusions (xlsx compared by content, `code_version.csv`
  excluded, manifest families verified by a mapping check).
- Equivalence check PASS: 523 scenario resolutions across 144 runscripts, old
  parser and new set folders producing identical results.
- 69 economy set folders generated.

Two real bugs were found and fixed by the P3 gate, both of which would have been
shipped otherwise: the estate valuation-bridge staleness check was hard-stopping
~90 legitimate runscripts with no override escape, and the new year-spec parser
broke ALTREP compactness in a way that changed the bytes of a serialized kg
state file.

### Phase 4 — the behavior leg (reverted)

Built on 2026-07-25 as commits `0fce0a93c` and `fff71f9f8`, preserved on tag
`p4-attempt-1`.

What it did:
- Moved the 13 behavior modules from `config/scenarios/behavior/{var}/{module}.R`
  to `src/behavior/{response}.R`, sourced at startup with the rest of `src/`.
- Added `src/behavior/registry.R`: canonical execution order, conflicts,
  requires/wants, and required MTRs per response, validated once per scenario at
  parse time.
- Replaced the space-delimited module-path list in the behavior column with a
  single set-folder name.
- Collapsed the parameter-only module variants: the five-file kg elasticity grid,
  the charity pair, and the ot trio each became one file plus a set delta.
- Deleted the five hand-written pairwise order guards the modules used to carry,
  and rewrote `lint_estate_module.R` to apply the registry checks across the
  whole runscript library.

Test results before revert: engine 46/46, parse smoke 23/23, a new behavior
smoke 119/119 (including param-literal equivalence against every deleted
module's hardcoded values, and all 33 combinations in the migration map), lint
green across 144 runscripts, hidden-ledger unit tests 40/40.

The regression gate was launched twice and completed neither time. The first
attempt died at startup on a duplicate staleness check in the legacy assumptions
layer; that was fixed. The second was cancelled during review.

---

## 3. Why it was abandoned

**The pluggable-module interface was removed.** Before Phase 4, adding a
behavioral response meant dropping an `.R` file into
`config/scenarios/behavior/{var}/` containing a `do_{var}()` function and naming
it in the runscript. No source change, no registration. The caller sourced the
file at runtime and called the function without knowing or caring what was in
it. This is documented as the crowning feature of the behavioral-module design
and the author had stated the redesign must preserve it.

Phase 4 closed it, in three steps, none of them individually flagged:

1. `load_behavior_module()` — the function that sourced a `.R` file out of the
   config tree at runtime — was deleted. Nothing under `config/` is executed
   anymore.
2. The dispatcher looks the function up by name in `BEHAVIOR_REGISTRY`, which
   hard-errors on any name it does not know.
3. The parse-time validator rejects unknown names.

After Phase 4, adding a behavior required two edits inside `src/`. The set of
behaviors was closed at the source level. That is the opposite of the design
premise.

The plan line that produced this read: *"Module code → `src/behavior/`
(define-only); registry with canonical order/deps."* Taken literally, those two
steps close the extension point. The conflict with the stated premise should
have been raised as a design question before implementation; it was not.

**Secondary problems surfaced in the same review:**

- *Naming and structure inconsistency between legs.* `tax_law/` uses plain
  descriptive folders (`baseline`, `public/`, `tests/`). `economy/` grew a
  `sets/` subfolder holding 69 machine-named folders like
  `ome_20250603_cbo_delta_xg0p001_allrev0_from2026`. The word "set" is jargon
  introduced by this work; it means nothing that "a named folder" does not
  already mean. The subfolder existed only to hide the auto-generated names.
  The legs should have had one shape.

- *Config/code binding is checked in one direction only.* A channel yaml is a
  bag of named values that the response reads by name. If the code asks for a
  value the yaml lacks, it errors. If the yaml holds a value the code no longer
  reads — or if the meaning of a parameter changes while its name does not —
  nothing catches it, and you are left with a number carrying a confident
  citation that describes something the code no longer does. A declared
  parameter list on the registry entry would have closed this; it was not built.

- *Communication.* Repeated requests for a plain explanation were answered with
  long structured prose rather than direct answers, which extended the review
  well past the point where the design problem had already been identified.

---

## 4. Open findings worth keeping regardless

These were surfaced by the redesign work and are true independently of it.

1. **sigma is calibrated under the wrong charity elasticity.** The shipped
   `sigma.conv = 0.16` was derived with the charity elasticity at −1.0 (the
   `sigma_calibration` stack), but the product runscripts run charity at −0.5.
   The author ruled 2026-07-25 that sigma should be calibrated at the −0.5
   default. This gap predates the redesign; it became visible only because the
   behavior leg started checking calibration conditions. **Re-derivation is
   owed**: re-run the two calibration legs on `tests/topord_plus5` with charity
   at −0.5, interpolate onto the top-subset ETI 0.25 target, re-pin.

2. **`config_repin_hashes()` strips comments.** It rewrites channel files
   through `write_yaml()`, deleting every provenance comment in them. The wealth
   hash was re-pinned by hand for this reason. The tool that maintains the
   provenance will silently delete it.

3. **The estate valuation-bridge staleness check has no escape hatch.** The
   entry is `locked`, so a scenario cannot override it, and the model
   deliberately runs against older Tax-Data vintages for historical comparisons.
   Under a strict staleness rule that combination is fatal for ~90 runscripts.
   The redesign added an `enforcement: warn` field to handle it; without the
   redesign, the point-of-use warning in `src/sim/estate.R` remains the only
   check.

4. **`test_hidden_ledger_guards.R` is dead on `wealth`.** It references
   `WEALTH_AVOID_PUBLIC_E`, `WEALTH_CHI_PUB` and other constants that the
   assumptions layer (`8abf0f0b4`) retired when it moved those values into
   config. The test has not run successfully since. It was rewritten during
   Phase 4 and that rewrite is on `p4-attempt-1`; the rewrite is coupled to the
   three-leg accessors, so it is not directly portable, but the structure is
   reusable.

5. **`config/runscripts/private/` is on the old schema** and was deliberately not
   migrated (untracked by design). Irrelevant now that the migration is
   abandoned, but noted in case any of this is revisited.

6. **`top_tax/dials.csv` and `factorial.csv` had drifted from their generators.**
   The generators (`other/top_tax/build_dial_runs.py`, `build_factorial.py`) were
   deleted on the abandoned branch after being found out of sync with the
   runscripts they produce. **They still exist on `wealth`** and are still out of
   sync — the CSVs are the artifact that matters, the generators would overwrite
   them with something different.

---

## 5. If anyone picks this up again

The provenance problem the redesign was solving is real. A number in a CSV cell
genuinely has no citation, and the assumptions layer already on `wealth`
(`8abf0f0b4`) demonstrates the fix working for one class of value. Nothing here
argues against finishing the idea.

What went wrong was scope and sequencing, not the goal:

- **The behavior leg should not have been folded in.** Tax law and economy are
  value libraries; behavior is a plugin system. Applying one uniform "folders of
  values" shape to all three was the error, and it was invisible in the plan
  because the plan described the behavior leg in the same vocabulary as the
  other two. The economy leg flip stands on its own and gated green. It did not
  need the behavior leg to follow.

- **A plan step that contradicts a stated premise must stop the work.** The
  premise ("behavioral modules stay pluggable") and the step ("move module code
  to src/, add a registry") were both written down, in the same document, and
  the contradiction was implemented rather than raised.

- **Name things the way the neighbouring thing is named.** If `tax_law/` has
  `baseline` and `public/`, then `economy/` gets `default` and `public/`, not
  `sets/` full of machine output.

Recoverable material, all on branch `config-redesign`:

| What | Where |
|---|---|
| Resolution engine + provenance schema | `src/misc/scenario_config.R` at `cfg-p3b` |
| Runscript migrator + set naming | `other/migrations/` at `cfg-p3b` |
| Golden manifests + gate comparator | `other/config_redesign/` at `cfg-p3b` |
| Behavior registry + relocated modules | tag `p4-attempt-1` |
| Full phase-by-phase status as of P3b | `other/config_redesign/PLAN.md` at `cfg-p3b` |

Tags: `cfg-p0`, `cfg-p1`, `cfg-p2`, `cfg-p3`, `cfg-p3b`, `p4-attempt-1`.

Scratch worktrees (`/nfs/roberts/scratch/pi_nrs36/jar335/cfg_*`) and the golden
output trees (`.../model_data/Tax-Simulator/v1/golds*`) still exist and can be
deleted; the golden md5 manifests in `other/config_redesign/golden/` at
`cfg-p3b` are the durable record.

---

## Closing note, 2026-07-26

The redesign was rebuilt, on branch `config-rebuild` off `wealth`, and it
landed. Same problem, same three-leg shape, most of the same engine — the
resolution core, the gate harness and the migration tooling were all recovered
from `cfg-p3b` rather than rewritten.

What changed was the one thing this document says was fatal. The behavior leg
kept the pluggable interface: `behavior.yaml` lists module paths, the loader
takes any path that exists, and there is no registry and no list of known names.
Execution order became one pinned family sort, which let five hand-written
in-module order guards go, and 500 pre-migration behavior cells were checked to
resolve to the identical module sequence before any of them were rewritten.

The rest went the way §5 recommends: one live configuration engine at a time,
with a six-scenario byte-identical gate after every model-facing phase.

- The layout, and how to add to it: `config/scenarios/README.md`.
- The full record, including what is still owed: `REBUILD_STATUS.md` in this
  folder.
