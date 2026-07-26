# Config rebuild v2 — where the work stands

*Written 2026-07-26 at the end of an autonomous overnight session on branch
`config-rebuild`, off `wealth` at `324a7cd38`. Plan of record:
`~/.claude/plans/cheerful-zooming-star.md`.*

---

## Done

| Phase | Commit | Gate |
|---|---|---|
| 0 — harness recovery | `5a12e07d8` | n/a (no model code) |
| 1 — excess-growth rip-out | `66c37a14c` | S1 byte-identical vs golds1 |
| 2 — resolution engine, dormant | `28ecf4f33` | 45/45 unit tests |
| 3a — tax law default/alternatives | `09c7e54f4` | S1 byte-identical vs golds1 |
| 3b — economy leg live, assumptions layer deleted | `749a5a97a` | S1 byte-identical; S2/S3/S4/S6/S7 launched, see below |

## Not started

Phase 3c (runscript library migration), Phase 4 (behavior leg flip), Phase 5
(calibration stamps), Phase 6 (docs sweep). CLAUDE.md was already rewritten for
the three-leg layout as part of 3b, so Phase 6's doc work is partly done.

---

## The gate as it stands

S1 (baseline, pct 0.05, `main.R` path) passed byte-identical against `golds1`
after each of phases 1, 3a and 3b. The five full-sample SLURM runs for 3b —
S2 `rebate_2025`, S3 `tests/multi_module_smoke`, S4
`tests/corp_kgwealth_verify`, S6 `wealth_tax`, S7 `estate_2009` — were launched
against `/nfs/roberts/scratch/pi_nrs36/jar335/cfg_rb_p3b` at the end of the
session and had not finished. **Check them first.**

```bash
ls /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/rb_p3b_s*
for n in 2 3 4 6 7; do
  bash other/config_redesign/gate_diff.sh \
    /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/rb_p3b_s$n \
    /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/golds$n
done
```

Caveat on the runs in flight: the `cfg_rb_p3b` worktree holds the Phase 3b
`src/` and `config/` (rsynced) on top of a Phase 3a checkout, so its `other/`
and `CLAUDE.md` are one commit behind. Neither affects model output, and
`code_version.csv` is excluded from the comparison. If a gate fails, re-run it
from a clean checkout of `749a5a97a` before believing the failure.

Two sanctioned exclusions were added to `gate_diff.sh` this session, both
narrow:

- `supplemental/excess_growth_offset.csv` is golden-only now. The comparator
  first asserts every golden copy has `income_factor == 1` in every year, so
  the exclusion cannot hide a real change.
- `assumptions.csv` is golden-only; the candidate writes `scenario_config.csv`
  in its place. `mapping_check.py` is what confirms the two carry the same
  values — **it has not been run yet against a 3b vintage.** Do that.

## Calls made during the session that the author should look at

1. **Dependency hashes were re-pinned** in `config/scenarios/economy/default/`
   (20 of them). The `assumption()` → `economy_param()` rename changed the
   content of `src/sim/kg/*`, `src/sim/sigma_conversion.R`,
   `src/sim/wealth_dynamics.R` and the evasion module, all of which sit in some
   calibrated entry's `invalidated_by`. The rename is behavior-preserving and
   the S1 gate confirmed byte-identity before the re-pin was kept, which is the
   order CLAUDE.md requires. No calibrated value moved. The re-pin was done by
   surgical text substitution on the hash lines, not by
   `config_repin_hashes()`, because that function round-trips the YAML through
   `write_yaml` and would strip every comment in the file. **That hazard is
   still live** — Phase 5 retires the function, but until then do not call it.

2. **The engine gained dated waivers on alternatives.** The plan called for
   `waiver: {date, reason}` in the pointing file; the recovered engine only had
   `acknowledged` on default entries. `config_resolve()` now accepts an entry
   in an alternative that carries a `waiver` block and no `value`, and
   `config_check_staleness()` skips those entries with a banner. The
   `multi_module_smoke` alternative uses one, because it pins Tax-Data
   `2026050315` on purpose (ruling 11).

3. **`config/batch-submissions/` was deleted** and `other/slurm_builder.R` now
   writes generated batch scripts and their SLURM logs to
   `/nfs/roberts/scratch/pi_nrs36/jar335/Tax-Simulator-batch-submissions/`. The
   old paths in that script were already inconsistent (some project, some
   scratch); they are all scratch now. The script is not exercised by any gate.

4. **The behavior leg has a placeholder default file**
   (`config/scenarios/behavior/default/placeholder.yaml`) so the engine has a
   loadable default layer while only the economy leg is live. Phase 4 replaces
   it with `behavior.yaml`. Nothing reads it.

5. **kg, sigma and evasion live in the economy leg temporarily**, with a header
   in each file saying so. That is what the plan asked for (exactly one live
   engine per phase); Phase 4 inlines the module-only parameters and Phase 5
   moves kg and sigma into calibration stamps.

6. **`wealth_financing_params.yaml` was absorbed** into the economy leg's
   `wealth.yaml` as three structural entries (`n_pctiles`, `fmax`,
   `r_total_additive_delta`). The `r_total` source string is now hardcoded in
   `wealth_dyn_load_params()` rather than configured, because it was never
   anything but `macro_gdp_per_capita`.

7. **`estate.valuation_bridge` reads warn-level stale** on every run right now:
   `other/estate_tax/write_frozen_params.R` changed when its output path was
   repointed to `config/calibrations/estate/bridge.yaml`. Its hash was
   re-pinned too, so this should be clean at `749a5a97a`; if it reappears,
   the tree being run is behind.

## Next steps, in order

1. Read the five gate results. If any fails, the diff will name files; the
   phases are small and separately committed, so bisecting is cheap.
2. Run `mapping_check.py` on a 3b vintage against its golden to confirm
   `scenario_config.csv` covers what `assumptions.csv` did.
3. Run the equivalence check (`other/config_redesign/equivalence_check.R`) —
   old parser in a detached pre-3b worktree vs new resolution over the migrated
   gate runscripts. It was recovered but not adapted to the new
   `alternative =` argument name or the removed `excess_growth` fields; it
   still references them at line 170.
4. Phase 3c: only the six gate runscripts were migrated. **Every other live
   runscript is still on the old schema and will now hard-error at parse**,
   with a message naming the replacement. That is the intended end state for
   the archive, but the ~30 live runscripts listed in the plan need migrating
   by hand, and the three generators (`build_dial_runs.py`,
   `build_factorial.py`, `build_revmax_grid.py`) need the byte-for-byte
   idempotence proof before they are taught the new schema.
5. Phases 4–6 as written in the plan.

## Things the plan asks for that are NOT done in 3b

- `config/scenarios/README.md` does not exist yet, but the parser's error
  message points at it. Write it in 3c.
- `config/runscripts/archive/README.md`'s dangling pointer is still dangling.
- The unit tests cover the alternatives shape and the reserved name, but not
  the new waiver path. Add a test.
