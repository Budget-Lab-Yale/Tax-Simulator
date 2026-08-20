---
title: "Research corpus index"
role: index
workstream: cross-cutting
status: current
updated: 2026-08-20
sot: self
supersedes: []
superseded_by: null
---

# `research/` — the state-tax research corpus

Design records, plans, evidence and reviews for the state-tax work: per-state law
encoding, and split state weights plus the non-filer rework. **If a document is not
reachable from this file, it does not exist.**

Shipped model documentation lives in `docs/`, not here. The cross-model harness *code*
lives in `src/tests/state/cross_model/`; only its *records* are here.

## Where to start

| You want | Read |
|---|---|
| What is true today | [`STATUS.md`](STATUS.md) |
| Why a thing is the way it is | the workstream's `role: method` document |
| What happens next | the workstream's **one** `role: plan` document |
| A settled argument, and why | [`decisions_log.md`](decisions_log.md) |
| The naming and front-matter rules | [`CONVENTIONS.md`](CONVENTIONS.md) |

## The two workstreams

### `state_tax/` — per-state law encoding

| Role | Document |
|---|---|
| **plan** | [`state_tax/plan.md`](state_tax/plan.md) |
| procedure | [`state_tax/state_parameter_workflow.md`](state_tax/state_parameter_workflow.md) — defines the status vocabulary the tracker uses |
| tracker | [`state_tax/state_parameter_rollout.csv`](state_tax/state_parameter_rollout.csv) — **the single per-state status surface** |
| review | `state_tax/CODE_REVIEW_2026_07_17.md`, `state_tax/STATE_ENCODING_REVIEW_2026_08_11.md` — point-in-time, see their `true_as_of:` |
| evidence | [`state_tax/cross_model/`](state_tax/cross_model/) — per-state triage reports, filed bug packets, divergence records |
| notes | [`state_tax/notes/`](state_tax/notes/) — cross-state surveys, CA analyses, deferred designs |

### `state_weights/` — split state weights and the non-filer rework

One workstream: the non-filer rework lands before the Phase 1 weights swap-in, so
they share a plan.

| Role | Document |
|---|---|
| **plan** | [`state_weights/plan.md`](state_weights/plan.md) |
| method — non-filers | [`state_weights/nonfiler_residual_design.md`](state_weights/nonfiler_residual_design.md) |
| method — the fit | [`state_weights/state_weights_phase1_summary.md`](state_weights/state_weights_phase1_summary.md) |
| procedure | [`state_weights/nonfiler_federal_validation.md`](state_weights/nonfiler_federal_validation.md) |
| evidence | [`state_weights/nonfiler_residual/04_findings.md`](state_weights/nonfiler_residual/04_findings.md) + `nonfiler_residual/results/` |
| notes | [`state_weights/notes/`](state_weights/notes/) |
| scripts | [`state_weights/scripts/`](state_weights/scripts/) — the Phase 1 harness (`sweep_`, `validate_`) and drivers |

## Folder rules

| Folder | Rule |
|---|---|
| `<workstream>/` | One line of work. Holds **exactly one `role: plan`** plus any number of `method` / `procedure` / `review` documents. Flat — roles are metadata, not directories. |
| `<workstream>/notes/` | Everything that is not plan, method, procedure or review: surveys, one-off analyses, deferred designs. A note with `status: open` **must** be cited from its workstream's plan. |
| `<workstream>/scripts/` | Research drivers. They may `source()` `src/`; **nothing in `src/` may invoke them.** That asymmetry is the test for where a script belongs. |
| `<workstream>/<bundle>/` | Numbered scripts whose outputs must stay adjacent to them (`nonfiler_residual/`). |
| `source_packets/` | One primary-source packet per jurisdiction, lowercase `{state}.md`, matching the config tree's lowercase-postal convention. |
| `raw/` | Verbatim research output as produced. **Append-only** — never edited, never archived, never superseded. |
| `docx_sources/` | Word-native documents that are **not** renders: authored in Word, carrying tracked changes. The Markdown-is-truth rule does not apply to these. |
| `tools/` | The release renderer, its style reference, and one manifest per release slug. |
| `releases/` | `YYYY-MM-DD_<slug>.docx`. Committed snapshots for outside review. **Never edited, never a source.** |
| `archive/` | Nothing here is current. Every entry is justified in [`archive/README.md`](archive/README.md). |

## Cutting a Word release

Markdown is the source of truth; a release is a dated snapshot of it for someone
outside the repo. See `CONVENTIONS.md` for the manifest format.

```bash
Rscript research/tools/render_release.R state_weights_plan --dry-run
Rscript research/tools/render_release.R state_weights_plan
```

## Drift checks

One command, ten checks. Run it before pushing documentation changes.

```bash
Rscript research/tools/check_conventions.R          # exits 1 on any finding
Rscript research/tools/check_conventions.R -v       # also say what each check covered
Rscript research/tools/check_conventions.R --check 6 # just one check
Rscript research/tools/check_conventions.R --report-only   # print, always exit 0
```

| # | Check |
|---|---|
| 1 | exactly one `role: plan` per workstream |
| 2 | front matter present on every document (artifact directories exempt) |
| 3 | `role` / `status` / `workstream` drawn from the closed vocabularies |
| 4 | front-matter `updated:` not behind the file's own last commit date |
| 5 | a note with `status: open` is cited from its workstream's plan |
| 6 | cited paths resolve |
| 7 | `sot:` and `supersedes:` targets exist |
| 8 | nothing outside `archive/` cites the pre-2026-08-19 locations |
| 9 | archive names match the convention and each has an `archive/README.md` entry |
| 10 | no living document declares itself superseded |

Check 4 catches the commonest real failure: a document edited without its header
bumped. Check 5 is the one that matters most, because an open note no plan cites
is how outstanding work goes missing — the failure this whole tree exists to fix.

Check 6 needs a way to tell a moved file from one that lives in another repo or
does not exist yet. Those go in `research/tools/known_external_paths.csv` with a
`kind` and a reason, so an unresolvable citation is always a reviewed decision
rather than a silent unknown.

**What it does not do:** it never reads prose for meaning. Whether `STATUS.md`
agrees with a plan is what `sot:` is for; a checker that tried would produce
noise, and noise is how a check gets ignored.
