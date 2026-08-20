---
title: "Research documentation conventions"
role: procedure
workstream: cross-cutting
status: current
updated: 2026-08-20
sot: self
supersedes: []
superseded_by: null
---

# Conventions for the `research/` tree

Three rules generate everything below.

1. **One job per document.** A document's job is its `(role, subject)` pair. Two
   `method` documents on different subjects are fine; two `plan` documents in one
   workstream never are.
2. **Markdown is the source of truth.** `.docx` is either a dated render
   (`releases/`) or a Word-native document that was never Markdown (`docx_sources/`).
   Nothing else.
3. **Amend in place; snapshot at milestones.** The living file carries the current
   truth and a `## Revision history` tail. A dated copy goes to `archive/` only at a
   milestone — see *When to archive*.

## Naming

| Kind | Form | Example |
|---|---|---|
| Living document | `snake_case.md`, **never a date in the filename** | `nonfiler_residual_design.md` |
| The one plan per workstream | `plan.md` | `state_weights/plan.md` |
| Review (inherently point-in-time) | `{SUBJECT}_REVIEW_{YYYY-MM-DD}.md` | `STATE_ENCODING_REVIEW_2026_08_11.md` |
| Archived snapshot | `{basename}_{YYYY-MM-DD}_{reason}.{ext}` | `nonfiler_residual_design_2026-08-16_pre-asec.md` |
| Release | `{YYYY-MM-DD}_{slug}.docx` | `2026-08-19_state_weights_plan.docx` |
| Source packet | lowercase `{state}.md` | `az.md` |
| Numbered bundle artifact | `{NN}_{verb}_{object}.{R,md}` | `04_findings.md` |

Dates live in front matter, not in filenames — except for the three kinds above that
*are* inherently point-in-time (reviews, archived snapshots, releases).

**Archive `{reason}` is a closed vocabulary:** `pre-{change}` · `stale-render` ·
`executed` · `superseded` · `imported`. `imported` is for documents that arrive from
outside version control.

**Cite research documents by repo-root-relative path** (`research/state_weights/plan.md`),
not by bare filename. A bare name cannot be checked mechanically, and the 2026-08-19
reorganization had to repair ~200 of them.

## Front matter

Line 1 of every living research `.md`. Pandoc parses it as document metadata and drops
the non-standard keys, so none of it appears in a render.

```yaml
---
title: "Non-Filer Estimation — Residual-Methodology Redesign"
role: method
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---
```

| Field | Rule |
|---|---|
| `title` | Human title. Overridden by `-M title=` when the document is one section of a release. |
| `role` | Closed set: `plan` · `method` · `procedure` · `evidence` · `notes` · `review` · `status` · `index` |
| `workstream` | `state_tax` · `state_weights` · `cross-cutting` |
| `status` | `current` · `open` · `deferred` · `historical` · `frozen` · `executed` · `superseded` |
| `updated` | `YYYY-MM-DD`. **Bump it in the same commit as any substantive edit** — drift check 3 catches this. |
| `true_as_of` | Reviews and frozen documents only: the state of the tree the document describes. This is what lets a superseded count stay in the tree honestly. |
| `sot` | The document that wins if this one disagrees with it. `self` for a plan. |
| `supersedes` | List of paths, usually into `archive/`. Empty list, never omitted. |
| `superseded_by` | `null`, or the path that replaced this document. Set at the moment of archiving. |

### What each role means

- **`plan`** — what is decided, what is next, what is blocked, the critical path.
  Exactly one per workstream; `sot: self`. **Start here.**
- **`method`** — the design of record: the *why*, and each decision with its evidence.
  No task lists.
- **`procedure`** — a runbook: an ordered sequence someone executes. Distinct from a
  plan because it sequences steps *within* one task and does not go stale when
  priorities move.
- **`evidence`** — frozen findings. Amended only by appended provenance notes, never by
  revising a number in place.
- **`notes`** — surveys, deferred designs, feature-scoped plans, one-off analyses.
- **`review`** — a dated critical read of a snapshot of the tree. Immutable once
  written; carries `true_as_of`.
- **`status`** — what is true today, as pointers rather than summaries.
- **`index`** — a map of other documents.

## When to archive

Copy the living file to `archive/{basename}_{YYYY-MM-DD}_{reason}.{ext}` and add an
entry to `archive/README.md` in the house style (what it was, *Superseded by*, *Still
useful for*) on exactly three triggers:

1. an external review going out, so the version discussed is recoverable;
2. a whole section being rewritten rather than amended;
3. the document becoming superseded or fully executed — *a plan whose tasks are all
   done is a record, not an instruction.*

Nothing else. Git holds every intermediate revision already.

## Cutting a Word release

A release combines one or more Markdown sources into a single dated `.docx` for someone
outside the repo. It stamps the render date, branch and commit into an *About this
document* table, so a reader can always tell what it was made from.

```bash
Rscript research/tools/render_release.R <slug> [--date YYYY-MM-DD] [--dry-run]
```

`<slug>` names a manifest at `research/tools/releases/<slug>.yaml`:

```yaml
title: "A state-weight-inclusive tax model with an updated non-filer pull"
subtitle: "Implementation plan and current status"
reference_doc: research/tools/reference.docx
sections:
  - {heading: "Where the work stands", source: research/STATUS.md, fence: nonfiler-status}
  - {heading: "The plan",              source: research/state_weights/plan.md}
```

To release part of a document rather than all of it, fence the region:

```markdown
<!-- release:begin nonfiler-status -->
...
<!-- release:end nonfiler-status -->
```

Fences work where headings cannot (the status block is a numbered list item, not a
heading) and survive renumbering. Pandoc drops HTML comments from `.docx`, so they are
invisible in the output.

Comments on a release are welcome; **edits to it are not carried back**. They are
applied to the Markdown and a new release is cut.

### Renders are no longer a Knit-button job

Removing the per-file `output: word_document` YAML block means RStudio's Knit button no
longer produces Word from these documents. That is deliberate: an undated render
sitting beside its source is exactly how
`state_weights_phase1_summary.docx` and `state_tax_implementation_plan.html` went
stale unnoticed. Use `render_release.R`.

## Enforcing all of this

```bash
Rscript research/tools/check_conventions.R
```

Ten checks over the whole tree, exiting 1 on any finding: the vocabularies above,
one plan per workstream, front matter present on documents, `updated:` not behind
the file's last commit, open notes cited from their plan, cited paths resolving,
`sot`/`supersedes` targets existing, no pre-reorganization citations, archive
naming and `archive/README.md` coverage, and no living document declaring itself
superseded. `research/README.md` lists them; `--report-only`, `--check N` and `-v`
are the flags.

Two things it cannot do, by design. It never reads prose for meaning — that is
what `sot:` is for. And it cannot tell a moved file from one in another repo or
one that does not exist yet, so those are declared in
`research/tools/known_external_paths.csv` with a kind and a reason.

## Glossary of renamed upstream things

| Now | Formerly | Note |
|---|---|---|
| `IRS-Ind` | `IRS-GEO` | The HT2 mirror repo, renamed 2026-08. `raw_data/IRS-GEO` is a symlink on the cluster, so old data paths still resolve; documentation should say `IRS-Ind`. |
| `research/docx_sources/nonfiler_proposal_jii.docx` | `Non-Filer Proposal.docx`, then `nonfiler_residual_design_jii.docx` | JI's narrative proposal. The middle name collided with the implementation memo `nonfiler_residual_design.md`; the current name does not. |
