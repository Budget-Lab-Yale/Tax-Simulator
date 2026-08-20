---
title: "State-tax research decisions log"
role: status
workstream: cross-cutting
status: current
updated: 2026-08-20
sot: self
supersedes: []
superseded_by: null
---

# Decisions log — state-tax research

Records the operative decisions for the state-tax workstreams and their rationale, so a
question settled once is not re-argued from a stale document. Add a dated row when a
decision is made, changed, or revisited. Same format as the sibling Affordability-Index
repo's `docs/decisions_log.md`, so the two read alike.

Legend — **Status:** `set` (decided, reflected in the tree) · `provisional` (decided for
now, revisit) · `open` (not yet resolved).

The **S-series** covers both workstreams. Where a row settles *which document owns a
claim*, the substantive question is unchanged: it moves to one home, it is not re-decided.

---

## Contradictions closed 2026-08-19

These five were live disagreements between documents, found by an inventory of the corpus
before the move to `research/`. Each is now stated once, in one place.

| ID | Decision | Status | Rationale / notes |
|----|----------|--------|-------------------|
| S1 | The **−7% filer-bias figure has two distinct roles** and both are live: it is *retired as the reconciliation basis*, and it *stands as an acceptance metric* for the non-filer rework. The acceptance definition lives in `research/state_weights/plan.md` §C8; every other site cites it rather than restating it. | set | `state_weights_fit_issues.md` recorded "**Superseded as the reconciliation basis (2026-07-13)**: per JI, reconciliation now runs at the individual level (`compare_individuals_acs_irs()`), which is model-free" — while the design memo §3.2.6, plan §C8 and `STATUS.md`'s headline all used the same number as the bar the rework must close. Both readings are correct about different things: reconciliation is how we *compare* ACS to IRS, acceptance is what the filing model must *achieve*. Nothing substantive changes; the definition stops being maintained in four places. |
| S2 | **Phase 1 closes after the non-filer rework**, not before (JI, 2026-08-16). | set | `state_weights_phase1_summary.md:260-263` is the close-out checklist that three documents cite by line number, and it predates the sequencing decision — a reader who follows the citation to the authoritative checklist got the pre-decision instruction. Now carries a dated amend-in-place note pointing at the plan. `STATUS.md` item 1 already said "Do not close Phase 1 first." |
| S3 | The **Phase 1 §4 comparison harness is built**: `research/state_weights/scripts/sweep_state_weights.R` (part 1, hyperparameter sweep) and `validate_state_weights.R` (part 2, decision-relevant validation). `STATUS.md` item 1 points at them. | set | Both scripts self-describe in their headers as "Phase 1 §4 comparison harness, part 1/part 2", but `STATUS.md` item 1 and `state_weights_fit_issues.md` cited "the §4 comparison harness" whose only *spec* is `research/archive/state_weights_ml_alternative_2026-07-08_superseded.md` — an archived document whose premise the data refuted. The spec stays cited as the origin; the built thing is what you run. This also fixes `src/data/state_weights.R:30`, which had pointed at the pre-archive path since 2026-08-18. |
| S4 | The HT2 mirror is **`IRS-Ind`**. Documentation says `IRS-Ind`; `raw_data/IRS-GEO` remains a working symlink on the cluster, so existing data paths are not edited as part of a documentation change. | set | The repo was renamed 2026-08 and the code followed (`state_weights.R:57`), but three documents still named `IRS-GEO`, including one giving the GitHub URL. A glossary row in `research/CONVENTIONS.md` lets old citations resolve. **`validate_state_weights.R:151` still reads `raw_data/IRS-GEO/state/HT2` and is deliberately left alone** — repointing a data path inside a documentation commit is how a silent break hides. It is a task in the plan. |
| S5 | **Encoding coverage is 51/51 jurisdictions as of 2026-08-19.** The 9 / 21 / 22 / 30-state figures in five documents are point-in-time snapshots, not competing claims, and each now carries `true_as_of:` in its front matter. | set | `STATUS.md` says 51 encoded and "nothing is unstarted"; `STATE_ENCODING_REVIEW_2026_08_11.md` (30 states) asserts "by that bar **no state is complete**", `CODE_REVIEW_2026_07_17.md` says 22, `elderly_retirement_provisions.md` 21, `child_credit_survey.md` 9 — and three were cited as live companions by `STATUS.md` itself. Reviews are immutable records of a moment; the fix is to date them, not to rewrite them. |

## Per-state status

| ID | Decision | Status | Rationale / notes |
|----|----------|--------|-------------------|
| S6 | **`research/state_tax/state_parameter_rollout.csv` is the single per-state status surface.** Source packets carry `Status: see ../state_parameter_rollout.csv` and a `Last updated:` date; the harness README carries no done-list; reviews carry `true_as_of:`. | set | Five surfaces were tracking per-state status in five incompatible vocabularies: the CSV, the 51 packets' free-text `Status:` lines (five formats, including `nd.md` and `sc.md` transcribing the CSV's own four columns inline), the 39 cross-model report verdicts, the harness README's embedded "Status as of 2026-07-19" done-list, and a review appendix. The CSV wins because `src/tests/test_state_tax_law.R` already validates the code against it, and `state_parameter_workflow.md` §Validation gates already defines its vocabulary. The reports keep their per-state verdicts — they are the *evidence* a row is `done`, which is a different thing from the row. |

## Documentation scheme

| ID | Decision | Status | Rationale / notes |
|----|----------|--------|-------------------|
| S7 | Research documents live in a top-level **`research/`** tree, one workstream per directory, **exactly one `role: plan` per workstream**, with roles as front-matter metadata rather than folders. `docs/` keeps the shipped model documentation. | set | `other/state_tax_research/` had reached 212 tracked files across ten filename patterns, with 20 documents tracking outstanding work. Conventions and drift checks in `research/CONVENTIONS.md` and `research/README.md`. |
| S8 | **Markdown is the source of truth; Word documents are dated snapshots.** Renders are cut by `research/tools/render_release.R` into `research/releases/YYYY-MM-DD_<slug>.docx`, stamped with the render date, branch and commit. Comments on a release are welcome; edits are applied to the Markdown, not carried back. Word-*native* documents (`research/docx_sources/`) are exempt — they were never Markdown. | set | Two undated renders had gone stale beside their sources unnoticed: `state_tax_implementation_plan.html` (six weeks stale, and it had lost the amendment saying its own §2.1 was superseded) and `state_weights_phase1_summary.docx` — whose staleness `archive/README.md` had predicted in writing and which happened anyway. Per-file rmarkdown `output:` blocks are removed; the Knit button is no longer the mechanism. |
| S9 | A living document is **amended in place** and carries a `## Revision history` tail. A dated snapshot goes to `research/archive/` on exactly three triggers: an external review going out, a whole section being rewritten rather than amended, or the document becoming superseded or fully executed. | set | Git holds every intermediate revision, so snapshotting each edit would add clutter without adding recoverability. The three triggers are the cases where a *named*, browsable version matters. Naming follows the convention already in use: `{basename}_{YYYY-MM-DD}_{reason}.{ext}`. |
| S10 | The **unified non-filer + state-weights plan is one in-repo document**: `research/state_weights/plan.md`. It merges the 2026-08-18 out-of-repo plan with the 2026-08-19 to-do; the to-do wins on every fact. Both predecessors are archived. | set | The fuller plan (1,163 lines) lived at `~/.claude/plans/review-the-new-non-filer-starry-rivest.md` — outside version control, cited by nothing in the repo, and stale in three specific ways, while the repo declared a 595-line to-do the "single operational entry point". Two same-scope documents is the pattern that caused the drift the corpus already had. Step 4 of the imported plan became `nonfiler_federal_validation.md` (`role: procedure`) rather than a second plan. |
| S11 | The **cross-model harness is split by what the test reaches for**: `src/tests/state/cross_model/` holds the PolicyEngine driver, its pinned requirements and the machine-read `known_differences.csv`; everything else — the CLI driver, results, reports, cache, bug reports, companion records — is in `research/state_tax/cross_model/`. | set | A `src/` test may not depend on a path in a tree whose defining property is that documents get archived and moved. Two constraints shaped where the line falls: nothing non-`.R` could previously live under `src/` at all (the recursive source walk in `main.R`, `slurm/{setup,common}.R` and the harness driver sourced *every* file), so those four walks are now filtered to `.R`; and **no executable R may live in `src/tests/state/cross_model/`**, because `run_cross_model.R` does its own recursive source of `src/` and would recurse infinitely if the walk picked it up. Hence the driver sits in `research/` while the driver's inputs sit in `src/`. |

---

## Revision history

- **2026-08-19** — Created with S1–S11 during the move of the research corpus to
  `research/`. S1–S5 close contradictions found by the pre-move inventory; S6–S11 record
  the scheme itself.
