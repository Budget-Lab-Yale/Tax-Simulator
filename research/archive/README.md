---
title: "Archive — superseded documents"
role: index
workstream: cross-cutting
status: current
updated: 2026-08-19
sot: research/README.md
supersedes: []
superseded_by: null
---

# Archive — superseded documents

Nothing here is current. Each entry says what replaced it and why it was moved,
so a reader who finds a citation to one of these knows where to go instead.
Files are kept rather than deleted because several are cited by name in memos
and commit messages.

Naming is `{basename}_{YYYY-MM-DD}_{reason}.{ext}`, where the date is the vintage
of the *content* and `reason` comes from a closed set: `pre-{change}`,
`stale-render`, `executed`, `superseded`, `imported`. When to archive at all is in
`research/CONVENTIONS.md` — three triggers, not every revision.

Moved 2026-08-18, during the documentation pass that accompanied the non-filer
residual rework and the ACS → CPS ASEC switch; extended 2026-08-19 with the move
of the corpus to `research/`.

**Paths inside archived documents are left as they were written.** They point at
the pre-2026-08-19 locations (`other/state_tax_research/...`), which is correct
for a record of what was true then; only this README was repointed.

---

## Executed plans

**`07_ssa_inputs_plan_2026-08-19_executed.md`** (2026-08-19) — the plan to close
the last Stage D blocker: verify the manually-downloaded SSA statcomps files,
register them, and document the two families.

*Fully executed the day it was written*, and moved here 2026-08-19 because a
plan whose tasks are all done is a record, not an instruction. Its substance now
lives in three current places: the store notes
(`raw_data/SSA-{OASDI,EEDATA}-SC/NOTES.md`) say what the data mean; the verification
record is `research/state_weights/nonfiler_residual/results/ssa_input_verification.csv`; and the three
findings that changed the design — the anchor is the **51-jurisdiction sum** not
`All areas`, **OASDI cannot support a state 65_74/75p split**, and **EEDATA is a 1%
sample** — are folded into `research/state_weights/nonfiler_residual_design.md` §3.1 and
`research/state_weights/plan.md`. The one task it left open, writing the two
readers, is task A1 in the to-do.

---

## Superseded designs

**`research/archive/state_weights_ml_alternative.md`** (2026-07-08) — the A/B bake-off spec:
"define a second, ML-based method for constructing the split state weights so
Phase 1 is a bake-off between two approaches rather than a bet on one."

*Superseded by* `research/state_weights/state_weights_phase1_summary.md`. The bake-off ran, and its
premise did not survive contact with the data: multi-series IPF was proven
**structurally** invalid (one multiplier against ~21 constraints per cell), not
merely worse, so "Approach A vs Approach B" collapsed into "prior-only vs
joint-fit." The summary records the reframing, the 13-config sweep, and the
adoption of config 7 (counts-IPF prior → gradient, β=1e-4, 95.3% within 2%,
MARD 0.43%).

*Still useful for:* the shared-invariant statement (§0) and the derivation of the
exponential-tilting objective, both of which the production engine still
implements.

*Not superseded, deliberately kept in the main directory:*
`research/state_weights/notes/state_weights_alternatives.md` (2026-07-13). Its deferred ideas — dual-space
maxent, ACS-donor matched priors, sign-split calibration for the excluded
`kg_amt` series — are still live paths ranked in `research/state_weights/state_weights_phase1_summary.md`
§7. `research/state_weights/notes/state_weights_fit_issues.md` is likewise kept: it is the engine root-cause
record and is cited by line number from the design memo.

## Pre-edit copies

Kept so the pre-amendment text is recoverable without digging through git, since
two of these are untracked working documents.

**`Non-Filer Proposal_2026-08-17_pre-edit.docx`** — the proposal as JI drafted it.
The live copy is `research/docx_sources/nonfiler_proposal_jii.docx` (renamed from
`Non-Filer Proposal.docx` on 2026-08-18, after the edit). It adds, as tracked
changes, a fourth robustness check — the 2020–2021 baseline rebate and advance-CTC
outlays against published administrative totals, which is the only check that ties
the modeled non-filer level to an observed dollar amount — plus a caveat that the
CBO and PolicyEngine comparisons bound the answer only loosely, and a note on ASEC
data sourcing. Accept or reject in Word to move between the two versions.

Beware the near-collision in names: `research/docx_sources/nonfiler_proposal_jii.docx` (the
proposal, narrative) and `research/state_weights/nonfiler_residual_design.md` (the implementation memo)
are different documents.

**`nonfiler_residual_design_jii_2026-08-18_pre-mok.docx`** — the proposal carrying only
the first round of tracked changes (the fourth robustness check and the ASEC sourcing
note), before the literature pass. The live copy adds a second round: Mok (2017)
replaces Cilke (1998) as the below-threshold model in §3.1, with the reasoning for why a
survey model is still right given our data position and what bias it carries; and §6's
dependent/MFS deferral is challenged on the post-TCJA threshold evidence. Both rounds
are tracked and attributed to Claude, so rejecting all changes returns the original
draft.

**`nonfiler_residual_design_2026-08-16_pre-asec.md`** also predates the §3.2 rewrite —
see below.

**`nonfiler_residual_design_2026-08-16_pre-asec.md`** — the design memo before the
2026-08-18 changes. The live copy carries a Change log at the top. Substantive
differences: **§3.2 was rewritten outright** (Mok 2017 replaces Cilke 1998 as the
below-threshold model; a new §3.2.0 explains why a survey model is still right for our
data position; §3.2.4 records the ~17% ASEC income-understatement bias and its
mitigations; §3.2.5 reopens the dependent/MFS scope decision), plus the survey switch
(the memo originally built the filing model on the ACS and asserted "no new IPUMS pull
is needed for anything in this memo"), the added §5.4 federal validation section, the
renumbered §7.1 roadmap, and the §8 revisions.

**`STATUS_2026-08-18_pre-nonfiler.md`** — `research/STATUS.md` before the non-filer
workstream was added to it as item 1b. Worth keeping only because the omission was
itself notable: the workstream had been running since 2026-08-16 with no trace in
the status document, which is why the pass added it.

## Stale renders

**`nonfiler_residual_design_2026-08-17_stale-render.docx`** — a Word render of the
design memo, made before the amendments. Do not read it; it disagrees with the
`.md` on the survey question. Regenerate from `research/state_weights/nonfiler_residual_design.md`
when a Word copy is next needed, rather than editing this one.

The hazard this section describes is now handled by convention rather than by
warnings. `state_weights_phase1_summary.docx` — an undated render of a `.md` that
had since been amended, exactly as predicted here — was **deleted** on 2026-08-19
during the move to `research/`. It was never committed, so git holds nothing that
the `.md` does not. Word copies are now cut by
`research/tools/render_release.R`, which stamps the render date, branch and commit
into the document and writes it to `research/releases/YYYY-MM-DD_<slug>.docx`; a
reader can always tell what a release was made from, and an undated render beside
its source is no longer possible. The `.md` is authoritative in every case.

---

## Added 2026-08-19, with the move to `research/`

### Executed plans

**`HARNESS_RERUN_2026-08-11_executed.md`** (2026-08-11) — a queued batch of
cross-model reruns and diagnostics (sections A/B/C plus housekeeping) for the
harness machine.

*Fully executed.* It said so itself — "**EXECUTED 2026-08-11** … Do not re-run as
written" — while still sitting in the live harness directory next to the README,
which is exactly the state the "a plan whose tasks are all done is a record" rule
exists to prevent. Its outcomes are in
`research/state_tax/cross_model/results/` and the tracker.

### Superseded plans

**`nonfiler_state_weights_todo_2026-08-19_pre-merge.md`** (2026-08-19) — the
non-filer + state-weights plan review and implementation to-do, as it stood before
the merge: Part 1 review, Part 1.5 decisions, Part 2's P/A–H task list, Part 3
critical path.

*Superseded by* `research/state_weights/plan.md`, which is this document plus the
imported plan's risk register, effort table and ops notes. Nothing was dropped —
the merge was additive on this side, because **this document won on every fact**:
its claims had been checked line-by-line against the code and the shared store on
2026-08-19, and its own rule was "where a memo and the tree disagree, the tree
wins."

*Still useful for:* seeing what the merge actually changed, in one diff.

**`nonfiler_unified_plan_2026-08-18_imported.md`** (2026-08-18) — the fuller of the
two same-scope plans: Context, Pre-flight, Steps 1–7, Discrepancies, Verification,
Sequence and effort, Risk register, Ops notes.

*Superseded by* `research/state_weights/plan.md`. **Imported from outside version
control** — it had been living in `~/.claude/plans/`, cited by nothing in the repo,
which is why the repo could declare a 595-line to-do the "single operational entry
point" while a 1,163-line same-scope plan existed elsewhere. Kept whole because it
is the origin of `research/state_weights/nonfiler_federal_validation.md` (its Step
4, extracted with the `## Verification` block appended) and of the merged plan's
risk register.

Three things in it were already wrong when it was imported, and are corrected in
the merged plan: it cites `Non-Filer Proposal.docx` (renamed 2026-08-18), it treats
Cilke (1998) as the below-threshold model (Mok 2017 replaced it on 2026-08-18), and
it calls the design memo 579 lines (1,079 now). Its `## Discrepancies between the
proposal and the implementation record` section is carried live into
`research/state_weights/notes/nonfiler_proposal_rewrite_plan.md`, since those are
corrections to make in the **proposal**, not in the method of record.

### Renamed, not archived

`nonfiler_residual_design_jii.docx` → **`research/docx_sources/nonfiler_proposal_jii.docx`**.
The near-collision this README warned about twice — one letter-group apart from
`nonfiler_residual_design.md`, a different document — is gone. Word-native
documents now live in `research/docx_sources/`, which is the one place the
"Markdown is the source of truth" rule does not apply, because these were never
Markdown.

`docs/Income.docx` → **`research/docx_sources/income_memo_affordability.docx`**.
The Affordability-Index income memo that prompted the non-filer rework. It had been
deleted from the working tree while still cited as the originating document at
`nonfiler_residual_design.md:38`; restored and moved so the citation resolves. It
is a **copy taken in as an input** — the Affordability-Index repo owns the
canonical version.
