# Archive — superseded documents

Nothing here is current. Each entry says what replaced it and why it was moved,
so a reader who finds a citation to one of these knows where to go instead.
Files are kept rather than deleted because several are cited by name in memos
and commit messages.

Moved 2026-08-18, during the documentation pass that accompanied the non-filer
residual rework and the ACS → CPS ASEC switch.

---

## Superseded designs

**`state_weights_ml_alternative.md`** (2026-07-08) — the A/B bake-off spec:
"define a second, ML-based method for constructing the split state weights so
Phase 1 is a bake-off between two approaches rather than a bet on one."

*Superseded by* `../state_weights_phase1_summary.md`. The bake-off ran, and its
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
`state_weights_alternatives.md` (2026-07-13). Its deferred ideas — dual-space
maxent, ACS-donor matched priors, sign-split calibration for the excluded
`kg_amt` series — are still live paths ranked in `state_weights_phase1_summary.md`
§7. `state_weights_fit_issues.md` is likewise kept: it is the engine root-cause
record and is cited by line number from the design memo.

## Pre-edit copies

Kept so the pre-amendment text is recoverable without digging through git, since
two of these are untracked working documents.

**`Non-Filer Proposal_2026-08-17_pre-edit.docx`** — the proposal as JI drafted it.
The live copy is `../nonfiler_residual_design_jii.docx` (renamed from
`Non-Filer Proposal.docx` on 2026-08-18, after the edit). It adds, as tracked
changes, a fourth robustness check — the 2020–2021 baseline rebate and advance-CTC
outlays against published administrative totals, which is the only check that ties
the modeled non-filer level to an observed dollar amount — plus a caveat that the
CBO and PolicyEngine comparisons bound the answer only loosely, and a note on ASEC
data sourcing. Accept or reject in Word to move between the two versions.

Beware the near-collision in names: `nonfiler_residual_design_jii.docx` (the
proposal, narrative) and `../nonfiler_residual_design.md` (the implementation memo)
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

**`STATUS_2026-08-18_pre-nonfiler.md`** — `STATUS.md` before the non-filer
workstream was added to it as item 1b. Worth keeping only because the omission was
itself notable: the workstream had been running since 2026-08-16 with no trace in
the status document, which is why the pass added it.

## Stale renders

**`nonfiler_residual_design_2026-08-17_stale-render.docx`** — a Word render of the
design memo, made before the amendments. Do not read it; it disagrees with the
`.md` on the survey question. Regenerate from `../nonfiler_residual_design.md`
when a Word copy is next needed, rather than editing this one.

Note the same hazard applies to `../state_weights_phase1_summary.docx`, which is a
render of a `.md` that is still current — it was left in place, but it will go
stale the moment the summary is amended. The `.md` is authoritative in every case.
