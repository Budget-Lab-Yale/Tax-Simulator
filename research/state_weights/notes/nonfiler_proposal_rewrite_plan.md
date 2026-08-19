---
title: "Rewriting the non-filer proposal for co-authors"
role: notes
workstream: state_weights
status: open
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Rewrite `research/docx_sources/nonfiler_proposal_jii.docx` as a co-author-facing methodology proposal

## Context

`research/docx_sources/nonfiler_proposal_jii.docx` is John's in-progress rewrite of the internal design memo `nonfiler_residual_design.docx` (whose full technical content survives in the repo as `research/state_weights/nonfiler_residual_design.md`). John has rewritten the front matter — Overview, §1 Current state, §2 Proposed residual methodology (§2.1 targets, §2.2 filing model) — in co-author-facing prose, and left three TODOs plus a specified outline for restructuring the rest. Everything below the big TODO is leftover old-memo material (including stray half-deleted fragments and an "OLD" block at the end) that must be replaced.

Since the old memo was written, the Stage-D diagnostic harness **was actually run** (`research/state_weights/nonfiler_residual/04_findings.md`, findings F1–F7, decisions D1–D6 resolved), so the rewrite can cite real evidence instead of speculation.

**Decisions confirmed with John:**
1. **CPS ASEC is deliberate** in §2.2 — Section 3 (National) is written CPS-first; ACS enters only via the state allocation in Section 4.
2. **Cite key Stage-D numbers** as motivating evidence (a handful of headline findings, not a diagnostics report).
3. **Drop all implementation detail** (file:line refs, script specs, decision tables, sequencing) — one-line pointer to the repo companion memo.
4. **Edit the jii docx in place** — keep John's Overview–§2.2 text and formatting (minus TODOs, plus typo fixes), replace everything below. Clean edits, no tracked changes.

## Resolutions for the two factual TODOs

- **TODO 1 (SSA persons-with-wages vintage):** SSA's *Earnings and Employment Data for Workers Covered Under Social Security and Medicare, by State and County* now has a **2023 edition** (https://www.ssa.gov/policy/docs/statcomps/eedata_sc/index.html); the store holds 2017 + 2022. The series is maintained and remains the concept-correct **persons**-based target; QCEW is timelier but counts jobs/dollars, not persons — keep it as the dollar-side cross-check, not a target. §2.1 target 3 gets a sentence saying so. (Also note SSA research note rsn2024-02 flags a forthcoming geographic-assignment methodology change — worth a caveat clause.)
- **TODO 2 (national target / projections):** for the aging/projection anchor, use **IRS projected individual return counts** (Pub 6187 / Pub 6292, Calendar/Fiscal Year Return Projections) on the filer side and **Census population projections** (with CBO's Demographic Outlook as cross-check) on the population side — the projected residual falls out by construction. Add as a short fourth item in §2.1.

## New document structure (per John's TODO outline)

**Keep (light copyedit only):** Title block, Overview, §1 Current state, §2/§2.1/§2.2.
- Fix typos: "Second the author's also assume" → "Second, the authors also assume"; check other front-matter typos in passing.
- §1 gains one short evidence paragraph quantifying the defects from Stage D: non-filer mass ≈15–25% short of the comparable anchor (F1); age shape inverted — 8.9% of non-filer adults at 18–25 vs 24.2% in the anchor, 42.9% at 65+ vs 25.1% (F2); investment income identically zero vs IRS potential-non-filer receipt rates of 14% interest / 9% dividends / 48% Social Security (F3).
- §2.1: resolve the two TODOs as above; delete the TODO paragraphs.
- §2.2: keep the CPS ASEC framing; delete the big restructure-TODO block and the stray fragments below it.

**Section 3 — National** (new prose, CPS-first):
- *3.1 CPS ASEC filing models*: tax-unit construction from CPS households; split at the filing threshold. Below threshold: Cilke (1998) group-probit structure with group constants recalibrated to current administrative counts. Above threshold: a tax-gap-based non-filing model from IRS Pub 5785 — 10.6–11.9M units with a filing obligation (TY2014–16), under 20% married, ~45% with business/farm income, motivating a self-employment dimension (F7).
- *3.2 National targets*: Pub 1304 Table 1.6 (returns by AGI × marital status × age) and 1.7 (dependent returns); Census PEP population by age; SSA OASDI beneficiaries and covered workers. Include the anchor-validation result: two independent SOI constructions of filing adults agree within ±0.5% (205.5M vs 206.1M in 2017), and the implied residual — 47.3M (2017) / 46.5M (2022) non-filing adults, ~18–19% of adults — triangulates against Pub 5785's independent 49.7–51.7M count.
- *3.3 Joint estimation*: both models' parameters selected jointly so the combined filer + non-filer file hits the targets; the residual is an anchor with an explicit tolerance (return-state vs residence, vintage, and dependent-netting wedges), not an exact count; adult-dependent netting (claimed adult dependents ride filer records).
- *3.4 Aging*: current drift stated with F4 (non-filer share of units rises 14.7% → 15.8% by 2035 with no return-count discipline after 2019); proposal: filer counts follow IRS return projections, population follows Census projections, non-filer mass is the residual by construction.
- *3.5 Robustness checks*: comparisons of national non-filer counts and composition against PolicyEngine, TPC, and CBO tax-unit/non-filer estimates; receipt-rate checks against Pub 5785 Table 1; sensitivity to the above-threshold scalar.

**Section 4 — State** (new prose):
- *4.1 Non-filer targets*: state residual = PEP resident adults − filing adults from HT2 filing-status identities (national levels from Pub 1304, HT2 supplies state shares); state residual shares run 10.6% (SD) to 27.7% (MS) of adults; OASDI beneficiaries discipline 65+, the covered-worker margin (SSA persons-with-wages minus returns-with-wages) the working ages. Group quarters handled in prose: the PUF universe includes GQ residents (verified against DINA totals), so the residual keeps them — differentiated treatment (institutional residents stay own-state non-filers; dorm students become dependents), with F6 sizing (GQ = 16.8% of the national residual, but 28–42% in MN/ND/VT/AK/SD).
- *4.2 Proposed procedure*: connect to the overall re-weighting proposal — filer weights fit to IRS state targets, non-filer weights to the residual anchors with survey-based within-state income/age shape as prior; if both partitions hit their targets the state population identity holds by construction. Note the current placement error being fixed: existing state non-filer margins run 0.78×–1.51× of the residual anchor (F5).
- *4.3 Validation*: the population-identity check (fitted filer + non-filer adults vs population, by state); correlation of placement errors with EITC take-up; a new held-out metric, state adults by age band vs PEP.
- *4.4 Expected effects, stated honestly*: fixes the state placement of ~27.6M non-filer units and the national level/age composition; does **not** fix filer-side income-concept misses, which need their own target expansion.

**Section 5 — Comparison to the eventual Affordability-Index work**: the two systems solve inverse problems (this work splits national weights across states; the Affordability spine calibrates an ACS file in place); what transfers is the filing model, the anchors, and the identities, not the architecture; the one legitimate universe difference is group quarters (household universe vs full resident population), so shared anchors must carry an explicit universe tag.

**Section 6 — Open questions**:
- How the CPS-estimated filing model transfers to the ACS-based state allocation (recalibration of constants on the ACS).
- Quantifying the residual anchor's tolerance from the known wedges.
- PEP vintage pairing with target years; back-year anchor availability.
- Geography of above-threshold non-filing (Pub 5785 is national).
- Dependent non-filers and MFS (out of scope for v1).

**Delete entirely:** the leftover fragment block after the TODO (duplicated v1a/v1b bullets, orphaned §3.3, Pub 1304 fragments), old §§4–8 (diagnostic harness, decision tables, Tax-Data/Tax-Simulator rework designs, sequencing, housekeeping), and the trailing "OLD" section. Add one sentence (end of Overview or Section 6) noting an implementation companion memo exists in the repo.

**Style rules** (from John's TODO guidelines + conventions): no program/function/stage names, no file:line references, no repo jargon ("Stage D", "v0/v1a/v1b", "T1–T7", "D1–D6" become plain descriptions); complete prose paragraphs suitable for co-author review; keep John's voice from the front matter (first person, plain declarative). Load `econ-write` for the drafting pass and `humanizer` before finalizing.

## Files

- **Modify:** `research/docx_sources/nonfiler_proposal_jii.docx` (in place).
- **Read-only sources:** `research/state_weights/nonfiler_residual_design.md` (old memo, canonical technical content), `research/state_weights/nonfiler_residual/04_findings.md` (F1–F7 numbers), `research/state_weights/nonfiler_residual/results/T*.csv` (spot-check any number cited).
- Untouched: `nonfiler_residual_design.docx` / `.md` (the old memo stays as the internal companion).

## Mechanics

1. Re-extract the jii docx fresh at implementation time (user edits files externally): `python scripts/office/unpack.py` into the scratchpad.
2. Edit `word/document.xml` with the Edit tool: preserve the front-matter XML verbatim (except TODO paragraph removals and the listed typo/TODO-resolution edits, matching existing run formatting); replace everything from the restructure-TODO onward with new §3–§6 paragraphs using the document's existing styles (Heading 2/3, its list numbering, smart-quote entities).
3. `python scripts/office/pack.py` back to the same path (validation on).
4. No tracked changes; document stays clean for co-authors.

## Verification

- `pandoc` extraction of the packed docx → read end-to-end: confirm all three TODOs gone, no fragments/OLD block remain, section numbering 1–6 coherent, every cross-reference resolves.
- Grep the extracted text for banned jargon (`build_`, `.R`, `v0`, `v1a`, `Stage D`, `T1`–`T7`, `D1`–`D6`, file paths) — zero hits expected outside legitimate prose.
- Verify every cited number against `04_findings.md` / the results CSVs before it goes in.
- Optional visual check: render to PDF via `scripts/office/soffice.py` and eyeball layout (headings, lists, hyperlinks intact).

---

## Discrepancies between the proposal and the implementation record

Carried in from the 2026-08-18 unified plan (archived at
`research/archive/nonfiler_unified_plan_2026-08-18_imported.md`), because these are
corrections to make in the **proposal** text rather than to the method of record. Worth
resolving there, since it is the document that will be read.

Worth resolving in the proposal text, since it is the document that will be read:

1. **Survey.** Proposal §2.2 says the filing model is built on the CPS ASEC; the design
   memo §3.2/§6.1 and all Stage-D code build it on the IPUMS **ACS** extract. Resolved
   here in favour of the proposal (decision 2), which means the design memo's §6.1 needs
   amending — it currently asserts "no new IPUMS pull is needed for anything in this memo."
   Note the ASEC route is also the *native* one for Cilke, which was estimated on the ASEC:
   the recalibration burden moves to the explicit ASEC→ACS transfer step rather than being
   embedded invisibly in an ACS-only fit.
2. **§3.5's robustness checks are weaker than they sound.** The proposal proposes comparing
   national non-filer counts and composition against PolicyEngine, TPC and CBO. Verified: the
   committed CBO reference is an IIT build-up with **no non-filer line at all**, so CBO can
   only confirm we did not disturb the filer side; and PolicyEngine's non-filer count rests on
   a different unit definition and its own filing model, so a level comparison is
   uninterpretable. TPC's tax-unit count is a genuine (if concept-gapped) check. The strongest
   external discipline available is one the proposal does not mention: the **2020–2021 baseline
   rebate and advance-CTC outlays against published actuals**, which depend directly on the
   non-filer level. Rewrite §3.5 around that.
3. **The parameter sources may be stale and the proposal treats them as fixed.** Cilke is
   1998 and Pub 5785 is TY2014–2016; the proposal cites both without noting that newer
   estimates may exist, and the 2020–2021 EIP non-filer outreach period generated substantial
   new evidence that postdates both. Research pass B (step 1.2) settles this; §2.2 and §3.1
   should record the outcome either way.
4. **The SSA targets are not yet obtainable.** Proposal §2.1 item 3 and §4.1 lean on SSA
   OASDI and covered-worker tabulations as anchors. ssa.gov 403-blocks the cluster; the
   stores exist but are empty. The proposal should note the manual-download dependency,
   and §6's vintage-pairing question should absorb the SSA geography-revision caveat that
   is currently a parenthetical.
5. **Worth adding to §6's open questions:** the proposal lists dependent non-filers and MFS as
   known omissions, but not the two model-side gaps the federal pass surfaced — that EITC has
   no become-filer path and that `become_filer_ctc` requires exactly zero earned income
   (4f-bis). Those bound what the rework can deliver for refundable-credit analysis, which is
   the proposal's headline motivation, so they belong in the memo rather than only in the code.
6. **Every quantitative claim in the proposal checks out** against Stage D — the 15–25%
   shortfall, the 9%/24% and 43%/25% age figures, 205.5/206.1 and 214.1/213.1 filing
