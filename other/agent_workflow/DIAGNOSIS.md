# Agent workflow: diagnosis and proposed design

Draft, 2026-07-25. Scope is Tax-Simulator only; the tariff model and the
interactives repo are deliberately excluded.

The question behind this document is how to use AI agents to (1) go faster,
(2) stay robust in a workflow where the principal does not read most of the
code, and (3) do both without building a pile of machinery that costs more to
maintain than it returns. A secondary question is where a second model from a
different lab fits.

---

## 1. Evidence base

Everything below is drawn from the repository itself: 182 commits since
2026-05-01 (106 in July alone), the contents of `other/` (23 project
directories), `CLAUDE.md`, `.claude/`, the session memory index, and the
calibration registry at `other/kg_model_tests/calibration_reference.csv`.

No interviews, no recollection. Where the record and intuition disagree, this
document follows the record.

---

## 2. What the record shows

### 2.1 This is a large engineering operation

Six substantial economic channels built or rebuilt since May: the on-model
wealth tax, the wealth-dynamics bathtub, corporate incidence, the estate
module and its heir allocator, the hidden-ledger concealment consistency
layer, and the sigma income-conversion channel. Alongside them, two complete
calibration regime changes in the capital-gains model (entropy Bellman, then
the single-pool spec v3, then a levels/logs response-form toggle), a 226-file
report project, a published interactive tool, and a SLURM pipeline that
duplicates the orchestration layer.

The binding constraint on this operation is not authoring speed. Code gets
written quickly and mostly correctly. The constraint is somewhere else.

### 2.2 The constraint is detection latency on silent numerical error

Roughly 24 of the 182 commits are repairs to previously shipped behavior.
That share (13%) understates the cost, because of what these repairs are.
None of them are crashes. Every one produced plausible output for some period
before being caught:

- the 1970 CPI splice, where the splice-year growth rate went `NA` and was
  silently treated as zero;
- the estate excess-growth double-count in post-processing;
- the §1250/collectibles headroom bug, which dropped special gains from the
  base at roughly $8.4B per year;
- the sample-universe bug, which quietly excluded 935 top-tail records worth
  $8.2T from every run before 2026-06-10;
- the SECA companion scaling error in entity shifting, which contaminated
  every conventional run involving a rate reform;
- the estate reporting-response activation bug, still present in shipped
  dials and atlas vintages as of 2026-07-16.

The true cost is the cascade, and the session memory index is where it is
legible. Recurring phrases: *pre-fix vintages no longer comparable*,
*re-run before comparing levels*, *SUPERSEDED*, *shipped vintages still carry
the bug until the next batch re-run*. Each instance means a vintage
invalidated, figures rebuilt, and a memo revisited, often weeks after the
work that produced them was declared finished.

This is a detection-latency problem, not a throughput problem. Any design that
buys throughput at the cost of detection latency will make things worse.

### 2.3 The right defenses already exist, once each

Two mechanisms in the repository are prototypes of the two general families of
defense, each built for a single narrow task type and never generalized.

**Mechanical invariant detection.** `other/kg_model_tests/calibration_reference.csv`
is a proper registry: per constant, the shipped value, a tier, the reference
moment it was pinned against, the derivation date, the code SHA at pinning,
the files whose modification invalidates it, and the Tax-Data and Macro
vintages it depends on. This is a well-designed object.

**Independent construction.** `.claude/agents/policy-extractor.md` and
`.claude/agents/policy-roundtrip.md` are exactly the blind-duplication pattern:
one agent writes reform YAML from the English description without seeing the
existing output, another reads YAML back to English without seeing the original
description, and disagreement between the two localizes the error. Built for
reform config authoring; used nowhere else.

### 2.4 Both prototypes are under-deployed, and one is switched off

The calibration registry covers five constants: two η values, two
timeable-share values, and σ. A scan of `src/sim/` alone finds several dozen
further pinned economic magnitudes outside it, including `CORP_SIGMA_N = 0.375`,
`CORP_KAPPA = 0.40`, `CORP_OMEGA_DIV = 0.85`, `CORP_OMEGA_KG = 0.50`,
`CORP_DELTA_NIPA = 0.057`, `CORP_THETA_RES = 0.40`, `SIGMA_PT_LABOR_SHARE = 0.75`,
and `WEALTH_CAP_FLOWS_PT_WEIGHT = 0.2`, several of which the memory record
describes as Phase-0c placeholders. The estate valuation bridge
(`r = 0.951`, `rho_pt = 0.612`, the cluster cap of 300, the 0.40 foreign share,
the 0.367 avoidance ETI) is pinned in YAML and likewise absent from the
registry. Memory already notes the estate parameters are stale-pinned on a
superseded Tax-Data vintage.

More pointedly: the enforcement half of the calibration watch is not running.
`core.hooksPath` is unset, and there is no `pre-push` hook installed in
`.git/hooks`. The memory note from 2026-07-12 lists "activate core.hooksPath"
as a follow-up. It was never done. The detector was built, recorded as
complete, and never switched on — which is itself an instance of the failure
mode this document is about.

### 2.5 Systematic review sweeps have the highest observed yield

Five discrete review passes appear in the record: the calc-function review
(5 bugs, one worth $8.4B/yr), the non-calc correctness review (10 fixes
including three silent-failure holes), the corporate-incidence external review
(5 findings incorporated pre-implementation), the simplification batch, and
the performance audit. These were ad hoc, triggered by intuition rather than
schedule, and every one of them found something material. Nothing in the
workflow currently causes them to happen on a cadence.

---

## 3. Task-type taxonomy

Six recurring shapes, ordered by how much of the last three months they
account for.

### T1. Channel build

A new economic mechanism: wealth dynamics, corporate incidence, the wealth
tax, the estate module, hidden ledger, sigma conversion. Six instances since
May, each spanning one to three weeks.

A de facto template already exists and is followed consistently: a
considerations or stakes memo, a formal model, a numbered ruling list
(D1–D18, P1–P14), implementation, guards, a verification harness, and a
CLAUDE.md section. This is the most mature process in the repository.

*Observed failure mode:* not implementation error. The costly errors are
specification errors caught after the code is correct-to-spec — the
mortality double-count in the wealth recurrence, the D16 double-count of kg
in the external-income contract. Both were caught in review of the *design*,
which is evidence that the design review is where the value is.

*Recommended mechanism:* independent construction of the formal model, not
of the code. Two arms receive the same problem statement and produce a
specification independently; disagreements are triaged into the ruling list.
This is a natural extension of what already happens, and it attacks the layer
where the errors actually are. Duplicating the implementation is not
recommended: two correct-to-spec implementations differ mostly in style and
the comparison is expensive.

### T2. Calibration and re-pinning

η re-pinned three times (4.4984 → 2.3992 → 2.4825, with a separate 1.6625 for
the logs form), σ twice (0.08 → 0.157 → 0.16), plus f, the timeable share, and
the default financing profile. This is the highest-frequency and
highest-consequence category, because a re-pin invalidates every vintage
derived under the old value, and because a wrong constant is invisible in
output.

*Recommended mechanism:* both families, together. Extend the registry to
every pinned economic magnitude with a tiering rule, and actually install the
hook. Then add blind re-derivation as the second arm: given the specification
document and the data, a second model derives the constant independently
without seeing the number. The object being compared is a scalar, so the
comparison is mechanical and cheap. This is the single best fit for the
cross-model pattern anywhere in this repository.

### T3. Run orchestration

SLURM launches, monitoring, restarts, vintage bookkeeping. High wall-clock,
low intellectual content, but errors are expensive because they contaminate
vintages that downstream artifacts then consume.

*Recommended mechanism:* no second arm; there is nothing to disagree about.
Invariants only — identity ties, telescoping checks, baseline-on-baseline ΔT.
Plus the provenance ledger described in §5, which is the highest-value item
in this document and is not an AI mechanism at all.

### T4. Verification and A/B

Unit-check logs, pre/post A/B runs, theorem checks, the byte-diff smoke
harness. Infrastructure here is already good, and this is the one category
where a mechanical oracle usually exists.

*Recommended mechanism:* keep as is. Do not add a second model. Where an
oracle exists, checking beats duplicating.

### T5. Systematic review

See §2.5. Highest yield per unit of effort in the record, currently
unscheduled.

*Recommended mechanism:* adversarial cross-model review, on a cadence tied to
an event (a channel declared complete, a vintage promoted to production)
rather than to the calendar. Different-lab review is genuinely valuable here
because a same-lab reviewer anchors on the frame it is reviewing.

### T6. Deliverable production

Memos, figures, artifacts, the atlas tools. `other/top_tax/` alone is 226
files. Highest taste bar; the standing voice guidance already reflects
repeated dissatisfaction with model defaults.

*Recommended mechanism:* never dual-write prose. Two drafts of the same memo
produce a merge problem and a diluted voice, and taste does not average well.
Do dual-check the numbers *in* the prose against the source tables, which is a
mechanical task with a right answer.

### T7. Reform config authoring

Already solved by the `policy-config` skill plus the two round-trip agents.
Listed for completeness and as the template for §4.

---

## 4. Where the cross-model pattern actually belongs

Consolidating the above: independent construction by a differently-trained
model earns its cost in exactly three places.

1. **Calibration re-derivation** (T2), because the comparison object is a
   scalar and disagreement is unambiguous.
2. **Design and specification review** (T1, T5), because the failure mode is
   a wrong frame, and a sequential reviewer inherits the frame it is given
   while a blind parallel arm does not.
3. **Interpretive claims destined for publication** (T6, numbers and
   conclusions only, never voice), because there is no oracle and the cost of
   being wrong in public is high.

It does not belong on implementation, on runs, or anywhere a mechanical check
is available. The general rule: *duplicate in proportion to the absence of an
oracle.*

Two operational notes. The Codex CLI is installed and authenticated at
`/home/jar335/.npm/_npx/c8ab89660c602c20/node_modules/.bin/codex` (v0.145.0)
but is not on `PATH`; `~/.codex/config.toml` currently specifies
`gpt-5.6-sol` at medium reasoning effort. The Claude-side codex plugin is not
installed in this environment. Neither is a blocker — `codex exec --cd <path>`
against a git worktree is sufficient for blind parallel work and gives more
control over isolation than the plugin does.

---

## 5. The item that is not about AI at all

The reason late detection is so expensive here is that nothing records which
downstream artifacts depend on which vintage, and which vintage depends on
which constants. When η moves, the answer to "what is now stale?" is
reconstructed from memory each time, which is why the memory index is full of
prose warnings about non-comparable vintages.

A provenance ledger — a checked-in table mapping vintage to the constants,
Tax-Data version, and code SHA it was produced under, and mapping each
published artifact to the vintage it consumed — turns that question into a
query. It converts "everything might be stale" into a specific list, and it
makes the cascade cost of a late-caught bug proportional to the actual
dependency set rather than to anxiety.

This is a hundred lines of bookkeeping and probably worth more than every
multi-agent mechanism in this document combined. It is listed first in the
build order for that reason.

---

## 6. The configuration layer

Findings on the current state, and where things should live.

**`CLAUDE.md` is 552 lines,** of which more than half is channel knowledge —
the estate, wealth-dynamics, and corporate-incidence sections — loaded in full
at the start of every session regardless of topic. That content is excellent
and should not be deleted; it should be moved into topic-scoped skills that
load when the topic comes up. What remains in the always-on file is
orientation, conventions, execution mechanics, and the SLURM sync table.

**`.claude/settings.local.json` has accreted to roughly 200 permission
entries,** most of them dead single-use literals: full paths to
`C:/Program Files/R/R-4.3.1` from the Windows era, half-migrated
`gpfs/gibbs/sarin` paths, individual `squeue -j 55532676` invocations, and
several multi-hundred-line inline R scripts pasted in as permission strings.
It should be pruned to a couple dozen patterns.

**There is no user-level `~/.claude/CLAUDE.md`.** The model-selection policy,
the compute rules (never run R on the login node), the voice guidance, and the
duplication policy are properties of the principal rather than of this
repository, and belong there. They currently live partly in this repo's
CLAUDE.md, partly in session memory, and partly nowhere.

**There are two skills** (`policy-config`, `run-sim`) and two agents. Given
seven recurring task types, the skill layer is thin relative to the process
maturity that actually exists — most of the T1 channel-build template lives in
convention rather than in anything a fresh session can load.

**A layering rule** worth adopting: policies about the principal go to the
user level; knowledge about a channel goes to a topic skill; procedures with a
fixed shape go to a task skill; only orientation and cross-cutting invariants
stay in always-on repo context.

---

## 7. Proposed build order

Gated on demonstrated recurrence: nothing here is for a task type that did not
occur at least three times in the last three months.

1. **Install the pre-push calibration hook.** It exists and is off. Minutes.
2. **Provenance ledger** (§5). Highest value per line in this document.
3. **Extend the calibration registry** to every pinned economic magnitude,
   with an explicit tier and a placeholder flag for the Phase-0c constants.
4. **Config hygiene:** prune `settings.local.json`; split `CLAUDE.md` into an
   always-on core plus topic skills; create the user-level policy file.
5. **Blind dual-arm harness,** scoped initially to calibration re-derivation
   (T2), since that is where the comparison is cheapest and the payoff
   clearest. Two git worktrees, identical self-contained spec, a pre-declared
   answer schema, and a structured comparison. Prove it there before extending
   to design review.
6. **Scheduled adversarial review** at channel-completion and
   vintage-promotion events.

Items 1 through 4 are mechanical and carry no model risk. Item 5 is the
experiment, and should be judged on whether the disagreements it produces are
informative or merely noisy, on a real task, before anything is built on top
of it.
