# State-Tax Module Architecture Review — 2026-07-17

Critical review of the state tax parameter files (`config/scenarios/tax_law_state/`)
and calculators (`src/calc/state/`), conducted immediately after the Connecticut
build (commit `d2436cc47`, 22 states encoded). Scope: the full structure, not a
single diff. Purpose: describe the modeling process that has emerged and identify
streamlining/simplification changes before the remaining ~29 income-tax states land.

Supporting inventory (counts below) was compiled by a repo-wide sweep on the same
date; calculator line counts and parameter tallies refer to `state-tax` @ `d2436cc47`.

---

## 1. The process that has emerged (descriptive)

Reconstructed from the code, `state_parameter_workflow.md`, and the source packets:

1. **Research**: primary DOR forms -> source packet (`source_packets/{st}.md`),
   the form as operational truth, values anchored at 2017. Recent practice:
   research agents transcribing every year-booklet with hash-verification across
   years (CT packets are the best-evidenced; raw records preserved under `raw/`).
2. **Fit-to-ledger**: map each state feature onto existing generic parameters;
   where nothing fits, extend the calculator with a new *generic* mechanism (the
   component ledger), never a state branch. This rule has held: there is no
   `state == 'XX'` branch anywhere in `src/calc/state/`.
3. **Encode**: six standard YAML files per state, filing-status mappers,
   `reference:` on every subparameter; inert "for completeness" params permitted.
4. **Default**: every new optional parameter gets a neutral no-op default in
   `ensure_st_params()` so absent-column and NA-cell law shapes both work.
5. **Verify**: hand-computed worksheet cases in `test_state_calc.R`, plus the
   smoke grid, subset-states regression, and YAML-convention tests.
6. **Track**: rollout CSV, known-differences notes, conformity groups gating
   federal-reform runs.

The resulting architecture: **one vectorized calculator that is the union of every
state's mechanisms**, states as pure configuration, features gated by neutral
defaults or vector-column sentinels. Dense tables ride either a CSV attribute
(CA `credit_tables.csv`) or long YAML vectors (NY household credit, CT Table E).

### Scale snapshot (2026-07-17)

- 22 states encoded; YAML corpora 50 lines (zero-tax stubs) to 575 (NY); CT 465.
- `src/calc/state/` totals 2,068 lines; `st_credits.R` is 627.
- `ensure_st_params()`: exactly **210 scalar defaults** + 8 vector-family
  sentinels. Roughly **40% of parameters are used by exactly one state**
  (WA ~28, CA ~27, NY ~12, CO ~7, GA 5, CT 3, singletons for NC/KY/ND).
- **16 band/step-lookup implementations** across six files in four idioms;
  **12 feature gates** in four spellings.
- CA is the only state on `credit_tables.csv` (20,739 rows, load-validated);
  CT/NY dense tables are YAML vectors.
- Inert (never read by the calculator): `conformity_year` (12 states),
  `k12_credit_*` (IL), `tuition_*`, `govt_pension_full_sub`,
  `item_base_pre_tcja`, `salt_cap_applies` (NY), `sub_529_*` (NY, CT).
  `sub_us_int` is read but deliberately no-op. (`conformity_group` IS live —
  consumed by summary_stats.)

Extrapolating the trajectory to ~51 jurisdictions: ~450+ parameters and a
1,200+ line `st_credits.R` unless the machinery below changes.

---

## 2. Critical issues (blocking-class hazards; not shipped bugs)

### #1 — Unknown YAML parameters fail silently into wrong answers

**Mechanics.** `parse_one_state()` (`src/data/state_tax_law.R`) converts *every*
subparameter found in YAML into a column — there is no list of legal names in the
load path. Separately, `ensure_st_params()` backfills every *known* optional
parameter that is absent with a neutral default. The two compose into a trap.

Example: suppose `ct/agi.yaml` said `pension_sub_shre: 1.0` (dropped "a"):

1. The parser creates `st_agi.pension_sub_shre = 1.0`. Nothing reads it. No error.
2. `ensure_st_params()` sees the real `st_agi.pension_sub_share` absent and fills
   it with its neutral default, **0**.
3. The calculator runs to completion. Connecticut simply stops deducting pension
   income. Every downstream number is finite, plausible, and wrong by exactly one
   policy feature.

The only current defense is a worksheet test that happens to exercise that
feature-state-year. `parse_calc_fn_input`'s `req_vars` check does not help: it
only fails on core parameters with no default (`st_ord.rates`); every optional
parameter is pre-filled before any check could fire.

The same trap catches more than typos: a parameter *renamed* in a refactor
silently orphans every state's YAML that used the old name; a reform overlay
written against a misremembered name is a reform that does nothing.

**Likely production incident**: a published state revenue estimate wrong by one
feature, with no error anywhere, discovered (if ever) by an external replicator.

### #2 — Documentation-only config is indistinguishable from modeled config

**Mechanics.** The sanctioned "encoded for completeness" pattern produces
parameters that look exactly like live ones — same YAML structure, same
`reference:` citations, same files. `il/credits.yaml` carries
`k12_credit_rate: 0.25` with a statutory citation; nothing on the page says the
IL K-12 credit is NOT modeled. Seven families across ~14 states are in this
state (list in §1). The realistic failure is a person, not a crash: someone
writing a methodology section, answering a referee, or scoping a reform reads
config as the specification of what is modeled — which is what config means
everywhere else in this codebase — and asserts coverage the model doesn't have.

Subtler variant: `sub_us_int` is read into `req_vars` by `calc_st_agi` and then
deliberately used for nothing (US-obligation share unobserved), so even "is it
referenced in code?" doesn't separate modeled from documented.

### Why #1 and #2 are one fix

Both are the same missing invariant: **every parameter the parser accepts must
have a declared status.** One build-time validator, hooked into
`build_state_tax_law()` and exercised by `test_state_tax_law()`:

1. **Registry**: the legal-name set already exists implicitly — the 210
   `ensure_st_params()` names, the core params, and the vector families
   (`step_recap_*`, `pct_credit_agi_bounds*`, ...). Write it down once (static
   list now; the §4 schema file later).
2. **Check A (kills #1)**: after parsing each state, anti-join parsed column
   names against the registry (pattern rule for vector families; mapper source
   names resolved before the check). Any unregistered parameter -> `stop()` with
   state, file, and name. A typo becomes a load-time build failure.
3. **Check B (kills #2)**: documentation-only values move out of the parameter
   namespace — a top-level `documented_not_modeled:` key the parser skips (or a
   `note_` prefix). Check A then automatically rejects any "for completeness"
   param left in the live namespace. The conventions test can still require
   citations on quarantined entries.

**Scope**: validator ~50 lines + tests. The real work is the retroactive audit
it forces: first run flags all seven inert families (each needing quarantine or
a decision to actually implement — `conformity_year` is a candidate for
implementation: load-bearing information in a dead variable), and it may surface
a true typo among the 22 encoded states that no worksheet test covers.

**Design decision to settle first**: registry as a standalone static list (an
afternoon; a third copy of the names to maintain) vs. pulling
`ensure_st_params()` into schema-as-data (§4) simultaneously so registry,
defaults, and validator are one artifact (day one of the bigger refactor).

---

## 3. Required changes

### #3 — One primitive, sixteen implementations

16 band/step lookups in four idioms: `rowSums(amt * (x > lb & x <= ub))`
(NY household credit, CT Table E, NC child table), `rowSums(b <= x)`
index-picking (CT retirement factor, bracket index), `ceiling(pmax(0, x -
thresh)/step) * amount` (seven separate phase-outs), and `findInterval`
(CA credit tables). Each hand-codes boundary semantics (`>` vs `>=`, `ceiling`
vs `floor` — which are *policy* semantics; cf. NY ESCC rounding up pre-2025,
down after). Three helpers — `band_lookup()`, `step_phaseout()`, an
interpolated variant — plus one `get_vector_family()` gate helper (replacing 12
gates in 4 spellings) delete ~200 lines and close the off-by-one class.
Highest-leverage refactor available.

### #4 — `ensure_st_params()` should be data, not code

210 defaults in a function body, ~40% single-state, ordered by accretion;
simultaneously the de facto registry, the semantics documentation, and a merge
hotspot for parallel state workstreams. Move to `params_schema.yaml` (name,
default, dims, consumer file, units, phase-out family, modeled flag) and
generate the defaults vector, the #1 validator, and reference docs from it.
The workflow doc's component ledger then stops being prose that drifts.

### #5 — Config values that appear on no form

CT's $24,000 per-return exemption is encoded as `12000` because
`personal_amount` is silently per-taxpayer (x2 MFJ) in `calc_st_exempt`. This
violates "the form is operational truth": an auditor comparing `ct/exempt.yaml`
to Table A sees a number that is not on the form, guarded only by a comment.
Same ambiguity latent in `pension_excl_*` and aged/blind add-ons. The schema
should carry `unit: per_taxpayer | per_return` and the calculator honor it, so
config always transcribes the form.

### #6 — `st_credits.R` (627 lines) is on the god-file trajectory

Eleven credit families; CA (3), NY (4), CO (2), CT (3) blocks interleaved as
matrix-preamble + `mutate` sections behind a single 95-line `req_vars`. Each
block is disciplined, but the next four states with EITC-adjacent credits all
land here. After #3 shrinks the blocks, split by family: earned-income credits,
child/dependent credits, care credits, percentage-of-tax/household credits.

---

## 4. Suggestions (think-big items)

### #7 — Converge the two dense-table mechanisms

CA's `credit_tables.csv` (year-versioned, schema-validated, overlap-checked at
load) and the YAML-vector approach (CT 28-bound Table E, NY household tables)
solve the same problem; the CSV path is better engineered but hard-coded to a
`child_count` key. Generalize to
`(table_id, state, year, filing_status, key_concept, lower, upper, value)` and
move every table over ~8 rows into it. YAML keeps short structural vectors
(rates, brackets, 4-segment recapture). Kills the long YAML lists, most
sentinel machinery, and gives every dense table load-time validation.

### #8 — Make every phase-out's income base explicit

The base is implicit and inconsistent per feature: exemptions got `po_agi_base`
(CT needed it), itemized limits have `item_limit_agi_base`, KY family credit
hardcodes `agi + st_additions`, credits hardcode `agi` or `st_agi` per family,
CT's retirement factor hardcodes federal `agi`. Each is one state away from
needing the switch another already has. Uniform `*_income_base` enum in the
schema (fed AGI / state AGI / AGI+additions / earned income).

### #9 — Two cheap missing test layers

(a) **Coverage**: assert every modeled parameter family is exercised by >= 1
worksheet case (CT Table E has seven tests; nothing enforces the next state's
novel feature gets any). (b) **Structure**: per state-year continuity /
monotonicity sweeps — liability as a function of AGI continuous except at
*declared* cliffs (IL exemption, CT SS threshold). Catches a mis-encoded band
table without hand-computing a return at the bad boundary. Also: cross-model
(TAXSIM/PolicyEngine) is `todo` on every tracker row — at 22 states it is now
the weakest link, and the only layer that catches *research* errors rather
than encoding errors.

### #10 — What NOT to do

No DSL / rules-engine rewrite (PolicyEngine-style parameter trees). The
nine-stage pipe mirrors `do_1040()`, is debuggable, and the team knows it.
Everything above is incremental hardening; a rewrite trades a working system
with two known silent-failure classes for a new one with unknown ones.

### Sequencing

#1+#2 first (one validator, ~a day, converts silent classes to loud ones and
retroactively audits all 22 states) -> #4 schema-as-data -> #3 lookup
primitives -> #6 file split -> #7 table convergence. Each independently
shippable behind green tests.

---

## 5. Credit where due

- The subset-states regression test (law built WITHOUT a state must not error
  on missing columns) and the 0/1-numeric-flag regression in `st_test_unit`
  are burn-scar tests — good ones.
- `credit_tables.csv` load validation (schema, ranges, overlaps) is the model
  the rest of the config layer should follow.
- The conventions test (2017 anchoring + citation on every subparameter) and
  the generic-components rule have held across 22 states with zero state-name
  branches in the calculator.
- CT research-record preservation (`raw/ct_research_*.md`, hash-verified
  booklet transcriptions) closes the provenance gap that previously cost a
  full re-research.

## 6. Verdict

**Needs Discussion.** Nothing shipped is broken. Issues #1 and #2 are standing
hazards that grow with every state; #3/#4 have a closing window — a week of
work now, a month at 40 states.

## 7. Status / triage

| # | Item | Tier | Status |
|---|------|------|--------|
| 1 | Unknown-param silent default | Blocking-class hazard | RESOLVED 2026-07-17: `validate_state_param_names()` in `state_tax_law.R`, registry from `st_param_name_registry()` (scalars = `st_param_defaults()` names + `conformity_group`; 15 vector-family regexes). Runs on every `parse_one_state()`. Retroactive audit of all 22 states clean. |
| 2 | Inert config indistinguishable from modeled | Blocking-class hazard | RESOLVED 2026-07-17: top-level `documented_not_modeled:` key, skipped by the parser, citations enforced by the conventions test. All seven inert families quarantined (conformity_year x12, IL k12_*, NY tuition_*/govt_pension/item_base_pre_tcja/salt_cap_applies, NY+CT sub_529_* incl. mapper removal). `sub_us_int` left live (read-but-no-op; calculator-design question). `conformity_year` implementation deferred. |
| 3 | Consolidate band/step-lookup primitives | Required | RESOLVED 2026-07-18: `src/calc/state/st_utils.R` — `st_family_matrix()` (gate+collect, replaces the 12 gate spellings), `st_band_value()` ((lb,ub], zero outside), `st_band_index_lower()`/`st_band_index_upper()` (schedule-style clamped indexes), `st_pick_slot()`, `st_step_reduction()` (round_up = policy semantics), `st_band_interp()`. All ~16 sites across 7 files converted; verified bit-identical on a 63,648-result dense grid (17 states x 8 years) vs pre-refactor HEAD. |
| 4 | `ensure_st_params` -> schema-as-data | Required | RESOLVED 2026-07-18: `config/scenarios/tax_law_state/params_schema.yaml` (NOT under src/ — the recursive source walk would parse it as R) holds defaults + sentinels + family patterns; memoized `st_param_schema()` reader in st_utils.R; thin accessors; do_state_taxes.R 470→108 lines. Bare value = default; mapping form reserved for #5 units metadata. Schema-integrity assertions pin .inf/-.inf/null parsing. Bit-identical on the 63,648-result grid. |
| 5 | Per-taxpayer vs per-return units in config | Required | RESOLVED 2026-07-18: `st_exempt.personal_per_return` flag (calculator honors it); CT re-encoded to Table A form values ($24,000 joint, was 12,000 + apology comment); schema mapping form carries `units:` metadata on the person/return-ambiguous amounts. Bit-identical. |
| 6 | Split `st_credits.R` by credit family | Required | RESOLVED 2026-07-18: `st_credits.R` is now a ~160-line orchestrator; families in `st_credits_household.R` (hh/exemption/pct-of-tax/family-size/property), `st_credits_earned.R` (EITC options + CLI + CalEITC + YCTC + age-package gate), `st_credits_child.R` (IL/NY/CO CTC + FATC + AZ dependent), `st_credits_care.R` (CDCTC). Each module declares its own req_vars fragment, concatenated by the orchestrator; cross-family inputs (hh credit → NY EITC offset; chosen EITC → IL CTC) flow through arguments. `lookup_state_credit_table` and the dependent-age counter promoted to st_utils.R. Bit-identical on the 63,648-result grid. |
| 7 | Generalize dense-table CSV; retire long YAML vectors | Suggestion | RESOLVED 2026-07-18 (two commits): credit_tables schema generalized to (credit_id, state, year, filing_status, key_concept, income_lower, income_upper, value); CA migrated in place (values untouched); then ALL income-banded YAML tables migrated with the sanctioned behavior change — **income is rounded to whole dollars before dense-table lookups (the forms' own instruction)**: CT Table E (108 rows, filing-status-keyed), CT retirement factors (40 rows, 2 vintages, zero-tails omitted; absent-table default stays factor 1), KY family-size Table C (80 rows, family-size-keyed, -Inf bottom band preserves the old clamp), VA CLI poverty table (72 rows). Lookup supports filing-status-keyed rows; loader forbids mixing status-0 and status-keyed rows in one table version. Verified: all whole-dollar worksheet pins unchanged; dense grid (whole-dollar incomes, incl. exact band edges) bit-identical; the fractional divergence is pinned by test CT-8. YAML keeps only short structural vectors (rates/brackets, recapture segments, NY hh base+incr formula tables, NC 7-band child table, interpolated CDCTC anchors). Sentinel count 8 -> 5. |
| 8 | Explicit income base per phase-out | Suggestion | RESOLVED 2026-07-18: uniform `st_income_base()` enum (1 fed AGI / 2 state AGI / 3 AGI+additions / 4 earned income / 5 AGI less taxable SS) in st_utils.R; existing base params route through it; KY family credit and CT retirement factor bases now configurable (defaults reproduce the old hardcodes); VA age_ded_po_base moved to enum code 5. Remaining hardcoded bases become configurable with a one-line change when a state needs it. Bit-identical. |
| 9 | Coverage + continuity test layers; start cross-model | Suggestion | OPEN |
| 10 | No DSL rewrite | Guidance | — |
