---
title: "Non-filer rework — federal validation procedure"
role: procedure
workstream: state_weights
status: current
updated: 2026-08-19
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Federal validation of a reworked non-filer population

The runbook for part (b) of the non-filer rework: proving that a new Tax-Data vintage
changes what it should and nothing else, before the state-weights fit is refit on it
and before either is swapped into production.

**Why this is a procedure and not part of the plan.** It sequences steps *within* one
task, so it does not go stale when priorities move. The plan
(`research/state_weights/plan.md`, task group E) owns whether and when this runs; this
document owns how. Its 4a table is the acceptance gate.

**Provenance.** Extracted 2026-08-19 from the unified non-filer + state-weights plan
drafted 2026-08-18, which is archived whole at
`research/archive/nonfiler_unified_plan_2026-08-18_imported.md`. The text is unchanged
apart from this header and the closing verification section, which was that document's
`## Verification` block.

---

The key insight that makes this testable cheaply: **the model's post-processing is sharply
split between filer-gated and full-population aggregates**, so the rework has a predicted
signature, and anything outside that signature is a bug.

Verified in `src/data/post_processing/summary_stats.R`:

- `get_1040_totals()` at `:141-149` pre-multiplies the reporting counts by `filer`
  (`n_returns = filer`, `n_returns_dep`, `n_adults`, `n_people`, `n_single`, `n_joint`,
  `n_hoh`, `n_dep`), and `:193-195` sums every **tax variable** as `. * weight * filer` —
  so non-filer records contribute **exactly zero** to every 1040 dollar aggregate.
- `:144` defines `n_nonfilers = !filer` and `:140` `n_tax_units = 1`, both summed on
  unrestricted `weight`.
- `:198-200` computes `mtr_*` as `weighted.mean(., weight)` — **unrestricted**, so
  non-filers are in the denominator.
- **`get_pr_totals()` at `:214-278` has NO `filer` gate** — `:267-270` sums every payroll
  variable on unrestricted `weight`. Payroll totals therefore **include non-filers**.

And `src/data/post_processing/distribution.R` uses unrestricted `weight` throughout:
`income_pctile` / `agi_pctile` at `:265` and `:296`, `n_tax_units = sum(weight)` at `:338`,
and the `share_cut.*` / `share_raise.*` denominators at `:354-361`. `horizontal.R`
references `filer` nowhere, so it too includes non-filers.

### 4.0 Three verified hazards that shape the whole battery

**(i) Random numbers are bound by row POSITION, not by id.** `src/sim/run.R:348-357` does
`read_microdata(year) %>% filter(id %in% globals$sample_ids) %>% ... %>%
bind_cols(globals$random_numbers)`, where `globals$random_numbers`
(`src/misc/config_parser.R:227-239`, seeded `set.seed(76)`) is a tibble of nine `r.*`
columns with `length(sample_ids)` rows, joined **positionally**. If the reworked Tax-Data
writes rows in a different order — a sort inside `calibrate_nonfilers.R`, a changed append
order — every *filer* record receives different draws for `r.cdctc_takeup`,
`r.salt_workaround`, `r.bus_loss`, `r.oasdi_exp`, `r.new_car`, `r.eitc_precert`. Filer-side
aggregates would then move for reasons having nothing to do with non-filers, and the entire
invariance battery becomes uninterpretable.

**Blocking gate: ordered `identical()` on the `id` column** — across vintages *and* across
years (the design also assumes every year's file shares the 2017 file's row order). Not
`setequal()`.

**(ii) `sample_ids` come from one vintage's 2017 file.** `config_parser.R:219-225` takes
`interface_paths %>% filter(interface == 'Tax-Data') %>% slice(1)` — the **first runscript
row**, which in a two-vintage A/B is the *old* vintage. Any record present only in the new
vintage is silently dropped by `filter(id %in% globals$sample_ids)`, even at
`pct_sample = 1`. Since the rework keeps the PSZ record set intact this should be a no-op,
but it must be verified rather than assumed.

**(iii) The current-law baseline already routes money to non-filers in 2020–2021.**
Verified in the baseline tax law: `config/scenarios/tax_law/baseline/rebate.yaml` sets
`value` to 1800 (2020), 1400 (2021), 0 (2022); `ctc.yaml` sets `min_refund_young` 3600 and
`min_refund_old` 3000 in 2021 only. So `become_filer_rebate` fires in 2020–2021 and
`become_filer_ctc` fires in 2021 **without any reform scenario**.

This is the most valuable single finding for part (b): **run the baseline over 2020–2022 and
the non-filer channel is live, disciplined against published actuals** — EIP3 ≈ $402–411B
disbursed, advance CTC ≈ $93B. It is the only place in the battery where a model output
that depends on the non-filer level is checked against a real-world number. It also means
my framing below has to be year-conditional: current-law invariance holds for 2017–2019 and
2022+, but **not** for 2020–2021, where the level change *should* move federal outlays.

### 4a. The predicted signature — write it down before running anything

**In years 2017–2019 and 2022+** (no refundable credit reaches non-filers under baseline law):

| Must NOT move | Must move |
|---|---|
| Every dollar aggregate and nonzero count in `totals/1040.csv` and `totals/1040_by_agi.csv` | `n_tax_units` (reported) |
| `n_returns`, `n_returns_dep`, `n_dep`, `n_simple_filers` | **`totals/payroll.csv` — every line**, because `get_pr_totals()` is not filer-gated |
| **Every line of `supplemental/cbo_comparison.csv`** — all mapped lines are 1040 tax vars plus `n_returns`/`n_itemizing`, all filer-gated | `revenues_payroll_tax` in `receipts.csv`, following payroll |
| `time_burden` outputs (it filters `filer != 0` at `time_burden.R:70`) | `mtr_*` weighted means (non-filers are in the denominator) |
| Any reform score with no `become_filer` path — see 4f | Percentile **cut points** in the production distribution universe (but *not* visible in the A/B's own `distribution.csv` — see 4g) |

**In 2020–2021** the rebate and fully-refundable-CTC baseline provisions are live, so
`n_returns`, `rebate`, `ctc_ref`, `ref` and `outlays_tax_credits` **must also move** — that
is the signal, not noise.

**`n_nonfilers` is computed and thrown away.** `summary_stats.R:143` derives
`n_nonfilers = !filer`, but the reported set `demographic_vars` (`:23-30`) is only
`n_tax_units`, `n_returns`, `n_returns_dep`, `n_dep`, `n_simple_filers`. The single most
relevant metric for this rework never reaches an output file. **Add `n_nonfilers` to
`demographic_vars`** — a one-line change that makes non-filer mass a permanent column of
`1040.csv` in every future run. While there, `n_adults` (`:144`) reads
`filer * (1 * (filing_status == 2))` where the adult count should be
`1 + (filing_status == 2)`, and `n_people` (`:145`) reads `n_dep` before its own
reassignment on the next line; both are unreported today, so fix or delete them rather than
leaving broken derivations in place.

**Payroll revenue will move, and that is the correct behaviour** — non-filers owe payroll
tax on wages whether or not they file, and the current non-filer slice already carries
$147.3B of wages (2022). Raking non-filer weights up by 15–25% and repairing their wage
distribution therefore changes a headline federal number. **Predict and quantify this
before the run**, roughly as the non-filer wage total × the effective payroll rate scaled
by the weight change; a payroll delta far from that estimate means the calibration moved
weight into the wrong wage cells. This is the one place where a non-filer-only change hits
federal revenue directly, and it would read as a bug if nobody expected it.

**The CBO comparison being invariant is the strongest single check in the battery.** If
`cbo_comparison.csv` is not identical between the old and new vintage under current law,
either a filer-side variable was touched or the filer weights moved — both contradict the
design (§5.2 rakes non-filer weights only; §5.3 leaves filer weights and the ledger
untouched). Investigate before proceeding, do not absorb it.

**The distributional shift is real and must be communicated, not explained away.** Adding
6–9M non-filer adults, correcting their age distribution, and giving them nonzero
investment income moves the income percentile boundaries that every published
distributional table rests on — with identical tax law. This is the mechanical consequence
of a better full-population base, but it means a vintage swap changes published
distribution tables, and that needs flagging to whoever owns those outputs.

### 4b. Pre-flight vintage gate and Tax-Data-side assertions, before any model run

Cheapest checks, and they catch most errors. Two layers.

**Layer 1 — a pre-flight vintage comparison, login-node, minutes.** New script
`nonfiler_residual/05_preflight_vintage.R --old <vintage> --new <vintage>`, run before any
cluster time is spent. Fail any of these and stop:

| Check | Criterion | Guards |
|---|---|---|
| Ordered id identity, every year 2017:2035 | `identical(old$id, new$id)` — exact, ordered | hazard 4.0(i), the positional random-number pairing |
| Within-vintage id order stability | `identical(ids_2017, ids_y)` for all `y` | the cross-year pairing assumption |
| Row count and column set equal; no new column names; `qual_div` absent | exact | the class of bug F3 came from |
| **Filer slice byte-equality** — the `filer == 1` subset identical on every column including `weight`, all years | exact | proves the "non-filer only" claim *before* any run, and is what makes 4a an exact-equality test rather than a tolerance argument |
| `filer ∈ {0,1}`, `dep_status ∈ {0,1}`, no NA; `filer == 0 ⟹ dep_status == 0` | hard | |

Also make the Stage-D diagnostics re-runnable against a candidate vintage: `03_diagnose_
current_nonfilers.R:47-53` currently hardcodes the vintage from `interface_versions.yaml`.
Add a `--vintage` argument, then re-run `--tables` on each candidate and diff T1–T4 against
V0. This costs nothing (13k rows per year) and is the primary acceptance evidence for F1–F4.

**Layer 2 — in-pipeline assertions.** In `impute_nonfilers.R` and `calibrate_nonfilers.R`:

**Hard (`stopifnot`) — invariants that cannot be true and wrong:**
- `filer == 0` and `dep_status == 0` on every appended record (§3a — currently only
  implicit via the zero-fill at `impute_nonfilers.R:136-142`).
- **Row order preserved end to end:** `identical(ids_in, ids_out)`. This is the assertion
  that makes hazard 4.0(i) a non-issue rather than a landmine.
- No `NA` in the columns the append writes; `age1` within the band it was drawn from;
  `age1 >= 18`; `filing_status == 2 ⟹ age2 >= 18`.
- `all(weight > 0)` and `is.finite(weight)` on the non-filer slice — a rake against a
  near-zero target cell can produce zeros or infinities.
- Filer weights bit-identical before and after `calibrate_nonfilers.R` — this is the
  guarantee that makes 4a's invariance prediction hold, so assert it rather than hope.
- Post-calibration non-filer adult total within the P4 tolerance of the target, and each of
  the ≤14 age × marital cells within 0.5% of its own target.
- `div_ord + div_pref` reconciles to the DINA `fidiv` aggregate within 1% — the actual test
  of the F3 dividend fix.
- **No column written that is absent from the committed output schema** — the generic form
  of the `qual_div` bug, which is exactly the failure of writing a column nobody checks.

**Soft (printed before/after diagnostic table) — judgment calls:**
- Non-filer adult level vs the 38–41M comparable-universe anchor.
- Age distribution vs `resources/nonfiler_age_shape.csv` (the F2 fix — report the 18–25
  and 65+ shares explicitly, since those are the two the diagnostics indicted).
- Receipt rates for interest / dividends / gains / SS / pensions against Pub 5785's
  14% / 9% / 4% / 48% / 14%. **These are ceilings, not targets** — our imputed non-filers
  should sit at or below them, because Pub 5785's universe is information-return based and
  wider than ours.
- Weighted totals by `income_tier()` before/after, since the state fit's non-filer prior
  keys on it.
- **Weight concentration.** 13,204 non-filer records carry 27.6M units today (mean weight
  ≈ 2,090); at 38–41M adults the mean goes to ~2,900–3,100, and the 18–25 band must carry
  ~11.5M adults on roughly 3,200 records. Report the max and 99th-percentile non-filer
  weight and the before/after weight-ratio distribution. Nationally this is tolerable; it is
  thin for the state × age-band cells the rework exists to serve, so record it here rather
  than letting step 5 discover it.

### 4c. Stage the changes as separate vintages

The four Tax-Data changes touch different things and two of them can mask each other (the
calibration rake and the aging fix both move non-filer weights). Build vintages
incrementally:

| Vintage | Contains | What its A/B isolates |
|---|---|---|
| V0 | current production | baseline for every diff |
| V1 | 3a composition fixes only (dividends, investment income, age draw) — **no reweighting** | Attribute changes at fixed weights. `n_tax_units`, `n_nonfilers` **and `totals/payroll.csv`** must all be **unchanged** (the fixes touch investment income and age, never wages); only distribution cut points and `mtr_*` move |
| V2 | V1 + `calibrate_nonfilers.R` | The level fix alone. `n_nonfilers` and payroll move; all **1040** filer-gated aggregates still frozen |
| V3 | V2 + `project_puf.R` aging fix | Projection-year behaviour only — 2017 and other historical years should be **identical to V2** |

The staging pays off precisely because payroll separates the two changes: it must be frozen
in V1 and must move in V2. V1's prediction is the sharpest — a change in `n_tax_units` or
payroll at that stage means the composition fix accidentally touched weights or wages. V3's
is nearly as sharp: any change in a pre-2018 year means the aging fix leaked into history.

Whether this is worth the extra cluster runs: **yes for V1 and V3**, because each has an
exact-equality prediction that a combined run cannot test. V2 could be folded into V3 if
cluster time is tight, at the cost of losing the level-vs-aging attribution.

### 4d. The vintage A/B runs

Use the pattern already in `config/runscripts/tests/new_baseline.csv`, which runs the same
`baseline` tax law twice against different Tax-Data vintages via the
`dep.Tax-Data.vintage` / `dep.Tax-Data.ID` columns:

**Run 1 — the cheap A/B, 3 years, all four vintages.**
`config/runscripts/tests/nonfiler_ab.csv`:

```csv
ID,tax_law,behavior,years,dist_years,mtr_vars,mtr_types,dep.Tax-Data.vintage,dep.Tax-Data.ID
baseline,baseline,,2020:2022,2022,,,<V0>,baseline
nf_v1,baseline,,2020:2022,2022,,,<V1>,baseline
nf_v2,baseline,,2020:2022,2022,,,<V2>,baseline
nf_v3,baseline,,2020:2022,2022,,,<V3>,baseline
```

Years **2020:2022** deliberately: 2020 and 2021 are the live become-filer years per hazard
4.0(iii), and 2022 is both the anchor year and the first CBO-overlap year. Because
`stacked = 1` makes `build_stacked_1040_reports()` compute `. - lag(.)` in runscript row
order within year, this single runscript yields the V1/V2/V3 incremental attribution for
free.

**Run 2 — the full window, for the aging fix only.** The aging fix is the one change whose
signature is *year-shaped*, so three years cannot test it. Same runscript with
`years 2017:2030`, `dist_years 2026`, and only the `baseline` (V0) and `nf_v3` rows — V1 and
V2 add nothing over the window once run 1 passes. Acceptance: `n_tax_units` growth smooth
and monotone (no year-over-year jump > 1%); non-filer share of units flat within ±0.3pp from
2019 on (the direct F4 fix, cross-checked against T4); `n_returns` path identical to V0's;
no discontinuity in `revenues_payroll_tax` at the 2020 `population_factors_2020plus` seam.

Notes that apply to both:

- **Set `dist_years` explicitly.** Blank means *all years* (`config_parser.R:351-352`),
  against a Phase-3b budget of roughly an hour and 16GB, while `process_for_distribution()`
  expands each year's records sixfold. This is the one configuration mistake that turns a
  trivial run into a failed one.
- **`pct_sample = 1`, not less.** The non-filer slice is only ~13,204 records; sampling at
  0.1 leaves ~1,320 and destroys precisely the signal under test, while saving little
  (runtime is dominated by filer records). The A/B *is* paired — same `set.seed(76)`, same
  `sample_ids` — so a sampled run is defensible for the filer-side invariance check alone,
  but since that check is exact-equality anyway there is no reason to accept the noise. Use
  a `pct_sample = 0.1` pass only as a smoke test that the pipeline runs.
- **`delete_detail = 0`** is required: every record-level diagnostic below reads
  `static/detail/{year}.csv`. Budget 10–20GB on scratch and clean up after run 2.
- **`mtr_vars`:** include at least one so the `mtr_*` shift is observed rather than inferred.
- **Hazard 4.0(ii) applies:** `sample_ids` come from the *first* row's vintage (V0). With the
  4b gate passed this is a no-op, but do not run the A/B before that gate passes.

**Files to diff**, per scenario, under `<vintage_root>/<id>/static/` (written at
`src/sim/run.R:212-232`): `totals/1040.csv`, `totals/1040_by_agi.csv`, `totals/payroll.csv`,
`totals/receipts.csv`, plus `stacked_1040.xlsx` for the V1/V2/V3 attribution. Write a diff
script that classifies every changed cell against the 4a table and **fails on any change in
the must-not-move column** — that is the acceptance gate, not eyeballing. Commit it beside
the runscript so the next Tax-Data vintage swap is checked the same way.

Acceptance, in order:

1. **V1:** `n_tax_units`, `n_returns` and every `payroll.csv` column identical to V0 to full
   printed precision, all years. Any movement means row order or filer weights changed —
   back to the 4b gate. Only 2020–2021 `rebate`/`ctc_ref` may move, and only through the AGI
   phase-out on newly-nonzero non-filer investment income; expect well under $1B.
2. **V2:** `n_returns` identical to V0 in 2022 (2020–21 exempt); `n_tax_units` up by exactly
   the non-filer unit delta measured in 4b; and the `revenues_payroll_tax` delta explained to
   within 10% by (Δ non-filer weighted `gross_wages`) × the effective OASDI+HI rate —
   **computed from the detail files, not asserted**.
3. **V2, 2021 — the external check.** Baseline `rebate`, `ctc_ref` and `outlays_tax_credits`
   rise, and their *levels* are compared against published actuals: EIP3 ≈ $402–411B
   disbursed (~165M payments), advance CTC ≈ $93B. **A vintage whose 2021 baseline overshoots
   the actuals is over-massed regardless of what the anchors say.** This is the only place a
   model output that depends on the non-filer level meets an independent real-world number.
4. **V3:** 2017 bit-identical to V2.
5. **Non-filer credit mass, from `static/detail/{year}.csv`** (all needed columns are in
   `detail_vars`, `config_parser.R:241-258`): weighted count of `filer == 0`, non-filer adults
   `sum(weight * (1 + (filing_status == 2)))`, and non-filer `sum(weight * eitc)`,
   `sum(weight * ctc_ref)`, `sum(weight * rebate)`. That last group is credit mass the model
   computes and then multiplies out of every total — see 4f.

### 4e. The CBO benchmark — a null test, and say so

`cbo_comparison.R` benchmarks the baseline against the committed
`resources/cbo/cbo_iit_detail_feb2026.csv`. Verified properties that matter:

- **It runs only for `ID == 'baseline'`** (stated in its own header, and gated in
  `run.R`/`slurm/aggregate.R`). In a `new_baseline.csv`-style A/B the new vintage sits in a
  non-baseline row and **gets no CBO comparison at all**. A separate single-row runscript is
  required: `config/runscripts/tests/nonfiler_cbo.csv` with one `baseline` row pointing at
  V3, years `2022:2030`, compared against the V0 production run's `cbo_comparison.csv`.
- **The reference covers 2022–2036 only** (verified: minimum year 2022; 2022
  `Number of returns (millions)` = **161.3M**). TY2022 is in scope; **TY2017 is not, and
  never will be from this reference.** Benchmark 2017 `n_returns` against SOI Pub 1304
  TY2017 (~152.9M returns) by hand and record it in the findings memo. Do **not** regenerate
  the reference via `other/cbo/process_cbo_revenue.py` for this exercise — changing both
  arms at once destroys the comparison.

**`n_returns` should not move, and that is the point.** `summary_stats.R:141` defines
`n_returns = filer`, and the rework changes non-filer records only, so the correct
expectation against CBO's 161.3M is **zero change**. The CBO comparison's role here is a
**tripwire on the invariance claim, not a benchmark on the reworked object** — CBO's 1040
build-up publishes no non-filer line and therefore provides no direct discipline on
non-filers whatsoever. Be explicit about this in the write-up. The defensible claim is: the
anchors set the level, the 2021 baseline refundable-credit actuals are the one external
check on that level, and CBO confirms we did not disturb the filer side. Anything stronger
overstates what CBO's build-up contains.

Acceptance: `n_returns` `pct_diff` changes by < 0.1pp from V0's; every AGI, taxable-income
and tax line changes by < 0.05pp (all are filer-restricted, so more than that is a 4b gate
failure that slipped through); `n_itemizing` unchanged (non-filers do not itemize).

Worth adding to `cbo_comparison_mapping()` while in the file — these rows exist in the
reference and are unmapped today: `Earned income tax credit` → `eitc`,
`Child tax credit/credit for other dependents` → `ctc_nonref + ctc_ref`, and
`Number affected by the AMT` → `n_liab_amt`. EITC and CTC are the two lines the reform tests
below exercise hardest; having them permanently benchmarked is cheap and useful beyond this
project.

### 4f. Reform scenarios — where the change *should* bite

The change shows up through the become-filer path: `src/calc/do_taxes.R:126-127` sets
`filer = filer + (become_filer_ctc == 1 | become_filer_rebate == 1)`, with
`become_filer_ctc` at `src/calc/functions/credits/ctc.R:232`
(`filer == 0 & qual_ei == 0 & ctc_ref > 0`) and `become_filer_rebate` at
`src/calc/functions/credits/rebate.R:86` (`filer == 0 & rebate > 0`). Both gate on
`filer == 0`, so the increment is at most 1 — `filer` stays binary and `n_returns` remains a
clean return count.

`calc_rev_est()` always differences against the single `baseline` row, so scoring a reform
*at* a vintage requires that vintage on **every** row. Build two paired runscripts,
`nonfiler_reforms_v0.csv` and `nonfiler_reforms_v3.csv`, identical but for the vintage
columns — and set the vintage on the reform rows too, since omitting it silently reverts to
the `interface_versions.yaml` default.

| Scenario | Expected | Red flag |
|---|---|---|
| **Fully refundable CTC** (`tests/ctc_baseline.csv` pattern) | Outlay **rises**, bounded by (Δ non-filer units with `n_dep > 0` and `qual_ei == 0`) × max refundable value. The F2 age fix moves mass from 65+ into 18–25, *raising* the share of non-filers with young children, so expect a **larger** rise than the pure level effect implies | Outlay falls, or rises beyond the bound. Also: `Δn_returns` and `Δoutlays` must move **together** — they are linked through `do_taxes.R:127` |
| **Rebate / UBI** (`tests/rebate.csv`; or free, via the 2021 baseline) | The cleanest quantitative check: `become_filer_rebate` has **no earnings condition**, so `Δn_returns(V3)/Δn_returns(V0)` should track `nonfiler_units(V3)/nonfiler_units(V0)` within ±5%, the residual attributable to the AGI phase-out on newly-nonzero investment income and to the marital-composition change from the rake | Ratio materially below the unit ratio ⇒ new non-filers are phasing out, i.e. the repaired investment income is too large (contradicting T3) or landed on the wrong 1040 line. Above ⇒ a weight/count accounting error |
| **Non-refundable control** (`tests/amt_repeal.csv` or `mort_int_repeal.csv`) | Score **bit-identical** across V0 and V3 | Any change at all. This is the single cleanest "did we break the filer side" test in the battery, because it has no non-filer channel whatsoever |

### 4f-bis. A finding the reform tests will surface: EITC has no become-filer path

**Correcting an assumption worth stating plainly.** I expected an EITC expansion to score
differently under the reworked file. It will not. `grep become_filer src/` returns exactly
two definitions — `ctc.R:232` and `rebate.R:86`. **There is no `become_filer_eitc`.** An
EITC reform therefore scores *identically* across vintages, and `Δscore ≈ 0` is the
**passing** result; a nonzero EITC delta would indicate the weight change leaked into filer
records.

That is a modelling gap, not a test artifact, and it cuts against the proposal's own
motivation. The proposal (§1) indicts DINA precisely because "all imputed tax units with
children and positive earned income file taxes, so there are no non-filers with eligibility
for refundable tax credits." The rework fixes that on the *data* side — it creates
earnings-bearing non-filer units with children. But on the *model* side those units still
cannot claim EITC by filing, and their computed `eitc` is multiplied out of every total by
the `* filer` gate in `get_1040_totals()`.

Compounding it, `become_filer_ctc` requires `qual_ei == 0` **exactly**, so a non-filer with
$1 of earned income who gains a refundable CTC keeps `filer = 0` and has its `ctc_ref`
multiplied out too. The rework *grows* the positive-earnings non-filer population, so this
silently-dropped credit mass grows with it — meaning the reworked file could make refundable-
credit scores *less* complete while looking like it did nothing.

**Action:** measure it (4d acceptance item 5 reports non-filer `eitc`/`ctc_ref`/`rebate`
mass), and raise the `qual_ei == 0` condition and the missing EITC path as an explicit
design decision **before** shipping the vintage. Do not fix either blind — both are
deliberate-looking choices whose rationale is not documented, and changing them changes
published refundable-credit scores. But do not let the rework ship with a memo claiming it
improves refundable-credit analysis when for EITC it cannot.

### 4g. Distributional and horizontal checks — do NOT read the built-in tables naively

Two verified structural problems mean the shipped tables cannot answer this question as-is:

- **`distribution.csv` is blind to a vintage reweighting.** `process_for_distribution()`
  (`distribution.R:130-163`) reads `weight`, `expanded_inc`, `agi`, ages and `filing_status`
  from the **baseline** detail file and joins only `(id, liab_iit_pr_reform)` from the
  counterfactual arm. In a vintage A/B, the new vintage's weights and incomes never enter the
  table. It is a *policy* differencer, not a *vintage* differencer. **This does not mean the
  distributional consequence is absent** — it means the A/B run will not show it. Once V3
  becomes the production baseline, every published reform's quintile boundaries and age cuts
  move, because the universe is all `dep_status == 0` units *including* non-filers.
- **`horizontal.csv` from a vintage A/B will contain garbage.** `build_horizontal_table()`
  (`horizontal.R:85-101`) filters `expanded_inc > 0` on the bound frame, then `left_join`s
  `inc` from the *baseline* arm also filtered to `expanded_inc > 0`. A record that is
  income-zero in the old vintage but income-positive in the new one — **exactly what the F3
  investment-income repair creates** — survives the filter with `inc = NA`, giving
  `etr = NA` and an NA income rank that contaminates the top quintile. Discard this table
  until `build_horizontal_table()` ranks on the scenario's own income (or uses an
  `inner_join`). Worth fixing on its own merits: it is a latent bug for *any* counterfactual
  that changes the set of income-positive records.

**Do this instead — a purpose-built level comparison.** New script
`nonfiler_residual/06_compare_level_dist.R`, reading `static/detail/2022.csv` from each arm
and computing, **on each arm separately** with `dep_status == 0`: weighted `expanded_inc`
quintile boundaries; count and share in the negative-income group; mean `expanded_inc` and
mean `liab_iit_net` by quintile; and the age cuts `distribution.R` uses. Expected:

- Quintile 1's boundary and mean income **fall**; Q2–Q5 boundaries shift down modestly.
- The percentile universe grows by the non-filer delta, so a fixed dollar income maps to a
  **higher** percentile.
- The `29 and under` share **rises** and `65+` **falls**, tracking the F2 correction. This is
  the most visible distributional consequence and the entire point of the age fix.
- **Red flags:** the negative-income group grows (nothing here should create negative
  expanded income — if it does, the DINA national-income mapping produced negative interest
  or dividends); any *upper*-quintile boundary moving more than ~0.5%; new non-filer mass
  landing above Q2; or a *fall* in the `29 and under` share.

**Horizontal equity, once the NA bug is fixed:** expect Quintile 1's within-group IQR to
**fall**, because the repair adds a large homogeneous block of `etr ≈ 0` records to the
bottom. That is a mechanical artifact of universe expansion, **not** an equity improvement,
and must be labelled as such. Red flag: Q1 IQR *rises*, which would mean the new non-filers
carry heterogeneous nonzero liability — i.e. repaired investment income large enough to
generate tax on people the model says do not file. Cross-check against Pub 5785's *amounts*
(≈$1.5B interest, $2.7B dividends, $1.2B gains across ~50M people in TY2016 — very small per
capita), not just its receipt rates. **This is the check most likely to catch an
over-generous hot-deck** in step 3a.

`time_burden.R:70` filters `filer != 0`, so its outputs belong in the must-not-move set.

### 4h. External triangulation — and its limits

Realistic:
- **The 2021 baseline refundable-credit levels vs Treasury/IRS actuals** (EIP3 ≈ $402–411B
  and ~165M payments; advance CTC ≈ $93B). The highest-value item in this section and the
  only external number that constrains the non-filer *level* through a tax-model output.
- **CBO's `Number of returns`** — filer side only, a null test (4e).
- **SOI Pub 1304 return counts** for TY2017 and TY2022 — filer side, and it covers 2017
  where the CBO reference does not.
- **TPC's published tax-unit count** (they report units *including* non-filers, ~190–200M)
  vs our `n_tax_units`. A genuine level check with a real concept gap: their unit definition
  and non-filer construction differ from ours, so treat a 5–10% gap as informative rather
  than failing.
- **The `cross_model/` harness, at record level.** It is unweighted, so it can say nothing
  about counts or levels. But it can do one very useful thing cheaply: push a stratified
  sample of the repaired non-filer records through TAXSIM-35 / PolicyEngine **as if they
  filed**, and check `agi`, `txbl_inc`, `liab_iit`, `eitc`, `ctc_ref` record by record. That
  is precisely the bug class that produced the `qual_div` failure — new income landing on the
  wrong line. Log divergences to `cross_model/federal_divergences.md`, which already exists.

Not realistic, and say so rather than implying otherwise:
- **The Pub 5785 receipt rates and the PEP−Pub 1304 age shape are calibration targets**, not
  validation. Checking output against them is a convergence check. There is no independent
  source for a non-filer age distribution or non-filer investment-income receipt rates, and
  Pub 1304 Table 1.1's bottom-AGI receipt counts — the closest thing to an independent bound
  — is also assigned as discipline in step 3a. Be honest that this loop is closed.
- **PolicyEngine's non-filer *count* is not a usable benchmark.** Their Enhanced-CPS unit
  definition, their own filing-requirement model, and their calibration targets differ enough
  that a level comparison is uninterpretable. Their value here is as a *record-level*
  calculator check (above) and as background for the ASEC construction research (step 1.1),
  not as a count benchmark.
- Anything requiring another model's *age distribution of non-filers* — not published at the
  granularity F2 is about.
- Reconciling our count directly to Pub 5785's 49.7–51.7M *persons*: different universe
  (information-return based, person level) and different years. A triangulation, not a
  target — which is exactly how the proposal §3.2 uses it.

### 4i. Regression guard

The rework makes `filer` load-bearing in ways nothing currently asserts. Permanent checks:

- **In Tax-Data:** the hard assertions from 4b, in the pipeline itself so a future change
  cannot silently reintroduce zero-investment-income or flat-age non-filers.
- **In Tax-Simulator `src/tests/`:** a new test asserting the filer-gating contract — that
  every `get_1040_totals()` tax aggregate is invariant to non-filer weights, while
  `n_tax_units`, `n_nonfilers` and every `get_pr_totals()` aggregate scale with them.
  Construct it by taking a small tax-unit fixture, doubling every non-filer weight, and
  asserting both halves. This encodes the 4a table as an executable invariant rather than
  a claim in a memo, and it would catch a future post-processing change that dropped the
  `* filer` gate in `get_1040_totals()` — or silently added one to `get_pr_totals()`.
- **The diff script from 4d**, committed alongside the runscript, so the next Tax-Data
  vintage swap is checked the same way.
- **An ordered-id-identity test** (`test_nonfiler_id_order()`), asserting `identical()` on the
  `id` column across all available years. This is the guard that would catch the positional
  random-number hazard 4.0(i) — the highest-probability way this rework silently corrupts the
  filer side, and currently unguarded anywhere.
- **A schema guard in `read_microdata()`** (`src/data/economy.R:477-494`), which today does
  `fread()` with **zero validation** — a renamed or dropped Tax-Data column produces NAs or an
  unhelpful downstream error. This is the natural home for the `filer`/`dep_status` domain
  checks and a column manifest, and it protects every consumer, not just this project.

Structure these as a `src/tests/test_nonfilers.R` function-only file following the convention
in `src/tests/test_state_tax_law.R`, guarded with a `file.exists()` skip so the suite still
runs where the restricted data is absent.

---

---

## End-to-end verification

The federal battery above is the acceptance gate. These are the state-side and
end-to-end checks that follow it.

The federal battery is step 4 (its 4a table is the acceptance gate). State-side and
end-to-end checks:

- **Anchors:** the two independent constructions of national filing adults must continue
  to agree within ~0.5% (205.5 vs 206.1M in 2017; 214.1 vs 213.1M in 2022), and the T1.6
  block sums must reproduce published all-returns totals exactly (the parse check already
  in `02_build_residual_anchors.R:96`).
- **Filing model:** ACS v1 filer units vs HT2 `n_returns` by state, inside the P4
  tolerance; ASEC-vs-ACS national filing rates reported side by side.
- **State fit:** targeted within-2% and MARD against config-7's 95.3%/0.43 baseline;
  the population identity per state within tolerance; state adults by age band vs PEP as
  the held-out metric; the EITC take-up correlation attenuated from −0.61.
- **Pilot liability:** IL / CO / NY recomputed; NY is the decisive case.
- **End-to-end:** a state-mode run under the new weights with `states` set, confirming
  federal outputs remain byte-identical to a federal-only run (the acceptance property
  already established in Phase 4).
- **Regression guard:** the permanent checks belong in `src/tests/` — see step 4.

