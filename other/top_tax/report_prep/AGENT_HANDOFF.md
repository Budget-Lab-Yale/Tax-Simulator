# Agent handoff — top-tax report v3 finish-line

**Written:** 2026-07-18 ~07:30 EDT, for the next LLM agent (or future me).
**Owner/user:** John Ricco (jar335), Budget Lab at Yale.

This is a self-contained runbook. Read it top to bottom before acting. Cross-refs:
`OVERNIGHT_STATUS.md` (same dir), the memory file `overnight_finish_line_v3.md`,
`DOSSIER.md`, and `output_data_map.md`.

---

## 1. The mission

Finish the deliverables for the top-tax public report, refreshed onto a new model
vintage **`top_tax_dials_30y_v3`**:

1. **All interactive calculations** — refit the atlas/surrogate that powers the
   interactive tool (`atlas2_built.html`).
2. **All figures** — Figures 1–5 in the house style, on v3 data.
3. **A new document** — `report_text_v3.md`: the user's front matter with every
   `TODO` filled and every cited number refreshed to v3.

The source front matter (verbatim, pre-refresh) is in
`report_text_v3_source.md` in this directory. Do NOT lose it.

Everything is **gated on the v3 SLURM run finishing.** As of this writing the run
is in **Phase 0** (building configs on the login node); no array jobs are queued
yet. Once it drains successfully, the whole downstream chain (§5) is login-node
python/node/writing — **no more SLURM.**

---

## 2. What v3 changes vs the shipped v2 (all deliberate, all in the working tree/branch)

1. **Uncapped capital-gains rate (`no_ord_cap`).** New flag in `calc_tax`
   (`src/calc/functions/tax/tax.R`): `pref.no_ord_cap` (default 0). When 1, it
   removes the `pmin(liab_max, …)` Schedule-D "gains taxed no more than ordinary"
   ceiling, so a CG rate above the 37% ordinary top rate actually bites. Set to 1
   on all 123 top-tax CG-rate scenarios (`pref.yaml` in each). Baseline stays 0
   (a no-op there anyway). Tested clean in `other/no_ord_cap/`.
   **Consequence:** Figure 5's Laffer curve no longer plateaus at ~37% — that
   plateau was the mechanical cap. CG rates now keep raising revenue past 37%
   until behavior turns them over (low-to-mid 40s effective).
2. **Corporate rate parameterized for entity-shifting (`corp.rate`).** v2 BUG:
   corp scenarios applied the 21→28% increase only through the Off-Model-Estimates
   revenue channel; `corp.rate` (which is the ONLY corporate input the
   `entity_shifting` behavior module reads) stayed at baseline 0.21, so the module
   produced ZERO response to the corporate increase. Fixed by adding a `corp.yaml`
   (`rate: {2014:0.35, 2018:0.21, 2027:0.28}`) to all 114 corp scenarios.
   `corp.rate` feeds only the shifting wedge, NOT corporate revenue (that stays
   with OME) — no double count. **Consequence:** corp scenarios now show real
   C-corp→pass-through recharacterization (corp revenue leaks, ordinary rises),
   which the Figure 3 narrative describes. Verify `corp_tax_change != 0` for corp
   scenarios in the v3 output.
3. **Estate-avoidance fix** (committed `ff5fd8037`) — v3 is the pending batch
   re-run that clears it; 198/199 dials scenarios carry the `estate/avoidance`
   behavior module.

---

## 3. Current run state & how it was launched

- **Vintage path:** `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/`
- **Launch command (already running):**
  `bash slurm_run.sh top_tax/dials NULL jar335 1 top_tax_dials_30y_v3 1 0 NULL 0`
  Args: runscript=`top_tax/dials`, all scenarios, user=jar335, **local=1** (scratch —
  NOT 0/production, see §7), vintage, pct_sample=1, stacked=0, baseline_vintage=NULL,
  delete_detail=0. 199 scenarios, 30 years (2027:2057).
- **Phases (DAG):** P1 baseline → P1B frozen → P2A static → P2B bathtub → P2N
  conv-no-wealth → P2W wealth → P2C conventional → P3a aggregate → P3b post-process.
  stacked=0 ⇒ no Phase 4. All array jobs are named `taxsim-*`.
- **Getting job IDs (once submitted):** `squeue -u jar335 -o "%.14i %.20j %.2t" | grep taxsim`.
  The terminal job is the P3b `taxsim-postproc` array — when it and all other
  `taxsim-*` jobs leave the queue, the run is done (or failed).
- **Per-task logs:** `…/top_tax_dials_30y_v3/_slurm_staging/logs/p{phase}_{jobid}_{taskidx}.log`.

---

## 4. How to monitor to completion

Poll until no `taxsim-*` jobs remain, e.g. a background command:

```bash
IDS=$(squeue -u jar335 -h -o "%A %j" | awk '$2 ~ /taxsim/ {print $1}' | sort -u | paste -sd,)
while squeue -j "$IDS" -h -t PD,R,CG,CF 2>/dev/null | grep -q .; do sleep 180; done
```

Then run the success check in §5.0. (Run this as a `run_in_background` bash command
so you're notified on exit; the run likely takes several hours — cluster-load
dependent.)

---

## 5. Downstream chain — DO THIS WHEN IT DRAINS

### 5.0 Verify success FIRST (do not build on a broken run)
```bash
IDS="<all taxsim job ids, comma-sep>"
sacct -j "$IDS" --format=JobID%20,JobName%16,State%18,ExitCode -n | grep -iE "FAILED|CANCELLED|TIMEOUT|OUT_OF"   # want empty
V3=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3
for p in baseline/static/supplemental/distribution_etrs.csv \
         stack_ref/conventional/supplemental/revenue_estimates.csv \
         s_corp_r28/conventional/supplemental/revenue_estimates.csv \
         s_cg_r50/conventional/supplemental/revenue_estimates.csv; do
  [ -f "$V3/$p" ] && echo "OK $p" || echo "MISS $p"
done
```
If tasks FAILED: read a failed task's log (path in §3). **The 2026-07-18 failure
was disk-full from launching with local=0 into the full /project quota — NOT a
code bug.** If FAILED again, get the real R error before assuming anything.

### 5.1 Refit the interactive/atlas
```bash
bash other/top_tax/run_fit_chain_v3.sh   # backs up v2 atlas, then fit → validate → build → render-check
```
Must print `ALL GREEN (v3)`. Renders `other/top_tax/atlas2_built.html`.

### 5.2 Regenerate metrics
`report_metrics.py` currently hard-codes the **v2** path in its `V2` constant.
Point it at v3 (verify the v3 CSV schema matches v2 first), then:
```bash
python3 other/top_tax/report_prep/report_metrics.py > other/top_tax/report_prep/metrics_results.md
```
Sanity checks: corp scenarios now show nonzero entity-shifting; CG rates bite
above 37% (s_cg_r45/r50 conv should be well above the old ~$564B plateau);
baseline-on-baseline residual near zero.

### 5.3 Figures 1–5 (house style: Okabe-Ito CSS-var palette, Charter+system-ui,
light+dark, inline SVG hand-drawn; see the `*_mock.html` files here for the exact
CSS. Smoke-test each with a node DOM stub before publishing; publish as Artifacts.)
- **Fig 1** — income by overlapping top group, cash + accrual bars (atlas
  `income_levels`). Build fresh (see `base_figure_mock.html` for style).
- **Fig 2** — effective rates cash + accrual. NOTE: the draft's "26 → 15%" is
  **self-ranked** (top 1% by each income concept). Fixed/cash-ranked would be
  "26 → 18". The draft intends self-ranked — keep it, footnote the convention.
- **Fig 3 & 4** — ordered parts-vs-whole: CG 20→25% then corp 21→25%, step-up
  (Fig 3) vs deemed-at-death (Fig 4). This IS `policy_figure_ordered_mock.html` —
  refresh its hardcoded numbers from v3 (the mock uses v2 surrogate values).
- **Fig 5** — CG Laffer by death regime = `fig5_cg_laffer_mock.html`. **MUST be
  rebuilt on v3 UNCAPPED data** — the step-up plateau is gone now, so the curves
  and the whole narrative change. See §6 for the reconciliation to write.

### 5.4 Write `report_text_v3.md`
From `report_text_v3_source.md`, fill every TODO and refresh every number:
- **Fig 4 TODOs:** "raising the top CG rate by 5pp yields **TODO** in the third
  decade [step-up]" and "**TODO** [deemed]." Third decade = FY2047-2056 (30y).
  Under step-up that's `s_cg_r25` conv (2047-56 window); the deemed value is the
  cg25+deemed cross (surrogate-composed or from a direct cross if present).
- **Fig 5 TODO:** write the Laffer-curve description.
- Refresh: income levels, ETRs, $1.4T remit, 26%/one-third revenue share, corp
  spillover (~11¢), "less than two-thirds of static" package survival, the
  "0.08pp of GDP" CG behavioral loss in the Fig 3 text.
- Voice: calm economist-memo voice (see `feedback_doc_voice` memory) — no LLM
  tics, no codenames, selection over compression.

---

## 6. The one narrative tension to handle in the document

Uncapping (§2.1) removes Figure 5's **mechanical** 37% plateau. But the draft's
prose says the CG revenue-maximizing rate is "somewhere in the low-to-mid 30s"
(a realization-elasticity literature claim). With the cap off, the model's own
curve keeps rising past 37% until the behavioral peak in the **low-to-mid 40s
effective**. So the figure and the prose will fight unless reconciled. Options to
present to the user: (a) cite the low-30s as external lit and note the model's
richer margins push the peak higher, or (b) rewrite the claim. Flag this
explicitly in the doc; don't silently paper over it. (Backstory: the 37% wall is
the Schedule-D "not more than ordinary" rule — see the per-segment elasticity
work; step-up base elasticity w.r.t. net-of-tax rate ≈ 1.3–1.6, declining.)

---

## 7. Gotchas / lessons (READ before touching the run)

- **local=1, NOT 0.** `output_roots.yaml`: local=`/scratch/pi_nrs36/jar335`
  (231G→3.7T free after cleanup), production=`/project/pi_nrs36/shared` (4.0T
  group quota, ~93% full). v2 and all report vintages live on the **local/scratch**
  root, and `report_metrics.py`/`run_fit_chain_v3.sh` point there. Launching with
  local=0 fills the shared quota and disk-fails the run. (That's exactly what
  happened on the first v3 attempt; it was cleaned up.)
- **Never run `Rscript`/`R` on the login node for compute** — sbatch it. The ONE
  exception is `slurm_run.sh`'s own Phase 0 setup (that's the sanctioned launcher).
- **Full run footprint ≈ 2.9T** (detail kept). Needs ~2.7T free on scratch. Space
  was freed on 2026-07-18 by deleting `top_tax_dials_30y_v1` (3.0T, superseded),
  `revmax_eta_v1`, `202607081937`.
- **pct_sample must be 1** for kg_dynamics scenarios (subsampling makes cell
  aggregates noisy). Guards enforce it.
- **Run one year past the window** — estate/wealth legs book FY death-year+1; the
  runscript already uses 2027:2057.
- **xlsx outputs are never byte-identical** (docProps timestamp) — don't treat
  that as a diff.
- **corp entity-shifting is NEW in v3** — v2 corp numbers will differ; that's
  expected and correct, not a regression.
- Print **full absolute paths** for every deliverable in summaries.

---

## 8. Key paths

| What | Path |
|---|---|
| v3 vintage | `/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3/` |
| Repo | `/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/` |
| Report-prep dir | `…/Tax-Simulator/other/top_tax/report_prep/` |
| Source front matter | `…/report_prep/report_text_v3_source.md` |
| Target document | `…/report_prep/report_text_v3.md` (create) |
| Metrics script / output | `…/report_prep/report_metrics.py` / `metrics_results.md` |
| Atlas fit chain (v3) | `…/other/top_tax/run_fit_chain_v3.sh` |
| Atlas data / built page | `…/other/top_tax/atlas2_data.json` / `atlas2_built.html` |
| Figure mocks | `…/report_prep/{base_figure_mock,policy_figure_ordered_mock,fig5_cg_laffer_mock}.html` |
| no_ord_cap test | `…/other/no_ord_cap/` |
| Relaunch helper | `…/other/top_tax/relaunch_v3.sh` |

---

## 9. If the user asks for status while the run is going

It's a multi-hour SLURM run (~19,400 array tasks across the phased DAG). Give an
honest ETA once you can read Phase 1/2A throughput off `squeue`/`sacct`; the
downstream chain (§5) is ~30–60 min after the sim drains.
