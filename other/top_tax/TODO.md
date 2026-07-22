# Top-Tax Project — Go-Live To-Do

Tracking the changes we need to land (and the re-runs they force) before the
top-tax report and interactive go live. Nothing here disappears when done — it
moves to the **Completed** bucket at the bottom.

Current shipped vintage: `top_tax_dials_30y_v3` (run 2026-07-18; uncapped CG +
corp.rate entity-shifting fix + estate-avoidance fix; on the OLD `levels`
semi-elasticity kg basis).

---

## To Do

### 1. Net-of-tax re-run

We flipped the kg realization response to the net-of-tax (`logs`) form as the
default (commit `66fb360af`). The top-tax vintages predate the flip and are on
the `levels` basis, so everything downstream must be regenerated on `logs`.

**No code change required — this is purely a re-run.** Verified 2026-07-22:
`logs` is fully pinned (`eta_tilde = 1.6625`, `timeable_share = 0.2542`,
Tax-Data `2026070814`), it is now the default, it propagates to every SLURM
phase via `--export=ALL`, and the calibration provenance guard passes for the
live form.

- [ ] Re-run `top_tax_dials_30y` on the net-of-tax basis → new vintage (e.g. `top_tax_dials_30y_v4`)
- [ ] Re-run the `kg_v3_revmax` death-regime Laffer grid on net-of-tax (currently flagged stale even pre-flip)
- [ ] Refit the atlas2 surrogate on the new vintage
- [ ] Regenerate metrics (`report_metrics.py` / `metrics_results.md`), Figures 1–5, the atlas2 interactive, and report text against the new vintage

### 2. Corporate tax rate scenarios

Two independent dimensions.

**Dimension A — denser corporate-rate grid.**
Currently the corporate rate dial spans 21 → 28. Expand to 21 → 35 so we have
more interpolation points for the surrogate.

- [ ] Decide the interval: 7-point (e.g. `{21, 28, 35}`) vs 1-point (`{21, 22, …, 35}`) — **undecided**
- [ ] Build the `corp.yaml` dial configs for the chosen grid
- [ ] Re-run (folds into the net-of-tax re-run above if sequenced together)

**Dimension B — corporate OME two-stream (static + conventional).**
The corporate Off-Model-Estimates interface must admit a *static* revenue-change
stream in addition to the conventional one. The static stream feeds (a) the
allocation of the corporate distribution and (b) the static-vs-conventional
breakout in our revenue modeling, including the top-tax interactive.

*(1) Standalone code change (feature):* **DONE 2026-07-22** (branch `wealth`;
built + full-sample smoke-verified; OME bumped v4→v5, `corporate_static` column).
- [x] Update the corporate OME interface to accept a static revenue-change stream alongside the conventional stream (`ome_corp_col()` in `revenue.R`; hard-stops on a pre-v5 vintage)
- [x] Wire the static stream into corporate distribution allocation (`get_other_taxes` in `distribution.R` reads the static stream; `distribution_etrs.R` inherits)
- [x] Wire the static stream into the static-vs-conventional revenue breakout (pass-aware `calc_receipts`; static books `corporate_static`, conventional books `corporate`; atlas2 python already reads both legs, so `sh`/`ch` corp vectors will differ after a real re-run — no python change)
- [x] Update downstream machinery to carry both streams (receipts booking + distribution carry both; `corp_incidence.R` record incidence deliberately stays on the conventional stream — D5)

*(2) Re-run + reproduce:* **not done** — only a fictitious-data smoke test
(`corporate_static = 1.1×corporate`) has been run. Real static numbers come from
upstream OME; the full re-run + figure/interactive/report regen folds into the
net-of-tax vintage regen (item 1). CAVEAT: v5 bump means any runscript with a
`dep.Off-Model-Estimates` override to a v4 vintage needs a v5 equivalent first.
- [ ] Re-run with the new two-stream corporate inputs
- [ ] Reproduce figures, the interactive, and all downstream deliverables

### 3. Reframe the "rate required to close the deficit" calculation

Reframe the current Table-1 question — *"what federal marginal rate would be
required to close the whole deficit from group X alone?"* — into its dual:

> If you imposed a **total federal marginal rate of 100%** on the income of
> each group, **what fraction of GDP would you raise?**

Do it **twice, separately**: once on a **cash** income basis and once on an
**accrual** income basis.

This is the naive upper-bound limit — no behavioral response, no consideration
of state taxes. Same underlying point as Table 1 (there isn't enough income at
the top to tax our way out of the deficit / fund new priorities), shown from
the other direction: the ceiling on revenue is the group's income itself, so
express it as a share of GDP and set it against the deficit.

- [x] Rework the calc script: 100% total federal marginal rate (INCLUSIVE of current law) on income above each group's threshold, ordinary + AGI bases → revenue as % of GDP. Done in `hundred_pct_ceiling.py` (+ `.json`); reproduces Table 1's base/current-marginal.
- [x] Report results and update `top_rate_needed_calcs.md` (new "100% rate ceiling" section; old rate-needed tables retained). Headline: 100% on AGI above the top-1% line raises 5.5% of GDP vs a 5.7%-of-GDP deficit; narrower groups far less.
- [ ] Still to update: the published Table 1 artifact + report text (folds into the report regen on the net-of-tax vintage).

**PARKED (2026-07-22) — accrual as a third column.** Explored adding accrual
(Haig-Simons) income as a third base. Landed on: adding accrued gains to AGI
doesn't change tax owed, it just enlarges the denominator, so the *implied
rate* roughly halves at the top (realized ETR ~26–30% → accrual ETR ~15% for
top 1%/0.1%/0.01%) — this is the accrual-ETR / economic-income story, already
in the top-400 breakout (31.8% → 8.1%) and `distribution_etrs`. Clean as an
*effective/average* rate; shaky as a *marginal* rate you could levy (needs
mark-to-market). OPEN QUESTION to resume on: **is there a cleaner POLICY
interpretation** of the accrual-widening exercise (beyond "effective rate on
economic income is ~half")? If we still want the exact above-threshold cut
(AGI-above-T vs AGI+accruals-above-T), it needs a short sbatch R pass — accrued
gains per record aren't in the flat detail file (built via `inc_hs_core` in
`distribution.R`: expanded − realized gains + `accruals_sum` − retirement
double-count).

---

## Completed / Resolved

- **Adjust corporate tax elasticity for entity shifting** (added then removed
  2026-07-22). Resolved: no adjustment needed. Coles-Patel-Seegert-Smith's 0.61
  accounting CETI is a bunching estimate on small C-corps at the $0 kink and
  excludes S-corps, so the C-corp ↔ pass-through margin we model downstream is
  largely NOT inside their 0.61 — the two are disjoint, so we don't subtract an
  entity-shift term from the upstream elasticity. (Whether to subtract at all
  hinges only on how the upstream team *uses* 0.61; if they apply it as total
  corporate-base erosion incl. conversion, revisit.)
