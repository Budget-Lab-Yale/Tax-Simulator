# Overnight status — top-tax v3 (morning briefing)

**Date:** 2026-07-18, ~01:10 EDT

## TL;DR

The v3 re-run **failed on a disk-full error caused by my mistake** (wrong `local`
flag). It is **not a code bug** — the calculator/config changes ran correctly for
thousands of tasks before the disk filled. I cleaned up the mess I made. The run
is **blocked on scratch space** and needs one decision from you (delete a
superseded 3TB vintage) before a one-command relaunch. I did **not** build the
figures/document on stale v2 data — that would be superseded the moment v3 lands.

## What went wrong

- I launched with `local=0`, which writes to the **production** root
  `/nfs/roberts/project/pi_nrs36/shared`. That project space is quota-capped at
  4.0T and was already ~93% full. The run wrote 321G, hit 100%, and every
  subsequent write failed — producing ~26% Phase 2A (static) task failures with
  truncated logs (the give-away: the log write itself failed, so no R error).
- **The changes are fine.** Phase 1 (baseline) and 1B (frozen) completed; ~3,300
  Phase 2A tasks completed before the disk filled. So `no_ord_cap`, the
  `corp.rate` entity-shifting fix, and the estate-avoidance fix all executed
  without error. This was purely out-of-space.
- Correct target is the **local/scratch** root (`local=1`, the CLAUDE.md default),
  where v2 lives and where `report_metrics.py` / `run_fit_chain_v3.sh` point.

## What I already did

- **Deleted** the 321G of failed-run partial output from the full `/project`
  space (freed the pi_nrs36 quota back to 93%). Nothing else on `/project` was
  touched.
- Staged the corrected relaunch and the downstream chain (see below).

## What you need to decide (the one blocker)

The full run keeps per-record detail → **~2.9T** footprint (v2 is 2.9T). Scratch
has only **231G free**. You need to free **~2.7T**. The clean candidate is the
superseded dev vintage (pre-eta-repin, replaced by v2):

```
rm -rf /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v1   # 3.0T
```

I did **not** delete it — a 3TB irreversible deletion of your data is your call,
not mine, especially since you didn't ask me to manage vintage storage. Other
reclaimable candidates if you'd rather keep v1: `top_tax_v1` (733G, superseded
factorial), `202607081937` (260G), `kg_dyn_*_check` (~90G each).

## One-command relaunch (after freeing space)

```
bash other/top_tax/relaunch_v3.sh
```

It refuses to launch unless scratch has ~2.7T free, then runs:
`bash slurm_run.sh top_tax/dials NULL jar335 1 top_tax_dials_30y_v3 1 0 NULL 0`
(note `local=1`). Vintage lands at
`/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3`.

## What's ready to finish the job once v3 drains

All login-node-safe, no more SLURM after the sim:

1. **Interactive/atlas refit:** `bash other/top_tax/run_fit_chain_v3.sh`
   (backs up the v2 atlas, then fit → validate → build → render-check).
2. **Metrics:** point `report_metrics.py`'s `V2` constant at the v3 path, run →
   `metrics_results.md`. Sanity-check: corp scenarios now show nonzero
   entity-shifting, and CG rates now bite above 37%.
3. **Figures 1–5:** house style (Okabe-Ito, Charter+system-ui, light/dark).
   Fig 5 (`fig5_cg_laffer_mock.html`) must be rebuilt on the **uncapped** data —
   the 37% plateau is gone now.
4. **Document:** `report_text_v3.md` from `report_text_v3_source.md`, filling the
   TODOs (Fig 4 third-decade cg+5pp step-up vs deemed; Fig 5 description) and
   refreshing every cited number.

Ping me after the relaunch and I'll drive steps 1–4 to completion.

## The unfilled TODOs in the text (need v3 numbers)

- Fig 4: "raising the top capital gains rate by 5 percentage points yields **TODO**
  in the third decade [step-up]" and "...would raise **TODO** [deemed]."
- Fig 5: description of the CG Laffer curve.
- Reconcile: uncapping removes Fig 5's mechanical plateau, which sits in tension
  with the draft's "low-to-mid 30s revenue-max" literature claim — I'll flag this
  in the document.
