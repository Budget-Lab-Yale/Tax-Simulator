#!/bin/bash
#-----------------------------------------------------------------------
# launch_form_memo.sh  --  Part C1 driver (three experiments x two forms).
#
# form_memo.csv = baseline + rate2pp + deemed + rate2pp_deemed, full dials
# behavior stack, effective 2027, years 2026:2037 (reported window FY2027-36).
# The baseline is form-invariant (empty behavior), so it is built once by the
# levels leg and reused by the logs leg via baseline_vintage.
#
# REQUIRES: the logs leg needs KG_DYN_DEFAULT_ETA_LOGS + KG_DYN_TIMEABLE_SHARE_LOGS
# pinned + stamped (Part B3). delete_detail=1: the memo reads revenue_estimates.csv
# from supplemental (survives the purge), never per-record detail.
#
# Usage (from repo root):
#   bash .../launch_form_memo.sh levels          # baseline + 3 CF (levels)
#   bash .../launch_form_memo.sh logs            # 3 CF (logs), reuse levels baseline
#-----------------------------------------------------------------------
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

MODE="${1:?usage: launch_form_memo.sh <levels|logs>}"

case "$MODE" in
  levels)
    echo "form_memo LEVELS: baseline + 3 CF -> vintage form_memo_levels"
    KG_RESPONSE_FORM=levels \
      bash slurm_run.sh tests/form_memo NULL jar335 1 form_memo_levels 1 0 NULL 1
    echo "when this finishes (Phase 3b), launch the logs leg:"
    echo "  bash other/kg_model_tests/form_ab/launch_form_memo.sh logs"
    ;;
  logs)
    # Own baseline (NULL): the levels legs ran delete_detail=1, which purges the
    # baseline static detail that setup.R reads for baseline MTRs -- so the
    # levels baseline cannot be reused. The baseline is form-invariant (empty
    # behavior), so a fresh one is identical.
    echo "form_memo LOGS: baseline + 3 CF -> vintage form_memo_logs"
    KG_RESPONSE_FORM=logs \
      bash slurm_run.sh tests/form_memo NULL jar335 1 form_memo_logs 1 0 NULL 1
    ;;
  *)
    echo "unknown mode '$MODE' (use levels|logs)"; exit 1 ;;
esac
