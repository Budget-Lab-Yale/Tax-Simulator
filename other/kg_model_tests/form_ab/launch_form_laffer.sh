#!/bin/bash
#-----------------------------------------------------------------------
# launch_form_laffer.sh  --  Part C2 driver (17 rate/regime legs x two forms).
#
# form_laffer.csv = baseline + 17 CF (cg_{00,05,10,15,20,25}pp x {stepup,
# carryover, deemed}, rate knots 20->45%, no_ord_cap=1 so the 40-45% points are
# not flattened), revmax behavior stack, years 2026:2057 (figure metric reads
# the third decade FY2047-56). The big batch: run the two forms as two
# sequential vintages, baseline built once by levels and reused by logs.
#
# REQUIRES: the logs leg needs the logs constants pinned + stamped (Part B3).
# delete_detail=1: the figure/appendix read revenue_estimates.csv + receipts.csv
# from supplemental, never per-record detail.
#
# Usage (from repo root):
#   bash .../launch_form_laffer.sh levels        # baseline + 17 CF (levels)
#   bash .../launch_form_laffer.sh logs          # 17 CF (logs), reuse levels baseline
#-----------------------------------------------------------------------
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

MODE="${1:?usage: launch_form_laffer.sh <levels|logs>}"

case "$MODE" in
  levels)
    echo "form_laffer LEVELS: baseline + 17 CF -> vintage form_laffer_levels"
    KG_RESPONSE_FORM=levels \
      bash slurm_run.sh tests/form_laffer NULL jar335 1 form_laffer_levels 1 0 NULL 1
    echo "when this finishes (Phase 3b), launch the logs leg:"
    echo "  bash other/kg_model_tests/form_ab/launch_form_laffer.sh logs"
    ;;
  logs)
    # Own baseline (NULL): the levels legs ran delete_detail=1, which purges the
    # baseline static detail setup.R needs for baseline MTRs, so the levels
    # baseline cannot be reused. The baseline is form-invariant, so a fresh one
    # is identical.
    echo "form_laffer LOGS: baseline + 17 CF -> vintage form_laffer_logs"
    KG_RESPONSE_FORM=logs \
      bash slurm_run.sh tests/form_laffer NULL jar335 1 form_laffer_logs 1 0 NULL 1
    ;;
  *)
    echo "unknown mode '$MODE' (use levels|logs)"; exit 1 ;;
esac
