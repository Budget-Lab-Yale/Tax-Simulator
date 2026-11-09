#!/bin/bash
#-----------------------------------------------------------------------
# calibration_watch.sbatch  --  Tier A of the calibration staleness watch
#
# Runs the cheap internal-moment drift check (calibration_moment_check.R) at the
# SHIPPED eta against calibration_reference.csv and writes a one-line PASS/DRIFT
# report. Exits nonzero on drift so the failure is visible to whatever submitted
# it. Does NOT re-pin anything.
#
#   sbatch other/kg_model_tests/calibration_watch.sbatch [<baseline_root>]
#   sbatch other/kg_model_tests/calibration_watch.sbatch <baseline_root> --seed
#
# <baseline_root> must contain baseline/static/detail/{year}.csv with mtr_kg_lt
# AND mtr_estate_ded for >= 30 years. Defaults to the eta_dial re-pin central
# vintage's baseline. Pass --seed once, from a known-clean state, to establish
# the reference moment.
#-----------------------------------------------------------------------
#SBATCH --job-name=calib_watch_A
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=0:45:00
#SBATCH --output=other/kg_model_tests/calibration_watch_A_%j.out

set -uo pipefail
module load R/4.4.1-foss-2022b

BASELINE_ROOT="${1:-/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/eta_dial_c_v2}"
SEED_FLAG="${2:-}"

echo "=== Tier A: kg eta internal-moment drift check ==="
echo "baseline root: $BASELINE_ROOT"
Rscript other/kg_model_tests/calibration_moment_check.R "$BASELINE_ROOT" $SEED_FLAG
RC=$?

echo ""
if [ "$RC" -eq 0 ]; then
  echo "WATCH TIER A: PASS (rc=0)"
elif [ "$RC" -eq 3 ]; then
  echo "WATCH TIER A: DRIFT (rc=3) -- eta calibration stale, run the eta_dial re-pin"
else
  echo "WATCH TIER A: ERROR (rc=$RC) -- inspect the log above"
fi
exit $RC
