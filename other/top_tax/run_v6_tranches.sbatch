#!/bin/bash
#-----------------------------------------------------------------------
# run_v6_tranches.sbatch
#
# Runs the top-tax dials grid on the three-rung model, one tranche at a
# time, into a single vintage.
#
# The grid is 225 counterfactuals over 31 years, and under the mechanical
# rung each carries five detail trees rather than three -- about 25G a
# scenario, against 1.8T free on scratch. Detail is purged at the end of
# each tranche, so the tranches run in sequence and peak disk is one
# tranche's worth. Every tranche writes to the same vintage, the way the v5
# batch spanned two runscripts.
#
# Usage:
#   sbatch other/top_tax/run_v6_tranches.sbatch [vintage] [years_per_task]
#-----------------------------------------------------------------------
#SBATCH --job-name=v6-tranches
#SBATCH --partition=week
#SBATCH -c 2
#SBATCH --time=3-00:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/v6_tranches_%j.log

set -euo pipefail

VINTAGE="${1:-top_tax_dials_30y_v6}"
K="${2:-1}"

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/top_tax/logs

echo "=== top-tax v6 grid ==="
echo "head=$(git rev-parse --short HEAD)"
echo "vintage=${VINTAGE}  years_per_task=${K}"
echo ""

for t in 1 2 3 4 5 6; do
  RUNSCRIPT="top_tax/v6/tranche${t}"

  echo "--- tranche ${t}: submitting ${RUNSCRIPT} at $(date) ---"
  bash slurm_run.sh "${RUNSCRIPT}" NULL 1 "${VINTAGE}" 1 0 NULL 1 batch "${K}"

  # Wait for the tranche to drain before starting the next one. Detail is
  # purged in the tranche's own Phase 4, which is what bounds peak disk, so
  # the next tranche must not start until that has run.
  sleep 60
  while squeue -u "$USER" -h -o '%j' | grep -q '^taxsim'; do
    sleep 120
  done

  echo "--- tranche ${t} drained at $(date) ---"
  df -h /nfs/roberts/scratch | tail -1
  echo ""
done

echo "=== all tranches complete at $(date) ==="
