#!/bin/bash
#-----------------------------------------------------------------------
# run_v6_all.sbatch
#
# Runs the whole top-tax dials grid on the three-rung model in one
# submission, 225 counterfactuals over 31 years.
#
# Phase 0 parses globals and reads Tax-Data in process, which is why the
# grid is launched from a compute node rather than the login node. The
# dependent array jobs outlive this wrapper.
#
# Usage:
#   sbatch other/top_tax/run_v6_all.sbatch [vintage] [years_per_task]
#-----------------------------------------------------------------------
#SBATCH --job-name=v6-all
#SBATCH --partition=week
#SBATCH -c 2
#SBATCH --time=1-00:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/v6_all_%j.log

set -euo pipefail

VINTAGE="${1:-top_tax_dials_30y_v6}"
K="${2:-2}"

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/top_tax/logs

echo "=== top-tax v6 grid, one submission ==="
echo "head=$(git rev-parse --short HEAD)"
echo "vintage=${VINTAGE}  years_per_task=${K}"
df -h /nfs/roberts/scratch | tail -1
echo ""

bash slurm_run.sh top_tax/v6/all NULL 1 "${VINTAGE}" 1 0 NULL 1 batch "${K}"

echo ""
echo "submitted at $(date). Read per-phase states, never the queue:"
echo "  sacct -u \$USER -X -n --format=JobName%20,State | grep taxsim | sort | uniq -c"
