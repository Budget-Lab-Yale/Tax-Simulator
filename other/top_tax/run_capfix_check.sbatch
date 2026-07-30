#!/bin/bash
#-----------------------------------------------------------------------
# run_capfix_check.sbatch
#
# Runs the two dials the gains bathtub stopped on, to confirm they clear
# the marginal cost cap now that out-of-range rates leave the cell
# averages.
#
# Usage:
#   sbatch other/top_tax/run_capfix_check.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=capfix
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=6:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/capfix_%j.log

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)"
bash slurm_run.sh tests/mtr_range_capfix NULL 1 mtr_range_capfix 1 0 NULL 1 batch 2
