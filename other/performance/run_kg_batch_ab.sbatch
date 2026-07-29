#!/bin/bash
#-----------------------------------------------------------------------
# run_kg_batch_ab.sbatch
#
# Submits one mech_kg_smoke run at a given years_per_task, from a compute
# node. slurm_run.sh's Phase 0 parses globals and reads Tax-Data in process,
# which does not belong on a login node.
#
# Usage:
#   sbatch other/performance/run_kg_batch_ab.sbatch <vintage> <years_per_task>
#-----------------------------------------------------------------------
#SBATCH --job-name=kg-batch-ab
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/performance/logs/kg_batch_ab_%j.log

set -euo pipefail

VINTAGE="$1"
K="$2"

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)  vintage=${VINTAGE}  years_per_task=${K}"

bash slurm_run.sh tests/mech_kg_smoke NULL 1 "${VINTAGE}" 1 1 NULL 0 batch "${K}"
echo "LAUNCH_EXIT=$?"
