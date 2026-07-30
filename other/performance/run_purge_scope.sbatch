#!/bin/bash
#-----------------------------------------------------------------------
# run_purge_scope.sbatch
#
# Four wealth-active scenarios through the transient purge, so that one
# scenario's wealth recurrence can delete another's measurement detail if
# the purge is not scoped to its own scenario. The earlier smoke had one
# wealth scenario and could not fail this way.
#
# Usage:
#   sbatch other/performance/run_purge_scope.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=purge_scope
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=3:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/performance/logs/purge_scope_%j.log

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)"
bash slurm_run.sh tests/purge_scope_smoke NULL 1 purge_scope 1 0 NULL transient batch 1
