#!/bin/bash
#SBATCH --job-name=slurm_verify
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/code_simplification/verify_%x_%j.log

# Launches the SLURM pipeline for a full-sample verification run. Phase 0 runs
# here (on a compute node, not the login node); the rest of the chain is
# submitted as dependent array jobs and outlives this wrapper.
#
# Usage: sbatch --job-name=<tag> slurm_verify.sbatch <tree> <runscript> <vintage> <pct> <scenario_id>
set -euo pipefail

TREE=$1
RUNSCRIPT=$2
VINTAGE=$3
PCT=$4
SCENARIO=${5:-NULL}

cd "$TREE"
echo "tree=$TREE  head=$(git rev-parse --short HEAD)  runscript=$RUNSCRIPT vintage=$VINTAGE"

# slurm_run.sh args: runscript scenario_id user_id local vintage pct_sample
#                    stacked baseline_vintage delete_detail
bash slurm_run.sh "$RUNSCRIPT" "$SCENARIO" user_test 1 "$VINTAGE" "$PCT" 1 NULL 0
echo "LAUNCH_EXIT=$?"
