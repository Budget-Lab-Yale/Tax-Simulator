#!/bin/bash
#SBATCH --job-name=b1_smoke
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=3:00:00
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/code_simplification/smoke_%x_%j.log

# Byte-identity smoke run for the simplification batches.
# Usage: sbatch --job-name=<tag> smoke_run.sbatch <tree_dir> <runscript> <vintage> <pct_sample> [scenario_id]
set -euo pipefail

TREE=$1
RUNSCRIPT=$2
VINTAGE=$3
PCT=$4
SCENARIO=${5:-NULL}

module load R/4.4.1-foss-2022b
cd "$TREE"

echo "tree=$TREE runscript=$RUNSCRIPT vintage=$VINTAGE pct=$PCT scenario=$SCENARIO"
git -C "$TREE" rev-parse --short HEAD

# main.R args: runscript scenario_id user_id local vintage pct_sample stacked
#              baseline_vintage delete_detail multicore
Rscript src/main.R "$RUNSCRIPT" "$SCENARIO" user_test 1 "$VINTAGE" "$PCT" 1 NULL 0 none
echo "SMOKE_RUN_EXIT=$?"
