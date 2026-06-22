#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=kg-deemed
#SBATCH --output=logs/kg_deemed_%j.log
# Phase 0 setup + array-chain submission for the value-based deemed-realization
# avoidance sweep. KG_DEEMED_AVOIDANCE and VINTAGE come from --export on submit.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
echo "KG_DEEMED_AVOIDANCE=$KG_DEEMED_AVOIDANCE  VINTAGE=$VINTAGE  BASELINE_VINTAGE=$BASELINE_VINTAGE"
bash slurm_run.sh tests/kg_dyn_value_avoid NULL user_test 1 "$VINTAGE" 1 1 "$BASELINE_VINTAGE" 0
