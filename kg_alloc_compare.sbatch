#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=kg-alloc
#SBATCH --output=logs/kg_alloc_%j.log
# Phase 0 setup + array-chain submission for the applier-allocation comparison
# (kg_lt-share 'R' vs G_unit-share 'G' in kg_dyn_apply_to_records).
# KG_APPLIER_ALLOCATION and VINTAGE come from --export on submit:
#   sbatch --export=ALL,KG_APPLIER_ALLOCATION=R,VINTAGE=kg_alloc_R,BASELINE_VINTAGE=sec121_carryover kg_alloc_compare.sbatch
#   sbatch --export=ALL,KG_APPLIER_ALLOCATION=G,VINTAGE=kg_alloc_G,BASELINE_VINTAGE=sec121_carryover kg_alloc_compare.sbatch
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
echo "KG_APPLIER_ALLOCATION=$KG_APPLIER_ALLOCATION  VINTAGE=$VINTAGE  BASELINE_VINTAGE=$BASELINE_VINTAGE"
bash slurm_run.sh tests/kg_alloc_compare NULL user_test 1 "$VINTAGE" 1 1 "$BASELINE_VINTAGE" 0
