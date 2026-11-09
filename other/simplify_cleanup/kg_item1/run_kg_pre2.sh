#!/bin/bash
#SBATCH --job-name=kg_item1_pre2
#SBATCH --partition=day
#SBATCH -c 8
#SBATCH --time=10:00:00
#SBATCH --mem=128G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/pre2_%j.out

# PRE v2 = reference run from the worktree at HEAD (ORIGINAL kg code), years
# 2026:2029 so the Estate-Tax-Distribution heir data exists (fully-green run).
cd /nfs/roberts/scratch/pi_nrs36/jar335/kg_item1_pre_wt
module load R/4.4.1-foss-2022b
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/kg_item1_tmp_pre2
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/kg_item1_regression_v2 NULL user_test 1 kg_sr_pre2 1 1 NULL 0 none
echo "KG_PRE2_EXIT=$?"
