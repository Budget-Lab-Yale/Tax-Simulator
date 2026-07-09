#!/bin/bash
#SBATCH --job-name=simplify_pre
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/logs/pre_%j.out

# Pre-edit reference run from a detached worktree at HEAD (087a51623)
cd /nfs/roberts/scratch/pi_nrs36/jar335/simplify_pre_wt
module load R/4.4.1-foss-2022b

# main.R args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/simplify_smoke NULL user_test 1 simplify_pre4 0.05 1 NULL 0 none
echo "SMOKE_RUN_EXIT=$?"
