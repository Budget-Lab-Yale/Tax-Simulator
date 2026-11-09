#!/bin/bash
#SBATCH --job-name=corp_dorm_post
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=3:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/corporate_incidence/logs/dorm_post_%j.out

# Dormancy regression, POST leg: worktree at 25a11e7de (corp channel built).
# Same runscript/sample/seed as the pre leg; outputs must be byte-identical.
set -euo pipefail
cd /nfs/roberts/scratch/pi_nrs36/jar335/corp_dorm_post
module load R/4.4.1-foss-2022b
Rscript src/main.R tests/simplify_smoke NULL user_test 1 corp_dorm_post 0.05 1 NULL 0 none
echo "SMOKE_RUN_EXIT=$?"
