#!/bin/bash
#SBATCH --job-name=corp_dorm_pre
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=3:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/corporate_incidence/logs/dorm_pre_%j.out

# Dormancy regression, PRE leg: worktree at 3bc1ba9dd (pre-corp-channel).
# Runs the simplify_smoke runscript (baseline + 2 non-corp reforms, 5% sample)
# so the post leg can be byte-compared (compare_smoke.sh precedent).
set -euo pipefail
cd /nfs/roberts/scratch/pi_nrs36/jar335/corp_dorm_pre
module load R/4.4.1-foss-2022b
Rscript src/main.R tests/simplify_smoke NULL user_test 1 corp_dorm_pre 0.05 1 NULL 0 none
echo "SMOKE_RUN_EXIT=$?"
