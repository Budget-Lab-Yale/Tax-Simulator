#!/bin/bash
#SBATCH --job-name=mtr_ext_post
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/tips_ot_index_fix/ab_post.log
#SBATCH --time=03:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=1
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript src/main.R tests/mtr_extensive_tips_ot NULL user_test 1 mtr_ext_post 0.05 0 NULL 0 none
