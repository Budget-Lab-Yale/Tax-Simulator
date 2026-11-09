#!/bin/bash
#SBATCH --job-name=mtr_ext_diff
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/tips_ot_index_fix/ab_diff.log
#SBATCH --time=00:30:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/tips_ot_index_fix/ab_diff.R
