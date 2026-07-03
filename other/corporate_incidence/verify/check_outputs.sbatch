#!/bin/bash
#SBATCH --job-name=corp_out_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:30:00
#SBATCH --output=other/corporate_incidence/logs/check_outputs_%j.out

set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"
module load R/4.4.1-foss-2022b
Rscript other/corporate_incidence/verify/check_outputs.R
