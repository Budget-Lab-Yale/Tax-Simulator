#!/bin/bash
#-----------------------------------------------------------------------
# check_core.sbatch — corp_incidence.R pure-core verification.
# Submit from repo root:
#   sbatch other/corporate_incidence/verify/check_core.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=corp_core_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:20:00
#SBATCH --output=other/corporate_incidence/logs/check_core_%j.out

set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"
mkdir -p other/corporate_incidence/logs

module load R/4.4.1-foss-2022b
Rscript other/corporate_incidence/verify/check_core.R
