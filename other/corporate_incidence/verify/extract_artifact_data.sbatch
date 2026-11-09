#!/bin/bash
#SBATCH --job-name=corp_extract
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:15:00
#SBATCH --output=other/corporate_incidence/logs/extract_%j.out
set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"
module load R/4.4.1-foss-2022b
Rscript other/corporate_incidence/verify/extract_artifact_data.R
