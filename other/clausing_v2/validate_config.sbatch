#!/bin/bash
#SBATCH --job-name=clausing_v2_validate
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:30:00
#SBATCH --output=other/clausing_v2/logs/validate_config_%j.out

set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"
module load R/4.4.1-foss-2022b
Rscript other/clausing_v2/validate_config.R
