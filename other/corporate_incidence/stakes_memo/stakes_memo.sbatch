#!/bin/bash
#-----------------------------------------------------------------------
# stakes_memo.sbatch
#
# SLURM wrapper for other/corporate_incidence/stakes_memo/stakes_memo.R
# (Phase 0a decision gate for the on-model corporate incidence channel).
#
# Submit from repo root:
#   sbatch other/corporate_incidence/stakes_memo/stakes_memo.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=corp_stakes
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=48G
#SBATCH --time=4:00:00
#SBATCH --output=other/corporate_incidence/logs/stakes_memo_%j.out

set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"
mkdir -p other/corporate_incidence/logs

module load R/4.4.1-foss-2022b
Rscript other/corporate_incidence/stakes_memo/stakes_memo.R
