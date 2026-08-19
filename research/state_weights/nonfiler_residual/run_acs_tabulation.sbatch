#!/bin/bash
#SBATCH --job-name=nonfiler_acs
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# Stage D --acs tabulation (research/state_weights/nonfiler_residual_design.md §4.3). The IPUMS
# extract read OOMs the login node; everything else in Stage D is login-safe.
# Submit from the repo root:
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/nonfiler_residual/run_acs_tabulation.sbatch [year]
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"

module load R/4.4.2-gfbf-2024a
Rscript research/state_weights/nonfiler_residual/03_diagnose_current_nonfilers.R --acs "${1:-2022}"

echo "ACS_TABULATION_SUCCESS"
