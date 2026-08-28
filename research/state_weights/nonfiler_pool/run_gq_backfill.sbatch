#!/bin/bash
#SBATCH --job-name=nonfiler_gq
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# Group C stage C5: the ACS group-quarters backfill (03_gq_backfill.R). The
# ACS extract read OOMs the ~5G login-node cap; 48G follows the measured
# pattern of run_acs_tabulation.sbatch (MaxRSS 3.8G with a transient read
# spike). Submit from the repo root, one tax year per job or both:
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/nonfiler_pool/run_gq_backfill.sbatch [tax_year]
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"

module load R/4.4.2-gfbf-2024a
Rscript research/state_weights/nonfiler_pool/03_gq_backfill.R ${1:-2017 2022}

echo "GQ_BACKFILL_SUCCESS"
