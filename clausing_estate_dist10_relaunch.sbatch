#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-dist10-relaunch
#SBATCH --output=logs/clausing_estate_dist10_relaunch_%j.log
# Re-runs ONLY setup (re-serializing configs with dist_years 2030:2039) and
# Phase 3b post-processing for the clausing_estate vintage. Detail and totals
# from the full run are untouched and reused; this just rebuilds the
# distribution tables (now 10 years), 1040/revenue/time-burden outputs.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

METADATA=$(Rscript src/slurm/setup.R clausing NULL jar335 1 clausing_estate 1 1 NULL 0)
eval "$METADATA"
echo "Staging: ${STAGING_DIR}; counterfactuals: ${N_SCENARIOS}"

P3B=$(sbatch --parsable --array=1-${N_SCENARIOS} \
  --partition=day -c 1 --time=3:00:00 --mem=32G \
  --job-name=taxsim-postproc10 \
  --output="${STAGING_DIR}/logs/p3b10_%A_%a.log" \
  --wrap="cd $(pwd) && module load R/4.4.1-foss-2022b && \
          Rscript src/slurm/aggregate.R ${STAGING_DIR} 3b")
echo "Phase 3b job: ${P3B}"
