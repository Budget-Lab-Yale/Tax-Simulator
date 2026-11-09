#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-v2-launch
#SBATCH --output=logs/clausing_v2_launch_%j.log
# CENTRAL Clausing-Sarin re-run with both mechanical channels active:
#   - wealth bathtub at flat s = 0.5 (identity M) on every reform layer
#   - on-model corporate incidence on 08_corporate (new OME vintage 20260706
#     carries corporate_meta.yaml; permanent shape, beyond_horizon: extend)
# Window is 2030:2040 -- SURPRISE framing (author ruling 2026-07-06): no
# 2029 lead-in, because kg_dynamics' calibrated announcement-year response
# would pull ~$81B of realizations into pre-window 2029 (run preserved at
# vintage clausing_v2_s50_anticipated). Static 2029 = 0, so nothing
# mechanical is lost; the +1 year past the window keeps the FY-lagged
# estate/wealth legs of 2039 that the original clausing run dropped.
# Corners (s = 0.25/0.75, corp sigma_N/kappa) are submitted by
# clausing_v2_corners.sh AFTER this run validates.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh clausing_v2 NULL user_test 1 clausing_v2_s50 1 1 NULL 0
