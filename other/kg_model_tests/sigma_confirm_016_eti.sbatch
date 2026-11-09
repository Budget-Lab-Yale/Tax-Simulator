#!/bin/bash
#SBATCH --job-name=sigma016_eti
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=1:00:00
#SBATCH --output=other/kg_model_tests/sigma_confirm_016_eti_%j.out
set -euo pipefail
module load R/4.4.1-foss-2022b
V1=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1
echo "=== CONFIRMATION leg: topord_plus5 at the SHIPPED SIGMA_CONV = 0.16 (target ETI 0.25) ==="
Rscript other/top_tax/archive/tests/compute_top_eti.R \
  "$V1/sigma_confirm_016" topord_plus5 "$V1/sigma_recal_estate" \
  "$V1/sigma_confirm_016/topord_plus5/conventional/supplemental/kg_dynamics_state" \
  2025 2035
