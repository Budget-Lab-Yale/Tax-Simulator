#!/bin/bash
#SBATCH --job-name=eta_estate_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=48G
#SBATCH --time=4:00:00
#SBATCH --output=other/kg_model_tests/eta_estate_check_%j.out
set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/eta_estate_check.R \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/eta_estate_check_v1
