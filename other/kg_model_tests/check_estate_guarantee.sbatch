#!/bin/bash
#SBATCH --job-name=estate_ab3_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:15:00
#SBATCH --output=other/kg_model_tests/check_estate_guarantee_%j.out
set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/check_estate_guarantee.R
