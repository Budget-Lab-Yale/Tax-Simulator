#!/bin/bash
#SBATCH --job-name=estate_ab2_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/check_estate_offset_ab2_%j.out
set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/check_estate_offset_ab2.R
