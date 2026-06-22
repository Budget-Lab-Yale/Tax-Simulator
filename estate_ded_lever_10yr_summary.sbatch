#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --partition=day
#SBATCH --job-name=estate-lever-10yr-sum
#SBATCH --output=logs/estate_ded_lever_10yr_summary_%j.log
# Ten-year budget summary for the estate.income_tax_ded lever comparison.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/ded_lever_10yr_summary.R
