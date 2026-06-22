#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-lever-check
#SBATCH --output=logs/estate_ded_lever_check_%j.log
# Acceptance + diff report for the estate.income_tax_ded lever comparison
# (estate_ded_lever vintage). See other/estate_tax/check_ded_lever.R.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/check_ded_lever.R
