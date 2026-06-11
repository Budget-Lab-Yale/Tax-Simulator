#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-ded-check
#SBATCH --output=estate_ded_pilot_check_%j.log
# Acceptance checks for the niskanen_estate_ded_pilot vintage (commit 2:
# deemed tax deductible against the taxable estate). See
# other/estate_tax/check_ded_pilot.R for the check list.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/check_ded_pilot.R
