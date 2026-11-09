#!/bin/bash
#SBATCH --job-name=estate_calib
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/estate_calib_%j.out
#SBATCH --time=01:30:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=1

# Re-calibrate estate valuation params (r, rho_pt) on Tax-Data 2026070814.
# Wealth/balance-sheet columns are unchanged from 2026060918, so we expect the
# fit to reproduce r=0.951, rho_pt=0.612 exactly (this run confirms it).
# The script only PRINTS the fit; transcription into the working yaml +
# write_frozen_params.R follows.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/estate_tax/calibrate_estate_v2.R
echo "=== calibrate_estate_v2.R exit: $? ==="
