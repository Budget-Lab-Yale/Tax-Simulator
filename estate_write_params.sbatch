#!/bin/bash
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --partition=day
#SBATCH --job-name=estate-params
#SBATCH --output=logs/estate_write_params_%j.log
# Regenerates config/estate/estate_valuation_params.yaml from the calibration
# outputs and the SOI table (see other/estate_tax/write_frozen_params.R).
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript other/estate_tax/write_frozen_params.R
