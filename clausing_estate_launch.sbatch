#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-estate-launch
#SBATCH --output=logs/clausing_estate_launch_%j.log
# Clausing-Sarin stack rebuilt on default Tax-Data (2026060918) with the new
# on-model estate layer (07_estate, $5M@2030/45%) and corporate moved to 08.
# Full sample, vintage clausing_estate.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh clausing NULL jar335 1 clausing_estate 1 1 NULL 0
