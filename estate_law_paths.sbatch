#!/bin/bash
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --partition=day
#SBATCH --job-name=estate-law
#SBATCH --output=estate_law_paths_%j.log
# Parses the estate.yaml configs through the tax-law machinery and prints
# generated exemption/schedule paths (other/estate_tax/verify_estate_law_paths.R).
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript other/estate_tax/verify_estate_law_paths.R
