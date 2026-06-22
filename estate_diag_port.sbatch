#!/bin/bash
#SBATCH --job-name=estate_diag_port
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=1
#SBATCH --output=logs/estate_diag_port_%j.log

module load R/4.4.1-foss-2022b

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax
Rscript estate_diagnostic.R /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2022.csv
