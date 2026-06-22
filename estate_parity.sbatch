#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=2
#SBATCH --partition=day
#SBATCH --job-name=estate-parity
#SBATCH --output=logs/estate_parity_%j.log
# Port-parity check: on-model estate path vs canonical estate_module.R
# (other/estate_tax/test_onmodel_parity.R).
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript other/estate_tax/test_onmodel_parity.R
