#!/bin/bash
#SBATCH --job-name=write_frozen
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/write_frozen_%j.out
#SBATCH --time=00:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/estate_tax/write_frozen_params.R
echo "=== write_frozen exit: $? ==="
