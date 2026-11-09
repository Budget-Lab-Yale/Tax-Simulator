#!/bin/bash
#SBATCH --job-name=dina_vs_td
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/dina_vs_td_%j.out
#SBATCH --time=00:15:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/taxdata_interface_2026070814/dina_vs_taxdata.R
echo "=== exit: $? ==="
