#!/bin/bash
#SBATCH --job-name=dina_etr
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/dina_etr_%j.out
#SBATCH --time=00:20:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/taxdata_interface_2026070814/dina_etr_check.R
echo "=== dina_etr exit: $? ==="
