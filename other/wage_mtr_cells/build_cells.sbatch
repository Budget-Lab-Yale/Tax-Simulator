#!/bin/bash
#SBATCH --job-name=wage_mtr_cells
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --mem=32G
#SBATCH --time=1:00:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wage_mtr_cells/logs/build_cells_%j.log

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/wage_mtr_cells/build_cells.R
