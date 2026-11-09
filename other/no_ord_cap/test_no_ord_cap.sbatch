#!/bin/bash
#SBATCH --job-name=no_ord_cap
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:15:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/no_ord_cap/test_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/no_ord_cap/test_no_ord_cap.R
