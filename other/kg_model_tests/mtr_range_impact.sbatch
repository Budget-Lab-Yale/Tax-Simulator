#!/bin/bash
#SBATCH --job-name=mtr_range
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=0:30:00
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/logs/mtr_range_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/mtr_range_impact.R
