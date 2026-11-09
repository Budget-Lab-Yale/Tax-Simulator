#!/bin/bash
#SBATCH --job-name=dbg-2m-direct
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/dbg_2m_direct_%j.log
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
S=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v6/_slurm_staging
Rscript other/top_tax/debug_2m_direct.R "$S" "$1" "$2"
