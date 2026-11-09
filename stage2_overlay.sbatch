#!/bin/bash
#SBATCH --job-name=stage2_overlay
#SBATCH --partition=day
#SBATCH --time=00:20:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1
#SBATCH --output=logs/stage2_overlay_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
TAXSIM_ROOT=$(pwd) Rscript other/estate_tax/stage2_overlay.R
