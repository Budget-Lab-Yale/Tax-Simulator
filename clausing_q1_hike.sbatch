#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-q1-hike
#SBATCH --output=logs/clausing_q1_hike_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/analysis/clausing_q1_hike_profile.R
