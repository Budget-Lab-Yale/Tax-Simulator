#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=24G
#SBATCH --partition=day
#SBATCH --job-name=diag2022
#SBATCH --output=diag2022_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/diag_2022_count.R
