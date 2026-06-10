#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=scf-cmp
#SBATCH --output=scf_compare_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/scf_toptail_compare.R
