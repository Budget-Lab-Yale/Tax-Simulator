#!/bin/bash
#SBATCH --time=0:25:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=2
#SBATCH --partition=day
#SBATCH --job-name=count-ret
#SBATCH --output=logs/count_returns_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/count_taxable_returns.R
