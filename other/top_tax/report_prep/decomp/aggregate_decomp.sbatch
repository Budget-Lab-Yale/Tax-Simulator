#!/bin/bash
#SBATCH --job-name=decomp_legs
#SBATCH --partition=day
#SBATCH --time=3:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/report_prep/decomp/decomp_%j.log

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript other/top_tax/report_prep/decomp/aggregate_decomp.R
