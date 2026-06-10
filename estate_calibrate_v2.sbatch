#!/bin/bash
#SBATCH --job-name=estate_cal_v2
#SBATCH --partition=day
#SBATCH --time=03:00:00
#SBATCH --mem=24G
#SBATCH --cpus-per-task=1
#SBATCH --output=estate_cal_v2_%j.log

module load R/4.4.1-foss-2022b

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax
Rscript calibrate_estate_v2.R
