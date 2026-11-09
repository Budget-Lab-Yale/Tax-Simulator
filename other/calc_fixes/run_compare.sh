#!/bin/bash
#SBATCH --job-name=calcfix_cmp
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/calc_fixes/logs/compare_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/calc_fixes/compare.R
echo "COMPARE_EXIT=$?"
