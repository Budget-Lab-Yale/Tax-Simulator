#!/bin/bash
#SBATCH --job-name=kg_item1_cmp
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=0:30:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/compare_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/simplify_cleanup/kg_item1/compare.R
echo "COMPARE_EXIT=$?"
