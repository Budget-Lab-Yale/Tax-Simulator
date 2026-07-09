#!/bin/bash
#SBATCH --job-name=kg_item1_cmp2
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=0:30:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/compare2_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/simplify_cleanup/kg_item1/compare.R kg_sr_pre2 kg_sr_post2
echo "COMPARE2_EXIT=$?"
