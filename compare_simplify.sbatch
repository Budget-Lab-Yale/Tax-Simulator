#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --job-name=kg-regression-diff
#SBATCH --dependency=afterok:11692960
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/compare_simplify.R
