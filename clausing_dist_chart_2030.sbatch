#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --partition=day
#SBATCH --job-name=clausing-dist-2030
#SBATCH --output=logs/clausing_dist_chart_2030_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
export CLAUSING_VINTAGE=clausing_2039_surprise
export CLAUSING_YEAR=2030
export CLAUSING_SCENARIO=06_niit_reform
Rscript other/analysis/clausing_hybrid_distribution.R
