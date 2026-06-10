#!/bin/bash
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --partition=day
#SBATCH --job-name=clausing-dist-chart
#SBATCH --output=clausing_dist_chart_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/analysis/clausing_hybrid_distribution.R
