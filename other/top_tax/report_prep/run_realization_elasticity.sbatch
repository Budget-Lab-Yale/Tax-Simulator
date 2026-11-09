#!/bin/bash
#SBATCH --job-name=relast
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=1:00:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/relast_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/top_tax/report_prep/realization_elasticity_by_regime.R
