#!/bin/bash
#SBATCH --job-name=relcurve
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=2:00:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/relcurve_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/top_tax/report_prep/realization_curve_by_regime.R
