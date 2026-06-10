#!/bin/bash
#SBATCH --time=0:20:00
#SBATCH --mem=24G
#SBATCH --partition=day
#SBATCH --job-name=clausing-corp-sin-chart
#SBATCH --output=clausing_corp_sin_chart_%j.log
# Builds the 3-block stacked distribution chart + broken-out data file for the
# Clausing-Sarin package (individual and estate 01-07 on-model + 08-10
# off-model income measures, corporate 11, excise 12-16).
# Requires the clausing_estate model run to have completed (reads each
# scenario's static/supplemental/distribution.csv).
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/analysis/clausing_corp_sin_stacked_distribution.R
