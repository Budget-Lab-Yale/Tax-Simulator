#!/bin/bash
#SBATCH --job-name=measure_timeable
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/form_ab/logs/measure_timeable_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/kg_model_tests/form_ab/logs
# Reads whole detail files at three years across three vintages.
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/form_ab/measure_timeable.R "${1:-logs}"
echo "MEASURE_EXIT=$?"
