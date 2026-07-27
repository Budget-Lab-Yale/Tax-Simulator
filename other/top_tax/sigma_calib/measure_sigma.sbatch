#!/bin/bash
#SBATCH --job-name=measure_sigma
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/sigma_calib/logs/sigma_%j.log

# Measures the top-subset ETI across the sigma grid, interpolates onto the 0.25
# target, and writes the value into config/calibrations/kg/conversion.yaml. Reads
# whole detail files, so not a login-node job.
#
#   sbatch other/top_tax/sigma_calib/measure_sigma.sbatch [charity]
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/top_tax/sigma_calib/logs
Rscript other/top_tax/sigma_calib/measure_sigma.R "${1:-100}"
echo "MEASURE_EXIT=$?"
