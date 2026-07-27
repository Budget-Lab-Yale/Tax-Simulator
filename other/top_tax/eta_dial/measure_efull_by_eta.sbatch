#!/bin/bash
#SBATCH --job-name=efull_by_eta
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=1:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/eta_dial/logs/efull_%j.log

# Measures E_full across the levels eta grid, inverts for eta, and writes the
# value into config/calibrations/kg/bathtub.yaml. Reads whole detail files, so it
# does not belong on a login node.
#
#   sbatch other/top_tax/eta_dial/measure_efull_by_eta.sbatch
#
# Honours KG_CALIB_OUTPUT_ROOT and VINTAGE_SUFFIX, both passed through from the
# submitting environment via --export (the default), for a spot check against
# suffixed vintages.
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/top_tax/eta_dial/logs
Rscript other/top_tax/eta_dial/measure_efull_by_eta.R
echo "MEASURE_EXIT=$?"
