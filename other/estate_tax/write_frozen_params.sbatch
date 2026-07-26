#!/bin/bash
#SBATCH --job-name=estate_bridge
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/config_redesign/logs/estate_bridge_%j.log

# Regenerates config/calibrations/estate/bridge.yaml -- the estate measurement
# parameters the model reads -- by merging the fitted valuation parameters with
# the chosen inputs and the SOI per-bin table. Run this after re-running
# calibrate_estate_v2.R, and commit the result.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/write_frozen_params.R
