#!/bin/bash
#SBATCH --job-name=widowhood_shares
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --mem=16G
#SBATCH --time=0:20:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax/output/widowhood_shares_%j.log

# Regenerates config/calibrations/estate/widowhood.yaml -- the widowhood shares
# the death-gain exclusion reads -- from the raw SCF. Run after changing the
# chosen inputs in config/calibrations/estate/settings.yaml, and commit the
# result.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/write_widowhood_shares.R
