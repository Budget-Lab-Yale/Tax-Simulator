#!/bin/bash
#SBATCH --job-name=excl_active
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=23:00:00
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/excl_active_%j.out

# Active-exclusion smoke for the death-gain exclusion: baseline plus the $1M
# deemed and carryover solos over seven years at full sample, in the feature
# tree. Sanity checks (deemed revenue below the no-exclusion run, allocator
# identity) are read off the outputs by hand afterward.
set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript src/main.R tests/excl_active NULL 1 excl_active_smoke 1 0 NULL 0 none
echo "ACTIVE_SMOKE_EXIT=$?"
