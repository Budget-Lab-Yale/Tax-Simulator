#!/bin/bash
#SBATCH --job-name=range_check
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics/logs/range_check_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

echo "=== parse check ==="
Rscript -e 'for (f in c("src/sim/kg/constants.R","src/sim/kg/apply.R","src/sim/kg/inputs.R","src/sim/wealth_dynamics.R")) { invisible(parse(f)); cat("ok", f, "\n") }'

echo ""
echo "=== regenerate the wealth financing profiles and diff ==="
python3 other/wealth_dynamics/write_profiles.py
git status --short config/calibrations/wealth_profiles/ || true
echo "(an empty listing above is the pass)"
