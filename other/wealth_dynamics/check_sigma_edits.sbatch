#!/bin/bash
#SBATCH --job-name=sig_check
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics/logs/sig_check_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript -e 'for (f in c("src/sim/sigma_conversion.R","src/sim/run.R","src/slurm/bathtub.R","src/behavior/conversion/sigma.R")) { invisible(parse(f)); cat("ok", f, "\n") }'
echo ""
echo "=== leftover references to the retired pieces ==="
grep -rn "sigma_module_recompute\|SIGMA_CONSERVE" src/ other/ || echo "none"
