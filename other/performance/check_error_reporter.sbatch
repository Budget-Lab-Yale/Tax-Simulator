#!/bin/bash
#SBATCH --job-name=err_chk
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/performance/logs/err_chk_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
echo "=== parse every driver ==="
Rscript -e 'for (f in list.files("src/slurm", full.names = TRUE, pattern = "[.]R$")) { invisible(parse(f)); cat("ok", f, "\n") }'
echo ""
echo "=== the reporter, on a real error, with a sidecar ==="
TMP=$(mktemp -d)
set +e
SLURM_ARRAY_TASK_ID=7 Rscript -e "
  source('src/slurm/common.R')
  tryCatch(stop('a deliberate test failure'),
           error = function(e) report_driver_error(e, 'test phase 9', '$TMP'))
"
echo "RSCRIPT_EXIT=$?"
set -e
echo ""
echo "=== sidecar written ==="
find "$TMP" -type f | while read -r f; do echo "-- $f"; cat "$f"; done
rm -rf "$TMP"
