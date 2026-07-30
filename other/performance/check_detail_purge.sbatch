#!/bin/bash
#SBATCH --job-name=purge_chk
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/performance/logs/purge_chk_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

echo "=== parse ==="
Rscript -e 'for (f in c("src/misc/utils.R","src/misc/cli.R","src/main.R","src/sim/run.R","src/sim/wealth_dynamics.R","src/slurm/setup.R","src/slurm/wealth.R","src/slurm/mech_wealth.R","src/slurm/aggregate.R")) { invisible(parse(f)); cat("ok", f, "\n") }'

echo ""
echo "=== resolver behaviour ==="
Rscript -e '
suppressPackageStartupMessages(library(tidyverse))
source("src/misc/utils.R")
for (v in list(0, 1, "none", "all", "transient", "transient,all", "all,transient")) {
  r = resolve_detail_purge(v)
  cat(sprintf("%-16s -> eager=%-5s final=%s\n", paste(v), r$eager, r$final))
}
for (v in c("some", "", "none,all")) {
  out = tryCatch({ resolve_detail_purge(v); "NO ERROR (wrong)" },
                 error = function(e) paste("rejected:", substr(conditionMessage(e), 1, 60)))
  cat(sprintf("%-16s -> %s\n", v, out))
}'
