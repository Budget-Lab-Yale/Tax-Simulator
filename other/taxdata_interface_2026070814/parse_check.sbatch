#!/bin/bash
#SBATCH --job-name=parse_check
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/parse_check_%j.out
#SBATCH --time=00:05:00
#SBATCH --mem=2G
#SBATCH --cpus-per-task=1

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
Rscript -e '
files <- c("src/data/post_processing/corp_alloc.R",
           "src/data/post_processing/distribution_etrs.R")
ok <- TRUE
for (f in files) {
  res <- tryCatch({ parse(file = f); cat(sprintf("PARSE OK: %s\n", f)); TRUE },
                  error = function(e) { cat(sprintf("PARSE ERROR in %s:\n  %s\n", f, conditionMessage(e))); FALSE })
  ok <- ok && res
}
if (ok) cat("ALL FILES PARSE CLEAN\n") else quit(status = 1)
'
