#!/bin/bash
#SBATCH --job-name=kg_item1_preflight
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=0:25:00
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/preflight_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

# 1) Parse every src/ R file (catches syntax errors before any sim time)
Rscript -e 'fs=list.files("src",pattern="[.]R$",recursive=TRUE,full.names=TRUE); for(f in fs) tryCatch(parse(f),error=function(e){cat("PARSE FAIL:",f,conditionMessage(e),"\n");quit(status=1)}); cat("PARSE_OK\n")' \
  || { echo "PREFLIGHT_EXIT=PARSEFAIL"; exit 1; }

# 2) Behavior-preservation identities for the three edits
Rscript other/simplify_cleanup/kg_item1/preflight_identity.R
echo "PREFLIGHT_EXIT=$?"
