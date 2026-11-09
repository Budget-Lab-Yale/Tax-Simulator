#!/bin/bash
#SBATCH --job-name=simplify_post
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/logs/post_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

# Syntax-parse every src/ R file before burning sim time
Rscript -e 'fs = list.files("src", pattern="[.]R$", recursive=TRUE, full.names=TRUE); for (f in fs) tryCatch(parse(f), error = function(e) { cat("PARSE FAIL:", f, conditionMessage(e), "\n"); quit(status = 1) }); cat("PARSE_OK\n")' || { echo "SMOKE_RUN_EXIT=PARSEFAIL"; exit 1; }

# main.R args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/simplify_smoke NULL user_test 1 bugfix_main 0.05 1 NULL 0 none
echo "SMOKE_RUN_EXIT=$?"
