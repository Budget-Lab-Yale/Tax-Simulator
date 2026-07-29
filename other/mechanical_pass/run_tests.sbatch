#!/bin/bash
#-----------------------------------------------------------------------
# run_tests.sbatch
#
# Parses every file under src/ and runs the employer payroll predicate
# checks. No model work; runs in seconds.
#-----------------------------------------------------------------------
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:15:00
#SBATCH --mem=8G
#SBATCH --job-name=mech-tests
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/mechanical_pass/logs/tests_%j.log

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

echo "START"

Rscript -e '
files = list.files("src", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
bad = 0
for (f in files) {
  msg = tryCatch({ parse(f); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(msg)) { bad = bad + 1; cat("PARSE FAIL", f, ":", msg, "\n") }
}
cat("parsed", length(files), "files;", bad, "failed\n")
if (bad > 0) quit(status = 1)
' 2>&1
echo "PARSE_EXIT=$?"

Rscript -e '
suppressMessages({
  library(tidyverse); library(magrittr); library(yaml); library(data.table)
})
TAX_LAW_ROOT = file.path("config", "scenarios", "tax_law")
source("src/data/tax_law.R")
source("src/sim/payroll.R")
source("other/mechanical_pass/test_er_payroll_predicate.R")
' 2>&1
echo "TESTS_EXIT=$?"

bash other/performance/test_slurm_dependency_graph.sh > /dev/null 2>&1
echo "SLURM_GRAPH_EXIT=$?"
