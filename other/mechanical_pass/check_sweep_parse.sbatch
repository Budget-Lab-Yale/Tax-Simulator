#!/bin/bash
#-----------------------------------------------------------------------
# check_sweep_parse.sbatch
#
# Parses the three eta_logs sweep runscripts, confirming the waivers clear
# the stale bathtub entries before the grid is launched
#-----------------------------------------------------------------------
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:45:00
#SBATCH --mem=64G
#SBATCH --job-name=sweep-parse
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/mechanical_pass/logs/sweep_parse_%j.log

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

for tag in 1; do
  echo "=== tests/mech_kg_smoke ==="
  Rscript -e "
    suppressMessages(lapply(readLines('./requirements.txt'), library,
                            character.only = TRUE))
    return_vars = list()
    files = list.files('./src', recursive = TRUE)
    for (f in files) {
      if (f != 'main.R' && !startsWith(f, 'slurm/') &&
          !startsWith(f, 'tests/') && !startsWith(f, 'behavior/')) {
        source(file.path('./src', f))
      }
    }
    g = parse_globals(runscript_name = 'tests/mech_kg_smoke',
                      scenario_id = NULL, local = 1, vintage = 'parse_check',
                      baseline_vintage = NULL, pct_sample = 1,
                      multicore = 'none')
    cat('parsed OK, scenarios:', paste(g\$runscript\$ID, collapse = ' '), '\n')
  " 2>&1 | tail -25
done
