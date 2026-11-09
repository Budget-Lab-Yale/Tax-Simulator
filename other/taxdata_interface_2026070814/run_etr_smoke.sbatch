#!/bin/bash
#SBATCH --job-name=etr_other_smoke
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/etr_smoke_%j.out
#SBATCH --time=02:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=2

# Task-2 verification: run baseline + a baseline-law "reform" (etr_check) pinned
# to the NEW Tax-Data vintage 2026070814, full sample, static, dist_years=2026.
# Exercises distribution_etrs.R end-to-end and produces the new standalone
# `other` (state+local+excise) ETR tier. Runs entirely on a compute node.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/etr_other_smoke NULL user_test 1 taxdata_etr_verify 1 0 NULL 0 none
echo "=== main.R exit code: $? ==="
