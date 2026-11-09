#!/bin/bash
#SBATCH --job-name=calcfix_c5xx
#SBATCH --partition=day
#SBATCH -c 8
#SBATCH --time=12:00:00
#SBATCH --mem=128G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/calc_fixes/logs/sim_c5x_%j.out

cd /nfs/roberts/scratch/pi_nrs36/jar335/calcfix_wt_c5
module load R/4.4.1-foss-2022b
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/calcfix_tmp_c5
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/calc_fixes_excdctc NULL user_test 1 calcfix_c5x 1 1 NULL 0 none
echo "SIM_EXIT=$?"
