#!/bin/bash
#SBATCH --job-name=calcfix_c4
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=8:00:00
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/calc_fixes/logs/sim_c4_%j.out

cd /nfs/roberts/scratch/pi_nrs36/jar335/calcfix_wt_c4
module load R/4.4.1-foss-2022b
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/calcfix_tmp_c4
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/calc_fixes_baseline NULL user_test 1 calcfix_c4 1 1 NULL 0 none
echo "SIM_EXIT=$?"
