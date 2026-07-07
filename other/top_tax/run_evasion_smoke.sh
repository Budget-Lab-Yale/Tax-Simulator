#!/bin/bash
#SBATCH --job-name=evasion_smoke
#SBATCH --partition=day
#SBATCH -c 8
#SBATCH --time=4:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/evasion_smoke_%j.out

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R tests/evasion_smoke NULL user_test 1 evasion_smoke 0.1 0 NULL 0 none
echo "SIM_EXIT=$?"
