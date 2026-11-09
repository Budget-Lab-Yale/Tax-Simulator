#!/bin/bash
#SBATCH --job-name=kg_item1_pre
#SBATCH --partition=day
#SBATCH -c 8
#SBATCH --time=10:00:00
#SBATCH --mem=128G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/pre_%j.out

# PRE = reference run from the detached worktree at HEAD a142825e8 (ORIGINAL,
# unedited kg_dynamics.R). Full sample, kg_dynamics/turnover scenarios.
cd /nfs/roberts/scratch/pi_nrs36/jar335/kg_item1_pre_wt
module load R/4.4.1-foss-2022b

# Pin tempdir to scratch (node /tmp proved flaky under load); doesn't affect output.
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/kg_item1_tmp_pre
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
# multicore=none: sequential, avoids the parallel-fork memory segfault in calc_time_burden at full sample.
Rscript src/main.R tests/kg_item1_regression NULL user_test 1 kg_sr_pre 1 1 NULL 0 none
echo "KG_PRE_EXIT=$?"
