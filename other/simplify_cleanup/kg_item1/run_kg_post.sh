#!/bin/bash
#SBATCH --job-name=kg_item1_post
#SBATCH --partition=day
#SBATCH -c 8
#SBATCH --time=10:00:00
#SBATCH --mem=128G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/simplify_cleanup/kg_item1/logs/post_%j.out

# POST = run from the main working tree with the EDITED kg_dynamics.R.
# Identical args/runscript as PRE; only the code differs. Vintage kg_sr_post.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

# Pin tempdir to scratch (node /tmp proved flaky under load); doesn't affect output.
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/kg_item1_tmp_post
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
# multicore=none: sequential, avoids the parallel-fork memory segfault in calc_time_burden at full sample.
Rscript src/main.R tests/kg_item1_regression NULL user_test 1 kg_sr_post 1 1 NULL 0 none
echo "KG_POST_EXIT=$?"
