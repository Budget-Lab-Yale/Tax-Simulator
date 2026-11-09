#!/bin/bash
#SBATCH --job-name=td_ab_diff
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/ab_diff_%j.out
#SBATCH --time=02:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=2

# Task-1 A/B: baseline (old vintage 2026060918) vs td_new (baseline law, new
# vintage 2026070814), full sample, static, no dist_years (totals + static
# detail only; avoids the ETR reader which would hit the old-vintage baseline).
# The td_new static delta vs baseline = pure Tax-Data data effect.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

Rscript src/main.R tests/taxdata_2026070814_diff NULL user_test 1 taxdata_ab_diff 1 0 NULL 0 none
echo "=== main.R exit code: $? ==="
