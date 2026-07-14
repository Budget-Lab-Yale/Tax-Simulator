#!/bin/bash
#SBATCH --job-name=obbba_smoke
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/obbba/retrospective_2026/logs/smoke_%j.out

# Config-plumbing smoke for the OBBBA retrospective stack:
#   - validates the pre-OBBBA counterfactual (baseline_2024_pre_obbba) runs on this branch
#   - validates the reversion overlays parse
#   - full_check (tax_law=baseline) must reproduce 08-estate exactly (endpoint check)
# Wealth dynamics OFF (wealth_financing=none) so pct_sample<1 is permitted and it's fast.

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
export TMPDIR=/nfs/roberts/scratch/pi_nrs36/jar335/obbba_smoke_tmp
mkdir -p "$TMPDIR"

# args: runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail multicore
Rscript src/main.R public/obbba/retrospective_2026/obbba_stack_smoke NULL user_test 1 obbba_smoke 0.1 1 NULL 0 none
echo "SIM_EXIT=$?"
