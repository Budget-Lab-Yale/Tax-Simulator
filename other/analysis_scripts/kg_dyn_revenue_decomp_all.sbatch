#!/bin/bash
#SBATCH --job-name=kg_dyn_decomp_all
#SBATCH --output=other/analysis_scripts/kg_dyn_revenue_decomp_all_%j.out
#SBATCH --time=02:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=1

module load R 2>/dev/null || true

VINTAGE=${1:-kg_dyn_scf_check}

SCENARIOS=(baseline_check carryover deemed delayed rate_down_5pp rate_up_5pp rate_up_carryover rate_up_deemed temp)

for scn in "${SCENARIOS[@]}"; do
  echo "============================================================"
  echo "  $scn"
  echo "============================================================"
  Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R "$VINTAGE" "$scn"
done
