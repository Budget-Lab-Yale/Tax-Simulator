#!/bin/bash
#SBATCH --job-name=kg_dyn_memo_decomp
#SBATCH --output=other/kg_model_tests/memo/decomp_and_panel_%j.out
#SBATCH --time=00:30:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=1

module load R 2>/dev/null || true

VINTAGE=${1:-kg_dyn_2pp_recal}

SCENARIOS=(rate_up_2pp carryover deemed rate_up_2pp_carryover rate_up_2pp_deemed delayed)

for scn in "${SCENARIOS[@]}"; do
  echo "============================================================"
  echo "  decomp: $scn"
  echo "============================================================"
  Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R "$VINTAGE" "$scn"
done

echo "============================================================"
echo "  panel"
echo "============================================================"
Rscript other/kg_model_tests/memo/build_decomp_panel.R "$VINTAGE"
