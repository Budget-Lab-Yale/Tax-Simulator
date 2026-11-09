#!/bin/bash
#SBATCH --job-name=kg_dyn_panel_only
#SBATCH --output=other/kg_model_tests/memo/panel_only_%j.out
#SBATCH --time=00:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1

module load R 2>/dev/null || true
VINTAGE=${1:-kg_dyn_2pp_recal}
Rscript other/kg_model_tests/memo/build_decomp_panel.R "$VINTAGE"
