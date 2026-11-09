#!/bin/bash
#SBATCH --job-name=kg_dyn_decomp
#SBATCH --output=other/analysis_scripts/kg_dyn_revenue_decomp_%j.out
#SBATCH --time=00:10:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1

module load R 2>/dev/null || true

VINTAGE=${1:-kg_dyn_scf_check}
SCENARIO=${2:-rate_up_5pp}

Rscript other/analysis_scripts/kg_dyn_revenue_decomp.R "$VINTAGE" "$SCENARIO"
