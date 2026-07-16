#!/bin/bash
#-----------------------------------------------------------------------
# test_hidden_ledger_guards.sbatch
#
# SLURM wrapper for other/top_tax/tests/test_hidden_ledger_guards.R:
# hidden-ledger concealment math, positive-leg flow scaling + SECA
# companions, the R6 kg_lt overlay, the R4 estate_concealed_frac column,
# the R3 evasion->wealth link, the conservation identity, CHI env parsing,
# the CHI=0 no-op property, the do_wealth / do_estate order + missing-MTR
# guards and the wealth->estate activation contract; then chains the
# estate-module runscript lint (lint_estate_module.R).
#
# Submit from repo root:
#   sbatch other/top_tax/tests/test_hidden_ledger_guards.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=hl_guards
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:15:00
#SBATCH --output=other/top_tax/tests/test_hidden_ledger_guards_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/top_tax/tests/test_hidden_ledger_guards.R
Rscript other/top_tax/tests/lint_estate_module.R
