#!/bin/bash
#-----------------------------------------------------------------------
# wealth_carry_tests.sbatch
#
# T1/T2/T3 for the wealth-tax deferral carrying cost (h) in the kg
# Bellman + tau_eq, plus a parse check of the touched sources and the
# pre-existing dependency-light kg unit tests (regression: the carry
# args must be invisible to every existing caller).
#
# Submit from repo root:
#   sbatch other/kg_model_tests/wealth_carry_tests.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=wealth_carry
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/wealth_carry_tests_%j.out

set -euo pipefail

module load R/4.4.1-foss-2022b

# Parse check of everything this change touched
Rscript -e 'invisible(parse("src/sim/kg_dynamics.R")); invisible(parse("src/sim/run.R")); cat("PARSE OK\n")'

# New wealth-carry tests
Rscript other/kg_model_tests/test_wealth_carry.R
Rscript other/kg_model_tests/test_tau_eq_wealth.R

# Pre-existing kg unit tests (regression)
Rscript other/kg_model_tests/test_planned_timing.R
Rscript other/kg_model_tests/test_terminal_charity.R
Rscript other/kg_model_tests/test_provenance_guard.R
Rscript other/kg_model_tests/test_naive_limit.R

echo "ALL WEALTH-CARRY + REGRESSION UNIT TESTS PASSED"
