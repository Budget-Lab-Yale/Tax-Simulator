#!/bin/bash
#-----------------------------------------------------------------------
# estate_offset_tests.sbatch
#
# Unit tests for the estate-margins build (mtr_estate primitive, the
# leg-paired (1 - e) offset in the kg Bellman + tau_eq, and the estate
# own-rate reported-base response), plus a parse check of every touched
# source and the pre-existing dependency-light suites (regression: the
# new args must be invisible to every existing caller).
#
# Submit from repo root:
#   sbatch other/kg_model_tests/estate_offset_tests.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=estate_offset
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:45:00
#SBATCH --output=other/kg_model_tests/estate_offset_tests_%j.out

set -euo pipefail

module load R/4.4.1-foss-2022b

# Parse check of everything this change touched
Rscript -e 'for (f in c("src/sim/kg_dynamics.R", "src/sim/run.R",
                        "src/calc/do_taxes.R",
                        "src/calc/functions/tax/estate.R",
                        "config/scenarios/behavior/wealth/avoidance.R"))
              invisible(parse(f)); cat("PARSE OK\n")'

# New estate-offset tests (Bellman/tau_eq/aggregator/calc_estate bump)
Rscript other/kg_model_tests/test_estate_offset.R

# Hidden-ledger suite (extended with the estate own-rate response tests)
Rscript other/top_tax/tests/test_hidden_ledger_guards.R

# Pre-existing kg unit tests (regression)
Rscript other/kg_model_tests/test_wealth_carry.R
Rscript other/kg_model_tests/test_tau_eq_wealth.R
Rscript other/kg_model_tests/test_planned_timing.R
Rscript other/kg_model_tests/test_terminal_charity.R
Rscript other/kg_model_tests/test_provenance_guard.R
Rscript other/kg_model_tests/test_naive_limit.R

echo "ALL ESTATE-OFFSET + REGRESSION UNIT TESTS PASSED"
