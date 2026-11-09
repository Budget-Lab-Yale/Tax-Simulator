#!/bin/bash
#-----------------------------------------------------------------------
# kg_unit_tests.sbatch
#
# Runs the dependency-light kg_dynamics unit tests (no full-sample data
# needed): planned timing / scenario-rate / Pass-1 inversion, terminal
# charity MC ordering, and the calibration-provenance guard. Sbatch-only
# (never run R on the login node).
#
# Submit from repo root:
#   sbatch other/kg_model_tests/kg_unit_tests.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_unit
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:20:00
#SBATCH --output=other/kg_model_tests/kg_unit_tests_%j.out

set -euo pipefail

module load R/4.4.1-foss-2022b

Rscript other/kg_model_tests/test_planned_timing.R
Rscript other/kg_model_tests/test_terminal_charity.R
Rscript other/kg_model_tests/test_provenance_guard.R
Rscript other/kg_model_tests/test_naive_limit.R
Rscript other/kg_model_tests/test_logs_form.R

echo "ALL KG UNIT TESTS PASSED"
