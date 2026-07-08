#!/bin/bash
#-----------------------------------------------------------------------
# test_tau_eq.sbatch
#
# SLURM wrapper for other/top_tax/tests/test_tau_eq.R: tau_eq linear
# recursion vs finite-difference ground truth (DESIGN_LOCK ruling 1),
# bounds, regime ordering, monotonicity, and recurrence-consistency
# checks on synthetic grids.
#
# Submit from repo root:
#   sbatch other/top_tax/tests/test_tau_eq.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=tau_eq_test
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:20:00
#SBATCH --output=other/top_tax/tests/test_tau_eq_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/top_tax/tests/test_tau_eq.R
