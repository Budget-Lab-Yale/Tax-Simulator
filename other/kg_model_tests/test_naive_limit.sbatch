#!/bin/bash
#-----------------------------------------------------------------------
# test_naive_limit.sbatch
#
# SLURM wrapper for other/kg_model_tests/test_naive_limit.R -- the
# constant-semi-elasticity / Phi->0 revmax check for the entropy-cost
# Bellman (spec v2). Dependency-light (synthetic grid); no full-sample
# data needed. Sbatch-only (never run R on the login node).
#
# Submit from repo root:
#   sbatch other/kg_model_tests/test_naive_limit.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_naive
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:20:00
#SBATCH --output=other/kg_model_tests/test_naive_limit_%j.out

set -euo pipefail

module load R/4.4.1-foss-2022b

Rscript other/kg_model_tests/test_naive_limit.R
