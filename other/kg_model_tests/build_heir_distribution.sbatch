#!/bin/bash
#-----------------------------------------------------------------------
# build_heir_distribution.sbatch
#
# SLURM wrapper for other/kg_model_tests/build_heir_distribution.R.
# Writes resources/heir_distribution_scf2022.csv from SCF 2022 raw data.
# Re-run only when the SCF vintage updates.
#
# Submit from repo root:
#   sbatch other/kg_model_tests/build_heir_distribution.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_heir
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:10:00
#SBATCH --output=other/kg_model_tests/build_heir_%j.out

set -euo pipefail

module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/build_heir_distribution.R
