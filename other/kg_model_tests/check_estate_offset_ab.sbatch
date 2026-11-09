#!/bin/bash
#-----------------------------------------------------------------------
# check_estate_offset_ab.sbatch
#
# Post-run verification of the estate-margins build A/B pipeline
# (vintage estate_offset_ab_v1). Submit AFTER (or with --dependency on)
# the estate_offset_ab slurm_run.sh pipeline:
#   sbatch [--dependency=afterok:<p2C>,<p3a>] \
#     other/kg_model_tests/check_estate_offset_ab.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=estate_ab_check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/check_estate_offset_ab_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/check_estate_offset_ab.R
