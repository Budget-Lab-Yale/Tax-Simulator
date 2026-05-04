#!/bin/bash
#-----------------------------------------------------------------------
# calibrate_eta.sbatch
#
# SLURM wrapper for other/kg_model_tests/calibrate_eta.R.
# Bisects KG_DYN_DEFAULT_ETA against the realization-weighted -0.6
# elasticity target using a baseline run on disk.
#
# Submit from repo root:
#   sbatch other/kg_model_tests/calibrate_eta.sbatch <baseline_root>
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_calib_eta
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/calibrate_eta_%j.out

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "Usage: sbatch other/kg_model_tests/calibrate_eta.sbatch <baseline_root>"
  exit 1
fi

BASELINE_ROOT="$1"

module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/calibrate_eta.R "${BASELINE_ROOT}"
