#!/bin/bash
#-----------------------------------------------------------------------
# calibrate_psi.sbatch
#
# SLURM wrapper for other/kg_model_tests/calibrate_psi.R.
# Bisects KG_DYN_DEFAULT_PSI against the realization-weighted
# semi-elasticity target dlog(R)/dtau ~= -0.6/0.238 using a baseline
# run on disk.
#
# Submit from repo root:
#   sbatch other/kg_model_tests/calibrate_psi.sbatch <baseline_root> [<macro_root>]
#
# <macro_root> is optional; defaults to the production Macro-Projections
# vintage in calibrate_psi.R if omitted.
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_calib_psi
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=other/kg_model_tests/calibrate_psi_%j.out

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "Usage: sbatch other/kg_model_tests/calibrate_psi.sbatch <baseline_root> [<macro_root>]"
  exit 1
fi

BASELINE_ROOT="$1"
MACRO_ROOT="${2:-}"

module load R/4.4.1-foss-2022b
if [ -n "${MACRO_ROOT}" ]; then
  Rscript other/kg_model_tests/calibrate_psi.R "${BASELINE_ROOT}" "${MACRO_ROOT}"
else
  Rscript other/kg_model_tests/calibrate_psi.R "${BASELINE_ROOT}"
fi
