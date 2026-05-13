#!/bin/bash
#-----------------------------------------------------------------------
# calibrate.sbatch
#
# SLURM wrapper for other/kg_model_tests/calibrate.R. Jointly bisects
# KG_DYN_DEFAULT_PSI (long-run permanent semi-elasticity) and
# KG_DYN_SHARE_PLANNED (short-run announced-shock semi-elasticity)
# against the two targets declared in calibrate.R.
#
# Submit from repo root:
#   sbatch other/kg_model_tests/calibrate.sbatch <baseline_root> [<macro_root>]
#
# <baseline_root> must contain >= 30 years of baseline/static/detail
# with mtr_kg_lt registered.
# <macro_root> is optional; defaults to the production Macro-Projections
# vintage in calibrate.R if omitted.
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_calib
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=2:00:00
#SBATCH --output=other/kg_model_tests/calibrate_%j.out

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "Usage: sbatch other/kg_model_tests/calibrate.sbatch <baseline_root> [<macro_root>]"
  exit 1
fi

BASELINE_ROOT="$1"
MACRO_ROOT="${2:-}"

module load R/4.4.1-foss-2022b
if [ -n "${MACRO_ROOT}" ]; then
  Rscript other/kg_model_tests/calibrate.R "${BASELINE_ROOT}" "${MACRO_ROOT}"
else
  Rscript other/kg_model_tests/calibrate.R "${BASELINE_ROOT}"
fi
