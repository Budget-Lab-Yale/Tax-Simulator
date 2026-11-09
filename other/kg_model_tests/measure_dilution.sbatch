#!/bin/bash
#-----------------------------------------------------------------------
# measure_dilution.sbatch
#
# SLURM wrapper for other/kg_model_tests/measure_dilution.R. Measures the
# full-sim kg_dynamics realization semi-elasticities on a completed
# vintage and prints the dilution factors calibrate.R needs.
#
# Submit from repo root:
#   sbatch other/kg_model_tests/measure_dilution.sbatch \
#          <vintage> <local_root> <E_int_long> <E_int_short>
#
# <vintage>     : the completed dilution-loop vintage (e.g. kg_eta_recal_iter1)
# <local_root>  : the model_data/v1 root the vintage lives under
# <E_int_long>  : calibrate.R's E_int_long  for the (eta, omega) the sim ran
# <E_int_short> : calibrate.R's E_int_short for the (eta, omega) the sim ran
#-----------------------------------------------------------------------
#SBATCH --job-name=kg_dilution
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=1:00:00
#SBATCH --output=other/kg_model_tests/measure_dilution_%j.out

set -euo pipefail

if [ "$#" -lt 4 ]; then
  echo "Usage: sbatch other/kg_model_tests/measure_dilution.sbatch <vintage> <local_root> <E_int_long> <E_int_short>"
  exit 1
fi

module load R/4.4.1-foss-2022b

Rscript other/kg_model_tests/measure_dilution.R "$1" "$2" "$3" "$4"
