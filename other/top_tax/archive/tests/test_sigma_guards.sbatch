#!/bin/bash
#-----------------------------------------------------------------------
# test_sigma_guards.sbatch
#
# SLURM wrapper for other/top_tax/tests/test_sigma_guards.R: sigma shared
# pure function (gate/pool/wedge/clamp), conservation identity, record
# applier + SECA companions, threshold extractor, and all do_conversion
# hard-stop guards.
#
# Submit from repo root:
#   sbatch other/top_tax/tests/test_sigma_guards.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=sigma_guards
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:20:00
#SBATCH --output=other/top_tax/tests/test_sigma_guards_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b
Rscript other/top_tax/tests/test_sigma_guards.R
