#!/bin/bash
#-----------------------------------------------------------------------
# run_sigma_fix_check.sbatch
#
# Runs q02, the heaviest of the packages the conversion check stopped, to
# confirm the pre-pass and the module now move the same dollars.
#
# Usage:
#   sbatch other/top_tax/run_sigma_fix_check.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=sigfix
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=6:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/sigfix_%j.log

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)"
bash slurm_run.sh tests/sigma_conserve_q02 NULL 1 sigma_q02_fixed 1 0 NULL 0 batch 1
