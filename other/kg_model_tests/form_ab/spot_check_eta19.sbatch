#!/bin/bash
#SBATCH --job-name=spot_eta19
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --mem=64G
#SBATCH --time=0:40:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/config_redesign/logs/spot_eta19_%j.log

# Compares the re-simulated eta-dial vintage against the one on scratch.
# See spot_check_eta19.R for what it reports and why it reports rather than asserts.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/form_ab/spot_check_eta19.R
