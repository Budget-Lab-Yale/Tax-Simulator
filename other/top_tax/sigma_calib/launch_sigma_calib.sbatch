#!/bin/bash
#SBATCH --job-name=launch_sigma
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/sigma_calib/logs/launch_%j.log

# Wrapper so Phase 0 (setup.R, in process) runs on a compute node.
#   sbatch other/top_tax/sigma_calib/launch_sigma_calib.sbatch [charity] [tag ...]
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/top_tax/sigma_calib/logs
bash other/top_tax/sigma_calib/launch_sigma_calib.sh "$@"
echo "LAUNCH_EXIT=$?"
