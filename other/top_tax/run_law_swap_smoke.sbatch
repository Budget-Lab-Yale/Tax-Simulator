#!/bin/bash
#SBATCH --job-name=lawswap-smoke
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=2:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/lawswap_smoke_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)"
bash slurm_run.sh tests/mech_law_swap_smoke NULL 1 lawswap_smoke 1 0 NULL 0 batch 2
