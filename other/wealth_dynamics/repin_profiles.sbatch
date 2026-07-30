#!/bin/bash
#SBATCH --job-name=repin
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --time=0:10:00
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics/logs/repin_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "=== regenerate the financing profiles ==="
python3 other/wealth_dynamics/write_profiles.py
echo ""
echo "=== diff (empty is the pass) ==="
git status --short config/calibrations/wealth_profiles/ || true
echo "(end)"
echo ""
echo "=== new hash ==="
md5sum src/sim/wealth_dynamics.R
