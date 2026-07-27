#!/bin/bash
#SBATCH --job-name=launch_timeable
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=2:00:00
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/form_ab/logs/launch_timeable_%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
mkdir -p other/kg_model_tests/form_ab/logs
# Wrapper so Phase 0 (setup.R, in process) runs on a compute node.
bash other/kg_model_tests/form_ab/launch_timeable.sh "$@"
echo "LAUNCH_EXIT=$?"
