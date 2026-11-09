#!/bin/bash
#SBATCH --job-name=eta_dial_logs
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=1:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/config_redesign/logs/eta_dial_logs_%x_%j.log

# Submits the logs eta-dial grid from a COMPUTE node.
#
# launch_eta_dial_logs.sh calls slurm_run.sh, whose Phase 0 runs setup.R in
# process -- parsing globals, reading Tax-Data and serializing the configs. That
# is real R work and does not belong on the login node, so the launcher goes
# through here, the same way the config-rebuild gate runs do. The dependent array
# jobs outlive this wrapper.
#
# Usage:
#   sbatch other/kg_model_tests/form_ab/launch_eta_dial_logs.sbatch            # whole grid
#   sbatch other/kg_model_tests/form_ab/launch_eta_dial_logs.sbatch 19         # one point
#   VINTAGE_SUFFIX=_spot sbatch --export=ALL \
#     other/kg_model_tests/form_ab/launch_eta_dial_logs.sbatch 19              # spot check
#
# VINTAGE_SUFFIX is required for a re-run of a point that already exists on
# scratch; see the launcher's header for why.
set -euo pipefail

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
echo "head=$(git rev-parse --short HEAD)  VINTAGE_SUFFIX=${VINTAGE_SUFFIX:-<none>}  tags=${*:-<all>}"

bash other/kg_model_tests/form_ab/launch_eta_dial_logs.sh "$@"
echo "LAUNCH_EXIT=$?"
