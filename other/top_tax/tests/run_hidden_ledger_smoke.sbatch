#!/bin/bash
#-----------------------------------------------------------------------
# run_hidden_ledger_smoke.sbatch
#
# Launches the full SLURM pipeline for the hidden_ledger_smoke fixture from
# INSIDE a compute-node job (so Phase 0 setup.R never touches the login node).
# slurm_run.sh submits its own array jobs; they inherit this job's environment
# via the default sbatch --export=ALL, so the WEALTH_CHI_* knobs (if exported
# into this job) reach the Phase 2C behavior workers.
#
# Submit from repo root, picking the vintage / CHI via --export, e.g.:
#   # CHI-central (defaults 1.0 / 0.5)
#   sbatch --export=ALL,HL_VINTAGE=hl_central \
#     other/top_tax/tests/run_hidden_ledger_smoke.sbatch
#   # CHI = 0 no-op control
#   sbatch --export=ALL,HL_VINTAGE=hl_chi0,WEALTH_CHI_PUB=0,WEALTH_CHI_PRIV=0 \
#     other/top_tax/tests/run_hidden_ledger_smoke.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=hl_launch
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=0:40:00
#SBATCH --output=other/top_tax/tests/run_hidden_ledger_%j.out

set -euo pipefail

REPO_DIR=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
cd "${REPO_DIR}"

: "${HL_VINTAGE:?set HL_VINTAGE via --export}"
echo "=== hidden_ledger_smoke launch: vintage=${HL_VINTAGE} "\
"CHI_PUB=${WEALTH_CHI_PUB:-<default 1.0>} CHI_PRIV=${WEALTH_CHI_PRIV:-<default 0.5>} ==="

# runscript scenario_id user_id local vintage pct_sample stacked baseline_vintage delete_detail
bash slurm_run.sh tests/hidden_ledger_smoke NULL user_test 1 "${HL_VINTAGE}" 1 1 NULL 0
