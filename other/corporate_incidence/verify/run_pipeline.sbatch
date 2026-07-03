#!/bin/bash
#-----------------------------------------------------------------------
# run_pipeline.sbatch — submits the full-sample SLURM pipeline for the
# corp_incidence test runscript (8 scenarios incl. wealth s=0.5 and
# kg_dynamics compositions). Phase 0 setup runs inside this job (off the
# login node); slurm_run.sh then submits the dependency chain and exits.
#
# Submit from repo root:
#   sbatch other/corporate_incidence/verify/run_pipeline.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=corp_pipeline
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=2:00:00
#SBATCH --output=other/corporate_incidence/logs/run_pipeline_%j.out

set -euo pipefail
cd "${SLURM_SUBMIT_DIR:-.}"

bash slurm_run.sh tests/corp_incidence NULL user_test 1 corp_test_v1 1 1 NULL 0
