#!/bin/bash
#-----------------------------------------------------------------------
# measure_efull_logs.sbatch  --  Part B1 measurement (read-only).
#
# Measures the full-sim E_full(eta_tilde) surface on the logs eta-dial grid
# (eta_dial_logs_15 / _19 / _23) and inverts for eta_tilde* at -2.52. Submit
# AFTER the three logs eta-dial vintages finish Phase 2N/3b.
#
#   sbatch other/kg_model_tests/form_ab/measure_efull_logs.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=efull_logs
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/kg_form_scratch/efull_logs_%j.out

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/kg_model_tests/form_ab/measure_efull_logs.R
