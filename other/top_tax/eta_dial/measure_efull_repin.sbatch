#!/bin/bash
#-----------------------------------------------------------------------
# measure_efull_repin.sbatch
#
# Re-measure the full-sim E_full(eta) surface on the 2026-07-12 re-pin grid
# (vintages eta_dial_e20_v2 / eta_dial_c_v2 / eta_dial_e30_v2; estate offset
# live; Tax-Data 2026070814) and invert the zero-intercept line for eta*.
# Writes efull_by_eta.csv + eta_repin_fit.csv. Read-only w.r.t. the sim.
#
# Submit AFTER the three eta_dial_repin vintages finish Phase 2C/3b:
#   sbatch other/top_tax/eta_dial/measure_efull_repin.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=efull_repin
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=0:30:00
#SBATCH --output=other/top_tax/eta_dial/repin_logs/measure_efull_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b

Rscript other/top_tax/eta_dial/measure_efull_by_eta.R
