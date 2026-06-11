#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-chain-check
#SBATCH --output=estate_chain_check_%j.log
# Commit-1 bit-identity check for the estate-into-do_taxes refactor:
# baseline-only re-run of private/niskanen_housing (full sample, Tax-Data
# 2026060918). Same runscript and 2026:2036 years => identical sample_ids
# construction and id->random-number mapping, so static detail and totals
# must be BIT-IDENTICAL to the existing niskanen_housing baseline. Compare
# with estate_chain_compare.sbatch after the pipeline drains. Parse check
# first so syntax errors fail fast.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript -e 'invisible(parse("src/calc/do_taxes.R")); invisible(parse("src/calc/functions/tax/estate.R")); invisible(parse("src/sim/run.R")); cat("parse OK\n")'
bash slurm_run.sh private/niskanen_housing baseline user_test 1 estate_chain_check 1 0 NULL 0
