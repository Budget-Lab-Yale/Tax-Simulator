#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-ded-lever
#SBATCH --output=logs/estate_ded_lever_%j.log
# With/without comparison for the estate.income_tax_ded law lever: dr_prim
# (deduction on, baseline default) vs dr_prim_noded (lever overridden to 0),
# 2026:2029 full sample, baseline reused from the niskanen_housing vintage.
# Expectations: dr_prim reproduces the niskanen_estate_ded_pilot leg exactly
# (default lever = 1 is arithmetic-identical); dr_prim_noded estate results
# match baseline exactly (no deduction => pre-deduction behavior); the two
# legs are identical on the income-tax side. Parse check first.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript -e 'invisible(parse("src/calc/functions/tax/estate.R")); cat("parse OK\n")'
bash slurm_run.sh tests/estate_ded_lever NULL user_test 1 estate_ded_lever 1 0 niskanen_housing 0
