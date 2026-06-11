#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-ded-pilot
#SBATCH --output=niskanen_estate_ded_pilot_%j.log
# Commit-2 pilot for the deemed-tax-deductible-against-estate change.
# dr_prim (deemed realization: estate totals must fall vs baseline, only
# through estates with a positive conditional deemed tax) and co_prim
# (carryover: no tax at death => no deduction => estate totals exactly
# equal baseline's), 2026:2029 full sample (kg_dynamics convention),
# baseline reused from the niskanen_housing vintage. Acceptance: dr_prim
# totals/estate.csv < baseline; receipts show negative
# delta_revenues_estate_tax lagged one FY; co_prim totals/estate.csv ==
# baseline exactly; estate_income_tax_ded lands in dr_prim static detail.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/niskanen_estate_ded_pilot NULL user_test 1 niskanen_estate_ded_pilot 1 0 niskanen_housing 0
