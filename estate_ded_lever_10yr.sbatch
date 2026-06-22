#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-lever-10yr
#SBATCH --output=logs/estate_ded_lever_10yr_%j.log
# Ten-year (FY2027-2036) budget comparison for the estate.income_tax_ded
# lever: dr_prim (deduction on) vs dr_prim_noded (off), 2026:2036 full
# sample, baseline reused from niskanen_housing (same years-span => same
# sample_ids/RNG mapping => clean deltas). Summarize with
# other/estate_tax/ded_lever_10yr_summary.R after the pipeline drains.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/estate_ded_lever_10yr NULL user_test 1 estate_ded_lever_10yr 1 0 niskanen_housing 0
