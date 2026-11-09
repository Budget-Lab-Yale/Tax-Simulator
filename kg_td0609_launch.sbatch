#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=kg-td0609-launch
#SBATCH --output=logs/kg_td0609_launch_%j.log
# Re-runs the kg mechanical-pass scenarios (baseline, carryover, deemed) on
# Tax-Data vintage 2026060918 (updated wealth imputation), full sample, for
# comparison against kg_mech_50 (Tax-Data 2026050315).
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/kg_alloc_compare_td0609 NULL user_test 1 kg_mech_50_td0609 1 1 NULL 0
