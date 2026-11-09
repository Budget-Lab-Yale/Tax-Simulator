#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=kg-frac-launch
#SBATCH --output=logs/kg_frac_launch_%j.log
# Two-leg expected-deemed comparison runs, deemed scenario only (carryover
# already validated identical to binary; baselines reused from prior vintages)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/kg_alloc_compare        deemed user_test 1 kg_mech_50_frac 1 1 kg_mech_50        0
bash slurm_run.sh tests/kg_alloc_compare_td0609 deemed user_test 1 kg_td0609_frac  1 1 kg_mech_50_td0609 0
