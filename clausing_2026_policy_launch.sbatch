#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-2026-policy
#SBATCH --output=logs/clausing_2026_policy_launch_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh clausing_2026_policy NULL user_test 1 clausing_2026_policy 1 1 NULL 0
