#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=clausing-launch
#SBATCH --output=clausing_launch_%j.log
# Runs Phase 0 setup on a compute node (not login) and submits the
# SLURM array chain for the clausing stack.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh clausing NULL user_test 1 clausing_2039_surprise 1 1 NULL 0
