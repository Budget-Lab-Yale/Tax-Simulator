#!/bin/bash
#SBATCH --job-name=revmax-v6-launch
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=4:00:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/top_tax/logs/revmax_v6_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
VINTAGE=kg_v6_revmax
bash slurm_run.sh tests/revmax_cg NULL 1 "${VINTAGE}" 1 0 NULL transient batch 2
