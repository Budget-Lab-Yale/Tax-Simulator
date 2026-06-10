#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --job-name=estate-stage2-launch
#SBATCH --output=estate_stage2_launch_%j.log
# Stage-2 heir-allocator validation: baseline + estate_sunset at full sample,
# dist_years 2026:2027 exercise the rank-matching allocator in Phase 3b.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
bash slurm_run.sh tests/estate_sunset NULL jar335 1 estate_stage2 1 1 NULL 0
