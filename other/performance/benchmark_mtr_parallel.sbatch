#!/bin/bash
#SBATCH --job-name=mtr-parity
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --time=0:30:00
#SBATCH --output=other/performance/benchmark_mtr_parallel_%j.log

set -euo pipefail
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

Rscript other/performance/benchmark_mtr_parallel.R \
  /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/perf_probe_stage/_slurm_staging
