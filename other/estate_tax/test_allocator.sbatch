#!/bin/bash
#SBATCH --job-name=test_allocator
#SBATCH --partition=day
#SBATCH -c 1
#SBATCH --mem=8G
#SBATCH --time=0:10:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax/output/test_allocator_%j.log

# Runs the synthetic unit tests for the rank-matching heir allocator
# (test_allocator.R). Exits nonzero on any failure.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
Rscript other/estate_tax/test_allocator.R
