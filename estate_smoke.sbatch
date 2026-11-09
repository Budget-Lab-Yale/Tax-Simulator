#!/bin/bash
#SBATCH --time=4:00:00
#SBATCH --mem=120G
#SBATCH --cpus-per-task=16
#SBATCH --partition=day
#SBATCH --job-name=estate-smoke
#SBATCH --output=logs/estate_smoke_%j.log
# Single-node smoke of the on-model estate tax: baseline + estate_sunset at
# 5% sample, years 2025:2034, multicore over years. Checks plumbing only
# (tax law parse, calc_estate join, totals/estate.csv, receipts delta);
# numbers are top-tail noisy at this sample and not for validation.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

Rscript src/main.R tests/estate_sunset NULL jar335 1 estate_smoke 0.05 1 NULL 0 year
