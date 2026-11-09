#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --partition=day
#SBATCH --job-name=estate-smoke
#SBATCH --output=logs/estate_calibrate_smoke_%j.log
# Quick smoke test of the gift add-back + level-constraint changes.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

TAX_DATA=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline
MACRO=/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration_smoke

Rscript other/estate_tax/calibrate_estate_tax.R \
  --tax-data-root "$TAX_DATA" \
  --soi-file other/estate_tax/estate_tax_filed_2019_2023.csv \
  --macro-root "$MACRO" \
  --output-dir "$OUT" \
  --quick
