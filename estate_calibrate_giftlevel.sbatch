#!/bin/bash
#SBATCH --time=2:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --partition=day
#SBATCH --job-name=estate-gl
#SBATCH --output=logs/estate_calibrate_giftlevel_%j.log
# Full calibration with gift add-back + level constraint. Writes to a NEW dir so
# the pre-change fit at .../estate_tax_calibration/ is preserved for comparison.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b

TAX_DATA=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline
MACRO=/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration_giftlevel

Rscript other/estate_tax/calibrate_estate_tax.R \
  --tax-data-root "$TAX_DATA" \
  --soi-file other/estate_tax/estate_tax_filed_2019_2023.csv \
  --macro-root "$MACRO" \
  --output-dir "$OUT"
