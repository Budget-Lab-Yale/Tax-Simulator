#!/bin/bash
#SBATCH --time=0:20:00
#SBATCH --mem=24G
#SBATCH --cpus-per-task=2
#SBATCH --partition=day
#SBATCH --job-name=estate-q
#SBATCH --output=estate_quick_%j.log
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
module load R/4.4.1-foss-2022b
TAX_DATA=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline
MACRO=/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline
Rscript other/estate_tax/calibrate_estate_tax.R \
  --tax-data-root "$TAX_DATA" \
  --soi-file other/estate_tax/estate_tax_filed_2019_2023.csv \
  --macro-root "$MACRO" \
  --output-dir /nfs/roberts/scratch/pi_nrs36/jar335/estate_tax_calibration_quick \
  --quick
