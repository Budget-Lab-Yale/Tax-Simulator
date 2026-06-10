#!/bin/bash
#SBATCH --job-name=clone_check
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --output=clone_check_newvintage_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax

echo "=== NEW pooled-SCF vintage 2026060918 ==="
python3 clone_fingerprint.py /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2022.csv

echo ""
echo "=== OLD vintage 2026052823 (reference) ==="
python3 clone_fingerprint.py /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026052823/baseline/tax_units_2022.csv
