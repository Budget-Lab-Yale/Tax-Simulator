#!/bin/bash
#SBATCH --job-name=shape_check
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --output=logs/shape_check_newvintage_%j.log

cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/estate_tax
NEW=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline/tax_units_2022.csv

echo "=== first_order_shape_multiyear, NEW pooled-SCF vintage 2026060918 ==="
python3 first_order_shape_multiyear.py $NEW

echo ""
echo "=== shape_decloned, NEW pooled-SCF vintage 2026060918 ==="
python3 shape_decloned.py $NEW
