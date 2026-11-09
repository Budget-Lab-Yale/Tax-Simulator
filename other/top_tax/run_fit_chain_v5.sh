#!/bin/bash
# fit -> validate -> build -> render-check for the 30y dials atlas, v5 vintage
# (net-of-tax realization form, on-model corporate rate to 35, death-gain
# exclusion dial). Pure file I/O + node; login-node safe. Run after the SLURM
# pipeline drains.
set -e
cd "$(dirname "$0")/../.."
ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v5

# Preserve the v3-derived atlas data before overwriting.
if [ -f other/top_tax/atlas2_data.json ] && [ ! -f other/top_tax/atlas2_data_v3_backup.json ]; then
  cp other/top_tax/atlas2_data.json other/top_tax/atlas2_data_v3_backup.json
  echo "backed up v3 atlas -> other/top_tax/atlas2_data_v3_backup.json"
fi

echo "== fit =="
python3 other/top_tax/fit_surrogate.py "$ROOT" other/top_tax/atlas2_data.json
echo "== validate =="
python3 other/top_tax/validate_surrogate.py "$ROOT" other/top_tax/atlas2_data.json
echo "== build =="
python3 other/top_tax/build_atlas.py other/top_tax/atlas2_data.json \
        other/top_tax/atlas2_built.html other/top_tax/atlas2.html
echo "== render harness =="
node other/top_tax/check_atlas2_render.js other/top_tax/atlas2_built.html
echo "== dist card =="
python3 other/top_tax/build_dist_card_data.py "$ROOT"
echo "ALL GREEN (v5): other/top_tax/atlas2_built.html"
