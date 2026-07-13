#!/bin/bash
# One-shot fit -> validate -> build -> render-check for the 30y dials atlas.
# Pure file I/O + node; login-node safe. Run when the SLURM pipeline drains.
set -e
cd "$(dirname "$0")/../.."
ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v2

echo "== fit =="
python3 other/top_tax/fit_surrogate.py "$ROOT" other/top_tax/atlas2_data.json
echo "== validate =="
python3 other/top_tax/validate_surrogate.py "$ROOT" other/top_tax/atlas2_data.json
echo "== build =="
python3 other/top_tax/build_atlas.py other/top_tax/atlas2_data.json \
        other/top_tax/atlas2_built.html other/top_tax/atlas2.html
echo "== render harness =="
node other/top_tax/check_atlas2_render.js other/top_tax/atlas2_built.html
echo "ALL GREEN: other/top_tax/atlas2_built.html"
