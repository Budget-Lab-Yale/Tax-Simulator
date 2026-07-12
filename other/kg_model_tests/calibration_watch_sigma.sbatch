#!/bin/bash
#-----------------------------------------------------------------------
# calibration_watch_sigma.sbatch  --  Tier B of the calibration staleness watch
#
# EXPENSIVE, on-trigger only (a commit touched the SIGMA_CONV invalidation list).
# Re-derives sigma* from the two-leg top-ordinary ETI and compares it to the
# SHIPPED SIGMA_CONV (0.16). Generalizes sigma_recal_estate_eti.sbatch to the
# tests/sigma_recal_eta legs; DETECT-ONLY -- no auto-repin.
#
# PREREQUISITE (run first, it is the expensive part -- 1 pipeline over 2 legs):
#   export SIGMA_CONV=0.60
#   bash slurm_run.sh tests/sigma_recal_eta NULL jar335 1 sigma_watch <pct=1> 0 NULL 0
#   # topord_plus5 runs the sigma leg at the 0.60 LEVER; topord_plus5_nosigma
#   # excludes conversion/sigma so SIGMA_CONV is a no-op for it.
# THEN:
#   sbatch other/kg_model_tests/calibration_watch_sigma.sbatch [<vintage_root>]
#
# <vintage_root> defaults to the scratch sigma_watch vintage.
#-----------------------------------------------------------------------
#SBATCH --job-name=calib_watch_B
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=1:00:00
#SBATCH --output=other/kg_model_tests/calibration_watch_B_%j.out

set -uo pipefail
module load R/4.4.1-foss-2022b

ROOT="${1:-/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/sigma_watch}"
SIGMA_LEVER=0.60   # the SIGMA_CONV the topord_plus5 leg was RUN at (the measurement lever)
SHIPPED=0.16       # current SIGMA_CONV central

STATE_DIR="$ROOT/topord_plus5/conventional/supplemental/kg_dynamics_state"

echo "=== Tier B: top-ordinary ETI, sigma leg (SIGMA_CONV lever = $SIGMA_LEVER) ==="
Rscript other/top_tax/archive/tests/compute_top_eti.R \
  "$ROOT" topord_plus5 "$ROOT" "$STATE_DIR" 2025 2035 | tee /tmp/eti_sigma_watch.txt

echo ""
echo "=== Tier B: top-ordinary ETI, no-sigma leg ==="
Rscript other/top_tax/archive/tests/compute_top_eti.R \
  "$ROOT" topord_plus5_nosigma "$ROOT" "$STATE_DIR" 2025 2035 | tee /tmp/eti_nosigma_watch.txt

ETI_S=$(grep 'Mean ETI' /tmp/eti_sigma_watch.txt   | grep -oP '[-0-9.]+$')
ETI_N=$(grep 'Mean ETI' /tmp/eti_nosigma_watch.txt | grep -oP '[-0-9.]+$')
echo ""
echo "ETI (sigma=$SIGMA_LEVER) = $ETI_S ; ETI (no sigma) = $ETI_N"

python3 -c "
lever, shipped = $SIGMA_LEVER, $SHIPPED
s, n = $ETI_S, $ETI_N
star = lever * (0.25 - n) / (s - n)
drift = star - shipped
print(f'solved sigma* = {star:.4f}  (shipped SIGMA_CONV central = {shipped})')
if abs(drift) <= 0.01:
    print(f'WATCH TIER B: PASS -- sigma* within +-0.01 of shipped (drift {drift:+.4f})')
    raise SystemExit(0)
else:
    print(f'WATCH TIER B: DRIFT -- update SIGMA_CONV to {round(star,2)} + bump SIGMA_CONV_VERSION (drift {drift:+.4f})')
    raise SystemExit(3)
"
exit $?
