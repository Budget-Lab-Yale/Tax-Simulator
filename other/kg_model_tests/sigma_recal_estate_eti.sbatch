#!/bin/bash
#-----------------------------------------------------------------------
# sigma_recal_estate_eti.sbatch
#
# ETI measurement + sigma residual for the estate-margins-build sigma
# re-derivation (plan verification #8). Runs compute_top_eti.R on both
# legs of vintage sigma_recal_estate (sigma leg ran at SIGMA_CONV=0.60)
# and prints the solved sigma* = 0.6*(0.25 - ETI_ns)/(ETI_s - ETI_ns).
# Expectation: sigma* ~= 0.08 (the estate term shifts tau_eq nearly
# symmetrically on this leg -- e_S = e_B, CG/death regime fixed).
#
# Submit with dependency on the sigma_recal_estate pipeline's 2C + 3a.
#-----------------------------------------------------------------------
#SBATCH --job-name=sigma_estate_eti
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=1:00:00
#SBATCH --output=other/kg_model_tests/sigma_recal_estate_eti_%j.out

set -euo pipefail
module load R/4.4.1-foss-2022b

ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/sigma_recal_estate

echo "=== ETI, sigma leg (SIGMA_CONV = 0.60) ==="
Rscript other/top_tax/archive/tests/compute_top_eti.R \
  "$ROOT" topord_plus5 "$ROOT" \
  "$ROOT/topord_plus5/conventional/supplemental/kg_dynamics_state" 2025 2035 \
  | tee /tmp/eti_sigma.txt

echo ""
echo "=== ETI, no-sigma leg ==="
Rscript other/top_tax/archive/tests/compute_top_eti.R \
  "$ROOT" topord_plus5_nosigma "$ROOT" \
  "$ROOT/topord_plus5/conventional/supplemental/kg_dynamics_state" 2025 2035 \
  | tee /tmp/eti_nosigma.txt

ETI_S=$(grep 'Mean ETI' /tmp/eti_sigma.txt   | grep -oP '[-0-9.]+$')
ETI_N=$(grep 'Mean ETI' /tmp/eti_nosigma.txt | grep -oP '[-0-9.]+$')
echo ""
echo "ETI (sigma=0.60) = $ETI_S ; ETI (no sigma) = $ETI_N"
python3 -c "
s, n = $ETI_S, $ETI_N
star = 0.6 * (0.25 - n) / (s - n)
print(f'solved sigma* = {star:.4f}  (current SIGMA_CONV central = 0.08)')
print('VERDICT:', 'UNCHANGED (keep 0.08)' if abs(round(star, 2) - 0.08) < 0.005
      else f'MOVED -> update SIGMA_CONV to {round(star, 2)} + bump SIGMA_CONV_VERSION')"
