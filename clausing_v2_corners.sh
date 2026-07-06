#!/bin/bash
#-----------------------------------------------------------------------
# clausing_v2_corners.sh — submits the clausing_v2 parameter corners
# AFTER the central run (vintage clausing_v2_s50) has completed and
# validated. All corners reuse the central baseline via baseline_vintage
# (setup.R skips Phase 1 and copies the baseline folder in).
#
#   wealth corners : s = 0.25 / 0.75, FULL 8-layer stack re-run
#                    (s enters every layer's conventional estimate)
#   corp corners   : 08_corporate ONLY, central s = 0.5 runscript, env
#                    overrides exported into the wrapper job (sbatch
#                    propagates env to the phase jobs, --export=ALL):
#                      CORP_SIGMA_N in {0, 0.5}    (central 0.375)
#                      CORP_KAPPA   in {0.25, 0.5} (central 0.40)
#
# Usage (from repo root, login node OK -- it only calls sbatch):
#   bash clausing_v2_corners.sh
#-----------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")"

CENTRAL=clausing_v2_s50

submit () {
  local name=$1 runscript=$2 scen=$3 envset=$4
  sbatch --parsable \
    --job-name="clausing_v2_${name}" \
    --partition=day --cpus-per-task=1 --mem=16G --time=0:30:00 \
    --output="logs/clausing_v2_${name}_%j.log" \
    ${envset:+--export=ALL,${envset}} \
    --wrap="cd '$PWD' && module load R/4.4.1-foss-2022b && bash slurm_run.sh ${runscript} ${scen} user_test 1 clausing_v2_${name} 1 1 ${CENTRAL} 0"
}

# wealth s corners: full stack, baseline reused from central
echo "s25:     $(submit s25 clausing_v2_s25 NULL '')"
echo "s75:     $(submit s75 clausing_v2_s75 NULL '')"

# corp corners: 08_corporate only, central runscript (s = 0.5)
echo "sigma0:  $(submit corp_sigma0  clausing_v2 08_corporate CORP_SIGMA_N=0)"
echo "sigma50: $(submit corp_sigma50 clausing_v2 08_corporate CORP_SIGMA_N=0.5)"
echo "kappa25: $(submit corp_kappa25 clausing_v2 08_corporate CORP_KAPPA=0.25)"
echo "kappa50: $(submit corp_kappa50 clausing_v2 08_corporate CORP_KAPPA=0.5)"
echo "All clausing_v2 corners submitted."
