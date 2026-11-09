#!/bin/bash
#-----------------------------------------------------------------------
# run_sweeps.sh — submits the corp_incidence sweep-corner pipelines
# (plan Verification item 6) AFTER the central corp_test_v1 run has
# validated. Each corner re-runs a single scenario against the already-
# run baseline (baseline_vintage=corp_test_v1), with the env override
# exported into the wrapper job; sbatch propagates the environment to
# the phase jobs (--export=ALL default).
#
#   sigma_N in {0, 0.5}   on corp_perm   (rent-only / house-VAT corners)
#   kappa   in {0.25, 0.5} on corp_perm  (Z.1 owner-occupied-housing fork)
#   priced-as-permanent    on corp_sunset (sunset-disbelief corner)
#
# Usage (from repo root, login node OK -- it only calls sbatch):
#   bash other/corporate_incidence/verify/run_sweeps.sh
#-----------------------------------------------------------------------
set -euo pipefail
cd "$(dirname "$0")/../../.."

submit_corner () {
  local name=$1 scen=$2 envset=$3
  sbatch --parsable \
    --job-name="corp_sweep_${name}" \
    --partition=day --cpus-per-task=2 --mem=48G --time=2:00:00 \
    --output="other/corporate_incidence/logs/sweep_${name}_%j.out" \
    --export=ALL,${envset} \
    --wrap="cd '$PWD' && bash slurm_run.sh tests/corp_incidence ${scen} user_test 1 corp_sweep_${name} 1 1 corp_test_v1 0"
}

echo "sigma0:  $(submit_corner sigma0  corp_perm   CORP_SIGMA_N=0)"
echo "sigma05: $(submit_corner sigma05 corp_perm   CORP_SIGMA_N=0.5)"
echo "kappa25: $(submit_corner kappa25 corp_perm   CORP_KAPPA=0.25)"
echo "kappa50: $(submit_corner kappa50 corp_perm   CORP_KAPPA=0.5)"
echo "pap:     $(submit_corner pap     corp_sunset CORP_PRICED_AS_PERMANENT=1)"
echo "All sweep corners submitted."
