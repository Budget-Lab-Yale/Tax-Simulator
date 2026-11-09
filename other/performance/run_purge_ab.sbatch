#!/bin/bash
#-----------------------------------------------------------------------
# run_purge_ab.sbatch
#
# Runs the kg-and-wealth smoke twice, once keeping every detail tree and
# once purging each measurement tree as its recurrence finishes with it,
# and compares everything that is not detail. Identical output is the
# proof that nothing downstream reads those trees.
#
# Usage:
#   sbatch other/performance/run_purge_ab.sbatch
#-----------------------------------------------------------------------
#SBATCH --job-name=purge_ab
#SBATCH --partition=day
#SBATCH -c 2
#SBATCH --time=8:00:00
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/performance/logs/purge_ab_%j.log

set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1

echo "=== leg A: delete_detail=none ==="
bash slurm_run.sh tests/mech_kg_smoke NULL 1 purge_ab_none 1 0 NULL none batch 1

# Wait for A to drain before starting B: both write their own vintage, but the
# comparison needs A finished and the cluster share is easier one at a time.
sleep 60
while squeue -u "$(whoami)" -h -o '%j' | grep -q taxsim; do sleep 60; done

echo ""
echo "=== leg B: delete_detail=transient ==="
bash slurm_run.sh tests/mech_kg_smoke NULL 1 purge_ab_transient 1 0 NULL transient batch 1

sleep 60
while squeue -u "$(whoami)" -h -o '%j' | grep -q taxsim; do sleep 60; done

echo ""
echo "=== what each leg left on disk ==="
for v in purge_ab_none purge_ab_transient; do
  echo "-- $v"
  du -sh "$ROOT/$v" 2>/dev/null || true
  find "$ROOT/$v" -type d -name detail | while read -r d; do
    printf '   %-58s %s file(s)\n' "${d#$ROOT/$v/}" "$(ls "$d" | wc -l)"
  done
done

echo ""
echo "=== comparing everything that is not detail ==="
diff -r -q \
  --exclude=detail --exclude=_slurm_staging --exclude=code_version.csv \
  "$ROOT/purge_ab_none" "$ROOT/purge_ab_transient" \
  && echo "IDENTICAL -- nothing downstream reads the measurement trees" \
  || echo "DIFFERENCES ABOVE -- something does read them"
