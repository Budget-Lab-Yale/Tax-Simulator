#!/bin/bash
#-------------------------------------------------------------------------------
# sm_bound_orchestrator.sh <stage> <id_runscript> <id_vintage> <un_runscript> <un_vintage>
#
# Generalized s×M bounding orchestrator: serializes an M=identity pipeline and an
# M=uniform pipeline for ANY reform, swapping the global transition_matrix_file
# (read live at Phase 2W) between them via a SLURM dependency chain.
#
#   p1 (launch): backup yaml -> M=identity (null) -> run identity pipeline
#                -> submit bridge gated afterok on ALL p1 jobs
#   p2 (bridge): M=uniform -> run uniform pipeline -> submit restore (afterany)
#   restore    : restore pristine yaml from backup
#
# Every stage runs slurm_run.sh inside an sbatch job (no login-node R). While the
# uniform pipeline runs, do NOT launch other s>0 wealth runs (they'd read uniform M).
#-------------------------------------------------------------------------------
set -uo pipefail

REPO=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
cd "$REPO" || exit 1
module load R/4.4.1-foss-2022b

YAML="config/wealth/wealth_financing_params.yaml"
BAK="$REPO/other/wealth_dynamics/.wealth_financing_params.yaml.bak"
SELF="$REPO/other/wealth_dynamics/sm_bound_orchestrator.sh"
LOGD="$REPO/other/wealth_dynamics/logs"
UNIFORM="./config/wealth/wealth_transition_uniform.rds"
mkdir -p "$LOGD"

set_M () { sed -i "s|^transition_matrix_file:.*|transition_matrix_file: $1|" "$YAML"; echo "  [set_M] -> $(grep -n '^transition_matrix_file:' "$YAML")"; }
ids_from () { echo "$1" | grep -oE 'Job ID: [0-9]+' | grep -oE '[0-9]+' | paste -sd:; }

STAGE="${1:?usage: sm_bound_orchestrator.sh <p1|p2|restore> <id_rs> <id_v> <un_rs> <un_v>}"
ID_RS="${2:-}"; ID_V="${3:-}"; UN_RS="${4:-}"; UN_V="${5:-}"
echo "=== sm_bound_orchestrator stage=$STAGE  id=$ID_RS/$ID_V  un=$UN_RS/$UN_V  $(date) ==="

case "$STAGE" in
  p1)
    cp -f "$YAML" "$BAK"
    set_M "null"
    OUT=$(bash slurm_run.sh "$ID_RS" NULL user_test 1 "$ID_V" 1 1 NULL 0 2>&1); RC=$?
    echo "$OUT"; [ $RC -eq 0 ] || { echo "ERROR: p1 slurm_run.sh exited $RC"; exit 1; }
    IDS=$(ids_from "$OUT"); echo "p1 ids: $IDS"; [ -n "$IDS" ] || { echo "ERROR: no p1 ids"; exit 1; }
    sbatch --partition=day -c1 --time=0:20:00 --mem=4G \
      --dependency=afterok:$IDS --kill-on-invalid-dep=yes \
      --job-name=smbnd-bridge --output="$LOGD/smbridge_%j.log" \
      --wrap="bash $SELF p2 '$ID_RS' '$ID_V' '$UN_RS' '$UN_V'"
    echo "submitted bridge (p2) gated afterok on p1" ;;
  p2)
    [ -f "$UNIFORM" ] || { echo "ERROR: missing $UNIFORM"; exit 1; }
    set_M "$UNIFORM"
    OUT=$(bash slurm_run.sh "$UN_RS" NULL user_test 1 "$UN_V" 1 1 NULL 0 2>&1); RC=$?
    echo "$OUT"; [ $RC -eq 0 ] || { echo "ERROR: p2 slurm_run.sh exited $RC"; exit 1; }
    IDS=$(ids_from "$OUT"); echo "p2 ids: $IDS"; [ -n "$IDS" ] || { echo "ERROR: no p2 ids"; exit 1; }
    sbatch --partition=day -c1 --time=0:10:00 --mem=4G \
      --dependency=afterany:$IDS \
      --job-name=smbnd-restore --output="$LOGD/smrestore_%j.log" \
      --wrap="bash $SELF restore"
    echo "submitted restore gated afterany on p2" ;;
  restore)
    if [ -f "$BAK" ]; then cp -f "$BAK" "$YAML"; rm -f "$BAK"; echo "  restored pristine yaml"; else set_M "null"; echo "  backup missing; forced null"; fi
    echo "  $(grep -n '^transition_matrix_file:' "$YAML")"
    echo "=== SM BOUNDING SWEEP COMPLETE $(date) ===" ;;
  *) echo "unknown stage: $STAGE"; exit 1 ;;
esac
