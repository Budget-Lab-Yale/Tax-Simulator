#!/bin/bash
#-------------------------------------------------------------------------------
# bounding_orchestrator.sh <p1|p2|restore>
#
# Serializes the two M-pipelines of the s x M bounding exercise (CG +5pp under
# carryover basis), swapping the GLOBAL transition_matrix_file in
# config/wealth/wealth_financing_params.yaml between them.
#
# WHY a chain: transition_matrix_file (the M operator) is read LIVE at Phase 2W
# (build_within_age_transition <- wealth_dyn_load_params, src/sim/wealth_dynamics.R),
# NOT captured at Phase 0 setup. So two pipelines with different M MUST NOT
# overlap. A SLURM dependency chain enforces strict serialization:
#
#   stage p1 (launch) : backup yaml -> force M=identity (null) -> run P1 pipeline
#                       (M=identity) -> submit 'bridge' gated afterok on ALL P1 jobs
#   stage p2 (bridge) : set M=uniform -> run P2 pipeline (M=uniform, own baseline)
#                       -> submit 'restore' gated afterany on ALL P2 jobs
#   stage restore     : restore the pristine yaml from backup
#
# Because the bridge is gated afterok on every P1 job (incl. Phase 4), P1's
# Phase 2W has long finished before the yaml is touched. Every stage runs
# slurm_run.sh INSIDE an sbatch job, so Phase 0 setup never runs on the login
# node. WARNING: while P2 runs (yaml=uniform) do NOT launch any other wealth
# (s>0) scenario -- it would pick up the uniform M. The window is one pipeline.
#-------------------------------------------------------------------------------
set -uo pipefail

REPO=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
cd "$REPO" || exit 1
module load R/4.4.1-foss-2022b

YAML="config/wealth/wealth_financing_params.yaml"
BAK="$REPO/other/wealth_dynamics/.wealth_financing_params.yaml.bak"
SELF="$REPO/other/wealth_dynamics/bounding_orchestrator.sh"
LOGD="$REPO/other/wealth_dynamics/logs"
UNIFORM="./config/wealth/wealth_transition_uniform.rds"
mkdir -p "$LOGD"

set_M () {  # $1 = value placed after 'transition_matrix_file:'
  sed -i "s|^transition_matrix_file:.*|transition_matrix_file: $1|" "$YAML"
  echo "  [set_M] -> $(grep -n '^transition_matrix_file:' "$YAML")"
}

ids_from () {  # ':'-joined SLURM job ids from slurm_run.sh stdout
  echo "$1" | grep -oE 'Job ID: [0-9]+' | grep -oE '[0-9]+' | paste -sd:
}

STAGE="${1:?usage: bounding_orchestrator.sh <p1|p2|restore>}"
echo "=== bounding_orchestrator stage=$STAGE $(date) ==="

case "$STAGE" in
  p1)
    cp -f "$YAML" "$BAK"            # pristine backup (restored at the end)
    set_M "null"                   # identity / full persistence
    OUT=$(bash slurm_run.sh tests/cgcarry_bound_identity NULL user_test 1 cgcarry_bound_identity 1 1 NULL 0 2>&1); RC=$?
    echo "$OUT"
    [ $RC -eq 0 ] || { echo "ERROR: P1 slurm_run.sh exited $RC"; exit 1; }
    IDS=$(ids_from "$OUT")
    echo "P1 job ids: $IDS"
    [ -n "$IDS" ] || { echo "ERROR: no P1 job ids captured"; exit 1; }
    sbatch --partition=day -c1 --time=0:20:00 --mem=4G \
      --dependency=afterok:$IDS --kill-on-invalid-dep=yes \
      --job-name=bound-bridge --output="$LOGD/bridge_%j.log" \
      --wrap="bash $SELF p2"
    echo "submitted bridge (p2) gated afterok on P1"
    ;;
  p2)
    [ -f "$UNIFORM" ] || { echo "ERROR: missing $UNIFORM"; exit 1; }
    set_M "$UNIFORM"               # uniform / extreme diffusion
    OUT=$(bash slurm_run.sh tests/cgcarry_bound_uniform NULL user_test 1 cgcarry_bound_uniform 1 1 NULL 0 2>&1); RC=$?
    echo "$OUT"
    [ $RC -eq 0 ] || { echo "ERROR: P2 slurm_run.sh exited $RC"; exit 1; }
    IDS=$(ids_from "$OUT")
    echo "P2 job ids: $IDS"
    [ -n "$IDS" ] || { echo "ERROR: no P2 job ids captured"; exit 1; }
    sbatch --partition=day -c1 --time=0:10:00 --mem=4G \
      --dependency=afterany:$IDS \
      --job-name=bound-restore --output="$LOGD/restore_%j.log" \
      --wrap="bash $SELF restore"
    echo "submitted restore gated afterany on P2"
    ;;
  restore)
    if [ -f "$BAK" ]; then cp -f "$BAK" "$YAML"; rm -f "$BAK"; echo "  restored pristine yaml"; \
    else set_M "null"; echo "  backup missing; forced null"; fi
    echo "  $(grep -n '^transition_matrix_file:' "$YAML")"
    echo "=== BOUNDING SWEEP COMPLETE $(date) ==="
    ;;
  *) echo "unknown stage: $STAGE"; exit 1;;
esac
