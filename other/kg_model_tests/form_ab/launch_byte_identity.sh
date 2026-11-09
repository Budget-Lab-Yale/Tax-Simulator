#!/bin/bash
#-----------------------------------------------------------------------
# launch_byte_identity.sh  --  Part B0 driver.
#
# Sets up a git worktree at the PRE-toggle commit, then submits the PRE run
# (from the worktree) and the POST run (from this working tree), both under the
# levels form. After both finish, run:
#   bash other/simplify_cleanup/compare_smoke.sh \
#     <local_root>/model_data/Tax-Simulator/v1/form_byte_pre \
#     <local_root>/model_data/Tax-Simulator/v1/form_byte_post
# An empty diff proves the levels path is unchanged by the toggle.
#
# Usage (from repo root):  bash other/kg_model_tests/form_ab/launch_byte_identity.sh
#-----------------------------------------------------------------------
set -euo pipefail

REPO=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
PRE_SHA=fb73c5fc0                 # HEAD before the functional-form toggle
PRE_WT=/nfs/roberts/scratch/pi_nrs36/jar335/form_byte_pre_wt
LOCAL_ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1

cd "$REPO"

# Pre-change worktree (detached at PRE_SHA). Reuses an existing one if present.
if [ ! -d "$PRE_WT" ]; then
  echo "creating pre-change worktree at $PRE_SHA -> $PRE_WT"
  git worktree add --detach "$PRE_WT" "$PRE_SHA"
else
  echo "reusing existing worktree $PRE_WT"
fi

echo "submitting PRE (worktree) run -> vintage form_byte_pre"
JPRE=$(sbatch --parsable other/kg_model_tests/form_ab/byte_identity.sbatch \
              "$PRE_WT" form_byte_pre)
echo "  pre job: $JPRE"

echo "submitting POST (working tree) run -> vintage form_byte_post"
JPOST=$(sbatch --parsable other/kg_model_tests/form_ab/byte_identity.sbatch \
               "$REPO" form_byte_post)
echo "  post job: $JPOST"

echo ""
echo "when both finish, byte-diff with:"
echo "  bash other/simplify_cleanup/compare_smoke.sh \\"
echo "    $LOCAL_ROOT/form_byte_pre $LOCAL_ROOT/form_byte_post"
echo ""
echo "(remove the pre worktree afterwards: git worktree remove $PRE_WT)"
