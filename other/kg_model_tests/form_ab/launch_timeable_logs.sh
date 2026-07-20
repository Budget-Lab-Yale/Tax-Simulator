#!/bin/bash
#-----------------------------------------------------------------------
# launch_timeable_logs.sh  --  Part B2 driver (pin timeable_share_logs).
#
# Runs the form_timeable runscript (baseline + delayed + rate_up_2pp) under
# KG_RESPONSE_FORM=logs at the pinned eta_tilde and one trial timeable share.
# Hand-iterate the share (start 0.2542) until measure_shortrun_logs.R reports
# E_full_short within ~5% of 5.04. eta_tilde must be pinned FIRST (B1); the
# long-run moment is timeable-invariant, so eta and the share identify
# sequentially.
#
# The first invocation runs the shared baseline; later shares reuse it via
# baseline_vintage to save a full baseline each iteration.
#
# Usage (from repo root):
#   bash .../launch_timeable_logs.sh <eta_tilde> <share> [<baseline_vintage>]
# e.g.  bash .../launch_timeable_logs.sh 1.92 0.2542
#       bash .../launch_timeable_logs.sh 1.92 0.30 form_tmbl_logs_0p2542
#-----------------------------------------------------------------------
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

ETA_TILDE="${1:?need eta_tilde (from B1)}"
SHARE="${2:?need trial timeable share}"
BASE_VINTAGE="${3:-NULL}"

tag=$(echo "$SHARE" | tr '.' 'p')
VINTAGE="form_tmbl_logs_${tag}"

echo "logs timeable pin: eta_tilde=$ETA_TILDE share=$SHARE -> vintage $VINTAGE (baseline_vintage=$BASE_VINTAGE)"
# delete_detail=0: measure_shortrun_logs.R reads the vintage's OWN baseline/
# static + delayed/conventional + rate_up_2pp/conventional detail; unlike the
# eta-dial there is no c_v2 equivalent for the delayed/rate_up legs.
KG_RESPONSE_FORM=logs KG_ETA_LOGS="$ETA_TILDE" KG_TIMEABLE_SHARE_LOGS="$SHARE" \
  bash slurm_run.sh tests/form_timeable NULL jar335 1 "$VINTAGE" 1 0 "$BASE_VINTAGE" 0

echo ""
echo "when it finishes, measure with:"
echo "  sbatch --wrap 'module load R/4.4.1-foss-2022b && Rscript other/kg_model_tests/form_ab/measure_shortrun_logs.R $VINTAGE' \\"
echo "    --partition=day -c1 --mem=32G --time=0:30:00 \\"
echo "    --output=/nfs/roberts/scratch/pi_nrs36/jar335/kg_form_scratch/shortrun_%j.out"
