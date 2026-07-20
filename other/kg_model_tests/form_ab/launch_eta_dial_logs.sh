#!/bin/bash
#-----------------------------------------------------------------------
# launch_eta_dial_logs.sh  --  Part B1 driver (pin eta_tilde for the logs form).
#
# Runs the eta_dial_repin.csv runscript (baseline + s_cg_r25, +5pp CG shock) at
# three trial eta_tilde values under KG_RESPONSE_FORM=logs. KG_ETA_LOGS is
# exported into the submitting shell so slurm_run.sh's sbatch phases inherit it
# (--export=ALL default; same precedent as the levels KG_ETA eta-dial). Full
# sample; delete_detail=1 keeps the conventional_no_wealth leg (the E_full
# numerator R_shock). measure_efull_logs.R pulls the form-INVARIANT base R and
# the shared-law dtau from the levels central vintage eta_dial_c_v2 (kept with
# full detail), so no logs-side baseline/conventional detail is needed here.
#
# After all three finish (Phase 2N/3b), measure + invert:
#   sbatch other/kg_model_tests/form_ab/measure_efull_logs.sbatch
#
# Usage (from repo root):  bash other/kg_model_tests/form_ab/launch_eta_dial_logs.sh
#-----------------------------------------------------------------------
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

# Trial grid straddling the expected eta_tilde* ~ 1.9 (net-of-tax elasticity
# matched to the same E_full = -2.52 local moment as the levels form). Each
# vintage runs its own baseline (NULL) so the three are independent and run
# concurrently -- the levels eta-dial precedent (eta_dial_*_v2). Each has its
# own baseline dir, which measure_efull_logs.R reads from the central _19.
PROVISIONAL_SHARE=0.2542   # see note above: long-run E_full is timeable-invariant
declare -A GRID=( [1.5]=eta_dial_logs_15 [1.9]=eta_dial_logs_19 [2.3]=eta_dial_logs_23 )

for eta in "${!GRID[@]}"; do
  vintage="${GRID[$eta]}"
  echo "launching logs eta-dial: eta_tilde=$eta -> vintage $vintage"
  KG_RESPONSE_FORM=logs KG_ETA_LOGS="$eta" KG_TIMEABLE_SHARE_LOGS="$PROVISIONAL_SHARE" \
    bash slurm_run.sh top_tax/eta_dial_repin NULL jar335 1 "$vintage" 1 0 NULL 1
done

echo ""
echo "3 logs eta-dial vintages launched. When all reach Phase 3b, run:"
echo "  sbatch other/kg_model_tests/form_ab/measure_efull_logs.sbatch"
