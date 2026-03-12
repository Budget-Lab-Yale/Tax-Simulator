#!/bin/bash
#-----------------------------------------------------------------------
# slurm_run.sh
#
# Bash orchestrator for SLURM-based multi-node parallelization of
# Tax-Simulator. Submits a dependency chain of SLURM array jobs that
# mirror the main.R pipeline:
#
#   Phase 0  — Setup (login node): parse globals, serialize configs
#   Phase 1  — Baseline (SLURM array): 1 job per year
#   Phase 2  — Counterfactuals (SLURM array): 1 job per scenario×year
#   Phase 3a — Aggregation (SLURM array): 1 job per scenario
#   Phase 3b — Post-processing (SLURM array): 1 job per counterfactual
#   Phase 4  — Stacked (single SLURM job): stacked reports + cleanup
#
# Usage:
#   bash slurm_run.sh <runscript> <scenario_id> <user_id> <local>
#                     <vintage> <pct_sample> <stacked>
#                     <baseline_vintage> <delete_detail>
#
# Arguments are identical to main.R except multicore is omitted
# (parallelization is handled by SLURM).
#-----------------------------------------------------------------------

set -euo pipefail

# Validate arguments
if [ "$#" -lt 9 ]; then
  echo "Usage: bash slurm_run.sh <runscript> <scenario_id> <user_id> <local> <vintage> <pct_sample> <stacked> <baseline_vintage> <delete_detail>"
  exit 1
fi

module load R/4.4.1-foss-2022b

# Resolve repository root (directory containing this script)
REPO_DIR=$(cd "$(dirname "$0")" && pwd)

echo "=== Tax-Simulator SLURM Pipeline ==="
echo "Repository: ${REPO_DIR}"
echo "Arguments: $@"
echo ""


#-------------------------------------------
# Phase 0: Setup (synchronous on login node)
#-------------------------------------------

echo "Phase 0: Running setup..."
METADATA=$(cd "$REPO_DIR" && Rscript src/slurm/setup.R "$@")
eval "$METADATA"

echo "  Staging dir: ${STAGING_DIR}"
echo "  Baseline year-tasks (Phase 1): ${N_PHASE1}"
echo "  Counterfactual year-tasks (Phase 2): ${N_PHASE2}"
echo "  Counterfactual scenarios: ${N_SCENARIOS}"
echo "  Stacked: ${STACKED}"
echo ""

# Common sbatch flags
SBATCH_COMMON="--partition=day -c 1"


#-------------------------------------------
# Phase 1: Baseline (skip if N_PHASE1 == 0)
#-------------------------------------------

P1_DEP=""
if [ "$N_PHASE1" -gt 0 ]; then
  echo "Phase 1: Submitting ${N_PHASE1} baseline year jobs..."
  P1=$(sbatch --parsable --array=1-${N_PHASE1} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-baseline \
    --output="${STAGING_DIR}/logs/p1_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 1")
  echo "  Job ID: ${P1}"
  P1_DEP="--dependency=afterok:${P1}"
fi


#-------------------------------------------
# Phase 2: Counterfactuals (skip if none)
#-------------------------------------------

P2_DEP=""
if [ "$N_PHASE2" -gt 0 ]; then
  echo "Phase 2: Submitting ${N_PHASE2} counterfactual year jobs..."
  P2=$(sbatch --parsable --array=1-${N_PHASE2} ${P1_DEP} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-cf \
    --output="${STAGING_DIR}/logs/p2_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2")
  echo "  Job ID: ${P2}"
  P2_DEP="--dependency=afterok:${P2}"
fi


#-------------------------------------------
# Phase 3a: Aggregation (all scenarios)
#-------------------------------------------

# Count total scenarios for aggregation: baseline (if ran) + counterfactuals
N_AGG=0
if [ "$N_PHASE1" -gt 0 ]; then
  N_AGG=$((N_AGG + 1))
fi
N_AGG=$((N_AGG + N_SCENARIOS))

P3A_DEP=""
if [ "$N_AGG" -gt 0 ]; then

  # Combine dependencies from P1 and P2
  ALL_DEPS="${P1_DEP} ${P2_DEP}"

  echo "Phase 3a: Submitting ${N_AGG} aggregation jobs..."
  P3A=$(sbatch --parsable --array=1-${N_AGG} ${ALL_DEPS} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-agg \
    --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
  echo "  Job ID: ${P3A}"
  P3A_DEP="--dependency=afterok:${P3A}"


  #-------------------------------------------
  # Phase 3b: Post-processing (counterfactuals)
  #-------------------------------------------

  if [ "$N_SCENARIOS" -gt 0 ]; then
    echo "Phase 3b: Submitting ${N_SCENARIOS} post-processing jobs..."
    P3B=$(sbatch --parsable --array=1-${N_SCENARIOS} ${P3A_DEP} \
      ${SBATCH_COMMON} --time=1:00:00 --mem=16G \
      --job-name=taxsim-postproc \
      --output="${STAGING_DIR}/logs/p3b_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3b")
    echo "  Job ID: ${P3B}"


    #-------------------------------------------
    # Phase 4: Stacked (single job)
    #-------------------------------------------

    if [ "$STACKED" == "1" ]; then
      echo "Phase 4: Submitting stacked post-processing job..."
      P4=$(sbatch --parsable --dependency=afterok:${P3B} \
        ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
        --job-name=taxsim-stacked \
        --output="${STAGING_DIR}/logs/p4.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/aggregate.R ${STAGING_DIR} 4")
      echo "  Job ID: ${P4}"
    fi
  fi
fi

echo ""
echo "All jobs submitted. Monitor with: squeue -u $USER"
echo "Logs: ${STAGING_DIR}/logs/"
