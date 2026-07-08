#!/bin/bash
#-----------------------------------------------------------------------
# slurm_run.sh
#
# Bash orchestrator for SLURM-based multi-node parallelization of
# Tax-Simulator. Submits a dependency chain of SLURM array jobs that
# mirror the main.R pipeline:
#
#   Phase 0  — Setup (login node): parse globals, serialize configs
#   Phase 1  — Baseline (SLURM array): 1 job per year (static-only,
#              pass_type='both' since no behavior modules)
#   Phase 1B — CF frozen mechanical pass (SLURM array): 1 job per scenario,
#              runs the kg_dynamics frozen-realization recurrence (no
#              Bellman); writes mechanical state the Phase 2A static
#              workers inject; no-op for non-kg_dynamics scenarios
#   Phase 2A — CF static-only (SLURM array): 1 job per scenario×year
#              (pass_type='static'; produces static MTRs + static_totals)
#   Phase 2B — CF bathtub (SLURM array): 1 job per scenario, runs the
#              kg_dynamics recurrence sequentially across years; no-op
#              for non-kg_dynamics scenarios
#   Phase 2N — CF conv-no-wealth (SLURM array): 1 job per scenario×year, only
#              for s>0 wealth scenarios (pass_type='conventional_no_wealth';
#              produces ΔT⁰ ingredients + mtr_cap_bundle on the un-eroded base)
#   Phase 2W — CF wealth bathtub (SLURM array): 1 job per s>0 scenario, runs the
#              wealth deficit recurrence sequentially across years (reads 2N +
#              baseline detail; writes the state Phase 2C applies)
#   Phase 2C — CF conventional-only (SLURM array): 1 job per scenario×year
#              (pass_type='conventional'; reads precomputed bathtub state, the
#              Phase 2A static MTRs, and the wealth deficit state from 2W)
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
echo "  CF frozen mechanical jobs (Phase 1B): ${N_PHASE1B}"
echo "  CF static-only year-tasks (Phase 2A): ${N_PHASE2A}"
echo "  CF bathtub jobs (Phase 2B): ${N_PHASE2B}"
echo "  CF conv-no-wealth year-tasks (Phase 2N): ${N_PHASE2N}"
echo "  CF wealth bathtub jobs (Phase 2W): ${N_PHASE2W}"
echo "  CF conventional-only year-tasks (Phase 2C): ${N_PHASE2C}"
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
# Phase 1B: CF frozen mechanical pass (one job per CF; no dependencies —
# needs only Tax-Data and the staged tax law, so it runs alongside Phase 1)
#-------------------------------------------

P1B_ID=""
if [ "$N_PHASE1B" -gt 0 ]; then
  echo "Phase 1B: Submitting ${N_PHASE1B} CF frozen mechanical jobs..."
  P1B=$(sbatch --parsable --array=1-${N_PHASE1B} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-frozen \
    --output="${STAGING_DIR}/logs/p1b_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/frozen.R ${STAGING_DIR}")
  echo "  Job ID: ${P1B}"
  P1B_ID="${P1B}"
fi


#-------------------------------------------
# Phase 2A: CF static-only year tasks (depends on Phase 1 baseline AND
# Phase 1B frozen state — static workers inject the mechanical state)
#-------------------------------------------

P2A_PREREQS=""
if [ "$N_PHASE1" -gt 0 ]; then
  P2A_PREREQS="${P2A_PREREQS}:${P1}"
fi
if [ -n "$P1B_ID" ]; then
  P2A_PREREQS="${P2A_PREREQS}:${P1B_ID}"
fi
P2A_PRE_DEP=""
if [ -n "$P2A_PREREQS" ]; then
  P2A_PRE_DEP="--dependency=afterok${P2A_PREREQS}"
fi

P2A_DEP=""
if [ "$N_PHASE2A" -gt 0 ]; then
  echo "Phase 2A: Submitting ${N_PHASE2A} CF static-only year jobs..."
  P2A=$(sbatch --parsable --array=1-${N_PHASE2A} ${P2A_PRE_DEP} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-cf-static \
    --output="${STAGING_DIR}/logs/p2a_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2A")
  echo "  Job ID: ${P2A}"
  P2A_DEP="--dependency=afterok:${P2A}"
fi


#-------------------------------------------
# Phase 2B: CF bathtub pre-pass (one job per CF; sequential within job)
#-------------------------------------------

P2B_DEP=""
if [ "$N_PHASE2B" -gt 0 ]; then
  # Bathtub depends on Phase 1 (baseline cells from Tax-Data + tax_law) and
  # in v2 will additionally depend on Phase 2A (cell MTRs). For v1 the 2A
  # dependency is harmless and keeps the DAG monotone.
  echo "Phase 2B: Submitting ${N_PHASE2B} CF bathtub jobs..."
  # 30 min (was 15): sigma-conversion scenarios add per-year raw Tax-Data +
  # detail reads and the tau_eq recursion inside the bathtub pass.
  P2B=$(sbatch --parsable --array=1-${N_PHASE2B} ${P2A_DEP} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
    --job-name=taxsim-bathtub \
    --output="${STAGING_DIR}/logs/p2b_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/bathtub.R ${STAGING_DIR}")
  echo "  Job ID: ${P2B}"
  P2B_DEP="--dependency=afterok:${P2B}"
fi


#-------------------------------------------
# Phase 2N: CF conv-no-wealth year tasks (only s>0 wealth scenarios). Depends on
# Phase 2B (kg bathtub state, when both channels are active) — which
# transitively covers Phase 2A's static MTRs the behavior modules read.
#-------------------------------------------

P2N_ID=""
if [ "$N_PHASE2N" -gt 0 ]; then
  echo "Phase 2N: Submitting ${N_PHASE2N} CF conv-no-wealth year jobs..."
  P2N=$(sbatch --parsable --array=1-${N_PHASE2N} ${P2B_DEP} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-cf-convnw \
    --output="${STAGING_DIR}/logs/p2n_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2N")
  echo "  Job ID: ${P2N}"
  P2N_ID="${P2N}"
fi


#-------------------------------------------
# Phase 2W: CF wealth bathtub pre-pass (one job per s>0 scenario; sequential
# recurrence within the job). Depends on Phase 2N (conv-no-wealth detail) AND
# Phase 1 (baseline static detail, the ΔT⁰ baseline leg) when the baseline ran.
#-------------------------------------------

P2W_ID=""
if [ "$N_PHASE2W" -gt 0 ]; then
  P2W_PREREQS=""
  if [ -n "$P2N_ID" ]; then
    P2W_PREREQS="${P2W_PREREQS}:${P2N_ID}"
  fi
  if [ "$N_PHASE1" -gt 0 ]; then
    P2W_PREREQS="${P2W_PREREQS}:${P1}"
  fi
  P2W_PRE_DEP=""
  if [ -n "$P2W_PREREQS" ]; then
    P2W_PRE_DEP="--dependency=afterok${P2W_PREREQS}"
  fi

  echo "Phase 2W: Submitting ${N_PHASE2W} CF wealth bathtub jobs..."
  P2W=$(sbatch --parsable --array=1-${N_PHASE2W} ${P2W_PRE_DEP} \
    ${SBATCH_COMMON} --time=0:15:00 --mem=8G \
    --job-name=taxsim-wealth \
    --output="${STAGING_DIR}/logs/p2w_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/wealth.R ${STAGING_DIR}")
  echo "  Job ID: ${P2W}"
  P2W_ID="${P2W}"
fi


#-------------------------------------------
# Phase 2C: CF conventional-only year tasks. Depends on the kg bathtub (2B) AND
# the wealth deficit state (2W, when present). 2W transitively covers 2N -> 2B,
# so :${P2B} + :${P2W} gates the whole upstream DAG.
#-------------------------------------------

P2C_PREREQS=""
if [ "$N_PHASE2B" -gt 0 ]; then
  P2C_PREREQS="${P2C_PREREQS}:${P2B}"
fi
if [ -n "$P2W_ID" ]; then
  P2C_PREREQS="${P2C_PREREQS}:${P2W_ID}"
fi
P2C_PRE_DEP=""
if [ -n "$P2C_PREREQS" ]; then
  P2C_PRE_DEP="--dependency=afterok${P2C_PREREQS}"
fi

P2C_DEP=""
if [ "$N_PHASE2C" -gt 0 ]; then
  echo "Phase 2C: Submitting ${N_PHASE2C} CF conventional-only year jobs..."
  P2C=$(sbatch --parsable --array=1-${N_PHASE2C} ${P2C_PRE_DEP} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-cf-conv \
    --output="${STAGING_DIR}/logs/p2c_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2C")
  echo "  Job ID: ${P2C}"
  P2C_DEP="--dependency=afterok:${P2C}"
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

  # Combine dependencies from Phase 1 (baseline) and Phase 2C (conv outputs).
  # Phase 2A and 2B feed into 2C, so transitively they're already gated.
  ALL_DEPS="${P1_DEP} ${P2C_DEP}"

  # The baseline aggregation (array task 1) runs BEFORE the counterfactual
  # aggregations: scenario receipts read the baseline's totals/estate.csv,
  # and racing it in one parallel array makes them fall back to rebuilding
  # the series from detail CSVs -- last-bit float drift vs main.R (detail
  # files carry 15 significant digits, not full doubles)
  SCEN_DEPS="${ALL_DEPS}"
  if [ "$N_PHASE1" -gt 0 ]; then
    echo "Phase 3a: Submitting baseline aggregation job..."
    P3A_BASE=$(sbatch --parsable --array=1-1 ${ALL_DEPS} \
      ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
      --job-name=taxsim-agg \
      --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
    echo "  Job ID: ${P3A_BASE}"
    SCEN_DEPS="--dependency=afterok:${P3A_BASE}"
    P3A_DEP="--dependency=afterok:${P3A_BASE}"
  fi

  if [ "$N_SCENARIOS" -gt 0 ]; then
    FIRST_SCEN_TASK=$((N_AGG - N_SCENARIOS + 1))
    echo "Phase 3a: Submitting ${N_SCENARIOS} counterfactual aggregation jobs..."
    P3A=$(sbatch --parsable --array=${FIRST_SCEN_TASK}-${N_AGG} ${SCEN_DEPS} \
      ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
      --job-name=taxsim-agg \
      --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
    echo "  Job ID: ${P3A}"
    P3A_DEP="--dependency=afterok:${P3A}"
  fi


  #-------------------------------------------
  # Phase 3b: Post-processing (counterfactuals)
  #-------------------------------------------

  P4_DEP="${P3A_DEP}"
  if [ "$N_SCENARIOS" -gt 0 ]; then
    echo "Phase 3b: Submitting ${N_SCENARIOS} post-processing jobs..."
    P3B=$(sbatch --parsable --array=1-${N_SCENARIOS} ${P3A_DEP} \
      ${SBATCH_COMMON} --time=1:00:00 --mem=16G \
      --job-name=taxsim-postproc \
      --output="${STAGING_DIR}/logs/p3b_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3b")
    echo "  Job ID: ${P3B}"
    P4_DEP="--dependency=afterok:${P3B}"
  fi


  #-------------------------------------------
  # Phase 4: Stacked + optional detail purge (single job)
  #-------------------------------------------

  # Submitted whenever there is stacked work OR a detail purge to do --
  # aggregate.R gates each internally on the runtime args. main.R runs
  # purge_detail() regardless of stacked, so gating Phase 4 on STACKED alone
  # would silently leave detail files on disk when delete_detail=1, stacked=0.
  DELETE_DETAIL="${9}"
  if [ "$STACKED" == "1" ] || [ "$DELETE_DETAIL" == "1" ]; then
    echo "Phase 4: Submitting stacked/cleanup post-processing job..."
    P4=$(sbatch --parsable ${P4_DEP} \
      ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
      --job-name=taxsim-stacked \
      --output="${STAGING_DIR}/logs/p4.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 4")
    echo "  Job ID: ${P4}"
  fi
fi

echo ""
echo "All jobs submitted. Monitor with: squeue -u $USER"
echo "Logs: ${STAGING_DIR}/logs/"
