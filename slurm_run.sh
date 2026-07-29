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
#   Phase 2MN— CF mech-no-wealth (SLURM array): 1 job per scenario×year, only
#              for s>0 wealth scenarios that also run the mechanical rung
#              (pass_type='mechanical_no_wealth'; measures the mechanical rung's
#              drawdown forcing on the un-eroded base)
#   Phase 2MW— CF mechanical wealth bathtub (SLURM array): 1 job per such
#              scenario, runs the deficit recurrence sequentially across years
#   Phase 2M — CF mechanical (SLURM array): 1 job per scenario×year, only for
#              scenarios with a transmission channel live (corporate incidence,
#              a nonzero wealth financing profile, or an employer-payroll
#              reform). pass_type='mechanical'
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
#   bash slurm_run.sh <runscript> <scenario_id> <local> <vintage>
#                     <pct_sample> <stacked> <baseline_vintage>
#                     <delete_detail> [submit_mode] [years_per_task]
#
# Arguments are identical to main.R except multicore is omitted
# (parallelization is handled by SLURM). The user_id argument was retired
# 2026-07-25 (it was never read); remove it from old invocations.
#
# submit_mode chooses the submission shape:
#   chains (default) : one dependency chain per scenario, so a fast scenario
#                      reaches post-processing without waiting for unrelated
#                      work. Costs eight sbatch calls per scenario, and the
#                      cluster refuses submissions beyond 200 per hour.
#   batch            : one array per phase spanning every scenario, with a
#                      barrier between phases. About ten sbatch calls for the
#                      whole runscript regardless of size. Use for large
#                      homogeneous batches; the barriers cost little there.
#
# years_per_task is how many consecutive years one array task of a per-year
# phase runs, defaulting to 1. Each task is its own R process and pays a fixed
# startup toll before any calculation, so batching years amortizes that toll
# over several years. It is a scheduling choice and does not change results.
# The pre-passes (1B, 2B, 2MW, 2W) already run all years in one job.
#-----------------------------------------------------------------------

set -euo pipefail

# Validate arguments
if [ "$#" -ge 9 ] && [ "$9" != "chains" ] && [ "$9" != "batch" ]; then
  echo "Got a ninth argument that is not a submit_mode -- the user_id argument was retired 2026-07-25; remove it (old position 3)."
  echo "(The ninth argument is read only as submit_mode: 'chains' or 'batch'.)"
  exit 1
fi
if [ "$#" -lt 8 ] || [ "$#" -gt 10 ]; then
  echo "Usage: bash slurm_run.sh <runscript> <scenario_id> <local> <vintage> <pct_sample> <stacked> <baseline_vintage> <delete_detail> [chains|batch] [years_per_task]"
  exit 1
fi
SUBMIT_MODE="${9:-chains}"
YEARS_PER_TASK="${10:-1}"
if ! [[ "$YEARS_PER_TASK" =~ ^[1-9][0-9]*$ ]]; then
  echo "years_per_task must be a positive integer, got '${YEARS_PER_TASK}'."
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
# Setup takes the eight model arguments plus the batch size; submit_mode is this
# script's alone.
METADATA=$(cd "$REPO_DIR" && Rscript src/slurm/setup.R "${@:1:8}" "$YEARS_PER_TASK")
eval "$METADATA"

echo "  Staging dir: ${STAGING_DIR}"
echo "  Years per task: ${YEARS_PER_TASK}"
echo "  Baseline year-tasks (Phase 1): ${N_PHASE1}"
echo "  CF frozen mechanical jobs (Phase 1B): ${N_PHASE1B}"
echo "  CF static-only year-tasks (Phase 2A): ${N_PHASE2A}"
echo "  CF mech-no-wealth year-tasks (Phase 2MN): ${N_PHASE2MN}"
echo "  CF mechanical wealth bathtub jobs (Phase 2MW): ${N_PHASE2MW}"
echo "  CF mechanical year-tasks (Phase 2M): ${N_PHASE2M}"
echo "  CF bathtub jobs (Phase 2B): ${N_PHASE2B}"
echo "  CF conv-no-wealth year-tasks (Phase 2N): ${N_PHASE2N}"
echo "  CF wealth bathtub jobs (Phase 2W): ${N_PHASE2W}"
echo "  CF conventional-only year-tasks (Phase 2C): ${N_PHASE2C}"
echo "  Counterfactual scenarios: ${N_SCENARIOS}"
echo "  Stacked: ${STACKED}"
echo ""

# Common sbatch flags. Year workers get two cores for the independent MTR
# recomputes; sequential pre/post-processing jobs remain single-core.
SBATCH_COMMON="--partition=day -c 1"
SBATCH_YEAR="--partition=day -c 2"

# A batched year-task does the same per-year work several times over, so the
# per-year phases ask for the walltime multiplied by the batch size. At a batch
# size of one this is the half hour it has always been.
YEAR_MINUTES=$(( 30 * YEARS_PER_TASK ))
YEAR_TIME=$(printf '%d:%02d:00' $(( YEAR_MINUTES / 60 )) $(( YEAR_MINUTES % 60 )))

# Populate AFTEROK with either one dependency argument or no arguments. Keeping
# this as an array avoids word-splitting bugs when a phase has no prerequisites.
set_afterok () {
  local ids=()
  local job_id
  local joined

  for job_id in "$@"; do
    if [ -n "$job_id" ]; then
      ids+=("$job_id")
    fi
  done

  AFTEROK=()
  if [ "${#ids[@]}" -gt 0 ]; then
    joined=$(IFS=:; echo "${ids[*]}")
    AFTEROK=("--dependency=afterok:${joined}")
  fi
}


#-------------------------------------------
# Phase 1: Baseline (skip if N_PHASE1 == 0)
#-------------------------------------------

P1=""
if [ "$N_PHASE1" -gt 0 ]; then
  echo "Phase 1: Submitting ${N_PHASE1} baseline year jobs..."
  P1=$(sbatch --parsable --array=1-${N_PHASE1} \
    ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
    --job-name=taxsim-baseline \
    --output="${STAGING_DIR}/logs/p1_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 1")
  echo "  Job ID: ${P1}"
fi


#-------------------------------------------
# Baseline aggregation: shared full-precision prerequisite for every
# counterfactual aggregation. It can run as soon as the baseline year array
# finishes, independently of all counterfactual work.
#-------------------------------------------

P3A_BASE=""
if [ "$N_PHASE1" -gt 0 ]; then
  set_afterok "$P1"
  echo "Phase 3a: Submitting baseline aggregation job..."
  P3A_BASE=$(sbatch --parsable --array=1-1 "${AFTEROK[@]}" \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-agg \
    --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
  echo "  Job ID: ${P3A_BASE}"
fi


#-------------------------------------------
# Counterfactual chains
#
# Phase 0 emits the existing global manifest indices belonging to each
# scenario. Submit one dependency chain per scenario so a fast scenario can
# reach aggregation and post-processing without waiting for unrelated work:
#
#   1B -> 2A -> [2MN -> 2MW] -> [2M] -> 2B -> [2N -> 2W] -> 2C -> 3a -> 3b
#
# Phase 1 and baseline aggregation remain shared prerequisites where required.
#-------------------------------------------

SUBMISSION_PLAN="${STAGING_DIR}/submission_plan.tsv"
if [ ! -f "$SUBMISSION_PLAN" ]; then
  echo "ERROR: missing Phase 0 submission plan: ${SUBMISSION_PLAN}" >&2
  exit 1
fi

P3B_IDS=()

#-------------------------------------------
# Batch mode: one array per phase spanning every scenario, a barrier between
# phases. Task ranges come from the submission plan's columns, so the manifest
# numbering stays the single source of truth.
#-------------------------------------------
if [ "$SUBMIT_MODE" == "batch" ]; then

  # min-max of a plan column, NA rows skipped; empty when the column is all NA.
  plan_range () {
    awk -F'\t' -v lo_col="$1" -v hi_col="$2" 'NR > 1 && $lo_col != "NA" {
      if (lo == "" || $lo_col < lo) lo = $lo_col
      if (hi == "" || $hi_col > hi) hi = $hi_col
    } END { if (lo != "") print lo "-" hi }' "$SUBMISSION_PLAN"
  }

  R1B=$(plan_range 2 2);  R2A=$(plan_range 3 4);  R2B=$(plan_range 5 5)
  R2N=$(plan_range 6 7);  R2W=$(plan_range 8 8);  R2C=$(plan_range 9 10)
  R3A=$(plan_range 11 11); R3B=$(plan_range 12 12)
  # Appended plan columns, so the indices above stay put
  R2MN=$(plan_range 13 14); R2MW=$(plan_range 15 15); R2M=$(plan_range 16 17)

  echo "Batch mode: submitting one array per phase..."

  P1B=$(sbatch --parsable --array=${R1B} \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-frozen \
    --output="${STAGING_DIR}/logs/p1b_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/frozen.R ${STAGING_DIR}")
  echo "  Phase 1B job ID: ${P1B} (tasks ${R1B})"

  set_afterok "$P1" "$P1B"
  P2A=$(sbatch --parsable --array=${R2A} "${AFTEROK[@]}" \
    ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
    --job-name=taxsim-cf-static \
    --output="${STAGING_DIR}/logs/p2a_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2A")
  echo "  Phase 2A job ID: ${P2A} (tasks ${R2A})"

  P2MW=""
  if [ -n "$R2MN" ]; then
    set_afterok "$P2A"
    P2MN=$(sbatch --parsable --array=${R2MN} "${AFTEROK[@]}" \
      ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
      --job-name=taxsim-cf-mechnw \
      --output="${STAGING_DIR}/logs/p2mn_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/worker.R ${STAGING_DIR} 2MN")
    echo "  Phase 2MN job ID: ${P2MN} (tasks ${R2MN})"

    set_afterok "$P2MN" "$P1"
    P2MW=$(sbatch --parsable --array=${R2MW} "${AFTEROK[@]}" \
      ${SBATCH_COMMON} --time=0:15:00 --mem=8G \
      --job-name=taxsim-mech-wealth \
      --output="${STAGING_DIR}/logs/p2mw_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/mech_wealth.R ${STAGING_DIR}")
    echo "  Phase 2MW job ID: ${P2MW} (tasks ${R2MW})"
  fi

  P2M=""
  if [ -n "$R2M" ]; then
    set_afterok "$P2A" "$P2MW"
    P2M=$(sbatch --parsable --array=${R2M} "${AFTEROK[@]}" \
      ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
      --job-name=taxsim-cf-mech \
      --output="${STAGING_DIR}/logs/p2m_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/worker.R ${STAGING_DIR} 2M")
    echo "  Phase 2M job ID: ${P2M} (tasks ${R2M})"
  fi

  set_afterok "$P2A" "$P2M"
  P2B=$(sbatch --parsable --array=${R2B} "${AFTEROK[@]}" \
    ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
    --job-name=taxsim-bathtub \
    --output="${STAGING_DIR}/logs/p2b_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/bathtub.R ${STAGING_DIR}")
  echo "  Phase 2B job ID: ${P2B} (tasks ${R2B})"

  P2W=""
  if [ -n "$R2N" ]; then
    set_afterok "$P2B"
    P2N=$(sbatch --parsable --array=${R2N} "${AFTEROK[@]}" \
      ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
      --job-name=taxsim-cf-convnw \
      --output="${STAGING_DIR}/logs/p2n_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/worker.R ${STAGING_DIR} 2N")
    echo "  Phase 2N job ID: ${P2N} (tasks ${R2N})"

    set_afterok "$P2N" "$P1"
    P2W=$(sbatch --parsable --array=${R2W} "${AFTEROK[@]}" \
      ${SBATCH_COMMON} --time=0:15:00 --mem=8G \
      --job-name=taxsim-wealth \
      --output="${STAGING_DIR}/logs/p2w_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/wealth.R ${STAGING_DIR}")
    echo "  Phase 2W job ID: ${P2W} (tasks ${R2W})"
  fi

  set_afterok "$P2B" "$P2W"
  P2C=$(sbatch --parsable --array=${R2C} "${AFTEROK[@]}" \
    ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
    --job-name=taxsim-cf-conv \
    --output="${STAGING_DIR}/logs/p2c_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/worker.R ${STAGING_DIR} 2C")
  echo "  Phase 2C job ID: ${P2C} (tasks ${R2C})"

  set_afterok "$P2C" "$P3A_BASE"
  P3A=$(sbatch --parsable --array=${R3A} "${AFTEROK[@]}" \
    ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
    --job-name=taxsim-agg \
    --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
  echo "  Phase 3a job ID: ${P3A} (tasks ${R3A})"

  set_afterok "$P3A"
  P3B=$(sbatch --parsable --array=${R3B} "${AFTEROK[@]}" \
    ${SBATCH_COMMON} --time=1:00:00 --mem=16G \
    --job-name=taxsim-postproc \
    --output="${STAGING_DIR}/logs/p3b_%A_%a.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/aggregate.R ${STAGING_DIR} 3b")
  echo "  Phase 3b job ID: ${P3B} (tasks ${R3B})"
  P3B_IDS+=("$P3B")

else
{
  # Discard the TSV header. NA placeholders keep every row field-aligned.
  IFS= read -r _

  while IFS=$'\t' read -r \
      SCENARIO P1B_TASK P2A_FIRST P2A_LAST P2B_TASK \
      P2N_FIRST P2N_LAST P2W_TASK P2C_FIRST P2C_LAST \
      P3A_TASK P3B_TASK \
      P2MN_FIRST P2MN_LAST P2MW_TASK P2M_FIRST P2M_LAST; do

    if [ -z "$SCENARIO" ]; then
      continue
    fi

    echo "Scenario ${SCENARIO}: submitting independent chain..."

    # Phase 1B needs only Tax-Data and staged law, so it runs alongside Phase 1.
    P1B=$(sbatch --parsable --array=${P1B_TASK}-${P1B_TASK} \
      ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
      --job-name=taxsim-frozen \
      --output="${STAGING_DIR}/logs/p1b_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/frozen.R ${STAGING_DIR}")
    echo "  Phase 1B job ID: ${P1B}"

    # Static workers consume both the baseline detail/MTRs and this scenario's
    # frozen mechanical state.
    set_afterok "$P1" "$P1B"
    P2A=$(sbatch --parsable --array=${P2A_FIRST}-${P2A_LAST} "${AFTEROK[@]}" \
      ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
      --job-name=taxsim-cf-static \
      --output="${STAGING_DIR}/logs/p2a_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/worker.R ${STAGING_DIR} 2A")
    echo "  Phase 2A job ID: ${P2A}"

    # The mechanical rung's own drawdown forcing, where the wealth channel is on.
    P2MW=""
    if [ "$P2MN_FIRST" != "NA" ]; then
      set_afterok "$P2A"
      P2MN=$(sbatch --parsable --array=${P2MN_FIRST}-${P2MN_LAST} "${AFTEROK[@]}" \
        ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
        --job-name=taxsim-cf-mechnw \
        --output="${STAGING_DIR}/logs/p2mn_%A_%a.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/worker.R ${STAGING_DIR} 2MN")
      echo "  Phase 2MN job ID: ${P2MN}"

      set_afterok "$P2MN" "$P1"
      P2MW=$(sbatch --parsable --array=${P2MW_TASK}-${P2MW_TASK} "${AFTEROK[@]}" \
        ${SBATCH_COMMON} --time=0:15:00 --mem=8G \
        --job-name=taxsim-mech-wealth \
        --output="${STAGING_DIR}/logs/p2mw_%A_%a.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/mech_wealth.R ${STAGING_DIR}")
      echo "  Phase 2MW job ID: ${P2MW}"
    fi

    # The mechanical rung, where a transmission channel is live.
    P2M=""
    if [ "$P2M_FIRST" != "NA" ]; then
      set_afterok "$P2A" "$P2MW"
      P2M=$(sbatch --parsable --array=${P2M_FIRST}-${P2M_LAST} "${AFTEROK[@]}" \
        ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
        --job-name=taxsim-cf-mech \
        --output="${STAGING_DIR}/logs/p2m_%A_%a.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/worker.R ${STAGING_DIR} 2M")
      echo "  Phase 2M job ID: ${P2M}"
    fi

    # The sequential kg recurrence needs every static year for this scenario, and
    # after the MTR relocation it reads the mechanical frame.
    set_afterok "$P2A" "$P2M"
    P2B=$(sbatch --parsable --array=${P2B_TASK}-${P2B_TASK} "${AFTEROK[@]}" \
      ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
      --job-name=taxsim-bathtub \
      --output="${STAGING_DIR}/logs/p2b_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/bathtub.R ${STAGING_DIR}")
    echo "  Phase 2B job ID: ${P2B}"

    P2W=""
    if [ "$P2N_FIRST" != "NA" ]; then
      set_afterok "$P2B"
      P2N=$(sbatch --parsable --array=${P2N_FIRST}-${P2N_LAST} "${AFTEROK[@]}" \
        ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
        --job-name=taxsim-cf-convnw \
        --output="${STAGING_DIR}/logs/p2n_%A_%a.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/worker.R ${STAGING_DIR} 2N")
      echo "  Phase 2N job ID: ${P2N}"

      # Wealth recurrence reads all conv-no-wealth years and baseline detail.
      set_afterok "$P2N" "$P1"
      P2W=$(sbatch --parsable --array=${P2W_TASK}-${P2W_TASK} "${AFTEROK[@]}" \
        ${SBATCH_COMMON} --time=0:15:00 --mem=8G \
        --job-name=taxsim-wealth \
        --output="${STAGING_DIR}/logs/p2w_%A_%a.log" \
        --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
                Rscript src/slurm/wealth.R ${STAGING_DIR}")
      echo "  Phase 2W job ID: ${P2W}"
    fi

    set_afterok "$P2B" "$P2W"
    P2C=$(sbatch --parsable --array=${P2C_FIRST}-${P2C_LAST} "${AFTEROK[@]}" \
      ${SBATCH_YEAR} --time=${YEAR_TIME} --mem=24G \
      --job-name=taxsim-cf-conv \
      --output="${STAGING_DIR}/logs/p2c_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/worker.R ${STAGING_DIR} 2C")
    echo "  Phase 2C job ID: ${P2C}"

    # Counterfactual receipts require the baseline aggregation's full-precision
    # totals. With a supplied baseline vintage, P3A_BASE is empty because those
    # outputs already exist.
    set_afterok "$P2C" "$P3A_BASE"
    P3A=$(sbatch --parsable --array=${P3A_TASK}-${P3A_TASK} "${AFTEROK[@]}" \
      ${SBATCH_COMMON} --time=0:30:00 --mem=16G \
      --job-name=taxsim-agg \
      --output="${STAGING_DIR}/logs/p3a_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3a")
    echo "  Phase 3a job ID: ${P3A}"

    set_afterok "$P3A"
    P3B=$(sbatch --parsable --array=${P3B_TASK}-${P3B_TASK} "${AFTEROK[@]}" \
      ${SBATCH_COMMON} --time=1:00:00 --mem=16G \
      --job-name=taxsim-postproc \
      --output="${STAGING_DIR}/logs/p3b_%A_%a.log" \
      --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
              Rscript src/slurm/aggregate.R ${STAGING_DIR} 3b")
    echo "  Phase 3b job ID: ${P3B}"
    P3B_IDS+=("$P3B")
  done
} < "$SUBMISSION_PLAN"
fi


#-------------------------------------------
# Phase 4: Stacked + optional detail purge
#-------------------------------------------

# This is deliberately the one all-scenario success barrier. A failed scenario
# does not stop unrelated post-processing, but stacked output and destructive
# detail cleanup must not run on a partial vintage.
DELETE_DETAIL="${8}"
if [ "$STACKED" == "1" ] || [ "$DELETE_DETAIL" == "1" ]; then
  if [ "${#P3B_IDS[@]}" -gt 0 ]; then
    set_afterok "${P3B_IDS[@]}"
  else
    set_afterok "$P3A_BASE"
  fi

  echo "Phase 4: Submitting stacked/cleanup post-processing job..."
  P4=$(sbatch --parsable "${AFTEROK[@]}" \
    ${SBATCH_COMMON} --time=0:30:00 --mem=8G \
    --job-name=taxsim-stacked \
    --output="${STAGING_DIR}/logs/p4.log" \
    --wrap="cd ${REPO_DIR} && module load R/4.4.1-foss-2022b && \
            Rscript src/slurm/aggregate.R ${STAGING_DIR} 4")
  echo "  Job ID: ${P4}"
fi

echo ""
echo "All jobs submitted. Monitor with: squeue -u $USER"
echo "Logs: ${STAGING_DIR}/logs/"
