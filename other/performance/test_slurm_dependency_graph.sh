#!/bin/bash
# Regression test for slurm_run.sh's submission graph. Replaces module,
# Rscript, and sbatch with local fakes; no cluster jobs or model work run.

set -euo pipefail

REPO_DIR=$(cd "$(dirname "$0")/../.." && pwd)
TEST_ROOT=$(mktemp -d)
MOCK_BIN="${TEST_ROOT}/bin"
mkdir -p "$MOCK_BIN"
trap 'rm -rf "$TEST_ROOT"' EXIT

cat > "${MOCK_BIN}/module" <<'EOF'
#!/bin/bash
exit 0
EOF

MOCK_BASH_ENV="${TEST_ROOT}/bash_env"
cat > "$MOCK_BASH_ENV" <<'EOF'
module () {
  return 0
}
EOF

cat > "${MOCK_BIN}/Rscript" <<'EOF'
#!/bin/bash
set -euo pipefail
mkdir -p "$MOCK_STAGING_DIR/logs"
cp "$MOCK_SUBMISSION_PLAN" "$MOCK_STAGING_DIR/submission_plan.tsv"
cat <<META
STAGING_DIR="$MOCK_STAGING_DIR"
N_PHASE1=$MOCK_N_PHASE1
N_PHASE1B=$MOCK_N_PHASE1B
N_PHASE2A=$MOCK_N_PHASE2A
N_PHASE2B=$MOCK_N_PHASE2B
N_PHASE2N=$MOCK_N_PHASE2N
N_PHASE2W=$MOCK_N_PHASE2W
N_PHASE2C=$MOCK_N_PHASE2C
N_SCENARIOS=$MOCK_N_SCENARIOS
STACKED=$MOCK_STACKED
META
EOF

cat > "${MOCK_BIN}/sbatch" <<'EOF'
#!/bin/bash
set -euo pipefail
job_id=$(<"$MOCK_COUNTER")
job_id=$((job_id + 1))
echo "$job_id" > "$MOCK_COUNTER"
{
  printf '%s' "$job_id"
  printf ' %s' "$@"
  printf '\n'
} >> "$MOCK_SBATCH_LOG"
echo "$job_id"
EOF

chmod +x "${MOCK_BIN}/module" "${MOCK_BIN}/Rscript" "${MOCK_BIN}/sbatch"

PLAN_HEADER=$'scenario\tphase1b_task\tphase2a_first\tphase2a_last\tphase2b_task\tphase2n_first\tphase2n_last\tphase2w_task\tphase2c_first\tphase2c_last\taggregate_task\tpostprocess_task'

assert_log () {
  local pattern="$1"
  if ! grep -F -- "$pattern" "$MOCK_SBATCH_LOG" >/dev/null; then
    echo "Missing submission pattern: $pattern" >&2
    cat "$MOCK_SBATCH_LOG" >&2
    exit 1
  fi
}

assert_no_log () {
  local pattern="$1"
  if grep -F -- "$pattern" "$MOCK_SBATCH_LOG" >/dev/null; then
    echo "Unexpected submission pattern: $pattern" >&2
    cat "$MOCK_SBATCH_LOG" >&2
    exit 1
  fi
}

run_launcher () {
  BASH_ENV="$MOCK_BASH_ENV" PATH="${MOCK_BIN}:${PATH}" \
    bash "${REPO_DIR}/slurm_run.sh" tests/mock NULL user_test 1 mock_vintage \
      1 "$MOCK_STACKED" "$1" "$2" >/dev/null
}

# Mixed run: one ordinary and one wealth scenario, with a newly run baseline.
MOCK_STAGING_DIR="${TEST_ROOT}/mixed_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/mixed_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/mixed_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/mixed_sbatch.log"
MOCK_N_PHASE1=2
MOCK_N_PHASE1B=2
MOCK_N_PHASE2A=4
MOCK_N_PHASE2B=2
MOCK_N_PHASE2N=2
MOCK_N_PHASE2W=1
MOCK_N_PHASE2C=4
MOCK_N_SCENARIOS=2
MOCK_STACKED=1
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_STACKED

{
  echo "$PLAN_HEADER"
  printf 'ordinary\t1\t1\t2\t1\tNA\tNA\tNA\t1\t2\t2\t1\n'
  printf 'wealth\t2\t3\t4\t2\t1\t2\t1\t3\t4\t3\t2\n'
} > "$MOCK_SUBMISSION_PLAN"
echo 1000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher NULL 0

assert_log "1002 --parsable --array=1-1 --dependency=afterok:1001"
assert_log "1004 --parsable --array=1-2 --dependency=afterok:1001:1003"
assert_log "1006 --parsable --array=1-2 --dependency=afterok:1005"
assert_log "1007 --parsable --array=2-2 --dependency=afterok:1006:1002"
assert_log "1010 --parsable --array=3-4 --dependency=afterok:1001:1009"
assert_log "1012 --parsable --array=1-2 --dependency=afterok:1011"
assert_log "1013 --parsable --array=1-1 --dependency=afterok:1012:1001"
assert_log "1014 --parsable --array=3-4 --dependency=afterok:1011:1013"
assert_log "1015 --parsable --array=3-3 --dependency=afterok:1014:1002"
assert_log "1017 --parsable --dependency=afterok:1008:1016"

# Supplied baseline: no baseline jobs or baseline dependency are submitted.
MOCK_STAGING_DIR="${TEST_ROOT}/supplied_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/supplied_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/supplied_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/supplied_sbatch.log"
MOCK_N_PHASE1=0
MOCK_N_PHASE1B=1
MOCK_N_PHASE2A=2
MOCK_N_PHASE2B=1
MOCK_N_PHASE2N=0
MOCK_N_PHASE2W=0
MOCK_N_PHASE2C=2
MOCK_N_SCENARIOS=1
MOCK_STACKED=0
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_STACKED

{
  echo "$PLAN_HEADER"
  printf 'ordinary\t1\t1\t2\t1\tNA\tNA\tNA\t1\t2\t1\t1\n'
} > "$MOCK_SUBMISSION_PLAN"
echo 2000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher existing_baseline 0

assert_no_log "taxsim-baseline"
assert_log "2002 --parsable --array=1-2 --dependency=afterok:2001"
assert_log "2005 --parsable --array=1-1 --dependency=afterok:2004"

# Baseline-only cleanup waits for baseline aggregation.
MOCK_STAGING_DIR="${TEST_ROOT}/baseline_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/baseline_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/baseline_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/baseline_sbatch.log"
MOCK_N_PHASE1=2
MOCK_N_PHASE1B=0
MOCK_N_PHASE2A=0
MOCK_N_PHASE2B=0
MOCK_N_PHASE2N=0
MOCK_N_PHASE2W=0
MOCK_N_PHASE2C=0
MOCK_N_SCENARIOS=0
MOCK_STACKED=0
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_STACKED

echo "$PLAN_HEADER" > "$MOCK_SUBMISSION_PLAN"
echo 3000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher NULL 1

assert_log "3002 --parsable --array=1-1 --dependency=afterok:3001"
assert_log "3003 --parsable --dependency=afterok:3002"

echo "SLURM dependency graph checks passed."
