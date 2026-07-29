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
N_PHASE2MN=$MOCK_N_PHASE2MN
N_PHASE2MW=$MOCK_N_PHASE2MW
N_PHASE2M=$MOCK_N_PHASE2M
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

PLAN_HEADER=$'scenario\tphase1b_task\tphase2a_first\tphase2a_last\tphase2b_task\tphase2n_first\tphase2n_last\tphase2w_task\tphase2c_first\tphase2c_last\taggregate_task\tpostprocess_task\tphase2mn_first\tphase2mn_last\tphase2mw_task\tphase2m_first\tphase2m_last'

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
    bash "${REPO_DIR}/slurm_run.sh" tests/mock NULL 1 mock_vintage \
      1 "$MOCK_STACKED" "$1" "$2" ${3:+"$3"}
}

# Mixed run: an ordinary scenario with no transmission channel, one with the
# mechanical rung but no wealth drawdown, and one with both, plus a newly run
# baseline.
MOCK_STAGING_DIR="${TEST_ROOT}/mixed_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/mixed_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/mixed_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/mixed_sbatch.log"
MOCK_N_PHASE1=2
MOCK_N_PHASE1B=3
MOCK_N_PHASE2A=6
MOCK_N_PHASE2MN=2
MOCK_N_PHASE2MW=1
MOCK_N_PHASE2M=4
MOCK_N_PHASE2B=3
MOCK_N_PHASE2N=2
MOCK_N_PHASE2W=1
MOCK_N_PHASE2C=6
MOCK_N_SCENARIOS=3
MOCK_STACKED=1
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_N_PHASE2MN MOCK_N_PHASE2MW MOCK_N_PHASE2M
export MOCK_STACKED

{
  echo "$PLAN_HEADER"
  printf 'ordinary\t1\t1\t2\t1\tNA\tNA\tNA\t1\t2\t2\t1\tNA\tNA\tNA\tNA\tNA\n'
  printf 'corp\t2\t3\t4\t2\tNA\tNA\tNA\t3\t4\t3\t2\tNA\tNA\tNA\t1\t2\n'
  printf 'wealth\t3\t5\t6\t3\t1\t2\t1\t5\t6\t4\t3\t1\t2\t1\t3\t4\n'
} > "$MOCK_SUBMISSION_PLAN"
echo 1000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher NULL 0

# Baseline aggregation follows the baseline year array.
assert_log "1002 --parsable --array=1-1 --dependency=afterok:1001"

# Scenario with no transmission channel: 1B -> 2A -> 2B -> 2C -> 3a -> 3b, and
# 2B carries no mechanical prerequisite.
assert_log "1004 --parsable --array=1-2 --dependency=afterok:1001:1003"
assert_log "1005 --parsable --array=1-1 --dependency=afterok:1004"
assert_log "1006 --parsable --array=1-2 --dependency=afterok:1005"
assert_log "1007 --parsable --array=2-2 --dependency=afterok:1006:1002"

# Mechanical rung without the wealth drawdown: 2M waits on 2A alone, and 2B
# waits on both.
assert_log "1010 --parsable --array=3-4 --dependency=afterok:1001:1009"
assert_log "1011 --parsable --array=1-2 --dependency=afterok:1010"
assert_log "1012 --parsable --array=2-2 --dependency=afterok:1010:1011"
assert_log "1013 --parsable --array=3-4 --dependency=afterok:1012"

# Mechanical rung with its own drawdown forcing:
#   2A -> 2MN -> 2MW -> 2M -> 2B -> 2N -> 2W -> 2C
# 2MW and 2W each also wait on the baseline year array, whose detail they read.
assert_log "1017 --parsable --array=5-6 --dependency=afterok:1001:1016"
assert_log "1018 --parsable --array=1-2 --dependency=afterok:1017"
assert_log "1019 --parsable --array=1-1 --dependency=afterok:1018:1001"
assert_log "1020 --parsable --array=3-4 --dependency=afterok:1017:1019"
assert_log "1021 --parsable --array=3-3 --dependency=afterok:1017:1020"
assert_log "1022 --parsable --array=1-2 --dependency=afterok:1021"
assert_log "1023 --parsable --array=1-1 --dependency=afterok:1022:1001"
assert_log "1024 --parsable --array=5-6 --dependency=afterok:1021:1023"
assert_log "1025 --parsable --array=4-4 --dependency=afterok:1024:1002"

# Stacked output waits on every scenario's post-processing.
assert_log "1027 --parsable --dependency=afterok:1008:1015:1026"

# Supplied baseline: no baseline jobs or baseline dependency are submitted.
MOCK_STAGING_DIR="${TEST_ROOT}/supplied_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/supplied_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/supplied_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/supplied_sbatch.log"
MOCK_N_PHASE1=0
MOCK_N_PHASE1B=1
MOCK_N_PHASE2A=2
MOCK_N_PHASE2MN=0
MOCK_N_PHASE2MW=0
MOCK_N_PHASE2M=0
MOCK_N_PHASE2B=1
MOCK_N_PHASE2N=0
MOCK_N_PHASE2W=0
MOCK_N_PHASE2C=2
MOCK_N_SCENARIOS=1
MOCK_STACKED=0
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_N_PHASE2MN MOCK_N_PHASE2MW MOCK_N_PHASE2M
export MOCK_STACKED

{
  echo "$PLAN_HEADER"
  printf 'ordinary\t1\t1\t2\t1\tNA\tNA\tNA\t1\t2\t1\t1\tNA\tNA\tNA\tNA\tNA\n'
} > "$MOCK_SUBMISSION_PLAN"
echo 2000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher existing_baseline 0

assert_no_log "taxsim-baseline"
assert_no_log "taxsim-cf-mech"
assert_no_log "taxsim-mech-wealth"
assert_log "2002 --parsable --array=1-2 --dependency=afterok:2001"
assert_log "2005 --parsable --array=1-1 --dependency=afterok:2004"

# Batch mode over the same three-scenario plan. One array per phase, so the task
# ranges are the min-max of each plan column -- which is where a mis-numbered
# column shows up.
MOCK_STAGING_DIR="${TEST_ROOT}/batch_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/batch_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/batch_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/batch_sbatch.log"
MOCK_N_PHASE1=2
MOCK_N_PHASE1B=3
MOCK_N_PHASE2A=6
MOCK_N_PHASE2MN=2
MOCK_N_PHASE2MW=1
MOCK_N_PHASE2M=4
MOCK_N_PHASE2B=3
MOCK_N_PHASE2N=2
MOCK_N_PHASE2W=1
MOCK_N_PHASE2C=6
MOCK_N_SCENARIOS=3
MOCK_STACKED=1
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_N_PHASE2MN MOCK_N_PHASE2MW MOCK_N_PHASE2M
export MOCK_STACKED

{
  echo "$PLAN_HEADER"
  printf 'ordinary\t1\t1\t2\t1\tNA\tNA\tNA\t1\t2\t2\t1\tNA\tNA\tNA\tNA\tNA\n'
  printf 'corp\t2\t3\t4\t2\tNA\tNA\tNA\t3\t4\t3\t2\tNA\tNA\tNA\t1\t2\n'
  printf 'wealth\t3\t5\t6\t3\t1\t2\t1\t5\t6\t4\t3\t1\t2\t1\t3\t4\n'
} > "$MOCK_SUBMISSION_PLAN"
echo 4000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher NULL 0 batch

# Each phase is one array spanning every scenario that has tasks in it. The
# mechanical columns must resolve to their own ranges, not to a neighbour's.
assert_log "4003 --parsable --array=1-3 "
assert_log "4004 --parsable --array=1-6 --dependency=afterok:4001:4003"
assert_log "4005 --parsable --array=1-2 --dependency=afterok:4004"
assert_log "4006 --parsable --array=1-1 --dependency=afterok:4005:4001"
assert_log "4007 --parsable --array=1-4 --dependency=afterok:4004:4006"
assert_log "4008 --parsable --array=1-3 --dependency=afterok:4004:4007"
assert_log "4009 --parsable --array=1-2 --dependency=afterok:4008"
assert_log "4010 --parsable --array=1-1 --dependency=afterok:4009:4001"
assert_log "4011 --parsable --array=1-6 --dependency=afterok:4008:4010"
assert_log "4012 --parsable --array=2-4 --dependency=afterok:4011:4002"

# Baseline-only cleanup waits for baseline aggregation.
MOCK_STAGING_DIR="${TEST_ROOT}/baseline_staging"
MOCK_SUBMISSION_PLAN="${TEST_ROOT}/baseline_plan.tsv"
MOCK_COUNTER="${TEST_ROOT}/baseline_counter"
MOCK_SBATCH_LOG="${TEST_ROOT}/baseline_sbatch.log"
MOCK_N_PHASE1=2
MOCK_N_PHASE1B=0
MOCK_N_PHASE2A=0
MOCK_N_PHASE2MN=0
MOCK_N_PHASE2MW=0
MOCK_N_PHASE2M=0
MOCK_N_PHASE2B=0
MOCK_N_PHASE2N=0
MOCK_N_PHASE2W=0
MOCK_N_PHASE2C=0
MOCK_N_SCENARIOS=0
MOCK_STACKED=0
export MOCK_STAGING_DIR MOCK_SUBMISSION_PLAN MOCK_COUNTER MOCK_SBATCH_LOG
export MOCK_N_PHASE1 MOCK_N_PHASE1B MOCK_N_PHASE2A MOCK_N_PHASE2B
export MOCK_N_PHASE2N MOCK_N_PHASE2W MOCK_N_PHASE2C MOCK_N_SCENARIOS
export MOCK_N_PHASE2MN MOCK_N_PHASE2MW MOCK_N_PHASE2M
export MOCK_STACKED

echo "$PLAN_HEADER" > "$MOCK_SUBMISSION_PLAN"
echo 3000 > "$MOCK_COUNTER"
touch "$MOCK_SBATCH_LOG"
run_launcher NULL 1

assert_log "3002 --parsable --array=1-1 --dependency=afterok:3001"
assert_log "3003 --parsable --dependency=afterok:3002"

echo "SLURM dependency graph checks passed."
