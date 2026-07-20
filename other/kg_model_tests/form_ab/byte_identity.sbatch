#!/bin/bash
#-----------------------------------------------------------------------
# byte_identity.sbatch  --  Part B0 of the functional-form toggle build.
#
# Runs the kg_v3_smoke runscript (a small CG-rate-reform kg scenario) under
# the LEVELS response form at full sample, from a caller-supplied repo dir,
# writing to a caller-supplied vintage. Launched twice by
# launch_byte_identity.sh: once from a worktree at the PRE-change commit and
# once from the POST-change working tree. compare_smoke.sh then byte-diffs the
# two output vintages (xlsx timestamp exception). Empty diff == the levels path
# is bit-identical to the pre-toggle code (the toggle only adds a logs branch).
#
#   sbatch byte_identity.sbatch <repo_dir> <vintage>
#-----------------------------------------------------------------------
#SBATCH --job-name=form_byte
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=3:00:00
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/kg_form_scratch/byte_%j.out

set -euo pipefail
REPO_DIR="$1"
VINTAGE="$2"

cd "$REPO_DIR"
module load R/4.4.1-foss-2022b

# Explicit levels form (the default, stated for the record). main.R args:
#   runscript scenario_id user_id local vintage pct_sample stacked
#   baseline_vintage delete_detail multicore
export KG_RESPONSE_FORM=levels
Rscript src/main.R tests/kg_v3_smoke NULL user_test 1 "$VINTAGE" 1 0 NULL 0 none
echo "BYTE_RUN_EXIT=$? repo=$REPO_DIR vintage=$VINTAGE"
