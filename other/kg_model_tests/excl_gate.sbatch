#!/bin/bash
#SBATCH --job-name=excl_gate
#SBATCH --partition=day
#SBATCH -c 4
#SBATCH --time=23:00:00
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/kg_model_tests/excl_gate_%x_%j.out

# One arm of the death-gain-exclusion byte-identity gate: runs the excl_gate
# runscript (baseline + s_deemed_deemed, full sample, seven years) in a given
# source tree and writes to a given vintage. Submit once from the pre-feature
# worktree and once from the feature tree, then byte-compare with
# excl_gate_compare.sh.
# Usage: sbatch --job-name=<tag> excl_gate.sbatch <tree_dir> <vintage>
set -euo pipefail

TREE=$1
VINTAGE=$2

module load R/4.4.1-foss-2022b
cd "$TREE"
git -C "$TREE" rev-parse --short HEAD || true
git -C "$TREE" status --porcelain | wc -l

Rscript src/main.R tests/excl_gate NULL 1 "$VINTAGE" 1 0 NULL 0 none
echo "GATE_RUN_EXIT=$?"
