#!/bin/bash
#-------------------------------------------------------------------------------
# diff_verify.sh
#
# Byte-compares the pre- and post-migration verification vintages. The
# assumptions refactor moved constants out of R and into config, reading them at
# the point of use; the defaults are unchanged, so every output file must be
# byte-identical. Any diff means the refactor changed behavior.
#
# Files legitimately expected to differ are excluded:
#   - assumptions.csv / code_version.csv : new manifest files (post only)
#   - _slurm_staging/                    : run scaffolding, timestamps, rds
#   - *.xlsx                             : never byte-identical (docProps stamps)
#-------------------------------------------------------------------------------

ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1

compare_pair() {
  local pre="$ROOT/$1"
  local post="$ROOT/$2"
  local label="$3"

  echo "=============================================================="
  echo "$label"
  echo "  pre:  $pre"
  echo "  post: $post"
  echo "=============================================================="

  if [ ! -d "$pre" ] || [ ! -d "$post" ]; then
    echo "  MISSING one or both vintages"
    return 1
  fi

  local diffs
  diffs=$(diff -rq "$pre" "$post" 2>&1 \
    | grep -v "_slurm_staging" \
    | grep -v "assumptions.csv" \
    | grep -v "code_version.csv" \
    | grep -v "\.xlsx")

  local n_files
  n_files=$(find "$post" -type f -name "*.csv" -not -path "*_slurm_staging*" | wc -l)

  if [ -z "$diffs" ]; then
    echo "  BYTE-IDENTICAL across $n_files csv files"
  else
    echo "  DIFFS FOUND ($(echo "$diffs" | wc -l) entries), first 40:"
    echo "$diffs" | head -40
  fi
  echo
}

compare_pair verify_pre_ckw   verify_post_ckw   "corp + kg + wealth + estate + distribution"
compare_pair verify_pre_sigma verify_post_sigma "sigma + evasion + entity shifting + charity"
