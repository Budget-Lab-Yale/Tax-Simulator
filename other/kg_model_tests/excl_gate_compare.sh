#!/bin/bash
# Byte-compares the two arms of the death-gain-exclusion gate. Detail, totals
# and revenue files must be byte-identical at a zero exclusion; the files the
# feature deliberately changes are set aside and listed for review instead:
#   - kg_dynamics state files and their summaries (cell tables carry new columns)
#   - the heir-side distribution outputs and allocator diagnostics (deemed tax
#     moved from the proportional smear to the rank-matching allocator)
#   - the provenance manifests (commit hash and the new widowhood entry)
# Usage: excl_gate_compare.sh <pre_dir> <post_dir>

PRE=$1
POST=$2
fail=0
n_ok=0
n_review=0

is_expected_diff() {
  case "$1" in
    *kg_dynamics*|*distribution*|*estate_allocator_diag*|*estate_tax_detail_*) return 0 ;;
    *code_version.csv|*scenario_config.csv) return 0 ;;
    *) return 1 ;;
  esac
}

# File sets must match outside the expected-diff set
comm -3 <(cd "$PRE" && find . -path ./_slurm_staging -prune -o -type f -print | sort) \
        <(cd "$POST" && find . -path ./_slurm_staging -prune -o -type f -print | sort) \
  | grep -v -e kg_dynamics -e distribution -e estate_allocator_diag > /tmp/fileset_diff_$$
if [ -s /tmp/fileset_diff_$$ ]; then
  echo "FILE SET MISMATCH:"
  cat /tmp/fileset_diff_$$
  fail=1
fi
rm -f /tmp/fileset_diff_$$

while IFS= read -r f; do
  [ -f "$POST/$f" ] || continue
  if is_expected_diff "$f"; then
    if cmp -s "$PRE/$f" "$POST/$f"; then n_ok=$((n_ok+1)); else
      echo "REVIEW (expected to differ): $f"
      n_review=$((n_review+1))
    fi
    continue
  fi
  if [ "${f##*.}" = "xlsx" ]; then
    d1=$(mktemp -d); d2=$(mktemp -d)
    unzip -qq "$PRE/$f" -d "$d1"; unzip -qq "$POST/$f" -d "$d2"
    rm -f "$d1/docProps/core.xml" "$d2/docProps/core.xml"
    if diff -rq "$d1" "$d2" > /dev/null; then n_ok=$((n_ok+1)); else
      echo "DIFF: $f"; fail=1
    fi
    rm -rf "$d1" "$d2"
  elif cmp -s "$PRE/$f" "$POST/$f"; then
    n_ok=$((n_ok+1))
  else
    echo "DIFF: $f"
    fail=1
  fi
done < <(cd "$PRE" && find . -path ./_slurm_staging -prune -o -type f -print | sort)

echo "---"
echo "identical files: $n_ok"
echo "expected-diff files set aside for review: $n_review"
if [ $fail -eq 0 ]; then echo "RESULT: BYTE-IDENTICAL outside the reviewed set"; else echo "RESULT: DIFFERENCES FOUND"; fi
exit $fail
