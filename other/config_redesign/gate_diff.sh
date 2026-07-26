#!/bin/bash
# Gate comparator for the config-redesign phases: compares a candidate vintage
# tree against its golden counterpart under the sanctioned exclusion rules
# (see golden/README.md).
#
# Usage: bash gate_diff.sh <candidate_vintage_dir> <golden_vintage_dir>
# Exit 0 = gate passes; nonzero = differences found (listed on stdout).
set -uo pipefail

CAND=$1
GOLD=$2
fail=0

# Manifest families are verified by the mapping check, not here. xlsx and
# code_version.csv are excluded from byte comparison (content-checked / by
# construction different). assumptions.csv is a golden-only manifest: the
# candidate writes scenario_config.csv in its place, and mapping_check.py is
# what confirms the two carry the same values.
EXCLUDE_RE='(^|/)(code_version\.csv|assumptions\.csv|behavioral_assumptions\.csv|scenarios\.csv|scenario_config\.csv|dependencies\.csv)$|\.xlsx$'

list_files () {
  (cd "$1" && find . -type f ! -path './_slurm_staging/*' | sort)
}

# The excess-growth machinery was removed in the config rebuild's Phase 1, so
# supplemental/excess_growth_offset.csv exists only on the golden side. That is
# only a safe exclusion if every golden copy is the neutral series -- an
# income_factor of exactly 1 in every year means the offset never moved a
# number. Check that before excusing the missing files.
GOLD_ONLY_RE='(^|/)excess_growth_offset\.csv$'
egrowth_neutral_check () {
  local n=0
  while IFS= read -r f; do
    n=$((n + 1))
    if awk -F, 'NR == 1 { for (i = 1; i <= NF; i++) if ($i == "income_factor") c = i; next }
                $c != 1 { bad = 1 } END { exit bad ? 1 : 0 }' "$GOLD/$f"; then :; else
      echo "GOLDEN EXCESS GROWTH NOT NEUTRAL: $f"
      fail=1
    fi
  done < <(list_files "$GOLD" | grep -E "$GOLD_ONLY_RE")
  echo "--- excess-growth neutrality: $n golden file(s) checked ---"
}
egrowth_neutral_check

# 1. File-set comparison (full sets, before exclusions -- a missing xlsx is
#    still a failure even though its content is compared differently)
comm -3 <(list_files "$CAND" | grep -Ev '(^|/)(scenarios\.csv|scenario_config\.csv)$') \
        <(list_files "$GOLD" | grep -Ev "$GOLD_ONLY_RE" \
                                 | grep -Ev '(^|/)assumptions\.csv$') > /tmp/gate_diff_sets.$$ || true
if [ -s /tmp/gate_diff_sets.$$ ]; then
  echo "FILE-SET MISMATCH (left-only = candidate, right-only = golden):"
  cat /tmp/gate_diff_sets.$$
  fail=1
fi

# 2. Byte comparison on the non-excluded set
while IFS= read -r f; do
  if ! cmp -s "$CAND/$f" "$GOLD/$f"; then
    echo "BYTE DIFF: $f"
    fail=1
  fi
done < <(list_files "$GOLD" | grep -Ev "$EXCLUDE_RE" | grep -Ev "$GOLD_ONLY_RE")

# 3. dependencies.csv: content equality, order-insensitive (header preserved)
for side in dependencies.csv; do
  if [ -f "$GOLD/$side" ] && [ -f "$CAND/$side" ]; then
    if ! cmp -s <(head -1 "$GOLD/$side"; tail -n +2 "$GOLD/$side" | sort) \
                <(head -1 "$CAND/$side"; tail -n +2 "$CAND/$side" | sort); then
      echo "CONTENT DIFF (sorted): $side"
      fail=1
    fi
  fi
done

# 4. xlsx content check (timestamp-free): compare the zipped sheet payloads
while IFS= read -r f; do
  g_hash=$(unzip -p "$GOLD/$f" 'xl/worksheets/*' 'xl/sharedStrings.xml' 2>/dev/null | md5sum | cut -d' ' -f1)
  c_hash=$(unzip -p "$CAND/$f" 'xl/worksheets/*' 'xl/sharedStrings.xml' 2>/dev/null | md5sum | cut -d' ' -f1)
  if [ "$g_hash" != "$c_hash" ]; then
    echo "XLSX CONTENT DIFF: $f"
    fail=1
  fi
done < <(list_files "$GOLD" | grep '\.xlsx$')

# 5. Positive assertions, listed by name so the exclusion list can't rot
echo "--- positive assertions ---"
for f in $(list_files "$GOLD" | grep 'tax_law\.csv$'); do
  if cmp -s "$CAND/$f" "$GOLD/$f"; then echo "ASSERT OK  $f"; else { echo "ASSERT FAIL $f"; fail=1; }; fi
done

rm -f /tmp/gate_diff_sets.$$
if [ "$fail" -eq 0 ]; then echo "GATE_PASS: $CAND == $GOLD (under sanctioned exclusions)"; else echo "GATE_FAIL"; fi
exit $fail
