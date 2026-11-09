#!/bin/bash
# Byte-compares two smoke-run output vintages. All non-xlsx files must be
# byte-identical; xlsx files are compared on their unzipped contents minus
# docProps/core.xml (openxlsx stamps a creation time there).
# Usage: compare_smoke.sh <pre_dir> <post_dir>

PRE=$1
POST=$2
fail=0
n_ok=0
n_xlsx=0

# File sets must match
comm -3 <(cd "$PRE" && find . -path ./_slurm_staging -prune -o -type f -print | sort) <(cd "$POST" && find . -path ./_slurm_staging -prune -o -type f -print | sort) > /tmp/fileset_diff_$$
if [ -s /tmp/fileset_diff_$$ ]; then
  echo "FILE SET MISMATCH:"
  cat /tmp/fileset_diff_$$
  fail=1
fi
rm -f /tmp/fileset_diff_$$

# Non-xlsx: byte compare
while IFS= read -r f; do
  if [ -f "$POST/$f" ]; then
    if cmp -s "$PRE/$f" "$POST/$f"; then
      n_ok=$((n_ok+1))
    else
      echo "DIFF: $f"
      fail=1
    fi
  fi
done < <(cd "$PRE" && find . -path ./_slurm_staging -prune -o -type f ! -name "*.xlsx" -print | sort)

# xlsx: compare unzipped contents except docProps/core.xml
while IFS= read -r f; do
  if [ -f "$POST/$f" ]; then
    d1=$(mktemp -d); d2=$(mktemp -d)
    unzip -qq "$PRE/$f" -d "$d1"
    unzip -qq "$POST/$f" -d "$d2"
    rm -f "$d1/docProps/core.xml" "$d2/docProps/core.xml"
    if diff -rq "$d1" "$d2" > /dev/null; then
      n_xlsx=$((n_xlsx+1))
    else
      echo "XLSX DIFF: $f"
      diff -rq "$d1" "$d2" | head -5
      fail=1
    fi
    rm -rf "$d1" "$d2"
  fi
done < <(cd "$PRE" && find . -path ./_slurm_staging -prune -o -type f -name "*.xlsx" -print | sort)

echo "---"
echo "identical non-xlsx files: $n_ok"
echo "identical xlsx (ex-timestamp): $n_xlsx"
if [ $fail -eq 0 ]; then echo "RESULT: BYTE-IDENTICAL"; else echo "RESULT: DIFFERENCES FOUND"; fi
exit $fail
