#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --mem=4G
#SBATCH --partition=day
#SBATCH --job-name=estate-chain-compare
#SBATCH --output=logs/estate_chain_compare_%j.log
# Bytewise comparison for the commit-1 bit-identity check: the
# estate_chain_check baseline re-run (post-refactor code) against the
# existing niskanen_housing baseline (pre-refactor code). Hard criteria:
# every static detail year and every totals file must be identical.
# Files present in the old vintage but not the new (full-run artifacts the
# baseline-only pipeline doesn't produce) are reported as informational.
set -u
OLD=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/niskanen_housing/baseline
NEW=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/estate_chain_check/baseline

fail=0
for y in $(seq 2026 2036); do
  a="$OLD/static/detail/$y.csv"; b="$NEW/static/detail/$y.csv"
  if [ ! -f "$b" ]; then echo "FAIL missing detail $y"; fail=1
  elif cmp -s "$a" "$b"; then echo "IDENTICAL static/detail/$y.csv"
  else echo "FAIL DIFF static/detail/$y.csv"; fail=1; fi
done

for f in estate.csv 1040.csv payroll.csv 1040_by_agi.csv receipts.csv receipts_full.csv; do
  a="$OLD/static/totals/$f"; b="$NEW/static/totals/$f"
  if [ ! -f "$a" ]; then echo "skip totals/$f (absent in old)"; continue; fi
  if [ ! -f "$b" ]; then echo "FAIL missing totals/$f"; fail=1
  elif cmp -s "$a" "$b"; then echo "IDENTICAL static/totals/$f"
  else echo "FAIL DIFF static/totals/$f"; fail=1; fi
done

if [ $fail -eq 0 ]; then echo "RESULT: BIT-IDENTICAL"; else echo "RESULT: MISMATCH"; fi
exit $fail
