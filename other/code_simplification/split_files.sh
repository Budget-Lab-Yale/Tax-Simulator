#!/bin/bash
# Splits src/sim/kg_dynamics.R into src/sim/kg/ and src/sim/corp_incidence.R
# into src/sim/corp/. Pure content moves: every line of each original lands in
# exactly one new file, in its original order, plus a short per-file header.
#
# The only relocation is kg_dyn_validate_timing_params() and its source-time
# self-call, which move from the timing section into constants.R -- the file
# that declares the constants it validates -- so no cross-file load-order
# dependency remains.
#
# Verified by multiset comparison at the end: the sorted non-header lines of the
# new files must equal the sorted lines of the original.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator

KG=src/sim/kg_dynamics.R
CORP=src/sim/corp_incidence.R
mkdir -p src/sim/kg src/sim/corp

# emit <out> <role line> <src> <range>...
emit() {
  local out=$1 role=$2 src=$3; shift 3
  {
    if [ -n "$role" ]; then
      echo "#-------------------------------------------------------------------------------"
      echo "# $(basename "$out")"
      echo "#"
      echo "# $role"
      echo "#-------------------------------------------------------------------------------"
      echo ""
      echo ""
    fi
    for r in "$@"; do sed -n "${r}p" "$src"; done
  } > "$out"
}

# --- kg_dynamics.R -> src/sim/kg/ --------------------------------------------
# constants.R keeps the module architecture header (original lines 1-58), so it
# takes no added header of its own.
emit src/sim/kg/constants.R "" $KG '1,460' '1276,1313'
emit src/sim/kg/inputs.R \
  'Cell aggregation, record attributes, and the Tax-Data / Macro / heir loaders.' \
  $KG '461,897' '2205,2543'
emit src/sim/kg/bellman.R \
  'Bellman backward induction and the tau / grid packing it consumes.' \
  $KG '898,1275'
emit src/sim/kg/timing.R \
  'Short-run realization timing overlay (planned-timing schedule, r_S).' \
  $KG '1314,1422'
emit src/sim/kg/tau_eq.R \
  'tau_eq: expected PV tax per dollar entering the gain state.' \
  $KG '1526,1856'
emit src/sim/kg/recurrence.R \
  'Bathtub recurrence: the step, the regime mix, the cell table, and the bathtub / frozen pass drivers.' \
  $KG '1423,1525' '1857,1952' '2607,3299'
emit src/sim/kg/apply.R \
  'Per-record appliers (pure allocators) and the cell-level MTR / carry / estate aggregators they pair with.' \
  $KG '1953,2097' '3300,3467'
emit src/sim/kg/state.R \
  'State-file paths, scenario activation, and per-year regime resolution.' \
  $KG '2098,2204'
emit src/sim/kg/diag.R \
  'Diagnostics: estate-exposure dump, bathtub summary, and the wealth-law predicate.' \
  $KG '2544,2606' '3468,3645'

# --- corp_incidence.R -> src/sim/corp/ ---------------------------------------
emit src/sim/corp/paths.R "" $CORP '1,1059'
emit src/sim/corp/apply.R \
  'Record appliers: the flow / stock / kg hits, and the kg bathtub glue.' \
  $CORP '1060,1231' '1356,1461'
emit src/sim/corp/diag.R \
  'Conservation diagnostic and the analytic path self-check.' \
  $CORP '1232,1355' '1462,1566'

# --- verification: no line lost, none duplicated -----------------------------
check() {
  local orig=$1; shift
  local tmp_new tmp_old
  tmp_new=$(mktemp); tmp_old=$(mktemp)
  # strip the 7-line headers we added (5 comment lines + 2 blanks)
  for f in "$@"; do
    if head -2 "$f" | tail -1 | grep -q "^# $(basename "$f")$"; then
      tail -n +8 "$f"
    else
      cat "$f"
    fi
  done | sort > "$tmp_new"
  sort "$orig" > "$tmp_old"
  if diff -q "$tmp_old" "$tmp_new" >/dev/null; then
    echo "OK  $orig: content preserved exactly ($(wc -l < "$tmp_old") lines)"
  else
    echo "FAIL $orig:"
    diff "$tmp_old" "$tmp_new" | head -20
    exit 1
  fi
  rm -f "$tmp_new" "$tmp_old"
}

check $KG src/sim/kg/*.R
check $CORP src/sim/corp/*.R

echo
echo "new file sizes:"
wc -l src/sim/kg/*.R src/sim/corp/*.R
