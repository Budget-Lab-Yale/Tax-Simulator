#!/bin/bash
#SBATCH --job-name=td_precheck
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/taxdata_interface_2026070814/logs/precheck_%j.out
#SBATCH --time=00:20:00
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1

# Compare the SHARED 188 columns of old vs new Tax-Data vintage across a spread
# of years. New vintage = old + dina_other_taxes_rate appended (col 189). For
# each year, stream both files line-by-line and report which of the first 188
# columns (if any) differ in value -- O(1) memory, one pass per file.

set -uo pipefail

OLD=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026060918/baseline
NEW=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026070814/baseline
YEARS=(2017 2026 2035 2050 2065 2081)

echo "=== Tax-Data vintage shared-column comparison ==="
echo "OLD: $OLD"
echo "NEW: $NEW"
echo ""

ALL_IDENTICAL=1
for y in "${YEARS[@]}"; do
  of="$OLD/tax_units_${y}.csv"
  nf="$NEW/tax_units_${y}.csv"
  if [[ ! -f "$of" || ! -f "$nf" ]]; then
    echo "year $y: MISSING FILE"; ALL_IDENTICAL=0; continue
  fi
  awk -v OLDF="$of" -v NEWF="$nf" -v YR="$y" '
    BEGIN {
      nrow=0; nmismatch_rows=0;
      while ((getline l1 < OLDF) > 0) {
        if ((getline l2 < NEWF) <= 0) { print "year " YR ": NEW ran out of rows at row " (nrow+1); exit 3 }
        nrow++;
        n1=split(l1,a,","); n2=split(l2,b,",");
        rowdiff=0;
        for (i=1;i<=188;i++) if (a[i] != b[i]) { d[i]++; rowdiff=1 }
        if (rowdiff) nmismatch_rows++;
      }
      if ((getline l2 < NEWF) > 0) { print "year " YR ": OLD ran out of rows (new has more)"; }
      ndiffcol=0; for (i=1;i<=188;i++) if (i in d) ndiffcol++;
      if (ndiffcol==0) {
        printf "year %s: IDENTICAL (all 188 shared cols match across %d rows)\n", YR, nrow;
      } else {
        printf "year %s: *** DIFFERS *** %d/188 cols differ, %d/%d rows affected\n", YR, ndiffcol, nmismatch_rows, nrow;
        for (i=1;i<=188;i++) if (i in d) printf "      col %d: %d row(s) differ\n", i, d[i];
      }
    }
  '
  rc=$?
  [[ $rc -ne 0 ]] && ALL_IDENTICAL=0
  # if this year differed, mark it (awk prints DIFFERS; grep the marker is overkill, just track via a probe)
done

echo ""
echo "(To map differing column indices to names: header col N ="
echo "  head -1 $NEW/tax_units_2026.csv | cut -d, -fN )"
