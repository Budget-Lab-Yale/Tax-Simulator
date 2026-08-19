#!/usr/bin/env python3
"""Verify the hand-transcribed filing-model coefficients against their PDFs.

Both `resources/mok_coefs.csv` (todo A4) and `resources/cilke_coefs.csv` (A5)
were transcribed by eye from rendered page images, because both tables defeat
automated extraction in ways that produce *plausible* wrong numbers rather than
obvious failures -- Mok's Panel E prints its columns in reverse age order, and
`pdftotext -layout` scrambles Cilke's row-label alignment. Transcription by eye
trades one failure mode for another, so this script closes the second: it
recomputes, per PDF page, the multiset of numeric tokens the page contains and
checks that every value the CSV claims from that page is actually there.

What this does and does not prove. It catches typos, dropped digits, transposed
figures and stray signs -- the errors hand transcription actually makes. It does
NOT prove a value landed in the right cell, since a swap of two cells on the
same page preserves the multiset. Cell assignment rests on the page images; this
is the arithmetic backstop, not a substitute for reading them.

Run from the repo root; needs `pdftotext` (module load poppler/25.07.0-GCC-13.3.0):
    python3 other/state_tax_research/nonfiler_residual/11_verify_coef_transcriptions.py
"""

import collections
import csv
import os
import re
import subprocess
import sys

RES = "other/state_tax_research/nonfiler_residual/resources"

# csv -> (pdf, extra per-group values printed once rather than once per row)
SOURCES = {
    "mok_coefs.csv": ("mok2017_cbo_wp2017-06.pdf", ["n_obs", "filing_rate_weighted"]),
    "cilke_coefs.csv": ("cilke1998_ota_wp78.pdf", ["n_obs"]),
}

# Cells the source prints as a placeholder rather than an estimate. Stored blank
# in the CSVs with a `note`, asserted here so a future edit cannot quietly turn
# one into a zero -- a zero coefficient and an unestimated one are not the same
# thing, and only one of them is safe to score.
EXPECTED_BLANKS = {"mok_coefs.csv": 3, "cilke_coefs.csv": 14}


def page_tokens(pdf, page):
    """Every numeric token on one page, as a multiset. Asterisks are
    significance markers, not digits, so they are stripped first; thousands
    separators are dropped so 24,634 matches 24634."""
    out = subprocess.run(
        ["pdftotext", "-f", str(page), "-l", str(page), "-raw", pdf, "-"],
        capture_output=True, text=True, check=True).stdout
    toks = re.findall(r"-?\d[\d,]*\.?\d*", out.replace("*", " "))
    return collections.Counter(t.replace(",", "") for t in toks)


def verify(csv_name, pdf_name, per_group_cols):
    rows = list(csv.DictReader(open(os.path.join(RES, csv_name))))
    pdf = os.path.join(RES, pdf_name)

    want = collections.defaultdict(collections.Counter)
    for r in rows:
        if r["coefficient"] == "":
            continue
        page = int(r["pdf_page"])
        want[page][r["coefficient"]] += 1
        want[page][r["std_error"]] += 1

    # n_obs and the filing rate are printed once per group, so counting them on
    # every row would demand multiplicity the page does not have.
    for gid in dict.fromkeys(r["group_id"] for r in rows):
        grp = [r for r in rows if r["group_id"] == gid]
        for col in per_group_cols:
            # take the page where that group's footer actually sits
            want[int(grp[-1]["pdf_page"])][grp[-1][col]] += 1

    ok = True
    for page in sorted(want):
        missing = want[page] - page_tokens(pdf, page)
        status = "OK" if not missing else "MISSING " + str(dict(missing))
        print(f"  page {page:>3}: {sum(want[page].values()):>4} values -> {status}")
        ok &= not missing

    blanks = sum(1 for r in rows if r["coefficient"] == "")
    expected = EXPECTED_BLANKS[csv_name]
    print(f"  placeholder cells (not estimated): {blanks}, expected {expected}")
    ok &= blanks == expected

    n_terms = len(dict.fromkeys(r["term"] for r in rows))
    n_groups = len(dict.fromkeys(r["group_id"] for r in rows))
    print(f"  {n_groups} groups x {n_terms} terms = {n_groups * n_terms} rows, "
          f"file has {len(rows)}")
    ok &= n_groups * n_terms == len(rows)
    return ok


def main():
    all_ok = True
    for csv_name, (pdf_name, per_group) in SOURCES.items():
        print(f"{csv_name} against {pdf_name}")
        all_ok &= verify(csv_name, pdf_name, per_group)
        print()
    print("PASS: every transcribed value appears on the page it is claimed from"
          if all_ok else "FAIL")
    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
