#!/usr/bin/env python3
"""
process_cbo_revenue.py

One-time prep that converts a CBO baseline revenue workbook into the tidy CSV
resource consumed at runtime by src/data/post_processing/cbo_comparison.R.

It extracts the "3. Individual Income Tax Details" sheet (CBO's full 1040
build-up by calendar year) into long form: cbo_row, cbo_line, year, value.
The whole sheet is kept (including per-bracket lines we don't currently map) so
future mappings need no re-processing. The line->our-column mapping lives in the
R module, not here.

Source workbook for the committed resource:
  CBO, "The Budget and Economic Outlook: 2026 to 2036", supplemental revenue
  file 51138-2026-02-Revenue (pub. 61882), February 2026.
The .xlsx is NOT committed; download the current release and regenerate when CBO
publishes a new baseline.

Pure stdlib (no pandas/openpyxl) so it runs anywhere. Parses the .xlsx via
zipfile + xml.etree.

Usage:
  python3 other/cbo/process_cbo_revenue.py <input.xlsx> <output.csv>
  # default output: resources/cbo/cbo_iit_detail_feb2026.csv
"""

import csv
import sys
import zipfile
import xml.etree.ElementTree as ET

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
RNS = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"
SHEET_NAME = "3.Individual Income Tax Details"
DEFAULT_OUT = "resources/cbo/cbo_iit_detail_feb2026.csv"


def col_to_idx(ref):
    """'AB12' -> 1-based column index."""
    letters = "".join(c for c in ref if c.isalpha())
    n = 0
    for c in letters:
        n = n * 26 + (ord(c.upper()) - 64)
    return n


def read_sheet_rows(xlsx_path):
    z = zipfile.ZipFile(xlsx_path)
    ss = []
    t = ET.fromstring(z.read("xl/sharedStrings.xml"))
    for si in t:
        ss.append("".join(x.text or "" for x in si.iter(NS + "t")))

    wb = ET.fromstring(z.read("xl/workbook.xml"))
    rels = ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))
    rid2tgt = {r.get("Id"): r.get("Target") for r in rels}
    sheet_file = None
    for s in wb.iter(NS + "sheet"):
        if s.get("name") == SHEET_NAME:
            sheet_file = "xl/" + rid2tgt[s.get(RNS + "id")]
    if sheet_file is None:
        sys.exit("Could not find sheet %r in %s" % (SHEET_NAME, xlsx_path))

    sh = ET.fromstring(z.read(sheet_file))
    rows = []
    for r in sh.iter(NS + "row"):
        ri = int(r.get("r"))
        cells = {}
        for c in r.iter(NS + "c"):
            ci = col_to_idx(c.get("r"))
            v = c.find(NS + "v")
            if v is None:
                continue
            val = v.text
            if c.get("t") == "s":
                val = ss[int(val)]
            cells[ci] = val
        rows.append((ri, cells))
    return rows


def main():
    if len(sys.argv) < 2:
        sys.exit("Usage: process_cbo_revenue.py <input.xlsx> [output.csv]")
    xlsx_path = sys.argv[1]
    out_path = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_OUT

    rows = read_sheet_rows(xlsx_path)

    # Map year -> column from the "Calendar year" header row.
    year_to_col = {}
    for ri, cells in rows:
        if (cells.get(1) or "").strip().startswith("Calendar year"):
            for ci, val in cells.items():
                try:
                    yr = int(float(val))
                except (ValueError, TypeError):
                    continue
                if 2000 <= yr <= 2100:
                    year_to_col[yr] = ci
            break
    if not year_to_col:
        sys.exit("Could not parse the calendar-year header row.")

    out_rows = []
    for ri, cells in rows:
        label = (cells.get(1) or "").strip()
        if not label or label.startswith("Calendar year"):
            continue
        for yr in sorted(year_to_col):
            raw = cells.get(year_to_col[yr])
            if raw is None:
                continue
            try:
                value = float(raw)
            except ValueError:
                continue
            out_rows.append((ri, label, yr, round(value, 4)))

    with open(out_path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cbo_row", "cbo_line", "year", "value"])
        w.writerows(out_rows)

    n_lines = len({r[0] for r in out_rows})
    yrs = sorted({r[2] for r in out_rows})
    print("Wrote %s: %d value rows, %d line items, years %d-%d"
          % (out_path, len(out_rows), n_lines, yrs[0], yrs[-1]))


if __name__ == "__main__":
    main()
