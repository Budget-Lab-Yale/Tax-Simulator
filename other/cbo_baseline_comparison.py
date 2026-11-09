#!/usr/bin/env python3
"""
cbo_baseline_comparison.py

Line-by-line comparison of Tax-Simulator's current-law baseline 1040 totals
against CBO's February 2026 baseline (pub. 61882, revenue file 51138), sheet
"3. Individual Income Tax Details".

Both sources are in billions of dollars (returns in millions). Comparison is
restricted to the overlapping calendar years 2025-2035.

Pure stdlib (no pandas/openpyxl) so it runs on the login node without R or
extra packages. Parses the CBO .xlsx directly via zipfile + xml.etree.

Outputs:
  - <RUN_DIR>/cbo_1040_comparison.csv  (tidy: section,line_item,our_mapping,
                                        year,cbo,ours,diff,pct_diff)
  - prints a scannable wide view for spot-check years to stdout.
"""

import csv
import os
import sys
import zipfile
import xml.etree.ElementTree as ET

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
RNS = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"

RUN_DIR = (
    "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/"
    "Tax-Simulator/v1/baseline_cbo_compare"
)
CBO_XLSX = os.path.join(RUN_DIR, "51138-2026-02-Revenue (3).xlsx")
OUR_1040 = os.path.join(RUN_DIR, "baseline/static/totals/1040.csv")
OUT_CSV = os.path.join(RUN_DIR, "cbo_1040_comparison.csv")
CBO_SHEET_NAME = "3.Individual Income Tax Details"

YEARS = list(range(2025, 2036))           # 2025-2035 inclusive
PRINT_YEARS = [2025, 2030, 2035]          # wide views printed to stdout

# Line items: (section, label, [our 1040 columns to SUM]).
# A derived row (income tax after credits) is handled specially below.
MAPPING = [
    ("AGI",   "Salaries and wages",                       ["wages"]),
    ("AGI",   "Taxable interest and ordinary dividends",  ["txbl_int", "div_ord"]),
    ("AGI",   "Qualified dividends",                       ["div_pref"]),
    ("AGI",   "Capital gain or loss",                      ["txbl_kg"]),
    ("AGI",   "Net business income (Sch C, E, F)",         ["sole_prop", "sch_e", "farm"]),
    ("AGI",   "Taxable pensions/annuities + IRA dist.",    ["txbl_pens_dist", "txbl_ira_dist"]),
    ("AGI",   "Taxable Social Security benefits",          ["txbl_ss"]),
    ("AGI",   "Total income",                              ["gross_inc"]),
    ("AGI",   "Subtract statutory adjustments",            ["above_ded"]),
    ("AGI",   "Adjusted gross income",                     ["agi"]),
    ("TXBL",  "Subtract personal exemption",               ["pe_ded"]),
    ("TXBL",  "Subtract standard deduction",               ["std_ded"]),
    ("TXBL",  "Subtract itemized deductions",              ["item_ded"]),
    ("TXBL",  "Subtract QBI deduction",                    ["qbi_ded"]),
    ("TXBL",  "Subtract additional deductions",            ["tip_ded", "ot_ded", "senior_ded"]),
    ("TXBL",  "Total exemptions and deductions",           ["ded"]),
    ("TXBL",  "Taxable income",                            ["txbl_inc"]),
    ("TAX",   "Total income tax before credits (incl AMT)", ["liab_bc"]),
    ("TAX",   "Tax at ordinary rates",                     ["liab_ord"]),
    ("TAX",   "Tax at reduced rates (cap gains/div)",      ["liab_pref", "liab_1250", "liab_collect"]),
    ("TAX",   "Tax from AMT",                              ["liab_amt"]),
    ("TAX",   "Total credits (refundable + nonrefundable)", ["nonref", "ref"]),
    ("TAX",   "Income tax after credits",                  ["__after_credits__"]),
    ("TAX",   "Net investment income tax",                 ["liab_niit"]),
    ("TAX",   "Individual income tax liability",           ["liab_iit"]),
    ("ADD",   "Number of returns (millions)",              ["n_returns"]),
    ("ADD",   "Number with itemized deductions",           ["n_itemizing"]),
]

# CBO row label (col 1 of sheet 3) matched against each line item above.
# Keyed by our label -> a distinctive substring of the CBO row label.
CBO_LABEL = {
    "Salaries and wages": "Salaries and wages",
    "Taxable interest and ordinary dividends": "Taxable interest and ordinary dividends",
    "Qualified dividends": "Qualified dividends",
    "Capital gain or loss": "Capital gain or loss",
    "Net business income (Sch C, E, F)": "Net business income",
    "Taxable pensions/annuities + IRA dist.": "Taxable pensions and annuities and IRA distributions",
    "Taxable Social Security benefits": "Taxable Social Security benefits",
    "Total income": "Total income",
    "Subtract statutory adjustments": "Subtract statutory adjustments",
    "Adjusted gross income": "Adjusted gross income",
    "Subtract personal exemption": "Subtract personal exemption",
    "Subtract standard deduction": "Subtract standard deduction",
    "Subtract itemized deductions": "Subtract total itemized deductions",
    "Subtract QBI deduction": "Subtract qualified business income deduction",
    "Subtract additional deductions": "Subtract additional deductions",
    "Total exemptions and deductions": "Total exemptions and deductions after limits",
    "Taxable income": "Taxable income",
    "Total income tax before credits (incl AMT)": "Total income tax (including AMT) before credits",
    "Tax at ordinary rates": "Tax from taxable income and taxed at ordinary rates",
    "Tax at reduced rates (cap gains/div)": "Tax from taxable income and taxed at reduced rates",
    "Tax from AMT": "Tax from AMT",
    "Total credits (refundable + nonrefundable)": "Total credits (refundable and nonrefundable)",
    "Income tax after credits": "Income tax after credits",
    "Net investment income tax": "Net investment income tax",
    "Individual income tax liability": "Individual income tax liability",
    "Number of returns (millions)": "Number of returns",
    "Number with itemized deductions": "Number with itemized deductions",
}


def col_to_idx(ref):
    """'AB12' -> 1-based column index 28."""
    letters = "".join(c for c in ref if c.isalpha())
    n = 0
    for c in letters:
        n = n * 26 + (ord(c.upper()) - 64)
    return n


def read_cbo_sheet():
    """Return (year_to_col, rows) where rows is list of (label, {colidx:val})."""
    z = zipfile.ZipFile(CBO_XLSX)
    ss = []
    t = ET.fromstring(z.read("xl/sharedStrings.xml"))
    for si in t:
        ss.append("".join(x.text or "" for x in si.iter(NS + "t")))

    wb = ET.fromstring(z.read("xl/workbook.xml"))
    rels = ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))
    rid2tgt = {r.get("Id"): r.get("Target") for r in rels}
    sheet_file = None
    for s in wb.iter(NS + "sheet"):
        if s.get("name") == CBO_SHEET_NAME:
            sheet_file = "xl/" + rid2tgt[s.get(RNS + "id")]
    if sheet_file is None:
        sys.exit("Could not locate CBO sheet '%s'" % CBO_SHEET_NAME)

    sh = ET.fromstring(z.read(sheet_file))
    rows = []
    for r in sh.iter(NS + "row"):
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
        if cells:
            label = (cells.get(1) or "").strip()
            rows.append((label, cells))

    # Build year -> column index from the "Calendar year" header row.
    year_to_col = {}
    for label, cells in rows:
        if label.startswith("Calendar year"):
            for ci, val in cells.items():
                try:
                    yr = int(float(val))
                except (ValueError, TypeError):
                    continue
                if 2000 <= yr <= 2100:
                    year_to_col[yr] = ci
            break
    if not year_to_col:
        sys.exit("Could not parse CBO calendar-year header row.")
    return year_to_col, rows


def cbo_value(rows, year_to_col, cbo_label_substr, year):
    """First data row whose label contains the substring; value for `year`."""
    col = year_to_col.get(year)
    if col is None:
        return None
    for label, cells in rows:
        if cbo_label_substr in label:
            raw = cells.get(col)
            if raw is None:
                return None
            try:
                return float(raw)
            except ValueError:
                return None
    return None


def read_our_1040():
    """year(int) -> {column: float}."""
    out = {}
    with open(OUR_1040) as f:
        for row in csv.DictReader(f):
            try:
                yr = int(row["year"])
            except (ValueError, KeyError):
                continue
            d = {}
            for k, v in row.items():
                try:
                    d[k] = float(v)
                except (ValueError, TypeError):
                    d[k] = None
            out[yr] = d
    return out


def our_value(our_row, cols):
    if cols == ["__after_credits__"]:
        # Income tax after credits = liab_iit - liab_niit - liab_surtax
        parts = [our_row.get("liab_iit"), our_row.get("liab_niit"),
                 our_row.get("liab_surtax")]
        if any(p is None for p in parts):
            return None
        return parts[0] - parts[1] - parts[2]
    total = 0.0
    for c in cols:
        v = our_row.get(c)
        if v is None:
            return None
        total += v
    return total


def main():
    for path in (CBO_XLSX, OUR_1040):
        if not os.path.exists(path):
            sys.exit("Missing input: %s" % path)

    year_to_col, cbo_rows = read_cbo_sheet()
    ours = read_our_1040()

    records = []  # (section, line_item, mapping_str, year, cbo, ours, diff, pct)
    for section, label, cols in MAPPING:
        mapping_str = (
            "liab_iit - liab_niit - liab_surtax"
            if cols == ["__after_credits__"] else " + ".join(cols)
        )
        cbo_sub = CBO_LABEL[label]
        for yr in YEARS:
            cbo_v = cbo_value(cbo_rows, year_to_col, cbo_sub, yr)
            our_row = ours.get(yr, {})
            our_v = our_value(our_row, cols)
            diff = pct = None
            if cbo_v is not None and our_v is not None:
                diff = our_v - cbo_v
                pct = (diff / cbo_v * 100.0) if cbo_v else None
            records.append((section, label, mapping_str, yr, cbo_v, our_v, diff, pct))

    # Write tidy CSV.
    with open(OUT_CSV, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["section", "line_item", "our_mapping", "year",
                    "cbo", "ours", "diff", "pct_diff"])
        for section, label, mapping_str, yr, cbo_v, our_v, diff, pct in records:
            w.writerow([
                section, label, mapping_str, yr,
                "" if cbo_v is None else round(cbo_v, 1),
                "" if our_v is None else round(our_v, 1),
                "" if diff is None else round(diff, 1),
                "" if pct is None else round(pct, 1),
            ])
    print("Wrote %s (%d rows)\n" % (OUT_CSV, len(records)))

    # Wide views for spot-check years.
    by_key = {(r[1], r[3]): r for r in records}
    for yr in PRINT_YEARS:
        print("=" * 78)
        print("Calendar year %d  ($ billions; returns in millions)" % yr)
        print("=" * 78)
        print("%-44s %10s %10s %8s" % ("Line item", "CBO", "Ours", "%diff"))
        print("-" * 78)
        last_section = None
        for section, label, cols in MAPPING:
            r = by_key[(label, yr)]
            cbo_v, our_v, pct = r[4], r[5], r[7]
            if section != last_section:
                print("[%s]" % section)
                last_section = section
            print("%-44s %10s %10s %8s" % (
                label[:44],
                "-" if cbo_v is None else "%10.1f" % cbo_v,
                "-" if our_v is None else "%10.1f" % our_v,
                "-" if pct is None else "%7.1f%%" % pct,
            ))
        print()


if __name__ == "__main__":
    main()
