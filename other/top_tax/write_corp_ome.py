#!/usr/bin/env python3
"""
Install the REAL corporate Off-Model-Estimate for the top_tax factorial,
replacing the fake placeholder written by make_placeholder_ome.py.

The author-supplied series is the corporate RECEIPTS WEDGE ($B, CY) from the
21 -> 28% rate hike, effective 2027 (unexpected). ONLY the first 10 years
(2027-2036) are used -- the author's out-year path (a ~7%/yr ramp to ~$373B by
2056) is a mistake for a permanent rate change and is discarded.

Permanence past year 10: hold the 2036 value ($100.5B) growing at ~4%/yr
(nominal profit / GDP proxy, i.e. pi) out to 2100. This is exactly what the
built-in CORP_PRICED_AS_PERMANENT mechanism does to the markdown price path
(continue the last nonzero level, growing with pi) -- baked into the file so it
(a) needs no env var threaded through 128 SLURM jobs, (b) can't silently fail to
a sunset, and (c) is verifiable in the CSV. The metadata `beyond_horizon:extend`
alone would NOT work: the baseline OME (20250925/baseline) runs to 2100, so the
wedge full_join pins file_last=2100 and a file stopping at 2036 reads as 0 for
2037-2100 (silent sunset) rather than extended.

The wedge baseline leg (20250925/baseline) is corporate = 0 across 2017-2100,
so this reform column IS the wedge. Only 2027-2036 enters the 10-yr reporting
window; the permanent tail exists solely so the perfect-foresight equity
markdown (corporate -> net worth -> estate/wealth) is priced as permanent
rather than sunsetting after a decade. The far tail is PV-negligible.

Run:  python3 other/top_tax/write_corp_ome.py   (pure file I/O; login-node safe)
"""

import csv
import os

OME_V4 = "/nfs/roberts/project/pi_nrs36/shared/model_data/Off-Model-Estimates/v4"
VINTAGE = "top_tax_corp_placeholder"   # path/ID unchanged so runscript dep cols still resolve
ID = "corp_28_2027"

FIRST_YEAR = 2017      # OME series start (matches existing files)
LAST_YEAR = 2100       # write to 2100; beyond_horizon:extend + Gordon handle 2101+
ENACT = 2027
TAIL_GROWTH = 1.04     # ~pi (nominal profit/GDP); mirrors CORP_PRICED_AS_PERMANENT

# Author-supplied corporate receipts wedge ($B, CY), 2027-2036 ONLY (10 years).
# Out-year (2037+) author numbers discarded as a bad ramp; extended below instead.
WEDGE_2027_2036 = [
    48.8, 85.7, 91.3, 91.8, 94.2, 96.1, 96.1, 96.9, 98.3, 100.5,   # 2027-2036
]
LAST_DATA_YEAR = 2027 + len(WEDGE_2027_2036) - 1   # 2036


def corp_for(year):
    if year < ENACT:
        return 0.0
    if year <= LAST_DATA_YEAR:
        return WEDGE_2027_2036[year - ENACT]
    # permanent tail: hold the last given (2036) value, growing at ~pi
    return round(WEDGE_2027_2036[-1] * (TAIL_GROWTH ** (year - LAST_DATA_YEAR)), 4)


def main():
    assert len(WEDGE_2027_2036) == 10, "expected 2027-2036 (10 years)"
    d = os.path.join(OME_V4, VINTAGE, ID)
    os.makedirs(d, exist_ok=True)

    rev = os.path.join(d, "revenues.csv")
    with open(rev, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["year", "individual", "payroll", "corporate", "estate", "vat"])
        for y in range(FIRST_YEAR, LAST_YEAR + 1):
            w.writerow([y, 0, 0, corp_for(y), 0, 0])

    meta = os.path.join(d, "corporate_meta.yaml")
    with open(meta, "w") as fh:
        fh.write(
            "gross_of_offset: true\n"
            "provision_type: rate\n"
            "beyond_horizon: extend\n"
            "tau_baseline: 0.21\n"
            "delta_tau:\n"
            "  '2027': 0.07\n"
            "produced_by: 'top_tax corp 21->28 rate hike, effective 2027 (unexpected) "
            "-- REAL author OME wedge 2027-2036 only; 2037-2100 held at 2036 level "
            "growing 4%/yr for permanence (out-year author ramp discarded)'\n"
            "date: '2026-07-09'\n"
        )

    print(f"Wrote {rev}")
    print(f"Wrote {meta}")


if __name__ == "__main__":
    main()
