#!/usr/bin/env python3
"""
Create a PLACEHOLDER corporate Off-Model-Estimate for the top_tax factorial.

This activates the on-model corporate-incidence channel for the corp->28% switch
so the campaign is runnable end-to-end. THE NUMBERS ARE FAKE -- a plausible
monotone 21->28-scale receipts path effective 2027 -- to be replaced by the
author's real OME later (just overwrite corporate column in revenues.csv; the
vintage/ID path and meta stay).

Writes to the SHARED PRODUCTION OME root (dependencies are always read from
production, regardless of the local flag):
  {prod}/model_data/Off-Model-Estimates/v4/{VINTAGE}/{ID}/
    revenues.csv         year,individual,payroll,corporate,estate,vat
    corporate_meta.yaml  gross_of_offset: true, provision_type: rate, beyond_horizon: extend

Run:  python3 other/top_tax/make_placeholder_ome.py   (pure file I/O; login-node safe)
"""

import csv
import os

OME_V4 = "/nfs/roberts/project/pi_nrs36/shared/model_data/Off-Model-Estimates/v4"
VINTAGE = "top_tax_corp_placeholder"
ID = "corp_28_2027"

FIRST_YEAR = 2017      # OME series start (matches existing files)
LAST_YEAR = 2100       # write out to 2100 so beyond_horizon:extend is genuinely permanent
ENACT = 2027           # corporate wedge turns on here -> t0 = 2027
BASE_2027 = 130.0      # $B, FAKE placeholder level (~21->28 scale; author replaces)
GROWTH = 1.039         # ~profits growth, matches the corp_rate_perm test path


def main():
    d = os.path.join(OME_V4, VINTAGE, ID)
    os.makedirs(d, exist_ok=True)

    rev = os.path.join(d, "revenues.csv")
    with open(rev, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["year", "individual", "payroll", "corporate", "estate", "vat"])
        for y in range(FIRST_YEAR, LAST_YEAR + 1):
            if y < ENACT:
                corp = 0.0
            else:
                corp = round(BASE_2027 * (GROWTH ** (y - ENACT)), 4)
            w.writerow([y, 0, 0, corp, 0, 0])

    meta = os.path.join(d, "corporate_meta.yaml")
    with open(meta, "w") as fh:
        fh.write(
            "gross_of_offset: true\n"
            "provision_type: rate\n"
            "beyond_horizon: extend\n"
            "tau_baseline: 0.21\n"
            "delta_tau:\n"
            "  '2027': 0.07\n"
            "produced_by: 'PLACEHOLDER top_tax corp 21->28 rate hike, effective 2027 "
            "-- FAKE numbers, replace corporate column with real OME'\n"
            "date: '2026-07-09'\n"
        )

    print(f"Wrote {rev}")
    print(f"Wrote {meta}")
    print(f"corp-ON runscript cols:  dep.Off-Model-Estimates.vintage={VINTAGE}  "
          f"dep.Off-Model-Estimates.ID={ID}")


if __name__ == "__main__":
    main()
