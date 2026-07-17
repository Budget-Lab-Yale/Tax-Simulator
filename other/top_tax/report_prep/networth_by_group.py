#!/usr/bin/env python3
"""
networth_by_group.py -- aggregate baseline net worth by income-percentile and
net-worth-percentile groups from the dials v2 baseline detail files (2027, 2036).

Ranking convention: tax-unit level, ranked by expanded_inc (income groups) or
net_worth (wealth groups), weighted by `weight`. NOTE this is a simple tax-unit
ranking -- the production distribution tables use their own ranking conventions,
so treat small differences vs distribution.csv cutoffs as convention, not error.

Pure stdlib; streams the ~158MB csv. Run via sbatch.
"""
import csv, sys

V2 = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v2"

def load(year):
    p = f"{V2}/baseline/static/detail/{year}.csv"
    rows = []
    with open(p) as f:
        r = csv.DictReader(f)
        for row in r:
            try:
                w = float(row["weight"])
                inc = float(row["expanded_inc"])
                nw = float(row["net_worth"])
                filer = row.get("dep_status", "0")
            except (ValueError, KeyError):
                continue
            # exclude dependent returns (zero wealth by construction)
            if filer not in ("0", "", "NA"):
                continue
            rows.append((w, inc, nw))
    return rows

def groups(rows, key_idx, topshares=(0.20, 0.10, 0.05, 0.01, 0.001, 0.0001)):
    rows = sorted(rows, key=lambda t: t[key_idx])
    W = sum(t[0] for t in rows)
    out = []
    for s in topshares:
        target = W * (1 - s)
        cum = 0.0
        tot_nw, tot_inc, n = 0.0, 0.0, 0.0
        floor_val = None
        for w, inc, nw in rows:
            if cum >= target:
                if floor_val is None:
                    floor_val = (inc if key_idx == 1 else nw)
                tot_nw += w * nw
                tot_inc += w * inc
                n += w
            cum += w
        out.append((s, floor_val, n, tot_inc, tot_nw))
    return W, out

for year in (2027, 2036):
    rows = load(year)
    print(f"\n===== {year} (non-dependent tax units) =====")
    for key_idx, label in ((1, "ranked by EXPANDED INCOME"), (2, "ranked by NET WORTH")):
        W, res = groups(rows, key_idx)
        print(f"\n-- {label} (total weight {W/1e6:.1f}M units) --")
        print(f"{'group':>10} | {'floor':>15} | {'n (M)':>7} | {'income $T':>9} | {'net worth $T':>12}")
        for s, fl, n, ti, tn in res:
            g = f"Top {s*100:g}%"
            print(f"{g:>10} | {fl:>15,.0f} | {n/1e6:>7.2f} | {ti/1e12:>9.2f} | {tn/1e12:>12.2f}")
    tot_nw = sum(w * nw for w, _, nw in rows)
    tot_inc = sum(w * inc for w, inc, _ in rows)
    print(f"\nTOTALS {year}: income ${tot_inc/1e12:.2f}T, net worth ${tot_nw/1e12:.2f}T")
