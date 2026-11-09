#!/usr/bin/env python3
"""
analyze_revmax.py  --  assemble the CG-rate x death-regime revmax result.

Reads each scenario's conventional (and static) revenue_estimates.csv from a
finished campaign vintage, cumulates the revenue delta over the 10y budget
window and the 30y horizon, lays out the three death-regime schedules, and marks
the revenue-maximizing point per regime per horizon.

Usage:  python3 other/top_tax/analyze_revmax.py [VINTAGE]
        (VINTAGE defaults to the value in scratch .../revmax_vintage.txt or the
         hardcoded fallback below.)

Metric note (OPEN author decision): reports BOTH undiscounted cumulative Δrev
and a placeholder-PV at PV_RATE. The argmax can move between them — swap PV_RATE
or drop PV once the metric is chosen.
"""
import csv
import os
import sys

OUTPUT_BASE = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
FALLBACK_VINTAGE = "202607081937"          # campaign B (authoritative)
ENACT = 2027
WIN_10Y = range(2027, 2037)                # FY27-36
WIN_30Y = range(2027, 2057)                # through FY56
PV_RATE = 0.03                             # placeholder real discount rate
REGIMES = ["stepup", "carryover", "deemed"]
PPS = [0, 5, 10, 15, 20, 25]
BASE_TOP_RATE = 0.20


def read_delta(path):
    """year -> total ($B) from a revenue_estimates.csv; {} if absent."""
    if not os.path.exists(path):
        return None
    out = {}
    with open(path) as f:
        for row in csv.DictReader(f):
            try:
                out[int(row["year"])] = float(row["total"])
            except (ValueError, KeyError):
                pass
    return out


def cumulate(delta, window):
    return sum(delta.get(y, 0.0) for y in window)


def pv(delta, window, rate):
    return sum(delta.get(y, 0.0) / (1 + rate) ** (y - ENACT) for y in window)


def main():
    vintage = sys.argv[1] if len(sys.argv) > 1 else FALLBACK_VINTAGE
    root = os.path.join(OUTPUT_BASE, vintage)
    print(f"vintage: {vintage}\nroot:    {root}\n")

    # rows[regime] = list of (pp, top_rate, cum10, cum30, pv10, pv30, ok)
    rows = {r: [] for r in REGIMES}
    missing = []
    for regime in REGIMES:
        for pp in PPS:
            if regime == "stepup" and pp == 0:
                # baseline anchor: zero delta by construction
                rows[regime].append((0, BASE_TOP_RATE, 0.0, 0.0, 0.0, 0.0, True))
                continue
            sid = f"cg_{pp:02d}pp_{regime}"
            p = os.path.join(root, sid, "conventional", "supplemental",
                             "revenue_estimates.csv")
            d = read_delta(p)
            if d is None:
                missing.append(sid)
                rows[regime].append((pp, BASE_TOP_RATE + pp/100, None, None, None, None, False))
                continue
            top = BASE_TOP_RATE + pp / 100
            rows[regime].append((pp, top,
                                 cumulate(d, WIN_10Y), cumulate(d, WIN_30Y),
                                 pv(d, WIN_10Y, PV_RATE), pv(d, WIN_30Y, PV_RATE),
                                 True))

    if missing:
        print(f"NOT YET WRITTEN ({len(missing)}): {', '.join(missing)}\n"
              f"(campaign still running? re-run when conventional receipts exist.)\n")

    # schedules
    for regime in REGIMES:
        print(f"=== {regime.upper()} ===")
        print(f"  {'top rate':>9} {'+pp':>4} {'cum10 $B':>10} {'cum30 $B':>10} {'pv10 $B':>9} {'pv30 $B':>9}")
        for pp, top, c10, c30, p10, p30, ok in rows[regime]:
            if not ok:
                print(f"  {top:>8.1%} {pp:>4} {'--':>10} {'--':>10} {'--':>9} {'--':>9}  (missing)")
            else:
                print(f"  {top:>8.1%} {pp:>4} {c10:>10.1f} {c30:>10.1f} {p10:>9.1f} {p30:>9.1f}")
        # argmax per horizon (over available points)
        for label, idx in (("cum10", 2), ("cum30", 3), ("pv30", 5)):
            avail = [r for r in rows[regime] if r[6]]
            if avail:
                best = max(avail, key=lambda r: r[idx])
                print(f"  revmax[{label}] -> +{best[0]}pp = top {best[1]:.1%}  (Δrev {best[idx]:.1f} $B)")
        print()

    print("Cross-regime read: compare revmax top rate across step-up / carryover /")
    print("deemed. The spread is the §B.1 finding — how far the revenue-maximizing")
    print("CG rate rises once the death-regime lock-in escape is closed.")


if __name__ == "__main__":
    main()
