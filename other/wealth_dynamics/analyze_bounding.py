#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# analyze_bounding.py
#
# s x M bounding exercise for CG +5pp under carryover basis. Reads the
# conventional totals/receipts.csv for both M-pipelines and computes, over the
# 10-fiscal-year window FY2027-2036 (policy CY2026, receipts FY-booked t+1):
#
#   (1) HEADLINE     conventional reform estimate by tax type = conv(s) - baseline
#                    at M=identity across s in {0,.25,.5,.75,1}.
#   (2) s-IMPACT     conv(s) - conv(s=0) at M=identity (baseline cancels) -- the
#                    cross-base cost of turning the saving-financing channel on.
#   (3) M-ENVELOPE   conv_uniform(s) - conv_identity(s) by tax type -- how much
#                    the within-age mobility assumption can move the answer.
#
# Baseline has no behavior module so only its STATIC totals are populated; it is
# used only for the (1) headline (cancels in (2),(3)). Totals-only -> light;
# safe to run on the login node (no R, tiny I/O). Writes summary CSVs + JSON.
# -----------------------------------------------------------------------------
import csv, os, json

ROOT = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT  = "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator/other/wealth_dynamics"
WIN  = set(range(2027, 2037))           # 10 FY: 2027..2036

REV = ["revenues_income_tax", "revenues_payroll_tax", "revenues_estate_tax",
       "revenues_wealth_tax", "revenues_corp_tax", "revenues_vat", "revenues_other"]
OUTLAY = "outlays_tax_credits"
TAX_TYPES = {"income": "revenues_income_tax", "payroll": "revenues_payroll_tax",
             "estate": "revenues_estate_tax", "wealth": "revenues_wealth_tax"}

S_VALUES = [("s00", 0.0), ("s25", 0.25), ("s50", 0.50), ("s75", 0.75), ("s100", 1.0)]

def load(path):
    if not os.path.exists(path):
        return None
    with open(path) as f:
        return list(csv.DictReader(f))

def winsum(rows, col):
    if rows is None:
        return 0.0
    tot = 0.0
    for r in rows:
        try:
            y = int(float(r["year"]))
        except (KeyError, ValueError):
            continue
        if y in WIN and r.get(col) not in (None, ""):
            tot += float(r[col])
    return tot

def receipts(vintage, scen, kind="conventional"):
    return load(os.path.join(ROOT, vintage, scen, kind, "totals", "receipts.csv"))

def total_net(rows):
    return sum(winsum(rows, c) for c in REV) - winsum(rows, OUTLAY)

def by_type(rows):
    # receipts.csv values are already in $ BILLIONS (no rescaling)
    d = {tt: winsum(rows, col) for tt, col in TAX_TYPES.items()}
    d["outlays"] = winsum(rows, OUTLAY)
    d["total"]   = total_net(rows)
    return d

# --- baseline (static levels; conventional absent for no-behavior baseline) ----
base_id = receipts("cgcarry_bound_identity", "baseline", "static")
base_un = receipts("cgcarry_bound_uniform",  "baseline", "static")
B = by_type(base_id)

# --- grid: conventional receipts per (M, s) ------------------------------------
grid = {"identity": {}, "uniform": {}}
for tag, sval in S_VALUES:
    scen = f"cgcarry_{tag}"
    ri = receipts("cgcarry_bound_identity", scen, "conventional")
    grid["identity"][tag] = by_type(ri) if ri else None
    if tag == "s00":
        # s=0 is channel-dormant -> M-independent; reuse identity's s00
        grid["uniform"][tag] = grid["identity"][tag]
    else:
        ru = receipts("cgcarry_bound_uniform", scen, "conventional")
        grid["uniform"][tag] = by_type(ru) if ru else None

TT = ["income", "payroll", "estate", "total"]

def fmt_row(label, d, ref=None):
    cells = []
    for tt in TT:
        if d is None:
            cells.append(f"{'NA':>12}")
        elif ref is None:
            cells.append(f"{d[tt]:>12.2f}")
        else:
            cells.append(f"{(d[tt]-ref[tt]):>12.3f}")
    return f"{label:<10}" + "".join(cells)

hdr = f"{'':<10}" + "".join(f"{tt:>12}" for tt in TT)

print("\n" + "="*70)
print("CG +5pp (top, CARRYOVER basis) -- 10y conventional revenue, $B, FY2027-36")
print("="*70)
print(f"baseline (static levels): income={B['income']:.1f}  payroll={B['payroll']:.1f}  estate={B['estate']:.2f}  (total={B['total']:.1f})")

print("\n(1) HEADLINE  reform - baseline   [M = identity]")
print(hdr)
for tag, sval in S_VALUES:
    print(fmt_row(f"s={sval:g}", grid["identity"][tag], ref=B))

print("\n(2) s-IMPACT  conv(s) - conv(s=0)  [M = identity]  (baseline cancels)")
print(hdr)
ref0 = grid["identity"]["s00"]
for tag, sval in S_VALUES:
    print(fmt_row(f"s={sval:g}", grid["identity"][tag], ref=ref0))

print("\n(3) M-ENVELOPE  uniform - identity   per s   (size of the M sensitivity)")
print(hdr)
for tag, sval in S_VALUES:
    gi, gu = grid["identity"][tag], grid["uniform"][tag]
    if gi is None or gu is None:
        print(fmt_row(f"s={sval:g}", None)); continue
    diff = {tt: gu[tt] - gi[tt] for tt in TT}
    print(fmt_row(f"s={sval:g}", diff, ref={tt: 0 for tt in TT}))

# --- dump CSVs + JSON ----------------------------------------------------------
rows_out = []
for M in ("identity", "uniform"):
    for tag, sval in S_VALUES:
        g = grid[M][tag]
        if g is None:
            continue
        rows_out.append(dict(M=M, s=sval,
                             income=g["income"], payroll=g["payroll"],
                             estate=g["estate"], wealth=g["wealth"],
                             outlays=g["outlays"], total=g["total"],
                             d_income=g["income"]-B["income"],
                             d_payroll=g["payroll"]-B["payroll"],
                             d_estate=g["estate"]-B["estate"],
                             d_total=g["total"]-B["total"]))
with open(os.path.join(OUT, "bounding_grid.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows_out[0].keys()))
    w.writeheader(); w.writerows(rows_out)

with open(os.path.join(OUT, "bounding_grid.json"), "w") as f:
    json.dump(dict(window=[2027, 2036], baseline=B, grid=grid, rows=rows_out), f, indent=1)

print("\nwrote bounding_grid.csv / bounding_grid.json to", OUT)
