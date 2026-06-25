#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# analyze_sm_bound.py  <id_vintage> <un_vintage> <tag:s,tag:s,...>
#
# Generalized s×M levels+delta printer. Reads conventional totals/receipts.csv
# for each scenario under the identity and uniform vintages, computes the 10-FY
# (2027-36) reform-minus-baseline estimate by tax type, and prints:
#   TABLE A  levels  M=identity
#   TABLE B  levels  M=uniform
#   TABLE C  delta   uniform - identity
# Baseline = identity-vintage STATIC totals (no-behavior baseline). s=0 is
# channel-dormant -> M-independent (uniform reuses identity's s=0). $B already.
# -----------------------------------------------------------------------------
import csv, os, sys

ROOT = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
WIN  = set(range(2027, 2037))
REV  = ["revenues_income_tax","revenues_payroll_tax","revenues_estate_tax",
        "revenues_wealth_tax","revenues_corp_tax","revenues_vat","revenues_other"]
OUTLAY = "outlays_tax_credits"
TT_COL = {"income":"revenues_income_tax","payroll":"revenues_payroll_tax",
          "estate":"revenues_estate_tax","wealth":"revenues_wealth_tax"}
TT = ["income","payroll","estate","wealth","total"]

id_v, un_v, spec = sys.argv[1], sys.argv[2], sys.argv[3]
SCEN = [(p.split(":")[0], float(p.split(":")[1])) for p in spec.split(",")]

def load(p):
    return list(csv.DictReader(open(p))) if os.path.exists(p) else None
def winsum(rows, col):
    if rows is None: return 0.0
    return sum(float(r[col]) for r in rows
              if int(float(r["year"])) in WIN and r.get(col) not in (None,""))
def receipts(v, scen, kind="conventional"):
    return load(os.path.join(ROOT, v, scen, kind, "totals", "receipts.csv"))
def by_type(rows):
    if rows is None: return None
    d = {tt: winsum(rows, c) for tt,c in TT_COL.items()}
    d["total"] = sum(winsum(rows,c) for c in REV) - winsum(rows, OUTLAY)
    return d

base = by_type(receipts(id_v, "baseline", "static"))
if base is None:
    print("FATAL: no baseline static receipts under", id_v); sys.exit(1)

def est(v, scen):                    # reform - baseline
    g = by_type(receipts(v, scen, "conventional"))
    return None if g is None else {tt: g[tt]-base[tt] for tt in TT}

A = {sv: est(id_v, scen) for scen,sv in SCEN}
B = {}
for scen,sv in SCEN:
    B[sv] = A[sv] if sv == 0 else est(un_v, scen)   # s=0 M-independent

def fmt(d, sign):
    if d is None: return f"{'  n/a':>10}"*len(TT)
    return "".join(f"{(('%+.2f'%d[t]) if sign else ('%.1f'%d[t])):>10}" for t in TT)
def table(title, D, sign):
    print("\n"+title)
    print(f"  {'s':>4} " + "".join(f"{t:>10}" for t in TT))
    for _,sv in SCEN:
        print(f"  {sv:>4g} {fmt(D.get(sv), sign)}")

print(f"\nbaseline (static, $B/10y): income {base['income']:.0f}  payroll {base['payroll']:.0f}  estate {base['estate']:.0f}  wealth {base['wealth']:.0f}")
table("TABLE A  LEVELS  M=identity   (reform - baseline, $B/10y)", A, False)
table("TABLE B  LEVELS  M=uniform    (reform - baseline, $B/10y)", B, False)
C = {sv: (None if (A.get(sv) is None or B.get(sv) is None) else {t: B[sv][t]-A[sv][t] for t in TT}) for _,sv in SCEN}
table("TABLE C  DELTA   uniform - identity  (= B - A)", C, True)
