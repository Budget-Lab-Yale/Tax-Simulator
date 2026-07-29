#!/usr/bin/env python3
"""Table: for each top group, a NEW top bracket at that group's cash-income
threshold, taxing income ABOVE it (nothing below), needed to close the FY2027
deficit statically. Two bases: ordinary taxable income, and AGI.

Base above a threshold T = sum over ALL units of weight*max(0, base_i - T)
(a bracket at $T applies to everyone with income above $T in that concept).
Required top rate = current income-weighted marginal on the above-T slice
                    + deficit / (base above T).
"""
import csv, json, os

V3 = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3"
DEFICIT_B = 1900.0  # FY2027

# cash-income group floors + self-ranked cash ETRs + 2027 cash income $B
# (metrics Sec 1/2/3, baseline). needed ETR = cash ETR + deficit / cash income.
GROUPS = [
    ("Top 10%",   231730.0,   24.0, 11531.0),
    ("Top 5%",    345205.0,   24.6,  8960.0),
    ("Top 1%",    911405.0,   26.2,  5212.0),
    ("Top 0.1%",  4236780.0,  29.2,  2520.0),
    ("Top 0.01%", 21391035.0, 30.0,  1194.0),
]

f = f"{V3}/baseline/static/detail/2027.csv"
h = next(csv.reader(open(f))); ix = {n: i for i, n in enumerate(h)}
C = lambda n: ix[n]
recs = []
with open(f) as fh:
    r = csv.reader(fh); next(r)
    for x in r:
        w = float(x[C("weight")])
        if w <= 0:
            continue
        agi = float(x[C("agi")]); txi = float(x[C("txbl_inc")]); tkg = float(x[C("txbl_kg")])
        mo = float(x[C("mtr_wages1")]); mcg = float(x[C("mtr_kg_lt")])
        ord_txbl = max(0.0, txi - tkg)
        ord_agi = max(0.0, agi - tkg); pref = max(0.0, tkg); den = ord_agi + pref
        m_agi = (ord_agi * mo + pref * mcg) / den if den > 0 else mo
        recs.append((w, agi, ord_txbl, mo, m_agi))

def bracket(T, base_ix, m_ix):
    base = 0.0; taxnow = 0.0
    for w, agi, ordt, mo, ma in recs:
        v = (agi if base_ix == "agi" else ordt) - T
        if v <= 0:
            continue
        m = ma if base_ix == "agi" else mo
        base += w * v; taxnow += w * v * m
    base_B = base / 1e9
    cur = 100.0 * taxnow / base if base else 0.0
    req = cur + 100.0 * DEFICIT_B / base_B if base_B else float("inf")
    return base_B, cur, req

rows = []
for name, T, etr, cash_inc in GROUPS:
    bo, mo_cur, ro = bracket(T, "ord", None)
    ba, ma_cur, ra = bracket(T, "agi", None)
    needed_etr = etr + 100.0 * DEFICIT_B / cash_inc
    rows.append(dict(group=name, thr=T, cash_etr=etr, cash_income_B=cash_inc,
                     needed_etr=needed_etr,
                     ord_base_T=bo, ord_cur=mo_cur, ord_req=ro,
                     agi_base_T=ba, agi_cur=ma_cur, agi_req=ra))

json.dump(dict(year=2027, deficit_B=DEFICIT_B, rows=rows),
          open(os.path.join(os.path.dirname(__file__), "mtr_table_data.json"), "w"), indent=1)

# print a readable table
def money(v):
    return f"${v/1e6:.2f}M" if v >= 1e6 else f"${v/1e3:.0f}k"
print(f"FY2027 deficit ${DEFICIT_B/1000:.1f}T  | new bracket at each group's cash threshold, income above it only\n")
hdr = ["Group", "Cash thr", "CashETR", "NeedETR", "NeedMTR-ord", "NeedMTR-AGI"]
print("  ".join(f"{c:>12}" for c in hdr))
for r in rows:
    print("  ".join([f"{r['group']:>12}", f"{money(r['thr']):>12}", f"{r['cash_etr']:>11.1f}%",
        f"{r['needed_etr']:>11.0f}%", f"{r['ord_req']:>11.0f}%", f"{r['agi_req']:>11.0f}%"]))
