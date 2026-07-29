#!/usr/bin/env python3
"""hundred_pct_ceiling.py -- the "100% rate" reframe of the top-rate-needed
calculation. DUAL of mtr_table_data.py, same machinery, same bases.

Old framing (mtr_table_data.py / top_rate_needed_calcs.md Table 1): solve for
the top rate that closes the FY2027 deficit from group X alone ->
    needed_rate = current_marginal_on_slice + deficit / base_above_T
which produced impossible >100% rates for the narrow groups.

This file fixes the rate at 100% and reads off the revenue instead:
    a TOTAL federal marginal rate of 100% INCLUSIVE of current law, on income
    ABOVE the group's threshold. The extra collected over current law is
        (1 - current_marginal_on_slice) * base_above_T,
    reported as a share of GDP. Naive upper bound: no behavioral response, no
    state taxes. It is the same identity as Table 1 with needed_rate := 100%.

Same construction as mtr_table_data.py:
  * groups are self-ranked by CASH income; the bracket sits at that cash floor.
  * two bases: ordinary taxable income (txbl_inc - txbl_kg) and AGI.
  * base above a threshold T = sum over ALL units of weight * max(0, base_i - T).
  * current marginal on the slice = income-weighted MTR over the above-T dollars
    (ordinary: mtr_wages1; AGI: composition blend of mtr_wages1 and mtr_kg_lt).

Pure stdlib, reads the baseline static detail directly (as mtr_table_data.py does).
"""
import csv, json, os

V3        = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3"
DETAIL    = f"{V3}/baseline/static/detail/2027.csv"
GDP_FY_B  = 33315.19   # FY2027 nominal GDP, Macro-Projections v3 2026022522 (gdp_fy)
DEFICIT_B = 1900.0     # FY2027 deficit, CBO Feb 2026

# cash-income group floors (same as mtr_table_data.py GROUPS)
GROUPS = [
    ("Top 10%",   231730.0),
    ("Top 5%",    345205.0),
    ("Top 1%",    911405.0),
    ("Top 0.1%",  4236780.0),
    ("Top 0.01%", 21391035.0),
]

h = next(csv.reader(open(DETAIL))); ix = {n: i for i, n in enumerate(h)}
C = lambda n: ix[n]
recs = []
with open(DETAIL) as fh:
    r = csv.reader(fh); next(r)
    for x in r:
        w = float(x[C("weight")])
        if w <= 0:
            continue
        agi = float(x[C("agi")]); txi = float(x[C("txbl_inc")]); tkg = float(x[C("txbl_kg")])
        mo  = float(x[C("mtr_wages1")]); mcg = float(x[C("mtr_kg_lt")])
        ord_txbl = max(0.0, txi - tkg)
        ord_agi  = max(0.0, agi - tkg); pref = max(0.0, tkg); den = ord_agi + pref
        m_agi = (ord_agi * mo + pref * mcg) / den if den > 0 else mo
        recs.append((w, agi, ord_txbl, mo, m_agi))

def bracket(T, base):
    # base above T and the income-weighted current marginal rate on that slice.
    tot = 0.0; taxnow = 0.0
    for w, agi, ordt, mo, ma in recs:
        v = (agi - T) if base == "agi" else (ordt - T)
        if v <= 0:
            continue
        m = ma if base == "agi" else mo
        tot += w * v; taxnow += w * v * m
    base_B = tot / 1e9
    cur = taxnow / tot if tot else 0.0          # current marginal on the slice (fraction)
    return base_B, cur

rows = []
for name, T in GROUPS:
    bo, curo = bracket(T, "ord")
    ba, cura = bracket(T, "agi")
    # revenue from a 100% TOTAL marginal rate (inclusive of current law):
    rev_ord = (1.0 - curo) * bo
    rev_agi = (1.0 - cura) * ba
    rows.append(dict(group=name, thr=T,
                     ord_base_B=bo, ord_cur=100 * curo,
                     ord_rev_B=rev_ord, ord_rev_pct_gdp=100 * rev_ord / GDP_FY_B,
                     agi_base_B=ba, agi_cur=100 * cura,
                     agi_rev_B=rev_agi, agi_rev_pct_gdp=100 * rev_agi / GDP_FY_B))

deficit_pct_gdp = 100 * DEFICIT_B / GDP_FY_B
json.dump(dict(year=2027, gdp_fy_B=GDP_FY_B, deficit_B=DEFICIT_B,
               deficit_pct_gdp=deficit_pct_gdp, rows=rows),
          open(os.path.join(os.path.dirname(__file__), "hundred_pct_ceiling.json"), "w"), indent=1)

def money(v):
    return f"${v/1e6:.2f}M" if v >= 1e6 else f"${v/1e3:.0f}k"
print(f"FY2027 GDP ${GDP_FY_B/1000:.1f}T | deficit ${DEFICIT_B/1000:.1f}T = {deficit_pct_gdp:.1f}% of GDP")
print("100% TOTAL federal marginal rate (incl. current law) on income above each "
      "group's threshold; revenue raised over current law, as % of GDP.\n")
hdr = ["Group", "Cash thr", "Ord base $B", "Ord cur mtr", "Ord %GDP",
       "AGI base $B", "AGI cur mtr", "AGI %GDP"]
print("  ".join(f"{c:>13}" for c in hdr))
for r in rows:
    print("  ".join([
        f"{r['group']:>13}", f"{money(r['thr']):>13}",
        f"{r['ord_base_B']:>13,.0f}", f"{r['ord_cur']:>12.0f}%", f"{r['ord_rev_pct_gdp']:>12.1f}%",
        f"{r['agi_base_B']:>13,.0f}", f"{r['agi_cur']:>12.0f}%", f"{r['agi_rev_pct_gdp']:>12.1f}%",
    ]))
print(f"\nYardstick: FY2027 deficit = {deficit_pct_gdp:.1f}% of GDP.")
