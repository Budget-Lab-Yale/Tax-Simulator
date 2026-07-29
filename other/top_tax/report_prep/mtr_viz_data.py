#!/usr/bin/env python3
"""Build data for the 'marginal rate by income percentile + deficit-closing top
rate' figure. Reads the v3 baseline 2027 detail (per-record, weighted).

Current marginal rate per unit:
  ordinary:  mtr_wages1 (next-dollar-of-wages EMTR = the ordinary bracket rate)
  cap gains: mtr_kg_lt
  AGI (composite): income-weighted blend of the two by the unit's ordinary vs
                   preferential AGI composition.
Deficit-closing top rate (levels): a NEW marginal rate on income ABOVE the
  top-1% threshold only (nothing below taxed extra). tau = m_current_on_slice +
  deficit / (income above threshold).  FY2027 deficit = $1.9T.
"""
import csv, json, os

V3 = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_dials_30y_v3"
DEFICIT_B = 1900.0
OUT = os.path.join(os.path.dirname(__file__), "mtr_viz_data.json")

f = f"{V3}/baseline/static/detail/2027.csv"
h = next(csv.reader(open(f))); ix = {n: i for i, n in enumerate(h)}
C = lambda n: ix[n]

rows = []
with open(f) as fh:
    r = csv.reader(fh); next(r)
    for x in r:
        w = float(x[C("weight")])
        if w <= 0:
            continue
        agi = float(x[C("agi")]); txi = float(x[C("txbl_inc")]); tkg = float(x[C("txbl_kg")])
        mo = float(x[C("mtr_wages1")]); mcg = float(x[C("mtr_kg_lt")])
        ord_agi = max(0.0, agi - tkg); pref = max(0.0, tkg)
        denom = ord_agi + pref
        m_agi = (ord_agi * mo + pref * mcg) / denom if denom > 0 else mo
        ord_txbl = max(0.0, txi - tkg)
        rows.append(dict(w=w, agi=agi, ord_txbl=ord_txbl, m_ord=mo, m_cg=mcg, m_agi=m_agi))

totW = sum(r["w"] for r in rows)

def pct_curve(rankkey, mkey, nbins=100):
    """Weighted-average marginal rate (mkey) across nbins equal-weight bins by rankkey."""
    s = sorted(rows, key=lambda z: z[rankkey])
    edges = [totW * i / nbins for i in range(nbins + 1)]
    out = []; cw = 0.0; b = 0; num = 0.0; den = 0.0; floor = None
    for z in s:
        while b < nbins - 1 and cw >= edges[b + 1]:
            out.append(100.0 * num / den if den else 0.0); b += 1; num = den = 0.0
        if floor is None or z[rankkey] < floor:
            pass
        num += z["w"] * z[mkey]; den += z["w"]; cw += z["w"]
    out.append(100.0 * num / den if den else 0.0)
    return out

def required(rankkey, mkey):
    s = sorted(rows, key=lambda z: z[rankkey], reverse=True)
    cut = 0.01 * totW; cw = 0.0; grp = []; thr = None
    for z in s:
        if cw >= cut:
            break
        grp.append(z); cw += z["w"]; thr = z[rankkey]
    base_above = sum(z["w"] * (z[rankkey] - thr) for z in grp) / 1e9  # $B
    m_cur = (sum(z["w"] * (z[rankkey] - thr) * z[mkey] for z in grp)
             / sum(z["w"] * (z[rankkey] - thr) for z in grp))          # fraction
    incr = DEFICIT_B / base_above                                       # fraction
    return dict(threshold=thr, base_above_B=base_above, m_current=m_cur * 100,
                increment=incr * 100, tau_required=(m_cur + incr) * 100,
                group_weight_M=cw / 1e6)

data = dict(
    year=2027, deficit_B=DEFICIT_B, units_M=totW / 1e6,
    pct_curve_by_agi=dict(
        mtr_ordinary=pct_curve("agi", "m_ord"),
        mtr_agi=pct_curve("agi", "m_agi"),
        mtr_capgains=pct_curve("agi", "m_cg"),
    ),
    pct_curve_by_ord=dict(
        mtr_ordinary=pct_curve("ord_txbl", "m_ord"),
    ),
    required_top_rate=dict(agi=required("agi", "m_agi"),
                           ordinary=required("ord_txbl", "m_ord")),
)
json.dump(data, open(OUT, "w"), indent=1)

# summary
print(f"units {totW/1e6:.1f}M  deficit ${DEFICIT_B/1000:.1f}T (FY2027)")
for base, d in data["required_top_rate"].items():
    print(f"\n{base.upper()} base (top 1%, floor ${d['threshold']/1e6:.2f}M, income above = ${d['base_above_B']/1000:.2f}T)")
    print(f"  current avg marginal on that slice: {d['m_current']:.0f}%")
    print(f"  increment to close deficit:         +{d['increment']:.0f} pts")
    print(f"  REQUIRED top marginal rate (level):  {d['tau_required']:.0f}%")
cur = data["pct_curve_by_agi"]
print("\ncurrent avg marginal (AGI composite) at p50/p90/p99/p100:",
      round(cur["mtr_agi"][49], 1), round(cur["mtr_agi"][89], 1),
      round(cur["mtr_agi"][98], 1), round(cur["mtr_agi"][99], 1))
print(f"wrote {OUT}")
