#!/usr/bin/env python3
"""
collect_form_results.py -- assemble the functional-form memo's numbers from the
Part C run outputs. Pure stdlib (login-node safe; reads CSVs only, no compute).

Reads, for each response form (levels/logs):
  - form_memo_<form>:   {rate2pp, deemed, rate2pp_deemed} x {static, conventional}
                        revenue_estimates.csv, summed over FY2027-2036 (W10).
                        -> Table 1 (three experiments x two forms, 10-yr revenue).
  - form_laffer_<form>: 17 cells (cg_{00,05,10,15,20,25}pp_{stepup,carryover,
                        deemed}), conventional revenue over FY2027-2036 ($B,
                        appendix) AND FY2047-2056 as % of third-decade GDP
                        (the fig5 convention, the figure metric).

Emits form_ab_results.json (embedded by the memo/figure -- numbers never
hand-entered) and prints a markdown summary. Missing runs are skipped with a
note, so this can be run incrementally as vintages complete.

Usage:  python3 other/kg_model_tests/form_ab/collect_form_results.py
"""
import csv, json, os

LOCAL = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1"
OUT   = "other/kg_model_tests/form_ab/form_ab_results.json"

W10 = range(2027, 2037)      # FY2027-2036 (memo revenue window)
W3D = range(2047, 2057)      # FY2047-2056 (third decade, fig5 %GDP metric)
GDP_3D_B = 822024.9          # cumulative FY2047-56 GDP $B (atlas meta gdp_fy_decades[2])

FORMS   = ["levels", "logs"]
MEMO_SC = ["rate2pp", "deemed", "rate2pp_deemed"]
RATE_PP = [0, 5, 10, 15, 20, 25]          # +pp on the 20% base -> 20..45%
REGIMES = ["stepup", "carryover", "deemed"]


def read_rev(vintage, scenario, leg):
    """revenue_estimates.csv -> {year: delta $B}; None if the file is absent."""
    p = os.path.join(LOCAL, vintage, scenario, leg, "supplemental",
                     "revenue_estimates.csv")
    if not os.path.exists(p):
        return None
    out = {}
    with open(p) as f:
        for row in csv.DictReader(f):
            if row["total"] in ("NA", "", "NaN"):
                continue
            out[int(row["year"])] = float(row["total"])
    return out


def wsum(d, win):
    return sum(v for y, v in d.items() if y in win) if d else None


results = {"window_memo": "FY2027-2036", "window_fig": "FY2047-2056",
           "gdp_3d_B": GDP_3D_B, "table1": {}, "laffer": {}, "missing": []}

# ---- Table 1: three experiments x two forms -------------------------------
for form in FORMS:
    vint = f"form_memo_{form}"
    results["table1"][form] = {}
    for sc in MEMO_SC:
        st = read_rev(vint, sc, "static")
        cv = read_rev(vint, sc, "conventional")
        if st is None or cv is None:
            results["missing"].append(f"{vint}/{sc}")
            continue
        results["table1"][form][sc] = {
            "static_10y_B": round(wsum(st, W10), 1),
            "conventional_10y_B": round(wsum(cv, W10), 1),
        }

# ---- Laffer grid: 17 cells x two forms ------------------------------------
for form in FORMS:
    vint = f"form_laffer_{form}"
    results["laffer"][form] = {}
    for regime in REGIMES:
        pts = []
        for pp in RATE_PP:
            rate = 20 + pp
            # stepup has no cg_00pp scenario (it IS baseline -> zero delta anchor).
            if pp == 0 and regime == "stepup":
                pts.append({"rate_pct": rate, "conv_10y_B": 0.0,
                            "conv_3d_B": 0.0, "conv_3d_pct_gdp": 0.0})
                continue
            sc = f"cg_{pp:02d}pp_{regime}"
            cv = read_rev(vint, sc, "conventional")
            if cv is None:
                results["missing"].append(f"{vint}/{sc}")
                continue
            r3 = wsum(cv, W3D)
            pts.append({
                "rate_pct": rate,
                "conv_10y_B": round(wsum(cv, W10), 1),
                "conv_3d_B": round(r3, 1),
                "conv_3d_pct_gdp": round(100.0 * r3 / GDP_3D_B, 4),
            })
        results["laffer"][form][regime] = pts

with open(OUT, "w") as f:
    json.dump(results, f, indent=2)

# ---- markdown summary ------------------------------------------------------
def fmt(x):
    return "   n/a" if x is None else f"{x:8.1f}"

print(f"wrote {OUT}\n")
print("## Table 1 -- three experiments x two forms, 10-yr conventional revenue ($B)")
print("| experiment | levels static | levels conv | logs static | logs conv |")
print("|---|---:|---:|---:|---:|")
for sc in MEMO_SC:
    lv = results["table1"].get("levels", {}).get(sc, {})
    lg = results["table1"].get("logs", {}).get(sc, {})
    print(f"| {sc} | {fmt(lv.get('static_10y_B'))} | {fmt(lv.get('conventional_10y_B'))} "
          f"| {fmt(lg.get('static_10y_B'))} | {fmt(lg.get('conventional_10y_B'))} |")

print("\n## Laffer grid -- conventional revenue, third decade as % of GDP")
for regime in REGIMES:
    print(f"\n### {regime}")
    print("| rate % | levels %GDP | logs %GDP |")
    print("|---:|---:|---:|")
    lv = {p["rate_pct"]: p for p in results["laffer"].get("levels", {}).get(regime, [])}
    lg = {p["rate_pct"]: p for p in results["laffer"].get("logs", {}).get(regime, [])}
    for pp in RATE_PP:
        rate = 20 + pp
        a = lv.get(rate, {}).get("conv_3d_pct_gdp")
        b = lg.get(rate, {}).get("conv_3d_pct_gdp")
        print(f"| {rate} | {fmt(a)} | {fmt(b)} |")

if results["missing"]:
    print(f"\n_MISSING (runs not yet complete): {len(results['missing'])} scenario-legs_")
    for m in results["missing"][:40]:
        print(f"  - {m}")
