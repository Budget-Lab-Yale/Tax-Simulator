#!/usr/bin/env python3
"""
Extract the top_tax factorial batch into one JSON for the atlas explorer
(atlas_mockup.html -> real-data rewire).

Reads, per combo cNNN (NNN = 7-bit switch mask) plus baseline:
  {scenario}/{leg}/totals/receipts_full.csv        FY receipt levels by head
  {scenario}/{leg}/totals/1040.csv                 CY liab_pref/1250/collect (CG carve)
  {scenario}/{leg}/supplemental/revenue_estimates.csv   official total deltas
  {scenario}/static/supplemental/distribution_etrs.csv  ETR levels by percentile

Emits other/top_tax/atlas_data.json:
  meta      - window, cumulative FY GDP, switch legend, sources
  etr_base  - baseline ETR stack by component x percentile (hs / fixed /
              wealth_cit_vat / capital_income), dist years only
  combos    - keyed by bitmask int as string:
                label, static/conv {total10, byyear, heads10}, etr {year: {group: [comps]}}

Heads10 is the destination ledger: iit ordinary (residual after the CG carve,
net of credit outlays), cg (liab_pref + liab_1250 + liab_collect from
totals/1040.csv -- CY liability carved from the FY income-tax head, ordinary is
the residual so the head total is preserved), payroll, corp, estate, wealth,
other (vat + other).

Usage (pure file I/O -- safe on the login node):
  python3 other/top_tax/extract_atlas_data.py [vintage_root] [out_json]
Defaults:
  vintage_root = /nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_v1
  out_json     = other/top_tax/atlas_data.json
"""

import csv
import json
import os
import sys

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
DEFAULT_ROOT = "/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1/top_tax_v1"
DEFAULT_OUT = os.path.join(REPO, "other", "top_tax", "atlas_data.json")
MACRO = "/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline/projections.csv"
LEGEND = os.path.join(REPO, "config", "runscripts", "top_tax", "factorial_legend.csv")

WINDOW = list(range(2027, 2037))          # 10-yr budget window
DIST_YEARS = [2027, 2036]                 # ETR display years
SWITCHES = ["ord", "cg", "corp", "wealth", "deemed", "estate", "qbi"]  # bit i = 2**i

# ETR slice for the chart (all-in tier, baseline-fixed ranking), extracted
# under both income definitions: hs = accrual (Haig-Simons), expanded = cash
ETR_INCOME_DEFS = ["hs", "expanded"]
ETR_FILTER = dict(ranking="fixed",
                  taxes_included="wealth_cit_vat", corp_convention="capital_income",
                  group_dimension="Income percentile")
ETR_COMPS = ["income_tax", "payroll", "estate", "deemed", "wealth", "corp", "vat", "other"]

# receipts_full heads -> ledger heads (iit gets the CG carve subtracted later)
CG_1040_COLS = ["liab_pref", "liab_1250", "liab_collect"]


def read_csv(path):
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh))


def year_map(rows, col):
    return {int(r["year"]): float(r[col]) for r in rows if r[col] not in ("", "NA")}


def receipts_heads(totals_dir):
    """Per-year receipt levels by ledger head (pre-CG-carve) + credit outlays."""
    rf = read_csv(os.path.join(totals_dir, "receipts_full.csv"))
    out = {}
    for r in rf:
        y = int(r["year"])
        out[y] = dict(
            iit=float(r["revenues_income_tax"]) - float(r["outlays_tax_credits"]),
            pay=float(r["revenues_payroll_tax"]),
            corp=float(r["revenues_corp_tax"]),
            est=float(r["revenues_estate_tax"]),
            wealth=float(r["revenues_wealth_tax"]),
            other=float(r["revenues_vat"]) + float(r["revenues_other"]),
        )
    return out


def cg_carve(totals_dir):
    """Per-year CY preferential-rate liability (the CG/dividends carve)."""
    rows = read_csv(os.path.join(totals_dir, "1040.csv"))
    return {int(r["year"]): sum(float(r[c]) for c in CG_1040_COLS) for r in rows}


def leg_deltas(scen_dir, base_heads, base_cg, leg):
    """10-yr deltas vs the same-leg baseline: total, per-year, per-head."""
    tdir = os.path.join(scen_dir, leg, "totals")
    sdir = os.path.join(scen_dir, leg, "supplemental")
    heads = receipts_heads(tdir)
    cg = cg_carve(tdir)
    rev = year_map(read_csv(os.path.join(sdir, "revenue_estimates.csv")), "total")

    heads10 = {k: 0.0 for k in ("iit", "cg", "pay", "corp", "est", "wealth", "other")}
    for y in WINDOW:
        d_cg = cg[y] - base_cg[leg][y]
        heads10["cg"] += d_cg
        heads10["iit"] += (heads[y]["iit"] - base_heads[leg][y]["iit"]) - d_cg
        for k in ("pay", "corp", "est", "wealth", "other"):
            heads10[k] += heads[y][k] - base_heads[leg][y][k]

    byyear = [round(rev.get(y, 0.0), 3) for y in WINDOW]
    total10 = sum(rev.get(y, 0.0) for y in WINDOW)

    # reconciliation: official deltas vs receipts_full deltas (booking/rounding drift ok)
    heads_total = sum(heads10.values())
    drift = heads_total - total10
    return dict(total10=round(total10, 3), byyear=byyear,
                heads10={k: round(v, 3) for k, v in heads10.items()}), drift


def etr_rows(scen_dir):
    path = os.path.join(scen_dir, "static", "supplemental", "distribution_etrs.csv")
    if not os.path.exists(path):
        return []
    keep = []
    with open(path, newline="") as fh:
        for r in csv.DictReader(fh):
            if int(r["year"]) not in DIST_YEARS:
                continue
            if r["income_definition"] not in ETR_INCOME_DEFS:
                continue
            if any(r[k] != v for k, v in ETR_FILTER.items()):
                continue
            keep.append(r)
    return keep


def etr_pack(rows, which):
    """{income_def: {year: {group: [comps in ETR_COMPS order]}}}, percent, 3dp."""
    out = {}
    for r in rows:
        d = r["income_definition"]
        y = str(r["year"])
        grp = r["group"]
        vals = [round(100.0 * float(r[f"etr_{c}_{which}"]), 3) for c in ETR_COMPS]
        out.setdefault(d, {}).setdefault(y, {})[grp] = vals
    return out


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_ROOT
    out_path = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_OUT

    # GDP denominator: cumulative FY nominal GDP over the window
    gdp = year_map(read_csv(MACRO), "gdp_fy")
    gdp10 = sum(gdp[y] for y in WINDOW)

    legend = {}
    if os.path.exists(LEGEND):
        for r in read_csv(LEGEND):
            legend[int(r["bits"])] = r["switches_on"]

    # baseline writes totals under static only (no behavior); both legs delta
    # against the same baseline run
    base_totals = os.path.join(root, "baseline", "static", "totals")
    bh, bcg = receipts_heads(base_totals), cg_carve(base_totals)
    base_heads = {"static": bh, "conventional": bh}
    base_cg = {"static": bcg, "conventional": bcg}

    combos, missing, drift_warn = {}, [], []
    etr_base = None
    etr_groups = None
    for bits in range(1, 128):
        scen = f"c{bits:03d}"
        scen_dir = os.path.join(root, scen)
        need = [os.path.join(scen_dir, leg, "totals", "receipts_full.csv")
                for leg in ("static", "conventional")]
        need += [os.path.join(scen_dir, leg, "supplemental", "revenue_estimates.csv")
                 for leg in ("static", "conventional")]
        if not all(os.path.exists(p) for p in need):
            missing.append(scen)
            continue

        static, d_s = leg_deltas(scen_dir, base_heads, base_cg, "static")
        conv, d_c = leg_deltas(scen_dir, base_heads, base_cg, "conventional")
        for leg_name, d, tot in (("static", d_s, static["total10"]),
                                 ("conv", d_c, conv["total10"])):
            if abs(d) > max(1.0, 0.005 * abs(tot)):
                drift_warn.append(f"{scen}/{leg_name}: heads-vs-official drift {d:+.2f}B")

        rows = etr_rows(scen_dir)
        etr = etr_pack(rows, "reform") if rows else {}
        if rows and etr_base is None:
            etr_base = etr_pack(rows, "baseline")
            # preserve file group order
            seen = []
            for r in rows:
                if r["group"] not in seen:
                    seen.append(r["group"])
            etr_groups = seen

        combos[str(bits)] = dict(label=legend.get(bits, ""), static=static,
                                 conv=conv, etr=etr)

    meta = dict(
        vintage=root, window=[WINDOW[0], WINDOW[-1]], gdp10_fy=round(gdp10, 1),
        switches=SWITCHES, dist_years=DIST_YEARS,
        etr_slice=ETR_FILTER, etr_income_defs=ETR_INCOME_DEFS,
        etr_comps=ETR_COMPS, etr_groups=etr_groups,
        units="$B; ETRs in percent",
        n_combos=len(combos), missing=missing,
        notes="heads10.cg = CY liab_pref+1250+collect carve; iit = FY income-tax "
              "head net of credit outlays, minus the carve. total10/byyear from "
              "supplemental/revenue_estimates.csv (official).",
    )
    payload = dict(meta=meta, etr_base=etr_base, combos=combos)
    with open(out_path, "w") as fh:
        json.dump(payload, fh, separators=(",", ":"))

    print(f"Wrote {out_path} ({os.path.getsize(out_path)/1e6:.2f} MB)")
    print(f"combos: {len(combos)}/127; missing: {len(missing)}"
          + (f" ({', '.join(missing[:8])}...)" if missing else ""))
    for w in drift_warn[:20]:
        print("WARN", w)
    if len(drift_warn) > 20:
        print(f"... {len(drift_warn) - 20} more drift warnings")


if __name__ == "__main__":
    main()
