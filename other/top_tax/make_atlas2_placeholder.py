#!/usr/bin/env python3
"""
Synthetic placeholder atlas2_data.json for the client stub track: the REAL
8-lever meta (from levers.py — no run data needed) with synthetic surrogate
numbers of plausible scale, so atlas2.html + check_atlas2_render.js can go
structural-green while the dials batch cooks. meta.placeholder is set —
the render harness fails a shipping check on it (run with --allow-placeholder
during the stub track only). Retired once fit_surrogate.py writes the real
file.

Usage: python3 other/top_tax/make_atlas2_placeholder.py
Writes: other/top_tax/atlas2_data_placeholder.json
"""

import itertools
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import levers as L
import fit_surrogate as F

OUT = os.path.join(HERE, "atlas2_data_placeholder.json")

GROUPS = ["Bottom quintile", "Second quintile", "Middle quintile",
          "Fourth quintile", "80-90", "90-99", "99-99.9", "Top 0.1%"]
COMPS = ["income_tax", "payroll", "estate", "deemed", "wealth", "corp", "vat", "other"]
DEFS = ["hs", "expanded"]
M_ETR = len(DEFS) * len(GROUPS) * len(COMPS)
N_DEC = 3
M_OF = {"ct": N_DEC, "cy": 30, "ch": 7 * N_DEC,
        "st": N_DEC, "sy": 30, "sh": 7 * N_DEC, "etr": M_ETR}
# synthetic decade growth factors (nominal-GDP-ish) for d2/d3 vs d1
DEC_GROW = [1.0, 1.55, 2.4]

# synthetic decade-1 static solo total at each lever's REF ($B) + conv survival
REF_ST = {"ord": 700, "cg": 900, "corp": 900, "wealth": 2600, "deemed": 300,
          "estate": 300, "qbi": 700, "taxmax": 1200}
SURV = {"ord": 0.89, "cg": 0.24, "corp": 0.88, "wealth": 0.67, "deemed": 2.5,
        "estate": 1.0, "qbi": 1.0, "taxmax": 0.9}
HEAD_MIX = {  # rough destination mix per lever (sums to 1)
    "ord": [0.9, -0.05, 0.1, 0, 0.03, 0, 0.02], "cg": [-0.1, 1.1, 0, 0, 0, 0, 0],
    "corp": [0.05, -0.08, 0, 1.05, -0.02, 0, 0], "wealth": [-0.09, -0.05, 0, 0, -0.02, 1.16, 0],
    "deemed": [0.05, 0.9, 0, 0, 0.05, 0, 0], "estate": [-0.02, -0.03, 0, 0, 1.05, 0, 0],
    "qbi": [1.0, 0, 0, 0, 0, 0, 0], "taxmax": [-0.05, 0, 1.05, 0, 0, 0, 0]}


def dial_strength(key, vals):
    """Smooth synthetic dial strength, 1.0 at REF."""
    lv = L.BY_KEY[key]
    if lv["interp"] == "binary":
        return 1.0
    if lv["interp"] == "ladder":
        return {"carryover": 0.45, "deemed": 1.0}[vals["pos"]]
    p0 = lv["params"][0]
    r = (vals["rate"] - p0["off"]) / (p0["ref"] - p0["off"])
    if key == "wealth":
        import math
        t = math.log(vals["thr"] / 50e6) / math.log(20.0)   # 0 at 50M, 1 at 1B
        r *= (1.0 - 0.72 * t)
    if key == "estate":
        p1 = lv["params"][1]
        e = (p1["off"] - vals["exem"]) / (p1["off"] - p1["ref"])
        r = 0.55 * r + 0.45 * e + 0.35 * r * e
    return r


def vec(qid, key, scale):
    st = REF_ST[key] * scale
    ct = st * SURV[key]
    if qid == "ct":
        return [ct * gf for gf in DEC_GROW]
    if qid == "st":
        return [st * gf for gf in DEC_GROW]
    if qid in ("cy", "sy"):
        tot = ct if qid == "cy" else st
        out = []
        for d, gf in enumerate(DEC_GROW):
            ramp = [0.6 + 0.08 * i for i in range(10)]
            s = sum(ramp)
            out.extend(tot * gf * r / s for r in ramp)
        return out
    if qid in ("ch", "sh"):
        tot = ct if qid == "ch" else st
        return [tot * gf * w for gf in DEC_GROW for w in HEAD_MIX[key]]
    # etr: small positive deltas concentrated at the top
    out = []
    for _d in DEFS:
        for gi in range(len(GROUPS)):
            for c in COMPS:
                bump = (gi / (len(GROUPS) - 1.0)) ** 3 * scale
                out.append(round(0.9 * bump if c == "income_tax" else
                                 0.25 * bump if c in ("wealth", "estate") else 0.0, 3))
    return out


def main():
    solo, g = {}, {}
    for key in L.LEVER_KEYS:
        cells = F.lever_grid_states(key)
        solo[key] = {qid: [] for qid in F.QIDS}
        gvals = []
        for vals, is_zero in cells:
            s = 0.0 if is_zero else dial_strength(key, vals)
            for qid in F.QIDS:
                solo[key][qid].append([round(v, 3) for v in vec(qid, key, s)])
            # per-decade g: g_d(ref)=1 for every decade, decade-dependent
            # curvature off-ref so cross-decade wiring bugs can't cancel out
            gvals.append([round(max(0.0, s) ** (1 + 0.15 * d), 6)
                          for d in range(N_DEC)])
        g[key] = gvals

    pairs = {}
    pair_ct = {("cg", "deemed"): 250, ("ord", "cg"): 60, ("wealth", "estate"): -8,
               ("cg", "wealth"): -25, ("cg", "estate"): 4, ("ord", "qbi"): 25,
               ("corp", "cg"): -20, ("wealth", "deemed"): -15}
    for a, b in itertools.combinations(L.LEVER_KEYS, 2):
        base = pair_ct.get((a, b), pair_ct.get((b, a), 3.0))
        entry = {}
        for qid in F.QIDS:
            if qid in ("ct", "st"):
                f = 1.0 if qid == "ct" else 0.6
                entry[qid] = [round(base * f * gf, 3) for gf in DEC_GROW]
            elif qid in ("cy", "sy"):
                entry[qid] = [round(base * 0.1 * gf, 3)
                              for gf in DEC_GROW for _ in range(10)]
            elif qid in ("ch", "sh"):
                entry[qid] = [round(base * gf * w, 3) for gf in DEC_GROW
                              for w in [0.5, 0.5, 0, 0, 0, 0, 0]]
            else:
                entry[qid] = [0.0] * M_ETR
        pairs[f"{a}|{b}"] = entry

    triples = {}
    for a, b, c in itertools.combinations(L.CLUSTER, 3):
        triples[f"{a}|{b}|{c}"] = {
            "ct": [round(12.0 * gf, 3) for gf in DEC_GROW],
            "st": [round(5.0 * gf, 3) for gf in DEC_GROW],
            "cy": [round(1.2 * gf, 3) for gf in DEC_GROW for _ in range(10)],
            "sy": [round(0.5 * gf, 3) for gf in DEC_GROW for _ in range(10)],
            "ch": [round(6.0 * gf, 3) * w for gf in DEC_GROW
                   for w in [1, 1, 0, 0, 0, 0, 0]],
            "sh": [round(2.5 * gf, 3) * w for gf in DEC_GROW
                   for w in [1, 1, 0, 0, 0, 0, 0]],
            "etr": [0.0] * M_ETR}

    lever_meta = []
    for lv in L.LEVERS:
        params = []
        for p, (pk, knots, scale) in zip(lv["params"], F.lever_axes(lv["key"])):
            pm = dict(key=p["key"], label=p["label"], unit=p["unit"], fmt=p["fmt"],
                      off=p["off"], ref=p["ref"], knots=knots, scale=scale)
            if "positions" in p:
                pm["positions"] = p["positions"]
            else:
                pm["min"], pm["max"] = p["min"], p["max"]
                pm["anchors"] = p["anchors"]
            params.append(pm)
        kind = {"linear1d": "continuous", "bilinear": "continuous",
                "ladder": "discrete", "binary": "binary"}[lv["interp"]]
        lever_meta.append(dict(key=lv["key"], label=lv["label"], grp=lv["grp"],
                               kind=kind, interp=lv["interp"], cluster=lv["cluster"],
                               params=params))

    etr_base = {}
    for d in DEFS:
        by = {}
        for gi, grp in enumerate(GROUPS):
            lev = 5 + 30 * (gi / (len(GROUPS) - 1.0))
            by[grp] = [round(lev * w, 3) for w in
                       [0.45, 0.3, 0.02, 0.0, 0.0, 0.15, 0.03, 0.05]]
        etr_base[d] = {"2027": by}

    data = dict(
        meta=dict(schema=3, window=[2027, 2056],
                  decades=[[2027, 2036], [2037, 2046], [2047, 2056]],
                  gdp_fy_decades=[397364.0, 615000.0, 952000.0],
                  dist_years=[2027], etr_slice={}, etr_income_defs=DEFS,
                  etr_comps=COMPS, etr_groups=GROUPS, levers=lever_meta,
                  surrogate=dict(quantities=F.QIDS, m=M_OF,
                                 heads_order=F.HEADS_ORDER,
                                 ref={k: L.REF[k] for k in L.LEVER_KEYS},
                                 cluster=L.CLUSTER,
                                 validation=None, checks=None),
                  provenance=dict(vintage="SYNTHETIC", git_sha="", date="2026-07-09",
                                  n_scenarios=0, corp_note="synthetic"),
                  units="$B; ETRs in percent (deltas in pp)",
                  placeholder="synthetic stub — awaiting top_tax_dials_v1"),
        etr_base=etr_base,
        surrogate=dict(solo=solo, g=g, pairs=pairs, triples=triples))

    # self-consistent holdout fixtures (actual = model prediction; bound trivial)
    probe_states = [
        {"cg": {"rate": 33.0}, "deemed": {"pos": "deemed"},
         "estate": {"rate": 55.0, "exem": 7e6}},
        {"ord": {"rate": 47.0}, "wealth": {"rate": 3.0, "thr": 200e6},
         "qbi": {"on": 1}, "taxmax": {"on": 1}},
        {k: L.REF[k] for k in L.LEVER_KEYS},
    ]
    checks = []
    for i, st in enumerate(probe_states):
        checks.append(dict(
            id=f"synth{i}", state=st,
            conv_totals=[round(v, 3) for v in F.eval_state(data, "ct", st)],
            static_totals=[round(v, 3) for v in F.eval_state(data, "st", st)]))
    data["meta"]["surrogate"]["checks"] = checks
    data["meta"]["surrogate"]["validation"] = dict(
        quiz=dict(n=len(checks), max_pct=0.0, median_pct=0.0),
        corners=dict(n=0, max_pct=None), byyear_max_pct=0.0, heads_max_b=0.0,
        etr_max_pp=0.0, bound_pct=0.5, bounds_pct=[0.5, 0.5, 0.5],
        static_bounds_pct=[0.5, 0.5, 0.5], hard_bar_pct=2.0, passed=True,
        date="2026-07-10")

    with open(OUT, "w") as fh:
        json.dump(data, fh, separators=(",", ":"))
    print(f"Wrote {OUT} ({os.path.getsize(OUT) / 1e3:.0f} KB)")


if __name__ == "__main__":
    main()
