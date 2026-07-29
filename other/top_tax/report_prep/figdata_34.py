#!/usr/bin/env python3
"""Emit Figs 3&4 by-head, third-decade (FY2047-56), % of GDP numbers from an
atlas2_data.json surrogate. Ordered marginal decomposition, deemed folded into
the cg head via the standard CY pref-rate carve already baked into ch/sh.

Usage: python3 figdata_34.py <atlas2_data.json>
Validate by running against the v2 backup and diffing vs the hardcoded
figures_3_4.html numbers before trusting v3 output.
"""
import json, os, sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
os.chdir(os.path.join(os.path.dirname(__file__), ".."))
import fit_surrogate as F

HEADS = ["iit", "cg", "pay", "corp", "est", "wealth", "other"]
DIDX = 2  # third decade (FY2047-56)


def heads_raw(data, state, qid):
    """Decade-3 head dict for a state, as % of GDP (before the deemed carve)."""
    gdp = data["meta"]["gdp_fy_decades"][DIDX]
    vec = F.eval_state(data, qid, state)  # 21 = 3 decades x 7 heads, decade-major
    seg = vec[DIDX * len(HEADS):(DIDX + 1) * len(HEADS)]
    return {h: 100.0 * v / gdp for h, v in zip(HEADS, seg)}


def heads_at(data, state, qid):
    """Decade-3 head dict with the deemed-at-death carve applied: the deemed
    lever's own contribution to the iit head is moved into the cg base (gains
    taxed at death are economically a capital-gains levy). Deemed portion =
    iit(state) - iit(state without deemed)."""
    h = heads_raw(data, state, qid)
    if "deemed" in state:
        base = {k: v for k, v in state.items() if k != "deemed"}
        portion = h["iit"] - heads_raw(data, base, qid)["iit"]
        h["iit"] -= portion
        h["cg"] += portion
    return {k: round(v, 3) for k, v in h.items()}


def sub(a, b):
    return {h: round(a[h] - b[h], 3) for h in a}


def nonzero(d):
    return {h: v for h, v in d.items() if abs(v) >= 0.0005}


def main(path):
    data = json.load(open(path))
    cg25 = {"cg": {"rate": 25.0}}
    cg25corp25 = {"cg": {"rate": 25.0}, "corp": {"rate": 25.0}}
    dm = {"deemed": {"pos": "deemed"}}
    dm_cg = {"deemed": {"pos": "deemed"}, "cg": {"rate": 25.0}}
    dm_cg_corp = {"deemed": {"pos": "deemed"}, "cg": {"rate": 25.0}, "corp": {"rate": 25.0}}

    def show(label, state_conv, state_stat=None, minus_conv=None, minus_stat=None):
        c = heads_at(data, state_conv, "ch")
        s = heads_at(data, state_stat if state_stat else state_conv, "sh")
        if minus_conv:
            c = sub(c, heads_at(data, minus_conv, "ch"))
        if minus_stat:
            s = sub(s, heads_at(data, minus_stat, "sh"))
        print(f"  {label:16} conv={nonzero(c)}")
        print(f"  {'':16} stat={nonzero(s)}")

    print(f"### {os.path.basename(path)}  (decade-3 FY2047-56, % of GDP)")
    print("FIG 3 (step-up):")
    show("Capital gains", cg25)
    show("+ Corporate", cg25corp25, minus_conv=cg25, minus_stat=cg25)
    show("Package", cg25corp25)
    print("FIG 4 (deemed):")
    show("Gains at death", dm)
    show("+ Capital gains", dm_cg, minus_conv=dm, minus_stat=dm)
    show("+ Corporate", dm_cg_corp, minus_conv=dm_cg, minus_stat=dm_cg)
    show("Package", dm_cg_corp)


if __name__ == "__main__":
    main(sys.argv[1])
