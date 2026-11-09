#!/usr/bin/env python3
"""
Declarative lever spec for the top_tax DIALS build (atlas v2 surrogate).

Pure data module: dicts + tiny YAML-template callables. Consumed by
build_dial_runs.py (scenario generation) and fit_surrogate.py /
validate_surrogate.py (fitting + holdout validation). No compute — safe to
import on the login node.

Author-final lever list (2026-07-09), 11 parameters = 8 levers (the death
lever gained an exclusion sub-axis 2026-07-28). Off-state = current law
everywhere; increases only. The surrogate formula these anchors feed is
    V(x) = sum_i f_i(x_i) + sum_{i<j} I_ij g_i g_j + sum_flagged T_ijk g_i g_j g_k
with f_i piecewise-linear through the solo anchors (bilinear for the 2-param
levers, ladder for discrete), I_ij measured once per pair at the REF settings,
and g_i(x) = the lever's static solo total10 at x divided by its ref value.

YAML text blocks reuse the factorial's idioms verbatim (build_factorial.py):
cg + deemed co-habit pref.yaml; wealth/estate re-anchor indexation the same
way. corp writes corp.yaml only — the rate is scored on-model
(src/sim/corp_rate.R) and no lever touches the economy column.
"""

import math
import os

import yaml

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))

# --------------------------------------------------------------------------- #
# Run-level constants (mirror build_factorial.py)
# --------------------------------------------------------------------------- #
YEARS = "2027:2057"          # SURPRISE-ENACTMENT convention (author call 2026-07-10):
                             # start AT the effective year, no 2026 lead-in, so the
                             # kg timing overlay never sees a pre-enactment year —
                             # no 2026 acceleration surge and no 2027 retiming
                             # crater (which made the impact-year realized ETR dip
                             # below baseline for ANY cg increase). Trade-off,
                             # accepted: FY2027 misses the Oct-Dec 2026 anticipation
                             # revenue (the usual start-at-t-1 rule is deliberately
                             # waived here). One year past the 2056 window for the
                             # FY-lagged estate/wealth legs.
DIST_YEARS = "2027 2036"     # distribution years; the atlas2 ETR panel ships 2027 only
                             # (heir files end 2055 — dist_years must stay <= 2055)
# Behavior-leg alternatives: folders under config/scenarios/behavior/
# alternatives/ naming the response stack, one cell in place of the old
# space-delimited module list. top_tax_full is the bathtub, conversion, entity
# shifting, evasion, charity at -0.5 and the estate reporting response;
# top_tax_full_wealth adds wealth-tax concealment. What each contains, and the
# order it runs in, is in the folder's behavior.yaml and in src/sim/behavior.R.
# (The estate reporting response was split into its own module on 2026-07-16 and
# the shipped rows patched by hand; that is part of top_tax_full now, so the
# drift cannot recur.)
BEHAVIOR_BASE = "top_tax_full"
BEHAVIOR_WEALTH = "top_tax_full_wealth"
MTR_VARS = "wages1 wages2 part_active sole_prop1 scorp_active kg_lt rent char_cash net_worth estate"
MTR_TYPES = " ".join(["nextdollar"] * len(MTR_VARS.split()))

# THE TOP-TAX BATCHES USE NO OFF-MODEL ESTIMATES AT ALL. Author instruction,
# 2026-07-26. Both lever states name the model default economy.
#
# The corporate rate went ON-MODEL on 2026-07-23 (src/sim/corp_rate.R): the rate
# in corp.yaml is scored against the CBO corporate level, statically as the
# mechanical (t-t0)*B0 change and conventionally with the CPSS avoidance ETI. So
# the off-model wedge these two folders used to pin is now a SECOND source for
# revenue the model already produces, and receipts add both. A corp scenario
# naming the old placeholder pin double-counts the rate hike -- roughly $49B in
# 2027 rising to $96B, twice.
#
# The corp-OFF pin cost nothing to drop: v4 20250925/baseline is all zeros, as is
# the v5 default, so that state was always a no-op. The corp-ON pin is the real
# change, and dropping it is the point -- it hands the corporate rate to the
# on-model machinery, which is where it now belongs.
#
# Naming the default also un-strands these batches. An economy alternative can pin
# an interface VINTAGE but not its VERSION, and a vintage lives under a version
# directory, so both old pins became unreachable when Off-Model-Estimates went to
# v5 on 2026-07-22 and nothing said so until launch.
CORP_ON_ECONOMY = "default"
CORP_OFF_ECONOMY = "default"

# --------------------------------------------------------------------------- #
# Current-law 2027 estate exemption, computed (NOT hardcoded — indexation).
# Replicates the model's machinery exactly (src/data/tax_law.R):
#   value(t) = floor(base_value * index(t-1)/index(base_year) / incr) * incr
# with base_value = the 2026-regime published amount from baseline estate.yaml
# and index = the chained-CPI (ccpiu_irs) level series from Macro-Projections.
# --------------------------------------------------------------------------- #
MACRO_ROOT = ("/nfs/roberts/project/pi_nrs36/shared/model_data/"
              "Macro-Projections/v3/2026022522/baseline")


def _ccpiu_irs(year):
    import csv
    for f in ("historical.csv", "projections.csv"):
        with open(os.path.join(MACRO_ROOT, f), newline="") as fh:
            for r in csv.DictReader(fh):
                if int(r["year"]) == year:
                    return float(r["ccpiu_irs"])
    raise KeyError(year)


def current_estate_exemption_2027():
    path = os.path.join(REPO, "config", "scenarios", "tax_law", "default", "estate.yaml")
    with open(path) as fh:
        raw = yaml.safe_load(fh)
    ex = raw["exemption"]
    base_value = ex["value"]["2026"]                    # OBBBA regime anchor
    base_year = ex["i_base_year"]["2026"]               # 2025
    incr = ex["i_increment"]                            # 10000
    assert ex["i_direction"] == -1                      # floor
    factor = _ccpiu_irs(2026) / _ccpiu_irs(base_year)   # lag convention: t-1 index
    return math.floor(base_value * factor / incr) * incr


ESTATE_CUR = current_estate_exemption_2027()            # $15,390,000 on 2026022522
# m1/m2 evenly spaced between current and $5M, rounded to the $10k law increment
ESTATE_M1 = round((2 * ESTATE_CUR + 5_000_000) / 3, -4)
ESTATE_M2 = round((ESTATE_CUR + 2 * 5_000_000) / 3, -4)

# --------------------------------------------------------------------------- #
# YAML template callables. Each returns {filename: [text blocks]} for the
# lever at the given param values. Blocks from co-habiting levers targeting
# the same file are concatenated by the generator (factorial convention).
# --------------------------------------------------------------------------- #

CORP_RATE_COMMENT = (
    '# top_tax corp dial: top corporate rate 21% -> {pct}% from 2027.\n'
    '# This rate is the WHOLE corporate story now: src/sim/corp_rate.R scores it\n'
    '# against the CBO corporate level, and it feeds the entity-shifting wedge.\n'
    '# No off-model estimate is involved.\n'
    '# Full baseline series retained (subparameter-override replaces wholesale).\n')

# The corporate rate became an ON-MODEL parameter on 2026-07-23. Before that a
# corp scenario was purely an off-model switch and wrote no YAML, which is what
# these generators used to believe; the 114 files the change needs were patched
# in by hand and the generators were not told until 2026-07-26. The lever writes
# this file and NOTHING else -- the off-model pin was dropped the same day, since
# with the rate on-model the two sources add and the hike gets counted twice.
def corp_files(vals):
    r = _pct(vals["rate"])
    return {"corp.yaml": [CORP_RATE_COMMENT.format(pct=f"{vals['rate']:g}") + f"""\
rate:
  value:
    '2014': 0.35
    '2018': 0.21
    '2027': {r}
"""]}


# Appended to pref.yaml, LAST, whenever the cg lever is on: lifting the cap that
# holds the preferred rate schedule below the ordinary one, without which a top
# preferred rate above the top ordinary rate cannot bind. Position at the end of
# the file is not meaningful -- it is where the hand patch put it, and staying
# there is what keeps regenerate-and-diff clean.
NO_ORD_CAP_BLOCK = """\
no_ord_cap:
  value: 1
"""


def _pct(x):
    """Rate in percent -> YAML decimal string (39.6 -> '0.396')."""
    return f"{x / 100:.6g}"


def ord_files(vals):
    r = _pct(vals["rate"])
    return {"ord.yaml": [f"""\
# top_tax dial ORD: top ordinary rate -> {vals['rate']}% from 2027 (top bracket only)
rates:
  value:
    '2014': [0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
    '2018': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
    '2027': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, {r}]
"""]}


def cg_files(vals):
    r = _pct(vals["rate"])
    return {"pref.yaml": [f"""\
# top_tax dial CG: top preferred (LTCG/qual-div) rate -> {vals['rate']}% from 2027
rates:
  value:
    '2014': [0.0, 0.15, 0.20]
    '2027': [0.0, 0.15, {r}]
"""]}


DEEMED_CODES = {"carryover": 1, "deemed": 2}


def deemed_files(vals):
    code = DEEMED_CODES[vals["pos"]]
    blk = [f"# top_tax dial DEEMED: {vals['pos']} at death, all asset classes, from 2027",
           "# (code 0=step-up baseline, 1=carryover, 2=deemed_realization)"]
    for asset in ("equities", "pass_throughs", "primary_home", "other_home", "re_fund"):
        blk.append(f"kg_death_regime_{asset}:")
        blk.append("  value:")
        blk.append("    '2014': 0")
        blk.append(f"    '2027': {code}")
    parts = ["\n".join(blk) + "\n"]
    # Per-decedent exclusion on top of §121: the first dollars of gain at death
    # keep step-up, married amounts double via the filing-status mapper. Zero
    # is current law for this parameter and writes nothing, which is what
    # keeps pre-exclusion scenario dirs byte-identical on regeneration.
    exem = float(vals.get("exem", 0.0))
    if exem:
        e = int(exem)
        parts.append(f"""\
# top_tax dial DEEMED exclusion: first ${e:,} of gain at death per decedent
# keeps step-up (on top of Sec. 121); married amount is double
kg_death_gain_excl_single:
  value:
    '2014': 0
    '2027': {e}
kg_death_gain_excl_married:
  value:
    '2014': 0
    '2027': {e * 2}
""")
    return {"pref.yaml": parts}


def wealth_files(vals):
    r, thr = _pct(vals["rate"]), int(vals["thr"])
    parts = [f"""\
# top_tax dial WEALTH: {vals['rate']}% annual net-worth tax above ${thr:,} from 2027
# (threshold applied per tax unit for all filing statuses)
rates:
  value:
    '2014': [0.0, 0.0]
    '2027': [0.0, {r}]
"""]
    for status in ("single", "married", "head"):
        parts.append(f"""\
brackets_{status}:
  value:
    '2014': [0, {thr}]
    '2027': [0, {thr}]
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default
""")
    return {"wealth.yaml": ["\n".join(parts)]}


def estate_files(vals):
    blocks = []
    if vals["exem"] != ESTATE_CUR:
        e = int(vals["exem"])
        blocks.append(f"""\
# top_tax dial ESTATE exemption -> ${e:,} at 2027, chained-CPI indexed after
# (re-anchored 2027 regime; pre-2027 path = current law)
exemption:
  value:
    '2014': 5340000
    '2018': 11180000
    '2026': 15000000
    '2027': {e}
  i_measure:
    '2013': cpi
    '2018': chained_cpi
  i_base_year:
    '2014': 2013
    '2018': 2017
    '2026': 2025
    '2027': 2026
  i_direction: -1
  i_increment: 10000
""")
    if vals["rate"] != 40:
        r = _pct(vals["rate"])
        blocks.append(f"""\
# top_tax dial ESTATE top rate -> {vals['rate']}% from 2027 (graduated ladder kept)
rates:
  value:
    '2014': [0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.30, 0.32, 0.34, 0.37, 0.39, 0.40]
    '2027': [0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.30, 0.32, 0.34, 0.37, 0.39, {r}]
""")
    return {"estate.yaml": blocks} if blocks else {}


def qbi_files(vals):
    return {"qbi.yaml": ["""\
# top_tax dial QBI: repeal Section 199A (QBI deduction) from 2027
rate:
  value:
    '2014': 0.0
    '2018': 0.2
    '2027': 0.0
min_value:
  value:
    '2014': 0
    '2026': 400
    '2027': 0
  i_measure:
    '1987': cpi
    '2017': chained_cpi
  i_base_year: 2025
  i_direction: 0
  i_increment: 5
"""]}


def taxmax_files(vals):
    return {"pr.yaml": ["""\
# top_tax dial TAXMAX: eliminate the OASDI taxable maximum from 2027.
# Earnings above the (unchanged, still-indexed) cap are taxed at the full
# 6.2% on both employee and employer sides; FICA and SECA read these same
# rate parameters, so self-employment earnings above the cap are covered too.
# Rates are unindexed scalars -- no i_ fields.
oasdi_ee_rates:
  value:
    '2014': [0.062, 0.0]
    '2027': [0.062, 0.062]
oasdi_er_rates:
  value:
    '2014': [0.062, 0.0]
    '2027': [0.062, 0.062]
"""]}


# --------------------------------------------------------------------------- #
# The 8 levers, canonical order. Cluster = {cg, wealth, deemed, estate}
# (triples measured there only).
#
# Per lever:
#   key/label/grp     : identity + UI grouping ('rate' | 'struct')
#   kind              : 'yaml' (writes reform YAML) | 'ome' (switches the
#                       off-model pin) | 'both'
#   cluster           : triples flag
#   wealth_behavior   : insert wealth/avoidance into the behavior stack
#   interp            : 'linear1d' | 'bilinear' | 'ladder' | 'binary'
#   params            : UI + interpolation axes. Continuous params carry
#                       off/min/max/anchors/ref (+ scale: 'log' for $ axes
#                       interpolated on log(x)). Ladder carries positions.
#   files(vals)       : YAML text blocks for a run at vals (kind='yaml')
# --------------------------------------------------------------------------- #
LEVERS = [
    dict(key="ord", label="Top ordinary rate", grp="rate", kind="yaml",
         cluster=False, wealth_behavior=False, interp="linear1d", files=ord_files,
         params=[dict(key="rate", label="Top rate", unit="%", fmt="pct",
                      # min = current law (37): the input can sit at zero
                      # effect; f interpolates law->39.6 like cg's 20->25 leg
                      off=37.0, min=37.0, max=50.0,
                      anchors=[39.6, 44.8, 50.0], ref=44.8)]),
    dict(key="cg", label="Capital gains & dividends top rate", grp="rate", kind="yaml",
         cluster=True, wealth_behavior=False, interp="linear1d", files=cg_files,
         params=[dict(key="rate", label="Top rate", unit="%", fmt="pct",
                      off=20.0, min=20.0, max=50.0,
                      anchors=[25.0, 30.0, 35.0, 40.0, 45.0, 50.0], ref=40.0)]),
    # Corp anchors extended past 28 on 2026-07-27, once the rate went on-model:
    # the single-anchor grid dated from the off-model wedge, which was linear in
    # the rate change by construction. Ref stays 28 so pairs and triples remain
    # comparable across vintages.
    dict(key="corp", label="Corporate rate", grp="rate", kind="yaml",
         cluster=False, wealth_behavior=False, interp="linear1d", files=corp_files,
         params=[dict(key="rate", label="Rate", unit="%", fmt="pct",
                      off=21.0, min=21.0, max=35.0,
                      anchors=[24.5, 28.0, 31.5, 35.0], ref=28.0)]),
    # Wealth anchors DENSIFIED by the 2026-07-09 patch loop (quiz failures):
    # the original rate {1..5} x thr {50M, 500M, 1B} grid overpredicted by
    # +5-11% at thresholds interior to the 50M->500M log segment (true
    # revenue-vs-log(thr) is convex) and underpredicted below 1% (Laffer
    # concavity in rate). Added thr {100M, 150M, 250M} and rate {0.5}.
    dict(key="wealth", label="Annual wealth tax", grp="rate", kind="yaml",
         cluster=True, wealth_behavior=True, interp="bilinear", files=wealth_files,
         params=[dict(key="rate", label="Rate", unit="%", fmt="pct",
                      off=0.0, min=0.0, max=5.0,
                      anchors=[0.5, 1.0, 2.0, 3.0, 4.0, 5.0], ref=2.0),
                 dict(key="thr", label="Threshold", unit="$", fmt="usd", scale="log",
                      off=50e6, min=50e6, max=1000e6,
                      anchors=[50e6, 100e6, 150e6, 250e6, 500e6, 1000e6], ref=50e6)]),
    # The exclusion axis was added 2026-07-28 (reviewer ask): a per-decedent
    # dollar exclusion under carryover or deemed realization, on top of §121.
    # interp='ladder_x' is the hybrid — discrete positions are still never
    # g-scaled (a real run per position), while the exclusion sub-axis
    # interpolates within a position via the within-position solo ratio.
    # exem ref stays 0 so every pre-exclusion pair/triple/stack remains valid.
    dict(key="deemed", label="Gains at death", grp="struct", kind="yaml",
         cluster=True, wealth_behavior=False, interp="ladder_x", files=deemed_files,
         params=[dict(key="pos", label="Treatment", unit="", fmt="pos",
                      positions=["off", "carryover", "deemed"], off="off", ref="deemed"),
                 dict(key="exem", label="Exclusion", unit="$", fmt="usd",
                      off=0.0, min=0.0, max=5e6,
                      anchors=[0.0, 1e6, 5e6], ref=0.0)]),
    dict(key="estate", label="Estate tax", grp="struct", kind="yaml",
         cluster=True, wealth_behavior=False, interp="bilinear", files=estate_files,
         params=[dict(key="rate", label="Top rate", unit="%", fmt="pct",
                      off=40.0, min=40.0, max=60.0,
                      anchors=[40.0, 50.0, 60.0], ref=50.0),
                 dict(key="exem", label="Exemption", unit="$", fmt="usd",
                      off=float(ESTATE_CUR), min=5e6, max=float(ESTATE_CUR),
                      anchors=[5e6, float(ESTATE_M2), float(ESTATE_M1), float(ESTATE_CUR)],
                      ref=float(ESTATE_M2))]),
    dict(key="qbi", label="Repeal QBI (§199A)", grp="struct", kind="yaml",
         cluster=False, wealth_behavior=False, interp="binary", files=qbi_files,
         params=[dict(key="on", label="Repeal", unit="", fmt="bool",
                      positions=[0, 1], off=0, ref=1)]),
    dict(key="taxmax", label="Eliminate SS taxable max", grp="struct", kind="yaml",
         cluster=False, wealth_behavior=False, interp="binary", files=taxmax_files,
         params=[dict(key="on", label="Eliminate", unit="", fmt="bool",
                      positions=[0, 1], off=0, ref=1)]),
]
LEVER_KEYS = [lv["key"] for lv in LEVERS]
CLUSTER = [lv["key"] for lv in LEVERS if lv["cluster"]]
BY_KEY = {lv["key"]: lv for lv in LEVERS}

# Reference settings (pairs/triples/full-stack measured here)
REF = {
    "ord":    {"rate": 44.8},
    "cg":     {"rate": 40.0},
    "corp":   {"rate": 28.0},
    "wealth": {"rate": 2.0, "thr": 50e6},
    "deemed": {"pos": "deemed", "exem": 0.0},
    "estate": {"rate": 50.0, "exem": float(ESTATE_M2)},
    "qbi":    {"on": 1},
    "taxmax": {"on": 1},
}


def is_off(key, vals):
    """True when vals is the lever's current-law off-state (f = 0)."""
    lv = BY_KEY[key]
    if lv["interp"] == "bilinear" and key == "wealth":
        return vals["rate"] == 0.0                       # rate-0 edge is identically 0
    if lv["interp"] == "ladder_x":
        return vals["pos"] == "off"                      # exclusion is moot when off
    return all(vals[p["key"]] == p["off"] for p in lv["params"])


def solo_cells(key):
    """Anchor cells for a lever's solo curve: [(vals, run_flag)].

    run_flag False = cell is identically zero by construction (current law)
    and is NOT run; its f rows are zero vectors. Bilinear levers return the
    COMPLETE anchor grid (axis product) so the client interpolates on a full
    lattice; wealth's rate axis excludes the off-value 0 (whole rate-0 edge
    implicit zero), estate's rate axis includes 40 (base broadening at the
    current rate is real revenue — only the (40, current-exemption) corner is
    zero).
    """
    lv = BY_KEY[key]
    if lv["interp"] == "linear1d":
        return [({"rate": a}, True) for a in lv["params"][0]["anchors"]]
    if lv["interp"] == "bilinear":
        p1, p2 = lv["params"]
        return [({p1["key"]: a1, p2["key"]: a2}, not is_off(key, {p1["key"]: a1, p2["key"]: a2}))
                for a1 in p1["anchors"] for a2 in p2["anchors"]]
    if lv["interp"] == "ladder":
        return [({"pos": p}, True) for p in lv["params"][0]["positions"] if p != "off"]
    if lv["interp"] == "ladder_x":
        p_pos, p_x = lv["params"]
        return [({"pos": p, p_x["key"]: a}, True)
                for p in p_pos["positions"] if p != "off"
                for a in p_x["anchors"]]
    if lv["interp"] == "binary":
        return [({"on": 1}, True)]
    raise ValueError(lv["interp"])


# --------------------------------------------------------------------------- #
# Pair corners: the pairs expected biggest (factorial evidence: cg+deemed
# +$580B, cg+ord, wealth spillovers, corp equity markdown) re-run at ONE
# off-reference dial corner each, to test the I_ij * g_i * g_j scaling away
# from ref. Held out of the fit; check (2) in validate_surrogate.py.
# --------------------------------------------------------------------------- #
PAIR_CORNERS = [
    ("cg", {"rate": 50.0}, "deemed", {"pos": "deemed"}),
    ("cg", {"rate": 30.0}, "wealth", {"rate": 3.0, "thr": 500e6}),
    ("cg", {"rate": 50.0}, "estate", {"rate": 60.0, "exem": 5e6}),
    ("wealth", {"rate": 4.0, "thr": 50e6}, "estate", {"rate": 60.0, "exem": 5e6}),
    ("wealth", {"rate": 1.0, "thr": 1000e6}, "deemed", {"pos": "deemed"}),
    ("ord", {"rate": 50.0}, "cg", {"rate": 30.0}),
    ("ord", {"rate": 50.0}, "qbi", {"on": 1}),
    ("corp", {"rate": 35.0}, "cg", {"rate": 30.0}),
    # Exclusion corners (2026-07-28): test that the within-position exclusion
    # ratio scales the deemed pair terms away from exem = 0.
    ("cg", {"rate": 50.0}, "deemed", {"pos": "deemed", "exem": 5e6}),
    ("wealth", {"rate": 2.0, "thr": 50e6}, "deemed", {"pos": "carryover", "exem": 1e6}),
]

# Quiz: seeded holdout packages, strictly excluded from fitting. Drawn in
# build_dial_runs.py (deterministic under seed): sizes 3-8, continuous params
# OFF-anchor at presentable values, >=4 packages with >=3 cluster members,
# >=2 with deemed=carryover inside a 3+ stack. deemed/qbi/taxmax enter at
# their discrete positions.
QUIZ = dict(seed=20260709, n=16, size_min=3, size_max=8,
            min_cluster_heavy=4, min_carryover=2,
            # Exclusion quiz packages (2026-07-28), drawn from their own seeded
            # stream so the original sixteen stay byte-identical: each contains
            # the death lever at an off-anchor exclusion inside a stack.
            n_exem=4, seed_exem=20260728)

# Patch scenarios appended after quiz failures (validate names the offending
# lever/pair -> add targeted anchors/corners here -> build_dial_runs.py
# --patch N emits dials_patch{N}.csv for a same-vintage additive run -> refit).
# NOTE 2026-07-09: patches 1-3 are FOLDED INTO the main build (wealth grid via
# the densified anchors above; pairco/all-triples/tripco emitted directly by
# build_scenarios()) — kept here as history for the pre-fix top_tax_dials_v1
# vintage only. New patches should start at "4".
# Each entry: dict(id=..., levers={key: vals, ...}, kind=...). kind='solo'
# rows join the solo grids; kind='pairco'/'tripco' rows fit per-position pair/
# triple vectors: DISCRETE POSITIONS ARE NEVER g-SCALED — every interaction
# term touching the deemed ladder is measured by a real run at each position
# (author ruling 2026-07-09: "any discrete non-ordinary change, run all the
# options").


def _nslug(x):
    return f"{float(x):g}".replace(".", "p")


PATCHES = {
    # Patch 1 (2026-07-09, first quiz round): wealth grid densification — the
    # 21 cells the anchor extension above added — plus carryover pair runs for
    # the four levers with the largest deemed interactions.
    "1": (
        [dict(id=f"s_wealth_r{_nslug(r)}_t{int(t / 1e6)}", kind="solo",
              levers={"wealth": {"rate": float(r), "thr": float(t)}})
         for r in (0.5, 1, 2, 3, 4, 5)
         for t in (50e6, 100e6, 150e6, 250e6, 500e6, 1000e6)
         if not (r in (1, 2, 3, 4, 5) and t in (50e6, 500e6, 1000e6))]
        + [dict(id=f"prco_{a}_deemed", kind="pairco",
                levers={a: REF[a], "deemed": {"pos": "carryover"}})
           for a in ("ord", "cg", "wealth", "estate")]
    ),
    # Patch 2 (2026-07-09, complete the discrete principle): the remaining
    # deemed pairs at carryover, plus the three cluster triples containing
    # deemed re-measured at carryover (no more g(carryover) scaling anywhere).
    # ALSO re-lists every patch-1 scenario: patch 1 ran without a baseline row
    # in its runscript, which broke Phase 3b post-processing (get_other_taxes
    # resolves the baseline OME interface from the runscript) — re-running is
    # idempotent for the already-complete sim outputs and backfills the
    # missing distribution files.
    "2": None,  # filled below (needs PATCHES["1"])
}
PATCHES["2"] = (
    PATCHES["1"]
    + [dict(id=f"prco_{a}_deemed", kind="pairco",
            levers={a: REF[a], "deemed": {"pos": "carryover"}})
       for a in ("corp", "qbi", "taxmax")]
    + [dict(id=f"tco_{a}_{b}_deemed", kind="tripco",
            levers={a: REF[a], b: REF[b], "deemed": {"pos": "carryover"}})
       for a, b in (("cg", "wealth"), ("cg", "estate"), ("wealth", "estate"))]
)

# Patch 3 (2026-07-09, author direction): measure ALL C(8,3) = 56 triples,
# not just the flagged cluster — deemed-containing triples at both positions.
# Kills the hand-flagged cluster as a concept: every triple is measured, the
# near-zero ones drop out of the shipped JSON automatically, and the only
# remaining assumptions are pair g-scaling on continuous dials (corner-
# tested) and 4-way-and-higher = 0 (quiz-tested).


def _patch3():
    import itertools
    have_triple = {("cg", "wealth", "deemed"), ("cg", "deemed", "estate"),
                   ("wealth", "deemed", "estate"), ("cg", "wealth", "estate")}
    have_tripco = {("cg", "wealth"), ("cg", "estate"), ("wealth", "estate")}
    out = []
    for trio in itertools.combinations(LEVER_KEYS, 3):
        if trio not in have_triple:
            out.append(dict(id="t3_" + "_".join(trio), kind="triple",
                            levers={k: REF[k] for k in trio}))
        if "deemed" in trio:
            others = tuple(k for k in trio if k != "deemed")
            if others not in have_tripco:
                lv = {k: REF[k] for k in others}
                lv["deemed"] = {"pos": "carryover"}
                out.append(dict(id="t3co_" + "_".join(others) + "_deemed",
                                kind="tripco", levers=lv))
    return out


PATCHES["3"] = _patch3()

# Patch 4 (2026-07-28, first v5 quiz round): the exclusion sub-axis broke the
# within-position ratio scaling of pair vectors — pc_cgr50_deemede5 missed by
# -30% and every failing quiz stacked cg with an excluded deemed regime. The
# ratio moves the interaction the way it moves the solo, but a large exclusion
# guts the interacting base faster than the solo. So extend the discrete
# principle to the exclusion anchors: measure every deemed pair at each
# nonzero exclusion anchor with a real run (kind='pairexem'); the evaluator
# interpolates between measured anchors instead of ratio-scaling.
PATCHES["4"] = [
    dict(id=f"prx_{a}_deemed_e{int(e / 1e6)}", kind="pairexem",
         levers={a: REF[a], "deemed": {"pos": "deemed", "exem": float(e)}})
    for a in LEVER_KEYS if a != "deemed"
    for e in (1e6, 5e6)
]
