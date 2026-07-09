#!/usr/bin/env python3
"""
Build the top_tax 7-switch binary factorial (VISION.md v3, D8).

Emits, under the repo:
  - config/scenarios/tax_law/top_tax/factorial/c{NNN}/   (127 reform dirs)
  - config/runscripts/top_tax/factorial.csv               (128 rows incl. baseline)

Each of 127 non-empty switch combinations gets a tax_law directory containing
ONLY the changed YAML files (baseline supplies the rest via subparameter merge).
Switches that target the same file (CG rate + deemed both touch pref.yaml) are
MERGED into one file. The corporate switch is NOT a YAML — it toggles the
Off-Model-Estimates dependency columns in the runscript.

Switch bit order (bit i = 2**i):
  0 ord     top ordinary rate +5pp   (37% -> 42%), 2027
  1 cg      top CG/qual-div rate -> 39.6% statutory, 2027
  2 corp    corporate rate -> 28% via OME (runscript dep columns), 2027
  3 wealth  1% annual net-worth tax above $50M, 2027
  4 deemed  deemed realization at death (all asset classes), 2027
  5 estate  Clausing-Sarin estate: $5M exemption, flat 45% top, 2027
  6 qbi     repeal Section 199A (QBI deduction), 2027

Run from the repo root:  python3 other/top_tax/build_factorial.py
(Pure file I/O -- safe on the login node; no R/compute.)
"""

import csv
import os
import shutil

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #
REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
TAXLAW_ROOT = os.path.join(REPO, "config", "scenarios", "tax_law", "top_tax", "factorial")
RUNSCRIPT_DIR = os.path.join(REPO, "config", "runscripts", "top_tax")
RUNSCRIPT = os.path.join(RUNSCRIPT_DIR, "factorial.csv")
# tax_law column is relative to config/scenarios/tax_law/
TAXLAW_REL = "top_tax/factorial"

# --------------------------------------------------------------------------- #
# Run-level constants
# --------------------------------------------------------------------------- #
YEARS = "2026:2037"          # FY lead-in 2026 + one year past the 2036 window
DIST_YEARS = "2027 2036"     # ETR/distribution display years: impact year + fully-phased terminal
# Behavior modules (behavior scout, 2026-07-09). Pinned order kg -> conversion
# -> entity -> evasion; charity + wealth/avoidance order-free. wealth/avoidance
# added only when the wealth-tax switch is on.
BEHAVIOR_BASE = "kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker charity/50"
BEHAVIOR_WEALTH = "kg_dynamics/turnover conversion/sigma entity_shifting/pearce_prisinzano evasion/debacker wealth/avoidance charity/50"
# MTRs the modules read (net_worth for wealth/avoidance + bathtub). Full list on
# every row so baseline_mtrs covers every module's needs.
MTR_VARS = "wages1 wages2 part_active sole_prop1 scorp_active kg_lt rent char_cash net_worth"
MTR_TYPES = " ".join(["nextdollar"] * len(MTR_VARS.split()))

# Corporate OME dependency columns.
# ON  -> the placeholder vintage (make_placeholder_ome.py): activates the channel.
# OFF -> the default OME (all-zero corporate) -> channel dormant, no warning.
CORP_ON_VINTAGE = "top_tax_corp_placeholder"
CORP_ON_ID = "corp_28_2027"
CORP_OFF_VINTAGE = "20250925"
CORP_OFF_ID = "baseline"

# Wealth financing: leave blank so the auto-applied calibrated `default` profile
# runs (bathtub on model-wide). Set to "none" to force off.
S_COL = ""

# --------------------------------------------------------------------------- #
# Switch definitions.  Each YAML switch maps target-filename -> YAML text block
# (top-level subparameter keys only; the generator prepends one '---' header per
# file and concatenates blocks from all on-switches that target that file).
# --------------------------------------------------------------------------- #

ORD_RATES = """\
# top_tax switch ORD: top ordinary rate +5pp from 2027 (37% -> 42%; top bracket only)
rates:
  value:
    '2014': [0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
    '2018': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37]
    '2027': [0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.42]
"""

CG_RATES = """\
# top_tax switch CG: top preferred (LTCG/qual-div) rate -> 39.6% from 2027
rates:
  value:
    '2014': [0.0, 0.15, 0.20]
    '2027': [0.0, 0.15, 0.396]
"""

DEEMED_REGIME = """\
# top_tax switch DEEMED: deemed realization at death, all asset classes, from 2027
# (code 0=step-up baseline, 1=carryover, 2=deemed_realization)
kg_death_regime_equities:
  value:
    '2014': 0
    '2027': 2
kg_death_regime_pass_throughs:
  value:
    '2014': 0
    '2027': 2
kg_death_regime_primary_home:
  value:
    '2014': 0
    '2027': 2
kg_death_regime_other_home:
  value:
    '2014': 0
    '2027': 2
kg_death_regime_re_fund:
  value:
    '2014': 0
    '2027': 2
"""

WEALTH_YAML = """\
# top_tax switch WEALTH: 1% annual net-worth tax above $50M from 2027
# ($50M threshold applied per tax unit for all filing statuses)
rates:
  value:
    '2014': [0.0, 0.0]
    '2027': [0.0, 0.01]
brackets_single:
  value:
    '2014': [0, 50000000]
    '2027': [0, 50000000]
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default
brackets_married:
  value:
    '2014': [0, 50000000]
    '2027': [0, 50000000]
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default
brackets_head:
  value:
    '2014': [0, 50000000]
    '2027': [0, 50000000]
  i_measure: default
  i_base_year: default
  i_direction: default
  i_increment: default
"""

ESTATE_YAML = """\
# top_tax switch ESTATE: Clausing-Sarin estate -- $5M exemption, flat 45% top,
# re-anchored to 2027 (from the clausing/07_estate layer, 2030 -> 2027).
exemption:
  value:
    '2014': 5340000
    '2018': 11180000
    '2026': 15000000
    '2027': 5000000
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
rates:
  value:
    '2014': [0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.30, 0.32, 0.34, 0.37, 0.39, 0.40]
    '2027': [0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.30, 0.32, 0.34, 0.37, 0.39, 0.45]
"""

QBI_YAML = """\
# top_tax switch QBI: repeal Section 199A (QBI deduction) from 2027
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
"""

# Switch table: (short, bit, is_corp, {filename: [text blocks]})
SWITCHES = [
    ("ord",    0, False, {"ord.yaml":    [ORD_RATES]}),
    ("cg",     1, False, {"pref.yaml":   [CG_RATES]}),
    ("corp",   2, True,  {}),                                   # OME dep, not YAML
    ("wealth", 3, False, {"wealth.yaml": [WEALTH_YAML]}),
    ("deemed", 4, False, {"pref.yaml":   [DEEMED_REGIME]}),
    ("estate", 5, False, {"estate.yaml": [ESTATE_YAML]}),
    ("qbi",    6, False, {"qbi.yaml":    [QBI_YAML]}),
]
N_SWITCHES = len(SWITCHES)


def combo_label(bits):
    """Short human label for a combo, e.g. 'ord+cg+corp' or 'baseline'."""
    ons = [s[0] for s in SWITCHES if bits & (1 << s[1])]
    return "+".join(ons) if ons else "baseline"


def write_combo_dir(idx, bits):
    """Write the tax_law dir for combo `bits`; return relative tax_law path."""
    cdir = os.path.join(TAXLAW_ROOT, f"c{idx:03d}")
    os.makedirs(cdir, exist_ok=True)
    # Collect YAML blocks per filename across all on-switches.
    per_file = {}
    for short, bit, is_corp, files in SWITCHES:
        if not (bits & (1 << bit)) or is_corp:
            continue
        for fname, blocks in files.items():
            per_file.setdefault(fname, []).append((short, blocks))
    for fname, contribs in per_file.items():
        parts = ["---\n"]
        for short, blocks in contribs:
            for blk in blocks:
                parts.append(blk if blk.endswith("\n") else blk + "\n")
                parts.append("\n")
        with open(os.path.join(cdir, fname), "w") as fh:
            fh.write("".join(parts).rstrip("\n") + "\n")
    return f"{TAXLAW_REL}/c{idx:03d}"


def corp_on(bits):
    return bool(bits & (1 << 2))


def main():
    # Fresh tax_law tree
    if os.path.isdir(TAXLAW_ROOT):
        shutil.rmtree(TAXLAW_ROOT)
    os.makedirs(TAXLAW_ROOT, exist_ok=True)
    os.makedirs(RUNSCRIPT_DIR, exist_ok=True)

    header = [
        "ID", "tax_law", "behavior", "years", "dist_years",
        "mtr_vars", "mtr_types",
        "dep.Off-Model-Estimates.vintage", "dep.Off-Model-Estimates.ID",
        "s",
    ]
    rows = []
    # combo 0 = all-off = baseline
    rows.append({
        "ID": "baseline", "tax_law": "baseline", "behavior": "",
        "years": YEARS, "dist_years": "",
        "mtr_vars": MTR_VARS, "mtr_types": MTR_TYPES,
        "dep.Off-Model-Estimates.vintage": CORP_OFF_VINTAGE,
        "dep.Off-Model-Estimates.ID": CORP_OFF_ID,
        "s": S_COL,
    })
    for bits in range(1, 1 << N_SWITCHES):
        taxlaw = write_combo_dir(bits, bits)
        on = corp_on(bits)
        wealth_on = bool(bits & (1 << 3))
        rows.append({
            "ID": f"c{bits:03d}",              # ID = combo bitmask, zero-padded
            "tax_law": taxlaw,
            "behavior": BEHAVIOR_WEALTH if wealth_on else BEHAVIOR_BASE,
            "years": YEARS, "dist_years": DIST_YEARS,
            "mtr_vars": MTR_VARS, "mtr_types": MTR_TYPES,
            "dep.Off-Model-Estimates.vintage": CORP_ON_VINTAGE if on else CORP_OFF_VINTAGE,
            "dep.Off-Model-Estimates.ID": CORP_ON_ID if on else CORP_OFF_ID,
            "s": S_COL,
        })

    with open(RUNSCRIPT, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=header)
        w.writeheader()
        w.writerows(rows)

    # Also emit a legend mapping ID <-> switches on (for the atlas/explorer).
    legend = os.path.join(RUNSCRIPT_DIR, "factorial_legend.csv")
    with open(legend, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["ID", "bits", "switches_on"] + [s[0] for s in SWITCHES])
        for bits in range(0, 1 << N_SWITCHES):
            ID = "baseline" if bits == 0 else f"c{bits:03d}"
            flags = [1 if bits & (1 << s[1]) else 0 for s in SWITCHES]
            w.writerow([ID, bits, combo_label(bits)] + flags)

    print(f"Wrote {len(rows)} runscript rows -> {RUNSCRIPT}")
    print(f"Wrote {(1 << N_SWITCHES) - 1} tax_law dirs -> {TAXLAW_ROOT}")
    print(f"Wrote legend -> {legend}")


if __name__ == "__main__":
    main()
