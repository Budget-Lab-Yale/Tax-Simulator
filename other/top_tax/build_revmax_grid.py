#!/usr/bin/env python3
"""
build_revmax_grid.py  --  generate the CG-rate x death-regime revmax grid.

VISION.md §B.1: revenue-maximizing top capital-gains rate BY DEATH REGIME.
The realization elasticity is endogenous (kg Bellman) and policy-dependent,
so the revmax rate differs by regime -- that spread is the finding.

Design (retune GRID below and re-run; it is pure file I/O, safe on login node):
  - enactment 2027 (sim starts 2026 = the t-1 FY lead-in); one contiguous
    run 2026:2057 per scenario yields BOTH the 10y (FY27-36) and 30y revmax
    for free (revenue totals are written every sim year; dist_years is kept
    minimal because revmax is a revenue concept, not a distribution one).
  - full behavioral stack (author call 2026-07-08): the pinned order
    kg_dynamics -> conversion/sigma -> entity_shifting -> evasion is REQUIRED
    (sigma hard-stops without kg; sigma=0.08 is only valid with the full
    {sigma,entity,evasion,charity} set present). charity appended last.
  - wealth bathtub ON via the calibrated default profile (leave s /
    wealth_financing unset). corp channel is fail-closed OFF here (no
    corporate provision -> zero wedge -> clean skip), so no OME columns.

Baseline top pref (CG) rate = 0.20 (statutory; 23.8% incl. NIIT). A +pp point
sets the top bracket to 0.20 + pp/100. pp=0 under a non-step-up regime is the
"regime-only, current rate" anchor of the curve.
"""
import csv
import os
import textwrap

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
TAXLAW_DIR = os.path.join(REPO, "config/scenarios/tax_law/alternatives/tests/revmax")
RUNSCRIPT = os.path.join(REPO, "config/runscripts/tests/revmax_cg.csv")

ENACT = 2027
BASE_PREF = [0.0, 0.15, 0.20]          # baseline top-bracket schedule
REGIMES = {"stepup": None, "carryover": 1, "deemed": 2}  # 0=step-up baseline
PP_POINTS = [0, 5, 10, 15, 20, 25]     # coarse ladder; refine near the peak later

BEHAVIOR = ("kg_dynamics/turnover conversion/sigma "
            "entity_shifting/pearce_prisinzano evasion/debacker charity/50")
MTR_VARS = "wages1 wages2 part_active sole_prop1 scorp_active kg_lt rent char_cash"
MTR_TYPES = " ".join(["nextdollar"] * len(MTR_VARS.split()))
YEARS = "2026:2057"
DIST_YEARS = "2036"                    # minimal; revmax needs revenue, not dist tables

REGIME_KEYS = ["equities", "pass_throughs", "primary_home", "other_home", "re_fund"]


def pref_yaml(pp, regime_code):
    top = round(BASE_PREF[2] + pp / 100.0, 4)
    lines = ["---"]
    if pp > 0:
        lines += [
            f"# CG top rate +{pp}pp (0.20 -> {top}); enactment {ENACT}",
            "rates:",
            "  value:",
            f"    '2014': [0.0, 0.15, 0.20]",
            f"    '{ENACT}': [0.0, 0.15, {top}]",
        ]
    else:
        lines += ["# CG rate unchanged (regime-only anchor of the schedule)"]
    if regime_code is not None:
        for k in REGIME_KEYS:
            lines += [f"kg_death_regime_{k}:", f"  value: {regime_code}"]
    return "\n".join(lines) + "\n"


def main():
    os.makedirs(TAXLAW_DIR, exist_ok=True)
    os.makedirs(os.path.dirname(RUNSCRIPT), exist_ok=True)

    rows = [["baseline", "baseline", "", YEARS, "", MTR_VARS, MTR_TYPES]]
    n_dirs = 0
    for regime, code in REGIMES.items():
        for pp in PP_POINTS:
            if regime == "stepup" and pp == 0:
                continue  # step-up + no rate change == baseline
            sid = f"cg_{pp:02d}pp_{regime}"
            d = os.path.join(TAXLAW_DIR, sid)
            os.makedirs(d, exist_ok=True)
            with open(os.path.join(d, "pref.yaml"), "w") as f:
                f.write(pref_yaml(pp, code))
            n_dirs += 1
            rows.append([sid, f"tests/revmax/{sid}", BEHAVIOR,
                         YEARS, DIST_YEARS, MTR_VARS, MTR_TYPES])

    with open(RUNSCRIPT, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["ID", "tax_law", "behavior", "years", "dist_years",
                    "mtr_vars", "mtr_types"])
        w.writerows(rows)

    print(f"wrote {n_dirs} reform dirs under {TAXLAW_DIR}")
    print(f"wrote runscript {RUNSCRIPT} with {len(rows)} rows "
          f"(1 baseline + {len(rows)-1} scenarios)")


if __name__ == "__main__":
    main()
