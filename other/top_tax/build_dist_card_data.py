#!/usr/bin/env python3
"""
Extracts dist_card_data.json from a dials vintage and builds dist_card_built.html.

Reads the stack_ref scenario's static/supplemental/distribution_etrs.csv
(wealth_cit_vat taxes, fixed income-percentile ranking, capital_income corp
convention) and emits the payload dist_card.html consumes: per-group baseline
tax, the static ask, the conventional collection, and the avoidance gap,
under both the cash (expanded) and accrual (Haig-Simons) income definitions.
Dollar views use disjoint bins differenced out of the cumulative top groups;
the rate view keeps the nested groups as published.

Usage:
  python3 other/top_tax/build_dist_card_data.py <vintage_root> [scenario]
"""

import csv
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

YEARS = ["2027", "2036"]
TAXES = "wealth_cit_vat"
RANKING = "fixed"
CORP_CONV = "capital_income"

NESTED = ["Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5",
          "Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"]
NESTED_OUT = ["Q1", "Q2", "Q3", "Q4", "Q5",
              "Top 10%", "Top 5%", "Top 1%", "Top 0.1%", "Top 0.01%"]

# Disjoint dollar bins: quintiles 1-4 as-is, then successive differences of
# the cumulative top groups.
DISJOINT = [
    ("Bottom 20%",  "Quintile 1", None),
    ("20–40%", "Quintile 2", None),
    ("40–60%", "Quintile 3", None),
    ("60–80%", "Quintile 4", None),
    ("80–90%", "Quintile 5", "Top 10%"),
    ("90–95%", "Top 10%",    "Top 5%"),
    ("95–99%", "Top 5%",     "Top 1%"),
    ("99–99.9%",    "Top 1%",   "Top 0.1%"),
    ("99.9–99.99%", "Top 0.1%", "Top 0.01%"),
    ("Top 0.01%",   "Top 0.01%",  None),
]


def load_slice(path):
    """Reads distribution_etrs.csv and indexes the card's slice by
    (year, leg, income_def, group).

    Returns: dict keyed as above, values = raw CSV rows.
    """
    out = {}
    with open(path, newline="") as fh:
        for r in csv.DictReader(fh):
            if (r["taxes_included"] == TAXES and r["ranking"] == RANKING
                    and r["corp_convention"] == CORP_CONV
                    and r["group_dimension"] == "Income percentile"
                    and r["year"] in YEARS and r["group"] in NESTED):
                out[(r["year"], r["reform_leg"], r["income_definition"], r["group"])] = r
    return out


def group_rec(sl, year, group):
    """Assembles one nested group's record: counts, income under both
    definitions, baseline tax, and the two reform-leg tax deltas.

    Returns: dict of floats (n, cash, accrual, tax_base, static, conv).
    """
    st = sl[(year, "static", "expanded", group)]
    cv = sl[(year, "conventional", "expanded", group)]
    hs = sl[(year, "static", "hs", group)]
    return dict(
        n=float(st["n_tax_units"]),
        cash=float(st["income_baseline"]),
        accrual=float(hs["income_baseline"]),
        tax_base=float(st["tax_baseline"]),
        static=float(st["tax_reform"]) - float(st["tax_baseline"]),
        conv=float(cv["tax_reform"]) - float(cv["tax_baseline"]),
    )


def pack(rec, label):
    """Rounds one record into the card's bin shape.

    Returns: dict matching dist_card_data.json's per-bin schema.
    """
    return dict(
        bin=label, n=round(rec["n"], 1),
        income=dict(cash=round(rec["cash"], 3), accrual=round(rec["accrual"], 3)),
        tax_base=round(rec["tax_base"], 3),
        static=round(rec["static"], 4),
        conv=round(rec["conv"], 4),
        avoid=round(rec["static"] - rec["conv"], 4),
    )


def diff(a, b):
    """Subtracts record b from record a, key by key.

    Returns: dict of floats.
    """
    return {k: a[k] - b[k] for k in a}


def main():
    if len(sys.argv) < 2:
        sys.exit("usage: build_dist_card_data.py <vintage_root> [scenario]")
    root = sys.argv[1].rstrip("/")
    scen = sys.argv[2] if len(sys.argv) > 2 else "stack_ref"
    csv_path = os.path.join(root, scen, "static", "supplemental",
                            "distribution_etrs.csv")
    sl = load_slice(csv_path)

    years = {}
    for year in YEARS:
        recs = {g: group_rec(sl, year, g) for g in NESTED}
        bins = []
        for label, hi, minus in DISJOINT:
            r = recs[hi] if minus is None else diff(recs[hi], recs[minus])
            assert minus is None or r["n"] > 0, f"{year} {label}: empty bin"
            bins.append(pack(r, label))
        nested = [pack(recs[g], lab) for g, lab in zip(NESTED, NESTED_OUT)]

        # The disjoint bins must re-add to the full population.
        assert abs(sum(b["n"] for b in bins) -
                   sum(recs[q]["n"] for q in NESTED[:5])) < 1, year
        years[year] = dict(bins=bins, nested=nested)

    payload = dict(
        meta=dict(
            scenario=scen, vintage=os.path.basename(root),
            ranking="Income percentile (fixed)",
            income_defs=dict(cash="expanded (cash) income",
                             accrual="Haig-Simons (accrual, incl. unrealized gains)"),
            taxes=f"{TAXES} (federal: iit+payroll+estate+deemed+wealth+corp+vat)",
            units="$B",
            note=("dollar views use disjoint bins; rate view uses nested "
                  "cumulative groups (atlas set); tax $ invariant to income basis"),
        ),
        years=years,
    )

    out_json = os.path.join(HERE, "dist_card_data.json")
    with open(out_json, "w") as fh:
        json.dump(payload, fh, indent=1)
    print(f"wrote {out_json}")

    # Substitute the payload into the page template.
    tmpl_path = os.path.join(HERE, "dist_card.html")
    tmpl = open(tmpl_path).read()
    marker = "__DATA__"
    if marker not in tmpl:
        sys.exit("dist_card.html has no __DATA__ marker")
    built = tmpl.replace(marker, json.dumps(payload), 1)
    built = built.replace("vintage top_tax_dials_30y_v1",
                          f"vintage {os.path.basename(root)}")
    out_html = os.path.join(HERE, "dist_card_built.html")
    with open(out_html, "w") as fh:
        fh.write(built)
    print(f"wrote {out_html}")


if __name__ == "__main__":
    main()
