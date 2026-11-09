#!/usr/bin/env python3
"""
Inject an atlas data JSON into an atlas template HTML.

Usage:  python3 other/top_tax/build_atlas.py [data_json] [out_html] [template]
Defaults: other/top_tax/atlas_data.json -> other/top_tax/atlas_built.html
          template = other/top_tax/atlas.html (v1)
For atlas v2: python3 other/top_tax/build_atlas.py other/top_tax/atlas2_data.json \
                other/top_tax/atlas2_built.html other/top_tax/atlas2.html
Pure file I/O -- safe on the login node.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
MARKER = "/*__ATLAS_DATA__*/null"


def main():
    data_path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "atlas_data.json")
    out_path = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "atlas_built.html")
    template = sys.argv[3] if len(sys.argv) > 3 else os.path.join(HERE, "atlas.html")
    with open(template) as fh:
        html = fh.read()
    if MARKER not in html:
        sys.exit(f"marker {MARKER!r} not found in {template}")
    with open(data_path) as fh:
        data = json.load(fh)  # validate
    html = html.replace(MARKER, json.dumps(data, separators=(",", ":")), 1)
    with open(out_path, "w") as fh:
        fh.write(html)
    n = data["meta"].get("n_combos")
    extra = f", {n}/127 combos" if n is not None else ""
    print(f"Wrote {out_path} ({os.path.getsize(out_path)/1e6:.2f} MB{extra})")


if __name__ == "__main__":
    main()
