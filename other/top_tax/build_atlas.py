#!/usr/bin/env python3
"""
Inject atlas_data.json into atlas.html (the real-data explorer template).

Usage:  python3 other/top_tax/build_atlas.py [data_json] [out_html]
Defaults: other/top_tax/atlas_data.json -> other/top_tax/atlas_built.html
Pure file I/O -- safe on the login node.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
TEMPLATE = os.path.join(HERE, "atlas.html")
MARKER = "/*__ATLAS_DATA__*/null"


def main():
    data_path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "atlas_data.json")
    out_path = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "atlas_built.html")
    with open(TEMPLATE) as fh:
        html = fh.read()
    if MARKER not in html:
        sys.exit(f"marker {MARKER!r} not found in {TEMPLATE}")
    with open(data_path) as fh:
        data = json.load(fh)  # validate
    html = html.replace(MARKER, json.dumps(data, separators=(",", ":")), 1)
    with open(out_path, "w") as fh:
        fh.write(html)
    print(f"Wrote {out_path} ({os.path.getsize(out_path)/1e6:.2f} MB, "
          f"{data['meta']['n_combos']}/127 combos)")


if __name__ == "__main__":
    main()
