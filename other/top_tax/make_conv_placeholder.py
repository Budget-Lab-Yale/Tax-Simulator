#!/usr/bin/env python3
"""
Build atlas_data_placeholder.json: the REAL static side of an extraction
(static totals, heads, byyear, and all ETRs are untouched — the static leg has
no behavior modules, so it is clean even in a run whose conventional leg is
contaminated), with the CONVENTIONAL side replaced by a synthetic placeholder
until the fixed re-run lands.

Synthetic conventional model (placeholder only, clearly badged):
  conv_i (standalone)  = static_i * SURV[i]                       per lever
  conv(S) (package)    = static(S) * wsurv(S) * (1 - DRAG*(k-1))
      wsurv(S) = static-weighted mean of SURV over selected levers
      the extra DRAG term is behavioral interaction beyond the base overlap
      that static(S) already carries; k = number of levers in S
  conv byyear          = static byyear, scaled by the conv/static ratio
  conv heads10         = static heads10 scaled per-head, small payroll
                         leakage added, then rescaled to sum to conv total

Usage:  python3 other/top_tax/make_conv_placeholder.py [in_json] [out_json]
Pure file I/O -- safe on the login node.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

# lever -> plausible 10-yr conventional survival rate (share of static that
# survives behavior); placeholder judgment values, not model output
SURV = {'ord': 0.78, 'cg': 0.55, 'corp': 0.92, 'wealth': 0.72,
        'deemed': 0.82, 'estate': 0.78, 'qbi': 0.88}
DRAG = 0.03          # extra behavioral interaction per additional lever
HEAD_F = {'iit': 0.80, 'cg': 0.55, 'pay': 1.0, 'corp': 1.0,
          'est': 0.80, 'wealth': 0.75, 'other': 1.0}
PAY_LEAK = -0.010    # payroll leg of entity shifting, as share of static iit


def main():
    in_path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, 'atlas_data.json')
    out_path = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, 'atlas_data_placeholder.json')
    d = json.load(open(in_path))
    switches = d['meta']['switches']
    bit = {k: 1 << i for i, k in enumerate(switches)}

    # standalone static totals, needed for weighted survival
    s1 = {k: d['combos'][str(bit[k])]['static']['total10'] for k in switches
          if str(bit[k]) in d['combos']}

    for key, c in d['combos'].items():
        bits = int(key)
        levers = [k for k in switches if bits & bit[k]]
        k = len(levers)
        st = c['static']['total10']
        tot_s1 = sum(s1[l] for l in levers)
        wsurv = (sum(s1[l] * SURV[l] for l in levers) / tot_s1) if tot_s1 else 0.75
        if k == 1:
            conv = st * SURV[levers[0]]
        else:
            conv = st * wsurv * (1 - DRAG * (k - 1))
        r = conv / st if st else 0.0
        heads_s = c['static']['heads10']
        raw = {h: heads_s.get(h, 0.0) * HEAD_F.get(h, 1.0) for h in heads_s}
        raw['pay'] = raw.get('pay', 0.0) + PAY_LEAK * heads_s.get('iit', 0.0)
        tot_raw = sum(raw.values())
        scale = conv / tot_raw if tot_raw else 0.0
        c['conv'] = {
            'total10': round(conv, 3),
            'byyear': [round(v * r, 3) for v in c['static']['byyear']],
            'heads10': {h: round(v * scale, 3) for h, v in raw.items()},
        }

    d['meta']['placeholder'] = 'conventional side synthetic pending fixed re-run; static side real'
    json.dump(d, open(out_path, 'w'), separators=(',', ':'))
    print(f"Wrote {out_path} ({d['meta']['n_combos']}/127 combos, conv synthetic)")


if __name__ == '__main__':
    main()
