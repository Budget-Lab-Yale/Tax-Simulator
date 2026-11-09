#!/usr/bin/env python3
"""Fit the eta-dial exponential transform from the KG_ETA batch.

Per (scenario, decade), the behavioral revenue offset B(eta) = conv - static
is fit with the 2-parameter constant-semi-elasticity form implied by the
spec-v3 entropy Bellman:

    B(eta) = K * (exp(-eta * w) - 1)

Given w, K solves in closed form (least squares); w is found by 1-D grid +
golden refine. Validation = leave-out-central: fit on {1.2, 3.6, 4.8}, predict
conv at eta=2.3992, compare against the shipped central run.

Reads revenue_estimates.csv (conv + static legs) for the 6 main scenarios and
stack_ref across 4 eta vintages. Writes eta_fit_results.csv + prints a summary.
"""
import csv
import math
import os

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
CENTRAL_ETA = 2.3992
ETAS = [1.2, CENTRAL_ETA, 3.6, 4.8]
DECADES = {'d1': (2027, 2036), 'd2': (2037, 2046), 'd3': (2047, 2056)}
MAIN_SCEN = ['s_cg_r25', 's_cg_r40', 's_cg_r50',
             's_deemed_carryover', 's_deemed_deemed', 'pr_cg_deemed']

def vintage(eta, scen):
    if eta == CENTRAL_ETA:
        return 'top_tax_dials_30y_v1'
    tag = {1.2: 'e12', 3.6: 'e36', 4.8: 'e48'}[eta]
    return f'eta_dial_{tag}_ref' if scen == 'stack_ref' else f'eta_dial_{tag}'

def decade_sums(vint, scen, leg):
    path = os.path.join(ROOT, vint, scen, leg, 'supplemental',
                        'revenue_estimates.csv')
    by_year = {}
    with open(path) as f:
        for row in csv.DictReader(f):
            if row['total'] == 'NA':  # lead-out year (2057), NA by design
                continue
            by_year[int(row['year'])] = float(row['total'])
    return {d: sum(by_year[y] for y in range(lo, hi + 1))
            for d, (lo, hi) in DECADES.items()}

def fit_kw(etas, bs):
    """Least-squares K, w for B = K*(exp(-eta*w)-1)."""
    def sse_at(w):
        x = [math.exp(-e * w) - 1 for e in etas]
        sxx = sum(v * v for v in x)
        k = sum(v * b for v, b in zip(x, bs)) / sxx if sxx else 0.0
        return sum((k * v - b) ** 2 for v, b in zip(x, bs)), k
    # grid then golden refine on w in (0, 2]
    grid = [i / 400 * 2 for i in range(1, 401)]
    w = min(grid, key=lambda g: sse_at(g)[0])
    lo, hi = max(w - 0.01, 1e-6), w + 0.01
    for _ in range(60):
        m1, m2 = lo + (hi - lo) * 0.382, lo + (hi - lo) * 0.618
        if sse_at(m1)[0] < sse_at(m2)[0]:
            hi = m2
        else:
            lo = m1
    w = (lo + hi) / 2
    return sse_at(w)[1], w

def main():
    rows, summary = [], []
    for scen in MAIN_SCEN + ['stack_ref']:
        for d in DECADES:
            conv, static = {}, {}
            for eta in ETAS:
                v = vintage(eta, scen)
                conv[eta] = decade_sums(v, scen, 'conventional')[d]
                static[eta] = decade_sums(v, scen, 'static')[d]
            s_vals = list(static.values())
            assert max(s_vals) - min(s_vals) < 1e-6 * max(1, abs(s_vals[0])), \
                f'static not eta-invariant: {scen} {d} {s_vals}'
            S = s_vals[0]
            B = {e: conv[e] - S for e in ETAS}

            # full fit (all 4 points)
            K, w = fit_kw(ETAS, [B[e] for e in ETAS])
            resid = max(abs(K * (math.exp(-e * w) - 1) - B[e]) for e in ETAS)

            # leave-out-central validation
            loo = [e for e in ETAS if e != CENTRAL_ETA]
            K3, w3 = fit_kw(loo, [B[e] for e in loo])
            pred_c = S + K3 * (math.exp(-CENTRAL_ETA * w3) - 1)
            err_c = pred_c - conv[CENTRAL_ETA]
            err_pct = 100 * err_c / abs(S) if S else float('nan')

            rows.append(dict(
                scenario=scen, decade=d, static=S,
                conv_e12=conv[1.2], conv_central=conv[CENTRAL_ETA],
                conv_e36=conv[3.6], conv_e48=conv[4.8],
                K=K, w=w, max_resid_B=resid,
                loo_pred_central=pred_c, loo_err_B=err_c, loo_err_pct_static=err_pct))
            summary.append((scen, d, err_c, err_pct))

    out = 'other/top_tax/eta_dial/eta_fit_results.csv'
    with open(out, 'w', newline='') as f:
        wtr = csv.DictWriter(f, fieldnames=list(rows[0]))
        wtr.writeheader()
        wtr.writerows(rows)

    print(f'{"scenario":<20} {"dec":<3} {"LOO err $B":>11} {"% of static":>12}')
    for scen, d, err, pct in summary:
        print(f'{scen:<20} {d:<3} {err:>11.1f} {pct:>11.2f}%')
    worst = max(summary, key=lambda r: abs(r[3]))
    print(f'\nworst LOO error: {worst[0]} {worst[1]}: '
          f'{worst[2]:+.1f}$B ({worst[3]:+.2f}% of static)')
    print(f'wrote {out}')

if __name__ == '__main__':
    main()
