#!/usr/bin/env python3
"""
write_extreme_profile.py -- generate a FLAT-s + uniform-M financing profile for
extreme-case wealth-dynamics analysis (not a calibrated profile).

Writes config/wealth/profiles/<name>/{s.csv, M.csv} where:
  s.csv : header age,nw_pctile,s ; every cell of AGES x N_PCTILES = constant `s`
  M.csv : headerless N_PCTILES x N_PCTILES uniform 1/n  ("perfect mobility":
          within-age percentile rank fully re-drawn each year)

Usage:  python3 other/wealth_dynamics/write_extreme_profile.py <name> <s>
Example: python3 other/wealth_dynamics/write_extreme_profile.py s1_uniform 1.0
"""
import csv, os, sys

AGE_MIN, AGE_MAX = 18, 80
N_PCTILES = 100
REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))

name = sys.argv[1] if len(sys.argv) > 1 else 's1_uniform'
s_val = float(sys.argv[2]) if len(sys.argv) > 2 else 1.0
assert 0.0 <= s_val <= 1.0, f's must be in [0,1]: {s_val}'

d = os.path.join(REPO, 'config', 'wealth', 'profiles', name)
os.makedirs(d, exist_ok=True)

# s.csv -- flat s on every (age, pctile) cell
with open(os.path.join(d, 's.csv'), 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['age', 'nw_pctile', 's'])
    for age in range(AGE_MIN, AGE_MAX + 1):
        for p in range(1, N_PCTILES + 1):
            w.writerow([age, p, f'{s_val:.4f}'])

# M.csv -- uniform 1/n (already doubly-stochastic; raked-idempotent on load)
u = 1.0 / N_PCTILES
with open(os.path.join(d, 'M.csv'), 'w', newline='') as f:
    w = csv.writer(f)
    for _ in range(N_PCTILES):
        w.writerow([f'{u:.6f}'] * N_PCTILES)

ncells = (AGE_MAX - AGE_MIN + 1) * N_PCTILES
print(f'wrote {name}/s.csv ({ncells} cells, s={s_val}) + M.csv ({N_PCTILES}x{N_PCTILES} uniform 1/n)')
