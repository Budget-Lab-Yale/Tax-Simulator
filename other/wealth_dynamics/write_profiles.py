#!/usr/bin/env python3
"""
write_profiles.py -- generate the wealth-dynamics financing PROFILE folders.

A financing profile (config/wealth/profiles/<name>/) is the per-scenario input
to the wealth bathtub: a bracket-varying saving share s(age, net-worth pctile)
and a within-age percentile transition matrix M. This script writes the two
shipped profiles deterministically so they can be regenerated/diffed:

  default/           flat s = 0  + identity M  (auto-applied; a NO-OP until
                     calibrated to realistic bracket values, at which point the
                     channel turns on model-wide from this one folder)
  example_age_wealth/  ILLUSTRATIVE bracket-varying s (NOT calibrated) + identity M
                     -- shows the file format and the age x wealth-rank shape

File contracts (see wealth_dyn_resolve_profile in src/sim/wealth_dynamics.R):
  s.csv : header age,nw_pctile,s ; one row per (age, pctile) cell; every cell of
          the AGES x N_PCTILES grid present exactly once; s in [0, 1] (= 1 - MPC)
  M.csv : headerless N_PCTILES x N_PCTILES grid (raked to doubly-stochastic on load)

Usage:  python3 other/wealth_dynamics/write_profiles.py
"""

import csv
import os

# Must match WEALTH_DYN_AGE_MIN/MAX and n_pctiles (wealth_financing_params.yaml).
AGE_MIN, AGE_MAX = 18, 80
N_PCTILES = 100

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
PROFILES_ROOT = os.path.join(REPO, 'config', 'wealth', 'profiles')

AGES = list(range(AGE_MIN, AGE_MAX + 1))
PCTILES = list(range(1, N_PCTILES + 1))


def write_s_csv(path, s_fn):
    with open(path, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['age', 'nw_pctile', 's'])
        for age in AGES:
            for p in PCTILES:
                s = s_fn(age, p)
                assert 0.0 <= s <= 1.0, f's out of [0,1] at age={age} p={p}: {s}'
                w.writerow([age, p, f'{s:.4f}'])


def write_M_identity(path):
    with open(path, 'w', newline='') as f:
        w = csv.writer(f)
        for i in range(N_PCTILES):
            w.writerow([1 if j == i else 0 for j in range(N_PCTILES)])


# --- default: flat zero (channel dormant until calibrated) -------------------
def s_default(age, p):
    return 0.0


# --- example: ILLUSTRATIVE age x wealth-rank surface (NOT calibrated) --------
# Saving share s = 1 - MPC rises with net-worth rank (the wealthy consume a
# smaller share of a wealth shock) and is hump-shaped in age (peak-earning
# middle age saves most; the young and the retired decumulate more). These
# numbers are a PLACEHOLDER shape to exercise the bracket machinery, NOT an
# empirical calibration -- replace via a proper MPC-by-age-by-wealth review.
def s_example(age, p):
    if   p <= 50:  base = 0.55     # bottom half of within-age net worth
    elif p <= 90:  base = 0.70     # upper-middle
    elif p <= 99:  base = 0.85     # top decile (ex top 1%)
    else:          base = 0.92     # top 1%

    if   age <= 34: tilt = -0.05   # young: more liquidity-constrained
    elif age <= 44: tilt =  0.00
    elif age <= 64: tilt =  0.03   # peak earners: save most
    else:           tilt = -0.03   # retirees: decumulate

    return max(0.0, min(1.0, base + tilt))


def write_profile(name, s_fn):
    d = os.path.join(PROFILES_ROOT, name)
    os.makedirs(d, exist_ok=True)
    write_s_csv(os.path.join(d, 's.csv'), s_fn)
    write_M_identity(os.path.join(d, 'M.csv'))
    print(f'wrote {name}/s.csv ({len(AGES) * len(PCTILES)} cells) + M.csv '
          f'({N_PCTILES}x{N_PCTILES} identity)')


if __name__ == '__main__':
    write_profile('default', s_default)
    write_profile('example_age_wealth', s_example)
    print('profiles root:', PROFILES_ROOT)
