#!/usr/bin/env python3
"""
write_profiles.py -- generate the wealth-dynamics financing PROFILE folders.

A financing profile (config/wealth/profiles/<name>/) is the per-scenario input
to the wealth bathtub: a bracket-varying saving share s(age, net-worth pctile)
and a within-age percentile transition matrix M. This script writes the two
shipped profiles deterministically so they can be regenerated/diffed:

  default/           CALIBRATED persistent-flow s surface + identity M
                     (auto-applied; calibration 2026-07-07 -- see s_default and
                     other/wealth_dynamics/default_s_calibration.md. This turns
                     the channel ON model-wide for any scenario that does not
                     set wealth_financing = none.)
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


# --- default: calibrated persistent-flow surface (2026-07-07) ----------------
# CONCEPT: s = 1 - MPC out of a PERSISTENT net income change (a permanent tax
# reform hits every year; by year 2+ the household has fully internalized it),
# NOT the one-off transitory-windfall MPC. Author decision 2026-07-07; memo with
# full sourcing: other/wealth_dynamics/default_s_calibration.md.
#
# BRIDGE FORMULA for the wealth-rank gradient: for a persistent flow change dY,
# dC = eps * (C/Y) * dY with eps ~= 0.7, the cross-sectional elasticity of
# consumption to permanent income (Straub 2019, "Consumption, Savings, and the
# Distribution of Permanent Income"). So s(p) = 1 - 0.7*(C/Y)(p), with C/Y by
# rank read off consumption-vs-income shares:
#   - top 1%: income share ~20% vs consumption share ~6-7% (Mian-Straub-Sufi
#     2021, "The Saving Glut of the Rich") => C/Y ~ 0.30 => s ~ 0.80
#   - P90-99: C/Y ~ 0.65-0.75 => s ~ 0.50-0.65
#   - middle: C/Y ~ 0.9-1.0 => bridge s ~ 0.3, hand-to-mouth mix (Kaplan-
#     Violante) pulls the realized value down to ~0.2
#   - bottom: liquidity-constrained mix dominates => s ~ 0.10
# Gradient SHAPE cross-checked against the transitory-MPC-by-liquidity gradient
# (Fagereng-Holm-Natvik 2021 AEJ:Macro, deposit-quartile MPCs .44/.42/.34/.22)
# and the DSZ saving-rate-by-lifetime-income gradient (Dynan-Skinner-Zeldes
# 2004 JPE: ~0 bottom quintile -> ~25% top quintile -> ~50% top 1%).
S_BASE_NODES = [   # (nw_pctile, s) -- piecewise-linear between nodes
    (1,   0.10),
    (30,  0.14),
    (50,  0.20),
    (70,  0.28),
    (85,  0.40),
    (90,  0.46),
    (95,  0.55),
    (99,  0.65),
    (100, 0.80),
]

# AGE TILT (additive), attenuated toward zero at the top of the wealth
# distribution: the young are more liquidity-constrained conditional on rank
# (lower s); peak earners 45-64 save the most out of a marginal persistent flow;
# ordinary retirees are in the decumulation phase (lower s; consistent with the
# rising-in-age transitory MPC in Fagereng-Holm-Natvik). The attenuation
# implements De Nardi-French-Jones (2010 JPE): high-permanent-income elderly do
# NOT run down wealth with age (bequest motives + medical-expense risk), so the
# retiree tilt must vanish at top ranks.
S_AGE_TILT_NODES = [   # (age, tilt) -- piecewise-linear between nodes
    (18, -0.06),
    (30, -0.05),
    (40,  0.00),
    (50,  0.03),
    (62,  0.03),
    (70, -0.03),
    (80, -0.05),
]

# Age-tilt attenuation in wealth rank: full tilt through P90, linearly to zero
# at the top percentile (age is second-order for the very wealthy, and the
# forcing dollars concentrate there).
TILT_ATTEN_NODES = [(1, 1.0), (90, 1.0), (100, 0.0)]


def _interp(nodes, x):
    if x <= nodes[0][0]:
        return nodes[0][1]
    for (x0, y0), (x1, y1) in zip(nodes, nodes[1:]):
        if x <= x1:
            return y0 + (y1 - y0) * (x - x0) / (x1 - x0)
    return nodes[-1][1]


def s_default(age, p):
    s = _interp(S_BASE_NODES, p) \
        + _interp(S_AGE_TILT_NODES, age) * _interp(TILT_ATTEN_NODES, p)
    return max(0.0, min(1.0, s))


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
