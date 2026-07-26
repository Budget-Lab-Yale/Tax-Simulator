#!/usr/bin/env python3
"""
write_profiles.py -- generate the wealth-dynamics financing PROFILE folders.

A financing profile (config/calibrations/wealth_profiles/<name>/) is the
per-scenario input to the wealth bathtub: a bracket-varying saving share
s(age, net-worth pctile) and a within-age percentile transition matrix M. This
script writes the two shipped profiles deterministically so they can be
regenerated/diffed:

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
  provenance.yaml : the sidecar. A profile is a TABLE-valued calibration, and a
          table cannot carry its own provenance the way a scalar entry in a YAML
          file can, so the metadata that would otherwise sit beside the value
          lives next to the files instead: what the surface targets, what it was
          derived under, what invalidates it, and the md5 of each table. Written
          here, by the script that writes the tables, for the same reason the kg
          calibrators now write their own entries -- so that a shipped table and
          the derivation behind it cannot drift apart unnoticed.

NOTE, config/calibrations/wealth_profiles/s1_uniform/ is NOT written by this
script and nothing generates it. It is the s=1, uniform-M corner from the
2026-06 bounding sweep, written by hand or by a script since removed, and as of
2026-07-26 no runscript, economy alternative or script in the repository refers
to it. It is left in place because CLAUDE.md describes it as the uniform-M
counterpart to the identity default, but it has no provenance and cannot be
regenerated. Worth a decision: give it a generator, or delete it.

Usage:  python3 other/wealth_dynamics/write_profiles.py
"""

import csv
import datetime
import hashlib
import os

# Must match WEALTH_DYN_AGE_MIN/MAX and economy.wealth.n_pctiles.
AGE_MIN, AGE_MAX = 18, 80
N_PCTILES = 100

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
PROFILES_ROOT = os.path.join(REPO, 'config', 'calibrations', 'wealth_profiles')

# This script, as the repository sees it -- named in the sidecars it writes.
THIS_SCRIPT = 'other/wealth_dynamics/write_profiles.py'

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


def md5(path):
    h = hashlib.md5()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 16), b''):
            h.update(chunk)
    return h.hexdigest()


# The provenance each shipped profile owes, in the same vocabulary the scalar
# calibration files use (kind / set / target / derived_under / invalidated_by /
# rederive), so one reader and one checker cover both.
#
# `derived_under` is EMPTY for both, and that is the honest answer rather than an
# omission: neither surface is fitted to model data. The default is a formula over
# published elasticities, evaluated in this script, with no Tax-Data or
# Macro-Projections input anywhere in it -- so no upstream vintage can make it
# stale. What CAN make it stale is this script changing, which is why the script
# is what it declares itself invalidated by.
SIDECAR_META = {
    'default': dict(
        kind='calibrated',
        set='2026-07-07',
        target=(
            'Saving share s = 1 - MPC out of a PERSISTENT net income change, by '
            'age and within-age net-worth percentile. Anchored on the '
            'cross-sectional elasticity of consumption to permanent income '
            '(Straub 2019, eps ~ 0.7) applied to consumption-to-income ratios by '
            'wealth rank (Mian-Straub-Sufi 2021), giving s ~ 0.10 at the bottom '
            'rising to ~0.80 at the top percentile; age tilt from '
            'Fagereng-Holm-Natvik 2021, attenuated to zero at top ranks per '
            'De Nardi-French-Jones 2010. Gradient shape cross-checked against '
            'Dynan-Skinner-Zeldes 2004. Full memo: '
            'other/wealth_dynamics/default_s_calibration.md.'),
        note=(
            'This profile is applied model-wide to any scenario whose economy leg '
            'does not set financing_profile to none, so it is the single largest '
            'lever on the wealth-bathtub channel. derived_under is deliberately '
            'empty: the surface is a literature bridge computed in the generating '
            'script, not a fit to model data, so no upstream data vintage can '
            'invalidate it.'),
    ),
    'example_age_wealth': dict(
        kind='judgment',
        set='2026-06-24',
        target=None,
        note=(
            'ILLUSTRATIVE, not calibrated. A placeholder age x wealth-rank shape '
            'that exists to exercise the bracket machinery and to show the file '
            'format. No scenario should ship results from it. Kept as judgment '
            'rather than calibrated precisely because there is no derivation to '
            'record.'),
    ),
}


def write_sidecar(name, tables):
    """provenance.yaml for one profile folder.

    Written as text rather than through a YAML library on purpose. These files are
    read by people as much as by the model, the explanatory prose is the point,
    and a YAML round-trip reflows it. Same reasoning as
    src/misc/calibration_writer.R, which does the scalar half of this job.
    """
    meta = SIDECAR_META[name]
    path = os.path.join(PROFILES_ROOT, name, 'provenance.yaml')

    def wrap(text, indent='    ', width=76):
        words, lines, cur = text.split(), [], indent
        for w in words:
            if len(cur) > len(indent) and len(cur) + 1 + len(w) > width:
                lines.append(cur)
                cur = indent + w
            else:
                cur = f'{cur} {w}' if len(cur) > len(indent) else cur + w
        lines.append(cur)
        return '\n'.join(lines)

    out = [
        f'# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.',
        '#',
        '# Provenance for a TABLE-valued calibration: the saving-share surface and',
        '# transition matrix in this folder. The tables cannot carry their own',
        '# metadata, so it lives here, in the same vocabulary the scalar',
        '# calibration files use.',
        '#',
        '# The md5 of each table is recorded below. A table edited by hand after',
        '# generation no longer matches its hash, which is the one thing a',
        '# generated data file cannot tell you about itself.',
        '',
        '_channel:',
        '  role: state',
        '',
        'financing_profile:',
        f'  value: {name}',
        f'  kind: {meta["kind"]}',
        f'  set: \'{meta["set"]}\'',
    ]

    if meta.get('target'):
        out += ['  target: >', wrap(meta['target'])]

    if meta['kind'] == 'calibrated':
        out += ['  derived_under: {}',
                '  invalidated_by:',
                f'    - {THIS_SCRIPT}',
                '  invalidated_by_hashes:',
                f'    {THIS_SCRIPT}: {md5(os.path.join(REPO, THIS_SCRIPT))}',
                f'  rederive: {THIS_SCRIPT}']

    out += ['  tables:']
    for t in tables:
        out.append(f'    {t}: {md5(os.path.join(PROFILES_ROOT, name, t))}')

    out += ['  note: >', wrap(meta['note']), '']

    with open(path, 'w', newline='\n') as f:
        f.write('\n'.join(out))
    return path


def write_profile(name, s_fn):
    d = os.path.join(PROFILES_ROOT, name)
    os.makedirs(d, exist_ok=True)
    write_s_csv(os.path.join(d, 's.csv'), s_fn)
    write_M_identity(os.path.join(d, 'M.csv'))
    write_sidecar(name, ['s.csv', 'M.csv'])
    print(f'wrote {name}/s.csv ({len(AGES) * len(PCTILES)} cells) + M.csv '
          f'({N_PCTILES}x{N_PCTILES} identity) + provenance.yaml')


if __name__ == '__main__':
    write_profile('default', s_default)
    write_profile('example_age_wealth', s_example)
    print('profiles root:', PROFILES_ROOT)
    print('\nRegenerate-and-diff is the check; an empty `git status --short '
          'config/` is the pass.\n'
          'Note the hash of this script appears in the sidecars it writes, so '
          'editing it\nrequires one regeneration to settle.')
