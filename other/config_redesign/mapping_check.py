#!/usr/bin/env python3
"""Manifest mapping check: verifies that every row of a golden vintage's
assumptions.csv (the old flat manifest) maps to exactly one row of a candidate
vintage's scenario_config.csv (the three-leg manifest) with an equal value,
and that every candidate row is either mapped or on the new-surface allowlist.

Usage: python3 mapping_check.py <golden_vintage_dir> <candidate_vintage_dir>
Exit 0 = pass."""

import csv, sys


def leg_map(ch, nm):
    """Old (channel, name) -> new (leg, channel, name)."""
    if (ch, nm) == ('corp', 'rate_eti'):
        return ('behavior', 'corp_avoidance', nm)
    if ch in ('corp', 'distribution'):
        return ('economy', ch, nm)
    if ch == 'kg' and nm.startswith('char_'):
        return ('economy', 'bequest', nm)
    if ch in ('kg', 'sigma', 'evasion'):
        return ('behavior', ch, nm)
    if ch == 'estate':
        return ('behavior', 'estate_avoidance', nm)
    if (ch, nm) == ('wealth', 'cap_flows_pt_weight'):
        return ('economy', 'wealth', nm)
    if ch == 'wealth':
        return ('behavior', 'wealth_avoidance', nm)
    raise ValueError(f'no mapping for {ch}.{nm}')


# Candidate rows with no old-manifest counterpart: genuinely new surface.
NEW_SURFACE = {
    ('economy', 'interfaces'),   # folded dep.* defaults
    ('economy', 'growth'),       # folded excess_growth* columns
    ('economy', 'estate'),       # valuation_bridge pointer (was outside the manifest)
    ('behavior', 'kg_static'), ('behavior', 'charity'),
    ('behavior', 'entity_shifting'), ('behavior', 'employment'),
    ('behavior', 'child_earnings'), ('behavior', 'ot'),
    ('behavior', 'tips'), ('behavior', 'auto'),
}
NEW_SURFACE_ENTRIES = {('economy', 'wealth', 'financing_profile')}


def norm(v):
    """Value comparison across the two writers: numeric-normalize when possible."""
    try:
        return f'{float(v):.12g}'
    except (TypeError, ValueError):
        return str(v)


def main(golden, cand):
    old = list(csv.DictReader(open(f'{golden}/assumptions.csv', newline='')))
    new = list(csv.DictReader(open(f'{cand}/scenario_config.csv', newline='')))

    new_idx = {(r['ID'], r['leg'], r['channel'], r['name']): r for r in new}
    problems, mapped_keys = [], set()

    for r in old:
        try:
            leg, ch, nm = leg_map(r['channel'], r['name'])
        except ValueError as e:
            problems.append(str(e)); continue
        key = (r['ID'], leg, ch, nm)
        c = new_idx.get(key)
        if c is None:
            problems.append(f'{r["ID"]}: {r["channel"]}.{r["name"]} has no new row at {leg}.{ch}.{nm}')
            continue
        mapped_keys.add(key)
        if norm(r['value']) != norm(c['value']):
            problems.append(f'{r["ID"]}: {ch}.{nm} value {r["value"]!r} vs new {c["value"]!r}')
        if r['kind'] != c['kind']:
            problems.append(f'{r["ID"]}: {ch}.{nm} kind {r["kind"]} vs new {c["kind"]}')
        if str(r['overridden']).upper() != str(c['overridden']).upper():
            problems.append(f'{r["ID"]}: {ch}.{nm} overridden flag {r["overridden"]} vs {c["overridden"]}')

    unmapped = [k for k in new_idx if k not in mapped_keys
                and (k[1], k[2]) not in NEW_SURFACE
                and (k[1], k[2], k[3]) not in NEW_SURFACE_ENTRIES]
    for k in unmapped:
        problems.append(f'{k[0]}: candidate row {k[1]}.{k[2]}.{k[3]} is neither mapped nor allowlisted')

    print(f'old rows: {len(old)}; new rows: {len(new)}; mapped: {len(mapped_keys)}')
    if problems:
        print('MAPPING FAILURES:')
        for p in problems:
            print(' -', p)
        sys.exit(1)
    print('MAPPING_CHECK_PASS')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
