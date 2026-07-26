#!/usr/bin/env python3
"""Rewrite runscript CSVs from the pre-redesign schema to the three-leg schema.

The new schema is exactly eight columns:

  ID, tax_law, economy, behavior, years, dist_years, mtr_vars, mtr_types

A runscript names FILES, never values. Every per-value column the old schema
carried therefore becomes an entry in a named economy SET folder under
config/scenarios/economy/sets/<name>/, and the row's `economy` cell names that
folder:

  dep.{X}.vintage / dep.{X}.ID  -> sets/<name>/interfaces.yaml
  assumption.{ch}.{nm}          -> sets/<name>/{ch}.yaml (economy channels) or
                                   a behavior set (behavior channels, Phase 4)
  s / wealth_financing          -> sets/<name>/wealth.yaml: financing_profile
  excess_growth*                -> sets/<name>/growth.yaml
  behavior (module paths)       -> behavior (set name), via
                                   other/migrations/behavior_combo_map.csv
  corp_incidence_phasein        -> dropped (dead)
  assumptions                   -> not auto-translatable (error; by hand)

Set names are derived from set CONTENT, so identical pins across runscripts
collapse onto one shared folder (the 237 rows pinning Off-Model-Estimates
20250925 all point at economy/sets/ome_20250925). Emission is idempotent:
re-running regenerates byte-identical folders.

Usage:
  python3 other/migrations/migrate_runscripts.py --check <file.csv> [...]
  python3 other/migrations/migrate_runscripts.py --write <file.csv> [...]
  python3 other/migrations/migrate_runscripts.py --check --all   # every live runscript

Add --legs=economy for the Phase 3 wave: translates only the economy-side
columns and leaves the behavior column as module paths (Phase 4 finishes it).

Run from the repo root. --check reports without writing; --write rewrites the
CSVs in place and writes the set folders (review the git diff).
"""

import csv, glob, os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from economy_sets import default_types, set_name_for, write_set, ECONOMY_SETS_ROOT

FIXED = ['ID', 'tax_law', 'economy', 'behavior', 'years', 'dist_years',
         'mtr_vars', 'mtr_types']
PASSTHROUGH_OLD = ['ID', 'tax_law', 'years', 'dist_years', 'mtr_vars', 'mtr_types']
DROP = ['corp_incidence_phasein', 'user_id']

COMBO_MAP_PATH = 'other/migrations/behavior_combo_map.csv'
# Phase 3 wave flag (--legs=economy): leave the behavior column untranslated.
ECONOMY_ONLY = False


def norm_interface(x):
    return re.sub(r'[ -]', '_', x).lower()


def map_assumption(ch, nm):
    """Old assumption.{ch}.{nm} -> (leg, channel, name)."""
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
        return ('economy', 'wealth', 'cap_flows_pt_weight')
    if ch == 'wealth':
        return ('behavior', 'wealth_avoidance', nm)
    raise ValueError(f'no leg mapping for assumption.{ch}.{nm}')


def load_combo_map():
    out = {}
    with open(COMBO_MAP_PATH, newline='') as fh:
        for row in csv.DictReader(fh):
            key = frozenset(row['combo'].split())
            set_name = row['set']
            if key in out and out[key] != set_name:
                raise ValueError(f'combo map collision after normalization: {row["combo"]}')
            out[key] = set_name
    return out


# ------------------------------------------------------------------ migration

def migrate_file(path, combo_map):
    """Returns (problems, notes, new_rows, header) -- no writes."""
    with open(path, newline='') as fh:
        reader = csv.reader(fh)
        header = next(reader, [])
    with open(path, newline='') as fh:
        rows = list(csv.DictReader(fh))

    problems, notes = [], []

    if 'first_year' in header or 'last_year' in header:
        return ([f'{path}: pre-years schema (first_year/last_year) -- archive it, not migratable'],
                [], None)
    if list(header) == FIXED:
        return [], [f'{path}: already on the three-leg schema, skipped'], None

    new_rows = []
    for r in rows:
        nr = {c: (r.get(c) or '').strip() for c in PASSTHROUGH_OLD}
        nr['economy'] = ''
        nr['behavior'] = ''
        entries = {}      # (channel, name) -> value, this row's economy set

        for h in header:
            v = (r.get(h) or '').strip()
            if h in PASSTHROUGH_OLD or h in DROP or not v:
                continue
            if h == 'assumptions':
                problems.append(f'{path}: `assumptions` folder column in use ({v}) -- migrate by hand')
            elif m := re.fullmatch(r'dep\.(.+)\.vintage', h):
                entries[('interfaces', f'{norm_interface(m.group(1))}_vintage')] = v
            elif m := re.fullmatch(r'dep\.(.+)\.ID', h):
                entries[('interfaces', f'{norm_interface(m.group(1))}_id')] = v
            elif m := re.fullmatch(r'assumption\.([a-z0-9_]+)\.([a-z0-9_]+)', h):
                try:
                    leg, ch, nm = map_assumption(m.group(1), m.group(2))
                except ValueError as e:
                    problems.append(f'{path}: {e}')
                    continue
                if leg == 'economy':
                    entries[(ch, nm)] = v
                else:
                    problems.append(
                        f'{path}: behavior-leg value assumption.{m.group(1)}.{m.group(2)}={v} '
                        f'needs a named behavior set ({ch}.{nm}) -- Phase 4, migrate by hand')
            elif h == 'economy':
                problems.append(
                    f'{path}: row {r.get("ID")} already names an economy set ({v}) -- '
                    'merge it by hand rather than re-migrating')
            elif m := re.fullmatch(r'economy\.([a-z0-9_]+)\.([a-z0-9_]+)', h):
                # The Phase 3 interim schema had dotted per-value columns; the
                # files-only ruling turns each into a set-folder entry.
                entries[(m.group(1), m.group(2))] = v
            elif m := re.fullmatch(r'behavior\.([a-z0-9_]+)\.([a-z0-9_]+)', h):
                problems.append(
                    f'{path}: behavior-leg value {h}={v} needs a named behavior '
                    f'set -- Phase 4, migrate by hand')
            elif h in ('excess_growth', 'excess_growth_start_year',
                       'excess_growth_all_rev'):
                entries[('growth', h)] = v
            elif h == 'wealth_financing':
                entries[('wealth', 'financing_profile')] = v
            elif h == 's':
                entries.setdefault(('wealth', 'financing_profile'), f'flat:{v}')
            elif h == 'behavior':
                if ECONOMY_ONLY:
                    nr['behavior'] = v   # Phase 3 wave: module paths stay live
                    continue
                key = frozenset(v.split())
                if key not in combo_map:
                    problems.append(f'{path}: unmapped behavior combo: {v!r} -- add it to {COMBO_MAP_PATH}')
                else:
                    nr['behavior'] = combo_map[key]
            else:
                problems.append(f'{path}: unknown column `{h}` (strict whitelist) -- resolve by hand')

        # wealth_financing wins over s where both are set (old precedence)
        if (r.get('wealth_financing') or '').strip() and (r.get('s') or '').strip():
            entries[('wealth', 'financing_profile')] = (r.get('wealth_financing') or '').strip()

        nr['_entries'] = entries
        new_rows.append(nr)

    if problems:
        return problems, notes, None
    return [], notes, new_rows


def main():
    global ECONOMY_ONLY
    args = sys.argv[1:]
    write = '--write' in args
    check = '--check' in args
    ECONOMY_ONLY = '--legs=economy' in args
    if write == check:
        sys.exit('pass exactly one of --check / --write')
    args = [a for a in args if a not in ('--check', '--write', '--legs=economy')]

    if '--all' in args:
        # private/ is untracked by design: its set folders would name private
        # scenarios in a tracked config file. Migrate those on demand by path.
        paths = [p for p in sorted(glob.glob('config/runscripts/**/*.csv', recursive=True))
                 if not p.startswith('config/runscripts/archive/')
                 and not p.startswith('config/runscripts/private/')
                 and not p.endswith('_legend.csv')]
    else:
        paths = args
    if not paths:
        sys.exit('no runscripts given')

    combo_map = load_combo_map()
    all_problems, all_notes = [], []
    migrated = {}                 # path -> rows
    sets = {}                     # name -> entries
    used_by = {}                  # name -> {"runscript:ID", ...}

    for p in paths:
        problems, notes, rows = migrate_file(p, combo_map)
        all_problems += problems
        all_notes += notes
        if rows is None:
            continue
        stem = os.path.relpath(p, 'config/runscripts')[:-len('.csv')]
        for nr in rows:
            entries = nr.pop('_entries')
            if not entries:
                continue
            name = set_name_for(entries)
            if name in sets and sets[name] != entries:
                all_problems.append(
                    f'set name collision: `{name}` maps to two different contents '
                    f'({sets[name]} vs {entries})')
                continue
            sets[name] = entries
            used_by.setdefault(name, set()).add(f'{stem}:{nr["ID"]}')
            nr['economy'] = os.path.join('sets', name)
        migrated[p] = rows

    for n in all_notes:
        print(n)
    if all_problems:
        print('\nPROBLEMS:')
        for p in all_problems:
            print(' -', p)
        sys.exit(1)

    print(f'\n{len(migrated)} runscripts migratable, {len(sets)} distinct economy sets')

    if not write:
        for name in sorted(sets):
            print(f'  sets/{name}  <- {len(used_by[name])} scenario row(s)')
        return

    types = default_types()
    for name, entries in sorted(sets.items()):
        write_set(entries, name, used_by[name], types)

    for p, rows in migrated.items():
        with open(p, 'w', newline='') as fh:
            w = csv.DictWriter(fh, fieldnames=FIXED, lineterminator='\n')
            w.writeheader()
            for nr in rows:
                w.writerow({c: nr.get(c, '') for c in FIXED})
    print(f'wrote {len(migrated)} runscripts and {len(sets)} set folders under {ECONOMY_SETS_ROOT}/')


if __name__ == '__main__':
    main()
