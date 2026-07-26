#!/usr/bin/env python3
"""Naming and emission for economy SET folders.

A runscript names files, not values, so anything that generates runscript rows
(the migration, the top-tax dial and factorial builders, any future sweep) also
generates the set folders those rows point at. This module is the one place
that decides what a set is called and what its YAML looks like, so two
generators asking for the same pins land on the same folder.

Set names are derived from CONTENT: same entries -> same name -> same folder,
which is what makes deduplication and re-running automatic.

Usage from a generator (run from the repo root):

    import sys; sys.path.insert(0, 'other/migrations')
    from economy_sets import ensure_set
    cell = ensure_set({('interfaces', 'off_model_estimates_vintage'): '20250925',
                       ('interfaces', 'off_model_estimates_id'):      'baseline',
                       ('wealth', 'financing_profile'):               'flat:0.5'},
                      used_by='top_tax/dials')
    # cell == 'sets/ome_20250925_s50'  -- write it into the row's economy column
"""

import glob, os, re

ECONOMY_DEFAULT_ROOT = 'config/scenarios/economy/default'
ECONOMY_SETS_ROOT = 'config/scenarios/economy/sets'

# Short tokens for interface keys, used to build set folder names.
IFACE_ABBREV = {
    'tax_data': 'td',
    'macro_projections': 'macro',
    'value_added_tax_model': 'vat',
    'off_model_estimates': 'ome',
    'cost_recovery_simulator': 'crs',
    'estate_tax_distribution': 'etd',
    'tax_expenditure_model': 'tem',
}

# Deterministic token order within a set name.
IFACE_ORDER = ['tax_data', 'macro_projections', 'value_added_tax_model',
               'off_model_estimates', 'cost_recovery_simulator',
               'estate_tax_distribution', 'tax_expenditure_model']


def _num_token(v):
    """0.001 -> 0p001, 0 -> 0, 2026 -> 2026."""
    return str(v).replace('.', 'p').replace('-', 'neg')


def set_name_for(entries):
    """Content-derived folder name for one economy set.

    entries: {(channel, name): value}
    """
    ifaces = {}
    for (ch, nm), v in entries.items():
        if ch != 'interfaces':
            continue
        if nm.endswith('_vintage'):
            ifaces.setdefault(nm[:-len('_vintage')], {})['vintage'] = v
        elif nm.endswith('_id'):
            ifaces.setdefault(nm[:-len('_id')], {})['id'] = v

    tokens = []
    for key in IFACE_ORDER + sorted(set(ifaces) - set(IFACE_ORDER)):
        if key not in ifaces:
            continue
        abbrev = IFACE_ABBREV.get(key, ''.join(w[0] for w in key.split('_')))
        parts = [abbrev]
        if 'vintage' in ifaces[key]:
            parts.append(str(ifaces[key]['vintage']))
        if str(ifaces[key].get('id', 'baseline')) != 'baseline':
            parts.append(str(ifaces[key]['id']))
        elif 'vintage' not in ifaces[key]:
            parts.append('baseline')
        tokens.append('_'.join(parts))

    prof = entries.get(('wealth', 'financing_profile'))
    if prof is not None:
        prof = str(prof)
        if prof == 'none':
            tokens.append('snone')
        elif prof.startswith('flat:'):
            pct = float(prof[5:]) * 100
            tokens.append('s' + (str(int(pct)) if pct == int(pct) else _num_token(pct)))
        else:
            tokens.append('prof_' + prof)

    for (ch, nm), v in sorted(entries.items()):
        if ch != 'growth':
            continue
        if nm == 'excess_growth':
            tokens.append('xg' + _num_token(v))
        elif nm == 'excess_growth_start_year':
            tokens.append('from' + str(v))
        elif nm == 'excess_growth_all_rev':
            tokens.append('allrev' + str(v))

    for (ch, nm), v in sorted(entries.items()):
        if ch in ('interfaces', 'wealth', 'growth'):
            continue
        tokens.append(f'{ch}_{nm}_{_num_token(v)}')

    if not tokens:
        raise ValueError('empty economy set has no name (and needs no folder)')
    return '_'.join(tokens)


def _entry_values(path):
    """{entry name: value} for one channel file.

    Uses PyYAML where it is installed, and falls back to a literal scan of the
    top-level `name:` / `  value:` pairs otherwise -- the migrator runs under
    the cluster's R module, whose python3 has no PyYAML, and this module only
    ever needs scalar entries. An entry whose value is not a plain scalar
    (a block list, a folded string) is skipped by the fallback and reports as
    unknown, which callers turn into an explicit error rather than a guess.
    """
    try:
        import yaml
    except ImportError:
        yaml = None

    if yaml is not None:
        return {nm: entry.get('value')
                for nm, entry in (yaml.safe_load(open(path)) or {}).items()
                if nm != '_channel' and isinstance(entry, dict)}

    out, entry = {}, None
    for line in open(path):
        line = line.rstrip('\n')
        if not line.strip() or line.lstrip().startswith('#'):
            continue
        m = re.match(r'^([A-Za-z_][\w]*):\s*$', line)
        if m:
            entry = m.group(1) if m.group(1) != '_channel' else None
            continue
        m = re.match(r'^\s+value:\s*(\S.*)$', line)
        if m and entry is not None:
            out[entry] = _literal(m.group(1).strip())
            entry = None
    return out


def _literal(text):
    """Python value for a scalar YAML literal (fallback parser only)."""
    if (text[:1], text[-1:]) in (("'", "'"), ('"', '"')):
        return text[1:-1]
    low = text.lower()
    if low in ('true', 'false'):
        return low == 'true'
    if low in ('.inf', '-.inf', 'inf'):
        return float('inf')
    try:
        return int(text)
    except ValueError:
        pass
    try:
        return float(text)
    except ValueError:
        return text


def default_types():
    """{(channel, name): python type} from the economy default set.

    A set entry must land on the default's type: '2026050315' is a Tax-Data
    vintage STRING, 0.001 is an excess-growth NUMBER, and quoting the second
    would hand the model a character where it expects a double.
    """
    out = {}
    for f in sorted(glob.glob(os.path.join(ECONOMY_DEFAULT_ROOT, '*.yaml'))):
        ch = os.path.basename(f)[:-len('.yaml')]
        for nm, value in _entry_values(f).items():
            out[(ch, nm)] = type(value)
    return out


def render_value(v, typ):
    if typ in (int, float):
        num = float(v)
        return str(int(num)) if num == int(num) and typ is int else str(v)
    if typ is bool:
        return 'true' if str(v).strip().lower() in ('true', '1', 't', 'yes') else 'false'
    return "'%s'" % v


def set_files(entries, name, used_by, types=None, generator=None):
    """Render one set folder: {filename: file text}."""
    types = types if types is not None else default_types()
    by_channel = {}
    for (ch, nm), v in entries.items():
        by_channel.setdefault(ch, {})[nm] = v

    users = sorted({str(u).split(':')[0] for u in used_by})
    shown = ', '.join(users[:6]) + (f' (+{len(users) - 6} more)' if len(users) > 6 else '')
    origin = generator or 'other/migrations/migrate_runscripts.py'

    out = {}
    for ch, items in sorted(by_channel.items()):
        lines = [
            '# Economy set `%s` -- %s channel.' % (name, ch),
            '#',
            '# Machine-generated by %s.' % origin,
            '# A sparse delta over config/scenarios/economy/default/.',
            '#',
            '# Used by: ' + shown,
            '',
        ]
        for nm in sorted(items):
            typ = types.get((ch, nm))
            if typ is None:
                raise ValueError(
                    f'set `{name}` names an economy entry with no readable '
                    f'default: {ch}.{nm}')
            lines.append('%s:' % nm)
            lines.append('  value: %s' % render_value(items[nm], typ))
            lines.append('')
        out['%s.yaml' % ch] = '\n'.join(lines).rstrip('\n') + '\n'
    return out


def write_set(entries, name, used_by, types=None, generator=None):
    folder = os.path.join(ECONOMY_SETS_ROOT, name)
    os.makedirs(folder, exist_ok=True)
    for fname, text in set_files(entries, name, used_by, types, generator).items():
        with open(os.path.join(folder, fname), 'w') as fh:
            fh.write(text)
    return folder


def read_set(name):
    """{(channel, name): value} of an existing set folder, {} if absent."""
    folder = os.path.join(ECONOMY_SETS_ROOT, name)
    out = {}
    for f in sorted(glob.glob(os.path.join(folder, '*.yaml'))):
        ch = os.path.basename(f)[:-len('.yaml')]
        for nm, value in _entry_values(f).items():
            out[(ch, nm)] = value
    return out


def ensure_set(entries, used_by, types=None, generator=None):
    """Write the set folder for `entries` if needed; return the runscript cell.

    A set already on disk with the same values is left alone -- names are
    content-derived, so several generators legitimately land on one shared
    folder, and the last one through should not claim its header comment.

    Returns '' for an empty entry set (the row wants the default economy).
    """
    entries = {k: v for k, v in entries.items() if v not in (None, '')}
    if not entries:
        return ''
    name = set_name_for(entries)
    existing = read_set(name)
    same = existing and all(
        str(existing.get(k)) == str(v) for k, v in entries.items())
    if not same:
        write_set(entries, name, used_by if isinstance(used_by, (list, set, tuple))
                  else [used_by], types, generator)
    return os.path.join('sets', name)
