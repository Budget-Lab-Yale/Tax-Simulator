#!/usr/bin/env python3
"""Manifest mapping check: did any assumption value change while being relocated?

The old model wrote one flat manifest, assumptions.csv, holding all 45 assumption
values for every scenario. The rebuild scattered those values across three
destinations, and this asks whether each one arrived intact:

  economy leg       -> scenario_config.csv in the candidate vintage
  kg calibrations   -> calibrations.csv in the candidate vintage, or, when the
                       scenario binds no kg pieces, the calibration file itself
  behavior module   -> a top-level constant in the module's own .R file

That third destination is why this script had to be rewritten. The version
recovered from the abandoned branch mapped module-only parameters onto a behavior
leg that carried VALUES; this design has no such thing -- the nine parameters
with exactly one module reader live in that module's file, and no manifest
mentions them. Checking them means reading the source.

The second destination has a wrinkle worth knowing: a BOUND calibration appears
in calibrations.csv only for scenarios that bind it, so a baseline row's kg.eta
is legitimately absent from the manifest. It is checked against the file instead.

Usage: python3 mapping_check.py <golden_vintage_dir> <candidate_vintage_dir>
Exit 0 = pass.
"""

import csv
import os
import re
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Old (channel, name) -> where the value lives now.
#   ('economy', channel, name)          a scenario_config.csv row
#   ('calib', file stem, name)          a calibrations.csv row, or that file
#   ('module', path, constant)          a top-level constant in an R file
EVASION = 'src/behavior/evasion/debacker.R'
ESTATE = 'src/behavior/estate/avoidance.R'
WEALTH = 'src/behavior/wealth/avoidance.R'

DESTINATION = {
    ('estate', 'report_eps'): ('module', ESTATE, 'ESTATE_REPORT_EPS'),
    ('evasion', 'e_schc'): ('module', EVASION, 'EVASION_E_SCHC'),
    ('evasion', 'e_pt'): ('module', EVASION, 'EVASION_E_PT'),
    ('evasion', 'e_rent'): ('module', EVASION, 'EVASION_E_RENT'),
    ('evasion', 'topend_mult'): ('module', EVASION, 'EVASION_TOPEND_MULT'),
    ('wealth', 'avoid_public_e'): ('module', WEALTH, 'WEALTH_AVOID_PUBLIC_E'),
    ('wealth', 'avoid_private_e'): ('module', WEALTH, 'WEALTH_AVOID_PRIVATE_E'),
    ('wealth', 'chi_pub'): ('module', WEALTH, 'CHI_PUB'),
    ('wealth', 'chi_priv'): ('module', WEALTH, 'CHI_PRIV'),
    ('sigma', 'conv'): ('calib', 'conversion.yaml', 'conv'),
    ('sigma', 'pt_labor_share'): ('calib', 'conversion.yaml', 'pt_labor_share'),
}

# The four calibrated response parameters, in bathtub.yaml.
for _nm in ('eta', 'eta_logs', 'timeable_share', 'timeable_share_logs'):
    DESTINATION[('kg', _nm)] = ('calib', 'bathtub.yaml', _nm)

# Everything else on the kg channel is a switch or judgment call, in settings.yaml.
for _nm in ('response_form', 'applier_allocation', 'dg_allocation', 'timing_window',
            'timing_ref_wedge', 'wealth_carry_scale', 'beta_fallback',
            'deemed_avoidance', 'char_extensive_intercept', 'char_extensive_ln_slope',
            'char_intensive_intercept', 'char_intensive_ln_slope', 'char_base_year'):
    DESTINATION[('kg', _nm)] = ('calib', 'settings.yaml', _nm)


def destination(channel, name):
    if (channel, name) in DESTINATION:
        return DESTINATION[(channel, name)]
    # corp, distribution and wealth.cap_flows_pt_weight stayed economy-side under
    # their own channel names.
    if channel in ('corp', 'distribution', 'wealth'):
        return ('economy', channel, name)
    raise ValueError(f'no destination recorded for {channel}.{name}')


def norm(v):
    """Compare across writers that disagree about formatting, not about value."""
    s = str(v).strip().strip('"\'')
    if s.upper() in ('TRUE', 'FALSE'):
        return s.upper()
    try:
        return f'{float(s):.10g}'
    except (TypeError, ValueError):
        return s


def r_constants(path):
    """Top-level NAME = value assignments in an R file."""
    out = {}
    pat = re.compile(r'^([A-Z][A-Z0-9_]*)\s*(?:=|<-)\s*([^\s#]+)')
    with open(os.path.join(REPO, path)) as fh:
        for line in fh:
            m = pat.match(line)
            if m:
                out.setdefault(m.group(1), m.group(2).rstrip(','))
    return out


def yaml_scalars(path):
    """`name:` followed by `value: x` -- enough for these flat calibration files,
    and deliberately not a YAML parser: nothing here may rewrite those files."""
    out, key = {}, None
    with open(os.path.join(REPO, path)) as fh:
        for line in fh:
            m = re.match(r'^([A-Za-z_][A-Za-z0-9_]*):\s*$', line)
            if m:
                key = m.group(1)
                continue
            m = re.match(r'^\s+value:\s*(.+?)\s*$', line)
            if m and key:
                out.setdefault(key, m.group(1))
                key = None
    return out


def main(golden, cand):
    old = list(csv.DictReader(open(f'{golden}/assumptions.csv', newline='')))
    eco = list(csv.DictReader(open(f'{cand}/scenario_config.csv', newline='')))
    cal = list(csv.DictReader(open(f'{cand}/calibrations.csv', newline='')))

    eco_idx = {(r['ID'], r['channel'], r['name']): r for r in eco}
    cal_idx = {(r['ID'], os.path.basename(r['file']), r['name']): r for r in cal}

    module_cache, yaml_cache = {}, {}
    problems, seen_eco = [], set()
    counts = {'economy': 0, 'calib': 0, 'calib_from_file': 0, 'module': 0}

    for r in old:
        ch, nm, want = r['channel'], r['name'], r['value']
        try:
            kind, where, key = destination(ch, nm)
        except ValueError as e:
            problems.append(str(e))
            continue

        if kind == 'economy':
            c = eco_idx.get((r['ID'], where, key))
            if c is None:
                problems.append(f'{r["ID"]}: {ch}.{nm} has no economy row at {where}.{key}')
                continue
            seen_eco.add((r['ID'], where, key))
            counts['economy'] += 1
            got, src = c['value'], f'scenario_config economy.{where}.{key}'
            if r['kind'] != c['kind']:
                problems.append(f'{r["ID"]}: {ch}.{nm} kind {r["kind"]} vs {c["kind"]}')

        elif kind == 'calib':
            c = cal_idx.get((r['ID'], where, key))
            if c is not None:
                counts['calib'] += 1
                got, src = c['value'], f'calibrations.csv {where}.{key}'
            else:
                # Not bound by this scenario. Check the file it would have bound.
                path = f'config/calibrations/kg/{where}'
                if path not in yaml_cache:
                    yaml_cache[path] = yaml_scalars(path)
                if key not in yaml_cache[path]:
                    problems.append(f'{r["ID"]}: {ch}.{nm} is in neither '
                                    f'calibrations.csv nor {path}')
                    continue
                counts['calib_from_file'] += 1
                got, src = yaml_cache[path][key], f'{path} (unbound by this scenario)'

        else:
            if where not in module_cache:
                module_cache[where] = r_constants(where)
            if key not in module_cache[where]:
                problems.append(f'{r["ID"]}: {ch}.{nm} -- no constant {key} in {where}')
                continue
            counts['module'] += 1
            got, src = module_cache[where][key], f'{where}:{key}'

        if norm(want) != norm(got):
            problems.append(f'{r["ID"]}: {ch}.{nm} was {want!r}, now {got!r} at {src}')

    # The reverse direction, economy leg only: a candidate row with no old
    # counterpart is either genuinely new surface or an accident.
    NEW_SURFACE_CHANNELS = {'interfaces'}
    NEW_SURFACE_ENTRIES = {('estate', 'valuation_bridge'),
                           ('wealth', 'financing_profile'),
                           ('wealth', 'n_pctiles'),
                           ('wealth', 'fmax'),
                           ('wealth', 'r_total_additive_delta')}
    for k in eco_idx:
        if (k in seen_eco or k[1] in NEW_SURFACE_CHANNELS
                or (k[1], k[2]) in NEW_SURFACE_ENTRIES):
            continue
        problems.append(f'{k[0]}: candidate economy row {k[1]}.{k[2]} '
                        'is neither mapped from the old manifest nor allowlisted')

    print(f'old manifest rows: {len(old)}')
    print(f'  located in scenario_config.csv : {counts["economy"]}')
    print(f'  located in calibrations.csv    : {counts["calib"]}')
    print(f'  read from the calibration file : {counts["calib_from_file"]} '
          '(scenario binds no kg pieces)')
    print(f'  read from a module file        : {counts["module"]}')

    if problems:
        print('MAPPING FAILURES:')
        for p in problems:
            print(' -', p)
        sys.exit(1)
    print('MAPPING_CHECK_PASS')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
