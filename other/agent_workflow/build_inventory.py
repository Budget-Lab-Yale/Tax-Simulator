#!/usr/bin/env python3
"""
build_inventory.py

Regenerates other/agent_workflow/MODEL_INVENTORY.md from config/assumptions/*.yaml
and config/interfaces/interface_versions.yaml, so the readable inventory and the
machine-readable config can never disagree.

Run from the repo root:  python3 other/agent_workflow/build_inventory.py
"""

import os
import glob
import textwrap
import yaml

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT = os.path.join(ROOT, 'other', 'agent_workflow', 'MODEL_INVENTORY.md')

KIND_ORDER = ['calibrated', 'sourced', 'judgment', 'structural']
KIND_BLURB = {
    'calibrated': 'Output of a procedure, so it can go stale. Checked on every '
                  'run against the data vintages it was derived under and the '
                  'content of the files listed as invalidating it. A mismatch '
                  'stops the run.',
    'sourced':    'Taken from a paper or an agency convention. Cannot go stale; '
                  'can be superseded by better evidence.',
    'judgment':   'Somebody chose it. No derivation recorded. These are the '
                  'entries worth ranking for sensitivity analysis.',
    'structural': 'A model-form switch or a conditioning rule rather than a '
                  'measured magnitude.',
}


def one_line(text):
    if text is None:
        return ''
    return ' '.join(str(text).split())


def truncate(text, n=200):
    t = one_line(text)
    return t if len(t) <= n else t[:n - 1].rstrip() + '…'


def main():
    channels = {}
    for path in sorted(glob.glob(os.path.join(ROOT, 'config', 'assumptions', '*.yaml'))):
        name = os.path.splitext(os.path.basename(path))[0]
        with open(path) as fh:
            channels[name] = yaml.safe_load(fh) or {}

    with open(os.path.join(ROOT, 'config', 'interfaces',
                           'interface_versions.yaml')) as fh:
        interfaces = yaml.safe_load(fh)

    counts = {k: 0 for k in KIND_ORDER}
    for entries in channels.values():
        for e in entries.values():
            counts[e.get('kind', 'judgment')] = counts.get(e.get('kind'), 0) + 1
    total = sum(counts.values())

    L = []
    add = L.append

    add('# What the model is currently made of')
    add('')
    add('**GENERATED FILE — do not hand-edit.** Rebuild with')
    add('`python3 other/agent_workflow/build_inventory.py` after changing anything')
    add('under `config/assumptions/`. The YAML is the source of truth; this page is')
    add('a rendering of it.')
    add('')
    add('Every fixed economic number the model relies on, and the input data')
    add('versions they were set against. Machinery: `src/misc/assumptions.R`;')
    add('conventions: the "Model Assumptions" section of CLAUDE.md.')
    add('')
    add(f'**{total} assumptions** across {len(channels)} channels: '
        + ', '.join(f'{counts[k]} {k}' for k in KIND_ORDER if counts.get(k)) + '.')
    add('')

    add('## Input data versions')
    add('')
    add('| Interface | Version | Default vintage |')
    add('|---|---|---|')
    for name, spec in interfaces.items():
        if not isinstance(spec, dict) or spec.get('default_vintage') is None:
            continue
        add(f"| {name} | {spec.get('version')} | `{spec['default_vintage']}` |")
    add('')

    add('## What each kind means')
    add('')
    for k in KIND_ORDER:
        add(f'- **{k}** ({counts.get(k, 0)}) — {KIND_BLURB[k]}')
    add('')

    for channel in sorted(channels):
        entries = channels[channel]
        if not entries:
            continue
        add(f'## {channel}')
        add('')
        add('| Assumption | Value | Kind | Set | Provenance |')
        add('|---|---|---|---|---|')
        for kind in KIND_ORDER:
            for name, e in entries.items():
                if e.get('kind') != kind:
                    continue
                prov = (e.get('target') or e.get('citation') or e.get('note') or '')
                extra = ''
                if kind == 'calibrated':
                    du = e.get('derived_under', {}) or {}
                    extra = ' <br> _derived under: ' + ', '.join(
                        f'{k} `{v}`' for k, v in du.items()) + '_'
                    if e.get('rederive'):
                        extra += f" <br> _re-derive: `{e['rederive']}`_"
                    if e.get('acknowledged'):
                        extra += (' <br> **ACKNOWLEDGED STALE** '
                                  f"({e['acknowledged'].get('date')})")
                add(f"| `{channel}.{name}` | `{e.get('value')}` | {kind} | "
                    f"{e.get('set', '—')} | {truncate(prov)}{extra} |")
        add('')

    unowned = [(c, n) for c, es in channels.items() for n, e in es.items()
               if e.get('kind') == 'judgment']
    if unowned:
        add('## Entries with no recorded derivation')
        add('')
        add('These are the numbers somebody chose. Nothing is wrong with a')
        add('judgment call, but these are where a sensitivity ranking should')
        add('start, and where a citation would retire the entry from this list.')
        add('')
        for c, n in sorted(unowned):
            add(f'- `{c}.{n}`')
        add('')

    add('## Not covered here')
    add('')
    add('- **Numerical plumbing** — epsilons, convergence tolerances, guard caps')
    add('  (`fmax`, `CORP_MU_MAX`), structural bounds (age topcodes). Not economics.')
    add('- **The estate measurement bridge** — `r`, `rho_pt`, the SOI per-bin')
    add('  fractions, the gift add-back, the cluster cap. They live in')
    add('  `config/estate/estate_valuation_params.yaml`, are generated by')
    add('  `other/estate_tax/write_frozen_params.R`, and measure the data rather')
    add('  than describe a counterfactual.')
    add('- **The wealth saving profile** `s` and transition matrix `M` — a table,')
    add('  not a scalar, selected per scenario by the `wealth_financing` runscript')
    add('  column from `config/wealth/profiles/`.')
    add('- **Behavior-module parameters** that only affect their own module (the')
    add('  entity-shifting `alpha`, the Bastian labor elasticities, and so on).')
    add('  A scenario varies those by pointing at a different module, which is the')
    add('  existing mechanism. Where another channel\'s calibration depends on one')
    add('  — evasion feeds sigma, for instance — the module file is listed in that')
    add('  calibration\'s `invalidated_by`, so a change to it still trips the check.')
    add('')

    with open(OUT, 'w') as fh:
        fh.write('\n'.join(L))
    print(f'wrote {OUT} ({total} assumptions across {len(channels)} channels)')


if __name__ == '__main__':
    main()
