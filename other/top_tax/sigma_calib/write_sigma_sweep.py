#!/usr/bin/env python3
"""Write the sigma sweep grid: calibration files, behavior alternatives, runscripts.

sigma is the conversion margin -- the share of a top ordinary-rate increase that
gets converted rather than avoided some other way. It is calibrated as a RESIDUAL:
entity shifting and evasion together already produce a top-subset ordinary ETI of
about 0.22, the Saez-Slemrod-Giertz central target is 0.25, and sigma is whatever
closes the gap. So pinning it means running the same +5pp top-ordinary shock at
several trial sigma values, measuring the ETI at each, and interpolating for the
sigma that lands on 0.25.

That interpolation was done by hand in July 2026, from numbers read out of a log.
This generator plus measure_sigma.R replace the hand step: a trial value is a file,
bound by its own behavior alternative, named by its own runscript, and the answer is
written into config/calibrations/kg/conversion.yaml by the script that measures it.

Three artifacts per grid point, mirroring the eta sweep
(other/kg_model_tests/form_ab/write_bathtub_sweep.py, whose block splitter this reuses):

  config/calibrations/kg/sweeps/conv_<tag>/conversion.yaml
      the shipped conversion calibration with `conv` replaced by the trial value.
      Keeps the base name `conversion.yaml` -- entries are labelled
      '{file stem}.{entry}', so renaming it would relabel every entry and detach any
      waiver written against `conversion.conv`.

  config/scenarios/behavior/alternatives/sigma_calib_<charity>_conv_<tag>/behavior.yaml
      the calibration stack: bathtub + conversion bound to the sweep file, entity
      shifting, evasion, charity, estate. NOT a product stack -- it exists to
      reproduce the conditions sigma was derived under.

  config/runscripts/tests/sigma_calib_<charity>_conv_<tag>.csv
      baseline plus the topord_plus5 leg naming that alternative.

WHICH CHARITY ELASTICITY, and why it is a flag rather than a constant. sigma's
shipped 0.16 was derived under charity/100 (elasticity -1.0), while every product
run uses charity/50 (-0.5). That mismatch is recorded as a dated waiver on the entry
and is the reason the sigma re-derivation is the first follow-up after the config
rebuild -- it WILL move the value. So:

    --charity 100   reproduces the conditions behind the shipped 0.16
    --charity 50    is the deferred re-derivation

Default is 100, so running this with no flag sets up a reproduction rather than
silently changing the basis of a shipped number.

DELIBERATELY NOT LAUNCHED. The re-derivation is an author decision, not a
consequence of generating its inputs. Nothing here runs the model.

Usage (from repo root):
    python3 other/top_tax/sigma_calib/write_sigma_sweep.py
    python3 other/top_tax/sigma_calib/write_sigma_sweep.py --charity 50

Regenerate-and-diff is the check: an empty `git status --short` means the tree
matches what the script would write.
"""

import importlib.util
import os
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))

# The bathtub sweep's block splitter, imported rather than copied: both scripts are
# rewriting one entry of a calibration file textually, for the same reason (the
# comments in those files ARE the provenance, and a YAML round-trip deletes them).
_eta_path = os.path.join(REPO, 'other', 'kg_model_tests', 'form_ab',
                         'write_bathtub_sweep.py')
_spec = importlib.util.spec_from_file_location('write_bathtub_sweep', _eta_path)
_eta = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_eta)
split_blocks = _eta.split_blocks

SOURCE_CALIB = 'config/calibrations/kg/conversion.yaml'
SWEEP_ROOT = 'config/calibrations/kg/sweeps'
BEHAVIOR_ROOT = 'config/scenarios/behavior/alternatives'
RUNSCRIPT_ROOT = 'config/runscripts/tests'

THIS_SCRIPT = 'other/top_tax/sigma_calib/write_sigma_sweep.py'
MEASURE_SCRIPT = 'other/top_tax/sigma_calib/measure_sigma.R'
LAUNCHER = 'other/top_tax/sigma_calib/launch_sigma_calib'

# The trial grid. 0.0 is the no-conversion floor, which is what shows sigma is a
# residual: it measures the ETI entity shifting and evasion produce alone. It is
# bound with sigma = 0 rather than by dropping the conversion module, so the leg
# still writes the sigma gate thresholds the ETI measurement reads -- the old
# no-sigma leg had none of its own and had to borrow them from another run.
GRID = [('00', '0.0'), ('16', '0.16'), ('30', '0.30')]

TARGET_ETI = 0.25

# The shock leg. Its tax law folder survived the 2026-07-26 runscript archiving.
SHOCK_TAXLAW = 'tests/topord_plus5'
SHOCK_ID = 'topord_plus5'
YEARS = '2025:2035'
DIST_YEARS = '2026:2026'
MTR_VARS = ('kg_lt wages1 wages2 part_active sole_prop1 scorp_active rent '
            'char_cash estate')

MODULES = [
    'src/behavior/conversion/sigma.R',
    'src/behavior/entity_shifting/pearce_prisinzano.R',
    'src/behavior/evasion/debacker.R',
    'src/behavior/charity/{charity}.R',
    'src/behavior/estate/avoidance.R',
]


def read(path):
    with open(os.path.join(REPO, path)) as f:
        return f.read()


def write(path, text):
    full = os.path.join(REPO, path)
    os.makedirs(os.path.dirname(full), exist_ok=True)
    with open(full, 'w', newline='\n') as f:
        f.write(text)
    return full


def sweep_header(tag, value, charity):
    grid_set = ', '.join(v for _, v in GRID)
    return f"""# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.
#
# SWEEP POINT: conv = {value}. One point on the trial grid {{{grid_set}}} used to
# pin the conversion margin.
# This is not a shipped calibration and no scenario should bind it except the
# sigma-calibration runscript generated alongside it.
#
# Everything here is the shipped config/calibrations/kg/conversion.yaml with
# exactly one entry replaced -- conv -- because a sweep varies one thing and holds
# the rest fixed. pt_labor_share keeps its own provenance verbatim.
#
# Bound by  : config/scenarios/behavior/alternatives/sigma_calib_{charity}_conv_{tag}/
# Measured by: {MEASURE_SCRIPT}
#
# The file name matters. It has to stay `conversion.yaml` inside a
# differently-named FOLDER, because entries are labelled '{{file stem}}.{{entry}}'
# -- calling it conv_{tag}.yaml would relabel every entry in it and detach any
# waiver written against `conversion.conv`.
"""


def trial_block(value):
    grid_set = ', '.join(v for _, v in GRID)
    return f"""conv:
  value: {value}
  kind: judgment
  note: >
    SWEEP TRIAL, not a calibrated value -- which is why the kind is judgment
    rather than calibrated. It has no derivation to go stale: it is an INPUT to
    the calibration that pins conv, one of the points on the grid
    {{{grid_set}}}. The measurement script reads the vintages this grid
    produces, measures the top-subset ordinary ETI at each, and interpolates for
    the sigma that hits the target ETI of {TARGET_ETI}. The result of that
    interpolation is the shipped value in config/calibrations/kg/conversion.yaml;
    this number is scaffolding for producing it.
    The 0.0 point is the floor: the ETI entity shifting and evasion produce with
    no conversion at all, which is what makes sigma a residual rather than an
    independent estimate. Measured by: {MEASURE_SCRIPT}
"""


def build_calibration(tag, value, charity):
    preamble, blocks = split_blocks(read(SOURCE_CALIB))
    if 'conv' not in [k for k, _ in blocks]:
        sys.exit(SOURCE_CALIB + ' has no conv entry to replace.')

    out = [sweep_header(tag, value, charity)]
    for key, block in blocks:
        out.append(trial_block(value) if key == 'conv' else block)
    return '\n'.join(out)


def build_behavior(tag, value, charity):
    sweep_path = f'{SWEEP_ROOT}/conv_{tag}/conversion.yaml'
    modules = '\n'.join(f'  - {m.format(charity=charity)}' for m in MODULES)
    return f"""# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.
#
# The sigma CALIBRATION stack at conv = {value}. Not a product stack: it exists to
# reproduce the conditions sigma is derived under, which is why the charity
# elasticity is pinned explicitly at charity/{charity} rather than inherited.
#
# charity/{charity} is load-bearing. The shipped sigma of 0.16 was derived under
# charity/100, while product runs use charity/50, and that mismatch is recorded as a
# dated waiver on the conversion.conv entry. Whichever this stack names is the basis
# of whatever the measurement writes, so it is stated here rather than assumed.
#
# Named by: {RUNSCRIPT_ROOT}/sigma_calib_{charity}_conv_{tag}.csv
# Measured by: {MEASURE_SCRIPT}
#
# Order of execution is NOT the order below: src/sim/behavior.R sorts the list
# against one pinned family order, because later families read what earlier ones
# wrote.

kg_dynamics:
  bathtub: config/calibrations/kg/bathtub.yaml
  conversion: {sweep_path}

modules:
{modules}
"""


def build_runscript(tag, value, charity):
    mtr_types = ' '.join(['nextdollar'] * len(MTR_VARS.split()))
    alt = f'sigma_calib_{charity}_conv_{tag}'
    header = 'ID,tax_law,economy,behavior,years,dist_years,mtr_vars,mtr_types'
    rows = [
        # The saving channel is off on both legs. sigma is measured on ordinary
        # income and the wealth bathtub would add a second moving part for nothing.
        f'baseline,default,no_saving_channel,,{YEARS},{DIST_YEARS},{MTR_VARS},{mtr_types}',
        f'{SHOCK_ID},{SHOCK_TAXLAW},no_saving_channel,{alt},{YEARS},{DIST_YEARS},'
        f'{MTR_VARS},{mtr_types}',
    ]
    return '\n'.join([header] + rows) + '\n'


def main():
    charity = '100'
    args = sys.argv[1:]
    if '--charity' in args:
        charity = args[args.index('--charity') + 1]
    if charity not in ('50', '100'):
        sys.exit(f'--charity must be 50 or 100, got {charity!r}')
    if not os.path.exists(os.path.join(REPO, f'src/behavior/charity/{charity}.R')):
        sys.exit(f'no such module: src/behavior/charity/{charity}.R')

    written = []
    for tag, value in GRID:
        written.append(write(f'{SWEEP_ROOT}/conv_{tag}/conversion.yaml',
                             build_calibration(tag, value, charity)))
        written.append(write(
            f'{BEHAVIOR_ROOT}/sigma_calib_{charity}_conv_{tag}/behavior.yaml',
            build_behavior(tag, value, charity)))
        written.append(write(
            f'{RUNSCRIPT_ROOT}/sigma_calib_{charity}_conv_{tag}.csv',
            build_runscript(tag, value, charity)))

    for path in written:
        print('wrote', os.path.relpath(path, REPO))
    print(f'\n{len(GRID)} grid points at charity/{charity}. Launch with:\n'
          f'  sbatch {LAUNCHER}.sbatch {charity}\n'
          f'then measure and write the value with:\n'
          f'  sbatch {MEASURE_SCRIPT.replace(".R", ".sbatch")} {charity}')
    if charity == '50':
        print('\nNOTE: charity/50 is the DEFERRED RE-DERIVATION, not a\n'
              'reproduction. It is expected to move sigma away from 0.16 and to\n'
              'clear the dated waiver on the conversion.conv entry.')


if __name__ == '__main__':
    main()
