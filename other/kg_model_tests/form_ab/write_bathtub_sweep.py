#!/usr/bin/env python3
"""Write a bathtub-calibration sweep grid: calibration files, behavior alternatives, runscripts.

Serves all four calibrated entries of config/calibrations/kg/bathtub.yaml. Two are
response elasticities and two are the short-run retiming overlay, one of each per
response form:

  logs           eta_logs = 1.6625             LIVE by default
  levels         eta = 2.4825                  dormant
  timeable_logs  timeable_share_logs = 0.2542  LIVE by default
  timeable       timeable_share = 0.2542       dormant

Pinning an elasticity means running the same +5pp capital-gains shock at three
trial values and inverting the measured long-run E_full curve. Pinning a timeable
share means the same thing against the SHORT-RUN announcement moment, on a
DELAYED shock -- the rate rises a year after it is announced, so the only thing
moving realizations in the announcement year is retiming.

Both used to be swept by exporting an environment variable into the submitting
shell, which left no trace of the trial value in the vintage it produced and, once
those back doors were retired, silently stopped working. A trial value is now a
file, bound by its own behavior alternative, named by its own runscript, so the
vintage records what produced it.

Three artifacts per grid point, all generated, none hand-edited:

  config/calibrations/kg/sweeps/<entry>_<tag>/bathtub.yaml
      the shipped bathtub calibration with ONE entry replaced. Everything else,
      including the other three entries and their provenance, is copied through
      verbatim, because a sweep varies one thing. NOTE the file keeps the base
      name `bathtub.yaml`: entries are labelled '{file stem}.{entry}', so renaming
      it would relabel every entry and quietly detach any waiver written against
      it.

  config/scenarios/behavior/alternatives/top_tax_full_<entry>_<tag>/behavior.yaml
      the top_tax_full stack with the bathtub piece bound to that sweep file.

  config/runscripts/top_tax/<runscript_stem>_<tag>.csv
      the dial runscript with the shock row's behavior cell naming that
      alternative. One runscript per grid point rather than one with three rows,
      because the measurement script reads three separate VINTAGES whose shock
      scenario is called `s_cg_r25`, and the vintages already on scratch have that
      shape.

ONE MANUAL STEP FOR THE LEVELS FORM, and it is not an oversight. `response_form`
is a FIXED setting in config/calibrations/kg/settings.yaml -- one value for every
scenario in a run -- because each form has its own calibrated elasticity and a run
mixing them would pair one of them with the wrong number. So a levels sweep needs
that setting flipped to `levels` before launching and back afterwards. The levels
launcher says so and refuses to run until it is.

Usage (from repo root):
    python3 other/kg_model_tests/form_ab/write_bathtub_sweep.py                  # all four
    python3 other/kg_model_tests/form_ab/write_bathtub_sweep.py logs             # one
    python3 other/kg_model_tests/form_ab/write_bathtub_sweep.py timeable_logs

Regenerate-and-diff is the check: an empty `git status --short` after running it
means the tree matches what the script would write.
"""

import os
import re
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))

SOURCE_CALIB = 'config/calibrations/kg/bathtub.yaml'
SWEEP_ROOT = 'config/calibrations/kg/sweeps'
BEHAVIOR_ROOT = 'config/scenarios/behavior/alternatives'
BASE_BEHAVIOR = 'top_tax_full'
# Default runscript home; a form may override it with runscript_root.
RUNSCRIPT_ROOT = 'config/runscripts/top_tax'

THIS_SCRIPT = 'other/kg_model_tests/form_ab/write_bathtub_sweep.py'

# The two forms. `grid` tags are the trial value with its decimal point dropped
# (logs) or the vintage shorthand already on scratch (levels) -- in both cases the
# tag is what the vintage names use, so a sweep file and the run it produced are
# findable from each other.
FORMS = {
    'logs': dict(
        entry='eta_logs',
        form_value='logs',
        # Straddles the expected eta_tilde* of about 1.9 -- the net-of-tax
        # elasticity matched to the same local moment (E_full = -0.6/0.238) the
        # levels form was pinned on.
        grid=[('15', '1.5'), ('19', '1.9'), ('23', '2.3')],
        runscript_stem='eta_dial_logs',
        base_runscript='eta_dial_repin.csv',
        measure_script='other/kg_model_tests/form_ab/measure_efull_logs.R',
        target_desc='eta_tilde for the net-of-tax response form',
        expect_rows=1,
    ),
    'levels': dict(
        entry='eta',
        form_value='levels',
        # Straddles the shipped 2.4825. These are the three points the 2026-07-12
        # re-pin actually ran, and the tags match the vintages it wrote
        # (eta_dial_e20_v2 / eta_dial_c_v2 / eta_dial_e30_v2), so a re-run is
        # comparable against them point for point.
        grid=[('e20', '2.0'), ('c', '2.3992'), ('e30', '3.0')],
        runscript_stem='eta_dial_levels',
        base_runscript='eta_dial_repin.csv',
        measure_script='other/top_tax/eta_dial/measure_efull_by_eta.R',
        target_desc='eta for the semi-elasticity response form',
        expect_rows=1,
    ),
    # The two timeable shares. Their moment is the ANNOUNCEMENT-year jump on a
    # delayed shock, so they share one runscript (tests/form_timeable: baseline, a
    # delayed leg, and a permanent leg that sanity-checks the long-run moment has
    # not moved -- it should not, the overlay nets to zero under a uniform
    # permanent shock).
    #
    # The grid straddles the shipped 0.2542 widely, because this is the one kg
    # parameter whose solver was DEMOTED for instability: the 2026-07-12 note says
    # the bathtub dilution is unstable in the share, which is why it was iterated by
    # hand instead of solved. A wide grid is what makes non-monotonicity visible
    # rather than silently interpolated through, and the measurement script refuses
    # to interpolate if the curve is not monotone.
    'timeable_logs': dict(
        entry='timeable_share_logs',
        form_value='logs',
        grid=[('15', '0.15'), ('25', '0.2542'), ('40', '0.40')],
        runscript_stem='form_timeable_logs',
        runscript_root='config/runscripts/tests',
        base_runscript='form_timeable.csv',
        measure_script='other/kg_model_tests/form_ab/measure_timeable.R',
        target_desc='the short-run retiming share for the net-of-tax response form',
        expect_rows=2,
    ),
    'timeable': dict(
        entry='timeable_share',
        form_value='levels',
        grid=[('15', '0.15'), ('25', '0.2542'), ('40', '0.40')],
        runscript_stem='form_timeable_levels',
        runscript_root='config/runscripts/tests',
        base_runscript='form_timeable.csv',
        measure_script='other/kg_model_tests/form_ab/measure_timeable.R',
        target_desc='the short-run retiming share for the semi-elasticity response form',
        expect_rows=2,
    ),
}


def rs_root(spec):
    return spec.get('runscript_root', RUNSCRIPT_ROOT)


def read(path):
    with open(os.path.join(REPO, path)) as f:
        return f.read()


def write(path, text):
    full = os.path.join(REPO, path)
    os.makedirs(os.path.dirname(full), exist_ok=True)
    with open(full, 'w', newline='\n') as f:
        f.write(text)
    return full


def split_blocks(text):
    """Top-level YAML blocks of a calibration file, in order.

    Returns (preamble, [(key, block_text), ...]). A block runs from its key line
    to the line before the next key at column zero, so its comments and blank
    lines travel with it. Deliberately textual: the point of these files is that
    the comments ARE the provenance, and a YAML round-trip deletes them.
    """
    lines = text.split('\n')
    starts = [i for i, ln in enumerate(lines) if re.match(r'^[A-Za-z_][A-Za-z0-9_]*:', ln)]
    if not starts:
        sys.exit('No top-level entries found in ' + SOURCE_CALIB)

    preamble = '\n'.join(lines[:starts[0]])
    blocks = []
    for n, start in enumerate(starts):
        end = starts[n + 1] if n + 1 < len(starts) else len(lines)
        key = lines[start].split(':')[0]
        blocks.append((key, '\n'.join(lines[start:end])))
    return preamble, blocks


def sweep_header(spec, tag, value):
    entry = spec['entry']
    return f"""# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.
#
# SWEEP POINT: {entry} = {value}. One point on the trial grid used to pin
# {spec['target_desc']}.
# This is not a shipped calibration and no scenario should bind it except the
# eta-dial runscript generated alongside it.
#
# Everything here is the shipped config/calibrations/kg/bathtub.yaml with exactly
# one entry replaced -- {entry} -- because a sweep varies one thing and holds the
# rest fixed. The other three entries keep their own provenance verbatim, so this
# file goes stale under the same conditions the shipped one does.
#
# Bound by  : config/scenarios/behavior/alternatives/{BASE_BEHAVIOR}_{entry}_{tag}/
# Measured by: {spec['measure_script']}
#
# The file name matters. It has to stay `bathtub.yaml` inside a differently-named
# FOLDER, because entries are labelled '{{file stem}}.{{entry}}' -- calling it
# {entry}_{tag}.yaml would relabel every entry in it and detach any waiver
# written against `bathtub.{entry}`.
"""


def trial_block(spec, value):
    entry = spec['entry']
    grid_set = ', '.join(v for _, v in spec['grid'])
    return f"""{entry}:
  value: {value}
  kind: judgment
  active_when:
    kg.response_form: {spec['form_value']}
  note: >
    SWEEP TRIAL, not a calibrated value -- which is why the kind is judgment
    rather than calibrated. It has no derivation to go stale: it is an INPUT to
    the calibration that pins {entry}, one of three points on the grid
    {{{grid_set}}}. The measurement script reads the three vintages this
    grid produces, measures E_full at each, and inverts for the value that
    hits E_full = -2.52. The result of that inversion is the shipped value in
    config/calibrations/kg/bathtub.yaml; this number is scaffolding for
    producing it. Measured by: {spec['measure_script']}
"""


def build_calibration(spec, tag, value):
    preamble, blocks = split_blocks(read(SOURCE_CALIB))
    keys = [k for k, _ in blocks]
    if spec['entry'] not in keys:
        sys.exit(f"{SOURCE_CALIB} has no {spec['entry']} entry to replace.")

    out = [sweep_header(spec, tag, value)]
    for key, block in blocks:
        out.append(trial_block(spec, value) if key == spec['entry'] else block)
    return '\n'.join(out)


def build_behavior(spec, tag, value):
    entry = spec['entry']
    base = read(os.path.join(BEHAVIOR_ROOT, BASE_BEHAVIOR, 'behavior.yaml'))
    sweep_path = f'{SWEEP_ROOT}/{entry}_{tag}/bathtub.yaml'

    marker = 'kg_dynamics:\n  bathtub: config/calibrations/kg/bathtub.yaml'
    if marker not in base:
        sys.exit(BASE_BEHAVIOR + "'s behavior.yaml no longer binds the bathtub "
                 'in the shape this script rewrites. Check it by hand.')
    body = base.replace(marker, f'kg_dynamics:\n  bathtub: {sweep_path}')

    # The inherited file names itself in its first line; relabel it so the copy
    # does not claim to be the alternative it was copied from.
    body = body.replace(f'# Behavior leg alternative -- {BASE_BEHAVIOR}\n',
                        f'# The stack below is inherited from the {BASE_BEHAVIOR} '
                        'alternative, comments and all:\n', 1)

    header = f"""# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.
#
# The {BASE_BEHAVIOR} stack with the bathtub bound to the {entry} = {value} sweep
# point instead of to the shipped calibration. Everything else about the stack is
# the product configuration, because the eta-dial measurement only means something
# if the trial value is the one thing that differs.
#
# Named by: {rs_root(spec)}/{spec['runscript_stem']}_{tag}.csv
#
"""
    return header + body


def build_runscript(spec, tag, value):
    """The base dial runscript with the behavior cell swapped.

    The economy cell is forced to `default`. The base runscript used to pin
    Off-Model-Estimates 20250925, inherited from the top-tax exercise it was
    written alongside, and that pin became unreachable when the interface went to
    v5 on 2026-07-22. The eta dial does not care either way: Off-Model-Estimates is
    read in receipts, the distribution smear and the corporate incidence channel,
    none of which touches the per-record detail files E_full is measured from.
    """
    base = read(os.path.join(rs_root(spec), spec['base_runscript']))
    lines = base.rstrip('\n').split('\n')
    header = lines[0].split(',')
    behavior_col = header.index('behavior')
    # `economy` is optional: a runscript without it resolves that leg to default,
    # which is what these sweeps want anyway.
    economy_col = header.index('economy') if 'economy' in header else None

    out = [lines[0]]
    rewritten = 0
    for line in lines[1:]:
        cells = line.split(',')
        if cells[behavior_col] == BASE_BEHAVIOR:
            cells[behavior_col] = f"{BASE_BEHAVIOR}_{spec['entry']}_{tag}"
            rewritten += 1
        if economy_col is not None:
            cells[economy_col] = 'default'
        out.append(','.join(cells))

    # Every shock row naming the base stack is rewritten -- the timeable runscript
    # has two of them (the delayed leg and the permanent one that sanity-checks the
    # long-run moment is still on target), the eta ones have one.
    if rewritten != spec['expect_rows']:
        sys.exit(f"{spec['base_runscript']}: expected {spec['expect_rows']} row(s) "
                 f'naming {BASE_BEHAVIOR}, found {rewritten}.')
    return '\n'.join(out) + '\n'


def main():
    which = sys.argv[1:] or list(FORMS)
    for name in which:
        if name not in FORMS:
            sys.exit(f'unknown form {name!r}; choose from {", ".join(FORMS)}')

    for name in which:
        spec = FORMS[name]
        written = []
        for tag, value in spec['grid']:
            written.append(write(f"{SWEEP_ROOT}/{spec['entry']}_{tag}/bathtub.yaml",
                                 build_calibration(spec, tag, value)))
            written.append(write(
                f"{BEHAVIOR_ROOT}/{BASE_BEHAVIOR}_{spec['entry']}_{tag}/behavior.yaml",
                build_behavior(spec, tag, value)))
            written.append(write(
                f"{rs_root(spec)}/{spec['runscript_stem']}_{tag}.csv",
                build_runscript(spec, tag, value)))

        print(f'--- {name} form ({spec["entry"]}) ---')
        for path in written:
            print('  wrote', os.path.relpath(path, REPO))

    if any(FORMS[n]['form_value'] == 'levels' for n in which):
        print('\nLEVELS SWEEP NEEDS ONE MANUAL STEP: set response_form to '
              '`levels` in\nconfig/calibrations/kg/settings.yaml before '
              'launching, and back to `logs`\nafterwards. It is a fixed setting '
              'on purpose -- each form has its own\ncalibrated elasticity, and a '
              'run mixing them would pair one with the wrong\nnumber. The '
              'launcher checks it and refuses otherwise.')


if __name__ == '__main__':
    main()
