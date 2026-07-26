#!/usr/bin/env python3
"""Write the eta_tilde sweep grid: calibration files, behavior alternatives, runscripts.

WHY THIS SCRIPT EXISTS. Pinning eta_tilde for the net-of-tax response form means
running the same +5pp capital-gains shock at three trial values of the parameter
and inverting the measured E_full curve. That used to be done by exporting
KG_ETA_LOGS into the submitting shell, which left no trace of the trial value
anywhere in the vintage it produced -- and, once the environment-variable back
doors were retired, silently stopped working at all. The launcher went unrunnable
for weeks and nobody found out, because nobody recalibrated in the meantime.

A trial value is now a file, bound by its own behavior alternative, named by its
own runscript. So the vintage records which eta_tilde produced it, and the sweep
is reproducible from the repository rather than from a shell history.

Three artifacts per grid point, all generated, none hand-edited:

  config/calibrations/kg/sweeps/eta_logs_<tag>/bathtub.yaml
      the shipped bathtub calibration with ONE entry replaced -- eta_logs becomes
      the trial value. Everything else, including the other three entries and
      their provenance, is copied through verbatim, because a sweep varies one
      thing. NOTE the file keeps the base name `bathtub.yaml`: entries are
      labelled '{file stem}.{entry}', so renaming it to eta_logs_15.yaml would
      relabel every entry and quietly detach any waiver written against
      `bathtub.eta_logs`.

  config/scenarios/behavior/alternatives/top_tax_full_eta_logs_<tag>/behavior.yaml
      the top_tax_full stack with the bathtub piece bound to that sweep file.

  config/runscripts/top_tax/eta_dial_logs_<tag>.csv
      eta_dial_repin.csv with the shock row's behavior cell naming that
      alternative. One runscript per grid point rather than one runscript with
      three rows, because measure_efull_logs.R reads three separate VINTAGES
      whose shock scenario is called `s_cg_r25`, and the vintages already on
      scratch have that shape.

Usage (from repo root):  python3 other/kg_model_tests/form_ab/write_eta_logs_sweep.py

Regenerate-and-diff is the check: an empty `git status --short` after running it
means the tree matches what the script would write.
"""

import os
import re
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))

SOURCE_CALIB   = 'config/calibrations/kg/bathtub.yaml'
SWEEP_ROOT     = 'config/calibrations/kg/sweeps'
BEHAVIOR_ROOT  = 'config/scenarios/behavior/alternatives'
BASE_BEHAVIOR  = 'top_tax_full'
RUNSCRIPT_ROOT = 'config/runscripts/top_tax'
BASE_RUNSCRIPT = 'eta_dial_repin.csv'

THIS_SCRIPT = 'other/kg_model_tests/form_ab/write_eta_logs_sweep.py'

# The trial grid, straddling the expected eta_tilde* of about 1.9 -- the
# net-of-tax elasticity matched to the same local moment (E_full = -0.6/0.238 =
# -2.52) the levels form was pinned on. The tag is the value with its decimal
# point dropped, which is what the vintage names on scratch already use.
GRID = [('15', '1.5'), ('19', '1.9'), ('23', '2.3')]

# The measurement script that consumes the grid, named in the generated files so
# a reader of one lands on the other.
MEASURE_SCRIPT = 'other/kg_model_tests/form_ab/measure_efull_logs.R'


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


def sweep_header(tag, value):
    return f"""# GENERATED FILE -- do not hand-edit. Written by {THIS_SCRIPT}.
#
# SWEEP POINT: eta_logs = {value}. One point on the trial grid used to pin
# eta_tilde for the net-of-tax response form. This is not a shipped calibration
# and no scenario should bind it except the eta-dial runscript generated
# alongside it.
#
# Everything here is the shipped config/calibrations/kg/bathtub.yaml with exactly
# one entry replaced -- eta_logs -- because a sweep varies one thing and holds the
# rest fixed. The other three entries keep their own provenance verbatim, so this
# file goes stale under the same conditions the shipped one does.
#
# Bound by  : config/scenarios/behavior/alternatives/top_tax_full_eta_logs_{tag}/
# Measured by: {MEASURE_SCRIPT}
#
# The file name matters. It has to stay `bathtub.yaml` inside a differently-named
# FOLDER, because entries are labelled '{{file stem}}.{{entry}}' -- calling it
# eta_logs_{tag}.yaml would relabel every entry in it and detach any waiver
# written against `bathtub.eta_logs`.
"""


def trial_block(value):
    return f"""eta_logs:
  value: {value}
  kind: judgment
  active_when:
    kg.response_form: logs
  note: >
    SWEEP TRIAL, not a calibrated value -- which is why the kind is judgment
    rather than calibrated. It has no derivation to go stale: it is an INPUT to
    the calibration that pins eta_logs, one of three points on the grid
    {{1.5, 1.9, 2.3}}. The measurement script reads the three vintages this
    grid produces, measures E_full at each, and inverts for the eta_tilde that
    hits E_full = -2.52. The result of that inversion is the shipped value in
    config/calibrations/kg/bathtub.yaml; this number is scaffolding for
    producing it. Measured by: {MEASURE_SCRIPT}
"""


def build_calibration(tag, value):
    preamble, blocks = split_blocks(read(SOURCE_CALIB))
    keys = [k for k, _ in blocks]
    if 'eta_logs' not in keys:
        sys.exit(SOURCE_CALIB + ' has no eta_logs entry to replace.')

    out = [sweep_header(tag, value)]
    for key, block in blocks:
        out.append(trial_block(value) if key == 'eta_logs' else block)
    return '\n'.join(out)


def build_behavior(tag, value):
    base = read(os.path.join(BEHAVIOR_ROOT, BASE_BEHAVIOR, 'behavior.yaml'))
    sweep_path = f'{SWEEP_ROOT}/eta_logs_{tag}/bathtub.yaml'

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
# The {BASE_BEHAVIOR} stack with the bathtub bound to the eta_logs = {value} sweep
# point instead of to the shipped calibration. Everything else about the stack is
# the product configuration, because the eta-dial measurement only means something
# if the trial value is the one thing that differs.
#
# Named by: {RUNSCRIPT_ROOT}/eta_dial_logs_{tag}.csv
#
"""
    return header + body


def build_runscript(tag, value):
    """eta_dial_repin.csv with the behavior cell swapped, and the OME pin dropped.

    The base runscript pins Off-Model-Estimates 20250925, inherited from the
    top-tax exercise it was written alongside. That pin is now unreachable and the
    reason has nothing to do with the eta dial: the OME interface went to v5 on
    2026-07-22 (the two-stream corporate revenue change), 20250925 exists only
    under v4, and a v4 vintage hard-stops in post-processing anyway because it has
    no `corporate_static` column. So eta_dial_repin.csv itself has not been
    runnable since that date -- discovered by running it, since resolving a
    runscript does not check that the vintage exists.

    The eta dial does not care. Off-Model-Estimates is read in three places --
    receipts, the distribution smear, and the corporate incidence channel -- and
    none of them touches the per-record detail files E_full is measured from. The
    corporate channel does not even activate here: both rows name the same OME ID,
    so the corporate wedge is identically zero. Dropping the pin to the model
    default leaves the measurement unchanged and makes the runscript run.
    """
    base = read(os.path.join(RUNSCRIPT_ROOT, BASE_RUNSCRIPT))
    lines = base.rstrip('\n').split('\n')
    header = lines[0].split(',')
    behavior_col = header.index('behavior')
    economy_col = header.index('economy')

    out = [lines[0]]
    rewritten = 0
    for line in lines[1:]:
        cells = line.split(',')
        if cells[behavior_col] == BASE_BEHAVIOR:
            cells[behavior_col] = f'{BASE_BEHAVIOR}_eta_logs_{tag}'
            rewritten += 1
        cells[economy_col] = 'default'
        out.append(','.join(cells))

    if rewritten != 1:
        sys.exit(f'{BASE_RUNSCRIPT}: expected exactly one row naming '
                 f'{BASE_BEHAVIOR}, found {rewritten}.')
    return '\n'.join(out) + '\n'


def main():
    written = []
    for tag, value in GRID:
        written.append(write(f'{SWEEP_ROOT}/eta_logs_{tag}/bathtub.yaml',
                             build_calibration(tag, value)))
        written.append(write(
            f'{BEHAVIOR_ROOT}/{BASE_BEHAVIOR}_eta_logs_{tag}/behavior.yaml',
            build_behavior(tag, value)))
        written.append(write(f'{RUNSCRIPT_ROOT}/eta_dial_logs_{tag}.csv',
                             build_runscript(tag, value)))

    for path in written:
        print('wrote', os.path.relpath(path, REPO))
    print(f'\n{len(GRID)} grid points. Launch with:\n'
          '  bash other/kg_model_tests/form_ab/launch_eta_dial_logs.sh')


if __name__ == '__main__':
    main()
