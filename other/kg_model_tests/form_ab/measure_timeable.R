#-------------------------------------------------------------------------------
# measure_timeable.R  --  pin the short-run retiming share, and write it.
#
# Restores the calibration path for timeable_share / timeable_share_logs, which has
# had none since 2026-07-12. On that date other/kg_model_tests/calibrate.R -- the
# standalone Bellman miniature that used to solve for eta and the share together --
# was demoted to a drift diagnostic when eta moved onto the full simulator. eta got
# a replacement; the share did not, and re-deriving it has meant running the
# short-run leg by hand and iterating, which is how it came to be the last
# hand-typed number in the model.
#
# THE MOMENT. On a DELAYED shock -- the gains rate is announced in 2026 and rises in
# 2027 -- nothing about 2026's tax law has changed, so any movement in 2026
# realizations is pure retiming: people selling early to beat the increase. That
# isolates the overlay from the elasticity.
#
#   E_full_short = log(R_delayed(2026) / R_base(2026)) / dtau(2027)   target +5.04
#   E_full_long  = log(R_rate2pp(2055) / R_base(2055)) / dtau(2055)   target -2.52
#     R    = sum(w * pmax(kg_lt, 0))
#     dtau = realization-weighted mean mtr_kg_lt, shock - base
#
# The long-run figure is a SANITY CHECK, not a target here: the overlay nets to zero
# under a uniform permanent shock, so the long-run moment is share-invariant by
# construction and should barely move across the grid. If it moves materially, the
# invariance the sequential identification depends on is not holding and the share
# cannot be pinned independently of eta. This script reports that spread and warns.
#
# WHY THIS REFUSES TO INTERPOLATE BLINDLY. The 2026-07-12 note demoting the solver
# says the bathtub dilution is UNSTABLE in the share, which is why it was iterated by
# hand rather than solved. So a monotone curve is not assumed: the grid is checked,
# and a non-monotone one stops with the measured points printed rather than returning
# a number interpolated through a fold. That check is the difference between this and
# the auto-loop that was abandoned.
#
# Usage (through the sbatch wrapper -- it reads whole detail files):
#   sbatch other/kg_model_tests/form_ab/measure_timeable.sbatch [logs|levels]
#
# Defaults to logs, the live form. For levels, response_form must have been `levels`
# in config/calibrations/kg/settings.yaml when the sweep was RUN; the launcher
# enforces that, and this script only reads what the launcher produced.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

source('src/misc/calibration_writer.R')

args = commandArgs(trailingOnly = TRUE)
FORM = if (length(args) >= 1) args[1] else 'logs'
stopifnot(FORM %in% c('logs', 'levels'))

ENTRY = if (FORM == 'logs') 'timeable_share_logs' else 'timeable_share'
STEM  = if (FORM == 'logs') 'form_timeable_logs'   else 'form_timeable_levels'

LOCAL_ROOT = Sys.getenv('KG_CALIB_OUTPUT_ROOT',
                        '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1')
VINTAGE_SUFFIX = Sys.getenv('VINTAGE_SUFFIX', '')

CALIB_FILE = 'config/calibrations/kg/bathtub.yaml'

TARGET_SHORT    = 5.04
TARGET_LONG     = -2.52
SHORT_ANCHOR    = 2026
SHORT_DTAU_YEAR = 2027
LONG_ANCHOR     = 2055

# The grid, which must agree with write_bathtub_sweep.py's FORMS[...]$grid.
runs = tribble(
  ~share,  ~tag,
  0.15,    '15',
  0.2542,  '25',
  0.40,    '40'
) %>%
  mutate(vintage = paste0(STEM, '_', tag, VINTAGE_SUFFIX))

agg = function(scn, pass, yr, vintage) {
  f = file.path(LOCAL_ROOT, vintage, scn, pass, 'detail', paste0(yr, '.csv'))
  if (!file.exists(f)) stop('missing detail: ', f)
  fread(f, select = c('weight', 'kg_lt', 'mtr_kg_lt'), showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    summarise(R      = sum(weight * kg_pos, na.rm = TRUE),
              tau_rw = sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                       sum(weight * kg_pos,             na.rm = TRUE)) %>%
    as.list()
}

missing = runs$vintage[!dir.exists(file.path(LOCAL_ROOT, runs$vintage))]
if (length(missing) > 0) {
  stop('these sweep vintages are not on disk:\n  ', paste(missing, collapse = '\n  '),
       '\nGenerate the grid and launch it first:\n',
       '  python3 other/kg_model_tests/form_ab/write_bathtub_sweep.py ',
       if (FORM == 'logs') 'timeable_logs' else 'timeable', '\n',
       '  sbatch other/kg_model_tests/form_ab/launch_timeable.sbatch ', FORM)
}

measured = runs %>%
  pmap_dfr(function(share, tag, vintage) {
    bS  = agg('baseline', 'static',       SHORT_ANCHOR,    vintage)
    sS  = agg('delayed',  'conventional', SHORT_ANCHOR,    vintage)
    bS2 = agg('baseline', 'static',       SHORT_DTAU_YEAR, vintage)
    sS2 = agg('delayed',  'conventional', SHORT_DTAU_YEAR, vintage)
    bL  = agg('baseline',    'static',       LONG_ANCHOR, vintage)
    sL  = agg('rate_up_2pp', 'conventional', LONG_ANCHOR, vintage)

    dtau_short = sS2$tau_rw - bS2$tau_rw
    dtau_long  = sL$tau_rw  - bL$tau_rw

    tibble(share        = share,
           vintage      = vintage,
           R_base_B     = bS$R / 1e9,
           R_delayed_B  = sS$R / 1e9,
           dtau_short   = dtau_short,
           E_full_short = log(sS$R / bS$R) / dtau_short,
           E_full_long  = log(sL$R / bL$R) / dtau_long)
  })

out = sprintf('other/kg_model_tests/form_ab/timeable_%s.csv', FORM)
write_csv(measured, out)
print(as.data.frame(measured))
cat(sprintf('\nwrote %s\n', out))

o = measured[order(measured$share), ]

#-------------------------------------------------------------------------------
# The share-invariance sanity check on the long-run moment.
#-------------------------------------------------------------------------------
long_spread = max(o$E_full_long) - min(o$E_full_long)
long_rel    = long_spread / abs(mean(o$E_full_long))

cat('\n--- long-run moment across the grid (should be flat) -------------\n')
cat(sprintf('E_full_long  : %s\n', paste(sprintf('%+.4f', o$E_full_long), collapse = ', ')))
cat(sprintf('spread       : %.4f (%.2f%% of mean); target %+.2f\n',
            long_spread, 100 * long_rel, TARGET_LONG))
if (long_rel > 0.05) {
  cat('WARNING: the long-run moment moves more than 5% across the grid. The\n')
  cat('overlay is supposed to net to zero under a uniform permanent shock, so\n')
  cat('share-invariance is what licenses pinning the share separately from eta.\n')
  cat('If this is real, the two are NOT sequentially identified and the pin\n')
  cat('below is conditional on an eta that would itself need re-deriving.\n')
} else {
  cat('OK: share-invariant to within 5%, so the sequential pin holds.\n')
}
cat('------------------------------------------------------------------\n')

#-------------------------------------------------------------------------------
# Monotonicity, then interpolate.
#-------------------------------------------------------------------------------
d = diff(o$E_full_short)
monotone = all(d > 0) || all(d < 0)

cat('\n--- short-run moment ---------------------------------------------\n')
cat(sprintf('grid share   : %s\n', paste(o$share, collapse = ', ')))
cat(sprintf('E_full_short : %s\n', paste(sprintf('%+.4f', o$E_full_short), collapse = ', ')))
cat(sprintf('target       : %+.2f\n', TARGET_SHORT))
cat(sprintf('monotone     : %s\n', if (monotone) 'yes' else 'NO'))
cat('------------------------------------------------------------------\n')

if (!monotone) {
  stop('The short-run response is NOT monotone in the share across this grid.\n',
       'That is the instability the 2026-07-12 note recorded when it demoted the\n',
       'solver, and it means there is no single share that hits the target -- an\n',
       'interpolation here would be reading a number off a fold. What to do:\n',
       '  - narrow the grid around the shipped 0.2542 and re-run the sweep, or\n',
       '  - if the non-monotonicity survives a narrower grid, the overlay does not\n',
       '    identify this moment and that is a finding for the author, not a\n',
       '    calibration to be forced.\n',
       'Measured points are in ', out, '.')
}

share_star = NA_real_
for (i in 1:(nrow(o) - 1)) {
  lo = o$E_full_short[i]; hi = o$E_full_short[i + 1]
  if ((lo - TARGET_SHORT) * (hi - TARGET_SHORT) <= 0) {
    w = (TARGET_SHORT - lo) / (hi - lo)
    share_star = o$share[i] + w * (o$share[i + 1] - o$share[i])
    break
  }
}

if (is.na(share_star)) {
  stop('The grid does not bracket the target short-run moment (', TARGET_SHORT,
       '). Measured range: ', sprintf('%+.4f to %+.4f', min(o$E_full_short),
                                      max(o$E_full_short)),
       '.\nWiden the grid in other/kg_model_tests/form_ab/write_bathtub_sweep.py ',
       'and re-run the sweep.')
}

cat(sprintf('share* (piecewise linear) : %.4f\n', share_star))

#-------------------------------------------------------------------------------
# Write it. Reproduce-in-place / drift-to-.proposed, the same contract every other
# calibrator here follows.
#-------------------------------------------------------------------------------

calib_write_entry(
  path   = CALIB_FILE,
  entry  = ENTRY,
  value  = round(share_star, 4),
  fields = list(
    kind = 'calibrated',
    set  = format(Sys.Date()),
    target = calib_prose(sprintf(
      'Short-run announcement moment: full-sim short-run semi-elasticity of %+.2f
       (twice the long-run magnitude with the sign flipped -- future-tax-up implies
       realize-today), measured at the announcement year %d under a +2pp DELAYED
       permanent shock whose rate rises in %d. Because %d law is unchanged, movement
       in that year is pure retiming, which is what separates the overlay from the
       elasticity. Identified GIVEN eta: the long-run moment is share-invariant
       (the overlay nets to zero under a uniform permanent shock), and that
       invariance was checked across this grid rather than assumed -- long-run
       E_full came out %s. Grid share %s gave short-run E_full %s; the pin is the
       piecewise-linear interpolation onto the target, and the grid was verified
       monotone before inverting.',
      TARGET_SHORT, SHORT_ANCHOR, SHORT_DTAU_YEAR, SHORT_ANCHOR,
      paste(sprintf('%+.4f', o$E_full_long), collapse = ' '),
      paste(o$share, collapse = ' '),
      paste(sprintf('%+.4f', o$E_full_short), collapse = ' '))),
    derived_under = list(tax_data          = '2026070814',
                         macro_projections = '2026022522'),
    invalidated_by = c('src/sim/kg/constants.R',
                       'src/sim/kg/timing.R',
                       'src/sim/kg/recurrence.R'),
    conditioned_on = list(settings.kg.timing_window      = 1,
                          settings.kg.timing_ref_wedge   = 0.05,
                          settings.kg.applier_allocation = '0.5'),
    rederive    = 'other/kg_model_tests/form_ab/measure_timeable.R',
    active_when = list(kg.response_form = FORM),
    note = calib_prose(sprintf(
      "%s
       This entry had NO working re-derivation path between 2026-07-12 and
       2026-07-26. other/kg_model_tests/calibrate.R used to solve for eta and this
       share together in a standalone Bellman miniature; when eta moved onto the
       full simulator that script was demoted to a drift diagnostic, and the share
       was left to be iterated by hand. measure_timeable.R replaces the hand step,
       and unlike the abandoned auto-loop it CHECKS the grid is monotone before
       inverting -- the instability that got the old solver demoted would otherwise
       be interpolated straight through.
       Measured from vintages: %s. Per-point measurements:
       other/kg_model_tests/form_ab/timeable_%s.csv.",
      if (FORM == 'logs') "LIVE by default (response_form = 'logs')."
      else "Live only when response_form = 'levels', which is not the shipped default.",
      paste(measured$vintage, collapse = ' '), FORM))))
