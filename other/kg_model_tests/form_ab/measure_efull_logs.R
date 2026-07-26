#-------------------------------------------------------------------------------
# measure_efull_logs.R  --  Part B1: pin eta_tilde for the net-of-tax form.
#
# Full-sim long-run realization semi-elasticity E_full at each trial eta_tilde,
# on the SAME convention as the levels re-pin (measure_efull_by_eta.R):
#
#   E_full(eta_tilde) = log(R_shock / R_base) / dtau_rw   at 2055 (sim-year 30)
#     R       = sum(w * pmax(kg_lt, 0))
#     dtau_rw = realization-weighted mean mtr_kg_lt, shock - base
#     shock   = s_cg_r25 (+5pp), leg = conventional_no_wealth
#
# The levels model makes E_full linear-through-origin in eta; the net-of-tax
# model need NOT be (the response is (1-MC)^eta_tilde, not exp), so this script
# reports the 3-point curve, its through-origin residuals, AND a
# piecewise-linear inversion that does not force the origin. Target moment
# E_full = -0.6/0.238 = -2.52 (author-locked top-rate divisor, same as levels).
#
# Output: other/kg_model_tests/form_ab/efull_logs.csv
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

source('src/misc/calibration_writer.R')

# The output tree the eta-dial vintages were written to. Overridable, because a
# spot check writes its vintages beside the originals under suffixed names and
# has to be measurable without editing this script -- which is the sort of edit
# that used to be how a calibration got re-run.
LOCAL_ROOT = Sys.getenv('KG_CALIB_OUTPUT_ROOT',
                        '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1')
VINTAGE_SUFFIX = Sys.getenv('VINTAGE_SUFFIX', '')
YEAR       = 2055
SHOCK      = 's_cg_r25'

# Where the measured value lands. Written by this script rather than copied out
# of its log by hand: see src/misc/calibration_writer.R for why that distinction
# is the point of the exercise.
CALIB_FILE = 'config/calibrations/kg/bathtub.yaml'

# The grid. These tags, the sweep files under config/calibrations/kg/sweeps/ and
# launch_eta_dial_logs.sh all have to agree; the launcher's generator
# (write_eta_logs_sweep.py) declares the same three points.
runs = tribble(
  ~eta_tilde, ~vintage,
  1.5,        'eta_dial_logs_15',
  1.9,        'eta_dial_logs_19',
  2.3,        'eta_dial_logs_23'
) %>%
  mutate(vintage = paste0(vintage, VINTAGE_SUFFIX))
# Base R_base + dtau come from the LEVELS re-pin central vintage (eta_dial_c_v2,
# kept with full detail). Two reasons this is correct, not a shortcut:
#   (1) the baseline scenario has no behavior -> its realizations are
#       form-INVARIANT, so R_base/tau_rw_base are identical to the logs runs';
#   (2) dtau is shared as "a law property to first order" -- exactly the 07-12
#       levels re-pin convention -- so BOTH forms are pinned on the identical
#       measurement convention (what the memo requires).
# The logs runs used delete_detail=1 (only conventional_no_wealth survives, the
# E_full numerator), so their own baseline/conventional detail is gone; c_v2
# supplies the invariant pieces.
BASE_DTAU_VINTAGE = 'eta_dial_c_v2'

agg = function(f, with_mtr = TRUE) {
  cols = c('weight', 'kg_lt', if (with_mtr) 'mtr_kg_lt')
  fread(f, select = cols, showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(kg_pos = pmax(kg_lt, 0)) %>%
    summarise(
      R      = sum(weight * kg_pos, na.rm = TRUE),
      tau_rw = if (with_mtr)
                 sum(weight * kg_pos * mtr_kg_lt, na.rm = TRUE) /
                 sum(weight * kg_pos,             na.rm = TRUE)
               else NA_real_
    ) %>%
    as.list()
}

base = agg(file.path(LOCAL_ROOT, BASE_DTAU_VINTAGE, 'baseline', 'static',
                     'detail', paste0(YEAR, '.csv')))
s_central = agg(file.path(LOCAL_ROOT, BASE_DTAU_VINTAGE, SHOCK,
                          'conventional', 'detail', paste0(YEAR, '.csv')))
dtau = s_central$tau_rw - base$tau_rw

results = runs %>%
  pmap_dfr(function(eta_tilde, vintage) {
    s = agg(file.path(LOCAL_ROOT, vintage, SHOCK, 'conventional_no_wealth',
                      'detail', paste0(YEAR, '.csv')), with_mtr = FALSE)
    tibble(
      eta_tilde = eta_tilde,
      vintage   = vintage,
      R_base_B  = base$R / 1e9,
      R_shock_B = s$R / 1e9,
      dtau      = dtau,
      E_full    = log(s$R / base$R) / dtau
    )
  })

out = 'other/kg_model_tests/form_ab/efull_logs.csv'
write_csv(results, out)
print(as.data.frame(results))
cat(sprintf('\nwrote %s\n', out))

#-------------------------------------------------------------------------------
# Invert for eta_tilde* at E_full_target = -2.52.
#-------------------------------------------------------------------------------
E_FULL_TARGET = -0.6 / 0.238

# (a) through-origin slope (the levels convention) + residuals, for comparison.
slope0   = with(results, sum(-E_full * eta_tilde) / sum(eta_tilde^2))
resid0   = with(results, -E_full - slope0 * eta_tilde)
eta_lin0 = abs(E_FULL_TARGET) / slope0

# (b) piecewise-linear inversion between the two bracketing grid points (does
# NOT force the origin -- robust to net-of-tax curvature).
o   = results[order(results$eta_tilde), ]
eta_pw = NA_real_
for (i in 1:(nrow(o) - 1)) {
  lo = o$E_full[i]; hi = o$E_full[i + 1]
  if ((lo - E_FULL_TARGET) * (hi - E_FULL_TARGET) <= 0) {
    w = (E_FULL_TARGET - lo) / (hi - lo)
    eta_pw = o$eta_tilde[i] + w * (o$eta_tilde[i + 1] - o$eta_tilde[i])
    break
  }
}

cat('\n--- eta_tilde re-pin (logs form) ----------------------------------\n')
cat(sprintf('target E_full            : %.4f  (= -0.6/0.238)\n', E_FULL_TARGET))
cat(sprintf('per-point -E_full/eta     : %s\n',
            paste(sprintf('%.4f', with(results, -E_full / eta_tilde)), collapse = ', ')))
cat(sprintf('through-origin slope      : %.5f  (residuals %s)\n',
            slope0, paste(sprintf('%+.4f', resid0), collapse = ', ')))
cat(sprintf('eta_tilde* (through-origin): %.4f\n', eta_lin0))
cat(sprintf('eta_tilde* (piecewise lin) : %.4f  <-- use this if residuals are large\n',
            eta_pw))
cat('If |residuals| are non-trivial the net-of-tax curve bends: prefer the\n')
cat('piecewise value and run the confirmation vintage at it (target +-2%).\n')
cat('-------------------------------------------------------------------\n')

fit_out = tibble(
  target_E_full  = E_FULL_TARGET,
  divisor        = 0.238,
  slope0         = slope0,
  eta_tilde_lin0 = eta_lin0,
  eta_tilde_pw   = eta_pw,
  grid_eta       = paste(results$eta_tilde, collapse = ' '),
  grid_E_full    = paste(sprintf('%.4f', results$E_full), collapse = ' '),
  tax_data_vintage = '2026070814'
)
write_csv(fit_out, 'other/kg_model_tests/form_ab/eta_tilde_fit.csv')
cat('wrote other/kg_model_tests/form_ab/eta_tilde_fit.csv\n')

#-------------------------------------------------------------------------------
# Write the value into the calibration file. This is the last step of the
# calibration, not a separate chore: the number that ships is the number this
# script measured, and nobody transcribes it.
#
# The piecewise inversion is the one that ships, per the note above -- the
# net-of-tax curve need not be linear through the origin, so forcing it would
# bias the pin. Four decimals, because that is the precision the moment supports
# and the precision the shipped value has always carried.
#
# If this re-run does not reproduce the shipped value, the writer leaves the file
# alone and puts its version at bathtub.yaml.proposed with a banner. That is
# deliberate: a calibrated value moving is a finding for the author, not a commit.
#-------------------------------------------------------------------------------

if (is.na(eta_pw)) {
  stop('The grid does not bracket the target moment (E_full = ', round(E_FULL_TARGET, 4),
       '), so there is no piecewise inversion to ship. Widen the grid in ',
       'other/kg_model_tests/form_ab/write_eta_logs_sweep.py and re-run it.')
}

calib_write_entry(
  path   = CALIB_FILE,
  entry  = 'eta_logs',
  value  = round(eta_pw, 4),
  fields = list(
    kind = 'calibrated',
    set  = format(Sys.Date()),
    target = calib_prose(sprintf(
      'Constant net-of-tax elasticity dlog(r_D)/dlog(1 - MC). Same protocol and
       same local moment as the levels eta: E_full measured at sim-year %d on the
       +5pp gains shock across the trial grid (%s), inverted piecewise for the
       eta_tilde that hits E_full = %.4f (= -0.6/%.3f, the author-locked top-rate
       divisor). Measured E_full at each grid point: %s.',
      YEAR, paste(results$eta_tilde, collapse = ' '), E_FULL_TARGET, 0.238,
      paste(sprintf('%.4f', results$E_full), collapse = ' '))),
    derived_under = list(tax_data           = fit_out$tax_data_vintage,
                         macro_projections  = '2026022522'),
    invalidated_by = c('src/sim/kg/constants.R',
                       'src/sim/kg/bellman.R',
                       'src/sim/kg/recurrence.R',
                       'src/sim/kg/timing.R',
                       'src/sim/kg/apply.R'),
    conditioned_on = list(settings.kg.applier_allocation = '0.5',
                          settings.kg.timing_ref_wedge   = 0.05,
                          settings.kg.timing_window      = 1),
    rederive    = 'other/kg_model_tests/form_ab/measure_efull_logs.R',
    active_when = list(kg.response_form = 'logs'),
    note = calib_prose(sprintf(
      "LIVE by default (response_form = 'logs' since 2026-07-22). Lower than the
       levels eta because the net-of-tax full-sim slope is steeper, so the same
       E_full needs a smaller eta_tilde. Through-origin inversion for comparison:
       %.4f (residuals %s) -- the shipped value is the piecewise one. Grid + fit:
       other/kg_model_tests/form_ab/eta_tilde_fit.csv, and the trial values
       themselves are config/calibrations/kg/sweeps/eta_logs_*/bathtub.yaml.",
      eta_lin0, paste(sprintf('%+.4f', resid0), collapse = ' ')))))
