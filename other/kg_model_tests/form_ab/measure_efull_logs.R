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

LOCAL_ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1'
YEAR       = 2055
SHOCK      = 's_cg_r25'

runs = tribble(
  ~eta_tilde, ~vintage,
  1.5,        'eta_dial_logs_15',
  1.9,        'eta_dial_logs_19',
  2.3,        'eta_dial_logs_23'
)
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
