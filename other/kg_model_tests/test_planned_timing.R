#-------------------------------------------------------------------------------
# test_planned_timing.R
#
# Focused checks for the two-bucket forced-window timing helper.
#-------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})

source('./src/sim/kg_dynamics.R')

ages  = 18:19
years = 2026:2030

make_cells = function(R_vals) {
  out = list()
  for (j in seq_along(years)) {
    G = rep(1000, length(ages))
    R = rep(R_vals[j], length(ages))
    out[[as.character(years[j])]] = tibble(
      age       = ages,
      G_B       = G,
      R_B       = R,
      r_B       = R / G,
      m         = 0,
      mG_record = 0,
      mR_record = 0
    )
  }
  out
}

make_tau = function(vals) {
  matrix(rep(vals, each = length(ages)),
         nrow = length(ages),
         dimnames = list(as.character(ages), as.character(years)))
}

cells = make_cells(rep(100, length(years)))
baseline_tau = make_tau(rep(0.20, length(years)))

# planned_share = 0 preserves the ordinary Bellman-only model.
z = kg_dyn_build_forced_window_timing(cells, baseline_tau, years,
                                      tau_B_mat = baseline_tau,
                                      planned_share = 0,
                                      ages_bathtub = ages)
stopifnot(all(z$R_forced_B == 0),
          all(z$R_forced_S == 0),
          all(z$forced_timing_shift == 0))

# No-reform paths reproduce baseline timing even if baseline MTR levels vary.
varying_tau = make_tau(c(0.25, 0.20, 0.22, 0.18, 0.24))
no_reform = kg_dyn_build_forced_window_timing(cells, varying_tau, years,
                                              tau_B_mat = varying_tau,
                                              planned_share = 0.2,
                                              ages_bathtub = ages)
stopifnot(all(no_reform$R_forced_B == no_reform$R_forced_S),
          all(no_reform$forced_timing_shift == 0))

# Delayed hike: forced dollars scheduled next year move into the current
# low-tax year when the generalized-cost advantage saturates the ref_wedge.
delayed = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.20, 0.25, 0.25, 0.25, 0.25)), years,
  tau_B_mat = baseline_tau, planned_share = 0.2, ages_bathtub = ages
)
stopifnot(all(abs(delayed$R_forced_S[, '2026'] - 40) < 1e-9),
          all(abs(delayed$R_forced_S[, '2027']) < 1e-9))

# Temporary hike: forced dollars scheduled in the high-tax year delay one year.
temporary = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.25, 0.20, 0.20, 0.20, 0.20)), years,
  tau_B_mat = baseline_tau, planned_share = 0.2, ages_bathtub = ages
)
stopifnot(all(abs(temporary$R_forced_S[, '2026']) < 1e-9),
          all(abs(temporary$R_forced_S[, '2027'] - 40) < 1e-9))

# End of a multi-year high-rate window: year 2029 can delay into lower-tax 2030.
sunset = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.25, 0.25, 0.25, 0.25, 0.20)), years,
  tau_B_mat = baseline_tau, planned_share = 0.2, ages_bathtub = ages
)
stopifnot(all(abs(sunset$R_forced_S[, '2029']) < 1e-9),
          all(abs(sunset$R_forced_S[, '2030'] - 40) < 1e-9))

# Forced-window dollars are conserved within each age cell.
stopifnot(all(rowSums(delayed$R_forced_B) == rowSums(delayed$R_forced_S)),
          all(rowSums(temporary$R_forced_B) == rowSums(temporary$R_forced_S)),
          all(rowSums(sunset$R_forced_B) == rowSums(sunset$R_forced_S)))

# With planned_share = 0, total scenario rates reduce to ordinary only.
bt = cells[['2026']]
rate_info = kg_dyn_build_scenario_rate(
  baseline_t      = bt,
  r_ordinary_S    = 0.03,
  R_planned_B_col = z$R_forced_B[, '2026'],
  R_planned_S_col = z$R_forced_S[, '2026']
)
stopifnot(all(rate_info$r_S == 0.03),
          all(rate_info$r_fixed_B == 0),
          all(rate_info$r_forced_B == 0),
          all(rate_info$r_forced_S == 0))

# Baseline Bellman inversion targets the ordinary bucket exactly.
grid_packed = list(
  m   = matrix(0, nrow = length(ages), ncol = 2,
               dimnames = list(as.character(ages), as.character(years[1:2]))),
  r_B = matrix(c(0.05, 0.04, 0.05, 0.04), nrow = length(ages),
               dimnames = list(as.character(ages), as.character(years[1:2])))
)
tau_mat = matrix(0.2, nrow = length(ages), ncol = 2,
                 dimnames = list(as.character(ages), as.character(years[1:2])))
pass = kg_dyn_solve_bellman_baseline(grid_packed, tau_mat, psi = 25,
                                      planned_share = 0.2,
                                      beta_by_year = c(0.96, 0.96))
stopifnot(all(abs(pass$r_D - 0.8 * grid_packed$r_B) < 1e-12))

# Friction: a 1pp delayed hike with default 5pp reference wedge moves only
# 20% of next year's forced bucket (4 of 20) into the announcement year.
small_delayed = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.20, 0.21, 0.21, 0.21, 0.21)), years,
  planned_share = 0.2, tau_B_mat = baseline_tau, ref_wedge = 0.05,
  ages_bathtub = ages
)
stopifnot(all(abs(small_delayed$R_forced_S[, '2026'] - 24) < 1e-9),
          all(abs(small_delayed$R_forced_S[, '2027'] - 16) < 1e-9),
          all(abs(rowSums(small_delayed$R_forced_B) -
                  rowSums(small_delayed$R_forced_S)) < 1e-9))

# Friction: a 10pp delayed hike saturates the clamp -- all forced dollars move.
big_delayed = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.20, 0.30, 0.30, 0.30, 0.30)), years,
  planned_share = 0.2, tau_B_mat = baseline_tau, ref_wedge = 0.05,
  ages_bathtub = ages
)
stopifnot(all(abs(big_delayed$R_forced_S[, '2026'] - 40) < 1e-9),
          all(abs(big_delayed$R_forced_S[, '2027']) < 1e-9))

# Friction: shrinking ref_wedge approaches the all-or-nothing limit even for
# small differentials. With ref_wedge = 0.005, a 1pp shock saturates.
tight = kg_dyn_build_forced_window_timing(
  cells, make_tau(c(0.20, 0.21, 0.21, 0.21, 0.21)), years,
  planned_share = 0.2, tau_B_mat = baseline_tau, ref_wedge = 0.005,
  ages_bathtub = ages
)
stopifnot(all(abs(tight$R_forced_S[, '2026'] - 40) < 1e-9),
          all(abs(tight$R_forced_S[, '2027']) < 1e-9))

# Baseline intercept: if a nearby baseline year has a lower tax rate, no-reform
# timing still stays at the scheduled year.
baseline_dip = make_tau(c(0.25, 0.20, 0.25, 0.25, 0.25))
intercept_case = kg_dyn_build_forced_window_timing(
  cells, baseline_dip, years, tau_B_mat = baseline_dip,
  planned_share = 0.2, ref_wedge = 0.05, ages_bathtub = ages
)
stopifnot(all(intercept_case$R_forced_B == intercept_case$R_forced_S))

# Validation: fixed bucket and unsupported H should fail-fast.
stopifnot(inherits(try(kg_dyn_validate_realization_buckets(fixed_share = 0.4),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(timing_window = 2),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(ref_wedge = 0),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(ref_wedge = -0.01),
                       silent = TRUE), 'try-error'))

cat("forced-window timing tests passed\n")
