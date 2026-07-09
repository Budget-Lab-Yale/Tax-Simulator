#-------------------------------------------------------------------------------
# test_planned_timing.R
#
# Focused checks for the single-pool timing overlay helper (spec v3).
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

# timeable_share = 0 shuts the timing overlay off entirely.
z = kg_dyn_build_planned_timing(cells, make_tau(rep(0.20, length(years))), years,
                                timeable_share = 0, timing_window = 1,
                                ages_bathtub = ages)
stopifnot(all(z$R_planned_B == 0),
          all(z$R_planned_S == 0),
          all(z$planned_timing_shift == 0))

# No-reform paths do not retime timeable dollars even if baseline MTR levels vary.
varying_tau = make_tau(c(0.25, 0.20, 0.22, 0.18, 0.24))
no_reform = kg_dyn_build_planned_timing(cells, varying_tau, years,
                                        tau_B_mat = varying_tau,
                                        timeable_share = 0.2,
                                        timing_window = 1,
                                        ages_bathtub = ages)
stopifnot(all(no_reform$R_planned_B == no_reform$R_planned_S),
          all(no_reform$planned_timing_shift == 0))

# Delayed hike: timeable dollars scheduled next year move into the current low-tax year.
baseline_tau = make_tau(rep(0.20, length(years)))
delayed = kg_dyn_build_planned_timing(cells, make_tau(c(0.20, 0.25, 0.25, 0.25, 0.25)),
                                      years, timeable_share = 0.2,
                                      tau_B_mat = baseline_tau,
                                      timing_window = 1, ages_bathtub = ages)
stopifnot(all(abs(delayed$R_planned_S[, '2026'] - 40) < 1e-9),
          all(abs(delayed$R_planned_S[, '2027'])      < 1e-9))

# Temporary hike: timeable dollars scheduled in the high-tax year delay one year.
temporary = kg_dyn_build_planned_timing(cells, make_tau(c(0.25, 0.20, 0.20, 0.20, 0.20)),
                                        years, timeable_share = 0.2,
                                        tau_B_mat = baseline_tau,
                                        timing_window = 1, ages_bathtub = ages)
stopifnot(all(abs(temporary$R_planned_S[, '2026'])      < 1e-9),
          all(abs(temporary$R_planned_S[, '2027'] - 40) < 1e-9))

# End of a multi-year high-rate window: year 2029 can delay into lower-tax 2030.
sunset = kg_dyn_build_planned_timing(cells, make_tau(c(0.25, 0.25, 0.25, 0.25, 0.20)),
                                     years, timeable_share = 0.2,
                                     tau_B_mat = baseline_tau,
                                     timing_window = 1, ages_bathtub = ages)
stopifnot(all(abs(sunset$R_planned_S[, '2029'])      < 1e-9),
          all(abs(sunset$R_planned_S[, '2030'] - 40) < 1e-9))

# Timeable dollars are conserved within each age cell.
stopifnot(all(rowSums(delayed$R_planned_B) == rowSums(delayed$R_planned_S)),
          all(rowSums(temporary$R_planned_B) == rowSums(temporary$R_planned_S)),
          all(rowSums(sunset$R_planned_B) == rowSums(sunset$R_planned_S)))

# With timeable_share = 0 the scenario rate is the full-pool Bellman rate alone
# (no fixed bucket, no timing shift): r_S = r_ordinary_S.
bt = cells[['2026']]
rate_info = kg_dyn_build_scenario_rate(
  baseline_t      = bt,
  r_ordinary_S    = 0.03,
  R_planned_B_col = z$R_planned_B[, '2026'],
  R_planned_S_col = z$R_planned_S[, '2026']
)
stopifnot(all(abs(rate_info$r_S - 0.03) < 1e-12),
          all(rate_info$r_planned_B == 0),
          all(rate_info$r_planned_S == 0))

# Baseline Bellman inversion: single pool r_D = r_D_B = r_B (the whole rate is
# discretionary). Under the entropy cost Pass-1 additionally recovers
# kappa = MC exactly (C'(r_D_B) = 0).
grid_packed = list(
  m   = matrix(0, nrow = length(ages), ncol = 2,
               dimnames = list(as.character(ages), as.character(years[1:2]))),
  r_B = matrix(c(0.05, 0.04, 0.05, 0.04), nrow = length(ages),
               dimnames = list(as.character(ages), as.character(years[1:2])))
)
tau_mat = matrix(0.2, nrow = length(ages), ncol = 2,
                 dimnames = list(as.character(ages), as.character(years[1:2])))
pass = kg_dyn_solve_bellman(grid_packed, tau_mat, c_phi = 0, eta = 5,
                            beta_by_year = c(0.96, 0.96))
stopifnot(all(abs(pass$r_D - grid_packed$r_B) < 1e-12),
          all(abs(pass$kappa - pass$MC) < 1e-12))

# Friction: a 1pp delayed hike with default 5pp reference wedge moves only
# 20% of next year's timeable bucket (4 of 20) into the announcement year. The
# announcement year retains its own 20 and gains 4 from 2027; 2027 keeps the
# other 16. Total timeable dollars per age cell remain conserved.
small_delayed = kg_dyn_build_planned_timing(cells,
                                            make_tau(c(0.20, 0.21, 0.21, 0.21, 0.21)),
                                            years, timeable_share = 0.2,
                                            tau_B_mat = baseline_tau,
                                            timing_window = 1,
                                            ref_wedge = 0.05,
                                            ages_bathtub = ages)
stopifnot(all(abs(small_delayed$R_planned_S[, '2026'] - 24) < 1e-9),
          all(abs(small_delayed$R_planned_S[, '2027'] - 16) < 1e-9),
          all(abs(rowSums(small_delayed$R_planned_B) -
                  rowSums(small_delayed$R_planned_S)) < 1e-9))

# Friction: a 10pp delayed hike saturates the clamp -- all timeable dollars move.
big_delayed = kg_dyn_build_planned_timing(cells,
                                          make_tau(c(0.20, 0.30, 0.30, 0.30, 0.30)),
                                          years, timeable_share = 0.2,
                                          tau_B_mat = baseline_tau,
                                          timing_window = 1,
                                          ref_wedge = 0.05,
                                          ages_bathtub = ages)
stopifnot(all(big_delayed$R_planned_S[, '2026'] == 40),
          all(big_delayed$R_planned_S[, '2027'] == 0))

# Friction: shrinking ref_wedge approaches the all-or-nothing limit even for
# small differentials. With ref_wedge = 0.005, a 1pp shock saturates.
tight = kg_dyn_build_planned_timing(cells,
                                    make_tau(c(0.20, 0.21, 0.21, 0.21, 0.21)),
                                    years, timeable_share = 0.2,
                                    tau_B_mat = baseline_tau,
                                    timing_window = 1,
                                    ref_wedge = 0.005,
                                    ages_bathtub = ages)
stopifnot(all(tight$R_planned_S[, '2026'] == 40),
          all(tight$R_planned_S[, '2027'] == 0))

# Validation: nonpositive ref_wedge should fail-fast.
stopifnot(inherits(try(kg_dyn_validate_timing_params(ref_wedge = 0),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_timing_params(ref_wedge = -0.01),
                       silent = TRUE), 'try-error'))

# Validation: timeable_share must lie in [0, 1] when set; NA (uncalibrated) is
# permitted so the module still loads before the calibration paste.
stopifnot(inherits(try(kg_dyn_validate_timing_params(timeable_share = 1.5),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_timing_params(timeable_share = -0.1),
                       silent = TRUE), 'try-error'),
          isTRUE(kg_dyn_validate_timing_params(timeable_share = NA_real_)),
          isTRUE(kg_dyn_validate_timing_params(timeable_share = 0.5)))

cat("planned timing tests passed\n")
