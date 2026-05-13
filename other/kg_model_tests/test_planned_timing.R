#-------------------------------------------------------------------------------
# test_planned_timing.R
#
# Focused checks for the forced-window Bellman state. The old planned_* names
# remain compatibility aliases only; forced-state outputs are authoritative.
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

force_state = function(cells, tau_S, lambda = 0.2, tau_B = make_tau(rep(0.20, length(years))),
                        ref_wedge = 0.05) {
  kg_dyn_solve_forced_window_state(
    baseline_cells = cells,
    tau_S_mat      = tau_S,
    years          = years,
    tau_B_mat      = tau_B,
    planned_share  = lambda,
    timing_window  = 1,
    ref_wedge      = ref_wedge,
    ages_bathtub   = ages
  )
}

forced_objective = function(x, tau_S, year, q, ref_wedge = 0.05) {
  j = match(as.character(year), colnames(x$q_forced_S))
  now_value = -tau_S[as.character(ages), as.character(year)]
  wait_value = x$F0_forced_S[, j + 1]
  q * now_value + (1 - q) * wait_value +
    x$forced_intercept[, j] * q -
    0.5 * ref_wedge * (q - KG_DYN_FORCED_Q_B)^2
}

cells = make_cells(rep(100, length(years)))
baseline_tau = make_tau(rep(0.20, length(years)))

# lambda = 0 preserves the ordinary Bellman-only model.
z = force_state(cells, baseline_tau, lambda = 0)
stopifnot(all(z$R_forced_B == 0),
          all(z$R_forced_S == 0),
          all(z$forced_timing_shift == 0),
          all(z$E_forced_B == 0))

bt = cells[['2026']]
rate_info = kg_dyn_build_scenario_rate(
  baseline_t     = bt,
  r_ordinary_S   = 0.03,
  R_forced_B_col = z$R_forced_B[, '2026'],
  R_forced_S_col = z$R_forced_S[, '2026'],
  fixed_share    = 0
)
stopifnot(all(rate_info$r_S == 0.03),
          all(rate_info$r_forced_B == 0),
          all(rate_info$r_forced_S == 0))

# Baseline entrant inference with q_B = 0.5 reproduces lambda * R_B.
base = force_state(cells, baseline_tau, lambda = 0.2)
stopifnot(all(abs(base$q_forced_B - 0.5) < 1e-12),
          all(abs(base$F0_forced_B + baseline_tau) < 1e-12),
          all(abs(base$F0_forced_S + baseline_tau) < 1e-12),
          all(abs(base$R_forced_B - 20) < 1e-12),
          all(abs(base$q_forced_B * base$E_forced_B +
                    cbind(0, (1 - base$q_forced_B[, -ncol(base$q_forced_B)]) *
                             base$E_forced_B[, -ncol(base$E_forced_B)]) -
                    base$R_forced_B) < 1e-12))

# No-reform paths reproduce baseline forced realizations even when baseline MTR
# levels vary over time.
varying_tau = make_tau(c(0.25, 0.20, 0.22, 0.18, 0.24))
no_reform = force_state(cells, varying_tau, lambda = 0.2, tau_B = varying_tau)
stopifnot(all(abs(no_reform$R_forced_B - no_reform$R_forced_S) < 1e-12),
          all(abs(no_reform$q_forced_S - no_reform$q_forced_B) < 1e-12),
          all(no_reform$planned_timing_shift == no_reform$forced_timing_shift))

# Entrant inference fails fast if the baseline forced realization path implies
# materially negative entrant cohorts under q_B = 0.5.
bad_cells = make_cells(c(100, 1, 100, 100, 100))
stopifnot(inherits(try(force_state(bad_cells, baseline_tau, lambda = 0.2),
                       silent = TRUE), 'try-error'))

# Delayed future hike: q_S rises in the announcement year and accelerates
# forced realizations from the one-year-left state.
delayed = force_state(cells, make_tau(c(0.20, 0.25, 0.25, 0.25, 0.25)))
stopifnot(all(abs(delayed$q_forced_S[, '2026'] - 1) < 1e-12),
          all(abs(delayed$R_forced_S[, '2026'] - 40) < 1e-12),
          all(abs(delayed$R_forced_S[, '2027']) < 1e-12))

# Temporary current hike: q_S falls and the same cohort realizes at deadline.
temporary = force_state(cells, make_tau(c(0.25, 0.20, 0.20, 0.20, 0.20)))
stopifnot(all(abs(temporary$q_forced_S[, '2026']) < 1e-12),
          all(abs(temporary$R_forced_S[, '2026']) < 1e-12),
          all(abs(temporary$R_forced_S[, '2027'] - 40) < 1e-12))

# Friction: a 1pp delayed hike moves q by 0.2 with the default 5pp wedge.
small_delayed_tau = make_tau(c(0.20, 0.21, 0.21, 0.21, 0.21))
small_delayed = force_state(cells, small_delayed_tau)
stopifnot(all(abs(small_delayed$q_forced_S[, '2026'] - 0.7) < 1e-12),
          all(abs(small_delayed$R_forced_S[, '2026'] - 28) < 1e-12),
          all(abs(small_delayed$R_forced_S[, '2027'] - 12) < 1e-12))

# The reported q_S is the F1 Bellman optimizer. For an interior case, it beats
# nearby controls; for corner cases, the chosen bound beats moving inward.
q_int = small_delayed$q_forced_S[, '2026']
stopifnot(all(abs(small_delayed$F1_forced_S[, '2026'] -
                    forced_objective(small_delayed, small_delayed_tau,
                                     2026, q_int)) < 1e-12),
          all(small_delayed$F1_forced_S[, '2026'] >=
                forced_objective(small_delayed, small_delayed_tau,
                                 2026, q_int - 0.01)),
          all(small_delayed$F1_forced_S[, '2026'] >=
                forced_objective(small_delayed, small_delayed_tau,
                                 2026, q_int + 0.01)))

stopifnot(all(delayed$F1_forced_S[, '2026'] >=
                forced_objective(delayed, make_tau(c(0.20, 0.25, 0.25, 0.25, 0.25)),
                                 2026, 0.99)),
          all(temporary$F1_forced_S[, '2026'] >=
                forced_objective(temporary, make_tau(c(0.25, 0.20, 0.20, 0.20, 0.20)),
                                 2026, 0.01)))

# Forced dollars are conserved across fully observed entrant cohorts. The last
# year's entrants may have deadline realizations beyond the simulation boundary.
observed_cohort_total = function(x) {
  rowSums(x$R_forced_S[, -ncol(x$R_forced_S), drop = FALSE]) +
    (1 - x$q_forced_S[, ncol(x$q_forced_S) - 1]) *
      x$E_forced_B[, ncol(x$E_forced_B) - 1]
}
source_cohort_total = function(x) {
  rowSums(x$E_forced_B[, -ncol(x$E_forced_B), drop = FALSE])
}
stopifnot(all(abs(observed_cohort_total(delayed) -
                  source_cohort_total(delayed)) < 1e-9),
          all(abs(observed_cohort_total(temporary) -
                  source_cohort_total(temporary)) < 1e-9))

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
                                      phi_I = 0, planned_share = 0.2,
                                      beta_by_year = c(0.96, 0.96))
stopifnot(all(abs(pass$r_D - 0.8 * grid_packed$r_B) < 1e-12))

# Validation: fixed buckets, unsupported windows, and nonpositive ref_wedge fail.
stopifnot(inherits(try(kg_dyn_validate_realization_buckets(fixed_share = 0.01),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(timing_window = 0),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(timing_window = 2),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(ref_wedge = 0),
                       silent = TRUE), 'try-error'),
          inherits(try(kg_dyn_validate_realization_buckets(ref_wedge = -0.01),
                       silent = TRUE), 'try-error'))

cat("forced-window Bellman-state timing tests passed\n")
